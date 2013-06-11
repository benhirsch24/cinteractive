// Official:
//
// State :: (Control, Env, Heap, Kont)
// Control :: CAST
// Env :: Name -> Addr
// type Stack = Env
// Heap :: Addr -> Value
// Kont :: State -> UI Callback x Stepper Callback
// Stepper :: State -> State
// 
// UI Callback :: State -> ()
//
// AST -> Initial State

define(["interp/eval", "interp/util"], function(evaler, util) {
   // Compiles an initial AST node (CTranslUnit) into an initial state
   function compile(ast) {
      var state = build_initial_heap(ast);

      // main should be the function name in the globals
      var main_addr = state.stack[0]["main"];
      
      return {
           control: state.heap[main_addr]
         , stack:   state.stack
         , heap:    state.heap
         , user_types: _.cloneDeep(state.user_types)
         , heapinfo: state.heapinfo
         , kont: function(ui) {
              ui.tell('Main on the stack');
           }
         , next: state.next
         , dump: []
         , frames: [{name: 'globals', params: []}]
         , ret: undefined
      };
   }

   // evalSpecifiers evaluates a list of specifiers into type information
   // eg pointers, structs, and unions.
   function evalSpecifiers(specs, state) {
      var types = _.map(specs, function(s) { 
         var type = s['spec']['node'];

         // If it's a structure or union we need to find out the fields
         if (util.isSUType(type)) {
            var su = s['spec']['sutype'];
            var name = util.unquotify(su['ident']);

            switch(su['node']) {
               case "CStruct":
                  // For each field:
                  //
                  // * Find the type of the field
                  // * Get the name its declared as (which may or may not exist)
                  var fields = _.map(su['fields'], function(f) {
                     var field_type = evalSpecifiers(f['specifiers']);
                     if (!(_.isUndefined(f['declarations'])) && _.isArray(f['declarations'])
                        && !(_.isEmpty(f['declarations'])))
                     {
                        var decl = f['declarations'][0]['declarator'];
                        var field_name = util.unquotify(decl['name']);
                        return { name: field_name, type: field_type };
                     }
                  });
                  return { type: 'CStruct', name: name, fields: fields };
               case "CUnion":
                  break;
            }
         }
         else {
            // Otherwise just return the name (CInteger, CLong, CLongUnsigned, etc)
            return s['spec']['node']; 
         }
      });

      return types;
   }


   // Push the parameters of the CFunctionDef pointed to by state.control
   // onto the stack with appropriate heap information.
   // 
   // TODO make this part of CCall
   function push_params(state) {
      var top_addr = state.next - 1;

      // Chain on the params of the function
      // For each CDecl, find the type specifiers and if there's multiple decls
      //   ie int a,b,c;
      // then flatten all these down and reverse them (first arg should be first on stack)
      // finally on each decl name add it to the stack starting from the top address down
      // since the CCall pushed the args into a new stack frame on the heap already
      _(state.control["fun_def"]["attrs"][0]["params"]).map(function(p) {
         var type = evalSpecifiers(p["specifiers"], state);
         var decls = _.map(p.declarations, function(decl) {
            var d = decl;
            d["type"] = type;
            return d;
         });
         return decls;
      }).flatten().reverse().map(function(decl) {
         var name = util.unquotify(decl["declarator"]["name"]);
         var type = decl["type"];
         state.stack[0][name] = top_addr;
         state.heapinfo[top_addr] = {'node': 'CDecl', type: type, name: name};
         top_addr -= 1;
      });
   }

   var step = {};

   // When we find a CThunk then there must be a return entry in the state.
   // :eval is the original node (could've been a CAssign rvalue fun call).
   // :hole is what attribute to fill in in the original node.
   step["CThunk"] = function(state) {
      var node = state.control["eval"];
      var line = state.control["line"];
      var hole = state.control["hole"];

      node["hole"] = "filled";
      node[state.control["hole"]] = state.ret;
      state.kont = function(ui, cm) {
         ui.tell('Executing thunk, filling hole ' + hole);
         ui.hilite_line(line);
      };

      state.ret = undefined;
      state.control = undefined;
      // Finally put the node back on the dump
      _(state.dump).last().unshift(node);

      return state;
   };

   // TODO: push params should go into evaling CCall
   // Pushes the parameters onto the stack and fills in heapinfo.
   step["CFunDef"] = function(state) {
      var name = state.control['fun_def']['name'];
      var line = state.control['line'];
      stack = push_params(state);
      state.kont = function(ui, cm) {
         ui.tell('Calling Function: ' + util.unquotify(name));
         ui.hilite_line(line);
      };
      state.dump = _(state.dump).push(_(state.control["statements"]["block_items"]).cloneDeep());
      state.control = undefined;

      return state;
   };

   // Declaring a variable.
   step["CDecl"] = function(state) {
      // Get the type of the variable.
      var type = evalSpecifiers(state.control["specifiers"], state);
      var names = '';
      var line = state.control['line'];

      // For each variable declaration for this type on this line...
      _(state.control["declarations"]).map(function (decl) {
         var val;

         // If there's a variable initializer ie int x = 0;
         // The = 0 is the initializer. Could be int x = times_two(3);
         if (_(decl).has('initializer') && decl['initializer'] !== null)
         {
            val = evaler.eval(decl['initializer'], state);
            state = val.state;
            val = val.val;
         } else
         {
            // Get a default value for the type
            // TODO make this random data for C evil
            var valM = initial_value(decl, type, state);
            val = valM.val;
            state = valM.state;
         }

         var name = util.unquotify(decl["declarator"]["name"]);
         names += name;

         // If this is just regular type, then make heap info the type.
         // otherwise we need to know if it's an array or pointer or whatevs.
         var heapinfo = {};
         var isCompound = false; // ie array, struct, union
         var compound_len = 0;

         // If there's no attributes the declaration should be dependent on the type.
         if (_.isEmpty(decl["declarator"]["attrs"])) {
            // If it's a struct or Union, then get the struct's type name (eg struct Point) 
            // and type info from the state's user_types
            // then enter it into the heapinfo as a CCompoundType and get the number of fields.
            if (type[0]['type'] === "CStruct" || type[0]['type'] === "CUnion") {
               var sname = type[0]['name'];
               var stype = state.user_types[sname];
               heapinfo = {node: "CCompoundType", type: stype};
               isCompound = true;
               compound_len = _(stype.fields).size();
            } else
            {
               // Otherwise it's just an elementary type like int or double.
               heapinfo = {node: "CElemType", type: type, name: name};
            }
         } else
         {
            // If there are attributes on the declarator then it's an array, pointer,
            // or Function pointer (I think it's a fun ptr).
            // Each array kinda builds on the last.
            var node_type = 'CDecl';
            var attrs = _.map(decl["declarator"]["attrs"], function(a) {
               var ret = {};
               ret["node"] = a["node"];

               // If it's an array, eval the size of the array
               // (eval because it could be 2 + 4 or crazyMath(2.9))
               //
               // TODO: Should be evalOrThunk
               if (ret["node"] === "CArrDeclr") {
                  node_type = 'CArrDeclr';
                  var size = evaler.eval(a["size"]["size"], state);
                  state = size.state;

                  ret["length"] = size.val;
                  ret["size"] = ret["length"] * evaler.sizes[type[0]];
                  ret["type"] = type;
                  compound_len += ret["length"];

                  isCompound = true;
               } else if (ret["node"] === "CFunDeclr")
               // TODO: Clearly
               {
                  node_type = "CFunDeclr";
               } else if (ret["node"] === "CPtrDeclr")
               // TODO: Clearly
               {
                  node_type = "CPtrDeclr";
               }

               return ret;
            });

            heapinfo = {node: node_type, type: attrs, name: name};
         }

         var addr = state.next;

         state.heapinfo[addr] = heapinfo
         state.stack[0][name] = addr;

         if (!isCompound) {
            state.heap[addr] = val;
            state.next += 1;
         } else
         {
            for (i = 0; i < compound_len; i++)
               state.heap[i + addr] = 0;
            state.next += compound_len;
         }
      });

      state.control = undefined;
      state.kont = function(ui, cm) {
         ui.tell('Declared: ' + names);
         ui.hilite_line(line);
      };

      return state;
   };
   step["CUnary"] = function(state) {
      var line = state.control.line;
      var msg = '';

      var val = evaler.evalLhs(state.control["expr"], state);
      var addr = val.val;
      var ret;

      var remove = false;

      if (_.isUndefined(state.heap[addr])) { // you can do eg 1++ which shouldn't actually store anywhere, just temporary
         remove = true;
         state.heap[-1] = v;
         addr = -1;
      }

      if (remove) {
         delete state.heap[-1];
      }

      var op = evaler.unops[state.control["op"]];
      var ret = op(addr, state.heap);

      var v = addr;
      var unmsgs = {};
      unmsgs["CPreIncOp"] = 'Pre-incrementing ' + v;
      unmsgs["CPreDecOp"] = 'Pre-decrementing ' + v;
      unmsgs["CPostIncOp"] = 'Post-incrementing ' + v + ', this takes place after the stmt';
      unmsgs["CPostDecOp"] = 'Post-decrementing ' + v + ', this takes place after the stmt';
      unmsgs["CAdrOp"] = 'Returning address of ' + v;
      unmsgs["CIndOp"] = 'Returing where ' + v + ' points to.';
      //unmsgs["CPlusOp"] = function(u) { return u; } // TODO ignoring these two for now because unary +?
      //unmsgs["CMinOp"] = function(u) { return u; }
      unmsgs["CCompOp"] = "Returning one's complement of " + v;
      unmsgs["CNegOp"] = "Returing negation of " + v;
      var msg = unmsgs[state.control["op"]];

      // TODO: haven't used val yet?
      var val = ret.val;
      state.heap = ret.heap;

      state.kont = function(ui, cm) {
         ui.hilite_line(line);
         ui.tell(msg);
      };
      return state;
   };
   step["CExpr"] = function(state) {
      state.control = state.control["expr"]
      return step[state.control["node"]](state);
   };
   step["CAssign"] = function(state) {
      var control = _.cloneDeep(state.control);
      var line = state.control['line'];

      var rvalue = evaler.evalOrThunk(state.control["rvalue"], state, "rvalue", state.control);
      if (_.isObject(rvalue.val) && !_.isUndefined(rvalue.val["node"]) && evaler.isThunk(rvalue.val)) {
         return state;
      }
      state = rvalue.state;

      var lvalue = evaler.evalLhs(state.control["lvalue"], state);
      state = lvalue.state;

      // Switch on assign ops

      var op = evaler.assops[state.control["op"]];
      var val = op(state.heap[lvalue.val], rvalue.val);

      state.control = undefined;
      state.heap[lvalue.val] = val;
      state.kont = function(ui, cm) {
         ui.tell('Changed var at ' + lvalue.val + ' to ' + val);
         ui.hilite_line(line);
      };
      return state;
   };
   step["CCall"] = function(state) {
      var newstack = sequenceEval(state.control["args"], state);
      state.stack.push(newstack.vals);

      state.next += newstack.vals.length;

      state.control = util.unquotify(state.heap[state["function"]["name"]]);
      return state;
   }
   step["CCompound"] = function(state) {
      var line = state.control.line;
      state.dump = _(state.dump).push(state.control["block_items"]);
      state.kont = function(ui, cm) {
         ui.tell('Compound statements');
         ui.hilite_line(line);
      };
      state.control = undefined;
      return state;
   }
   step["CFor"] = function(state) {
      var line = state.control.line;

      if (!(_.isUndefined(state.control["init"]))) {
         var init = evaler.eval(state.control["init"], state);
         state = init.state;
         state.control["init"] = undefined;
      }

      var guard = evaler.eval(state.control["guard"], state);
      state = guard.state;

      var msg = '';

      if (guard.val === 0 || !guard.val) {
         msg = "Guard was false, skipping loop";
      } else {
         msg = "Guard was true, looping";
         _(state.dump).last().unshift(_.cloneDeep(state.control));
         _(state.dump).last().unshift(_.cloneDeep(state.control["step"]));
         _(state.dump).last().unshift(_.cloneDeep(state.control["next"]));
      }

      state.kont = function(ui, cm) {
         ui.hilite_line(line);
         ui.tell(msg);
      };

      return state;
   };

   step["CWhile"] = function(state) {
      var line = state.control.line;
      var guard = evaler.eval(state.control["guard"], state);
      state = guard.state;

      if (guard.val === 0 || !guard.val) {
         state.kont = function(ui, cm) {
            ui.tell("Condition was false, continuing");
            ui.hilite_line(line);
         };
      } else {
         state.kont = function(ui, cm) {
            ui.tell("Condition was true, looping");
            ui.hilite_line(line);
         };
         _(state.dump).last().unshift(_.cloneDeep(state.control));
         _(state.dump).last().unshift(_.cloneDeep(state.control["next"]));
      }

      return state;
   };

   step["CIf"] = function(state) {
      var line = state.control.line;
      var guard = evaler.eval(state.control["guard"], state);
      state = guard.state;
      var happ = '';

      if (guard.val === 0 || !guard.val) {
         _(state.dump).last().unshift(state.control["false"]);
         happ = 'Condition was false.';
      } else {
         _(state.dump).last().unshift(state.control["true"]);
         happ = 'Condition was true.';
      }

      state.kont = function(ui, cm) {
         ui.tell(happ);
         ui.hilite_line(line);
      };
      state.control = undefined;

      return state;
   };

   step["CReturn"] = function(state) {
      var ret = evaler.eval(state.control, state);
      var line = state.control.line;

      if (_.isString(ret.val))
         ret.val = state.heap[varLookup(ret.val, state.stack)];

      state = ret.state;

      var stacklen = _(state.stack[0]).keys().size();
      state.next -= stacklen;

      _(state.stack[0]).forIn(function(addr, name) {
         delete state.heap[addr];
      });

      _(state.stack).shift();
      state.kont = function(ui, cm) {
         ui.tell('Returning val: ' + ret.val);
         ui.hilite_line(line);
      };
      state.ret = ret.val;
      state.control = undefined;
      state.dump.pop();
      state.frames.shift();

      return state;
   };


   function initial_value(decl, type, state) {
      var val = 0,
          s = {};

      // if there's an initializer, use that
      if (!(_.isNull(decl["initializer"])) && decl["initializer"]["node"] === "CInit") {
         var init = evaler.eval(decl["initializer"], state);
         s = init.state;
         val = init.val;
      } // otherwise decide based on the type
      else {
         s = state;
      }

      return {val: val, state: s};
   }

   function resolveVar(node, state) {
      var type = evalSpecifiers(node["specifiers"], state);

      _(node["declarations"]).map(function(d) {
         var value = initial_value(d, type, state);
         state = value.state;
         var name = util.unquotify(d['declarator']['name']);
         state.heap[state.next] = value.val;
         state.stack[0][name] = state.next;
         state.next += 1;
      });

      return state;
   };

   var compileDecl = {};
   compileDecl["CDecl"] = function(node, state) {
      var type = evalSpecifiers(node["specifiers"], state);

      if (type[0]['type'] === "CStruct" || type[0]['type'] === "CUnion") {
         state.user_types[type[0]['name']] = type[0];
      }

      _.map(node["declarations"], function(d) {
         var value = initial_value(d, type, state);
         state = value.state;
         var name = util.unquotify(d['declarator']['name']);
         state.heap[state.next] = value.val;
         state.heapinfo[state.next] = { 'type': _.map(node["specifiers"], function(s) { return s['spec']['node']; }), 'name': name, 'node': 'CDecl' };
         state.stack[0][name] = state.next;
         state.next += 1;
      });

      return state;
   };
   compileDecl["CFunDef"] = function(node, state) {
      var name = util.unquotify(node["fun_def"]["name"]);
      var fun_type = evalSpecifiers(node['specifiers'], state);
      state.heap[state.next] = node;
      state.stack[0][name] = state.next;

      // Get param info
      var params = node['fun_def']['attrs'][0]['params'];

      var fun_params = {};
      var param_info = _.map(params, function(p) {
         var type = evalSpecifiers(p['specifiers'], state);
         var name = util.unquotify(p['declarations'][0]['declarator']['name']);
         fun_params[name] = {'node': 'CDecl', type: type};
      });

      state.heapinfo[state.next] = { 'type': fun_type, 'name': name, 'node': 'CFunDef', 'params': fun_params };

      state.next += 1;

      return state;
   };

   function build_initial_heap(ast) {
      var state = {heap: {}, stack: [{}], next: 0, dump: [], heapinfo: {}, user_types: {}};
      var decls = ast["decls"];

      _(decls).map(function(n) {
         return compileDecl[n["node"]](n, state);
      });

      return state;
   }

   return {
      step: step,
      compile: compile
   };
});
