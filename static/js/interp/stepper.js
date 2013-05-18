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

var currLine = 0;
function hilite_line(line, cm) {
   cm.removeLineClass(currLine, 'wrap', 'active_line');
   cm.addLineClass(line - 1, 'wrap', 'active_line');
   currLine = line - 1;
};

function compile(ast) {
   var state = build_initial_heap(ast);
   
   return {
        control: state.heap[state.stack[0]["main"]]
      , stack:   state.stack
      , heap:    state.heap
      , heapinfo: state.heapinfo
      , kont: function(ui, cm) {
           ui.html('Main on the stack');
        }
      , next: state.next
      , dump: []
      , ret: undefined
   };
}

function evalSpecifiers(specs, state) {
   var types = _.map(specs, function(s) { return s['spec']['node']; });
   console.log("evaling specifiers");
   console.log(types);
   return types;
}


function push_params(state) {
   var top_addr = state.next - 1;
   console.log("Pushing params");

   // chain on the params of the function
   // for each CDecl, find the type specifiers and if there's multiple decls
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
      var name = unquotify(decl["declarator"]["name"]);
      var type = decl["type"];
      state.stack[0][name] = top_addr;
      state.heapinfo[top_addr] = {'node': 'CDecl', type: type};
      top_addr -= 1;
   });
}

var step = {};
step["CThunk"] = function(state) {
   var node = state.control["eval"];
   var line = state.control["line"];
   node["thunk"] = "filled";
   node[state.control["thunk"]] = state.ret;
   state.kont = function(ui, cm) {
      ui.html('Executing thunk, filling hole ' + node["thunk"]);
      hilite_line(line, cm);
   };
   state.ret = undefined;
   state.control = undefined;
   _(state.dump).last().unshift(node);
   return state;
};
step["CFunDef"] = function(state) {
   var name = state.control['fun_def']['name'];
   var line = state.control['line'];
   stack = push_params(state);
   state.kont = function(ui, cm) {
      ui.html('Calling Function: ' + unquotify(name));
      hilite_line(line, cm);
   };
   state.dump = _(state.dump).push(_(state.control["statements"]["block_items"]).cloneDeep());
   state.control = undefined;
   return state;
}
step["CDecl"] = function(state) {
   var type = evalSpecifiers(state.control["specifiers"], state);
   var names = '';
   var line = state.control['line'];

   _(state.control["declarations"]).map(function (decl) {
      var val;
      if (_(decl).has('initializer') && decl['initializer'] !== null) {
         val = eval(decl['initializer'], state);
         state = val.state;
         val = val.val;
      } else {
         var valM = initial_value(decl, type, state);
         val = valM.val;
         state = valM.state;
      }
         
      var name = unquotify(decl["declarator"]["name"]);
      names += name;

      // if this is just regular type, then make heap info the type.
      // otherwise we need to know if it's an array or pointer or whatevs
      var heapinfo = {};
      var array = false;
      var arr_len = 0;
      if (_.isEmpty(decl["declarator"]["attrs"])) {
         heapinfo = {node: "CElemType", type: type};
      } else {
         var attrs = _.map(decl["declarator"]["attrs"], function(a) {
            var ret = {};
            ret["node"] = a["node"];

            if (ret["node"] === "CArrDeclr") {
               var size = eval(a["size"]["size"], state);
               state = size.state;
               ret["length"] = size.val;
               console.log(type);
               ret["size"] = ret["length"] * sizes[type[0]];
               ret["type"] = type;
               arr_len += ret["length"];

               array = true;
            } else if (ret["node"] === "CFunDeclr") {

            } else if (ret["node"] === "CPtrDeclr") {

            }

            return ret;
         });
         heapinfo = attrs;
      }

      var addr = state.next;

      state.heapinfo[addr] = heapinfo
      state.stack[0][name] = addr;
      if (!array) {
         state.heap[addr] = val;
         state.next += 1;
      } else {
         for (i = 0; i < arr_len; i++)
            state.heap[i + addr] = 0;
         state.next += arr_len;
      }
   });

   state.control = undefined;
   state.kont = function(ui, cm) {
      ui.html('Declared: ' + names);
      hilite_line(line, cm);
   };

   return state;
};
step["CUnary"] = function(state) {
   var line = state.control.line;
   var msg = '';

   var val = eval(state.control["expr"], state);
   var v = val.val;
   var ret;

   var remove = false;

   if (_.isString(v)) // this is a variable of some kind
      addr = varLookup(v, state.stack);
   else { // you can do eg 1++ which shouldn't actually store anywhere, just temporary
      remove = true;
      state.heap[-1] = v;
      addr = -1;
   }

   if (remove) {
      delete state.heap[-1];
   }

   var op = unops[state.control["op"]];
   var ret = op(addr, state.heap);

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

   state.kont = function(ui, cm) {
      hilite_line(line, cm);
      ui.html(msg);
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

   var rvalue = evalOrThunk(state.control["rvalue"], state, "rvalue", state.control);
   if (_.isObject(rvalue.val) && !_.isUndefined(rvalue.val["node"]) && isThunk(rvalue.val)) {
      return state;
   }
   state = rvalue.state;

   var lvalue = evalLhs(state.control["lvalue"], state);
   state = lvalue.state;

   // Switch on assign ops

   var op = assops[state.control["op"]];
   var val = op(state.heap[lvalue.val], rvalue.val);

   state.control = undefined;
   state.heap[lvalue.val] = val;
   state.kont = function(ui, cm) {
      ui.html('Changed var at ' + lvalue.val + ' to ' + val);
      hilite_line(line, cm);
   };
   return state;
};
step["CCall"] = function(state) {
   var newstack = sequenceEval(state.control["args"], state);
   state.stack.push(newstack.vals);

   state.next += newstack.vals.length;

   state.control = unquotify(state.heap[state["function"]["name"]]);
   return state;
}
step["CCompound"] = function(state) {
   var line = state.control.line;
   state.dump = _(state.dump).push(state.control["block_items"]);
   state.kont = function(ui, cm) {
      ui.html('Compound statements');
      hilite_line(line, cm);
   };
   state.control = undefined;
   return state;
}
step["CFor"] = function(state) {
   var line = state.control.line;

   if (!(_.isUndefined(state.control["init"]))) {
      var init = eval(state.control["init"], state);
      state = init.state;
      state.control["init"] = undefined;
   }

   var guard = eval(state.control["guard"], state);
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
      hilite_line(line, cm);
      ui.html(msg);
   };

   return state;
};

step["CWhile"] = function(state) {
   var line = state.control.line;
   var guard = eval(state.control["guard"], state);
   state = guard.state;

   if (guard.val === 0 || !guard.val) {
      state.kont = function(ui, cm) {
         ui.html("Condition was false, continuing");
         hilite_line(line, cm);
      };
   } else {
      state.kont = function(ui, cm) {
         ui.html("Condition was true, looping");
         hilite_line(line, cm);
      };
      _(state.dump).last().unshift(_.cloneDeep(state.control));
      _(state.dump).last().unshift(_.cloneDeep(state.control["next"]));
   }

   return state;
};

step["CIf"] = function(state) {
   var line = state.control.line;
   var guard = eval(state.control["guard"], state);
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
      ui.html(happ);
      hilite_line(line, cm);
   };
   state.control = undefined;

   return state;
};

step["CReturn"] = function(state) {
   var ret = eval(state.control, state);
   var line = state.control.line;

   if (_(ret.val).isString())
      ret.val = state.heap[varLookup(ret.val, state.stack)];

   state = ret.state;

   console.log(_(state.stack[0]).keys().size());
   var stacklen = _(state.stack[0]).keys().size();
   state.next -= stacklen;

   _(state.stack[0]).forIn(function(addr, name) {
      delete state.heap[addr];
   });

   _(state.stack).shift();
   state.kont = function(ui, cm) {
      ui.html('Returning val: ' + ret.val);
      hilite_line(line, cm);
   };
   state.ret = ret.val;
   state.control = undefined;
   state.dump.pop();
   return state;
};


function initial_value(decl, type, state) {
   var val = 0,
       s = {};

   // if there's an initializer, use that
   if (!(_.isNull(decl["initializer"])) && decl["initializer"]["node"] === "CInit") {
      var init = eval(decl["initializer"], state);
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
      var name = unquotify(d['declarator']['name']);
      state.heap[state.next] = value.val;
      state.stack[0][name] = state.next;
      state.next += 1;
   });

   return state;
};

var compileDecl = {};
compileDecl["CDecl"] = function(node, state) {
   var type = evalSpecifiers(node["specifiers"], state);

   _(node["declarations"]).map(function(d) {
      var value = initial_value(d, type, state);
      state = value.state;
      var name = unquotify(d['declarator']['name']);
      state.heap[state.next] = value.val;
      state.heapinfo[state.next] = { 'type': _.map(node["specifiers"], function(s) { return s['spec']['node']; }), 'name': name, 'node': 'CDecl' };
      state.stack[0][name] = state.next;
      state.next += 1;
   });

   return state;
};

compileDecl["CFunDef"] = function(node, state) {
   var name = unquotify(node["fun_def"]["name"]);
   var fun_type = evalSpecifiers(node['specifiers'], state);
   state.heap[state.next] = node;
   state.stack[0][name] = state.next;

   // Get param info
   var params = node['fun_def']['attrs'][0]['params'];

   var fun_params = {};
   var param_info = _.map(params, function(p) {
      var type = evalSpecifiers(p['specifiers'], state);
      var name = unquotify(p['declarations'][0]['declarator']['name']);
      fun_params[name] = {'node': 'CDecl', type: type};
   });

   state.heapinfo[state.next] = { 'type': fun_type, 'name': name, 'node': 'CFunDef', 'params': fun_params };

   state.next += 1;

   return state;
};

function build_initial_heap(ast) {
   var state = {heap: {}, stack: [{}], next: 0, dump: [], heapinfo: {}};
   var decls = ast["decls"];

   _(decls).map(function(n) {
      return compileDecl[n["node"]](n, state);
   });

   return state;
}
