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
      , heap:    state.heap,
        kont: function(ui, cm) {
           ui.html('Main on the stack');
        }
      , next: state.next
      , dump: []
      , ret: undefined
   };
}

function push_params(state) {
   var top_addr = state.next - 1;

   // chain on the params of the function
   // for each CDecl, find the type specifiers and if there's multiple decls
   //   ie int a,b,c;
   // then flatten all these down and reverse them (first arg should be first on stack)
   // finally on each decl name add it to the stack starting from the top address down
   // since the CCall pushed the args into a new stack frame on the heap already
   _(state.control["fun_def"]["attrs"][0]["params"]).map(function(p) {
      var type = evalSpecifiers(p["specifiers"], state);
      var decls = _(p.declarations).map(function(decl) {
         return unquotify(decl["declarator"]["name"]);
      });
      return decls;
   }).flatten().reverse().map(function(decl) {
      state.stack[0][decl] = top_addr;
      top_addr -= 1;
   });
}

var step = {};
step["CThunk"] = function(state) {
   var node = state.control["eval"];
   var line = state.control["line"];
   node["thunk"] = state.control["thunk"];
   node[state.control["thunk"]] = {val: state.ret, state: state};
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
         val = valM.value;
         state = valM.state;
      }
         
      var name = unquotify(decl["declarator"]["name"]);
      names += name;
      var addr = state.next;
      state.heap[addr] = val;
      state.stack[0][name] = addr;
      state.next += 1;
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

   var ops = {};
   ops["CPreIncOp"] = function(u) { msg = 'Pre-incrementing ' + v; state.heap[addr] = u + 1; return u + 1; }
   ops["CPreDecOp"] = function(u) { msg = 'Pre-decrementing ' + v; state.heap[addr] = u - 1; return u - 1; }
   ops["CPostIncOp"] = function(u) { msg = 'Post-incrementing ' + v + ', this takes place after the stmt'; state.heap[addr] = u + 1; return u; }
   ops["CPostDecOp"] = function(u) { msg = 'Post-decrementing ' + v + ', this takes place after the stmt'; state.heap[addr] = u - 1; return u; }
   ops["CAdrOp"] = function(u) { msg = 'Returning address of ' + v; return addr; }
   ops["CIndOp"] = function(u) { msg = 'Returing where ' + v + ' points to.'; return state.heap[addr]; }
   ops["CPlusOp"] = function(u) { return u; } // TODO ignoring these two for now because unary +?
   ops["CMinOp"] = function(u) { return u; }
   ops["CCompOp"] = function(u) { msg = "Returning one's complement of " + v; return ~u; }
   ops["CNegOp"] = function(u) { msg = "Returing negation of " + v; return -u; }

   if (remove) {
      delete state.heap[-1];
   }

   var op = ops[state.control["op"]];
   var ret = op(state.heap[addr]);

   console.log("Popping back from CUnary" + msg);
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
   var control = _(state.control).cloneDeep();
   var line = control['line'];
   var lvalue = eval(state.control["lvalue"], state);

   if (_(state.control["thunk"]).isUndefined()) {
      var rvalue = eval(state.control["rvalue"], lvalue.state);
   } else {
      rvalue = state.control["rvalue"];
      rvalue.state = lvalue.state;
   }
   state = rvalue.state;

   // if the rvalue is a function (ie var = functionCall()) then put a thunk
   // where this node was on the dump
   console.log("rvalue");
   console.log(rvalue.val);
   if (_(rvalue.val).isObject() && rvalue.val["node"] === "CFunDef") {
      var newNode = {};
      newNode["node"] = "CThunk";
      newNode["eval"] = control;
      newNode["thunk"] = "rvalue";
      newNode["line"] = line;
      newNode["rvalue"] = {};
      _(state.dump).last().unshift(newNode);
      _(state.dump).push([rvalue.val]);
      state.kont = function(ui, cm) {
         ui.html('Created thunk on assign');
         hilite_line(line, cm);
      }
      console.log("Created thunk");
      console.log(_(state).cloneDeep());
      return state;
   }

   // Switch on assign ops
   var ops = {};
   ops["CAssignOp"] = function(l, r) { return r; };
   ops["CMulAssOp"] = function(l, r) { return l * r; }
   ops["CDivAssOp"] = function(l, r) { return l / r; }
   ops["CRemAssOp"] = function(l,r) { return l % r; };
   ops["CAddAssOp"] = function(l,r) { return l + r; };
   ops["CSubAssOp"] = function(l,r) { return l - r; };
   ops["CShlAssOp"] = function(l,r) { return l * 2 * r; };
   ops["CShrAssOp"] = function(l,r) { return l / 2 * r; };
   ops["CAndAssOp"] = function(l,r) { return l & r; };
   ops["COrAssOp"] = function(l,r) { return l | r; };
   ops["CXorAssOp"] = function(l,r) { return l ^ r; };

   var op = ops[state.control["op"]];
   var val = op(state.heap[state.stack[0][lvalue.val]], rvalue.val);

   state.control = undefined;
   state.heap[state.stack[0][lvalue.val]] = val;
   state.kont = function(ui, cm) {
      ui.html('Changed var ' + lvalue.val + ' to ' + val);
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
      msg = "Guard was false, continuting";
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

// TODO: rest of specifiers
function evalSpecifiers(specs, state) {
   var type = '';

   _(specs).map(function(s) {
      switch(s['spec']['node']) {
         case 'CUnsigType':
            type += 'unsigned ';
            break;
         case 'CIntType':
            type += 'int ';
            break;
         default:
            type += 'UNKNOWN ';
            break;
      }
   });

   type = type.slice(0, type.length - 1);
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
      state.stack[0][name] = state.next;
      state.next += 1;
   });

   return state;
};

compileDecl["CFunDef"] = function(node, state) {
   state.heap[state.next] = node;
   state.stack[0][unquotify(node["fun_def"]["name"])] = state.next;
   state.next += 1;

   return state;
};

function build_initial_heap(ast) {
   var state = {heap: {}, stack: [{}], next: 0, dump: []};
   var decls = ast["decls"];

   _(decls).map(function(n) {
      return compileDecl[n["node"]](n, state);
   });

   return state;
}
