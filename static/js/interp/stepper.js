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

function compile(ast) {
   var state = build_initial_heap(ast);
   
   return {
        control: state.heap[state.stack[0]["main"]]
      , stack:   state.stack
      , heap:    state.heap,
        kont: function(ui) {
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
   node["thunk"] = state.control["thunk"];
   node[state.control["thunk"]] = {val: state.ret, state: state};
   state.kont = function(ui) {
      ui.html('Executing thunk, filling hole ' + node["thunk"]);
   };
   state.ret = undefined;
   state.control = undefined;
   _(state.dump).last().unshift(node);
   return state;
};
step["CFunDef"] = function(state) {
   var name = state.control['fun_def']['name'];
   stack = push_params(state);
   state.kont = function(ui) {
      ui.html('Calling Function: ' + unquotify(name));
   };
   state.dump = _(state.dump).push(_(state.control["statements"]["block_items"]).clone());
   state.control = undefined;
   return state;
}
step["CDecl"] = function(state) {
   var type = evalSpecifiers(state.control["specifiers"], state);
   var names = '';

   _(state.control["declarations"]).map(function (decl) {
      var val;
      if (_(decl).has('initializer') && decl['initializer'] !== null) {
         val = eval(decl['initializer'], state);
         state = val.state;
         val = val.val;
      } else {
         val = initial_value(type, state);
      }
         
      var name = unquotify(decl["declarator"]["name"]);
      names += name;
      var addr = state.next;
      state.heap[addr] = val;
      state.stack[0][name] = addr;
      state.next += 1;
   });

   state.control = undefined;
   state.kont = function(ui) {
      ui.html('Declared: ' + names);
   };

   return state;
};
step["CExpr"] = function(state) {
   state.control = state.control["expr"]
   return step[state.control["node"]](state);
};
step["CAssign"] = function(state) {
   var control = _(state.control).clone();
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
      newNode["rvalue"] = {};
      _(state.dump).last().unshift(newNode);
      _(state.dump).push([rvalue.val]);
      state.kont = function(ui) {
         ui.html('Created thunk on assign');
      }
      console.log("Created thunk");
      console.log(_(state).clone());
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
   state.kont = function(ui) {
      ui.html('Changed var ' + lvalue.val + ' to ' + val);
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
   // idents?
   state.dump = _(state.dump).push(state.control["block_items"]);
   state.kont = function(ui) {
      ui.html('Compound statements');
   };
   state.control = undefined;
   return state;
}

step["CIf"] = function(state) {
   var guard = eval(state.control["guard"], state);
   state = guard.state;
   var happ = '';

   if (guard.val === 0) {
      _(state.dump).last().unshift(state.control["false"]);
      happ = 'Condition was false.';
   } else {
      _(state.dump).last().unshift(state.control["true"]);
      happ = 'Condition was true.';
   }

   state.kont = function(ui) {
      ui.html(happ);
   };
   state.control = undefined;

   return state;
};

step["CReturn"] = function(state) {
   var ret = eval(state.control, state);

   if (_(ret.val).isString())
      ret.val = state.heap[varLookup(ret.val, state.stack)];

   state = ret.state;
   _(state.stack[0]).forIn(function(addr, name) {
      delete state.heap[name];
   });
   _(state.stack).shift();
   state.kont = function(ui) {
      ui.html('Returning val: ' + ret.val);
   };
   state.ret = ret.val;
   state.control = undefined;
   state.dump.pop();
   return state;
};


function initial_value(type) {
   var val = 0;

   /*
    * if (type = blah) {
    * }
    *
    */

   return val;
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
      var name = unquotify(d['declarator']['name']);
      state.heap[state.next] = initial_value(d, type);
      state.stack[0][name] = state.next;
      state.next += 1;
   });

   return state;
};

var compileDecl = {};
compileDecl["CDecl"] = function(node, state) {
   var type = evalSpecifiers(node["specifiers"], state);

   _(node["declarations"]).map(function(d) {
      var name = unquotify(d['declarator']['name']);
      state.heap[state.next] = initial_value(d, type);
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
   var state = {heap: {}, stack: [{}], next: 0};
   var decls = ast["decls"];

   _(decls).map(function(n) {
      return compileDecl[n["node"]](n, state);
   });

   return state;
}
