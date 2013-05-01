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
   };
}

function push_params(state) {
   var top_addr = state.next - 1;

   // chain on the params of the function
   // for each CDecl, find the specifiers (type) and if there's multiple decls
   //   ie int a,b,c;
   // then flatten all these down and reverse them
   // finally on each decl name add it to the stack starting from the top address down
   // since the CCall pushed the args into a new stack frame already
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
step["CFunDef"] = function(state) {
   var name = state.control['fun_def']['name'];
   stack = push_params(state);
   state.kont = function(ui) {
      ui.html('Defining Function: ' + unquotify(name));
   };
   state.control = state.control["statements"];
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

   state.kont = function(ui) {
      ui.html('Declared: ' + names);
   };

   return state;
};
step["CExpr"] = function(state) {
   state.control = state.control["expr"]
   console.log("Nevermind, stepping to " + state.control["node"])
   return step[state.control["node"]](state);
};
step["CAssign"] = function(state) {
   console.log(state.control);
   var lvalue = eval(state.control["lvalue"], state);
   console.log("rvalue");
   var rvalue = eval(state.control["rvalue"], lvalue.state);
   state = rvalue.state;

   state.heap[state.stack[0][lvalue.val]] = rvalue.val;
   state.kont = function(ui) {
      ui.html('Changed var ' + lvalue.val + ' to ' + rvalue.val);
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
   state.control = state.control["block_items"];
   state.kont = function(ui) {
      ui.html('Compound statements');
   };
   return state;
}

step["CIf"] = function(state) {
   var guard = eval(state.control["guard"], state);
   state = guard.state;
   var happ = '';

   if (guard.val === 0) {
      state.control = state.control["false"];
      happ = 'Condition was false.';
   } else {
      state.control = state.control["true"];
      happ = 'Condition was true.';
   }

   state.kont = function(ui) {
      ui.html(happ);
   };

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
