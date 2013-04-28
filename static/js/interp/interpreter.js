/*
 *
 * eval :: (ASTNode, Env) -> (Value, Env)
 * type Env = (Stack, Memory)
 * type Stack = Map VariableName Addr
 * type Memory = Map Addr Value
 * type VariableName = String
 * type Addr = Int
 * 
 */

// Monadic plumbing, essentially: forM nodes eval
function sequenceEval(nodes, state) {
   var vals = [];
   var ret;

   for (var i = 0; i < nodes.length; i++) {
      ret = eval(nodes[i], state);
      vals.push(ret.val);
      state = ret.state;
   }

   return {vals: vals, state: state};
};

function initialValue(type) {
   var val = 0;

   /*
    * if (type = blah) {
    * }
    *
    */

   return val;
}

function AddGlobalToEnv(name, value, state) {
   state.stack[0][name] = state.next;
   state.heap[state.next] = value;
   state.next = state.next + 1;
   return state;
}

function AddToCurFrame(name, value, state) {
   state.stack[1][name] = state.next;
   state.heap[state.next] = value;
   state.next = state.next + 1;
   return state;
}

function evalLhs(lhs) {
   if (lhs["node"] === "CVar") {
      return unquotify(lhs["name"]);
   }
   return "";
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

var compileDecl = {};
compileDecl["CDecl"] = function(node, state) {
   var type = evalSpecifiers(node["specifiers"]);

   _(node["declarations"]).map(function(d) {
      var name = d['declarator']['name'];
      state = AddGlobalToEnv(name, initialValue(type), state);
   });

   return state;
};

compileDecl["CFunDef"] = function(node, state) {
   return AddGlobalToEnv(node["fun_def"]["name"], node, state);
};

function initEnv(ast) {
   var state = {stack: [{}], heap: {}, next: 0};
   var decls = ast["decls"];

   _(decls).map(function(n) {
      state = compileDecl[n["node"]](n, state);
   });

   return state;
}

evalNode = {};
evalNode["CTranslUnit"] = function(node, state) {
   
};
evalNode["CFunDef"] = function(node, state) {
   
};

evalNode["CMulOp"] = function(node, state) {
   return function(a, b, state) {
      return {val: a * b, state: state};
   };
};
evalNode["CDivOp"] = function(node, state) {
   return function(a, b, state) {
      return {val: a / b, state: state};
   };
};
evalNode["CRmdOp"] = function(node, state) {
   return function(a, b, state) {
      return {val: a % b, state: state};
   };
};
evalNode["CAndOp"] = function(node, state) {
   return function(a, b, state) {
      return {val: a && b, state: state};
   };
};
evalNode["COrOp"] = function(node, state) {
   return function(a, b, state) {
      return {val: a || b, state: state};
   };
};
evalNode["CXorOp"] = function(node, state) {
   return function(a, b, state) {
      return {val: a ^ b, state: state};
   };
};
evalNode["CNeqOp"] = function(node, state) {
   return function(a, b, state) {
      return {val: a !== b, state: state};
   };
};
evalNode["CEqOp"] = function(node, state) {
   return function(a, b, state) {
      return {val: a === b, state: state};
   };
};
evalNode["CGeqOp"] = function(node, state) {
   return function(a, b, state) {
      return {val: a >= b, state: state};
   };
};
evalNode["CLeqOp"] = function(node, state) {
   return function(a, b, state) {
      return {val: a <= b, state: state};
   };
};
evalNode["CLeOp"] = function(node, state) {
   return function(a, b, state) {
      return {val: a < b, state: state};
   };
};
evalNode["CGrOp"] = function(node, state) {
   return function(a, b, state) {
      return {val: a > b, state: state};
   };
};
evalNode["CAddOp"] = function(node, state) {
   return function(a, b, state) {
      return {val: a + b, state: state};
   };
};
evalNode["CSubOp"] = function(node, state) {
   return function(a, b, state) {
      return {val: a -b, state: state};
   };
};

/**
 * eval'ing Expressions
 * CExprs
 */
evalNode["CVar"] = function(node, state) {
   return {val: node["ident"], state: state};
};
evalNode["CConst"] = function(node, state) {
   return eval(node["const"], state);
};
evalNode["CUnary"] = function(node, state) {
   var val = eval(node["expr"], state);
   var ret;

   switch (node["op"]) {
      case "CNegOp":
         ret = -val.val;
         break;
   }

   return {val: val.val, state: val.state};
};
evalNode["CBinary"] = function(node, state) {
   var vals = sequenceEval(["op", "erand1", "erand2"], state);
   var fun = vals[0],
       erand1 = vals[1],
       erand2 = vals[2];

   if (typeof(fun) !== 'function')
      throw "Not a function";

   return fun(erand1, erand2, state);
};
evalNode["CCall"] = function(node, state) {
   var arg, fun,
       arg_vals = [];

   // fun is likely a CVar, meaning {"val": "function_name", "node": "CVar"}
   fun = eval(node["function"], state);
   state = fun.state;

   for (var i = 0; i < state["args"].length; i++) {
      arg = eval(state["args"][i], state);
      arg_vals.push(arg.val);
      state = arg.state;
   }

   return callFn(fun.val, arg_vals, state);
};
//TODO not sure about handling vars on lhs vs associating them
evalNode["CAssign"] = function(node, state) {
   var vals = sequenceEval([ node["lvalue"], node["rvalue"] ], state);
   var name = vals.val[0];
   var rhs = vals.val[1];
   state = vals.state;
   
   // Switch on assign ops
   var ops = {};
   ops["CAssignOp"] = function(lhs, rhs) {
      return rhs;
   };
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

   var op = ops[node["ops"]];
   var val = op(state.heap[name], rhs);
   state.heap[name] = val;
   return {val: val, state: state};
};

/**
 * eval'ing Statements
 * CStat
 */
evalNode["CExpr"] = function(node, state) {
   return eval(node["expr"], state);
};
evalNode["CWhile"] = function(node, state) {
   var guard = eval(node["guard"], state);
   var next;

   while (isTrue(guard.val)) {
      next = eval(node["next"], guard.state);
      guard = eval(node["guard"], next.state);
   }
   return {val: {}, state: guard.state};
};
evalNode["CIf"] = function(node, state) {
   var ret = eval(node["expr"], state);

   if (isTrue(ret.val))
      return eval(node["true"], expr_ret.state);
   else
      return eval(node["false"], expr_ret.state);
};
evalNode["CFor"] = function(node, state) {
   var init = eval(node["init"], state);
   var guard = eval(node["guard"], init.state);
   var stat;

   while (isTrue(guard.val)) {
      stat = eval(node["next"], guard.state);
      guard = eval(node["guard"], stat.state);
   }
   return {val: {}, state: guard.state};
};
evalNode["CReturn"] = function(node, state) {
   return eval(node["expr"], state);
};

evalNode["CInit"] = function(node, state) {
   return eval(node["assignment"], state);
};

/**
 * Values
 */
evalNode["CCharConst"] = evalNode["CFloatConst"] = evalNode["CStrConst"] = evalNode["CIntConst"] = function(node, state) {
   // we know val is a CInteger, doesn't change state
   return eval(node["val"], state);
};
evalNode["CStr"] = function(node, state) {
   return {val: node["string"], state: state};
};
evalNode["CStrLit"] = function(node, state) {
   return {val: node["string"], state: state};
};
evalNode["CInteger"] = function(node, state) {
   var radix;
   switch (node["base"]) {
      case "d":
         radix = 10;
         break;
      case "o":
         radix = 8;
         break;
      case "x":
         radix = 16;
         break;
   }
   return {val: parseInt(node["int"], radix), state: state};
};
evalNode["CChar"] = function(node, state) {
   return {val: node["char"], state: state};
};
evalNode["CChars"] = function(node, state) {
   return {val: node["chars"], state: state};
};
evalNode["CFloat"] = function(node, state) {
   return {val: parseFloat(node["float"]), state: state};
};

function eval(node, state) {
   return evalNode[node["node"]](node, state);
}


