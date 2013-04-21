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
function sequenceEval(nodes, env) {
   var vals = [];
   var ret;

   for (var i = 0; i < nodes.length; i++) {
      ret = eval(nodes[i], env);
      vals.push(ret.val);
      env = ret.env;
   }

   return {vals: vals, env: env};
};

evalNode = {};
evalNode["CTranslUnit"] = function(node, env) {
   
};
evalNode["CFunDef"] = function(node, env) {

};

evalNode["CMulOp"] = function(node, env) {
   return function(a, b, env) {
      return {val: a * b, env: env};
   };
};
evalNode["CDivOp"] = function(node, env) {
   return function(a, b, env) {
      return {val: a / b, env: env};
   };
};
evalNode["CRmdOp"] = function(node, env) {
   return function(a, b, env) {
      return {val: a % b, env: env};
   };
};
evalNode["CAndOp"] = function(node, env) {
   return function(a, b, env) {
      return {val: a && b, env: env};
   };
};
evalNode["COrOp"] = function(node, env) {
   return function(a, b, env) {
      return {val: a || b, env: env};
   };
};
evalNode["CXorOp"] = function(node, env) {
   return function(a, b, env) {
      return {val: a ^ b, env: env};
   };
};
evalNode["CNeqOp"] = function(node, env) {
   return function(a, b, env) {
      return {val: a !== b, env: env};
   };
};
evalNode["CEqOp"] = function(node, env) {
   return function(a, b, env) {
      return {val: a === b, env: env};
   };
};
evalNode["CGeqOp"] = function(node, env) {
   return function(a, b, env) {
      return {val: a >= b, env: env};
   };
};
evalNode["CLeqOp"] = function(node, env) {
   return function(a, b, env) {
      return {val: a <= b, env: env};
   };
};
evalNode["CLeOp"] = function(node, env) {
   return function(a, b, env) {
      return {val: a < b, env: env};
   };
};
evalNode["CGrOp"] = function(node, env) {
   return function(a, b, env) {
      return {val: a > b, env: env};
   };
};
evalNode["CAddOp"] = function(node, env) {
   return function(a, b, env) {
      return {val: a + b, env: env};
   };
};
evalNode["CSubOp"] = function(node, env) {
   return function(a, b, env) {
      return {val: a -b, env: env};
   };
};

/**
 * eval'ing Expressions
 * CExprs
 */
evalNode["CVar"] = function(node, env) {
   return {val: lookup(node["ident"], env), env: env};
};
evalNode["CConst"] = function(node, env) {
   return {val: eval(node["const"], env), env: env};
};
evalNode["CUnary"] = function(node, env) {
   var val = eval(node["expr"], env);
   var ret;

   switch (node["op"]) {
      case "CNegOp":
         ret = -val.val;
         break;
   }

   return {val: val.val, env: val.env};
};
evalNode["CBinary"] = function(node, env) {
   var vals = sequenceEval(["op", "erand1", "erand2"], env);
   var fun = vals[0],
       erand1 = vals[1],
       erand2 = vals[2];

   if (typeof(fun) !== 'function')
      throw "Not a function";

   return fun(erand1, erand2, env);
};
evalNode["CCall"] = function(node, env) {
   var arg, fun,
       arg_vals = [];

   // fun is likely a CVar, meaning {"val": "function_name", "node": "CVar"}
   fun = eval(node["function"], env);
   env = fun.env;

   for (var i = 0; i < env["args"].length; i++) {
      arg = eval(env["args"][i], env);
      arg_vals.push(arg.val);
      env = arg.env;
   }

   return callFn(fun.val, arg_vals, env);
};
//TODO not sure about handling vars on lhs vs associating them
evalNode["CAssign"] = function(node, env) {
   var lhs = eval(node["lhs"], env);
   var rhs = eval(node["rhs"], lhs.env);
   env = rhs.env;

   var op = assoc(lhs.val, doOp(node["op"], lookup(lhs.val, env), rhs, env));
};

/**
 * eval'ing Statements
 * CStat
 */
evalNode["CExpr"] = function(node, env) {
   return eval(node["expr"], env);
};
evalNode["CWhile"] = function(node, env) {
   var guard = eval(node["guard"], env);
   var next;

   while (isTrue(guard.val)) {
      next = eval(node["next"], guard.env);
      guard = eval(node["guard"], next.env);
   }
   return {val: {}, env: guard.env};
};
evalNode["CIf"] = function(node, env) {
   var ret = eval(node["expr"], env);

   if (isTrue(ret.val))
      return eval(node["true"], expr_ret.env);
   else
      return eval(node["false"], expr_ret.env);
};
evalNode["CFor"] = function(node, env) {
   var init = eval(node["init"], env);
   var guard = eval(node["guard"], init.env);
   var stat;

   while (isTrue(guard.val)) {
      stat = eval(node["next"], guard.env);
      guard = eval(node["guard"], stat.env);
   }
   return {val: {}, env: guard.env};
};
evalNode["CReturn"] = function(node, env) {
   return eval(node["expr"], env);
};

/**
 * Values
 */
evalNode["CCharConst"] = evalNode["CFloatConst"] = evalNode["CStrConst"] = evalNode["CIntConst"] = function(node, env) {
   // we know val is a CInteger, doesn't change env
   return eval(node["val"], env);
};
evalNode["CStr"] = function(node, env) {
   return {val: node["string"], env: env};
};
evalNode["CStrLit"] = function(node, env) {
   return {val: node["string"], env: env};
};
evalNode["CInteger"] = function(node, env) {
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
   return {val: parseInt(node["int"], radix), env: env};
};
evalNode["CChar"] = function(node, env) {
   return {val: node["char"], env: env};
};
evalNode["CChars"] = function(node, env) {
   return {val: node["chars"], env: env};
};
evalNode["CFloat"] = function(node, env) {
   return {val: parseFloat(node["float"]), env: env};
};

function eval(node, env) {
   return evalNode[node["node"], env];
}


