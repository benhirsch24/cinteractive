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

evalNode = {};
evalNode["CTranslUnit"] = function(node, env) {
   
};
evalNode["CFunDef"] = function(node, env) {

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
         ret = val.val;
         break;
   }

   return {val: val.val, env: val.env};
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

function eval(node, env) {
   evalNode[node["node"], env];
}

