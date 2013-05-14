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

var sizes = {};
sizes["CVoidType"] = 1;
sizes["CCharType"] = 1;
sizes["CShortType"] = 2;
sizes["CIntType"] = 4;
sizes["CLongType"] = 4;
sizes["CFloatType"] = 4;
sizes["CDoubleType"] = 8;
sizes["CSignedType"] = 2;
sizes["CUnsigType"] = 4;
sizes["CBoolType"] = 1;

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

function varLookup(varname, stack) {
   var locals = stack[0],
       globals = _(stack).last();

   if (!_(locals[varname]).isUndefined())
      return locals[varname];
   else if (!_(globals[varname]).isUndefined())
      return globals[varname];
   else
      return undefined;
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

// returns an ADDRESS IN THE HEAP
function evalLhs(lhs, state) {
   var val = undefined;

   if (lhs["node"] === "CVar") {
      val = varLookup(unquotify(lhs["name"]), state.stack);
   } else if (lhs["node"] === "CIndex") {
      var index = eval(lhs["index"], state);
      var array = eval(lhs["array"], index.state);
      state = array.state;

      var ind_addr = state.heap[varLookup(index.val, state.stack)];
      var array_addr = varLookup(array.val, state.stack);

      val = array_addr + ind_addr;
   }

   return {val: val, state: state};
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
   return {val: node, state: state};
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
   return {val: unquotify(node["name"]), state: state};
};
evalNode["CConst"] = function(node, state) {
   return eval(node["const"], state);
};
evalNode["CUnary"] = function(node, state) {
   var val = eval(node["expr"], state);
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
   ops["CPreIncOp"] = function(u) { state.heap[addr] = u + 1; return u + 1; }
   ops["CPreDecOp"] = function(u) { state.heap[addr] = u - 1; return u - 1; }
   ops["CPostIncOp"] = function(u) { state.heap[addr] = u + 1; return u; }
   ops["CPostDecOp"] = function(u) { state.heap[addr] = u - 1; return u; }
   ops["CAdrOp"] = function(u) { return addr; }
   ops["CIndOp"] = function(u) { return state.heap[addr]; }
   ops["CPlusOp"] = function(u) { return u; } // TODO ignoring these two for now because unary +?
   ops["CMinOp"] = function(u) { return u; }
   ops["CCompOp"] = function(u) { return ~u; }
   ops["CNegOp"] = function(u) { return -u; }

   if (remove) {
      delete state.heap[-1];
   }

   return {val: ret, state: val.state};
};
evalNode["CBinary"] = function(node, state) {
   var erand1 = eval(node["erand1"], state),
       erand2 = eval(node["erand2"], erand1.state);
   state = erand2.state;

   if (_(erand1.val).isString())
      erand1.val = state.heap[varLookup(erand1.val, state.stack)];
   if (_(erand2.val).isString())
      erand2.val = state.heap[varLookup(erand2.val, state.stack)];

   var ops = {};
   ops["CMulOp"] = function(l, r) { return l * r; }
   ops["CDivOp"] = function(l, r) { return l / r; }
   ops["CRmdOp"] = function(l,r) { return l % r; };
   ops["CAddOp"] = function(l,r) { return l + r; };
   ops["CSubOp"] = function(l,r) { return l - r; };
   ops["CShlOp"] = function(l,r) { return l * 2 * r; };
   ops["CShrOp"] = function(l,r) { return l / 2 * r; };
   ops["CAndOp"] = function(l,r) { return l & r; };
   ops["COrAssOp"] = function(l,r) { return l | r; };
   ops["CXorAssOp"] = function(l,r) { return l ^ r; };
   ops["CLndOp"] = function(l,r) { return l && r; };
   ops["CLorOp"] = function(l,r) { return l || r; };
   ops["CLeOp"] = function(l,r) { return l < r; };
   ops["CGrOp"] = function(l,r) { return l > r; };
   ops["CLeqOp"] = function(l,r) { return l <= r; };
   ops["CGeqOp"] = function(l,r) { return l >= r; };
   ops["CEqOp"] = function(l,r) { return l === r; };
   ops["CNeqOp"] = function(l,r) { return l !== r; };

   var val = ops[node["op"]](erand1.val, erand2.val);

   return {val: val, state: erand2.state};
};
evalNode["CCall"] = function(node, state) {
   var arg, fun,
       arg_vals = [],
       fun_name = unquotify(node["function"]["name"]);
   var fun_addr;

   console.log("Calling " + fun_name);
   // functions should be defined in globals which is last stack frame
   fun_addr = _(state.stack).last()[fun_name];
   if (_(fun_addr).isUndefined()) {
      console.log("No function defined");
      state.kont = function(ui) {
         ui.html("Error in CCall");
      }
      return {val: undefined, state: state};
   }

   // fun is likely a CVar, meaning {"val": "function_name", "node": "CVar"}
   fun = eval(state.heap[fun_addr], state);
   state = fun.state;

   var evalArgument = function(arg, state) {
      var argval = undefined;
      if (arg["node"] === "CVar") {
         argval = eval(arg, state);
         var varname = argval.val;
         var addr = varLookup(varname, state.stack);
         argval.val = state.heap[addr];
         state.heapinfo[addr] = state.heapinfo[fun_addr]['params'][varname];
      } else if (arg["node"] === "CConst") {
         argval = eval(arg["node"], state);
      }
      return argval;
   };

   for (var i = 0; i < node["args"].length; i++) {
      arg = evalArgument(node["args"][i], state);
      arg_vals.push(arg.val);
      state = arg.state;
      state.heap[state.next] = arg.val;
      state.next += 1;
   }

   _(state.stack).unshift({});
   state.control = fun.val;
   state.kont = function(ui) {
      ui.html('About to call ' + fun_name);
   };
   return {val: fun.val, state: state};
};
//TODO not sure about handling vars on lhs vs associating them
evalNode["CAssign"] = function(node, state) {
   var lhs = evalLhs(node["lvalue"], state);
   var rval = eval(node["rvalue"], lhs.state);
   var addr = lhs.val;
   var rhs = rval.val;
   state = rval.state;
   
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

   var op = ops[node["op"]];

   var val = op(state.heap[addr], rhs);
   state.heap[addr] = val;

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
   console.log(node["node"]);
   if (_.isUndefined(evalNode[node["node"]]))
      throw "evalNode " + node["node"] + " not defined.";
   return evalNode[node["node"]](node, state);
}


