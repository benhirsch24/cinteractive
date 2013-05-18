function unquotify(str) {
   str = str.slice(1);
   return str.slice(0, str.length - 1);
}

function print_type(type) {
   var ret;

   switch (type) {
      case "CIntType":
         ret = "int";
         break;
      case "CCharType":
         ret = "char";
         break;
      case "CFloatType":
         ret = "float";
         break;
      default: ret = type;
   }

   return ret;
}

function pprint(ast, level) {
   var ccat = function(w, s) { return w + s; }
   var ident = _.range(level).map(function(){return "..."}).reduce(ccat, '');
   var br = "<br>",
       si = "<strong>",
       sc = "</strong>";


   var writer = ident + si + ast["node"] + sc + br;
   _(ast).forIn(function(value, key) {
      if (key === "node")
         return "";

      if (_(value).isString())
         writer += ident + key + ": " + value + br;
      else if (_(value).isArray()) {
         var values = _(value).map(function(o){ return "# " + pprint(o, level + 1); }).reduce(ccat, '');
         writer += ident + key + ":[ " + br;
         writer += values;
         writer += ident + "]" + br;
      }
      else if (_(value).isObject()) {
         var owriter = pprint(value, level + 1);
         writer += ident + key + ":{ " + br;
         writer += owriter;
         writer += ident + "}" + br;
      }
   });

   return writer;
}

function ipprint(ast) {
   return pprint(ast, 0);
}

function ppType(types, v) {
   var type = '';
   var ts = _.isArray(types) ? types : types.valueOf();

   var show_type = {};
   show_type["CVoidType"] = "void";
   show_type["CCharType"] = "char";
   show_type["CShortType"] = "short";
   show_type["CIntType"] = "int";
   show_type["CLongType"] = "long";
   show_type["CFloatType"] = "float";
   show_type["CDoubleType"] = "double";
   show_type["CSignedType"] = "signed";
   show_type["CUnsigType"] = "unsigned";
   show_type["CBoolType"] = "bool";

   _.map(ts, function(t) {
         type += show_type[t] + ' ';
   });
   return type;
}

function ppParams(params) {
   var ret = '';

   _.forIn(params, function(info, name) {
      ret += ppType(info['type']) + ' ' + name + ', ';
   });

   ret = ret.slice(0, ret.length - 2);
   return ret;
}

function ppMemValue(value, info) {
   var type = '';
   if (!_.isUndefined(info) && !_.isUndefined(info["type"])) {
      type = ppType(info['type'], value);
   }

   if (_.isObject(value)) {
      switch(value["node"]) {
         case "CFunDef":
            return unquotify(value["fun_def"]["name"]) + " :: Function (" + ppParams(info['params']) + ")";
         case "CDecl":
            return info["name"] + " :: " + type;
      }
   } else if (_.isArray(info)) {
      _.map(info, function(i) {
         var t = ppType(i['type']);
         switch(i["node"]) {
            case "CArrDeclr":
               //var theType = '';
               //_.map(i['type'], function(t){ theType += t + ' '; });
               type += 'Array of ' + i["length"] + " " + t + 's ';
               break;
            case "CPtrDeclr":
               type += ' pointer to ';
               break;
            case "CFunDeclr":
               type += ' function returning ';
               break;
         }
      });

      return type;
   } else {
      return value + ' :: ' + type;
   }
}
