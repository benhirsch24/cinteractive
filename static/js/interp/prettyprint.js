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
   var ident = _.range(level).map(function(){return "&nbsp;&nbsp;&nbsp;"}).reduce(ccat, '');
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
         var values = _(value).map(function(o){ return pprint(o, level + 1); }).reduce(ccat, '');
         writer += ident + key + "[]: " + br;
         writer += values;
      }
      else if (_(value).isObject()) {
         var owriter = pprint(value, level + 1);
         writer += ident + key + "{}: " + br;
         writer += owriter;
      }
   });

   return writer;
}

function ipprint(ast) {
   return pprint(ast, 0);
}
