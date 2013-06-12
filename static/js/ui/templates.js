define(function() {

   function ppType(types, isKey, offset) {
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

      if (_.isArray(ts)) {
         // For each type in this array of types, if it's not an object (just a string like CVoidType)
         // then use the table above to get a prettier name for it.
         // Otherwise if it's an array get some niceties like int [length] for the actual variable or for
         // each individual entry int [0], int [1], etc.
         _.map(ts, function(t) {
            if (!_.isObject(t)) {
               type += show_type[t] + ' ';
            } else 
            {
               if (t.node === "CArrDeclr") {
                  if (isKey)
                     type += ppType(t.type) + '[' + t.length + ']';
                  else
                     type += ppType(t.type) + '[' + offset + ']';
               }
            }
         });
      } else if (_.isObject(ts) && ts.type === "CStruct") 
      {
         if (isKey) {
            return 'struct ' + ts.name;
         } else 
         {
            var field_type = ppType(ts.fields[offset].type);
            var field_name = ts.fields[offset]['name'];
            return field_type + ' .' + field_name;
         }
      }
      return type;
   }

   Handlebars.registerHelper('params', function(params) {
      var str = ''
      var i = 0;
      var num_params = _(params).size();

      if (_.isEmpty(params))
         return 'void';

      for (; i < num_params; i++) {
         str += ppType(params[i].type) + params[i].name;
         if (i < num_params - 1)
            str += ', ';
      }

      return str;
   });

   Handlebars.registerPartial('signature', '{{frame.name}}({{params frame.params}})');

   Handlebars.registerHelper('print_type', function(type, isKey, offset) {
      return ppType(type, isKey, offset);
   });

   // Prints a single value in the heap.
   Handlebars.registerHelper('heapval', function(val, info) {
      var str_templ, templ, context = {};

      switch(info["node"]) {
         case "CFunDef":
            str_templ = "Function of ({{params params}})";

            var params = [];
            _.forEach(info.params, function(param, name) {
               _(params).push({name: name, type: param.type});
            });
            context = {params: params};
            break;
         case "CArrDeclr":
            // if it's just an array, just an array
            if (!(_.isUndefined(val))) {
               return val;
            }
            // otherwise give a link to expand/collapse
            str_templ = "<a href='#' class='compound_key' data-compound='{{name}}'>Expand/Collapse Array</a>";
            context = {name: info.name};
            break;
         case "CDecl":
            return val;
         case "CElemType":
            return val;
         case "CCompoundType":
            if (!(_.isUndefined(val)))
               return val;

            str_templ = "<a href='#' class='compound_key' data-compound='{{name}}'>Expand/Collapse Struct</a>";
            context = {name: info.name};
            break;
      };
      templ = Handlebars.compile(str_templ);

      return templ(context);
   });

   Handlebars.registerHelper('stackvars', function(vars, options) {
      var ret = '';
      for (var i = 0; i < vars.length; i++) {
         ret += options.fn(vars[i]);
      }
      return ret;
   });

   var frame = function(frame, stack_frame) {
      var str = "<table class='table table-bordered table-striped'><caption>{{> signature}}</caption>";

      var vars = "{{#stackvars stack_frame}}";
         vars += "<tr class='frame_var' {{#if isCpound}} {{#unless isKey}}data-var='{{info.name}}'{{/unless}}{{/if}} style='{{#if isCpound}}{{#unless isKey}} display:none {{/unless}}{{/if}}'>";
         vars += "<td class='var_addr'><a href='#' data-addr='{{addr}}'>{{addr}}</a></td>";
         vars += "<td class='variable'>{{print_type info.type isKey offset}} {{name}}</td>";
         vars += "<td class='value'>{{{heapval value info}}}</td></tr>{{/stackvars}}";

      var end = "</table>";
      var templ = str + vars + end;

      var template = Handlebars.compile(templ);
      var frame_templ = template({stack_frame: stack_frame, frame: frame});

      return frame_templ;
   };

   // Basically on function calls we enter the data in the heap, but the heapinfo isn't
   // filled in until the actual CFunDef node is evaluated. Just print it then.
   var heap_entry = function(entry) {
      if (_.isUndefined(entry.info))
         return '';

      var templ_str = "<tr class='heap_entry'>";
      templ_str += "<td class='addr'>{{addr}}</td>";
      templ_str += "<td class='value'>{{{heapval value info}}}</td>";
      templ_str += "</tr>";

      var template = Handlebars.compile(templ_str);
      return template(entry);
   };

   return {
      frame: frame,
      heap_entry: heap_entry
   };
});
