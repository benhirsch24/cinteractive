define(["ui/templates", "interp/util"], function(templates, util) {
   function unquotify(str) {
      str = str.slice(1);
      return str.slice(0, str.length - 1);
   }

   var ppFrame = function(state, frameIdx) {
      var frame = state.frames[frameIdx];
      var params = [];
      _.forEach(frame.params, function(param, name) {
         _(params).push({name: name, type: param.type});
      });

      // this is each variable in the frame
      var stack_frame = [];
      _.forEach(state.stack[frameIdx], function(addr, name) {
         if (util.isArray(state.heapinfo[addr])) {
            _(stack_frame).push({name: name, addr: addr, value: undefined, info: state.heapinfo[addr], isCpound: true, isKey: true});
            _.range(0, util.getArrayLen(state.heapinfo[addr])).map(function(i) {
               _(stack_frame).push({name: name, addr: addr + i, value: state.heap[addr + i], info: state.heapinfo[addr], offset: i, isKey: false, isCpound: true});
            });
         } else if (util.isStruct(state.heapinfo[addr])) {
            _(stack_frame).push({name: name, addr: addr, value: undefined, info: state.heapinfo[addr], isCpound: true, isKey: true});
            _.range(0, util.getStructSize(state.heapinfo[addr])).map(function(i) {
               _(stack_frame).push({name: name, addr: addr + i, value: state.heap[addr + i], info: state.heapinfo[addr], offset: i, isKey: false, isCpound: true});
            });
         } else {
            _(stack_frame).push({name: name, addr: addr, value: state.heap[addr], info: state.heapinfo[addr]});
         }
      });

      var tbody = templates.frame({name: frame.name, params: params}, stack_frame);
      return tbody;
   };

   var ppHeap = function(state) {
      var heap = '';
      var cp_ty, cp_len = 0;

      _(state.heap).map(function(value, addr) {
         if (cp_len === 0 && !_.isUndefined(state.heapinfo[addr]) && 
               (state.heapinfo[addr].node === 'CArrDeclr' || state.heapinfo[addr].node === "CCompoundType")) {
            if (state.heapinfo[addr].node === 'CArrDeclr') {
               cp_len = _.reduce(state.heapinfo[addr].type, function(sum, t) {
                  return sum + t.length;
               }, 0);
            } else if (state.heapinfo[addr].node === 'CCompoundType') {
               cp_len = util.getStructSize(state.heapinfo[addr]);
            }
            cp_ty = state.heapinfo[addr];
         }

         if (cp_len > 0) {
            heap += templates.heap_entry({addr: addr, value: value, info: cp_ty});
            cp_len -= 1;
         } else if (_.isUndefined(state.heapinfo[addr])) {
            heap += templates.heap_entry({addr: addr, value: value, info: undefined});
         } else {
            heap += templates.heap_entry({addr: addr, value: value, info: state.heapinfo[addr]});
         }
      });

      return heap;
   };

   var update = function(e, state, frameIdx) {
      var frame = ppFrame(state, frameIdx);
      $(e.currentTarget).empty();

      $(e.currentTarget).html(frame);
      return frame;
   };

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

   return {
      unquotify: unquotify,
      ipprint: ipprint,
      ppFrame: ppFrame,
      ppHeap:  ppHeap,
      update: update
   }
});
