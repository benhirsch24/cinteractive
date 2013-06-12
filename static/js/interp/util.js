// These are utility functions.
define(function() {
   var isArray = function(info) {
      return !(_.isUndefined(info)) && !(_.isUndefined(info["node"])) && info["node"] === "CArrDeclr";
   };

   var getArrayLen = function(info) {
      return _.reduce(info.type, function(sum, t) {
         return sum + t.length;
      }, 0);
   };

   var isSUType = function(type) {
      return !(_.isUndefined(type)) && (type === "CSUType");
   };

   var isStruct = function(info) {
      return !(_.isUndefined(info)) && !(_.isUndefined(info["node"])) && info["node"] === "CCompoundType"
         && info.type.type === "CStruct";
   };

   var getStructSize = function(info) {
      return _(info.type.fields).size();
   };

   var getSMemberOffset = function(member, info) {
      for (var i = 0; i < _(info.type.fields).size(); i++) {
         if (member === info.type.fields[i]['name'])
            return i;
      }
      return -1;
   };

   var unquotify = function(str) {
      if (str.slice(0, 1) !== '"')
         return str;
      str = str.slice(1);
      return str.slice(0, str.length - 1);
   }


   return {
      getArrayLen: getArrayLen,
      isArray: isArray,
      isSUType: isSUType,
      isStruct: isStruct,
      getStructSize: getStructSize,
      getSMemberOffset: getSMemberOffset,
      unquotify: unquotify
   }
});
