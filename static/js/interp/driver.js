// THIS IS THE STATE OF THE INTERPRETER
var glState;
var dump = [];
var onDump = false;
var done = false;
var fullStepColl = [];

function fun_args(fun) {
   var ret = '<div id="args">';
   ret += '<ul class="args">';
   _(fun["attrs"][0]["params"]).map(function(p) {
      ret += '<li class="arg">';
      ret += print_type(p["specifiers"][0]["spec"]["node"]);
      ret += "&nbsp;&nbsp;&nbsp;";
      ret += "<input type='text' class='param' value='";
      ret += unquotify(p["declarations"][0]['declarator']["name"]);
      ret += "'>";
      ret += '</li>';
   });
   ret += '</ul>';
   ret += '</div>';
   return ret;
}

function updateMemory(st) {
   var controls = $('#controls');
   controls.empty();

   var stack = $('<table></table>');
   stack.attr('id', 'stack');

   var num_frames = st.stack.length;
   _(st.stack).map(function(frame, frameIdx){
      _(frame).forIn(function(addr, name) {
         var var_row = $('<tr></tr>');
         var_row.addClass('var_row');
         if (frameIdx === num_frames - 1)
         var_row.addClass('global');

      var varbl = $('<td></td>');
      varbl.addClass('var_name');
      varbl.html(name);
      var adcl = $('<td></td>');
      adcl.addClass('var_addr');
      adcl.html(addr);

      var_row.append(varbl);
      var_row.append(adcl);
      stack.append(var_row);
      });
   });

   var heap = $('<table></table>');
   heap.attr('id', 'heapory');

   _(st.heap).forIn(function(value, addr) {
      var tr = $('<tr></tr>');
      tr.addClass('heap_row');
      
      var loc = $('<td></td>');
      loc.addClass('heap_loc');
      var node = $('<td></td>');
      node.addClass('heap_node');

      loc.html(addr);
      node.html(ppMemValue(value));

      tr.append(loc);
      tr.append(node);
      heap.append(tr);
   });
   
   controls.append(stack);
   controls.append('<hr></hr>');
   controls.append(heap);
}

function receiveAST(data) {
   $('#source').html(ipprint(data));
   fullStepColl = [];
   done = false;

   // TODO: error handling
   if (data["node"] !== "CTranslUnit") {
      $('#functions').html('error');
      return;
   }

   glState = compile(data);
   glState.kont($('#whatsgoingon'));
   updateMemory(glState);
   var newstack = [{}];
   glState.stack = newstack.concat(glState.stack);
}

function enlarge() { $('#source').addClass('enlarge'); }

/**
 * type InstructionPointer = Int
 * Context :: Context [Expr] InstructionPointer
 * Dump :: [Context]
 * 
 * Use: Any array construct (ie C block) in state.control is added to the dump. The IP 
 * is the current index used to pop up out of the end of a block
 */

function uiStep() {
   if (_(glState.dump).isEmpty()) {
      if (!_(glState.control).isUndefined()) {
         console.log("Dump is empty, use state.control");
      }
      else {
         $('#whatsgoingon').html('Done!');
         done = true;
         return;
      }
      // don't do anything?
   } else {
      if (!glState.dump.isEmpty() && _(glState.dump).last().length === 0) {
         _(glState.dump).pop();
      }
      if (glState.dump.isEmpty()) {
         $('#whatsgoingon').html('Done!');
         return;
      }
      glState.control = _(glState.dump).last().shift();
   }

   glState = step[glState.control["node"]](glState);
   updateMemory(glState);
   glState.kont($('#whatsgoingon'));
};

function collectSteps() {
   var stepColl = [];
   while (!done) {
      uiStep();
      var st = {
         stack: _(glState.stack).clone()
       , heap:  _(glState.heap).clone()
       , kont: _(glState.kont).clone() };
      _(stepColl).push(st);
   }

   $('#steps_collected').html('Steps collected!');
   fullStepColl = stepColl;
   $('#stepper').prop('min', 0);
   $('#stepper').prop('max', stepColl.length);
   $('#stepper').change(function() {
      var step = $('#stepper').val();
      var state = stepColl[step];
      updateMemory(state);
      state.kont($('#whatsgoingon'));
   });
};
