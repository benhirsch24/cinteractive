// THIS IS THE STATE OF THE INTERPRETER
var glState;
var glStep;
var dump = [];
var onDump = false;
var done = false;
var fullStepColl = [];
var CM;

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

   controls.append("<h3>Environment Variables</h3>");
   var stack = $('<table></table>');
   stack.attr('id', 'stack');
   var headings = $('<tr></tr>');
   headings.html('<th>Variable</th><th>Address</th>');
   stack.append(headings);

   var num_frames = st.stack.length;
   _(st.stack).map(function(frame, frameIdx){
      var frameLen = _.keys(frame).length;
      var v = 0;
      _(frame).forIn(function(addr, name) {
         var var_row = $('<tr></tr>');
         var_row.addClass('var_row');
         if (v === 0)
            var_row.addClass('frame_top');
         if (v === frameLen - 1)
            var_row.addClass('frame_bottom');

         var varbl = $('<td></td>');
         varbl.addClass('var_name');
         varbl.html(name);
         var adcl = $('<td></td>');
         adcl.addClass('var_addr');
         adcl.html(addr);

         var_row.append(varbl);
         var_row.append(adcl);
         stack.append(var_row);

         v++;
      });
   });
   controls.append(stack);
   controls.append('<hr></hr>');

   controls.append("<h3>Memory Values</h3>");
   var heap = $('<table></table>');
   heap.attr('id', 'heapory');

   var headings = $('<tr></tr>');
   headings.html('<th>Location</th><th>Value</th>');
   heap.append(headings);

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
   
   controls.append(heap);
}

function receiveAST(data) {
   fullStepColl = [];
   done = false;

   $('#ast').html(ipprint(data));

   // TODO: error handling
   if (data["node"] !== "CTranslUnit") {
      $('#functions').html('error');
      return;
   }

   glState = compile(data);
   updateMemory(glState);

   var st = {
      stack: _(glState.stack).clone()
    , heap:  _(glState.heap).clone()
    , kont: _(glState.kont).clone() };
   _(fullStepColl).push(st);

   var newstack = [{}];
   glState.stack = newstack.concat(glState.stack);

   glStep = 0;
   //collectSteps();
   glState.kont($('#whatsgoingon'), CM);
   CM.removeLineClass(currLine, 'wrap', 'active_line');
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
   console.log("Stepping");
   console.log(_.cloneDeep(glState));
   if (_(glState.dump).isEmpty()) {
      if (!_(glState.control).isUndefined()) {
         console.log("Dump is empty, use state.control");
      }
      else {
         $('#whatsgoingon').html('Done!');
         done = true;
         return;
      }
   } else {
      if (!glState.dump.isEmpty() && _(glState.dump).last().length === 0) {
         _(glState.dump).pop();
      }

      if (glState.dump.isEmpty()) {
         $('#whatsgoingon').html('Done!');
         done = true;
         return;
      }
      glState.control = _(glState.dump).last().shift();
   }

   glState = step[glState.control["node"]](glState);
   updateMemory(glState);
   glState.kont($('#whatsgoingon'), CM);
};

function nextStep() {
   if (glStep < fullStepColl.length - 1)
      stepTo(glStep + 1);
};

function prevStep() {
   if (glStep > 0)
      stepTo(glStep -  1);
};

function stepTo(step) {
   glState = fullStepColl[step];
   updateMemory();
   state.kont($('#whatsgoingon'), CM);
   glStep = step;
};

function collectSteps() {
   while (!done) {
      uiStep();
      if (!done) {
         var st = {
            stack: _(glState.stack).clone()
          , heap:  _(glState.heap).clone()
          , kont: _(glState.kont).clone() };
         _(fullStepColl).push(st);
      }
   }

   $('#steps_collected').html('Steps collected!');
   $('#stepper').prop('min', 0);
   $('#stepper').prop('max', fullStepColl.length - 1);
   $('#stepper').change(function() {
      var step = $('#stepper').val();
      var state = fullStepColl[step];
      updateMemory(state);
      state.kont($('#whatsgoingon'), CM);
   });
   glState = fullStepColl[0];
};
