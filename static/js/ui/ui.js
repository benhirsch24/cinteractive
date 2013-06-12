define(["interp/stepper", "ui/prettyprint"], function(stepper, prettyprint) { return function(CM) {
   var numFrames = 0;
   var curLine = 0;

   // This handles all the logic to update the memory UI.
   var updateMemory = function(state) {
      var curFrames = state.stack.length;

      // If there's a new frame make a nice sliding motion.
      if (curFrames > numFrames) {
         var frame = $('<li class="frame">' + prettyprint.ppFrame(state, 0) + '</li>');
         frame.hide();
         $('#frames').prepend(frame);
         frame.slideDown();
      }
      else if (curFrames < numFrames) 
      {
         // If we popped back from a function the slide the frame back up.
         $('#frames li:first').slideUp();
         $('#frames li:first').remove();
      }

      var frames = $('#frames > li');
      _(state.stack).map(function(frame, frameIdx) {
         $(frames[frameIdx]).trigger('state_update', [state, frameIdx]);
      });

      $('#heap tbody').empty();
      $('#heap tbody').html(prettyprint.ppHeap(state));
   }

   var uiStep = function(state) {
      updateMemory(state);
      state.kont(kont);

      numFrames = _(state.stack).size();
   };

   var kont = {
      tell: function(msg) {
         $('#whatsgoingon').html(msg + '<br>');
      },
      hilite_line: function(line) {
         CM.removeLineClass(curLine, 'wrap', 'active_line');
         CM.addLineClass(line - 1, 'wrap', 'active_line');
         curLine = line - 1;
      }
   };

   return {
      uiStep: uiStep,
      kont: kont
   };
}});
