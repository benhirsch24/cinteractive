requirejs(["ui/ui", "ui/prettyprint", "interp/stepper", "ui/tooltips"], function(ui, prettyprint, stepper, tooltips) {
   var currLine = 0;
   var glState = {},
       glStepColl = [],
       glDone = false;

   // First create the global CodeMirror object, set up any options
   var CM = CodeMirror.fromTextArea(document.getElementById("code"), {
      indentUnit: 3,
      smartIndent: true,
      tabSize: 3,
      lineWrapping: true,
      lineNumbers: true,
      theme: 'neat',
      autoCloseBrackets: true
   });
   CM.addKeyMap({
      'Shift-Ctrl-f': function(cm) {
         nextStep();
      }
   });

   var receiveAST = function(data) {
      var stepColl = [];
      var initial_state = {};

      $('#ast').html(prettyprint.ipprint(data));

      initial_state = stepper.compile(data);

      var st = {
         stack:    _.cloneDeep(initial_state.stack)
       , heap:     _.cloneDeep(initial_state.heap)
       , frames:     _.cloneDeep(initial_state.frames)
       , user_types: _.cloneDeep(initial_state.user_types)
       , heapinfo: _.cloneDeep(initial_state.heapinfo)
       , kont:     _.cloneDeep(initial_state.kont) };
      _(stepColl).push(st);


      return { state: initial_state, stepColl: stepColl };
   };

   var doStep = function(state, done) {
      if (done) { return {state: state, done: done}; }

      console.log("Stepping");
      console.log(_.cloneDeep(state));
      // The dump is the instruction stack essentially,
      // If it's empty then there better be something pointed to by the control
      // otherwise it's got to be done.
      if (_.isEmpty(state.dump)) {
         if (!_.isUndefined(state.control)) {
            console.log("Dump is empty, use state.control");
         }
         else {
            $('#whatsgoingon').html('Done!');
            done = true;
            return {state: state, done: done};
         }
      } else {
         if (!state.dump.isEmpty() && _(state.dump).last().length === 0) {
            _(state.dump).pop();
         }

         if (state.dump.isEmpty()) {
            $('#whatsgoingon').html('Done!');
            done = true;
            return {state: state, done: done};
         }
         state.control = _(state.dump).last().shift();
      }

      state = stepper.step[state.control["node"]](state);
      return {state: state, done: done};
   };

   var collectSteps = function(state, stepColl) {
      var done = false;
      glDone = false;

      while (!done) {
         var s = doStep(state, done);
         console.log(s);
         state = s.state;
         done = s.done;
         if (!done) {
            var st = {
               stack:    _.cloneDeep(state.stack)
             , heap:     _.cloneDeep(state.heap)
             , frames:     _.cloneDeep(state.frames)
             , user_types: _.cloneDeep(state.user_types)
             , heapinfo: _.cloneDeep(state.heapinfo)
             , kont:     _.cloneDeep(state.kont) };
            _(stepColl).push(st);
         }
      }
      glDone = true;

      $('#stepper').prop('min', 0);
      $('#stepper').prop('max', stepColl.length - 1);
      $('#stepper').val(0);
      $('#stepper').change(function(e) {
         ui.uiStep(glStepColl[$(e.currentTarget).val()]);
      });

       return stepColl;
   };

   var uiBindings = function() {
      // CodeMirror options
      $('#autoclose').change(function(e){ CM.setOption('autoCloseBrackets', $(e.currentTarget).prop('checked')) });
      $('.opts_menu').toggle();
      $('#options').click(function() {$('.opts_menu').animate({height: 'toggle'});});
      $('#code').focus();

      $('#state').click(function() {
         console.log(glState);
      });
      $('#give').click(function(){ a = glState; });
      $('#scoll').click(function(){ console.log(glStepColl); });
      $('#step').click(function(){ 
         glState = (doStep(glState, glDone)).state; 
         ui.uiStep(glState);
      });

      $("#compile").click(function() {
         $('#functions').empty();
         $('.alert').hide();
         $('.collected a').text('Data Received, Click to Collect Steps');
         $('.collected a').one('click', function() {
            glStepColl = collectSteps(glState, glStepColl);
            glState = glStepColl[0];

            $('.collected a').html('Steps compiled');
         });

         $('#source').empty();

         $.post('http://localhost:3000/parse', CM.getValue())
         .success(function(data) {
            var r = receiveAST(data);
            var state    = r.state,
                stepColl = r.stepColl;

            glState = state;
            glStepColl = stepColl;
            ui.uiStep(state);

            var newstack = [{}];
            glState.stack = newstack.concat(glState.stack);
            glState.frames.unshift({name: 'main', params: state.heapinfo[_(state.stack).last()["main"]].params});
         })
         .error(function(err) {
            $('.alert').html(err);
            $('.alert').toggle();
         });
         return false;
      });

      tooltips.enableTooltips();
      tooltips.tooltipBindings();
   };

   // close the ui over the CodeMirror object
   //  and bind event handlers for clicky things
   ui = ui(CM);
   uiBindings();

   // attach invocation to dom because we're going to be removing and stuff a lot?
   $(document).on('state_update', '#frames li', prettyprint.update);
   $(document).on('click', '.compound_key', function(e) {
      var key = $(e.currentTarget);
      var name = key.data('compound');
      $('[data-var="' + name + '"]').toggle();
   });
});
