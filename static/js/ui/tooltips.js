define(function() {
   var tooltipsOn = true;
   var tooltips = [
      { selector: '#frames_container h4',
         title: "When a function is called, a frame is pushed onto the stack. Frames keep track of all function's variables and data. Clicking the & will highlight the address of the variable in memory.",
      placement: 'bottom' },
      { selector: '#mem h4',
         title: "The memory is where all your data actually lives. Variables have addresses which is where they are located, and values which is what is at that location",
      placement: 'bottom' },
      { selector: '#globals',
         title: "Globals are where data that is outside functions live. These are accessible from any function; in addition, functions are also globals. Functions have their own locations in memory. Globals aren't actually a frame like this, but you can think of them as a base frame.",
      placement: 'top' }
   ];

   var enableTooltips = function() {
      _.map(tooltips, function(t) {
         $(t.selector).tooltip({title: t.title, placement: t.placement});});
   };

   var tooltipBindings = function() {
      $('.array_key').click(function(e) {
         var aname = $(e.currentTarget).data('array');
         $("tr[data-var="+aname+"][data-first!=true]").toggle();
      });

      $('#tooltips').click(function() {
         if (tooltipsOn) {
            _.map(tooltips, function(t) { $(t.selector).tooltip('destroy'); });
            tooltipsOn = false;
         } else {
            enableTooltips();
            tooltipsOn = true;
         }
      });
   };

   return {
      tooltipBindings: tooltipBindings,
      enableTooltips: enableTooltips
   };
});
