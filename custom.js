// DOMContentLoaded callback  
document.addEventListener('DOMContentLoaded', function() {

  // Initialize ScrollSpy
  var scrollSpy = new ScrollSpy();

  // Save reference to default activate
  var defaultActivate = scrollSpy.activate;

  // Custom activate method
  scrollSpy.activate = function(target) {
    
    // Get scroll position 
    var scrollPos = document.body.scrollTop;

    // Previous target offset
    var prevOffset = $(scrollSpy.activeTarget).offset().top;

    // Only activate if past halfway
    if(scrollPos >= prevOffset + ($(scrollSpy.activeTarget).height()/2)) {
      defaultActivate.call(scrollSpy, target); 
    }

  };
  
  // Refresh ScrollSpy after overriding
  scrollSpy.refresh();

});