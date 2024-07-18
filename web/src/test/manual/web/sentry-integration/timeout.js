(function() {
  var me = document.currentScript;
  console.log(me);
  document.body.addEventListener('load', function(e) { 
    // Prevent the element's onload event from firing
    if(e.srcElement == me || e.target == me) {
      e.cancelBubble = true;
    }
  }, {capture: true});
})();