(function() {
  var me = document.currentScript;
  console.log(me);

  var onload = me.onload;
  if(onload) {
    me.onload = null;
  }

  document.body.addEventListener('load', function(e) {
    // Prevent the element's onload event from firing
    if(e.srcElement == me || e.target == me) {
      e.cancelBubble = true;
    }
  }, {capture: true});

  window.setTimeout(function () {
    // Restores the function after a slight delay.
    me.onload = onload;
  }, 1);
})();