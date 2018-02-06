var setupKMW = function(ui) {
  var kmw = setupScript('source/keymanweb.js');
  fixture.el.appendChild(kmw);

  var kmwOptions = {
    attachType:'auto',
    root:'source',
    resources:'source'
  };

  if(ui) {
    var ui = setupScript('source/kmwui' + ui + '.js');
    fixture.el.appendChild(ui);

    kmwOptions.ui=ui;
  }

  var initFunc = function() {
    if(window['keyman']) {
      window['keyman'].init(kmwOptions);
    } else {
      window.setTimeout(function() {
        initFunc();
      })
    }
  };

  /* Keep this timeout short - if set too long, kmwinit's default initialization
   * will kick in and prevent our settings from going through!
   */
  window.setTimeout(function() {
    initFunc();
  }, 5);
}

var setupScript = function(src) {
  var Lscript = document.createElement('script');
  Lscript.charset="UTF-8";        // KMEW-89
  Lscript.type = 'text/javascript';
  Lscript.async = false;

  Lscript.src = src;

  return Lscript;
}

var teardownKMW = function() {
  delete window.keyman;
  window["keyman"] = undefined;
}

// Make sure the main script loads...
var initTimer = function(done, timeout) {
  window.setTimeout(function() {
    if(window['keyman']) {
      // ... and then give KMW and the UI a bit more time to init and attach.
      window.setTimeout(function() {
        done();
      }, timeout ? timeout : 1000);
    } else {
      initTimer(done);
    }
  }, 50);
};