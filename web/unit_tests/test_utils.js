var setupKMW = function(kmwOptions, done, timeout, uiInitCheck) {
  var ui;

  if(typeof(kmwOptions) == 'string' || typeof(kmwOptions) == 'undefined' || kmwOptions == null) {
    ui = kmwOptions;

    var kmwOptions = {
      attachType:'auto',
      root:'source',
      resources:'source'
    };
  
    if(ui) {
      kmwOptions.ui = ui;
    }
  }

  var kmw = setupScript('source/keymanweb.js', done, timeout, uiInitCheck);
  fixture.el.appendChild(kmw);

  ui = kmwOptions.ui;

  kmwOptions.attachType = kmwOptions.attachType ? kmwOptions.attachType : 'auto';
  kmwOptions.root = 'source';
  kmwOptions.resources = 'source';

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
  }, 8);
}

var setupScript = function(src, done, timeout, uiInitCheck) {
  var Lscript = document.createElement('script');
  Lscript.charset="UTF-8";        // KMEW-89
  Lscript.type = 'text/javascript';
  Lscript.async = false;
  if(done) {
    Lscript.onload = initTimer(done, timeout, uiInitCheck);
  }

  Lscript.src = src;

  return Lscript;
}

var teardownKMW = function() {
  keyman['shutdown']();
  delete window.keyman;
  window["keyman"] = undefined;
}

// Make sure the main script loads...
var initTimer = function(done, timeout, uiInitCheck) {
  var uiLoadDelay;
  if(typeof(uiInitCheck) != 'function') {
    uiInitCheck = function() { return true; };
    uiLoadDelay = false;
  } else {
    uiLoadDelay = true;
  }

  // We need managed state for this.
  var InitializationManager = function() {
    this.killSwitch = false;

    this.initCheckCallback = function() {
      if(window['keyman'] && window['keyman'].initialized == 2 && uiInitCheck()) {
        if(done) {
          this.timer = window.setTimeout(function() { 
            // There can be some odd cross-interference with the UI modules and their initialization.
            // We use a significant delay here to avoid said problems.
            done();
          }, uiLoadDelay ? 2000 : 0);
        }
      } else if(!this.killSwitch) {
        this.timer = window.setTimeout(this.initCheckCallback, 50);
      }
    }.bind(this);

    if(timeout) {
      window.setTimeout(function() {
        this.killSwitch = true;

        if(this.observer) {
          this.observer.end();
        }

        if(this.timer) {
          window.clearTimeout(this.timer);
          this.timer = 0;
        }
      }.bind(this), timeout);
    }
  }

  var im = new InitializationManager();
  return im.initCheckCallback;
};