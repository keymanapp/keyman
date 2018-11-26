// KeymanWeb test suite - processing of the Karma configuration's client.args parameter.

(function() {
  var device = new com.keyman.Device();
  device.detect();
  var mobile = (device.formFactor != 'desktop');

  var kmwconfig = window['kmwconfig'] = {mobile: mobile};

  var configArgs = window['__karma__'].config.args;  // Where Karma gives us our custom args.
  for(var i = 0; i < configArgs.length; configArgs++) {
    switch(configArgs[i].type) {
      case 'timeouts':
        var timeouts = JSON.parse(JSON.stringify(configArgs[i]));
        delete timeouts.type;

        if(mobile) {
          for(var key in timeouts) {
            if(key != 'mobileFactor') {
              timeouts[key] = timeouts[key] * timeouts['mobileFactor'];
            }
          }
        }
        kmwconfig['timeouts'] = timeouts;
        break;
    }
  }
})();

// Keyman test suite utility methods

var setupKMW = function(kmwOptions, done, timeout, uiInitCheck) {
  var ui;

  if(typeof(kmwOptions) == 'string' || typeof(kmwOptions) == 'undefined' || kmwOptions == null) {
    ui = kmwOptions;

    var kmwOptions = {
      attachType:'auto',
      root:'source',
      resources:'../../../../source'
    };
  
    if(ui) {
      kmwOptions.ui = ui;
    }
  }

  var kmw = setupScript('source/keymanweb.js', done, timeout, uiInitCheck);
  fixture.el.appendChild(kmw);

  ui = kmwOptions.ui;

  kmwOptions.attachType = kmwOptions.attachType ? kmwOptions.attachType : 'auto';
  
  if(!kmwOptions.root) {
    kmwOptions.root = 'source';
  }

  if(!kmwOptions.resources) {
    kmwOptions.resources = '../../../../source';
  }

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
  var error = null;
  if(keyman) { // If our setupKMW fails somehow, this guard prevents a second error report on teardown.

    // We want to be SURE teardown works correctly, or we'll get lots of strange errors on other tests.
    // Thus, error-handling on shutdown itself.  It HAS mattered.
    try {
      keyman['shutdown']();
    } catch(err) {
      error = err;
    }

    try {
      var success = delete window["keyman"];
      if(!success) {
        window["keyman"] = undefined;
      }
    } finally {
      if(error) {
        console.log("Error during KMW shutdown!");
        throw error;
      }
    }
  }
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

// Make sure the main script loads...
var onScriptLoad = function(scriptURL, callback, timeout) {
  var ScriptLoadObserver = function() {
    this.target = document.createElement('a');
    this.target.href = scriptURL;

    if(timeout) {
      this.timer = window.setTimeout(function() {
        if(this.mo) {
          this.mo.disconnect();
        }
      }.bind(this), timeout);
    }

    var moCallback = function(mutations) {
      for(var i=0; i < mutations.length; i++) {
        var mutation = mutations[i];
        for(var j=0; j < mutation.addedNodes.length; j++) {
          var child = mutation.addedNodes[j];
          if(child instanceof HTMLScriptElement) {
            if(child.src == this.target.href) {
              child.onload = callback;
            }
          }
        }
      }
    }

    this.observe = function() {
      var config = { childList: true, subtree: true };
      this.mo = new MutationObserver(moCallback.bind(this));
      this.mo.observe(document, config);
    }
  }

  var slo = new ScriptLoadObserver();
  slo.observe();
};

var loadKeyboardStub = function(stub, callback, timeout, params) {
  var kbdName = "Keyboard_" + stub.id;

  keyman.addKeyboards(stub);
  if(!params || !params.passive) {
    keyman.setActiveKeyboard(kbdName, stub.languages.id);
  }

  if(keyman.getActiveKeyboard() != kbdName) {
    onScriptLoad(stub.filename, function() {
      callback();
    }, timeout);
  } else {
    callback();
  }
}

var loadKeyboardFromJSON = function(jsonPath, callback, timeout) {
  var stub = fixture.load(jsonPath, true);

  loadKeyboardStub(stub, callback, timeout);
}

function runLoadedKeyboardTest(testDef, usingOSK, assertCallback) {
  var inputElem = document.getElementById('singleton');
    if(inputElem['kmw_ip']) {
      inputElem = inputElem['kmw_ip'];
    }

    testDef.run(inputElem, usingOSK, assertCallback);
}

function runKeyboardTestFromJSON(jsonPath, params, callback, assertCallback, timeout) {
  var testSpec = new KMWRecorder.KeyboardTest(fixture.load(jsonPath, true));

  loadKeyboardStub(testSpec.keyboard, function() {
    runLoadedKeyboardTest(testSpec, params.usingOSK, assertCallback);
    keyman.removeKeyboards(testSpec.keyboard.id);
    callback();
  }, timeout);
}

function retrieveAndReset(Pelem) {
  var alias = Pelem['kmw_ip'];
  var val = "";
  if(alias) {
    val = alias.textContent;
    keyman.touchAliasing.setText(alias, "", 0);
  } else {
    val = Pelem.value;
    Pelem.value = "";
  }

  return val;
}