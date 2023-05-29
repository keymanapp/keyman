// Contains event management for mobile device rotation events.
/// <reference path="kmwrotation.ts" />

/***
   KeymanWeb 11.0
   Copyright 2019 SIL International
***/

// If KMW is already initialized, the KMW script has been loaded more than once. We wish to prevent resetting the
// KMW system, so we use the fact that 'initialized' is only 1 / true after all scripts are loaded for the initial
// load of KMW.
if(!window['keyman']['initialized']) {
  /*****************************************/
  /*                                       */
  /*   On-Screen (Visual) Keyboard Code    */
  /*                                       */
  /*****************************************/
  (function() {
    // Declare KeymanWeb object
    var keymanweb=window['keyman'],osk=keymanweb['osk'],util=keymanweb['util'],device=util.device;
    var dbg=keymanweb.debug;
    var dom = com.keyman.dom;

    // Force full initialization
    keymanweb.isEmbedded = false;

    /**
     * Set default device options
     * @param {OptionType}  opt device options object
     */
    keymanweb.setDefaultDeviceOptions = function(opt : com.keyman.OptionType) {
      // Element attachment type
      if (!opt['attachType']) {
        opt['attachType'] = (device.touchable ? 'manual' : 'auto');
      }
    }

    // Get default style sheet path
    keymanweb.getStyleSheetPath=function(ssName) {
      var ssPath = util['getOption']('resources')+'osk/'+ssName;
      return ssPath;
    }

    /**
     * Get keyboard path (relative or absolute)
     * KeymanWeb 2 revised keyboard location specification:
     *  (a) absolute URL (includes ':') - load from specified URL
     *  (b) relative URL (starts with /, ./, ../) - load with respect to current page
     *  (c) filename only (anything else) - prepend keyboards option to URL
     *      (e.g. default keyboards option will be set by Cloud)
     *
     * @param {string}  Lfilename  keyboard file name with optional prefix
     */
    keymanweb.getKeyboardPath=function(Lfilename) {
      var rx=RegExp('^(([\\.]/)|([\\.][\\.]/)|(/))|(:)');
      return (rx.test(Lfilename) ? '' : keymanweb.options['keyboards']) + Lfilename;
    }

    /**
     * Use rotation events to adjust OSK and input element positions and scaling as necessary
     */
    keymanweb.handleRotationEvents=function() {
      var rotationManager = new com.keyman.RotationManager(keymanweb);

      rotationManager.init();
    }

    /**
     * Possible way to detect the start of a rotation and hide the OSK before it is adjusted in size
     *
     *  @param  {Object}    e   accelerometer rotation event
     *
    keymanweb.testRotation = function(e)
    {
      var r=e.rotationRate;
      if(typeof(r) != 'undefined')
      {
        dbg(r.alpha+' '+r.beta+' '+r.gamma);
      }
    }
    */
  })();
}