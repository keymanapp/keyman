// Preprocessing of the Karma configuration's client.args parameter.

var com = com || {};
com.keyman = com.keyman || {};
com.keyman.karma = com.keyman.karma || {};

(function() {
  const testconfig = window['testconfig'] = {};

  // Default value.
  let mobile = false;

  // If we've set things up to support Device dection without loading KMW...
  if(com.keyman.Device) {
    try {
      let device = new com.keyman.Device();
      device.detect();

      mobile = (device.formFactor != 'desktop');
    } finally {
      // no-op; silent failure's fine here.
    }
  }

  let configArgs = window['__karma__'].config.args;  // Where Karma gives us our custom args.
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
        testconfig['timeouts'] = timeouts;
        break;
    }
  }
})();
