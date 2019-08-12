var helpers;

// Establishes the equivalent of a TS namespace.
(function(helpers){
  helpers.defaultCapabilities = {
    maxLeftContextCodeUnits: 64
  };

  var config = window['config'] = {};
  var configArgs = window['__karma__'].config.args;  // Where Karma gives us our custom args.
  for(var i = 0; i < configArgs.length; configArgs++) {
    switch(configArgs[i].type) {
      case 'timeouts':
        var timeouts = JSON.parse(JSON.stringify(configArgs[i]));
        delete timeouts.type;

        config['timeouts'] = timeouts;
        break;
    }
  }
})(helpers || (helpers = {}));