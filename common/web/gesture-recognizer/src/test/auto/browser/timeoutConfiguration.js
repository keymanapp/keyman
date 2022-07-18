// Preprocessing of the Karma configuration's client.args parameter.

com = com || {};
com.keyman = com.keyman || {};
com.keyman.karma = com.keyman.karma || {};

(function() {
  var kmwconfig = window['testconfig'] = {};

  var configArgs = window['__karma__'].config.args;  // Where Karma gives us our custom args.
  for(var i = 0; i < configArgs.length; configArgs++) {
    switch(configArgs[i].type) {
      case 'timeouts':
        var timeouts = JSON.parse(JSON.stringify(configArgs[i]));
        delete timeouts.type;

        kmwconfig['timeouts'] = timeouts;
        break;
    }
  }
})();
