var BASE_CONFIG = require("./base.conf.cjs");
var ci_config_adapter = require("../../../../../../../common/test/resources/karma-browserstack-config.js");

module.exports = ci_config_adapter(BASE_CONFIG, "partial");