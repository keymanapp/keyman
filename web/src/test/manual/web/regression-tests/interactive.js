/**
 * Start a http server to run interactive sessions of the regression tests. This
 * module is intended to help iron out any configuration or compatibility issues
 * that may arise as well as simplifying the process of diagnosing a single test 
 * interactively.
 * 
 * Change configurable parameters in **config.js**.
 * 
 * To start this server, run `node index.js`. Then navigate to `localhost:1337`
 * (or as specified in **config.js**). Press Ctrl+C to stop the server.
 * 
 * Automated testing uses Karma (running with `karma.conf.js`), normally run
 * with **test.js** in this folder. To start Karma manually:
 *    `./node_modules/.bin/karma start karma.conf.js` or
 *    `node ./node_modules/karma/bin/karma start karma.conf.js`
 */
const fs = require("fs");
const path = require('path');

const express = require("express");
const config = require('./node_src/config.js');
const testHost = require('./node_src/test-host.js');

const app = express();

// Setup static server

app.use(express.json());
app.use(express.static(__dirname));
app.use('/web/', express.static(config.KEYMANWEB_ROOT, {index: false}));
app.use('/keyboards/', express.static(path.join(config.KEYBOARDS_ROOT, config.KEYBOARDS_GROUP), {index: false}));

// Dynamic back-end results

app.get("/list-keyboards", testHost.listKeyboards);
app.post("/save-results", testHost.saveResults);

console.log('Listening on http://'+config.host+':'+config.port);

app.listen(config.port, config.host);
