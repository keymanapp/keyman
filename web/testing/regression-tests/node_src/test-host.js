/**
 * test-host: Dynamic HTTP server response handlers for regression tests
 */


const fs = require("fs");
const path = require('path');
const express = require('express');
const config = require('./config.js');
const util = require('./util.js');

const KEYBOARDS_ROOT = path.join(config.KEYBOARDS_ROOT, config.KEYBOARDS_GROUP);

module.exports = {

  /**
   * Middleware handler used with Karma
   * @param {express.Request} request 
   * @param {express.Response} response
   * @param {express.Next} next
   */
  handleRequest: function(request, response, next) {
    if(request.url == '/list-keyboards') {
      return module.exports.listKeyboards(request, response);
    } else if(request.url == '/save-results') {
      return module.exports.saveResults(request, response);
    } else {
      return next();
    }
  },

  /**
   * Gets a list of all keyboards that have tests available from the keyboards repository.
   * @param {express.Request} request 
   * @param {express.Response} response 
   */
  listKeyboards: function(request, response) {
    console.log(`/list-keyboards`);
    let keyboards = util.getKeyboardFolders(KEYBOARDS_ROOT);
    response.setHeader('Content-Type', 'application/json');
    response.writeHead(200);
    return response.end(JSON.stringify(keyboards));
  },

  /**
   * Saves the results of a test to the build folder of the selected keyboard in the keyboards
   * repository.
   * @param {express.Request} request 
   * @param {express.Response} response 
   */
  saveResults: function(request, response) {
    // Take the posted JSON file, save to KEYBOARDS_ROOT/x/id/build/id.results
    let body = [];

    function finish(json) {
      response.setHeader('Content-Type', 'application/json');
      if(request.method !== 'POST' || 
          !json || 
          typeof json != 'object' || 
          !json.id || 
          !json.shortname || 
          !json.hasOwnProperty('compilerVersion') || 
          !json.hasOwnProperty('engineVersion')) {
        console.warn('/save-results Invalid request');
        response.writeHead(400);
        return response.end(JSON.stringify({result: "Invalid request"}));
      }

      // We can override the compiler and engine versions because for 
      // source builds, the version numbers provided by the compiler and/or
      // KeymanWeb are not relevant.
      const compilerVersion = config.compilerVersion || json.compilerVersion;
      const engineVersion = config.engineVersion || json.engineVersion;
      const filename = `${json.id}-${compilerVersion}-${engineVersion}.results`;
  
      console.log(`/save-results ${json.shortname}/${filename}`);

      // Do some basic sanity checks to avoid disasters
      if(typeof(json.id) != 'string' || !json.id.match(/^[a-z0-9_]+$/) ||
          typeof(json.shortname) != 'string' || !json.shortname.match(/^[a-z]+$/) ||
          typeof(compilerVersion) != 'string' || !compilerVersion.match(/^((\d+)(\.\d+)*)|source$/) ||
          typeof(engineVersion) != 'string' || !engineVersion.match(/^((\d+)(\.\d+)*)|source$/)) {
        console.warn('/save-results Invalid request');
        response.writeHead(400);
        return response.end(JSON.stringify({result: "Invalid request"}));
      }
    
      // Ensure that the 'shortname/id/build' folder exists
      let base = path.join(KEYBOARDS_ROOT, json.shortname, json.id);
      if(!fs.existsSync(base)) {
        const msg = `Keyboard ${json.shortname}/${json.id} not found at ${base}`;
        console.warn('/save-results 404 '+msg);
        response.writeHead(404);
        return response.end(JSON.stringify({result: msg}));
      }
      base = path.join(base, 'tests');
      if(!fs.existsSync(base)) {
        fs.mkdirSync(base);
        if(!fs.existsSync(base)) {
          const msg = `Could not create tests folder ${base}`;
          console.warn('/save-results 500 '+msg);
          response.writeHead(500);
          return response.end(JSON.stringify({result: msg}));
        }
      }

      // Assuming for now that the json is valid. This is internal use so we are not too worried
      // about integrity.
      fs.writeFileSync(path.join(base, filename), JSON.stringify(json.results, null, 2));

      response.writeHead(200);
      return response.end(JSON.stringify({result: 'success'}));
    };

    if(request.body) {
      // Express hosted (interactive)
      return finish(request.body);
    } else {
      // Karma-based (CI)
      request.on('error', (err) => {
        console.error(err);
      }).on('data', (chunk) => {
        body.push(chunk);
      }).on('end', () => {
        body = Buffer.concat(body).toString();
        finish(JSON.parse(body));
      });
    }
  }
};
