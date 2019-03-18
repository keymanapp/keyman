/**
 * test-host: Dynamic HTTP server response handlers for regression tests
 */


const fs = require("fs");
const path = require('path');
const express = require('express');
const config = require('./config.js');

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
    let shortnames = fs.readdirSync(KEYBOARDS_ROOT);
    let keyboards = [];
    shortnames.forEach(function(shortname) {
      let kbds = fs.readdirSync(path.join(KEYBOARDS_ROOT, shortname));
      if(kbds && kbds.length) {
        kbds.forEach(function(kbd) {
          if(fs.existsSync(path.join(KEYBOARDS_ROOT, shortname, kbd, 'build', kbd+'.tests')) &&
              fs.existsSync(path.join(KEYBOARDS_ROOT, shortname, kbd, 'build', kbd+'.js'))) {
            keyboards.push({s:shortname, id: kbd});
          }
        });
      }
    });
    console.log(`/list-keyboards`);
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
    response.setHeader('Content-Type', 'application/json');

    let json = request.body;
    if(request.method !== 'POST' || !json || typeof json != 'object' || !json.id || !json.shortname ) {
      console.error('/save-results Invalid request');
      response.writeHead(400);
      return response.end(JSON.stringify({result: "Invalid request"}));
    }

    console.log(`/save-results ${json.shortname}/${json.id}`);
  
    // Do some basic sanity checks to avoid disasters
    if(typeof(json.id) != 'string' || 
        !json.id.match(/^[a-z0-9_]+$/) ||
        typeof(json.shortname) != 'string' ||
        !json.shortname.match(/^[a-z]+$/)) {
      console.error('/save-results Invalid request');
      response.writeHead(400);
      return response.end(JSON.stringify({result: "Invalid request"}));
    }
  
    // Ensure that the 'shortname/id/build' folder exists
    let base = path.join(KEYBOARDS_ROOT, json.shortname, json.id, 'build');
    if(!fs.existsSync(base)) {
      const msg = `Keyboard ${json.shortname}/${json.id} not found at ${base}`;
      console.error('/save-results 404 '+msg);
      response.writeHead(404);
      return response.end(JSON.stringify({result: msg}));
    }
  
    // Assuming for now that the json is valid. This is internal use so we are not too worried
    // about integrity.
    fs.writeFileSync(path.join(base, json.id+'.results'), JSON.stringify(json.results, null, 2));
    response.writeHead(200);
    return response.end(JSON.stringify({result: 'success'}));
  }
};
