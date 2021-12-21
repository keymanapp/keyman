import express = require('express');
import ws = require('ws');
import os = require('os');
import multer = require('multer');
import fs = require('fs');
import setupRoutes from './routes';
import { environment } from './environment';
import { configuration } from './config';
import Tray from './tray';

/* Lock file - report on PID and prevent multiple instances cleanly */

console.log(`Starting Keyman Developer Server, listening on port ${configuration.port}.`);

// This triggers the exit event for Ctrl+C which allows us to cleanup
// consistently
process.on('SIGINT', ()=>{process.exit(0)});

if(fs.existsSync(configuration.lockFilename)) {
  // Attempt connection to existing port

  // If that fails, we'll assume the previous process died and delete the lockfile.
}

// TODO: flock with fs-ext
let lockFileDescriptor = fs.openSync(configuration.lockFilename, 'w');
fs.writeFileSync(configuration.pidFilename, process.pid.toString());
fs.writeFileSync(lockFileDescriptor, process.pid.toString());
//console.log('lfd = '+JSON.stringify(lockFileDescriptor));

process.on('exit', () => {
  if(fs.existsSync(configuration.pidFilename)) {
    fs.unlinkSync(configuration.pidFilename);
  }
  if(lockFileDescriptor) {
    fs.closeSync(lockFileDescriptor);
    lockFileDescriptor = null;
    fs.unlinkSync(configuration.lockFilename);
  }
});

/* Server Setup */

const app = express();
const upload = multer({ storage: multer.memoryStorage(), limits: { fileSize: 1024*1024*10 /*10MB*/ } });

/* Websockets */

const wsServer = new ws.Server({ noServer: true });
wsServer.on('connection', socket => {
  socket.on('message', (message) => {
    console.debug('wsServer.socket.onmessage: '+message.toString());
    if(message.toString() == 'ping')
      socket.send('pong');
  });
  socket.send('refresh');
});

/* Setup routes */

setupRoutes(app, upload, wsServer, environment);

/* Start the server */

const server = app.listen(configuration.port);

server.on('upgrade', (request, socket, head) => {
  wsServer.handleUpgrade(request, socket, head, socket => {
    wsServer.emit('connection', socket, request);
  });
});

/* Launch ngrok if enabled */

configuration.ngrokEndpoint = '';

if(configuration.useNgrok && os.platform() == 'win32' && fs.existsSync(configuration.ngrokBinPath)) {
  const ngrok = require('ngrok');
  (async function() {
    configuration.ngrokEndpoint = await ngrok.connect({
      proto: 'http',
      addr: configuration.port,
      authtoken: configuration.ngrokToken,
      region: configuration.ngrokRegion,
      binPath: () => configuration.ngrokBinPath
    });
  })();
}

/* Load the tray icon */

const tray = new Tray();
tray.start();
