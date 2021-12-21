import express = require('express');
import ws = require('ws');
import os = require('os');
import multer = require('multer');
import fs = require('fs');
import setupRoutes from './routes';
import { environment } from './environment';
import { configuration } from './config';
import Tray from './tray';
import chalk = require('chalk');

const options = {
  ngrokLog: false,   // Set this to true if you need to see ngrok logs in the console
};

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
      bind_tls: true,
      addr: configuration.port,
      authtoken: configuration.ngrokToken,
      region: configuration.ngrokRegion,
      binPath: () => configuration.ngrokBinPath,
      onLogEvent: (msg: string) => {
        if(options.ngrokLog) {
          console.log(chalk.cyan(('\n'+msg).split('\n').join('\n[ngrok] ').trim()));
        }
      },
      onStatusChange: (state: string) => {
        if(state == 'connected') {
          setTimeout(async () => {
            const api = ngrok.getApi();
            const tunnels = await api.listTunnels();
            configuration.ngrokEndpoint = tunnels.tunnels[0]?.public_url ?? '';
            console.log(chalk.blueBright('ngrok tunnel established at %s'), configuration.ngrokEndpoint);
          }, 1000);
        } else if(state == 'closed') {
          configuration.ngrokEndpoint = '';
          console.log(chalk.blueBright('ngrok tunnel closed'));
        }
      }
    });
    console.log(chalk.blueBright('ngrok tunnel initially established at %s'), configuration.ngrokEndpoint);
  })();
}

/* Load the tray icon */

const tray = new Tray();
tray.start();
