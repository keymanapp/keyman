import { environment } from './environment.js';
import { KeymanSentry } from '@keymanapp/developer-utils';
import express from 'express';
import * as ws from 'ws';
import * as os from 'os';
import multer from 'multer';
import * as fs from 'fs';
import setupRoutes from './routes.js';
import { configuration } from './config.js';
import { initTray } from './tray.js';
import chalk from 'chalk';
import { shutdown } from './shutdown.js';

const options = {
  ngrokLog: false,   // Set this to true if you need to see ngrok logs in the console
};

/* Lock file - report on PID and prevent multiple instances cleanly */

console.log(`Starting Keyman Developer Server ${environment.versionWithTag}, listening on port ${configuration.port}.`);

KeymanSentry.init();
try {
  await run();
} catch(e) {
  KeymanSentry.captureException(e);
  throw e;
}

// Ensure any messages reported to Sentry have had time to be uploaded before we
// exit. In most cases, this will be a no-op so should not affect performance.
await KeymanSentry.close();

export async function run() {
  // This triggers the exit event for Ctrl+C which allows us to cleanup
  // consistently
  process.on('SIGINT', shutdown);

  const tray = await initTray();

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
  const upload = multer({ storage: multer.memoryStorage(), limits: { fileSize: 1024*1024*100 /*100MB*/ } });

  /* Websockets */

  const wsServer = new ws.WebSocketServer({ noServer: true });
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
    const ngrok: any = await import('ngrok');
    (async function() {
      configuration.ngrokEndpoint = await ngrok.connect({
        proto: 'http',
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
      tray.start(configuration.port, configuration.ngrokEndpoint);
    })();
  }
  else {
    /* Load the tray icon */
    tray.start(configuration.port, configuration.ngrokEndpoint);
  }
}