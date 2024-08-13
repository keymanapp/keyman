import { environment } from './environment.js';
import { KeymanSentry } from '@keymanapp/developer-utils';
import express from 'express';
import * as ws from 'ws';
import * as os from 'os';
import multer from 'multer';
import * as fs from 'fs';
import * as path from 'path';
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
  /* Ensure single instance */

  if(!writeLockFile()) {
    return;
  }

  /* Setup the tray */

  const tray = await initTray();

  // This triggers the exit event for Ctrl+C which allows us to cleanup
  // consistently
  process.on('SIGINT', () => {
    tray.shutdown();
    shutdown();
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

  let server = null;
  try {
    server = app.listen(configuration.port);
  } catch(err) {
    console.error(err);
    // TODO handle and cleanup EADDRINUSE, throw anything else
  }

  server.on('upgrade', (request, socket, head) => {
    wsServer.handleUpgrade(request, socket, head, socket => {
      wsServer.emit('connection', socket, request);
    });
  });

  /* Launch ngrok if enabled */

  configuration.ngrokEndpoint = '';

  if(configuration.useNgrok
    && os.platform() == 'win32'
    && fs.existsSync(path.join(configuration.ngrokBinPath, 'ngrok.exe'))
  ) {
    const ngrok: any = await import('ngrok');
    (async function() {
      configuration.ngrokEndpoint = await ngrok.connect({
        proto: 'http',
        addr: configuration.port,
        authtoken: configuration.ngrokToken,
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

function getRunningInstancePid(pidFilename: string) {
  try {
    return fs.readFileSync(pidFilename, 'utf-8');
  } catch(err) {
    return null;
  }
}

function writeLockFile() {
  const lockFilename = configuration.lockFilename.replaceAll(/[\\\/]/g, path.sep);
  const pidFilename = configuration.pidFilename.replaceAll(/[\\\/]/g, path.sep);

  // console.debug(`Testing existence of ${lockFilename}`);
  if(fs.existsSync(lockFilename)) {
    // If that fails, we'll assume the previous process died and delete the lockfile.
    try {
      // console.debug(`Attempting to delete.`);
      fs.unlinkSync(lockFilename);
      if(fs.existsSync(lockFilename)) {
        // console.debug(`Unlink silently failed`);
        console.warn(`Lock file cannot be deleted, probably owned by ${getRunningInstancePid(pidFilename) ?? 'unknown instance'}`);
        return false;
      }
    } catch(err) {
      // assume that it is actually locked!
      console.warn(`Lock file cannot be deleted, probably owned by ${getRunningInstancePid(pidFilename) ?? 'unknown instance'}\n  ${err}`);
      return false;
    }
  }

  // console.debug('Creating lock file');
  let lockFileDescriptor: number = null;
  try {
    lockFileDescriptor = fs.openSync(lockFilename, 'w');
  } catch(err) {
    console.warn(`Lock file could not be created, assuming another instance already running, probably ${getRunningInstancePid(pidFilename) ?? 'unknown instance'}\n  ${err}`);
    return false;
  }
  // console.debug(`Lock file descriptor: ${lockFileDescriptor}, pid ${process.pid.toString()}`);

  fs.writeFileSync(pidFilename, process.pid.toString());
  fs.writeFileSync(lockFileDescriptor, process.pid.toString());

  process.on('exit', () => {
    if(fs.existsSync(pidFilename)) {
      fs.unlinkSync(pidFilename);
    }
    if(lockFileDescriptor) {
      fs.closeSync(lockFileDescriptor);
      lockFileDescriptor = null;
      try {
        fs.unlinkSync(lockFilename);
      } catch(err) {
        // This can happen if an unlink call is made on a file with an open
        // descriptor, such as happens above; when the file is closed it is
        // automatically deleted. On Windows at least.
        //
        // console.debug(`Did not unlink ${lockFilename}, probably already deleted`);
      }
    }
  });
  return true;
}