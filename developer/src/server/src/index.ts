import * as fs from 'node:fs';
import * as path from 'node:path';
import chalk from 'chalk';
import express from 'express';
import multer from 'multer';
import * as ws from 'ws';
import { KeymanSentry, loadOptions } from '@keymanapp/developer-utils';
import { configuration } from './config.js';
import { environment } from './environment.js';
import setupRoutes from './routes.js';
import { shutdown } from './shutdown.js';
import { initTray } from './tray.js';

const options = {
  ngrokLog: false,   // Set this to true if you need to see ngrok logs in the console
};

/* Lock file - report on PID and prevent multiple instances cleanly */

console.log(`Starting Keyman Developer Server ${environment.versionWithTag}, listening on port ${configuration.port}.`);

// We need to load the Keyman Developer options before attempting to initialize
// Sentry. `loadOptions` silently suppresses exceptions and returns a default
// set of options if an error occurs.
await loadOptions();

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

  /* Start the web server */

  let server = null;
  try {
    server = app.listen(configuration.port);
  } catch(err) {
    console.error(err);
    // TODO handle and cleanup EADDRINUSE, throw anything else
  }

  /* Attach the web socket server */

  server.on('upgrade', (request, socket, head) => {
    wsServer.handleUpgrade(request, socket, head, socket => {
      wsServer.emit('connection', socket, request);
    });
  });

  /* Launch ngrok if enabled */

  configuration.ngrokEndpoint = '';
  if(configuration.useNgrok) {
    await startNGrok();
  }

  /* Load the tray icon */

  tray.start(configuration.port, configuration.ngrokEndpoint);
}

async function loadNGrok() {
  try {
    return await import('@ngrok/ngrok');
  } catch(e) {
    // ngrok can fail import if it cannot load its dependent binary library,
    // either because it is missing or because it has missing dependencies
    // itself. But we don't want to crash the server if that happens
    console.error(e);
    return null;
  }
}

async function startNGrok() {
  console.log('Attempting to start ngrok');
  const ngrok = await loadNGrok();
  if(!ngrok) {
    return false;
  }

  let started = false;
  const listener = await ngrok.forward({
    proto: 'http',
    addr: configuration.port,
    authtoken: configuration.ngrokToken,
    onLogEvent: (msg: string) => {
      if(options.ngrokLog) {
        console.log(chalk.cyan(('\n'+msg).split('\n').join('\n[ngrok] ').trim()));
      }
    },
    onStatusChange: (state: string) => {
      if(state == 'connected' && started) {
        // We only announce reconnection after initial start
        configuration.ngrokEndpoint = listener.url();
        console.log(chalk.blueBright('ngrok tunnel reconnected at %s'), configuration.ngrokEndpoint);
      } else if(state == 'closed') {
        configuration.ngrokEndpoint = '';
        console.log(chalk.blueBright('ngrok tunnel closed'));
      }
    }
  });
  started = true;
  configuration.ngrokEndpoint = listener.url();
  console.log(chalk.blueBright('ngrok tunnel established at %s'), configuration.ngrokEndpoint);

  return true;
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