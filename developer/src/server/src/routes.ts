import * as express from 'express';
import * as ws from 'ws';
import * as multer from 'multer';
import handleIncKeyboardsJs from './handlers/inc/keyboards-js.js';
import { data, DebugFont, DebugKeyboard, DebugModel, DebugObject, DebugPackage, isValidId } from './data.js';
import apiGet from './handlers/api/debugobject/get.js';
import apiRegister, { apiRegisterFile } from './handlers/api/debugobject/register.js';
import apiKeyboardRegister from './handlers/api/keyboard/register.js';
import apiFontRegister from './handlers/api/font/register.js';
import apiUnregister from './handlers/api/debugobject/unregister.js';
import handleIncPackagesJson from './handlers/inc/packages-json.js';
import apiPackageRegister from './handlers/api/package/register.js';
import handleIncKeyboardsCss from './handlers/inc/keyboards-css.js';
import { Environment } from './version-data.js';
import { configuration } from './config.js';
import chalk from 'chalk';
import { shutdown } from './shutdown.js';

export default function setupRoutes(app: express.Express, upload: multer.Multer, wsServer: ws.WebSocketServer, environment: Environment ) {

  /* Middleware - JSON and logging */

  app.use(express.json()); // for parsing application/json

  app.use(function (req, _res, next) {
    // if(environment.environment == 'local') {
      console.log(req.method + ' ' + req.path);
    // }
    next();
  });

  function isLocalhost(req: express.Request) {
    return (
      req.socket.remoteAddress == '127.0.0.1' ||
      req.socket.remoteAddress == '::1' ||
      req.socket.remoteAddress == '::ffff:127.0.0.1' // ipv4 localhost over ipv6
    );
  }
  function localhostOnly(req: express.Request, res: express.Response, next: express.NextFunction) {
    if(!isLocalhost(req)) {
      res.sendStatus(401);
    } else {
      next();
    }
  }

  // Only allow connections from localhost to /api/

  app.use('/api/', localhostOnly);

  /* All routes */

  app.use('/', express.static('build/src/site'));

  app.post('/upload', localhostOnly, upload.single('file'), (req, res, next) => {
    const name = req.file.originalname;
    if(!isValidId(name)) {
      res.sendStatus(400);
      return;
    }
    let fileType = '';
    const rModel = /^([a-zA-Z0-9_\.-]+)\.model\.js$/.exec(name);
    const rKeyboard = /^([a-zA-Z0-9_]+)\.js$/.exec(name);
    const rPackage = /^([a-zA-Z0-9_]+)\.kmp$/.exec(name);
    const rFont = /^(.+\.(ttf|otf))$/.exec(name);
    if(rModel) {
      apiRegisterFile(DebugModel, data.models, rModel[1], req.file.buffer);
      fileType = 'model';
    } else if(rKeyboard) {
      apiRegisterFile(DebugKeyboard, data.keyboards, rKeyboard[1], req.file.buffer);
      fileType = 'keyboard';
    } else if(rPackage) {
      apiRegisterFile(DebugPackage, data.packages, rPackage[1], req.file.buffer);
      fileType = 'package';
    } else if(rFont) {
      apiRegisterFile(DebugFont, data.fonts, rFont[1], req.file.buffer);
      fileType = 'font';
    } else {
      res.status(400).json({message: 'unrecognised file type'});
      return;
    }

    res.json({message: 'success', type: fileType});
    next();
  },
    saveState,
    (req,res,next)=>notifyClients(wsServer,req,res,next)
  );

  app.get('/inc/keyboards.js', handleIncKeyboardsJs);
  app.get('/inc/keyboards.css', handleIncKeyboardsCss);
  app.get('/inc/packages.json', handleIncPackagesJson);

  app.get('/api-public/version', (req,res,next)=>{
    res.json({version: environment.versionWithTag, isApiAvailable: isLocalhost(req)});
    next();
  });

  /* Localhost only routes -- todo /api/internal/ vs /api/... */

  app.post('/api/shutdown', (_req,res) => { setTimeout(shutdown, 100); res.send('ok'); });

  appGetData(app, /\/data\/keyboard\/(.+)\.js$/, data.keyboards);
  appGetData(app, /\/data\/model\/(.+)\.model\.js$/, data.models);
  appGetData(app, /\/data\/package\/(.+)\.kmp$/, data.packages);
  appGetData(app, /\/data\/font\/(.+)\.ttf$/, data.fonts);

  app.get('/api/font', (req,res,next)=>apiGet(data.fonts,req,res,next));
  app.post('/api/font/register',
    upload.single('file'),
    (req,res,next)=>apiRegister(DebugFont, data.fonts, req, res, next),
    apiFontRegister,
    saveState,
    (req,res,next)=>notifyClients(wsServer,req,res,next)
  );
  app.post('/api/font/unregister',
    (req,res,next)=>apiUnregister(data.fonts,req,res,next),
    saveState,
    (req,res,next)=>notifyClients(wsServer,req,res,next)
  );

  app.get('/api/keyboard', (req,res,next)=>apiGet(data.keyboards,req,res,next));
  app.post('/api/keyboard/register',
    upload.single('file'),
    (req,res,next)=>apiRegister(DebugKeyboard, data.keyboards, req, res, next),
    apiKeyboardRegister,
    saveState,
    (req,res,next)=>notifyClients(wsServer,req,res,next)
  );
  app.post('/api/keyboard/unregister',
    (req,res,next)=>apiUnregister(data.keyboards,req,res,next),
    saveState,
    (req,res,next)=>notifyClients(wsServer,req,res,next)
  );

  app.get('/api/model', (req,res,next)=>apiGet(data.models,req,res,next));
  app.post('/api/model/register',
    upload.single('file'),
    (req,res,next)=>apiRegister(DebugModel, data.models, req, res, next),
    saveState,
    (req,res,next)=>notifyClients(wsServer,req,res,next)
  );
  app.post('/api/model/unregister',
    (req,res,next)=>apiUnregister(data.models,req,res,next),
    saveState,
    (req,res,next)=>notifyClients(wsServer,req,res,next)
  );

  app.get('/api/package', (req,res,next)=>apiGet(data.packages,req,res,next));
  app.post('/api/package/register',
    upload.single('file'),
    (req,res,next)=>apiRegister(DebugPackage, data.packages, req, res, next),
    apiPackageRegister,
    saveState,
    (req,res,next)=>notifyClients(wsServer,req,res,next)
  );
  app.post('/api/package/unregister',
    (req,res,next)=>apiUnregister(data.packages,req,res,next),
    saveState,
    (req,res,next)=>notifyClients(wsServer,req,res,next)
  );

  /* ngrok data */

  app.get('/api/status', (_req,res,next) => {
    const response = { ngrokEnabled: configuration.useNgrok, ngrokEndpoint: configuration.ngrokEndpoint };
    res.send(response);
    next();
  });
}

/* Utility functions */

function notifyClients(wsServer: ws.WebSocketServer, res: express.Request, req: express.Response, next: express.NextFunction) {
  wsServer.clients.forEach(c => {
    c.send('refresh', (err) => {
      if(err) console.error(chalk.red('Websocket send error '+err.message));
    });
  });
  next();
}

function saveState (res: express.Request, req: express.Response, next: express.NextFunction) {
  data.saveState();
  if(next) next();
}

function appGetData(app: express.Express, pathregex: RegExp,  root: { [id: string]: DebugObject }) {
  app.get(pathregex, (req,res,next)=>{

    const r = pathregex.exec(req.path);
    if(!r) {
      res.status(404).send('invalid path');
      return;
    }
    if(!isValidId(r[1])) {
      res.sendStatus(400);
      return;
    }
    const o = root[r[1]];
    if(!o) {
      res.status(404).send('not found');
    } else {
      o.lastUse = new Date();
      saveState(null, null, null);
      res.sendFile(o.filename, (err)=>{if(err) console.error(chalk.red(err))});
    }
  });
}

