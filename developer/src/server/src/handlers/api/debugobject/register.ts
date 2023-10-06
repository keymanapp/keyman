import * as express from 'express';
import { DebugObject, isValidId, simplifyId } from "../../../data.js";
import * as fs from 'fs';
import * as crypto from 'crypto';
import { configuration } from '../../../config.js';
import chalk from 'chalk';

// We allow only 12 objects of each type in the cache
const MAX_OBJECTS = 12;

export default function apiRegister<O extends DebugObject> (intf: new () => O, root:{ [id: string]: O }, req: express.Request, res: express.Response, next: express.NextFunction) {
  if(!apiRegisterFile<O>(intf, root, req.body['id'], req.file.buffer)) {
    res.sendStatus(400);
    return;
  }
  next();
}

export function apiRegisterFile<O extends DebugObject> (intf: new () => O, root: { [id: string]: O }, id: string, file: Buffer) {
  if(!isValidId(id)) {
    return false;
  }

  id = simplifyId(id);

  let keys = Object.keys(root);
  if(keys.length > MAX_OBJECTS) {
    // remove least recently used objects
    let oldestKey = Object.keys(root).reduce((lastKey, key) => root[key].lastUse < root[lastKey].lastUse ? key : lastKey, keys[0]);
    console.log(chalk.grey('%s'), 'removing least recently used object '+root[oldestKey].id);
    if(fs.existsSync(root[oldestKey].filename)) {
      fs.unlinkSync(root[oldestKey].filename);
    }
    delete root[oldestKey];
  }

  // Register or re-register the object

  const o: O = root[id] ?? new intf();
  root[id] = o;

  o.lastUse = new Date();
  o.id = id;
  o.filename = configuration.cachePath + o.filenameFromId(id);
  fs.writeFileSync(o.filename, file);
  o.sha256 = crypto.createHash('sha256').update(file).digest('hex');

  console.log(chalk.grey('%s'), 'registering object '+o.id+' at '+o.filename+' sha '+o.sha256);

  return true;
}