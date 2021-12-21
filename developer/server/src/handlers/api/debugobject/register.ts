import express = require('express');
import { DebugObject } from "../../../data";
import fs = require('fs');
import crypto = require('crypto');
import { configuration } from '../../../config';

export default function apiRegister<O extends DebugObject> (intf: new () => O, root:{ [id: string]: O }, req: express.Request, res: express.Response, next: express.NextFunction) {
  const id = req.body['id'];
  // TODO: verify that id matches filename pattern (no .., no /, no \)
  // TODO: unregister least recently used object
  const o: O = root[id] ?? new intf();
  root[id] = o;

  o.id = id;
  o.filename = configuration.cachePath + o.filenameFromId(id);
  fs.writeFileSync(o.filename, req.file.buffer);
  o.sha256 = crypto.createHash('sha256').update(req.file.buffer).digest('hex');

  console.log('registering object '+o.id+' at '+o.filename+' sha '+o.sha256);

  next();
}

export function apiRegisterFile<O extends DebugObject> (intf: new () => O, root: { [id: string]: O }, id: string, file: Buffer) {
  // TODO: verify that id matches filename pattern (no .., no /, no \)
  // TODO: unregister least recently used object

  const o: O = root[id] ?? new intf();
  root[id] = o;

  o.id = id;
  o.filename = configuration.cachePath + o.filenameFromId(id);
  fs.writeFileSync(o.filename, file);
  o.sha256 = crypto.createHash('sha256').update(file).digest('hex');

  console.log('registering object '+o.id+' at '+o.filename+' sha '+o.sha256);
}