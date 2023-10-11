import * as express from 'express';
import { DebugObject, isValidId, simplifyId } from "../../../data.js";
import * as fs from 'fs';
import chalk from 'chalk';

export default function apiUnregister<O extends DebugObject> (root:{ [id: string]: O }, req: express.Request, res: express.Response, next: express.NextFunction) {
  let id = req.body['id'];
  if(!isValidId(id)) {
    res.sendStatus(400);
    return;
  }

  id = simplifyId(id);

  const o = root[id];

  if(!o) {
    console.error(chalk.red('  unregister: object '+id+' not found'));
    res.sendStatus(404);
    return;
  }

  console.log(chalk.grey('  unregistering object '+o.id+' at '+o.filename+' sha '+o.sha256));

  if(fs.existsSync(o.filename)) {
    fs.unlinkSync(o.filename);
  }

  delete root[id];
  next();
};
