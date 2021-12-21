import express = require('express');
import {  DebugObject } from "../../../data";
import fs = require('fs');
import chalk = require('chalk');

export default function apiUnregister<O extends DebugObject> (root:{ [id: string]: O }, req: express.Request, res: express.Response, next: express.NextFunction) {
  const id = req.body['id'];
  // TODO: verify that id matches filename pattern (no .., no /, no \)
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
