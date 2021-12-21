import chalk = require('chalk');
import express = require('express');
import { DebugObject, isValidId } from "../../../data";

export default function apiGet (data: { [id: string]: DebugObject }, req: express.Request, res: express.Response, next: express.NextFunction) {
  const id = req.query['id'] as string;
  if(!isValidId(id)) {
    res.sendStatus(400);
    return;
  }

  const o: DebugObject = data[id];
  if(!o) {
    console.error(chalk.red('  not found'));
    res.status(404).send(JSON.stringify({error: 'not found'}));
  } else {
    res.send(JSON.stringify(o));
  }
  next();
}
