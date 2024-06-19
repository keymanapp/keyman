import * as express from 'express';
import chalk from 'chalk';
import { DebugObject, isValidId, simplifyId } from "../../../data.js";

export default function apiGet (data: { [id: string]: DebugObject }, req: express.Request, res: express.Response, next: express.NextFunction) {
  let id = req.query['id'] as string;
  if(!isValidId(id)) {
    res.sendStatus(400);
    return;
  }

  id = simplifyId(id);

  const o: DebugObject = data[id];
  if(!o) {
    console.error(chalk.red(id+' not found'));
    res.status(404).send(JSON.stringify({error: id+' not found'}));
  } else {
    res.send(JSON.stringify(o));
  }
  next();
}
