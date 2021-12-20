import express = require('express');
import { DebugObject } from "../../../data";

export default function apiGet (data: { [id: string]: DebugObject }, req: express.Request, res: express.Response, next: express.NextFunction) {
  const id = req.query['id'] as string;
  // TODO: verify that id matches filename pattern (no .., no /, no \)
  const o: DebugObject = data[id];
  if(!o) {
    console.error('  not found');
    res.status(404).send(JSON.stringify({error: 'not found'}));
  } else {
    res.send(JSON.stringify(o));
  }
  next();
}
