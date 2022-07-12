import chalk = require('chalk');
import express = require('express');
import { data, DebugKeyboard } from "../../../data";

export default function apiKeyboardRegister (req: express.Request, res: express.Response, next: express.NextFunction) {
  const keyboard: DebugKeyboard = data.keyboards[req.body['id']];
  if(!keyboard) {
    console.error(chalk.red('unexpected missing keyboard'));
    return;
  }
  keyboard.fontFace = req.body['fontFace'];
  keyboard.oskFontFace = req.body['oskFontFace'];
  next();
}
