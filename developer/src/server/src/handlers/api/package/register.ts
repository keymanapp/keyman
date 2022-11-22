import chalk = require('chalk');
import express = require('express');
import { data, DebugPackage } from "../../../data";

export default function apiPackageRegister (req: express.Request, res: express.Response, next: express.NextFunction) {
  const kmp: DebugPackage = data.packages[req.body['id']];
  if(!kmp) {
    console.error(chalk.red('unexpected missing package'));
    return;
  }
  kmp.name = req.body['name'];
  next();
}
