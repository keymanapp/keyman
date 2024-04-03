import { KeymanUrls } from '@keymanapp/developer-utils';
import * as express from 'express';
import { data } from "../../data.js";
import { environment } from '../../environment.js';

export default function handleIncPackagesJson (req: express.Request, res: express.Response) {
  const packages = Object.keys(data.packages).map(id => { return { id: id, filename: id+'.kmp', name: data.packages[id].name} });
  res.send({
    packages: packages,
    urls: {
      installLinkAndroid: KeymanUrls.KeymanDeveloper_KeymanForAndroidDownload(environment.versionRelease),
      installLinkIos: KeymanUrls.KeymanDeveloper_KeymanForIosDownload(environment.versionRelease),
    }
  });
}
