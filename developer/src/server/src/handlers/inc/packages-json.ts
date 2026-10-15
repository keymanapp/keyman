/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * API: return list of packages under testing and urls for installing in mobile
 * apps
 */
import * as express from 'express';
import { KeymanUrls } from '@keymanapp/developer-utils';
import KEYMAN_VERSION from '@keymanapp/keyman-version';
import { data } from "../../data.js";

export default function handleIncPackagesJson (req: express.Request, res: express.Response) {
  const packages = Object.keys(data.packages).map(id => { return { id: id, filename: id+'.kmp', name: data.packages[id].name} });
  res.send({
    packages: packages,
    urls: {
      installLinkAndroid: KeymanUrls.KeymanDeveloper_KeymanForAndroidDownload(KEYMAN_VERSION.VERSION_RELEASE),
      installLinkIos: KeymanUrls.KeymanDeveloper_KeymanForIosDownload(KEYMAN_VERSION.VERSION_RELEASE),
    }
  });
}
