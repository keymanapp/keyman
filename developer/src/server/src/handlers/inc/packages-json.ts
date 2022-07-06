import express = require('express');
import { data } from "../../data";
import { environment } from '../../environment';

export default function handleIncPackagesJson (req: express.Request, res: express.Response) {
  const packages = Object.keys(data.packages).map(id => { return { id: id, filename: id+'.kmp', name: data.packages[id].name} });
  res.send({
    packages: packages,
    urls: {
      installLinkAndroid: makeKeymanURL(URLPath_KeymanDeveloper_KeymanForAndroidDownload),
      installLinkIos: makeKeymanURL(URLPath_KeymanDeveloper_KeymanForIosDownload),
    }
  });
}

const URLPath_KeymanDeveloper_KeymanForAndroidDownload = '/go/developer/'+environment.versionRelease+'/android-app'
const URLPath_KeymanDeveloper_KeymanForIosDownload = '/go/developer/'+environment.versionRelease+'/ios-app'

function makeKeymanURL(base: string) {
  return 'https://keyman.com' + base;
}
