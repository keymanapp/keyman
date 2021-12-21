import express = require('express');
import { data } from "../../data";

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

const SKeymanVersion = '15.0'; //TODO
const URLPath_KeymanDeveloper_KeymanForAndroidDownload = '/go/developer/'+SKeymanVersion+'/android-app'
const URLPath_KeymanDeveloper_KeymanForIosDownload = '/go/developer/'+SKeymanVersion+'/ios-app'

function makeKeymanURL(base: string) {
  return 'https://keyman.com' + base;
}
