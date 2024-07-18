import * as express from 'express';
import { SiteData, data } from "../../data.js";

export default function handleIncKeyboardsJs (req: express.Request, res: express.Response) {
  let headers = {"Content-Type": "application/javascript"};
  res.writeHead(200, headers);
  res.write(emitJavascript(data));
  res.end();
}


function emitJavascript(data: SiteData) {
  let response =
    '(function() {\n'+
    '  var kmw=KeymanWeb;\n';

  // TODO: refactor so it's client side code with XmlHttpRequest style call

  for(let keyboard in data.keyboards) {
    response += 'kmw.KRS('+data.keyboards[keyboard].toRegistrationBlob()+');\n';
  }

  for(let model in data.models) {
    response += 'registerModel('+data.models[model].toRegistrationBlob()+');\n';
  }

  response += '})();';

  return response;
}
