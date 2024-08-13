import * as express from 'express';
import * as path from 'path';
import { data, SiteData } from "../../data.js";

export default function handleIncKeyboardsCss (req: express.Request, res: express.Response) {
  let headers = {"Content-Type": "text/css"};
  res.writeHead(200, headers);
  res.write(emitCSS(data));
  res.end();

};

function emitCSS(data: SiteData) {
  let response = '/* Dynamic fonts */';

  for(let id in data.fonts) {
    const font = data.fonts[id];
    const filename = path.basename(font.filename.replace(path.sep=='/'?'\\':'/',path.sep));
    response += `
    @font-face {
      font-family: ${JSON.stringify(font.facename)};
      src:         local(${JSON.stringify(font.facename)}),
                   url(${JSON.stringify('/data/font/'+filename)}) format("truetype");
      font-weight: normal;
      font-style:  normal;
    }

    `;
  }

  return response;
}
