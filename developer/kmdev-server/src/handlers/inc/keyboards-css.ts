import express = require('express');
import { data, SiteData } from "../../data";

export default function handleIncKeyboardsCss (req: express.Request, res: express.Response) {
  let headers = {"Content-Type": "text/css"};
  res.writeHead(200, headers);
  //*/
  //response.
  res.write(emitCSS(data));
  res.end();

};

function emitCSS(data: SiteData) {
  let response = '/* Dynamic fonts */';

  for(let id in data.fonts) {
    const font = data.fonts[id];
    response += `
    @font-face {
      font-family: ${JSON.stringify(font.facename)}
      src:         local(${JSON.stringify(font.facename)}),
                   url(${JSON.stringify('/data/font/'+font.filename)}) format("truetype");
      font-weight: normal;
      font-style:  normal;
    }

    `;
  }

  return response;
}
