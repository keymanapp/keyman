
export interface ETLResult {
  js: string;
  touchLayout: string;
}

export function extractTouchLayout(js: string): ETLResult|null  {
  let m = /KVKL=(?<kvkl>.+?);[\r\n]/ds.exec(js);
  if(!m) {
    return {
      js: js,
      touchLayout: 'null'
    };
  }

  let kvkl = (<any>m).indices.groups.kvkl;

  return {
    js: js.substring(0, kvkl[0]) + 'null' + js.substring(kvkl[1]),
    touchLayout: m.groups?.['kvkl'] ?? ''
  };
}
