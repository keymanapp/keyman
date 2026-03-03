//TODO: move to
  export function cleanKeyboardID(name: string): string {
  name = name.toLowerCase();
  if(name.length == 0) {
    return name;
  }
  if(name[0].match(/\d/)) {
    name = '_' + name;
  }

  let result = '';
  for(let i = 0; i < name.length; i++) {
    if(!name[i].match(/[a-z0-9_]/)) {
      result += '_';
    } else {
      result += name[i];
    }
  }
  return result;
}
