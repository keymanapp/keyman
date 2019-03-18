const fs = require('fs');
const path = require('path');

module.exports = {
  getKeyboardFolders: function(root) {
    let shortnames = fs.readdirSync(root);
    let keyboards = [];
    shortnames.forEach(function(shortname) {
      let kbds = fs.readdirSync(path.join(root, shortname));
      if(kbds && kbds.length) {
        kbds.forEach(function(kbd) {
          if(fs.existsSync(path.join(root, shortname, kbd, 'build', kbd+'.tests')) &&
              fs.existsSync(path.join(root, shortname, kbd, 'build', kbd+'.js'))) {
            keyboards.push({s:shortname, id: kbd});
          }
        });
      }
    });
    return keyboards;
  }
}
