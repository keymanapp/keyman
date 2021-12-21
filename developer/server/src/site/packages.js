/*
 TODO: predicate the download link on platform

*/

const packageDropdown = new DropdownMenu('package');
let packages = null;
let packagesJSON = null;

packageDropdown.onclick = (value) => {
  packageDropdown.set(''); // we never show an 'active' package
  if(value == '#install-keyman') {
    let href = '';
    switch(keyman.util.device.OS) {
      case 'iOS':     href = 'https://keyman.com/go/developer/15.0/ios-app'; break;
      case 'Android': href = 'https://keyman.com/go/developer/15.0/android-app'; break;
      case 'Linux':   href = 'https://keyman.com/linux/download'; break;
      case 'Windows': href = 'https://keyman.com/go/download/keyman-windows'; break;
      case 'MacOSX':  href = 'https://keyman.com/go/download/keyman-mac'; break;
      default:        href = 'https://keyman.com/downloads'; break;
    }
    location.href = href;
  } else {
    location.href = '/data/package/'+value+'.kmp';
  }
};

function updatePackages(data) {
  var dataJSON = JSON.stringify(data);
  if(packagesJSON === dataJSON) return;

  packagesJSON = dataJSON;
  packages = data;

  if(!data.packages) {
    return false;
  }

  packageDropdown.removeAll();

  packageDropdown.add('#install-keyman', 'Download and install Keyman');
  packageDropdown.addDivider();

  for(var i = 0; i < data.packages.length; i++) {
    let name = 'Install ' + data.packages[i].name ? data.packages[i].name + ' (' + data.packages[i].filename + ')' : data.packages[i].filename;
    packageDropdown.add(data.packages[i].id, name);
  }
}

function checkPackages() {
  var req=new XMLHttpRequest();
  req.onreadystatechange = function() {
    if (req.readyState==4) {
      if (req.status==200) {
        updatePackages(JSON.parse(req.responseText));
      }
    }
  }
  req.open("GET", "/inc/packages.json", true);
  req.send(null);
}

checkPackages();

// TODO: link to websocket