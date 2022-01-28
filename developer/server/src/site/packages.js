const menuDropdown = new DropdownMenu('menu');
let packages = null;
let packagesJSON = null;
let helpUrl = '';
let versionMajor = '15.0'; // will be updated from the server below

fetch('/api-public/version').
  then(response => response.json()).
  then(value => {
    const versionMajorRx = /^(\d+\.\d+)/.exec(value.version);
    versionMajor = versionMajorRx[1];
    helpUrl = 'https://help.keyman.com/developer/'+versionMajor+'/context/server';
    document.getElementById('about-version').innerText = value.version;
    document.getElementById('about-help-link').href = helpUrl;
    document.getElementById('keyman-developer-logo').title = 'Keyman Developer Server '+value.version;
  });

menuDropdown.onclick = (value) => {
  menuDropdown.set(''); // we never show an 'active' package
  if(value == '#install-keyman') {
    let href = '';
    switch(keyman.util.device.OS) {
      case 'iOS':     href = 'https://keyman.com/go/developer/'+versionMajor+'/ios-app'; break;
      case 'Android': href = 'https://keyman.com/go/developer/'+versionMajor+'/android-app'; break;
      case 'Linux':   href = 'https://keyman.com/linux/download'; break;
      case 'Windows': href = 'https://keyman.com/go/download/keyman-windows'; break;
      case 'MacOSX':  href = 'https://keyman.com/go/download/keyman-mac'; break;
      default:        href = 'https://keyman.com/downloads'; break;
    }
    location.href = href;
  } else if(value == '#upload') {
    document.getElementById('drop-file').click();
  } else if(value == '#help') {
    window.open(helpUrl);
  } else if(value == '#about') {
    let aboutModal = new bootstrap.Modal(document.getElementById('about-modal'));
    aboutModal.show();
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

  menuDropdown.removeAll();
  menuDropdown.add('#install-keyman', 'Download and install Keyman');
  menuDropdown.addDivider();
  for(var i = 0; i < data.packages.length; i++) {
    let name = 'Install ' + (data.packages[i].name ? data.packages[i].name + ' (' + data.packages[i].filename + ')' : data.packages[i].filename);
    menuDropdown.add(data.packages[i].id, name);
  }
  menuDropdown.addDivider();
  menuDropdown.add('#upload', 'Upload file...');
  menuDropdown.addDivider();
  menuDropdown.add('#help', 'Help...');
  menuDropdown.add('#about', 'About Keyman Developer Server');
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
