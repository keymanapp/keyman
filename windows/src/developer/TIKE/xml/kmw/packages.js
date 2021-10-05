// The following dynamic script will register each of the additional packages
var packagesJSON = '';

function ajaxRequest(){
  var activexmodes=["Msxml2.XMLHTTP", "Microsoft.XMLHTTP"] //activeX versions to check for in IE
  if (window.ActiveXObject) { //Test for support for ActiveXObject in IE first (as XMLHttpRequest in IE7 is broken)
    for (var i=0; i<activexmodes.length; i++) {
      try {
        return new ActiveXObject(activexmodes[i]);
      } catch(e) {
        //suppress error
      }
    }
  } else if (window.XMLHttpRequest) { // if Mozilla, Safari etc
    return new XMLHttpRequest();
  }
 return false;
}

function updatePackages(data) {
  var dataJSON = JSON.stringify(data);
  if(packagesJSON !== dataJSON) {
    packagesJSON = dataJSON;

    if(data.urls) {
      document.getElementById('install-link-ios').href = data.urls.installLinkIos;
      document.getElementById('install-link-android').href = data.urls.installLinkAndroid;
    }

    if(!data.packages) {
      return false;
    }

    var ul = document.getElementById('packages');
    ul.innerHTML = '';
    if(data.packages.length == 0) {
      var li = document.createElement('li');
      li.innerText = 'No packages available.';
      ul.appendChild(li);
      return true;
    }

    for(var i = 0; i < data.packages.length; i++) {
      var li = document.createElement('li');
      var a = document.createElement('a');
      a.href = '/package/'+data.packages[i].id;
      a.innerText = data.packages[i].name;
      li.appendChild(a);
      ul.appendChild(li);
    }
  }
}

function checkPackages() {
  var req=new ajaxRequest();
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

window.setInterval(checkPackages, 2000);
