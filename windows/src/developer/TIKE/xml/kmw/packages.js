// The following dynamic script will register each of the additional packages
var packagesJSON = '';

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

    var tbody = document.getElementById('packages');
    tbody.innerHTML = '';
    if(data.packages.length == 0) {
      var tr = document.createElement('tr');
      var td = document.createElement('td');
      td.colSpan = 2;
      td.innerText = 'No packages available.';
      tr.appendChild(td);
      tbody.appendChild(tr);
      return true;
    }

    for(var i = 0; i < data.packages.length; i++) {
      var tr = document.createElement('tr');
      var tdname = document.createElement('td');
      var tdid = document.createElement('td');
      var a = document.createElement('a');
      a.href = '/package/'+data.packages[i].id;
      a.title = data.packages[i].filename;
      a.innerText = data.packages[i].name;
      tdname.appendChild(a);
      var a = document.createElement('a');
      a.href = '/package/'+data.packages[i].id;
      a.title = data.packages[i].filename;
      a.innerText = data.packages[i].id;
      tdid.appendChild(a);
      tr.appendChild(tdname);
      tr.appendChild(tdid);
      tbody.appendChild(tr);
    }
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

window.setInterval(checkPackages, 2000);
