var
  IsLoggedIn = false,
  LoginHash = '',
  LoginAccessLevel;

var setupLogin = function(accessLevel)
{
  accessLevel = parseInt(accessLevel);

  // Clear out the username and password from memory (as far as we can...)
  //document.getElementById('dlgLogin_Password').value = '';
  //document.getElementById('dlgLogin_Username').value = '';
  
  YAHOO.config.dlgLogin.hide();
  YAHOO.config.dlgLogin.destroy();
  
  document.getElementById('dlgAddLicence_uph').value=LoginHash;
  document.getElementById('dlgEditLicence_uph').value=LoginHash;
  document.getElementById('dlgAddComputer_uph').value=LoginHash;
  document.getElementById('dlgEditComputer_uph').value=LoginHash;
  document.getElementById('dlgActivations_uph').value=LoginHash;
  document.getElementById('dlgImportActivations_uph').value=LoginHash;
  
  switch(accessLevel)
  {
  case 2:
    YAHOO.config.btnAddLicence.set('disabled', false);
    /*YAHOO.config.btnEditLicence.set('disabled', false);
    YAHOO.config.btnDeleteLicence.set('disabled', false);
    YAHOO.config.btnAddComputer.set('disabled', false);
    YAHOO.config.btnEditComputer.set('disabled', false);
    YAHOO.config.btnDeleteComputer.set('disabled', false);*/
    YAHOO.config.btnLog.set('disabled', false);
  case 1:
    YAHOO.config.btnProcessActivations.set('disabled', false);
    YAHOO.config.btnImportActivations.set('disabled', false);
    break;
  case 0:
    break;
  }
  
  LoginAccessLevel = accessLevel;
  
  YAHOO.util.Cookie.set("loginHash", LoginHash);
  YAHOO.util.Cookie.set("loginAccessLevel", LoginAccessLevel);
  
	initData(accessLevel);
  initActivations();
}

function initDlgLogin()
{
  
  var submitCallback = function(o)
  {
    /* Ask the user to confirm that they will send the blobs */
    
    var data = YAHOO.config.dlgLogin.getData();
    
    LoginHash = b64_rmd160(data.username+':'+data.password+':TavultesoftActivationServer');
    
    YAHOO.util.Connect.asyncRequest(
      'POST',
      '/cmd/login',
      {
        success:function(o) {
          if(cmdSucceeded(o)) {
            // If Ok, do the change
            var e = o.responseXML.documentElement.attributes.getNamedItem('AccessLevel');
            if(!e) cmdFailure(o); else setupLogin(e.value);
          } else {
            cmdFailure(o);
          }
        },
        failure:function(o) {
          dlgAlert(o.statusText);
        },
        scope:YAHOO.config.dlgLogin
      },
      // data to be sent to the server
      'uph='+LoginHash
    );
  }
  
	YAHOO.config.dlgLogin = new YAHOO.widget.Dialog("dlgLogin", { modal:true, close:false, visible:false, width:"220px", fixedcenter:true, constraintoviewport:true, draggable:true });
  YAHOO.config.dlgLogin.cfg.queueProperty('postmethod','none');
  YAHOO.config.dlgLogin.cfg.queueProperty('hideaftersubmit',false);
  YAHOO.config.dlgLogin.subscribe('manualSubmit', submitCallback);

	YAHOO.config.dlgLogin.cfg.queueProperty("buttons", 
	  [ 
	    { text:'Login', handler: { fn:function() { this.submit(); } }, isDefault: true }
	  ]);
  
	YAHOO.config.dlgLogin.render();
}

function login()
{
  var _LoginHash = YAHOO.util.Cookie.get("loginHash");
  var _LoginAccessLevel = YAHOO.util.Cookie.get("loginAccessLevel");
  if(LoginHash != null && _LoginAccessLevel != null)
  {
    IsLoggedIn = true;
    LoginHash = _LoginHash;
    setupLogin(_LoginAccessLevel);
  }
  else YAHOO.config.dlgLogin.show();
}

