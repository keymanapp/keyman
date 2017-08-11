YAHOO.namespace('config');

function cmdSucceeded(o)
{
  var r = o.responseXML;
  return r.documentElement && r.documentElement.tagName != 'Fail';
}

function cmdFailure(o)
{
  var r = o.responseXML;
  if(r)
  {
    r = r.documentElement;
    if(r)
    {
      r = r.attributes.getNamedItem("Message");
      if(r)
      {
        dlgAlert(r.value);
        return;
      }
    }
  }
  dlgAlert("Failed to receive a valid response: "+o.statusText+"\n"+o.responseText);
}

function resizeLicenceDataTable()
{
  if(YAHOO.config.licenceDataTable)
	  YAHOO.config.licenceDataTable.set('height', (YAHOO.config.subLayout.getUnitByPosition('top').body.clientHeight - 72) + 'px');
}

function resizeComputerDataTable()
{
  if(YAHOO.config.computerDataTable)
    YAHOO.config.computerDataTable.set('height', (YAHOO.config.subLayout.getUnitByPosition('center').body.clientHeight - 72) + 'px');
}

function init() {
  initDlgLogin();
	initDlgEditLicence();
	initDlgAddLicence();
	initDlgEditComputer();
	initDlgAddComputer();
	initDlgLog();
	
	var layout = new YAHOO.widget.Layout({
	  units: [
	    { position: 'top', height: 44, body: 'panelTitle', collapse: false, resize: false, gutter: '5px 5px 0 5px' },
	    { position: 'center' } ]
	  });
	  
	layout.on('render', function() {
	  var el = layout.getUnitByPosition('center').get('wrap');
  	var layout2 = YAHOO.config.subLayout = new YAHOO.widget.Layout(el, {
  	  parent: layout,
	    units: [
	      { position: 'top', height: 180, body: 'panelLicences', header: 'Licences', resize: true, gutter: '5px' },
	      { position: 'bottom', height: 75, body: 'panelControl', header: 'Management', gutter: '5px' },
	      { position: 'center', body: 'panelComputers', header: 'Computers', gutter: '0 5px' }
	    ]
	  });
	  layout2.render();
  	layout2.getUnitByPosition('top').on('resize', resizeLicenceDataTable);
  	layout2.getUnitByPosition('center').on('resize', resizeComputerDataTable);
	  layout2.getUnitByPosition('center').resize();
	  layout2.getUnitByPosition('top').resize();
	});
 
	layout.render();
	
	/* fix for IE */
	var top = layout.getUnitByPosition('top');
	function resizeTop() {
	  top.header.style.display = 'none';
	  top.body.style.top = '0px';
	  top.body.style.height = '44';
	}
	top.subscribe('resize', resizeTop);
	resizeTop();

  login();
}

function dlgAlert(msg)
{
  var dlg = new YAHOO.widget.SimpleDialog("dlg", {
    //width: "30em",
    fixedcenter:true,
    modal:true,
    visible:false,
    draggable:true});
  dlg.setHeader("Error");
  dlg.setBody(msg);
  dlg.cfg.setProperty("icon", YAHOO.widget.SimpleDialog.ICON_ALARM);
  dlg.cfg.queueProperty("buttons", [ 
    { text: "OK", handler: function() { this.destroy(); }, isDefault: true }
    ]);
  dlg.render(document.body);
  dlg.show();
}

function dlgQuery(header, body, icon, onYes, onNo)
{
  var dlg = new YAHOO.widget.SimpleDialog("dlg", {
    width: "30em",
    fixedcenter:true,
    modal:true,
    visible:false,
    draggable:true});
  dlg.setHeader(header);
  dlg.setBody(body);
  dlg.cfg.setProperty("icon",icon == null ? YAHOO.widget.SimpleDialog.ICON_WARN : icon);
  dlg.cfg.queueProperty("buttons", [ 
    { text: "Yes", handler: function() { this.destroy(); if(onYes) onYes(); }, isDefault: true },
    { text: "No", handler: function() { this.destroy(); if(onNo) onNo(); } } ]);
  dlg.render(document.body);
  dlg.show();
}

function cmdDeleteLicence()
{
  var data = YAHOO.config.licenceDataTable.getRecord(YAHOO.config.licenceDataTable.getSelectedRows()[0]);
  YAHOO.config.licenceDataTable.disable();
  dlgQuery("Delete Licence", "Are you sure you want to delete the licence "+data.getData("LicenceKey")+" for product "+data.getData("LicenceProductName")+"?",
    null,
    function() { 
      YAHOO.util.Connect.asyncRequest(
        'POST',
        '/cmd/deletelicence',
        {
          success:function(o) {
            if(cmdSucceeded(o)) {
              // If Ok, do the change
              refreshLicences();
            } else {
              cmdFailure(o);
            }
            // unblock the interface
            YAHOO.config.licenceDataTable.undisable();
          },
          failure:function(o) {
            dlgAlert(o.statusText);
            YAHOO.config.licenceDataTable.undisable();
          },
          scope:YAHOO.config.licenceDataTable
        },
        // data to be sent to the server
        'licence='+escape(data.getData('LicenceKey'))+'&uph='+LoginHash
      );
    }
  );
}  

function cmdDeleteComputer()
{
  var licdata = YAHOO.config.licenceDataTable.getRecord(YAHOO.config.licenceDataTable.getSelectedRows()[0]);
  var data = YAHOO.config.computerDataTable.getRecord(YAHOO.config.computerDataTable.getSelectedRows()[0]);
  YAHOO.config.computerDataTable.disable();
  dlgQuery("Delete Computer", "Are you sure you want to delete the computer "+data.getData("Name")+" for the licence "+licdata.getData("LicenceKey")+"?",
    null,
    function() { 
      YAHOO.util.Connect.asyncRequest(
        'POST',
        '/cmd/deletecomputer',
        {
          success:function(o) {
            if(cmdSucceeded(o)) {
              // If Ok, do the change
              refreshLicences();
            } else {
              cmdFailure(o);
            }
            // unblock the interface
            YAHOO.config.computerDataTable.undisable();
          },
          failure:function(o) {
            dlgAlert(o.statusText);
            YAHOO.config.computerDataTable.undisable();
          },
          scope:YAHOO.config.computerDataTable
        },
        // data to be sent to the server
        'licence='+escape(licdata.getData('LicenceKey'))+'&computer='+escape(data.getData('Name'))+'&uph='+LoginHash
      );
    }
  );
}  

YAHOO.util.Event.onContentReady('licenceButtons', function() {
  YAHOO.config.btnAddLicence = new YAHOO.widget.Button( "btnAddLicence", {onclick: {fn: function() { YAHOO.config.dlgAddLicence.show() }}, disabled: true});
  YAHOO.config.btnEditLicence = new YAHOO.widget.Button( "btnEditLicence", {onclick: {fn: function() { YAHOO.config.dlgEditLicence.show() }}, disabled: true});
  YAHOO.config.btnDeleteLicence = new YAHOO.widget.Button( "btnDeleteLicence", {onclick: {fn: cmdDeleteLicence}, disabled: true});
});

YAHOO.util.Event.onContentReady('computerButtons', function() {
  YAHOO.config.btnAddComputer = new YAHOO.widget.Button( "btnAddComputer", {onclick: {fn: function() { YAHOO.config.dlgAddComputer.show() }}, disabled: true});
  YAHOO.config.btnEditComputer = new YAHOO.widget.Button( "btnEditComputer", {onclick: {fn: function() { YAHOO.config.dlgEditComputer.show() }}, disabled: true});
  YAHOO.config.btnDeleteComputer = new YAHOO.widget.Button( "btnDeleteComputer", {onclick: {fn: cmdDeleteComputer}, disabled: true});
});

YAHOO.util.Event.onContentReady('controlButtons', function() {
  YAHOO.config.btnProcessActivations = new YAHOO.widget.Button( "btnProcessActivations", {onclick: {fn: function() { YAHOO.config.dlgActivations.show(); }}, disabled: true});
  YAHOO.config.btnImportActivations = new YAHOO.widget.Button( "btnImportActivations", {onclick: {fn: function() { YAHOO.config.dlgImportActivations.show(); }}, disabled: true});
  YAHOO.config.btnLog = new YAHOO.widget.Button( "btnLog", {onclick: {fn: function() { showLog() }}, disabled: true});
});

function refreshLicences(lastSelectedLicence)
{
  YAHOO.config.licenceDataTable.disable();
  var data = YAHOO.config.licenceDataTable.getRecord(YAHOO.config.licenceDataTable.getSelectedRows()[0]);
  var lastSelectedLicence = data ? data.getData("LicenceKey") : null;
  
  var oCallback = {
    success : function(s,r,p)
    {
      YAHOO.config.licenceDataTable.undisable();
      YAHOO.config.licenceDataTable.onDataReturnInitializeTable(s,r,p);
      if(lastSelectedLicence)
      {
        var r = YAHOO.config.licenceDataTable.getRecordSet();
         
        for(var i = 0; i < r.getLength(); i++)
        {
          if(r.getRecord(i).getData("LicenceKey") == lastSelectedLicence)
          {
            YAHOO.config.licenceDataTable.selectRow(i);
            return;
          }
        }
      }
      YAHOO.config.licenceDataTable.selectRow(0);
    },
    failure : function(s,r,p) { YAHOO.config.licenceDataTable.undisable(); YAHOO.config.licenceDataTable.onDataReturnInitializeTable(s,r,p); },
    scope : YAHOO.config.licenceDataTable
  };
  YAHOO.config.licenceDataSource.sendRequest(null, oCallback);
}

function refreshComputers()
{
  YAHOO.config.computerDataTable.disable();
      
  var data = YAHOO.config.computerDataTable.getRecord(YAHOO.config.computerDataTable.getSelectedRows()[0]);
  var lastSelectedComputer = data ? data.getData("Name") : null;
  
  var oCallback = {
    success : function(s,r,p)
    {
      YAHOO.config.computerDataTable.undisable();
      YAHOO.config.computerDataTable.onDataReturnInitializeTable(s,r,p);
      if(lastSelectedComputer)
      {
        var r = YAHOO.config.computerDataTable.getRecordSet();
    
        for(var i = 0; i < r.getLength(); i++)
        {
          if(r.getRecord(i).getData("Name") == lastSelectedComputer)
          {
            YAHOO.config.computerDataTable.selectRow(i);
            return;
          }
        }
      }
      YAHOO.config.computerDataTable.selectRow(0);
    },
    failure : function(s,r,p) { YAHOO.config.computerDataTable.undisable(); YAHOO.config.computerDataTable.onDataReturnInitializeTable(s,r,p); },
    scope : YAHOO.config.computerDataTable
  };
  
  var licdata = YAHOO.config.licenceDataTable.getRecord(YAHOO.config.licenceDataTable.getSelectedRows()[0]);
  
  YAHOO.config.computerDataSource.sendRequest("?uph="+LoginHash+"&licence="+licdata.getData('LicenceKey'), oCallback);
}

function initDlgStandard(dlg,okButtonCaption)
{
  var submitCallback = function(obj) {
    if(cmdSucceeded(obj))
      refreshLicences();
    else cmdFailure(obj);
  }

  var submitFailure = function(obj) {
	  dlgAlert("Submission failed: " + obj.status);
  }
  
  dlg.callback.success = submitCallback;
	dlg.callback.failure = submitFailure;

	var listeners = new YAHOO.util.KeyListener(document, { keys : 27 }, {fn:function(){this.cancel()},scope:dlg,correctScope:true} );

	dlg.cfg.queueProperty("keylisteners", listeners);
	dlg.cfg.queueProperty("buttons", 
	  [ 
	    { text:okButtonCaption?okButtonCaption:"Save", handler: { fn:function(){this.submit();} }, isDefault: true }, 
	    { text:"Cancel", handler: { fn:function(){this.cancel();} } }
	   ]);
	dlg.render();
}

function initDlgEditLicence()
{
  var beforeShow = function(obj) {
    var data = YAHOO.config.licenceDataTable.getRecord(YAHOO.config.licenceDataTable.getSelectedRows()[0]);
    this.form.licence.value = data.getData("LicenceKey");
    this.form.licence_show.value = data.getData("LicenceKey");
    this.form.maxcomputers.value = data.getData("MaxComputers");
    this.form.allowautoregistration.checked = data.getData("AllowAutoRegistration") ? "checked" : "";
  }

	YAHOO.config.dlgEditLicence = new YAHOO.widget.Dialog("dlgEditLicence", { modal:true, visible:false, width:"350px", fixedcenter:true, constraintoviewport:true, draggable:true });
  YAHOO.config.dlgEditLicence.subscribe("beforeShow", beforeShow);

  initDlgStandard(YAHOO.config.dlgEditLicence);
}

function initDlgAddLicence()
{
  var beforeShow = function(obj) {
      this.form.enabled.checked = 'checked';
  }
  
	YAHOO.config.dlgAddLicence = new YAHOO.widget.Dialog("dlgAddLicence", { modal:true, visible:false, width:"350px", fixedcenter:true, constraintoviewport:true, draggable:true });
  YAHOO.config.dlgAddLicence.subscribe("beforeShow", beforeShow);
  initDlgStandard(YAHOO.config.dlgAddLicence);
}

function initDlgAddComputer()
{
  var beforeShow = function(obj) {
    var licdata = YAHOO.config.licenceDataTable.getRecord(YAHOO.config.licenceDataTable.getSelectedRows()[0]);
    this.form.licence.value = licdata.getData("LicenceKey");
    this.form.licence_show.value = licdata.getData("LicenceKey");
    this.form.enabled.checked = 'checked';
  }

	YAHOO.config.dlgAddComputer = new YAHOO.widget.Dialog("dlgAddComputer", { modal:true, visible:false, width:"450px", fixedcenter:true, constraintoviewport:true, draggable:true });
  YAHOO.config.dlgAddComputer.subscribe("beforeShow", beforeShow);
  initDlgStandard(YAHOO.config.dlgAddComputer);
}

function initDlgEditComputer()
{
  var beforeShow = function(obj) {
    var licdata = YAHOO.config.licenceDataTable.getRecord(YAHOO.config.licenceDataTable.getSelectedRows()[0]);
    var data = YAHOO.config.computerDataTable.getRecord(YAHOO.config.computerDataTable.getSelectedRows()[0]);
    this.form.licence.value = licdata.getData("LicenceKey");
    this.form.licence_show.value = licdata.getData("LicenceKey");
    this.form.computer.value = data.getData("Name");
    this.form.computer_show.value = data.getData("Name");
    this.form.ip.value = data.getData("IPAddress");
  }

	YAHOO.config.dlgEditComputer = new YAHOO.widget.Dialog("dlgEditComputer", { modal:true, visible:false, width:"450px", fixedcenter:true, constraintoviewport:true, draggable:true });
  YAHOO.config.dlgEditComputer.subscribe("beforeShow", beforeShow);
  initDlgStandard(YAHOO.config.dlgEditComputer);
}

function enableLicenceControls(enableLicenceButtons)
{  
  if(LoginAccessLevel > 1)
  {
    YAHOO.config.btnEditLicence.set('disabled', !enableLicenceButtons);
    YAHOO.config.btnDeleteLicence.set('disabled', !enableLicenceButtons);
    YAHOO.config.btnAddComputer.set('disabled', !enableLicenceButtons);
  }
}

function enableComputerControls(enableComputerButtons)
{  
  if(LoginAccessLevel > 1)
  {
    YAHOO.config.btnEditComputer.set('disabled', !enableComputerButtons);
    YAHOO.config.btnDeleteComputer.set('disabled', !enableComputerButtons);
  }
}



function initData(accessLevel)
{
  YAHOO.widget.DataTable.Formatter['bool'] = function(elCell,oRecord,oColumn,oData) { elCell.innerHTML = oData?'Yes':'No' };
  
  var activationStatusFormatter = YAHOO.widget.DataTable.Formatter['activationStatus'] = function(elCell,oRecord,oColumn,oData) {
    elCell.style.color = '';
    switch(oData)
    {
      case 0: 
        if(oRecord.getData("ActivationRequestBlob") != "") 
          if(oRecord.getData("ActivationResponseBlob") != "") 
            elCell.innerHTML = 'Ready to activate';
          else 
            elCell.innerHTML = 'Awaiting activation response';
        else
          elCell.innerHTML = 'Never activated'; 
        break;
      case 1: elCell.innerHTML = 'Activation failed'; elCell.style.color = 'red'; break;
      case 2:
        var s = 'Activated';
        var date = oRecord.getData("ActivationDate");
        if(date != null) s += ' '+YAHOO.util.Date.format(date, {format: '%Y-%m-%d %H:%M'});
        elCell.innerHTML = s; break;
      default: elCell.innerHTML = oData;
    }
  };
  
  var createTypeFormatter = YAHOO.widget.DataTable.Formatter['createType'] = function(elCell,oRecord,oColumn,oData) {
    switch(oData)
    {
      case 0: elCell.innerHTML = 'Yes'; break;
      case 1: elCell.innerHTML = 'No'; break;
      default: elCell.innerHTML = oData;
    }
  }
  
  YAHOO.util.DataSource.Parser['sqlDate'] = function(oData) {
    var parts = oData.split(' ');
    if(parts[0] == "") return null;
    var datePart = parts[0].split('-');
    if(parts.length > 1) {
      var timePart = parts[1].split(':');
      return new Date(datePart[0], datePart[1]-1, datePart[2], timePart[0], timePart[1]);
    }
    return new Date(datePart[0], datePart[1]-1, datePart[2]);
  };
  
  var licenceDataSource = YAHOO.config.licenceDataSource = new YAHOO.util.XHRDataSource('/xml/computers?uph='+LoginHash);
  licenceDataSource.responseType = YAHOO.util.XHRDataSource.TYPE_XML;
  licenceDataSource.responseSchema = {
    resultNode: 'Licence',
    fields : [
      { key: "MaxComputers", parser: "number" },
      { key: "AllowAutoRegistration", parser: "number" },
      { key: "LicenceKey" }, 
      { key: "Enabled", parser: "number" },
      { key: "LicenceProductID", parser: "number" },
      { key: "LicenceSeatCount", parser: "number" },
      { key: "LicenceProductName" },
      { key: "StatusTotalComputers", parser: "number" },
      { key: "StatusEnabledComputers", parser: "number" },
      { key: "StatusActivatedComputers", parser: "number" },
      { key: "StatusFailedComputers", parser: "number" }
    ] };
    
  var computerDataSource = YAHOO.config.computerDataSource = new YAHOO.util.XHRDataSource('/xml/computers');
  computerDataSource.responseType = YAHOO.util.XHRDataSource.TYPE_XML;
  computerDataSource.responseSchema = {
    resultNode: 'Computer',
    fields : [
      { key: "Name" },
      { key: "CreateDate", parser: 'sqlDate' },
      { key: "LastRequestDate", parser: 'sqlDate' }, 
      { key: "ActivationStatus", parser: "number" },
      { key: "ActivationDate", parser: "sqlDate" },
      { key: "IPAddress" },
      { key: "CreateType", parser: "number" },
      { key: "Enabled", parser: "number" },
      { key: "ActivationRequestBlob" },
      { key: "ActivationResponseBlob" }
    ] };
    
  var licenceColumnDefs = [
    { key: "Enabled", label: "Enabled", sortable: true, formatter: accessLevel > 0 ? 'checkbox' : 'bool' },
    { key: "LicenceProductName", label: "Product", sortable: true, formatter: "text" },
    { key: "LicenceKey", label: "Licence Key", sortable: true, formatter: "text" },
    { key: "AllowAutoRegistration", label: "Automatic Registration", sortable: true, formatter: 'bool' },
    { key: "MaxComputers", label: "Max Seats", sortable: true, formatter: "number" },
    //{ key: "SeatStatus",  children: [
      { key: "StatusEnabledComputers", label: "In Use", sortable: true, formatter: "number" }, // Technically this is "enabled" not "in use" but...
      { key: "StatusActivatedComputers", label: "Activated", sortable: true, formatter: "number" },
      { key: "StatusFailedComputers", label: "Failed", sortable: true, formatter: "number" }
    //] }
  ];
  
  var computerColumnDefs = [
    { key: "Enabled", label: "Enabled", formatter: accessLevel > 0 ? 'checkbox' : 'bool' },
    { key: "Name", label: "Name", sortable: true, formatter: "text" },
    { key: "CreateDate", label: "Created", formatter: 'date', dateOptions: { format: '%Y-%m-%d %H:%M' } },
    { key: "LastRequestDate", label: "Last Contact", formatter: 'date', dateOptions: { format: '%Y-%m-%d %H:%M' } },
    { key: "ActivationStatus", label: "Activation Status", formatter: activationStatusFormatter },
    { key: "IPAddress", label: "IP Address" },
    { key: "CreateType", label: "Automatically Registered", formatter: createTypeFormatter }
  ];
  
  var computerDataTable = YAHOO.config.computerDataTable = new YAHOO.widget.ScrollingDataTable("computers", computerColumnDefs, computerDataSource, {initialLoad: false});
  computerDataTable.subscribe("rowClickEvent", computerDataTable.onEventSelectRow);

  var licenceDataTable = YAHOO.config.licenceDataTable = new YAHOO.widget.ScrollingDataTable("licences", licenceColumnDefs, licenceDataSource);
  licenceDataTable.subscribe("rowClickEvent", licenceDataTable.onEventSelectRow);
  
  licenceDataTable.subscribe("dataReturnEvent", function() { enableLicenceControls(false) });
  computerDataTable.subscribe("dataReturnEvent", function() { enableComputerControls(false) });
  
  licenceDataTable.subscribe("rowSelectEvent", function(oArgs) {
    var el = oArgs.el, record = oArgs.record;
    
    enableLicenceControls(record != null);
      
    refreshComputers();
    }
  );
  
  if(accessLevel > 0) licenceDataTable.subscribe('checkboxClickEvent', function(oArgs) {
    YAHOO.util.Event.preventDefault(oArgs.event);

    this.disable();

    var elCheckbox = oArgs.target,
      newValue = elCheckbox.checked,
      record = this.getRecord(elCheckbox),
      column = this.getColumn(elCheckbox),
      recordIndex = this.getRecordIndex(record);

    if(column.key != "Enabled") { this.undisable(); return; }
    
    YAHOO.util.Connect.asyncRequest(
      'POST',
      newValue ? '/cmd/enablelicence' : '/cmd/disablelicence', 
      {
        success:function(o) {
          if(cmdSucceeded(o)) {
            // If Ok, do the change
            var data = record.getData();
            data[column.key] = newValue;
            this.updateRow(recordIndex,data);
          } else {
            cmdFailure(o);
          }
          // unblock the interface
          this.undisable();
        },
        failure:function(o) {
          dlgAlert(o.statusText);
          this.undisable();
        },
        scope:this
      },
      // data to be sent to the server
      'licence='+escape(record.getData('LicenceKey'))+'&uph='+LoginHash
    );                                              
  });
  
  computerDataTable.subscribe("rowSelectEvent", function(oArgs) {
    var el = oArgs.el, record = oArgs.record;
    
    enableComputerControls(record != null);
    }
  );
  
  if(accessLevel > 0) computerDataTable.subscribe('checkboxClickEvent', function(oArgs) {
    YAHOO.util.Event.preventDefault(oArgs.event);

    this.disable();

    var elCheckbox = oArgs.target,
      newValue = elCheckbox.checked,
      record = this.getRecord(elCheckbox),
      column = this.getColumn(elCheckbox),
      recordIndex = this.getRecordIndex(record);
      
    var licdata = YAHOO.config.licenceDataTable.getRecord(YAHOO.config.licenceDataTable.getSelectedRows()[0]);

    if(column.key != "Enabled") { this.undisable(); return; }
    
    YAHOO.util.Connect.asyncRequest(
      'POST',
      newValue ? '/cmd/enablecomputer' : '/cmd/disablecomputer', 
      {
        success:function(o) {
          if(cmdSucceeded(o)) {
            // If Ok, do the change
            var data = record.getData();
            data[column.key] = newValue;
            this.updateRow(recordIndex,data);
          } else {
            cmdFailure(o);
          }
          // unblock the interface
          this.undisable();
        },
        failure:function(o) {
          dlgAlert(o.statusText);
          this.undisable();
        },
        scope:this
      },
      // data to be sent to the server
      'licence='+escape(licdata.getData('LicenceKey'))+'&computer='+escape(record.getData('Name'))+'&uph='+LoginHash
    );                                              
  });
  
  resizeLicenceDataTable();
  resizeComputerDataTable();
}

function initDlgLog()
{
}

function showLog()
{
  window.open('/xml/log?uph='+LoginHash, '_blank');
}

YAHOO.util.Event.addListener(window, "load", init);

