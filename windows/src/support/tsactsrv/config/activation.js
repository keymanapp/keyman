var globalActivationResponse = new Array();

function initActivations()
{
	initDlgActivations();
  initDlgActivationsReview();
  initDlgImportActivations();
}

function initDlgActivations()
{
  var prepareActivations = function(o)
  {
    /* Ask the user to confirm that they will send the blobs */
    YAHOO.config.activationsData = o.responseText;
    initDlgActivationsReviewData(o.responseXML);
  }
  
	YAHOO.config.dlgActivations = new YAHOO.widget.Dialog("dlgActivations", { modal:true, visible:false, width:"450px", fixedcenter:true, constraintoviewport:true, draggable:true });
  YAHOO.config.dlgActivations.cfg.queueProperty('hideaftersubmit',false);

  initDlgStandard(YAHOO.config.dlgActivations, 'Continue');
  
  YAHOO.config.dlgActivations.callback.success = function(obj) {
    if(cmdSucceeded(obj))
      prepareActivations(obj);
    else cmdFailure(obj);
  }

}

function initDlgActivationsReview()
{
  var fulfillActivations = function(o)
  {
    /* Save the activation response blobs returned in globalActivationResponse[sCookie] back to the server */
    YAHOO.config.licenceDataTable.disable();
    
    YAHOO.util.Connect.asyncRequest(
      'POST',
      '/cmd/saveactivationresponse',
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
      'activationresponseblob='+escape(o)+'&uph='+LoginHash
    );
    
    //alert(o);
    /*if(!o.licences) { dlgAlert('Invalid response from Tavultesoft'); return; }
    for(var i = 0; i < o.licences.length; i++)
    {
      var licence = o.licences[i];
      for(var j = 0; j < licence.computers.length; j++)
      {
      }
    }*/
  }
  
  var processActivations = function(o)
  {
    /* Send the activation request blobs to the Tavultesoft online activation server */
    
    var method = YAHOO.config.dlgActivations.getData().method;
    var target = YAHOO.config.dlgActivations.getData().target;
    
    if(method == 'upload')
    {
      var sCookie = Math.random() * new Date().getTime();
      var iframe = document.createElement('iframe');
      iframe.style.display='none';
      document.body.appendChild(iframe);
      var wnd = iframe.contentWindow;
      var doc = wnd.document;
      var s = 
        '<html><head><title>Uploading to Tavultesoft</title></head><body>'+
          '<form method="post" action="https://secure.tavultesoft.com/prog/70/activation_server.php">'+
          '<input type="hidden" name="cookie" value="'+sCookie+'" />'+
          '<textarea id="xml" name="xml" cols="1" rows="1"></textarea>'+
          '</form>'+
        '</body></html>';
      doc.write(s);
      doc.close();
      wnd.onunload = function()
      {
        var objGet = YAHOO.util.Get.script('https://secure.tavultesoft.com/prog/70/activation_server_response.php?cookie='+sCookie,
          {
            onSuccess: function() { fulfillActivations(globalActivationResponse[sCookie]); iframe.parentNode.removeChild(iframe); YAHOO.config.dlgActivations.hide(); YAHOO.config.dlgActivationsReview.hide(); },
            onFailure: function() { dlgAlert("Failed to retrieve activation response"); iframe.parentNode.removeChild(iframe); YAHOO.config.dlgActivations.hide(); YAHOO.config.dlgActivationsReview.hide(); }
          });
      }

      var e = doc.getElementById('xml');
      e.value = YAHOO.config.activationsData;
      e.form.submit();
    }
    else
    {
      YAHOO.config.dlgActivations.hide();
      YAHOO.config.dlgActivationsReview.hide();
      window.open('/xml/computers?target='+target+'&action=save&uph='+LoginHash, '_blank');
    }
  }

  var submitCallback = function(obj) {
    processActivations(obj);
  }

	YAHOO.config.dlgActivationsReview = new YAHOO.widget.Dialog("dlgActivationsReview", { modal:true, visible:false, fixedcenter:true, constraintoviewport:true, draggable:true });
  YAHOO.config.dlgActivationsReview.cfg.queueProperty('postmethod','none');
  YAHOO.config.dlgActivationsReview.cfg.queueProperty('hideaftersubmit',false);
  YAHOO.config.dlgActivationsReview.subscribe('manualSubmit', submitCallback);
  initDlgStandard(YAHOO.config.dlgActivationsReview, 'Process Activation');
}

function initDlgActivationsReviewData(obj)
{  
  var activationsDataSource = YAHOO.config.activationsDataSource = new YAHOO.util.LocalDataSource(obj); //, {responseType: YAHOO.util.LocalDataSource.TYPE_XML});
  //activationsDataSource.responseType = YAHOO.util.LocalDataSource.TYPE_XML;
  activationsDataSource.responseSchema = {
    resultNode: 'Computer',
    fields : [
      { key: "LicenceProductName" },
      { key: "LicenceKey" }, 
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
    
  var activationsColumnDefs = [
    { key: "Name", label: "Computer", sortable: true, formatter: "text" },
    { key: "LicenceProductName", label: "Product", sortable: true, formatter: "text" },
    { key: "LicenceKey", label: "Licence Key", sortable: true, formatter: "text" },
    { key: "LastRequestDate", label: "Last Contact", formatter: 'date', dateOptions: { format: '%Y-%m-%d %H:%M' } },
    { key: "ActivationStatus", label: "Activation Status", formatter: 'activationStatus' }
  ];
  var activationsDataTable = YAHOO.config.activationsDataTable = new YAHOO.widget.ScrollingDataTable("activations", activationsColumnDefs, activationsDataSource);
  activationsDataTable.subscribe("rowClickEvent", activationsDataTable.onEventSelectRow);
  activationsDataTable.subscribe("postRenderEvent", function() { YAHOO.config.dlgActivationsReview.show(); });
  activationsDataTable.render();
}

function initDlgImportActivations()
{
	YAHOO.config.dlgImportActivations = new YAHOO.widget.Dialog("dlgImportActivations", { modal:true, visible:false, width:"500px", fixedcenter:true, constraintoviewport:true, draggable:true });
  initDlgStandard(YAHOO.config.dlgImportActivations);
}
