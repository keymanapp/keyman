function loadKeyboards() 
{ 
	var kmw=tavultesoft.keymanweb;

	// The first keyboard added will be the default keyboard for touch devices.
	// For faster loading, it may be best for the default keybaord to be 
	// locally sourced.
	kmw.addKeyboards({id:'us',name:'English',language:{id:'eng',name:'English'},
	  filename:'../us-1.0.js'});
	  
	// Add more keyboards to the language menu, by keyboard name,
	// keyboard name and language code, or just the ISO 639 language code.  
	kmw.addKeyboards('french','european2@swe','european2@nor','@heb');

	// Add a keyboard by language name.  Note that the name must be spelled
	// correctly, or the keyboard will not be found.  (Using ISO codes is
	// usually easier.)
	kmw.addKeyboardsForLanguage('Dzongkha');

	// Add a fully-specified, locally-sourced, keyboard with custom font  
	kmw.addKeyboards({id:'lao_2008_basic',name:'Lao Basic',
	  language:{
		id:'lao',name:'Lao',region:'Asia',
		font:{family:'LaoWeb',source:['../font/saysettha_web.ttf','../font/saysettha_web.woff','../font/saysettha_web.eot']}
		},
	  filename:'../lao_2008_basic.js'
	  });   
}
  
	// Page-global variable definitions.
  {
	var inputCounter = 0;
  }
	
	function generateDiagnosticDiv(elem) {
		var div = document.createElement("div");
		div.style.border = "1px solid black";
		div.style.borderRadius = "5px";
		//div.style.mozBorder
		div.style.paddingTop = "5px";
		div.style.paddingLeft = "5px";
		div.style.paddingRight = "5px";
		div.style.paddingBottom = "5px";
		div.appendChild(elem);
		div.appendChild(document.createElement("p"));
		
		var elemId = elem.id;
		
		// Attachment button
		var attachBtn = document.createElement("input");
		attachBtn.type 		= 'button';
		attachBtn.id   		= 'attach_' + elemId;
		attachBtn.onclick = function() {
			kmw.attachToControl(document.getElementById(elemId));
			var attached = kmw.isAttached(elem);
			document.getElementById('attachment_' + elemId).textContent = " Currently " + (attached ? "attached." : "detached.");
			document.getElementById("independence_" + elemId).textContent = attached ? " Using globally-selected keyboard." : '';
		}; 
		attachBtn.value   = 'Attach';
		div.appendChild(attachBtn);
		
		// Detachment button
		var detachBtn = document.createElement("input");
		detachBtn.type 		= 'button';
		detachBtn.id   		= 'detach_' + elemId;
		detachBtn.onclick = function() {
			kmw.detachFromControl(document.getElementById(elemId));
			var attached = kmw.isAttached(elem);
			document.getElementById('attachment_' + elemId).textContent = " Currently " + (attached ? "attached." : "detached.");
			if(!attached) {
				document.getElementById('independence_' + elemId).textContent = "";
			}
		}; 
		detachBtn.value   = 'Detach';
		div.appendChild(detachBtn);
		
		var attachLabel = document.createElement("a");
		attachLabel.id	= 'attachment_' + elemId;
		window.setTimeout(function() {
			attachLabel.textContent = " Currently " + (kmw.isAttached(elem) ? "attached." : "detached.");
		}, 1);
		div.appendChild(attachLabel);
		
		div.appendChild(document.createElement("p"));
		
		// Enablement button
		var enableBtn = document.createElement("input");
		enableBtn.type 		= 'button';
		enableBtn.id   		= 'enable_' + elemId;
		enableBtn.onclick = function() {
			kmw.enableControl(document.getElementById(elemId));
		}; 
		enableBtn.value   = 'Enable';
		div.appendChild(enableBtn);
		
		// Disablement button
		var disableBtn = document.createElement("input");
		disableBtn.type 		= 'button';
		disableBtn.id   		= 'disable_' + elemId;
		disableBtn.onclick = function() {
			kmw.disableControl(document.getElementById(elemId));
		}; 
		disableBtn.value   = 'Disable';
		div.appendChild(disableBtn);
		
		var enabledLabel = document.createElement("a");
		enabledLabel.id	= 'enablement_' + elemId;
		enabledLabel.textContent = " Currently enabled.";
		
		// We must rely on MutationObservers to stay current about enablement.
		var observationTarget = elem;
		var observationConfig = {attributes: true, attributeFilter: ['class']};
		
		var enablementObserver = new MutationObserver(function(mutations) {
			var isDisabled = mutations[0].target.className.indexOf('kmw-disabled') >= 0;
			enabledLabel.textContent = (isDisabled ? " Currently disabled." : " Currently enabled.");
		});
		enablementObserver.observe(observationTarget, observationConfig);
		
		div.appendChild(enabledLabel);
		div.appendChild(document.createElement("p"));
		
		if(elem.tagName.toLowerCase() != "iframe") { 
			// Set Keyboard button
			var setBtn = document.createElement("input");
			setBtn.type 		= 'button';
			setBtn.id   		= 'set_' + elemId;
			setBtn.onclick = function() {
				kmw.setKeyboardForControl(document.getElementById(elemId), 'dzongkha', 'dzo');
				
				if(kmw.isAttached(elem)) {
					kbdLabel.textContent = " Using independently-tracked keyboard.";
				}
			}; 
			setBtn.value   = 'Set to Dzongkha';
			div.appendChild(setBtn);
			
			// Clear Keyboard button
			var clearBtn = document.createElement("input");
			clearBtn.type 		= 'button';
			clearBtn.id   		= 'clear_' + elemId;
			clearBtn.onclick = function() {
				kmw.setKeyboardForControl(document.getElementById(elemId), null, null);
				
				if(kmw.isAttached(elem)) {
					kbdLabel.textContent = " Using globally-selected keyboard.";
				}
			}; 
			clearBtn.value   = 'Clear Keyboard';
			div.appendChild(clearBtn);
			
			var kbdLabel = document.createElement("a");
			kbdLabel.id	= 'independence_' + elemId;
			window.setTimeout(function () {
				if(kmw.isAttached(elem)) {
					var attached = kmw.isAttached(elem);
					kbdLabel.textContent = attached ? " Using globally-selected keyboard." : '';
				}
			}, 1);
			div.appendChild(kbdLabel);
		}
		
		return div;
	}
  
	function addInput() {
	  var masterDiv = document.getElementById('DynamicInputs');
	  var newInput = document.createElement("input");
		var i = inputCounter++;
		
	  newInput.id = 'input' + i;
	  newInput.className = 'test';
	  newInput.placeholder = "Dynamic area #" + i + "!";
	  
	  var newDiv = generateDiagnosticDiv(newInput);
	  
	  masterDiv.appendChild(newDiv);
  }
	
  function addText() {
	  var masterDiv = document.getElementById('DynamicTextareas');
	  var newTextArea = document.createElement("textarea");
	  var i = inputCounter++;
	  
	  newTextArea.id = 'textarea' + i;
	  newTextArea.className = 'test';
	  newTextArea.placeholder = "Dynamic area #" + i + "!";
	  
	  var newDiv = generateDiagnosticDiv(newTextArea);
	  
	  masterDiv.appendChild(newDiv);
  }
  
  function addIFrame() {
	  var masterDiv = document.getElementById('DynamicIFrames');
	  var frame = document.createElement("iframe");
		var i = inputCounter++;
		
	  frame.height = "100";
	  frame.src = "iframe.html";
		frame.id = 'iframe' + i;
	  
	  var newDiv = generateDiagnosticDiv(frame);
	  masterDiv.appendChild(newDiv);
  }

