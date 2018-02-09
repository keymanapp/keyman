var assert = chai.assert;

var DynamicElements;
var inputCounter = 0;

if(typeof(DynamicElements) == 'undefined') {
  DynamicElements = {};

  DynamicElements.addInput = function() {
	  var masterDiv = document.getElementById('DynamicElements');
	  var newInput = document.createElement("input");
		var i = inputCounter++;
		
	  newInput.id = 'input' + i;
	  newInput.className = 'test';
		newInput.placeholder = "Dynamic area #" + i + "!";
		
		masterDiv.appendChild(newInput);
		return newInput.id;
  }
	
  DynamicElements.addText = function () {
	  var masterDiv = document.getElementById('DynamicElements');
	  var newTextArea = document.createElement("textarea");
	  var i = inputCounter++;
	  
	  newTextArea.id = 'textarea' + i;
	  newTextArea.className = 'test';
		newTextArea.placeholder = "Dynamic area #" + i + "!";
		
		masterDiv.appendChild(newTextArea);
		return newTextArea.id;
  }
  
  DynamicElements.addIFrame = function(loadCallback) {
	  var masterDiv = document.getElementById('DynamicElements');
	  var frame = document.createElement("iframe");
		var i = inputCounter++;
		
	  frame.height = "100";
		frame.id = 'iframe' + i;
		if(loadCallback) {
			frame.addEventListener('load', function() {
				// Give KMW's attachment events a chance to run first.
				window.setTimeout(loadCallback, 100);
			});
		}
		frame.setAttribute("src", "resources/html/iframe.html");
			
		masterDiv.appendChild(frame);
		return frame.id;
  }
	
	DynamicElements.addEditable = function() {
		var masterDiv = document.getElementById('DynamicElements');
		var editable = document.createElement("div");
		var i = inputCounter++;
		
		editable.contentEditable = true;
		editable.textContent = "Edit me!";
		editable.id = 'editable' + i;
		editable.style.width="500px";
		
		masterDiv.appendChild(editable);
		return editable.id;
	}

	DynamicElements.assertAttached = function(ele, done) {
		window.setTimeout(function() {
			assert.isTrue(keyman.isAttached(ele), "Element tag '" + ele.tagName + "', id '" + ele.id + "' was not attached!");

			if(done) {
				done();
			}
		}, 50);
	}
}

Modernizr.on('touchevents', function(result) {
	if(result) {
		describe('Attachment (Touch, \'auto\')', function() {

			this.timeout(5000);

			before(function(done) {
				this.timeout(10000);

				fixture.setBase('unit_tests/fixtures');
				setupKMW({ attachType:'auto' });

				// Pass the initTimer method our 'done' callback so it can handle our initialization delays for us.
				initTimer(done);
			});
			
			beforeEach(function() {
				fixture.load("robustAttachment.html");
			});
			
			after(function() {
				teardownKMW();
			});
			
			afterEach(function(done) {
				fixture.cleanup();
				window.setTimeout(function(){
					done();
				}, 500);
			})
			
			describe('Element Type', function() {
				it('<input>', function(done) {
					var ID = DynamicElements.addInput();
					var ele = document.getElementById(ID);

					DynamicElements.assertAttached(ele, done);
				});

				it('<textarea>', function(done) {
					var ID = DynamicElements.addText();
					var ele = document.getElementById(ID);

					DynamicElements.assertAttached(ele, done);
				});

				it('<iframe>', function(done) {
					var ID = DynamicElements.addIFrame(function() {
						var ele = document.getElementById(ID);
						var innerEle = ele.contentDocument.getElementById('iframe_input');

						assert.isFalse(keyman.isAttached(ele));
						assert.isNotNull(innerEle);
						assert.isFalse(keyman.isAttached(innerEle));

						window.setTimeout(function() {
							done();
						}, 50);
					});
				});

				it('contentEditable=true', function(done) {
					var ID = DynamicElements.addEditable();
					var ele = document.getElementById(ID);

					assert.isFalse(keyman.isAttached(ele));
					done();
				});
			});
		});
	} else {
		describe('Attachment (Desktop, \'auto\')', function() {

			this.timeout(5000);

			before(function(done) {
				this.timeout(10000);

				fixture.setBase('unit_tests/fixtures');
				setupKMW({ attachType:'auto' });

				// Pass the initTimer method our 'done' callback so it can handle our initialization delays for us.
				initTimer(done);
			});
			
			beforeEach(function() {
				fixture.load("robustAttachment.html");
			});
			
			after(function() {
				teardownKMW();
			});
			
			afterEach(function(done) {
				fixture.cleanup();
				window.setTimeout(function(){
					done();
				}, 500);
			})
			
			describe('Element Type', function() {
				it('<input>', function(done) {
					var ID = DynamicElements.addInput();
					var ele = document.getElementById(ID);

					DynamicElements.assertAttached(ele, done);
				});

				it('<textarea>', function(done) {
					var ID = DynamicElements.addText();
					var ele = document.getElementById(ID);

					DynamicElements.assertAttached(ele, done);
				});

				// We still have weird collateral issues with IE and Edge if this is enabled.
				it('<iframe>', function(done) {
					var ID = DynamicElements.addIFrame(function() {
						var ele = document.getElementById(ID);
						var innerEle = ele.contentDocument.getElementById('iframe_input');

						assert.isTrue(keyman.isAttached(ele));
						assert.isNotNull(innerEle);
						assert.isTrue(keyman.isAttached(innerEle));
						keyman.detachFromControl(ele);

						window.setTimeout(function() {
							done();
						}, 50);
					});
				});

				it('contentEditable=true', function(done) {
					var ID = DynamicElements.addEditable();
					var ele = document.getElementById(ID);

					DynamicElements.assertAttached(ele, done);
				});
			});
		});

	}
});