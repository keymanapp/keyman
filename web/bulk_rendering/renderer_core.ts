// Includes KeymanWeb's Device class, as it's quite a useful resource for KMW-related projects.
/// <reference path="../source/kmwdevice.ts" />
// Needed for OSK rendering to image files.
/// <reference path="../node_modules/html2canvas/dist/html2canvas.js" />
// Ensure that Promises are within scope.
/// <reference path="../node_modules/es6-shim/es6-shim.min.js" />

type KeyboardMap = {[id: string]: any};

namespace com.keyman.renderer {
  export class BatchRenderer {
    static divMaster: HTMLDivElement;
    static dummy: HTMLInputElement;

    // Filters the keyboard array to ensure only a single entry remains, rather than an entry per language.
    private filterKeyboards(): KeyboardMap {
      let keyman = window['keyman'];

      let kbds = keyman['getKeyboards']();

      let keyboardMap = [];

      for(var i = 0; i < kbds.length; i++) {
        let id: string = kbds[i]['InternalName'];
        if(keyboardMap[id]) {
          continue;
        } else {
          keyboardMap[id] = kbds[i];
        }
      }

      return keyboardMap;
    }

    private render(ele: HTMLElement, isMobile?: boolean): Promise<HTMLImageElement> {
      let html2canvas = window['html2canvas'];

      let imgOut = document.createElement('img');

      let canvasParams = {
        'logging': false,
        'scale': 1,
        'width': window.innerWidth // Good for mobile, less-so for desktop.
      }

      // So, if it's desktop, we set more reasonable values.
      if(!isMobile) {
        canvasParams['width'] = 500;
        ele.style.width = '500px';
      }

      return html2canvas(ele, canvasParams).then(function(canvas) {
        imgOut.src = canvas.toDataURL();
        return imgOut;
      });
    }

    createKeyboardHeader(kbd, loaded: boolean): HTMLDivElement {
      let divHeader = document.createElement('div');
      let eleName = document.createElement('h2');

      eleName.textContent = 'ID:  ' + kbd['InternalName'];
      divHeader.appendChild(eleName);

      let eleDescription = document.createElement('p');

      if(loaded) {

        eleDescription.appendChild(document.createTextNode('Name: ' + kbd['Name']));
        eleDescription.appendChild(document.createElement('br'));
        eleDescription.appendChild(document.createTextNode('Font:  ' + window['keyman'].core.activeKeyboard._legacyLayoutSpec.F));

      } else {
        eleDescription.appendChild(document.createTextNode('Unable to load this keyboard!'));
      }
      
      divHeader.appendChild(eleDescription);

      return divHeader;
    }

    private processKeyboard(kbd) {
      let keyman = window['keyman'];
      let p: Promise<void> = keyman.setActiveKeyboard(kbd['InternalName']);
      let isMobile = keyman.util.device.formFactor != 'desktop';

      // Establish common keyboard header info.
      let divSummary = document.createElement('div');
      // Establishes a linkable target for this keyboard's data.
      divSummary.id = "summary-" + kbd['InternalName'];
      
      BatchRenderer.divMaster.insertAdjacentElement('afterbegin', divSummary);

      // A nice, closure-friendly reference for use in our callbacks.
      let renderer = this;

      // Once the keyboard's loaded, we can really get started.
      return p.then(function() {
        let box: HTMLDivElement = keyman.osk._Box;
        
        divSummary.appendChild(renderer.createKeyboardHeader(kbd, true));

        let divRenders = document.createElement('div');
        divSummary.appendChild(divRenders);
  
        // Uses 'private' APIs that may be subject to change in the future.  Keep it updated!
        var layers;
        if(isMobile) {
          layers = keyman.osk.vkbd.layers;
        } else {
          // The desktop OSK will be overpopulated, with a number of blank layers to display in most cases.
          // We instead rely upon the KLS definition to ensure we keep the renders sparse.
          layers = keyman.core.activeKeyboard._legacyLayoutSpec.KLS;
        }

        let renderLayer = function(i: number) {
          return new Promise(function(resolve) {
            // (Private API) Directly sets the keyboard layer within KMW, then uses .show to force-display it.
            if(keyman.osk.vkbd) {
              if(isMobile) {
                keyman.core.keyboardProcessor.layerId = layers[i].id;
              } else {
                keyman.core.keyboardProcessor.layerId = Object.keys(layers)[i];
              }
            } else {
              console.error("Error - keyman.osk.vkbd is undefined!");
            }
            // Make sure the active element's still set!
            renderer.setActiveDummy();
            keyman.osk.show(true);

            renderer.render(box, isMobile).then(function(imgEle: HTMLImageElement) {
              let eleLayer = document.createElement('div');
              let eleLayerId = document.createElement('p');
              eleLayerId.textContent = 'Layer ID:  ' + (isMobile ? layers[i].id : Object.keys(layers)[i]);

              eleLayer.appendChild(eleLayerId);
              eleLayer.appendChild(imgEle);
              eleLayer.appendChild(document.createElement('br'));

              divRenders.appendChild(eleLayer);          
              resolve(i);
            });
          })
        };

        // The resulting Promise will only call it's `.then()` once all of this keyboard's renders have been completed.
        return renderer.arrayPromiseIteration(renderLayer, isMobile ? layers.length : Object.keys(layers).length);
      }).catch(function() {
        console.log("Failed to load the \"" + kbd['InternalName'] + "\" keyboard for rendering!");
        divSummary.appendChild(renderer.createKeyboardHeader(kbd, false));
        return Promise.resolve();
      });
    }

    // Synchronously performs asynchronous operations across a loop, one at a time.
    // Necessary due to the nature of KMW OSK rendering.
    private arrayPromiseIteration(promiseGenerator: (i: number) => Promise<any>, length: number): Promise<any> {
      let iteration = function(index: number): Promise<any> {
        if(index < length) {
          var promise = promiseGenerator(index);
          return promise.then(function(index: number) {
            return iteration(++index);
          })
        } else {
          return Promise.resolve();
        }
      }

      return iteration(0);
    }

    fillDeviceNotes() {
      let description = document.createElement('p');
      let device = new com.keyman.Device();
      device.detect();

      description.appendChild(document.createTextNode('Browser: ' + device.browser));
      description.appendChild(document.createElement('br'));
      description.appendChild(document.createTextNode('OS:  ' + device.OS));
      description.appendChild(document.createElement('br'));
      description.appendChild(document.createTextNode('Form factor: ' + device.formFactor));
      description.appendChild(document.createElement('br'));
      description.appendChild(document.createTextNode('Touchable:  ' + device.touchable));

      document.getElementById('deviceNotes').appendChild(description);
    }

    setActiveDummy() {
      com.keyman['dom']['DOMEventHandlers'].states.activeElement = BatchRenderer.dummy;
    }

    run() {
      if(window['keyman']) {
        let keyman = window['keyman'];

        // Establish a 'dummy' element to bypass the 'nothing's active' check KMW usualy uses.
        BatchRenderer.dummy = document.createElement('input');
        this.setActiveDummy();

        BatchRenderer.divMaster = <HTMLDivElement> document.getElementById('renderList');
        if(BatchRenderer.divMaster.childElementCount > 0) {
          console.log("Prior bulk-renderer run detected.  Terminating execution.");
          return;
        }

        // We want the renderer to control where the keyboard is displayed.
        // Also bypasses another 'fun' OSK complication.
        if(keyman.util.device.formFactor == 'desktop') {
          keyman.osk.userPositioned = true;
        }

        // Assumes that the keyboards have been preloaded for us.
        let kbds = this.filterKeyboards();

        console.log("Unique keyboard ids detected: " + Object.keys(kbds).length);

        let renderer = this;

        let keyboardIterator = function(i) {
          return new Promise(function(resolve) {
            renderer.processKeyboard(kbds[Object.keys(kbds)[i]]).then(function () {
              //console.log("Keyboard " + i + " processed!");
              resolve(i);
            });
          });
        };

        this.arrayPromiseIteration(keyboardIterator, Object.keys(kbds).length).then(function() {
          // Once all renders are done, we can now tidy the page up and prep it for final display + potential file-saving.
          
          // This will go at the top of the page when finished, but not when actively rendering.
          // We want to leave as much space visible as possible when actively rendering keyboards
          // so that auto-scrolling isn't an issue.
          renderer.fillDeviceNotes();
        });
      } else {
        console.error("KeymanWeb not detected!");
      }
    }
  }

  (function(){
    window['kmw_renderer'] = new com.keyman.renderer.BatchRenderer();
  })();
}