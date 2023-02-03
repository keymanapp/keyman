// Includes KeymanWeb's Device class, as it's quite a useful resource for KMW-related projects.

type KeyboardMap = {[id: string]: any};

namespace com.keyman.renderer {
  export class BatchRenderer {
    static divMaster: HTMLDivElement;
    static dummy: HTMLInputElement;
    static captureStream: any;
    static video: any;
    static boundingRect: DOMRect;
    static allLayers = false;
    static keyboardMatch: RegExp;

    async startCapture() {
      let captureStream = null;

      try {
        captureStream = await (navigator.mediaDevices as any).getDisplayMedia({
          audio: false,
          video: {
            width: screen.width,
            height: screen.height,
            frameRate: 60,
          }
        });
      } catch(err) {
        console.error("Error: " + err);
      }

      const video = document.createElement("video");
      video.srcObject = captureStream;
      video.play();

      const result = await new Promise<void>((resolve, reject) => {
        video.onloadedmetadata = function () {
          resolve();
        }
      });

      return {captureStream: captureStream, video: video};
    }

    // Filters the keyboard array to ensure only a single entry remains, rather than an entry per language.
    private filterKeyboards(): KeyboardMap {
      let keyman = window['keyman'];

      let kbds = keyman['getKeyboards']();

      let keyboardMap = [];

      for(var i = 0; i < kbds.length; i++) {
        let id: string = kbds[i]['InternalName'];
        if(!id.match(BatchRenderer.keyboardMatch)) continue;
        if(id.match(/^Keyboard_bod/)) continue; // bod keyboards currently don't load on Chrome and break the test run; they need rebuild.
        if(keyboardMap[id]) {
          continue;
        } else {
          keyboardMap[id] = kbds[i];
        }
      }

      return keyboardMap;
    }

    private render(ele: HTMLElement, isMobile?: boolean): Promise<HTMLImageElement> {
      const capture = async () => {
        const result = await new Promise<HTMLImageElement>((resolve, reject) => {
          // from: https://github.com/kasprownik/electron-screencapture/blob/master/index.js
          const canvas = document.createElement('canvas');
          canvas.width = BatchRenderer.boundingRect.width;
          canvas.height = BatchRenderer.boundingRect.height;
          const context = canvas.getContext('2d');
          // see: https://developer.mozilla.org/en-US/docs/Web/API/HTMLVideoElement
          context.drawImage(BatchRenderer.video,
              BatchRenderer.boundingRect.left,
              BatchRenderer.boundingRect.top,
              canvas.width, canvas.height,
              0, 0, canvas.width, canvas.height);

          const frame = canvas.toDataURL("image/png");
          let imgOut = document.createElement('img');
          imgOut.src = frame;

          resolve(imgOut);
        });

        return result;
      }

      return capture();
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

        BatchRenderer.boundingRect = box.getBoundingClientRect();

        // Appromixate handling for non-fullscreen mode runs.
        // Adjusts for the browser window's title bars, address bars, etc.
        const screenOffsetY = window.outerHeight - window.innerHeight;
        BatchRenderer.boundingRect.y += screenOffsetY;

        divSummary.appendChild(renderer.createKeyboardHeader(kbd, true));

        let divRenders = document.createElement('div');
        divSummary.appendChild(divRenders);

        // Uses 'private' APIs that may be subject to change in the future.  Keep it updated!
        var layers;
        if(isMobile) {
          layers = keyman.osk.vkbd.layerGroup.layers;
        } else {
          // The desktop OSK will be overpopulated, with a number of blank layers to display in most cases.
          // We instead rely upon the KLS definition to ensure we keep the renders sparse.
          layers = keyman.core.activeKeyboard._legacyLayoutSpec.KLS;
        }

        let renderLayer = function(i: number) {
          return new Promise(function(resolve) {
            // (Private API) Directly sets the keyboard layer within KMW, then uses .show to force-display it.
            if(keyman.osk.vkbd) {
              keyman.core.keyboardProcessor.layerId = Object.keys(layers)[i];
            } else {
              console.error("Error - keyman.osk.vkbd is undefined!");
            }
            // Make sure the active element's still set!
            renderer.setActiveDummy();
            keyman.osk.show(true);

            (document as any).fonts.ready.then(function() {
              window.setTimeout(function() {
                renderer.render(box, isMobile).then(function(imgEle: HTMLImageElement) {
                  let eleLayer = document.createElement('div');
                  let eleLayerId = document.createElement('p');
                  eleLayerId.textContent = 'Layer ID:  ' + Object.keys(layers)[i];

                  eleLayer.appendChild(eleLayerId);
                  eleLayer.appendChild(imgEle);
                  eleLayer.appendChild(document.createElement('br'));

                  divRenders.appendChild(eleLayer);
                  resolve(i);
                });
              }, 100);
            });
          });
        };

        // The resulting Promise will only call it's `.then()` once all of this keyboard's renders have been completed.
        return renderer.arrayPromiseIteration(renderLayer, Object.keys(layers).length);
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
      window['keyman'].domManager.activeElement = BatchRenderer.dummy;
    }

    async run(allLayers, filter) {
      BatchRenderer.allLayers = allLayers;
      BatchRenderer.keyboardMatch = new RegExp('^Keyboard_('+filter+')', 'i');
      if(window['keyman']) {
        let keyman = window['keyman'];

        let cc = await this.startCapture();
        BatchRenderer.captureStream = cc.captureStream;
        BatchRenderer.video = cc.video;

        // Establish a 'dummy' element to bypass the 'nothing's active' check KMW usually uses.
        BatchRenderer.dummy = document.createElement('input');
        window['keyman'].attachToControl(BatchRenderer.dummy);
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