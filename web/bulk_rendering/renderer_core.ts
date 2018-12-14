// Includes KeymanWeb's Device class, as it's quite a useful resource for KMW-related projects.
/// <reference path="../source/kmwdevice.ts" />
// Needed for OSK rendering to image files.
/// <reference path="../node_modules/html2canvas/dist/html2canvas.js" />
// Ensure that Promises are within scope.
/// <reference path="../node_modules/promise-polyfill/lib/polyfill.js" />

type KeyboardMap = {[id: string]: any};

namespace com.keyman.renderer {
  export class BatchRenderer {
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

    private render(resolve, ele: HTMLElement, layerIndex: number, isMobile?: boolean) {
      let html2canvas = window['html2canvas'];

      let imgOut = document.createElement('img');      
      document.body.appendChild(imgOut);

      // Warning - needs Promises, so it'll need a polyfill for IE.
      let canvasParams = {
        'scale': 1,
        'width': window.innerWidth // Good for mobile, less-so for desktop.
      }

      if(!isMobile) {
        canvasParams['width'] = 500;
        ele.style.width = '500px';
      }

      html2canvas(ele, canvasParams).then(function(canvas) {
        imgOut.src = canvas.toDataURL();
        resolve(layerIndex);
      });
    }

    private processKeyboard(kbd) {
      let keyman = window['keyman'];
      keyman.setActiveKeyboard(kbd['InternalName']);
      let isMobile = keyman.util.device.formFactor != 'desktop';

      // Really could use promises to tie these two together...
      setTimeout(function() {
        let box: HTMLDivElement = keyman.osk._Box;
        keyman.osk.show(true);
  
        // Forcing display, width, and height here helps to ensure a nice, consistent image.
        box.style.display = 'block';
  
        let layers = keyman.osk._DivVKbd.firstChild.childNodes;

        let renderLayer = function(i) {
          return new Promise(function(resolve) {
            for(var j = 0; j < layers.length; j++) {
              layers[j].style.display = 'none';
            }

            layers[i].style.display = 'block';

            this.render(resolve, box, i, isMobile);
          }.bind(this))
        }.bind(this);

        return this.arrayPromiseIteration(renderLayer, layers.length).then(function() {
          console.log("All renders for the first keyboard should now be complete.");
        });
      }.bind(this), 2500);
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
        // return promise.then(function(index: number) {
        //   if(++index < length) {
        //       return iteration(index);
        //   } // else instantly return.
        // });
      }

      return iteration(0);
    }

    run() {
      if(window['keyman']) {
        let keyman = window['keyman'];

        // Assumes that the keyboards have been preloaded for us.
        let kbds = this.filterKeyboards();

        console.log("Unique keyboard ids detected: " + Object.keys(kbds).length);

        // Temporary - just load the first keyboard.
        this.processKeyboard(kbds[Object.keys(kbds)[0]]);
      } else {
        console.error("KeymanWeb not detected!");
      }
    }
  }

  (function(){
    window['kmw_renderer'] = new com.keyman.renderer.BatchRenderer();
  })();
}