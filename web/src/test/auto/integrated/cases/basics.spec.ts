import { assert } from 'chai';

import { DEVICE_DETECT_FAILURE, setupKMW, teardownKMW } from "../test_utils.js";
import { Device } from "keyman/engine/device-detect";
import { KeymanEngine } from "keyman/app/browser";

const baseTimeout = 5000;

const host = document.createElement('div');
document.body.appendChild(host);

describe('Basic KeymanWeb', function() {
  this.timeout(baseTimeout);

  before(function() {
    // These tests require use of KMW's device-detection functionality.
    assert.isFalse(DEVICE_DETECT_FAILURE, "Cannot run due to device detection failure.");
  })

  beforeEach(function() {
    this.timeout(baseTimeout);

    const singleton = document.createElement('input');
    singleton.id = 'singleton';
    host.appendChild(singleton);

    return setupKMW(null, baseTimeout);
  });

  afterEach(function() {
    host.innerHTML = '';
    teardownKMW();
  });

  describe('Initialization', function() {
    it('KMW should attach to the input element.', function() {
      const keyman = window['keyman'] as KeymanEngine;
      var singleton = document.getElementById('singleton');
      assert.isTrue(keyman.isAttached(singleton), "KeymanWeb did not automatically attach to the element!");
    });

    it('KMW\'s initialization variable should indicate completion.', function() {
      const keyman = window['keyman'] as KeymanEngine;
      assert.equal(keyman.initialized, 2, 'Keyman indicates incomplete initialization!');
    });
  });
});

const device = new Device();
device.detect();
if(!device.touchable) {
  describe('Basic Toggle UI', function() {
    this.timeout(baseTimeout);

    beforeEach(async function() {
      this.timeout(baseTimeout);

      const singleton = document.createElement('input');
      singleton.id = 'singleton';
      host.appendChild(singleton);

      // Loads two scripts in parallel, but just in case, 2x timeout.
      return setupKMW('toggle', baseTimeout);
    });

    afterEach(function() {
      host.innerHTML = '';
      teardownKMW();
    });

    it('The Toggle UI initializes correctly.', function() {
      const keyman = window['keyman'] as KeymanEngine;

      // UI-module specific typings are currently not available.
      const ui = keyman.ui as any;

      assert(ui.initialized, 'Initialization flag is set to false!');
      assert.isNotNull(ui.controller, 'Failed to create the controller element!');

      var divs = document.getElementsByTagName("div");
      var match = false;

      for(var i=0; i < divs.length; i++) {
        if(divs[i] == ui.controller) {
          match = true;
        }
      }

      assert.isTrue(match, 'Controller element has not been added to the page!');
    })
  });

  describe('Basic Button UI', function() {

    beforeEach(async function() {
      this.timeout(baseTimeout);

      const singleton = document.createElement('input');
      singleton.id = 'singleton';
      host.appendChild(singleton);

      // Loads two scripts in parallel, but just in case, 2x timeout.
      return setupKMW('button', baseTimeout);
    });

    afterEach(function() {
      host.innerHTML = '';
      teardownKMW();
    });

    it('The Button UI initializes correctly.', function() {
      const keyman = window['keyman'] as KeymanEngine;

      // UI-module specific typings are currently not available.
      const ui = keyman.ui as any;

      assert.isTrue(ui.init, 'Initialization flag is set to false!');
    })
  });

  describe('Basic Float UI', function() {

    beforeEach(async function() {
      this.timeout(baseTimeout);

      const singleton = document.createElement('input');
      singleton.id = 'singleton';
      host.appendChild(singleton);

      // Loads two scripts in parallel, but just in case, 2x timeout.
      return setupKMW('float', baseTimeout);
    });

    afterEach(function() {
      host.innerHTML = '';
      teardownKMW();
    });

    it('The Float UI initializes correctly.', function() {
      const keyman = window['keyman'] as KeymanEngine;

      // UI-module specific typings are currently not available.
      const ui = keyman.ui as any;

      assert.isTrue(ui.initialized, 'Initialization flag is set to false!');

      assert.isNotNull(ui.outerDiv, 'Failed to create the floating controller element!');

      var divs = document.getElementsByTagName("div");
      var match = false;

      for(var i=0; i < divs.length; i++) {
        if(divs[i] == ui.outerDiv) {
          match = true;
        }
      }

      assert.isTrue(match, 'Floating controller element has not been added to the page!');
    })
  });

  describe('Basic Toolbar UI', function() {

    beforeEach(async function() {
      this.timeout(baseTimeout);

      const singleton = document.createElement('input');
      singleton.id = 'singleton';
      host.appendChild(singleton);

      // Loads two scripts in parallel, but just in case, 2x timeout.
      return setupKMW('toolbar', baseTimeout);
    });

    afterEach(function() {
      host.innerHTML = '';
      teardownKMW();
    });

    it('The Toolbar UI initializes correctly.', function() {
      const keyman = window['keyman'] as KeymanEngine;

      // UI-module specific typings are currently not available.
      const ui = keyman.ui as any;

      assert.isTrue(ui.init, 'Initialization flag is set to false!');

      var kwc = document.getElementById('KeymanWebControl');
      assert.isNotNull(kwc, 'Toolbar DIV was not added to the page!');

      var toolbar = document.getElementById('kmw_controls');
      assert.isNotNull(toolbar, 'The main toolbar element was not added to the page!');
    })
  });
}