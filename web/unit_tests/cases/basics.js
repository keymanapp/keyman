var assert = chai.assert;

describe('Basic KeymanWeb', function() {
  this.timeout(kmwconfig.timeouts.standard);

  before(function() {
    // These tests require use of KMW's device-detection functionality.
    assert.isFalse(com.keyman.karma.DEVICE_DETECT_FAILURE, "Cannot run due to device detection failure.");
  })

  beforeEach(function(done) {
    this.timeout(kmwconfig.timeouts.scriptLoad);

    fixture.setBase('fixtures');
    fixture.load("singleInput.html");
    setupKMW(null, done, kmwconfig.timeouts.scriptLoad);
  });
  
  afterEach(function() {
    fixture.cleanup();
    teardownKMW();
  });
  
  describe('Initialization', function() {
    it('KMW should attach to the input element.', function() {
      var singleton = document.getElementById('singleton');
      assert.isTrue(keyman.isAttached(singleton), "KeymanWeb did not automatically attach to the element!");
    });
    it('KMW\'s initialization variable should indicate completion.', function() {
      assert(keyman.initialized == 2, 'Keyman indicates incomplete initialization!');
    });
  });
});

Modernizr.on('touchevents', function(result) {
  if(!result) {
    describe('Basic Toggle UI', function() {
      this.timeout(kmwconfig.timeouts.scriptLoad);

      beforeEach(function(done) {
        this.timeout(kmwconfig.timeouts.uiLoad);
        fixture.setBase('fixtures');
        fixture.load('singleInput.html');

        // Sequentially loads two scripts, so 2x timeout.
        setupKMW('toggle', done, kmwconfig.timeouts.uiLoad, function() { return keyman.ui.initialized; });
      });
      
      afterEach(function() {
        fixture.cleanup();
        teardownKMW();
      });

      it('The Toggle UI initializes correctly.', function() {
        assert(keyman.ui.initialized, 'Initialization flag is set to false!');
        
        assert.isNotNull(keyman.ui.controller, 'Failed to create the controller element!');

        var divs = document.getElementsByTagName("div");
        var match = false;

        for(var i=0; i < divs.length; i++) {
          if(divs[i] == keyman.ui.controller) {
            match = true;
          }
        }

        assert(match, 'Controller element has not been added to the page!');
      })
    });

    describe('Basic Button UI', function() {

      beforeEach(function(done) {
        this.timeout(kmwconfig.timeouts.uiLoad);
        fixture.setBase('fixtures');
        fixture.load('singleInput.html');

        // Sequentially loads two scripts, so 2x timeout.
        setupKMW('button', done, kmwconfig.timeouts.uiLoad, function() { return keyman.ui.init; });
      });
      
      afterEach(function() {
        fixture.cleanup();
        teardownKMW();
      });

      it('The Button UI initializes correctly.', function() {
        assert(keyman.ui.init, 'Initialization flag is set to false!');
      })
    });

    describe('Basic Float UI', function() {

      beforeEach(function(done) {
        this.timeout(kmwconfig.timeouts.uiLoad);
        fixture.setBase('fixtures');
        fixture.load('singleInput.html');

        // Sequentially loads two scripts, so 2x timeout.
        setupKMW('float', done, kmwconfig.timeouts.uiLoad, function() { return keyman.ui.initialized; });
      });
      
      afterEach(function() {
        fixture.cleanup();
        teardownKMW();
      });

      it('The Float UI initializes correctly.', function() {
        assert(keyman.ui.initialized, 'Initialization flag is set to false!');
        
        assert.isNotNull(keyman.ui.outerDiv, 'Failed to create the floating controller element!');

        var divs = document.getElementsByTagName("div");
        var match = false;

        for(var i=0; i < divs.length; i++) {
          if(divs[i] == keyman.ui.outerDiv) {
            match = true;
          }
        }

        assert(match, 'Floating controller element has not been added to the page!');
      })
    });

    describe('Basic Toolbar UI', function() {

      beforeEach(function(done) {
        this.timeout(kmwconfig.timeouts.uiLoad);
        fixture.setBase('fixtures');
        fixture.load('singleInput.html');

        setupKMW('toolbar', done, kmwconfig.timeouts.uiLoad, function() { return keyman.ui.init; });
      });
      
      afterEach(function() {
        fixture.cleanup();
        teardownKMW();
      });

      it('The Toolbar UI initializes correctly.', function() {
        assert(keyman.ui.init, 'Initialization flag is set to false!');
        
        var kwc = document.getElementById('KeymanWebControl');
        assert.isNotNull(kwc, 'Toolbar DIV was not added to the page!');

        var toolbar = document.getElementById('kmw_controls');
        assert.isNotNull(toolbar, 'The main toolbar element was not added to the page!');
      })
    });
  }
});