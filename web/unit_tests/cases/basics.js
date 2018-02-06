var assert = chai.assert;

describe('KeymanWeb Initialization', function() {

  beforeEach(function(done) {
    this.timeout(10000);

    fixture.setBase('unit_tests/fixtures');
    fixture.load("singleInput.html");
    setupKMW();

    // Pass the initTimer method our 'done' callback so it can handle our initialization delays for us.
    initTimer(done);
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
    describe('Toggle UI Initialization', function() {

      beforeEach(function(done) {
        this.timeout(10000);
        fixture.setBase('unit_tests/fixtures');
        fixture.load('singleInput.html');

        setupKMW('toggle');

        initTimer(done);
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

    describe('Button UI Initialization', function() {

      beforeEach(function(done) {
        this.timeout(10000);
        fixture.setBase('unit_tests/fixtures');
        fixture.load('singleInput.html');

        setupKMW('button');

        initTimer(done);
      });
      
      afterEach(function() {
        fixture.cleanup();
        teardownKMW();
      });

      it('The Button UI initializes correctly.', function() {
        assert(keyman.ui.init, 'Initialization flag is set to false!');
      })
    });

    describe('Float UI Initialization', function() {

      beforeEach(function(done) {
        this.timeout(10000);
        fixture.setBase('unit_tests/fixtures');
        fixture.load('singleInput.html');

        setupKMW('float');

        initTimer(done);
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

    describe('Toolbar UI Initialization', function() {

      beforeEach(function(done) {
        this.timeout(10000);
        fixture.setBase('unit_tests/fixtures');
        fixture.load('singleInput.html');

        setupKMW('toolbar');

        // The Toolbar UI has extra-special init to do... may as well feed it more time.
        initTimer(done, 2500);
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