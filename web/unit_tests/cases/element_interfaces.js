var assert = chai.assert;

var InterfaceTests;

// Define common interface testing functions that can be run upon the EditableElement interface.
if(typeof InterfaceTests == 'undefined') {
  InterfaceTests = {};

  (function(){
    // Makes a nice Unicode shortcut.
    var u = toSupplementaryPairString;

    InterfaceTests.Strings = {};

    InterfaceTests.Strings.Apple = {};
    InterfaceTests.Strings.Apple.normal = 'apple';
    // Built in-line via function.  Looks functionally equivalent to "apple", but with SMP characters.
    InterfaceTests.Strings.Apple.smp = u(0x1d5ba)+u(0x1d5c9)+u(0x1d5c9)+u(0x1d5c5)+u(0x1d5be);
    InterfaceTests.Strings.Apple.mixed = 'a'+u(0x1d5c9)+'p'+'l'+u(0x1d5be); 

    //#region Defines helpers related to HTMLInputElement / Input test setup.
    InterfaceTests.Input = {};

    InterfaceTests.Input.setupElement = function() {
      var id = DynamicElements.addInput();
      var elem = document.getElementById(id);
      var wrapper = new com.keyman.dom.Input(elem);

      return {elem: elem, wrapper: wrapper};
    }

    InterfaceTests.Input.resetWithText = function(pair, string) {
      pair.elem.value = string;
      pair.elem.setSelectionRange(0, 0);
    }

    // Implemented for completeness and generality with other tests.
    InterfaceTests.Input.setCaret = function(pair, index) {
      pair.elem.setSelectionRange(index, index);
    }

    // Implemented for completeness and generality with other tests.
    InterfaceTests.Input.getCaret = function(pair) {
      return pair.elem.selectionStart;
    }
    //#endregion

  })();
}

describe('Element Input/Output Interfacing', function() {
  this.timeout(kmwconfig.timeouts.standard);

  before(function() {
    fixture.setBase('unit_tests/fixtures');
    
    // Make sure the basic SMP extension hooks exist to prevent errors later.
    String.kmwEnableSupplementaryPlane(false);
  });

  beforeEach(function() {
    fixture.load("robustAttachment.html");
  })

  afterEach(function() {
    fixture.cleanup();
  });

  describe('Wrapper: HTMLInputElement', function() {
    /**
     * The design of these tests is to ensure that all caret handling works correctly, 
     * independently of other methods.  Other tests will then rely upon these methods
     * to simplify their code.
     */
    describe('Caret Handling', function() {
      describe('setCaret', function() {
        it('correctly places the caret (no prior selection)', function() {
          var TestObj = InterfaceTests.Input;
          var Apple = InterfaceTests.Strings.Apple;
          var input = TestObj.setupElement();

          String.kmwEnableSupplementaryPlane(false);
          TestObj.resetWithText(input, Apple.normal);
          input.wrapper.setCaret(4);
          assert.equal(TestObj.getCaret(input), 4, "Failed to correctly set the caret within a simple string!");
          String.kmwEnableSupplementaryPlane(false);

          TestObj.resetWithText(input, Apple.smp);
          String.kmwEnableSupplementaryPlane(true);
          input.wrapper.setCaret(4);
          assert.equal(TestObj.getCaret(input), 8, "Failed to correctly set the caret within an SMP string!");
          String.kmwEnableSupplementaryPlane(false);

          TestObj.resetWithText(input, Apple.mixed);
          String.kmwEnableSupplementaryPlane(true);
          input.wrapper.setCaret(4);
          assert.equal(TestObj.getCaret(input), 5, "Failed to correctly set the caret within a mixed SMP string!");
          String.kmwEnableSupplementaryPlane(false);
        })
      });

      describe('getCaret', function() {
        it('correctly reports the position of the caret (no selection)', function() {
          var TestObj = InterfaceTests.Input;
          var Apple = InterfaceTests.Strings.Apple;
          var input = TestObj.setupElement();

          String.kmwEnableSupplementaryPlane(false);
          TestObj.resetWithText(input, Apple.normal);
          TestObj.setCaret(input, 4);
          assert.equal(input.wrapper.getCaret(), 4, "Failed to correctly read the caret's position within a simple string!");
          String.kmwEnableSupplementaryPlane(false);

          TestObj.resetWithText(input, Apple.smp);
          String.kmwEnableSupplementaryPlane(true);
          TestObj.setCaret(input, 8);
          assert.equal(input.wrapper.getCaret(), 4, "Failed to correctly read the caret's position within an SMP string!");
          String.kmwEnableSupplementaryPlane(false);

          TestObj.resetWithText(input, Apple.mixed);
          String.kmwEnableSupplementaryPlane(true);
          TestObj.setCaret(input, 5);
          assert.equal(input.wrapper.getCaret(), 4, "Failed to correctly read the caret's position within a mixed SMP string!");
          String.kmwEnableSupplementaryPlane(false);
        });
      });
    });
    // it("", function() {
    //   // Base element setup.
    //   var id = DynamicElements.addInput();
    //   var elem = document.getElementById(id);
    //   var wrapper = new com.keyman.dom.Input(elem);

    //   // Connect with JSON fixtures for some nice, common test data.
    // });
  });
});