let assert = chai.assert;

describe("'Canary' checks", function() {
  this.timeout(testconfig.timeouts.standard);

  before(function() {
    fixture.setBase('');
  })

  it('host-fixture.html + gestureHost.css', function() {
    fixture.setBase('');
    let element = fixture.load('host-fixture.html')[0];

    // Ensure that not only did we get an element, we got the expected element.
    assert.isNotNull(element);
    assert.isDefined(element);
    assert.equal(element.id, 'host-fixture');
    // If the CSS is missing, the element will default to zero height.
    assert.notEqual(element.getBoundingClientRect().height, 0);
  })

  after(function() {
    fixture.cleanup();
  })
});