var assert = chai.assert;

describe("'Canary' checks", function() {
  this.timeout(testconfig.timeouts.standard);

  before(function() {
    fixture.setBase('');
  })

  it('host-fixture.html + gestureHost.css', function() {
    let element = fixture.load('host-fixture.html')[0];

    // Ensure that not only did we get an element, we got the expected element.
    assert.isNotNull(element);
    assert.isDefined(element);
    assert.equal(element.id, 'host-fixture');
    // If the CSS is missing, the element will default to zero height.
    assert.notEqual(element.getBoundingClientRect().height, 0);
  });

  it('canaryRecording.json', function() {
    let jsonObject = window['__json__'].canaryRecording;

    assert.isNotNull(jsonObject);
    assert.isDefined(jsonObject);

    let config = new Testing.FixtureLayoutConfiguration(jsonObject.config);
    assert.equal(config.deviceStyle, 'screen4');
  });

  it('Testing.HostFixtureLayoutController', function(done) {
    let targetRoot = fixture.load('host-fixture.html')[0];
    let jsonObject = window['__json__'].canaryRecording;

    let controller = new Testing.HostFixtureLayoutController();
    // Note:  this is set BEFORE the controller is configured (in the following line).
    // The class is designed to support this.
    controller.layoutConfiguration = new Testing.FixtureLayoutConfiguration(jsonObject.config);
    controller.connect().then(() => {
      assert.isTrue(targetRoot.className.indexOf('screen4') > -1, "Could not apply configuration spec from recorded JSON!");
      done();
    }).finally(() => controller.destroy());
  })

  after(function() {
    fixture.cleanup();
  })
});