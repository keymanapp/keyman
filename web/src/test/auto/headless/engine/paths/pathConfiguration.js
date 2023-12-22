import { assert } from 'chai';
import sinon from 'sinon';

import { PathOptionDefaults, PathConfiguration } from 'keyman/engine/paths';

// Tests the activation-state logic abstraction & implementations used to model and control OSK visibility.

describe("Path Configuration", () => {
  it('https://test.site.com/folder/, default options', () => {
    const paths = new PathConfiguration(PathOptionDefaults, 'https://test.site.com/folder/');

    assert.equal(paths.protocol, 'https:');
    assert.equal(paths.root, 'https://test.site.com/');
    assert.equal(paths.resources, 'https://test.site.com/folder/');
    assert.equal(paths.keyboards, '');
    assert.equal(paths.fonts, '');
  });

  it('https://test.site.com/folder, non-standard option values', () => {
    const paths = new PathConfiguration({...PathOptionDefaults,
      keyboards: 'https://s.keyman.com/keyboard',
      fonts: '/fonts',
      resources: 'resources',
      root: '//something.or.other'
    }, 'https://test.site.com/folder/');

    assert.equal(paths.protocol, 'https:');
    assert.equal(paths.root, 'https://something.or.other/');
    assert.equal(paths.resources, 'https://test.site.com/folder/resources/');
    assert.equal(paths.keyboards, 'https://s.keyman.com/keyboard/');
    assert.equal(paths.fonts, 'https://something.or.other/fonts/');
  });

  it('http://test.site.com/folder, non-standard option values', () => {
    const paths = new PathConfiguration({...PathOptionDefaults,
      keyboards: 'https://s.keyman.com/keyboard',
      fonts: '/fonts',
      resources: 'resources',
      root: '//something.or.other'
    }, 'http://test.site.com/folder');

    assert.equal(paths.protocol, 'http:');
    assert.equal(paths.root, 'http://something.or.other/');
    assert.equal(paths.resources, 'http://test.site.com/folder/resources/');
    assert.equal(paths.keyboards, 'https://s.keyman.com/keyboard/');
    assert.equal(paths.fonts, 'http://something.or.other/fonts/');
  });

  it('http://localhost/keymanweb/src/test/manual/web', () => {
    const paths = new PathConfiguration(PathOptionDefaults, 'http://localhost/keymanweb/src/test/manual/web');

    assert.equal(paths.protocol, 'http:');
    assert.equal(paths.root, 'http://localhost/');
    assert.equal(paths.resources, 'http://localhost/keymanweb/src/test/manual/web/');
    assert.equal(paths.keyboards, '');
    assert.equal(paths.fonts, '');
  });

  it('file:///C:/keymanapp/keyman/web/src/test/manual/web', () => {
    const paths = new PathConfiguration({...PathOptionDefaults,
      resources: '../../../../build/resources'
    }, 'file:///C:/keymanapp/keyman/web/src/test/manual/web');

    assert.equal(paths.protocol, 'file:');
    // Is this proper?  We just treat 'root' differently (by default) if being served via file:/// ?
    assert.equal(paths.root, 'file:///C:/keymanapp/keyman/web/src/test/manual/web/');
    assert.equal(paths.resources, 'file:///C:/keymanapp/keyman/web/src/test/manual/web/../../../../build/resources/');
    assert.equal(paths.keyboards, '');
    assert.equal(paths.fonts, '');
  });
});