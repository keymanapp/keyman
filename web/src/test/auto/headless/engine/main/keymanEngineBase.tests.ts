/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2026-08-12
 *
 * basic unit tests for KeymanEngineBase class
 */
import { assert } from 'chai';
import { ContextManagerBase, EngineConfiguration, KeyboardInfoPair, KeymanEngineBase } from 'keyman/engine/main';
import KEYMAN_VERSION from '@keymanapp/keyman-version';
import { Keyboard, TextStore } from 'keyman/engine/keyboard';
import { KeyboardStub } from 'keyman/engine/keyboard-storage';
import { DeviceSpec } from 'keyman/common/web-utils';

class ContextManagerTest extends ContextManagerBase<EngineConfiguration> {
  initialize(): void {}
  get activeTextStore(): TextStore { return null; }
  get activeKeyboard(): {keyboard: Keyboard, metadata: KeyboardStub} { return null };
  protected currentKeyboardSrcTextStore(): TextStore | null { return null; }
  protected activateKeyboardForTextStore(kbd: KeyboardInfoPair, textStore: TextStore): void {}
  protected getFallbackStubKey(): { id: string, langId: string } { return null; }
}

/**
 * construct a stubbed KeymanEngineBase suitable for unit testing
 */
function instantiate() {
  const device = new DeviceSpec(DeviceSpec.Browser.Chrome, DeviceSpec.FormFactor.Desktop, DeviceSpec.OperatingSystem.Windows, false);
  const config = new EngineConfiguration('', device);
  const keyman = new KeymanEngineBase(null, config, new ContextManagerTest(config), (engine) => ({
    baseLayout: 'us',
    keyboardInterface: null, // new KeyboardInterfaceBase(window, engine, config.stubNamespacer),
    defaultOutputRules: null //new DefaultOutputRules()
  }));
  return keyman;
}

describe('KeymanEngineBase', function () {
  it('returns expected version information', async function () {
    const keyman = instantiate();
    assert.isNumber(keyman.build);
    assert.equal(keyman.build, parseInt(KEYMAN_VERSION.VERSION_PATCH, 10));

    assert.isString(keyman.version);
    assert.equal(keyman.version, KEYMAN_VERSION.VERSION_RELEASE);

    assert.oneOf(keyman.versionInfo.environment, ['alpha', 'beta', 'test', 'local']);
    assert.equal(keyman.versionInfo.environment, KEYMAN_VERSION.VERSION_ENVIRONMENT);

    assert.isNumber(keyman.versionInfo.major);
    assert.equal(keyman.versionInfo.major, parseInt(KEYMAN_VERSION.VERSION_MAJOR, 10));

    assert.isNumber(keyman.versionInfo.minor);
    assert.equal(keyman.versionInfo.minor, parseInt(KEYMAN_VERSION.VERSION_MINOR, 10));

    assert.isNumber(keyman.versionInfo.patch);
    assert.equal(keyman.versionInfo.patch, parseInt(KEYMAN_VERSION.VERSION_PATCH, 10));

    assert.oneOf(keyman.versionInfo.tier, ['alpha', 'beta', 'stable']);
    assert.equal(keyman.versionInfo.tier, KEYMAN_VERSION.TIER);

    assert.match(keyman.versionInfo.version, /^\d+\.\d+\.\d+$/);
    assert.equal(keyman.versionInfo.version, KEYMAN_VERSION.VERSION);

    // see builder-basic.inc.sh for details on how VERSION_WITH_TAG is constructed
    assert.match(keyman.versionInfo.full, /^\d+\.\d+\.\d+(-(alpha|beta))?(-(local|(test(-.+)?)))?$/);
    assert.equal(keyman.versionInfo.full, KEYMAN_VERSION.VERSION_WITH_TAG);
  });
});