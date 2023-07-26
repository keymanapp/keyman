
//
// This mirrors UKeymanTargets.pas
//

export enum
  KeymanTarget {
    any = 'any',
    windows = 'windows', macosx = 'macosx', linux = 'linux',
    web = 'web', iphone = 'iphone', ipad = 'ipad', androidphone = 'androidphone', androidtablet = 'androidtablet',
    mobile = 'mobile', desktop = 'desktop', tablet = 'tablet'
  };

export const
  // Note: if these change, update the copied values in KeyboardParser.pas accordingly
  AllKeymanTargets: KeymanTarget[] = [
    KeymanTarget.any,
    KeymanTarget.windows, KeymanTarget.macosx, KeymanTarget.linux,
    KeymanTarget.web, KeymanTarget.iphone, KeymanTarget.ipad, KeymanTarget.androidphone, KeymanTarget.androidtablet,
    KeymanTarget.mobile, KeymanTarget.desktop, KeymanTarget.tablet
  ],

  TouchKeymanTargets: KeymanTarget[] = [
    KeymanTarget.iphone, KeymanTarget.ipad, KeymanTarget.androidphone, KeymanTarget.androidtablet,
    KeymanTarget.mobile, KeymanTarget.tablet
  ],

  // Compile to .kmx
  KMXKeymanTargets: KeymanTarget[] = [
    KeymanTarget.windows, KeymanTarget.macosx, KeymanTarget.linux, KeymanTarget.desktop
  ],

  // Compile to .js
  KMWKeymanTargets: KeymanTarget[] = [
    KeymanTarget.web, KeymanTarget.iphone, KeymanTarget.ipad, KeymanTarget.androidphone, KeymanTarget.androidtablet,
    KeymanTarget.mobile, KeymanTarget.tablet
  ],

  // Supports .kvks
  KeymanTargetsUsingKVK: KeymanTarget[] = [
    KeymanTarget.windows, KeymanTarget.macosx, KeymanTarget.linux, KeymanTarget.desktop, KeymanTarget.web
  ],

  // Friendly names for targets, e.g. for documentation

  SKeymanTargetNames: { [index: string]: string } = {
    [KeymanTarget.any]: 'All',
    [KeymanTarget.windows]: 'Windows', [KeymanTarget.macosx]: 'macOS', [KeymanTarget.linux]: 'Linux',
    [KeymanTarget.web]: 'Web', [KeymanTarget.iphone]: 'iPhone', [KeymanTarget.ipad]: 'iPad',
      [KeymanTarget.androidphone]: 'Android phone', [KeymanTarget.androidtablet]: 'Android tablet',
    [KeymanTarget.mobile]: 'Mobile devices', [KeymanTarget.desktop]: 'Desktop devices',
      [KeymanTarget.tablet]: 'Tablet devices'
  };


  export function keymanTargetsFromString(targets: string, options?: {expandTargets?: boolean}): KeymanTarget[] {
    let result = new Set<KeymanTarget>(<KeymanTarget[]>targets.split(/ +/));
    if(options?.expandTargets) {
      if(result.has(KeymanTarget.any)) {
        return AllKeymanTargets;
      }
      if(result.has(KeymanTarget.mobile)) {
        result.delete(KeymanTarget.mobile);
        result.add(KeymanTarget.androidphone);
        result.add(KeymanTarget.iphone);
      }
      if(result.has(KeymanTarget.tablet)) {
        result.delete(KeymanTarget.tablet);
        result.add(KeymanTarget.androidtablet);
        result.add(KeymanTarget.ipad);
      }
      if(result.has(KeymanTarget.desktop)) {
        result.delete(KeymanTarget.desktop);
        result.add(KeymanTarget.linux);
        result.add(KeymanTarget.macosx);
        result.add(KeymanTarget.windows);
      }
    }
    return [...result];
  }