//
//  KeyboardScaleMap.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 1/9/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
import DeviceKit
import UIKit

/**
 * Documents the basic size properties of the default system keyboard, _as seen from within a `UIInputView` or its `UIInputViewController`_.
 */
struct KeyboardSize {
  var width: CGFloat
  var keyboardHeight: CGFloat
  var bannerHeight: CGFloat

  init(width: CGFloat, keyboardHeight: CGFloat, bannerHeight: CGFloat) {
    self.width = width
    self.keyboardHeight = keyboardHeight
    self.bannerHeight = bannerHeight
  }

  init(w: CGFloat, h: CGFloat, b: CGFloat) {
    self.init(width: w, keyboardHeight: h, bannerHeight: b)
  }
}

struct Scaling {
  var landscape: KeyboardSize
  var portrait: KeyboardSize

  init(portrait: KeyboardSize, landscape: KeyboardSize) {
    self.portrait = portrait
    self.landscape = landscape
  }
}

class KeyboardScaleMap {
  private var scalings: [String: Scaling]
  static var _shared: KeyboardScaleMap?

  private init() {
    scalings = [:]

    // All scalings below are taken from Simulator readings, using the most-up-to-date version of iOS that they support.
    // Note that the banner heights (at minimum) may vary across iOS versions!
    //
    // None of the widths seem accurate, but I've yet to figure out how to get the exact readings.

    // Nothing available for iOS 9.3.5 - Xcode does not provide a Simulator version.

    // Max iOS:  10.x
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone5)] = Scaling(portrait: KeyboardSize(w: 320, h: 216, b: 37), landscape: KeyboardSize(w: 568, h: 162, b: 31)) // 10.3.1

    // Max iOS:  12.x
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone5s)] = Scaling(portrait: KeyboardSize(w: 320, h: 216, b: 37), landscape: KeyboardSize(w: 568, h: 162, b: 37)) // b: 31 for landscape on 10.3.1

    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone6)] = Scaling(portrait: KeyboardSize(w: 375, h: 216, b: 44), landscape: KeyboardSize(w: 667, h: 162, b: 38))  // does not use full width in landscape
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone6Plus)] = Scaling(portrait: KeyboardSize(w: 414, h: 226, b: 45), landscape: KeyboardSize(w: 736, h: 162, b: 38))  // does not use full width in landscape
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone6s)] = Scaling(portrait: KeyboardSize(w: 375, h: 216, b: 44), landscape: KeyboardSize(w: 667, h: 162, b: 38))  // does not use full width in landscape
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone6sPlus)] = Scaling(portrait: KeyboardSize(w: 414, h: 226, b: 45), landscape: KeyboardSize(w: 736, h: 162, b: 38))  // does not use full width in landscape

    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone7)] = Scaling(portrait: KeyboardSize(w: 375, h: 216, b: 44), landscape: KeyboardSize(w: 667, h: 162, b: 38))  // does not use full width in landscape
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone7Plus)] = Scaling(portrait: KeyboardSize(w: 414, h: 226, b: 45), landscape: KeyboardSize(w: 736, h: 162, b: 38))  // does not use full width in landscape

    scalings[KeyboardScaleMap.hashKey(for: Device.iPad5)] = Scaling(portrait: KeyboardSize(w: 768, h: 258, b: 55), landscape: KeyboardSize(w: 1024, h: 343, b: 55)) // Banner always shows; merges copy ctrls with pred-banner.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPad6)] = Scaling(portrait: KeyboardSize(w: 768, h: 258, b: 55), landscape: KeyboardSize(w: 1024, h: 343, b: 55)) // Banner always shows; merges copy ctrls with pred-banner.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadAir)] = Scaling(portrait: KeyboardSize(w: 768, h: 258, b: 55), landscape: KeyboardSize(w: 1024, h: 343, b: 55)) // Banner always shows; merges copy ctrls with pred-banner.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadAir2)] = Scaling(portrait: KeyboardSize(w: 768, h: 258, b: 55), landscape: KeyboardSize(w: 1024, h: 343, b: 55)) // Banner always shows; merges copy ctrls with pred-banner.

    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro10Inch)] = Scaling(portrait: KeyboardSize(w: 834, h: 258, b: 55), landscape: KeyboardSize(w: 1112, h: 343, b: 55)) // Banner always shows; merges copy ctrls with pred-banner.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro12Inch)] = Scaling(portrait: KeyboardSize(w: 1024, h: 323, b: 55), landscape: KeyboardSize(w: 1366, h: 416, b: 55))
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro12Inch2)] = Scaling(portrait: KeyboardSize(w: 1024, h: 323, b: 55), landscape: KeyboardSize(w: 1366, h: 416, b: 55))

    // Max iOS:  13.x (current)
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhoneSE)] = Scaling(portrait: KeyboardSize(w: 320, h: 216, b: 38), landscape: KeyboardSize(w: 568, h: 162, b: 38))  // landscape banner in 10.3.1:  31, not 37 (portrait) or 38!

    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone8)] = Scaling(portrait: KeyboardSize(w: 375, h: 216, b: 44), landscape: KeyboardSize(w: 667, h: 162, b: 38))  // does not use full width in landscape
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone8Plus)] = Scaling(portrait: KeyboardSize(w: 414, h: 226, b: 45), landscape: KeyboardSize(w: 736, h: 162, b: 38))  // does not use full width in landscape

    // Start:  'safe area insets', which are not counted (here) as part of a keyboard's height.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhoneX)]  = Scaling(portrait: KeyboardSize(w: 375, h: 216, b: 45), landscape: KeyboardSize(w: 724, h: 150, b: 38))  // insets:  p: 34, l: 21
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhoneXR)]  = Scaling(portrait: KeyboardSize(w: 414, h: 226, b: 45), landscape: KeyboardSize(w: 808, h: 150, b: 38))  // insets:  p: 34, l: 21
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhoneXS)]  = Scaling(portrait: KeyboardSize(w: 414, h: 216, b: 44), landscape: KeyboardSize(w: 724, h: 150, b: 38))  // insets:  p: 34, l: 21.  And yeah, that '44' isn't a typo.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhoneXSMax)]  = Scaling(portrait: KeyboardSize(w: 414, h: 226, b: 45), landscape: KeyboardSize(w: 808, h: 150, b: 38))  // insets:  p: 34, l: 21

    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone11)]  = Scaling(portrait: KeyboardSize(w: 414, h: 226, b: 45), landscape: KeyboardSize(w: 808, h: 150, b: 38))  // insets:  p: 34, l: 21
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone11Pro)]  = Scaling(portrait: KeyboardSize(w: 375, h: 216, b: 45), landscape: KeyboardSize(w: 724, h: 150, b: 38))  // insets:  p: 34, l: 21
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone11ProMax)]  = Scaling(portrait: KeyboardSize(w: 414, h: 226, b: 45), landscape: KeyboardSize(w: 808, h: 150, b: 38))  // insets:  p: 34, l: 21

    scalings[KeyboardScaleMap.hashKey(for: Device.iPad7)] = Scaling(portrait: KeyboardSize(w: 810, h: 265, b: 55), landscape: KeyboardSize(w: 1080, h: 353, b: 55)) // Banner always shows; merges copy ctrls with pred-banner.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadAir3)] = Scaling(portrait: KeyboardSize(w: 810, h: 265, b: 55), landscape: KeyboardSize(w: 1112, h: 353, b: 55)) // Banner always shows; merges copy ctrls with pred-banner.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro9Inch)] = Scaling(portrait: KeyboardSize(w: 768, h: 265, b: 55), landscape: KeyboardSize(w: 1024, h: 353, b: 55)) // Banner always shows; merges copy ctrls with pred-banner.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro11Inch)] = Scaling(portrait: KeyboardSize(w: 834, h: 265, b: 55), landscape: KeyboardSize(w: 1194, h: 353, b: 55)) // Banner always shows; has a safe-area inset {p: 20, l: 20}
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro12Inch3)] = Scaling(portrait: KeyboardSize(w: 1024, h: 328, b: 55), landscape: KeyboardSize(w: 1366, h: 423, b: 55)) // Banner always shows; has a safe-area inset {p: 20, l: 20}
  }

  public static var shared: KeyboardScaleMap {
    get {
      if _shared == nil {
        _shared = KeyboardScaleMap()
      }

      return _shared!
    }
  }

  private static func hashKey(for device: Device) -> String {
    // Ignore any Simulator intermediaries.
    return device.realDevice.description
  }

  static func getBaseTargetHeight(forPortrait: Bool) -> CGFloat {
    let device = Device.current

    if let scaling = shared.scalings[KeyboardScaleMap.hashKey(for: device)] {
      return forPortrait ? scaling.portrait.keyboardHeight : scaling.landscape.keyboardHeight
    }

    // VERY temporary stop-gap.
    let scaling = shared.scalings[KeyboardScaleMap.hashKey(for: Device.iPhoneSE)]!
    return forPortrait ? scaling.portrait.keyboardHeight : scaling.landscape.keyboardHeight
  }
}
