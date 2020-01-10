//
//  KeyboardScaleMap.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 1/9/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
import DeviceKit

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

    // All scalings below are taken from Simulator readings of the default system keyboard with its predictive text inactive.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhoneSE)] = Scaling(portrait: KeyboardSize(w: 320, h: 216, b: 37), landscape: KeyboardSize(w: 568, h: 162, b: 37))
    // with prediction, 336 portrait, 209 landscape
//    scalings[KeyboardScaleMap.hashKey(for: Device.iPhoneX)]  = Scaling(portrait: CGSize(width: 375, height: 291), landscape: CGSize(width: 812, height: 171))
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhoneX)]  = Scaling(portrait: KeyboardSize(w: 375, h: 223, b: 45), landscape: KeyboardSize(w: 812, h: 150, b: 38))
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
    let device = Device.init()

    if let scaling = shared.scalings[KeyboardScaleMap.hashKey(for: device)] {
      return forPortrait ? scaling.portrait.keyboardHeight : scaling.landscape.keyboardHeight
    }

    // VERY temporary stop-gap.
    let scaling = shared.scalings[KeyboardScaleMap.hashKey(for: Device.iPhoneSE)]!
    return forPortrait ? scaling.portrait.keyboardHeight : scaling.landscape.keyboardHeight
  }
}
