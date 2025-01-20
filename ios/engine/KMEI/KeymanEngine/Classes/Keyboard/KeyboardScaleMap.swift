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
import os.log

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

  /**
  * Based off of Device's UIKit size (in points):  refer to
  * https://developer.apple.com/library/archive/documentation/DeviceInformation/Reference/iOSDeviceCompatibility/Displays/Displays.html
  */
  private var pointSizes: [String: CGSize]

  private var phoneScalingThresholds: [Device]
  private var tabletScalingThresholds: [Device]

  static var _shared: KeyboardScaleMap?

  private init() {
    scalings = [:]

    // All scalings below are taken from Simulator readings, using the most-up-to-date version of iOS that they support.
    // Note that the banner heights (at minimum) may vary across iOS versions!
    //
    // None of the widths seem accurate _for the keyboard_, but I've yet to figure out how to get the exact readings.

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

    scalings[KeyboardScaleMap.hashKey(for: Device.iPadMini2)] = Scaling(portrait: KeyboardSize(w: 768, h: 258, b: 55), landscape: KeyboardSize(w: 1024, h: 343, b: 55)) // Banner always shows; merges copy ctrls with pred-banner.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadMini3)] = Scaling(portrait: KeyboardSize(w: 768, h: 258, b: 55), landscape: KeyboardSize(w: 1024, h: 343, b: 55)) // Banner always shows; merges copy ctrls with pred-banner.



    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro10Inch)] = Scaling(portrait: KeyboardSize(w: 834, h: 258, b: 55), landscape: KeyboardSize(w: 1112, h: 343, b: 55)) // Banner always shows; merges copy ctrls with pred-banner.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro12Inch)] = Scaling(portrait: KeyboardSize(w: 1024, h: 323, b: 55), landscape: KeyboardSize(w: 1366, h: 416, b: 55))
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro12Inch2)] = Scaling(portrait: KeyboardSize(w: 1024, h: 323, b: 55), landscape: KeyboardSize(w: 1366, h: 416, b: 55))

    // Max iOS:  13.x (current)
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhoneSE)] = Scaling(portrait: KeyboardSize(w: 320, h: 216, b: 38), landscape: KeyboardSize(w: 568, h: 162, b: 38))  // landscape banner in 10.3.1:  31, not 37 (portrait) or 38!

    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone8)] = Scaling(portrait: KeyboardSize(w: 375, h: 216, b: 44), landscape: KeyboardSize(w: 667, h: 162, b: 38))  // does not use full width in landscape
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone8Plus)] = Scaling(portrait: KeyboardSize(w: 414, h: 226, b: 45), landscape: KeyboardSize(w: 736, h: 162, b: 38))  // does not use full width in landscape

    // Start:  'safe area insets', which are not counted (here) as part of a keyboard's height.
    //         Note that the widths reported for notched phones exclude part of the area to the sides in landscape b/c
    //         of safe-area guidelines.  It's not exact, though, as the system globe & dictation keys go beyond those guides.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhoneX)]  = Scaling(portrait: KeyboardSize(w: 375, h: 216, b: 45), landscape: KeyboardSize(w: 724, h: 150, b: 38))  // insets:  p: 34, l: 21
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhoneXR)]  = Scaling(portrait: KeyboardSize(w: 414, h: 226, b: 45), landscape: KeyboardSize(w: 808, h: 150, b: 38))  // insets:  p: 34, l: 21
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhoneXS)]  = Scaling(portrait: KeyboardSize(w: 414, h: 216, b: 44), landscape: KeyboardSize(w: 724, h: 150, b: 38))  // insets:  p: 34, l: 21.  And yeah, that '44' isn't a typo.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhoneXSMax)]  = Scaling(portrait: KeyboardSize(w: 414, h: 226, b: 45), landscape: KeyboardSize(w: 808, h: 150, b: 38))  // insets:  p: 34, l: 21

    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone11)]  = Scaling(portrait: KeyboardSize(w: 414, h: 226, b: 45), landscape: KeyboardSize(w: 808, h: 150, b: 38))  // insets:  p: 34, l: 21
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone11Pro)]  = Scaling(portrait: KeyboardSize(w: 375, h: 216, b: 45), landscape: KeyboardSize(w: 724, h: 150, b: 38))  // insets:  p: 34, l: 21
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone11ProMax)]  = Scaling(portrait: KeyboardSize(w: 414, h: 226, b: 45), landscape: KeyboardSize(w: 808, h: 150, b: 38))  // insets:  p: 34, l: 21

    scalings[KeyboardScaleMap.hashKey(for: Device.iPadMini4)] = Scaling(portrait: KeyboardSize(w: 768, h: 265, b: 55), landscape: KeyboardSize(w: 1024, h: 343, b: 55)) // Banner always shows; merges copy ctrls with pred-banner.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadMini5)] = Scaling(portrait: KeyboardSize(w: 768, h: 265, b: 55), landscape: KeyboardSize(w: 1024, h: 343, b: 55)) // Banner always shows; merges copy ctrls with pred-banner.

    scalings[KeyboardScaleMap.hashKey(for: Device.iPad7)] = Scaling(portrait: KeyboardSize(w: 810, h: 265, b: 55), landscape: KeyboardSize(w: 1080, h: 353, b: 55)) // Banner always shows; merges copy ctrls with pred-banner.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadAir3)] = Scaling(portrait: KeyboardSize(w: 810, h: 265, b: 55), landscape: KeyboardSize(w: 1112, h: 353, b: 55)) // Banner always shows; merges copy ctrls with pred-banner.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro9Inch)] = Scaling(portrait: KeyboardSize(w: 768, h: 265, b: 55), landscape: KeyboardSize(w: 1024, h: 353, b: 55)) // Banner always shows; merges copy ctrls with pred-banner.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro11Inch)] = Scaling(portrait: KeyboardSize(w: 834, h: 265, b: 55), landscape: KeyboardSize(w: 1194, h: 353, b: 55)) // Banner always shows; has a safe-area inset {p: 20, l: 20}
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro12Inch3)] = Scaling(portrait: KeyboardSize(w: 1024, h: 328, b: 55), landscape: KeyboardSize(w: 1366, h: 423, b: 55)) // Banner always shows; has a safe-area inset {p: 20, l: 20}

    // The following definitions are in sorted order of increasing device resolution.
    // Currrently, this also tends to correlates with the underlying devices' release dates.
    phoneScalingThresholds =  [ Device.iPhoneSE, // smallest phone for 13.x
                                Device.iPhone8,  // is 'pre-notch'
                                Device.iPhoneX,  // smallest with 'notch'
                                Device.iPhoneXR, // largest current model
                              ]

    tabletScalingThresholds = [ Device.iPadMini4, // smallest supported tablet (and is also 13.x)
                                Device.iPad7,
                                Device.iPadAir3,
                                Device.iPadPro11Inch,
                                Device.iPadPro12Inch3, // taller than others of width, but is most recent at the same scale.
                              ]

    pointSizes = [:]

    // `UIScreen.mains.bounds` values for threshold devices.  Taken in "portrait" orientation.
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPhoneSE)] = CGSize(width: 320, height: 568)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPhone8)] = CGSize(width: 375, height: 667)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPhoneX)] = CGSize(width: 375, height: 812)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPhoneXR)] = CGSize(width: 414, height: 896)

    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPadMini4)] = CGSize(width: 768, height: 1024)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPad7)] = CGSize(width: 810, height: 1080)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPadAir3)] = CGSize(width: 834, height: 1112)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPadPro11Inch)] = CGSize(width: 834, height: 1194)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPadPro12Inch3)] = CGSize(width: 1024, height: 1366)
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

  /**
   * Compares the detected dimensions of the current device to those of devices with known keyboard dimensions, returning
   * the largest device smaller than or equal to the detected dimensions.
   */
  private static func getUnknownDeviceMapping(screenSize _screenSize: CGSize = UIScreen.main.bounds.size, asPhone: Bool? = nil) -> Device {
      // Shouldn't happen, but just in case.
      if _screenSize == CGSize.zero {
      // This would notify us whenever new devices are out that we haven't build a mapping for.
      let message = "Cannot detect device dimensions; defaulting to smallest device for form factor."
      os_log("%{public}s", log: KeymanEngineLogger.ui, type: .error, message)
      SentryManager.capture(message, sentryLevel: .error)
    }

    // Convert to CGSize in portrait orientation.
    var screenSize: CGSize
    if _screenSize.width > _screenSize.height {
      screenSize = CGSize(width: _screenSize.height, height: _screenSize.width)
    } else {
      screenSize = CGSize(width: _screenSize.width, height: _screenSize.height)
    }

    // Array for search is determined by form-factor.
    var searchSet: [Device]
    if asPhone ?? false {
      searchSet = KeyboardScaleMap.shared.phoneScalingThresholds
    } else {
      searchSet = KeyboardScaleMap.shared.tabletScalingThresholds
    }

    // Okay, now that we've got the important values, it's time to search.
    let potentialMatches = searchSet.compactMap { device in
      let matchSize = KeyboardScaleMap.shared.pointSizes[KeyboardScaleMap.hashKey(for: device)]!
      if matchSize.width > screenSize.width ||  // if searchSet's device is wider
         matchSize.height > screenSize.height { // or taller
        return nil as Device?                   // it's not a permitted match for mapping uses.
      } else {
        return device
      }
    } as [Device]

    if potentialMatches.count == 0 {
      return searchSet[0] // in case no matches can be found (like the screenSize == CGRect.zero case) - use smallest
    } else {
      // Use the largest potential device, which is the last one returned from the mapping (b/c we presorted the base array)
      return potentialMatches.last!
    }
  }

  static func getDeviceDefaultKeyboardScale(forPortrait: Bool,
                                            onDevice device: Device = Device.current,
                                            screenSize: CGSize = UIScreen.main.bounds.size,
                                            asPhone: Bool? = nil) -> KeyboardSize? {
    if let scaling = shared.scalings[KeyboardScaleMap.hashKey(for: device)] {
      let kbHeight = (forPortrait ? scaling.portrait : scaling.landscape).keyboardHeight
      os_log("KeyboardScaleMap getDeviceDefaultKeyboardScale keyboard height: %f", log:KeymanEngineLogger.ui, type: .debug, kbHeight)
      return forPortrait ? scaling.portrait : scaling.landscape
    }

    var isUnknown = false
    switch device {
      case .unknown(_):
        isUnknown = true
        fallthrough
      default:
        if !isUnknown {
          // We can still perform a mapping, but it's not ideal.
          os_log("Keyboard scaling definition missing for device %{public}s", log: KeymanEngineLogger.ui, type: .default, device.description)
        }

        // The expected case:  isUnknown = true.
        // Applies to future models of iPhone and iPad.  DeviceKit is of no help here, so we
        // compute the closest-resolution known device.
        let mappedDevice = getUnknownDeviceMapping(screenSize: screenSize, asPhone: asPhone ?? (UIDevice.current.userInterfaceIdiom == .phone))

        // As noted in the scalings data-map, devices of the same dimensions tend to have the same
        // keyboard dimensions.  It's not a perfect rule, but should suffice for a stop-gap solution.
        let scaling = shared.scalings[KeyboardScaleMap.hashKey(for: mappedDevice)]!

        let kbHeight = (forPortrait ? scaling.portrait : scaling.landscape).keyboardHeight
        os_log("KeyboardScaleMap getDeviceDefaultKeyboardScale keyboard height for missing device: %f", log:KeymanEngineLogger.ui, type: .debug, kbHeight)

        return forPortrait ? scaling.portrait : scaling.landscape
    }
  }
}
