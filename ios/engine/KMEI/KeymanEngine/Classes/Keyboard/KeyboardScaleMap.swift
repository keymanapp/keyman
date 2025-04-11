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

    // All scalings below are taken from Simulator readings against iOS 18.x.
    //
    // None of the widths seem accurate _for the keyboard_, but I've yet to figure out how to get the exact readings.

    // Notchless
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhoneSE2)] = Scaling(portrait: KeyboardSize(w: 375, h: 216, b: 44), landscape: KeyboardSize(w: 667, h: 162, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhoneSE3)] = Scaling(portrait: KeyboardSize(w: 375, h: 216, b: 44), landscape: KeyboardSize(w: 667, h: 162, b: 38))
    
    // Notched
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone16ProMax)] = Scaling(portrait: KeyboardSize(w: 440, h: 226, b: 45), landscape: KeyboardSize(w: 832, h: 160, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone16Pro)] = Scaling(portrait: KeyboardSize(w: 402, h: 216, b: 45), landscape: KeyboardSize(w: 750, h: 160, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone16Plus)] = Scaling(portrait: KeyboardSize(w: 430, h: 226, b: 45), landscape: KeyboardSize(w: 814, h: 160, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone16)] = Scaling(portrait: KeyboardSize(w: 393, h: 216, b: 45), landscape: KeyboardSize(w: 734, h: 160, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone15ProMax)] = Scaling(portrait: KeyboardSize(w: 430, h: 226, b: 45), landscape: KeyboardSize(w: 814, h: 160, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone15Plus)] = Scaling(portrait: KeyboardSize(w: 430, h: 226, b: 45), landscape: KeyboardSize(w: 814, h: 160, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone15Pro)] = Scaling(portrait: KeyboardSize(w: 393, h: 216, b: 45), landscape: KeyboardSize(w: 734, h: 160, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone15)] = Scaling(portrait: KeyboardSize(w: 393, h: 216, b: 45), landscape: KeyboardSize(w: 734, h: 160, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone14ProMax)] = Scaling(portrait: KeyboardSize(w: 430, h: 226, b: 45), landscape: KeyboardSize(w: 814, h: 160, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone14Plus)] = Scaling(portrait: KeyboardSize(w: 428, h: 226, b: 45), landscape: KeyboardSize(w: 832, h: 160, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone14Pro)] = Scaling(portrait: KeyboardSize(w: 393, h: 216, b: 45), landscape: KeyboardSize(w: 734, h: 160, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone14)] = Scaling(portrait: KeyboardSize(w: 390, h: 216, b: 45), landscape: KeyboardSize(w: 750, h: 160, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone13ProMax)] = Scaling(portrait: KeyboardSize(w: 428, h: 226, b: 45), landscape: KeyboardSize(w: 832, h: 160, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone13Pro)] = Scaling(portrait: KeyboardSize(w: 390, h: 216, b: 45), landscape: KeyboardSize(w: 750, h: 160, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone13)] = Scaling(portrait: KeyboardSize(w: 390, h: 216, b: 45), landscape: KeyboardSize(w: 750, h: 160, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone13Mini)] = Scaling(portrait: KeyboardSize(w: 375, h: 216, b: 45), landscape: KeyboardSize(w: 712, h: 150, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone12ProMax)] = Scaling(portrait: KeyboardSize(w: 428, h: 226, b: 45), landscape: KeyboardSize(w: 832, h: 160, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone12Pro)] = Scaling(portrait: KeyboardSize(w: 390, h: 216, b: 45), landscape: KeyboardSize(w: 750, h: 160, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone12)] = Scaling(portrait: KeyboardSize(w: 390, h: 216, b: 45), landscape: KeyboardSize(w: 750, h: 160, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone12Mini)] = Scaling(portrait: KeyboardSize(w: 375, h: 216, b: 45), landscape: KeyboardSize(w: 712, h: 150, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone11ProMax)] = Scaling(portrait: KeyboardSize(w: 414, h: 226, b: 45), landscape: KeyboardSize(w: 808, h: 150, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone11Pro)] = Scaling(portrait: KeyboardSize(w: 375, h: 216, b: 45), landscape: KeyboardSize(w: 724, h: 150, b: 38))
    
    // Slight difference here on landscape width from the iOS 13 measurement, but that's all.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhone11)] = Scaling(portrait: KeyboardSize(w: 414, h: 226, b: 45), landscape: KeyboardSize(w: 800, h: 150, b: 38))
    
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhoneXSMax)] = Scaling(portrait: KeyboardSize(w: 414, h: 226, b: 45), landscape: KeyboardSize(w: 808, h: 150, b: 38))
    
    // This had markedly different measurements when taken back in iOS 13.x!
    // (Was there a misrecording at the time?)
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhoneXS)] = Scaling(portrait: KeyboardSize(w: 375, h: 216, b: 45), landscape: KeyboardSize(w: 724, h: 150, b: 38))
    
    // Slight difference here on landscape width from the iOS 13 measurement, but that's all.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPhoneXR)]  = Scaling(portrait: KeyboardSize(w: 414, h: 226, b: 45), landscape: KeyboardSize(w: 800, h: 150, b: 38))
    
    // -----------
 
    // Note:  tablets have an "assistant" bar that iOS also uses for its system
    // suggestions.  We can't tap into that.  So, while we want a similar banner
    // shape... it gets double-bannered in use. :(

    scalings[KeyboardScaleMap.hashKey(for: Device.iPadMini5)] = Scaling(portrait: KeyboardSize(w: 768, h: 265, b: 55), landscape: KeyboardSize(w: 1024, h: 353, b: 55)) // Banner always shows; merges copy ctrls with pred-banner.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadMini6)] = Scaling(portrait: KeyboardSize(w: 744, h: 265-5, b: 55), landscape: KeyboardSize(w: 1133, h: 353-5, b: 55)) // has negative 'system inset' (-5), which causes a mismatch in final height balanced by the subtraction
    
    // Mini A17 Pro does not appear to be supported by our current DeviceKit.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPad7)] = Scaling(portrait: KeyboardSize(w: 810, h: 265, b: 55), landscape: KeyboardSize(w: 1080, h: 353, b: 55))
    scalings[KeyboardScaleMap.hashKey(for: Device.iPad8)] = Scaling(portrait: KeyboardSize(w: 810, h: 265, b: 55), landscape: KeyboardSize(w: 1080, h: 353, b: 55))
    scalings[KeyboardScaleMap.hashKey(for: Device.iPad9)] = Scaling(portrait: KeyboardSize(w: 810, h: 265, b: 55), landscape: KeyboardSize(w: 1080, h: 353, b: 55))
    scalings[KeyboardScaleMap.hashKey(for: Device.iPad10)] = Scaling(portrait: KeyboardSize(w: 820, h: 262-5, b: 55), landscape: KeyboardSize(w: 1180, h: 347-5, b: 55)) // has negative 'system inset' (-5), which causes a mismatch in final height balanced by the subtraction
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadAir3)] = Scaling(portrait: KeyboardSize(w: 834, h: 265, b: 55), landscape: KeyboardSize(w: 1112, h: 353, b: 55))
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadAir4)] = Scaling(portrait: KeyboardSize(w: 820, h: 262-5, b: 55), landscape: KeyboardSize(w: 1180, h: 347-5, b: 55))
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadAir5)] = Scaling(portrait: KeyboardSize(w: 820, h: 262-5, b: 55), landscape: KeyboardSize(w: 1180, h: 347-5, b: 55))
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadAir11M2)] = Scaling(portrait: KeyboardSize(w: 820, h: 262-5, b: 55), landscape: KeyboardSize(w: 1180, h: 347-5, b: 55))
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadAir13M2)] = Scaling(portrait: KeyboardSize(w: 1024, h: 328-5, b: 55), landscape: KeyboardSize(w: 1366, h: 423-5, b: 55))
    // paused after iPadAir set.
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro11Inch)] = Scaling(portrait: KeyboardSize(w: 834, h: 265-5, b: 55), landscape: KeyboardSize(w: 1194, h: 353-5, b: 55))
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro11Inch2)] = Scaling(portrait: KeyboardSize(w: 834, h: 265-5, b: 55), landscape: KeyboardSize(w: 1194, h: 353-5, b: 55))
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro11Inch3)] = Scaling(portrait: KeyboardSize(w: 834, h: 265-5, b: 55), landscape: KeyboardSize(w: 1194, h: 353-5, b: 55))
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro11Inch4)] = Scaling(portrait: KeyboardSize(w: 834, h: 265-5, b: 55), landscape: KeyboardSize(w: 1194, h: 353-5, b: 55))
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro11M4)] = Scaling(portrait: KeyboardSize(w: 834, h: 265-5, b: 55), landscape: KeyboardSize(w: 1210, h: 353-5, b: 55))
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro12Inch3)] = Scaling(portrait: KeyboardSize(w: 1024, h: 328-5, b: 55), landscape: KeyboardSize(w: 1366, h: 423-5, b: 55))
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro12Inch4)] = Scaling(portrait: KeyboardSize(w: 1024, h: 328-5, b: 55), landscape: KeyboardSize(w: 1366, h: 423-5, b: 55))
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro12Inch5)] = Scaling(portrait: KeyboardSize(w: 1024, h: 328-5, b: 55), landscape: KeyboardSize(w: 1366, h: 423-5, b: 55))
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro12Inch6)] = Scaling(portrait: KeyboardSize(w: 1024, h: 328-5, b: 55), landscape: KeyboardSize(w: 1366, h: 423-5, b: 55))
    scalings[KeyboardScaleMap.hashKey(for: Device.iPadPro13M4)] = Scaling(portrait: KeyboardSize(w: 1032, h: 330.5-5, b: 55), landscape: KeyboardSize(w: 1376, h: 426-5, b: 55))

    // The following definitions are in sorted order of increasing device resolution.
    // This tends to correlates with the underlying devices' release dates,
    // though not perfectly - note the XR's position!
    phoneScalingThresholds =  [ Device.iPhoneSE3, // only 'pre-notch' size available
                                Device.iPhoneXS,  // is smallest 'notch' still current
                                Device.iPhone14,  // moving up in size from here
                                Device.iPhone15Pro,
                                Device.iPhoneXR,
                                Device.iPhone15ProMax,
                                Device.iPhone16ProMax // largest current device
                              ]

    tabletScalingThresholds = [ Device.iPadMini5, // absolute smallest?
                                Device.iPadMini6, // thinner but taller than Mini5.
                                Device.iPad7,
                                Device.iPad10,
                                Device.iPadAir3,
                                Device.iPadPro11Inch,
                                Device.iPadPro11M4,
                                Device.iPadAir13M2,// same as the 12.9" models.
                                Device.iPadPro13M4
                              ]

    pointSizes = [:]

    // `UIScreen.mains.bounds` values for threshold devices.  Taken in "portrait" orientation.
    // SEs are notchless.
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPhoneSE2)] = CGSize(width: 375, height: 667)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPhoneSE3)] = CGSize(width: 375, height: 667)
    
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPhoneXS)] = CGSize(width: 375, height: 812)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPhone11Pro)] = CGSize(width: 375, height: 812)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPhone14)] = CGSize(width: 390, height: 844)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPhone15Pro)] = CGSize(width: 393, height: 852)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPhoneXR)] = CGSize(width: 414, height: 896)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPhone15ProMax)] = CGSize(width: 430, height: 932)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPhone16ProMax)] = CGSize(width: 440, height: 956)
    
    
    // ------
    
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPadMini6)] = CGSize(width: 744, height: 1133)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPadMini5)] = CGSize(width: 768, height: 1024)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPad7)] = CGSize(width: 810, height: 1080)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPad10)] = CGSize(width: 820, height: 1180)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPadAir3)] = CGSize(width: 834, height: 1112)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPadPro11Inch)] = CGSize(width: 834, height: 1194)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPadPro11M4)] = CGSize(width: 834, height: 1210)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPadAir13M2)] = CGSize(width: 1024, height: 1366)
    pointSizes[KeyboardScaleMap.hashKey(for: Device.iPadPro13M4)] = CGSize(width: 1032, height: 1376)
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
          SentryManager.capture("Keyboard scaling definition missing for device \(device.description)", sentryLevel: .warning)
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
