//
//  KeyboardScaleTests.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 2/20/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import XCTest
@testable import KeymanEngine
import DeviceKit
import os.log

extension KeyboardSize: Equatable {
  public static func == (lhs: KeyboardSize, rhs: KeyboardSize) -> Bool {
    if lhs.bannerHeight != rhs.bannerHeight {
      return false
    }
    
    if lhs.width != rhs.width {
      return false
    }
    
    if lhs.keyboardHeight != rhs.keyboardHeight {
      return false
    }
    
    return true
  }
  
  
}

class KeyboardScaleTests: XCTestCase {
  
  // Known to be in the scaling map directly.
  func testScaleForSE() {
    let portraitScale = KeyboardScaleMap.getDeviceDefaultKeyboardScale(forPortrait: true, onDevice: Device.iPhoneSE)!
    let landscapeScale = KeyboardScaleMap.getDeviceDefaultKeyboardScale(forPortrait: false, onDevice: Device.iPhoneSE)!
    // scalings[KeyboardScaleMap.hashKey(for: Device.iPhoneSE)]
    //   = Scaling(portrait: KeyboardSize(w: 320, h: 216, b: 38),
    //             landscape: KeyboardSize(w: 568, h: 162, b: 38))
    
    XCTAssertEqual(portraitScale.bannerHeight, 38)
    XCTAssertEqual(portraitScale.width, 320)
    XCTAssertEqual(portraitScale.keyboardHeight, 216)
    
    XCTAssertEqual(landscapeScale.bannerHeight, 38)
    XCTAssertEqual(landscapeScale.width, 568)
    XCTAssertEqual(landscapeScale.keyboardHeight, 162)
  }
  
  func testScaleForUnknown() {
    let device = Device.unknown("granny smith")
    let message = "\(device.isPad)"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, message)
    let size1 = CGSize(width: 375, height: 667) // iPhone 8 device height
    
    let mappedScale1 = KeyboardScaleMap.getDeviceDefaultKeyboardScale(forPortrait: true,
                                                                      onDevice: device,
                                                                      screenSize: size1,
                                                                      asPhone: true)
    
    let correctScale1 = KeyboardScaleMap.getDeviceDefaultKeyboardScale(forPortrait: true, onDevice: Device.iPhone8)
    
    XCTAssertEqual(mappedScale1, correctScale1, "Unknown phone with same resolution as iPhone 8 not given same keyboard dimensions")
    
    let size2 = CGSize(width: 834, height: 1112) // iPad Air 3
    
    let mappedScale2 = KeyboardScaleMap.getDeviceDefaultKeyboardScale(forPortrait: false,
                                                                      onDevice: device,
                                                                      screenSize: size2,
                                                                      asPhone: false)
    
    let correctScale2 = KeyboardScaleMap.getDeviceDefaultKeyboardScale(forPortrait: false, onDevice: Device.iPadAir3)
    
    XCTAssertEqual(mappedScale2, correctScale2, "Unknown tablet with same resolution as iPadAir 3 not given same keyboard dimensions")
    
    let size3 = CGSize(width: 1800, height: 2000) // larger than any existing device
    
    let mappedScale3 = KeyboardScaleMap.getDeviceDefaultKeyboardScale(forPortrait: true,
                                                                      onDevice: device,
                                                                      screenSize: size3,
                                                                      asPhone: false)
    
    let correctScale3 = KeyboardScaleMap.getDeviceDefaultKeyboardScale(forPortrait: true, onDevice: Device.iPadPro12Inch3) // largest iPad
    
    XCTAssertEqual(mappedScale3, correctScale3, "Unknown super-large tablet not given largest keyboard dimension mapping")
  }
}
