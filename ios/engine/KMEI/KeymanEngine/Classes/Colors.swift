//
//  Colors.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 1/15/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
import UIKit

// Used to facilitate constant colors with legacy (pre iOS-11.0) devices,
// as they can't use "Color Assets".
public class Colors {
  private static var engineBundle: Bundle {
    get {
      // We have to specify the bundle for _this framework_ since these are not
      // set by the app.
      return Bundle(for: Manager.self)
    }
  }

  public static var popupBorder: UIColor {
    get {
//      if #available(iOSApplicationExtension 11.0, *) {
//        return UIColor(named: "SelectionPrimary")!
//      } else {
        return UIColor(red: 134.0 / 255.0,
                       green: 137.0 / 255.0,
                       blue: 139.0 / 255.0,
                       alpha: 1.0)
//      }
    }
  }

  public static var popupKey: UIColor {
    get {
      if #available(iOSApplicationExtension 11.0, *) {
        return UIColor(named: "KeyPrimary", in: engineBundle, compatibleWith: nil)!
      } else {
        return UIColor(red: 244.0 / 255.0,
                       green: 244.0 / 255.0,
                       blue: 244.0 / 255.0,
                       alpha: 1.0)
      }
    }
  }

  public static var keyText: UIColor {
    get {
      if #available(iOSApplicationExtension 11.0, *) {
        return UIColor(named: "KeyText", in: engineBundle, compatibleWith: nil)!
      } else {
        return UIColor.black
      }
    }
  }

  public static var popupKeyHighlighted: UIColor {
    get {
//      if #available(iOSApplicationExtension 11.0, *) {
//        return UIColor(named: "SelectionPrimary")!
//      } else {
        return UIColor(red: 136.0 / 255.0,
                       green: 136.0 / 255.0,
                       blue: 1.0,
                       alpha: 1.0)
//      }
    }
  }

  public static var popupKeyTint: UIColor {
    get {
      if #available(iOSApplicationExtension 11.0, *) {
        return UIColor(named: "SelectionPrimary")!
      } else {
        return UIColor(red: 181.0 / 255.0,
                       green: 181.0 / 255.0,
                       blue: 181.0 / 255.0,
                       alpha: 1.0)
      }
    }
  }

  public static var helpBubbleGradient1: UIColor {
    get {
      if #available(iOSApplicationExtension 11.0, *) {
        return UIColor(named: "HelpBubbleGradient1")!
      } else {
        return UIColor(red: 253.0 / 255.0,
                       green: 244.0 / 255.0,
                       blue: 196.0 / 255.0,
                       alpha: 1.0)
      }
    }
  }

  public static var helpBubbleGradient2: UIColor {
    get {
      if #available(iOSApplicationExtension 11.0, *) {
        return UIColor(named: "HelpBubbleGradient2")!
      } else {
        return UIColor(red: 233.0 / 255.0,
                       green: 224.0 / 255.0,
                       blue: 176.0 / 255.0,
                       alpha: 1.0)
      }
    }
  }

  public static var keyboardBackground: UIColor {
    get {
      if #available(iOSApplicationExtension 11.0, *) {
        return UIColor(named: "KeyboardBackground", in: engineBundle, compatibleWith: nil)!
      } else {
        return UIColor(red: 210.0 / 255.0,
                       green: 214.0 / 255.0,
                       blue: 220.0 / 255.0,
                       alpha: 1.0)
      }
    }
  }
}
