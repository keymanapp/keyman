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
      if #available(iOSApplicationExtension 11.0, *) {
        return UIColor(named: "SelectedKey", in: engineBundle, compatibleWith: nil)!
      } else {
        return UIColor(red: 136.0 / 255.0,
                       green: 136.0 / 255.0,
                       blue: 1.0,
                       alpha: 1.0)
      }
    }
  }

  // I'm not 100% on the distinction, but this should be fine for now.
  public static var popupKeyTint: UIColor {
    get {
      if #available(iOSApplicationExtension 11.0, *) {
        return UIColor(named: "SelectedKey", in: engineBundle, compatibleWith: nil)!
      } else {
        return UIColor(red: 181.0 / 255.0,
                       green: 181.0 / 255.0,
                       blue: 181.0 / 255.0,
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

  public static var keyboardSelectionPrimary: UIColor {
    get {
      if #available(iOS 11.0, *) {
        return UIColor(named: "KeyboardSelectionPrimary", in: engineBundle, compatibleWith: nil)!
      } else {
        return UIColor(red: 204.0 / 255.0,
                       green: 136.0 / 255.0,
                       blue: 34.0 / 255.0,
                       alpha: 1.0)
      }
    }
  }
}
