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
      return UIColor(named: "KeyPrimary", in: engineBundle, compatibleWith: nil)!
    }
  }
  
  public static var keyText: UIColor {
    get {
      return UIColor(named: "KeyText", in: engineBundle, compatibleWith: nil)!
    }
  }
  
  public static var popupKeyHighlighted: UIColor {
    get {
      return UIColor(named: "SelectedKey", in: engineBundle, compatibleWith: nil)!
    }
  }
  
  // I'm not 100% on the distinction, but this should be fine for now.
  public static var popupKeyTint: UIColor {
    get {
      return UIColor(named: "SelectedKey", in: engineBundle, compatibleWith: nil)!
    }
  }
  
  public static var keyboardBackground: UIColor {
    get {
      return UIColor(named: "KeyboardBackground", in: engineBundle, compatibleWith: nil)!
    }
  }
  
  public static var keyboardSelectionPrimary: UIColor {
    get {
      return UIColor(named: "KeyboardSelectionPrimary", in: engineBundle, compatibleWith: nil)!
    }
  }
  
  public static var helpBubbleGradient1: UIColor {
    get {
      return UIColor(named: "HelpBubbleGradient1", in: engineBundle, compatibleWith: nil)!
    }
  }
  
  public static var helpBubbleGradient2: UIColor {
    get {
      return UIColor(named: "HelpBubbleGradient2", in: engineBundle, compatibleWith: nil)!
    }
  }
}
