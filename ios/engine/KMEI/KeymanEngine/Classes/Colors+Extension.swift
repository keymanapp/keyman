//
//  Colors+Extensions.swift
//  Keyman
//
//  Created by Joshua Horton on 1/15/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
import UIKit

// TODO:  Relocated to the Keyman App's project.  Not possible while the Settings menus are part of KMEI.
extension Colors {
  public static var systemBackground: UIColor {
    get {
      if #available(iOS 13.0, *) {
        return UIColor.systemBackground
      } else {
        return UIColor.white
      }
    }
  }

  // The primary color used for selected UI elements in the settings menu.
  public static var selectionPrimary: UIColor {
    get {
      if #available(iOS 11.0, *) {
        return UIColor(named: "SelectionPrimary")!
      } else {
        return UIColor(red: 204.0 / 255.0,
                       green: 136.0 / 255.0,
                       blue: 34.0 / 255.0,
                       alpha: 1.0)
      }
    }
  }

  // The primary color used for selected UI elements in Get Started menu.
  public static var selectionSecondary: UIColor {
    get {
      if #available(iOS 11.0, *) {
        return UIColor(named: "SelectionSecondary")!
      } else {
        return UIColor(red: 95.0 / 255.0,
                       green: 196.0 / 255.0,
                       blue: 217.0 / 255.0,
                       alpha: 1.0)
      }
    }
  }

  public static var statusToolbar: UIColor {
    get {
      if #available(iOS 11.0, *) {
        return UIColor(named: "StatusToolbar")!
      } else {
        return UIColor(red: 0.5,
                       green: 0.75,
                       blue: 0.25,
                       alpha: 0.9)
      }
    }
  }

  public static var statusResourceUpdateButton: UIColor {
    get {
      if #available(iOS 11.0, *) {
        return UIColor(named: "StatusResourceUpdateButton")!
      } else {
        return UIColor(red: 0.75,
                       green: 1.0,
                       blue: 0.5,
                       alpha: 1.0)
      }
    }
  }

  public static var spinnerBackground: UIColor {
    get {
      if #available(iOS 11.0, *) {
        return UIColor(named: "SpinnerBackground")!
      } else {
        return UIColor(white: 0.5,
                       alpha: 0.8)
      }
    }
  }

  public static var labelNormal: UIColor {
    get {
      if #available(iOS 11.0, *) {
        return UIColor(named: "LabelNormal")!
      } else {
        return UIColor.lightGray
      }
    }
  }

  public static var labelHighlighted: UIColor {
    get {
      if #available(iOS 11.0, *) {
        return UIColor(named: "LabelHighlighted")!
      } else {
        return UIColor.darkGray
      }
    }
  }

  public static var listSeparator: UIColor {
    get {
      if #available(iOS 11.0, *) {
        return UIColor(named: "ListSeparator")!
      } else {
        return UIColor.lightGray
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
}
