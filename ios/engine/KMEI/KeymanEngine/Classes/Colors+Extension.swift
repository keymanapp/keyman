//
//  Colors+Extensions.swift
//  Keyman
//
//  Created by Joshua Horton on 1/15/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
import UIKit

// TODO:  Relocate to the Keyman App's project.  Not possible while the Settings menus are part of KMEI.
extension Colors {
  public static var systemBackground: UIColor {
    get {
      return UIColor.systemBackground
    }
  }

  // The primary color used for selected UI elements in the settings menu.
  public static var selectionPrimary: UIColor {
    get {
      return UIColor(named: "SelectionPrimary")!
    }
  }

  // The primary color used for selected UI elements in Get Started menu.
  public static var selectionSecondary: UIColor {
    get {
      return UIColor(named: "SelectionSecondary")!
    }
  }

  public static var statusToolbar: UIColor {
    get {
      return UIColor(named: "StatusToolbar")!
    }
  }

  public static var statusResourceUpdateButton: UIColor {
    get {
      return UIColor(named: "StatusResourceUpdateButton")!
    }
  }

  public static var spinnerBackground: UIColor {
    get {
      return UIColor(named: "SpinnerBackground")!
    }
  }

  public static var labelNormal: UIColor {
    get {
      return UIColor(named: "LabelNormal")!
    }
  }

  public static var labelHighlighted: UIColor {
    get {
      return UIColor(named: "LabelHighlighted")!
    }
  }

  public static var listSeparator: UIColor {
    get {
      return UIColor(named: "ListSeparator")!
    }
  }
}
