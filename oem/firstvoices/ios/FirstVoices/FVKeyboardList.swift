//
//  FVKeyboardList.swift
//  FirstVoices app
//
//  License: MIT
//
//  Copyright © 2019 FirstVoices.
//
//  Created by Marc Durdin on 21/05/2019.
//

import Foundation
import KeymanEngine

/// A single keyboard in the list of available keyboards
class FVKeyboard {
  let id, name, legacyId: String
  init(id: String, name: String, legacyId: String) {
    self.id = id
    self.name = name
    self.legacyId = legacyId
  }
}

typealias FVKeyboardList = [FVKeyboard]

/// A region with a list of keyboards
class FVRegion {
  let name: String
  var keyboards: FVKeyboardList = []
  init(name: String) {
    self.name = name
  }
}
typealias FVRegionList = [FVRegion]

class FVRegionStorage {

  /// Loads the available keyboards from keyboards.csv to present
  /// in the user interface. This keyboards.csv file is maintained
  /// manually but the keyboards listed within are imported into the
  /// app during build (into the Keyboards/files directory) from the
  /// keymanapp/keyboards repository. This means that a rebuild of the
  /// app always gets the latest version of the keyboards. Adding a new
  /// keyboard to the app is as simple as updating keyboards.csv and
  /// rebuilding.
  ///
  /// The keyboards.csv file has 5 columns:
  ///   - shortname  - the keyboards repository shortname (e.g. fv)
  ///   - id         - the identifier of the keyboard
  ///   - name       - the name of the keyboard to be shown in UI in app
  ///                  (sometimes may differ from the compiled keyboard)
  ///   - regionName - the name of the region to group the keyboard under
  ///   - legacyId   - the legacy filename of the keyboard, used only
  ///                  during upgrades
  /// The file is sorted according to how the keyboards should be
  /// displayed in the Keyboard Selection UI.
  class func load() -> FVRegionList {
    var regionList = FVRegionList()
    do {
      let keyboardsFile: String = Bundle.main.path(forResource: FVConstants.keyboardsCSVName,
                                                   ofType: FVConstants.keyboardsCSVType,
                                                   inDirectory: FVConstants.keyboardsCSVPath)!

      let fileContents: String = try String(contentsOfFile: keyboardsFile, encoding: .utf8).replacingOccurrences(of: "\r", with: "")
      let lines: [String] = fileContents.components(separatedBy: "\n")
      for i in 1..<lines.count { // skip first line; it is a header row
        let line: String = lines[i]
        if line.count == 0 {
          continue
        }
        let values: [String] = line.components(separatedBy: ",")
        if values.count > 0 {
          // Columns: shortname,id,name,region,legacyid
          let kbId: String = values[1]
          let kbName: String = values[2]
          let regionName: String = values[3]
          let legacyId: String = values[4]

          var region = regionList.first(where: { $0.name == regionName })
          if region == nil {
            region = FVRegion(name: regionName)
            regionList.append(region!)
          }

          let kb = FVKeyboard(id: kbId, name: kbName, legacyId: legacyId)
          region!.keyboards.append(kb)
        }
      }
    } catch {
      print("Unexpected error loading keyboards.csv: \(error).")
    }
    return regionList
  }

  /// Load shared user data from earlier versions of the app and
  /// convert into the desired format, then unload the old keyboards
  /// and load the new ones corresponding. If a keyboard is not available
  /// in the new version, it will be uninstalled.
  class func upgrade() {

    let sharedData: UserDefaults = FVShared.userDefaults()
    let list = sharedData.array(forKey: FVConstants.legacy_kFVKeyboardList)
    if(list == nil) {
      // No legacy data to upgrade
      return
    }

    //
    // The legacy format is a bit messy. It mixes static data and configuration.
    // Fortunately, we only need the checkstatekey value (and corresponding
    // keyboard name) in order to transfer state to the new app.
    // array(
    //    0: region_name
    //    1: array(
    //      dictionary(
    //        checkstatekey
    //        filenamekey
    //        ...
    //      )
    //    )
    //    ...
    // )
    //

    let regions = self.load()
    var loadedKeyboards: [String] = []

    var i = 1
    while i < list!.count {
      let keyboards = list![i] as? [[String:String]]
      if let keyboards = keyboards {
        for keyboard in keyboards {

          if keyboard[FVConstants.legacy_kFVKeyboardCheckStateKey] == "YES" {

            let legacyId = keyboard[FVConstants.legacy_kFVKeyboardFilenameKey]
            print("Legacy keyboard \(legacyId!) is installed")

            for region in regions {
              for rk in region.keyboards {
                if rk.legacyId == legacyId {
                  print("Found corresponding keyboard \(rk.id)")
                  loadedKeyboards.append(rk.id)
                }
              }
            }
          }
        }
      } else {
        // Some intermediate debug builds had different formats
        // so we just abandon data from those
        print("Unable to load corrupted data")
        break
      }
      i += 2
    }

    // Save the new configuration (which could be empty), and update Keyman Engine
    self.saveKeyboardListToUserDefaults(loadedKeyboards: loadedKeyboards)
    self.updateActiveKeyboardsList(keyboardList: regions, loadedKeyboards: loadedKeyboards)

    // Finally, remove the legacy data
    sharedData.removeObject(forKey: FVConstants.legacy_kFVKeyboardList)
    sharedData.removeObject(forKey: FVConstants.legacy_kKeyboardsFileLastModDateKey)
  }

  /// Loads the list of keyboards that the user has selected; in theory we could
  /// collect these from Keyman Engine but at present there is no API to iterate
  /// over the installed keyboards.
  class func loadKeyboardListFromUserDefaults() -> [String] {
    let sharedData: UserDefaults = FVShared.userDefaults()
    let keyboards = sharedData.array(forKey: FVConstants.kFVLoadedKeyboardList)
    if keyboards != nil {
      return keyboards as! [String]
    } else {
      return []
    }
  }

  /// Saves the list of keyboards to shared data
  class func saveKeyboardListToUserDefaults(loadedKeyboards: [String]) {
    let sharedData: UserDefaults = FVShared.userDefaults()
    sharedData.set(loadedKeyboards, forKey: FVConstants.kFVLoadedKeyboardList)
    sharedData.synchronize()
  }

  /// Updates Keyman Engine with the list of installed keyboards. Removes all
  /// the keyboards and reinstalls them to maintain a clean configuration.
  class func updateActiveKeyboardsList(keyboardList: FVRegionList, loadedKeyboards: [String]) {

    // Remove all installed keyboards first -- we'll re-add them below

    while Manager.shared.removeKeyboard(at: 0) {
    }

    // Iterate through the available keyboards

    for region in keyboardList {
      let keyboards = region.keyboards
      for kb in keyboards {
        if loadedKeyboards.contains(kb.id) {

          // Load the .keyboard_info file and find its first language code

          var keyboardInfo: KeyboardInfo
          do {
            let kbinfoFilename: String = Bundle.main.path(forResource: kb.id, ofType: FVConstants.keyboardInfoType, inDirectory: FVConstants.keyboardsPath)!
            keyboardInfo = try KeyboardInfoParser.decode(file: kbinfoFilename)
          } catch {
            print("Failed to load keyboard info for " + kb.id+": " + error.localizedDescription)
            continue
          }

          // Preload the keyboard

          do {
            let kbPath: String = Bundle.main.path(forResource: kb.id,
                                                  ofType: FVConstants.keyboardType,
                                                  inDirectory: FVConstants.keyboardsPath)!
            let pathUrl = URL(fileURLWithPath: kbPath)
            try Manager.shared.preloadFiles(forKeyboardID: kb.id, at: [pathUrl], shouldOverwrite: true)
          } catch {
            print("Failed to load preload "+kb.id+": " + error.localizedDescription)
            continue
          }

          // Install the keyboard into Keyman Engine

          let keyboard: InstallableKeyboard = InstallableKeyboard.init(id: kb.id,
                                                                       name: kb.name,
                                                                       languageID: keyboardInfo.languages.keys[keyboardInfo.languages.startIndex],
                                                                       languageName: kb.name,
                                                                       version: keyboardInfo.version,
                                                                       isRTL: false,
                                                                       font: nil,
                                                                       oskFont: nil,
                                                                       isCustom: true)
          Manager.shared.addKeyboard(keyboard)
        }
      }
    }
  }

}
