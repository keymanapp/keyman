//
//  KeyboardsViewController.swift
//  FirstVoices
//
//  Created by Serkan Kurt on 17/11/2015.
//  Converted by Marc Durdin on 15/5/19.
//  Copyright © 2015-2019 FirstVoices. All rights reserved.
//

import UIKit
import KeymanEngine

let helpLink: String = "https://help.keyman.com/keyboard/"
let kKeyboardsFileLastModDateKey: String = "KeyboardsFileLastModDate"
let tHelpButtonTag: NSInteger = 1000
let tCheckBoxTag: NSInteger = 2000

typealias KeyboardDict = [String:String]
typealias KeyboardList = [KeyboardDict]
struct Region {
  var code: String
  var keyboards: KeyboardList
}
typealias RegionList = [Region]

class KeyboardsViewController: UIViewController, UITableViewDelegate {

  @IBOutlet weak var tableView: UITableView?
  // TODO: Turn this into an array of structured objects rather than an array of array of dictionaries
  // current structure: region: array of 'keyboard info'
  var _keyboardList: RegionList?

  override func viewDidLoad() {
    super.viewDidLoad()
    // Do any additional setup after loading the view, typically from a nib.
  }

  override func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)
    self.navigationController!.isNavigationBarHidden = false
  }

  override func didReceiveMemoryWarning() {
    super.didReceiveMemoryWarning()
    // Dispose of any resources that can be recreated.
  }

  // MARK: - Table view data source
  func numberOfSectionsInTableView(_ tableView: UITableView) -> Int {
    return self.keyboardList().count
  }

  func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    //let index: Int = (section*2)+1
    let kbArray = self.keyboardList()[section].keyboards
    return kbArray.count
  }

  func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    let cellIdentifier: String = "Cell"
    var cell: UITableViewCell? = tableView.dequeueReusableCell(withIdentifier: cellIdentifier)
    if cell != nil {
      cell = UITableViewCell(style: UITableViewCell.CellStyle.subtitle, reuseIdentifier: cellIdentifier)
      let selectionColor: UIView = UIView()
      selectionColor.backgroundColor = UIColor(red: 204.0/255.0, green: 136.0/255.0, blue: 34.0/255.0, alpha: 1.0)
      cell!.selectedBackgroundView = selectionColor
      cell!.textLabel!.font = UIFont.systemFont(ofSize: 14.0)
    }
    if cell!.accessoryView == nil {
      let cellH: CGFloat = cell!.frame.height
      let margin: CGFloat = 0.0
      let helpButton: UIButton = UIButton(type: .custom)
      helpButton.tag = tHelpButtonTag
      helpButton.addTarget(self, action: "helpButtonTapped:forEvent:", for: UIControl.Event.touchUpInside)
      helpButton.setImage(UIImage(named: "739-question"), for: UIControl.State.normal)
      helpButton.setImage(UIImage(named: "739-question-selected"), for: UIControl.State.highlighted)
      let w1: CGFloat = cellH
      let h1: CGFloat = cellH
      helpButton.frame = CGRect(x:margin, y:0, width:w1, height:h1)
      let w2: CGFloat = cellH-10
      let h2: CGFloat = cellH
      // TODO: consider using a table cell accessory checkbox
      let checkBox = CheckBox(frame: CGRect(x:margin+w1, y:0, width:w2, height:h2))
      checkBox.tag = tCheckBoxTag
      checkBox.addTarget(self, action: "checkBoxTapped:forEvent:", for: UIControl.Event.valueChanged)
      checkBox.isOpaque = false
      checkBox.tintColor = UIColor.lightGray
      cell!.accessoryView = UIView(frame: CGRect(x:0, y:0, width:margin+w1+w2+margin, height:cellH))
      cell!.accessoryView!.addSubview(helpButton)
      cell!.accessoryView!.addSubview(checkBox)
    }
    return cell!
  }

  func tableView(_ tableView: UITableView,
                 titleForHeaderInSection section: Int) -> String? {
    return self.keyboardList()[section].code
  }

  // MARK: - Table view delegate
  func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
    cell.selectionStyle = .none
    cell.accessoryType = .none
    let kbArray: KeyboardList = self.keyboardList()[indexPath.section].keyboards
    let kbDict: KeyboardDict = kbArray[indexPath.row]
    cell.textLabel!.text = kbDict[kFVKeyboardNameKey]
    let checkState: String = kbDict[kFVKeyboardCheckStateKey]!
    let checkbox: CheckBox = (cell.accessoryView!.viewWithTag(tCheckBoxTag) as! CheckBox)
    checkbox.isChecked = checkState.isEqual("YES")
  }

  func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    //
  }

  func keyboardList() -> RegionList {
    if _keyboardList == nil {
      let sharedData: UserDefaults = FVShared.userDefaults()
      _keyboardList = sharedData.array(forKey: kFVKeyboardList) as? RegionList
      var oldKeyboardList: RegionList? = nil
      if _keyboardList == nil || self.shouldLoadFromKeyboardsFile() {
        do {
          if _keyboardList != nil {
            oldKeyboardList = _keyboardList
            _keyboardList = nil
          }
          let keyboardsFile: String = Bundle.main.path(forResource: "keyboards", ofType: "csv")!

          let fileContents: String = try String(contentsOfFile: keyboardsFile, encoding: .utf8).replacingOccurrences(of: "\r", with: "")
          let lines: [String] = fileContents.components(separatedBy: "\n")
          for i in 1..<lines.count {
            let line: String = lines[i]
            if line.count == 0 {
              continue
            }
            let values: [String] = line.components(separatedBy: ",")
            if values.count > 0 {
              if _keyboardList == nil {
                _keyboardList = RegionList.init()
              }
              let kbName: String = values[0]
              let regionName: String = values[3]
              let langCode: String = values[9]
              let kbFilename: String = values[7].replacingOccurrences(of: ".kmn", with: "-9.0.js") // TODO: this rename is no longer correct
              var region = _keyboardList!.first(where: { $0.code == regionName })
              if region == nil {
                region = Region.init(code: regionName, keyboards: [])
                _keyboardList!.append(region!)
              }

              let kbDict: KeyboardDict = [
                kFVKeyboardNameKey: kbName,
                kFVKeyboardLanguageCodeKey: langCode,
                kFVKeyboardFilenameKey: kbFilename,
                kFVKeyboardCheckStateKey: "NO"
              ]

              region!.keyboards.append(kbDict)
            }
          }
          let sharedData: UserDefaults = FVShared.userDefaults()
          sharedData.set(_keyboardList, forKey: kFVKeyboardList)
          sharedData.set(NSArray(object: self.keyboardsFileModDate()), forKey: kKeyboardsFileLastModDateKey)
          sharedData.synchronize()
        } catch {
          NSLog("Unexpected error: \(error).")
        }
      }
      if oldKeyboardList != nil {
        self.restoreActiveKeyboardsFromArray(oldKeyboardList!)
      }
    }
    return _keyboardList!
  }

  func shouldLoadFromKeyboardsFile() -> Bool {
    var sharedData: UserDefaults = FVShared.userDefaults()
    var lastModDate: NSDate? = (sharedData.object(forKey: kKeyboardsFileLastModDateKey) as? Array<NSDate>)![0]
    if lastModDate == nil {
      return true
    }
    var fileModDate: NSDate? = self.keyboardsFileModDate()
    if fileModDate == nil {
      return true
    }
    if fileModDate!.timeIntervalSince(lastModDate! as Date) > 0 {
      return true
    }
    return false
  }

  func keyboardsFileModDate() -> NSDate? {
    var filePath: String = Bundle.main.path(forResource: "keyboards", ofType: "csv")!
    do {
      var attrs: [FileAttributeKey : Any] = try FileManager.default.attributesOfItem(atPath: filePath)
      return attrs[FileAttributeKey.modificationDate] as? NSDate // NSFileModi] .object(forKey: NSFileModificationDate)
    } catch {
      print(error)
    }
    return nil
  }

  func helpButtonTapped(_ sender: Any, forEvent event: UIEvent) {
    let touches: Set<UITouch>? = event.allTouches
    let touch: UITouch = touches!.first!
    let currentTouchPosition: CGPoint = touch.location(in: self.tableView)
    let indexPath: IndexPath? = self.tableView!.indexPathForRow(at: currentTouchPosition)
    if indexPath != nil {
      let kbArray: KeyboardList = self.keyboardList()[indexPath!.section].keyboards
      let kbDict: KeyboardDict = kbArray[indexPath!.row]
      let kbFilename: String = kbDict[kFVKeyboardFilenameKey]!
      let kbID: String = kbFilename.substring(to: kbFilename.range(of: "-")!.lowerBound)
      let helpUrl: URL = URL.init(string: "\(helpLink)\(kbID)")!
      UIApplication.shared.openURL(helpUrl)
    }
  }

  func checkBoxTapped(_ sender: Any, forEvent event: UIEvent) {
    let touches: Set<UITouch>? = event.allTouches
    let touch: UITouch = touches!.first!
    let currentTouchPosition: CGPoint = touch.location(in: self.tableView)
    let indexPath: IndexPath? = self.tableView!.indexPathForRow(at: currentTouchPosition)
    if indexPath != nil {
      // Update the checkState with the new checked state
      var kbArray: KeyboardList = self.keyboardList()[indexPath!.section].keyboards
      var kbDict: KeyboardDict = kbArray[indexPath!.row]
      kbDict[kFVKeyboardCheckStateKey] = (sender as! CheckBox).isChecked ? "YES" : "NO"
      kbArray[indexPath!.row] = kbDict
      _keyboardList![indexPath!.section].keyboards = kbArray

      let sharedData: UserDefaults = FVShared.userDefaults()
      sharedData.set(_keyboardList, forKey: kFVKeyboardList)
      sharedData.synchronize()
      self.updateActiveKeyboardsList()
    }
  }

  func updateActiveKeyboardsList() {
    //Manager.shared.currentKeyboard
    //Manager.shared.removeKeyboard(0)
    //var sharedData: UserDefaults = FVShared.userDefaults()
    //sharedData.remove(forKey: kKeymanUserKeyboardsListKey)
    var kbList: RegionList = self.keyboardList()
    for i in 0...kbList.count-1 {
      var kbArray: KeyboardList? = kbList[i].keyboards
      for kbDict in kbArray! {
        if kbDict[kFVKeyboardCheckStateKey]!.isEqual("YES") {
          var kbFilename: String = kbDict[kFVKeyboardFilenameKey]!
          var kbID: String = kbFilename.substring(to: kbFilename.range(of: "-")!.lowerBound)
          var langCode: String = kbDict[kFVKeyboardLanguageCodeKey]!
          var langID: String = langCode.substring(to: langCode.index(langCode.startIndex, offsetBy: langCode.count < 3 ? langCode.count : 3)).lowercased()
          var kbName: String = kbDict[kFVKeyboardNameKey]!
          var langName: String = kbName
          var keyboard: InstallableKeyboard = InstallableKeyboard.init(id: kbID,
                                                                       name: kbName,
                                                                       languageID: langID,
                                                                       languageName: langName,
                                                                       version: "1.0", //TODO fix versions
                                                                       isRTL: false,
                                                                       font: nil,
                                                                       oskFont: nil,
                                                                       isCustom: true)
          Manager.shared.addKeyboard(keyboard)
        }
      }
    }
/*
    if !sharedData.object(forKey: kKeymanUserKeyboardsListKey) {
      var keyboard: InstallableKeyboard = InstallableKeyboard.init(id: kKeymanDefaultKeyboardID,
                                                                   name: kKeymanDefaultKeyboardName,
                                                                   languageID: kKeymanDefaultLanguageID,
                                                                   languageName: kKeymanDefaultLanguageName,
                                                                   version: "1.0", //TODO fix versions
                                                                   isRTL: kKeymanDefaultKeyboardRTL.isEqual(to: "Y") ? true : false,
                                                                   font: kKeymanDefaultKeyboardFont,
                                                                   oskFont: nil,
                                                                   isCustom: false)
      Manager.shared.addKeyboard(keyboard)
    }
 */

  }

  func restoreActiveKeyboardsFromArray(_ oldKeyboardList: RegionList) {
    var oldActiveKeyboards: [String] = []
    for region in oldKeyboardList {
      for kbDict in region.keyboards {
        if kbDict[kFVKeyboardCheckStateKey]!.isEqual("YES") {
          let kbFilename: String = kbDict[kFVKeyboardFilenameKey]!
          let kbID: String = kbFilename.substring(to: kbFilename.range(of: "-")!.lowerBound)
          oldActiveKeyboards.append(kbID)
        }
      }
    }
    for region in self.keyboardList() {
      for var kbDict in region.keyboards {
        let kbFilename: String = kbDict[kFVKeyboardFilenameKey]!
        let kbID: String = kbFilename.substring(to: kbFilename.range(of: "-")!.lowerBound)
        if oldActiveKeyboards.contains(kbID) {
          kbDict[kFVKeyboardCheckStateKey] = "YES"
        }
      }
    }
    self.updateActiveKeyboardsList()
  }

}

