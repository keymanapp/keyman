//
//  KeyboardsViewController.swift
//  FirstVoices
//
//  Created by Serkan Kurt on 17/11/2015.
//  Rewritten by Marc Durdin on 15/5/2019.
//  Copyright © 2015-2019 FirstVoices. All rights reserved.
//

import UIKit
import KeymanEngine

let helpLink: String = "https://help.keyman.com/keyboard/"
let tHelpButtonTag: NSInteger = 1000
let tCheckBoxTag: NSInteger = 2000

class FVKeyboard {
  let id, name: String
  init(id: String, name: String) {
    self.id = id
    self.name = name
  }
}
typealias FVKeyboardList = [FVKeyboard]
class FVRegion {
  var name: String = ""
  var keyboards: FVKeyboardList = []
}
typealias FVRegionList = [FVRegion]

class KeyboardsViewController: UIViewController, UITableViewDelegate, UITableViewDataSource {

  @IBOutlet var tableView: UITableView!
  var _keyboardList: FVRegionList = []
  var _loadedKeyboards: [String] = []

  override func viewDidLoad() {
    loadKeyboardListFromUserDefaults()
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
  func numberOfSections(in tableView: UITableView) -> Int {
    return self.keyboardList().count
  }

  func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    let kbArray = self.keyboardList()[section].keyboards
    return kbArray.count
  }

  func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    let cellIdentifier: String = "Cell"
    var cell: UITableViewCell? = tableView.dequeueReusableCell(withIdentifier: cellIdentifier)
    if cell == nil {
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
      helpButton.addTarget(self, action: #selector(helpButtonTapped(_:forEvent:)), for: UIControl.Event.touchUpInside)
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
      checkBox.addTarget(self, action: #selector(checkBoxTapped(_:forEvent:)), for: UIControl.Event.valueChanged)
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
    return self.keyboardList()[section].name
  }

  // MARK: - Table view delegate
  func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
    cell.selectionStyle = .none
    cell.accessoryType = .none
    let keyboards = (self.keyboardList()[indexPath.section]).keyboards
    let kb = keyboards[indexPath.row]
    cell.textLabel!.text = kb.name
    let checkbox: CheckBox = (cell.accessoryView!.viewWithTag(tCheckBoxTag) as! CheckBox)
    checkbox.isChecked = _loadedKeyboards.contains(kb.id)
  }

  func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    //
  }

  func keyboardList() -> FVRegionList {
    if _keyboardList.count == 0 {
      do {
        #warning("TODO: refactor the .csv load + related struct/class types into a separate module")
        let keyboardsFile: String = Bundle.main.path(forResource: "keyboards", ofType: "csv", inDirectory: "Keyboards")!

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
            // TODO: let legacyId: String = values[3]
            #warning("TODO: deal with legacy ids")
            var region = _keyboardList.first(where: { $0.name == regionName })
            if region == nil {
              region = FVRegion.init()
              region!.name = regionName
              _keyboardList.append(region!)
            }

            let kb = FVKeyboard(id: kbId, name: kbName)
            region!.keyboards.append(kb)
          }
        }
      } catch {
        NSLog("Unexpected error: \(error).")
      }
    }
    return _keyboardList
  }

  @objc func helpButtonTapped(_ sender: Any, forEvent event: UIEvent) {
    let touches: Set<UITouch>? = event.allTouches
    let touch: UITouch = touches!.first!
    let currentTouchPosition: CGPoint = touch.location(in: self.tableView)
    let indexPath: IndexPath? = self.tableView!.indexPathForRow(at: currentTouchPosition)
    if indexPath != nil {
      let kbArray: FVKeyboardList = self.keyboardList()[indexPath!.section].keyboards
      let kbDict: FVKeyboard = kbArray[indexPath!.row]
      let helpUrl: URL = URL.init(string: "\(helpLink)\(kbDict.id)")!
      UIApplication.shared.openURL(helpUrl)
    }
  }

  @objc func checkBoxTapped(_ sender: Any, forEvent event: UIEvent) {
    let touches: Set<UITouch>? = event.allTouches
    let touch: UITouch = touches!.first!
    let currentTouchPosition: CGPoint = touch.location(in: self.tableView)
    let indexPath: IndexPath? = self.tableView!.indexPathForRow(at: currentTouchPosition)
    if indexPath != nil {
      // Update the checkState with the new checked state
      let keyboards = self.keyboardList()[indexPath!.section].keyboards
      let kb = keyboards[indexPath!.row]

      _loadedKeyboards = _loadedKeyboards.filter { $0 != kb.id }
      if (sender as! CheckBox).isChecked {
        _loadedKeyboards += [kb.id]
      }
      self.saveKeyboardListToUserDefaults()
      self.updateActiveKeyboardsList()
    }
  }

  func loadKeyboardListFromUserDefaults() {
    let sharedData: UserDefaults = FVShared.userDefaults()
    let keyboards = sharedData.array(forKey: kFVLoadedKeyboardList)
    if keyboards != nil {
      _loadedKeyboards = keyboards as! [String]
    } else {
      _loadedKeyboards = []
    }
  }

  func saveKeyboardListToUserDefaults() {
    let sharedData: UserDefaults = FVShared.userDefaults()
    sharedData.set(_loadedKeyboards, forKey: kFVLoadedKeyboardList)
    sharedData.synchronize()
  }

  func reportFatalError(message: String) {
    let alertController = UIAlertController(title: title, message: message,
      preferredStyle: UIAlertController.Style.alert)
    alertController.addAction(UIAlertAction(title: "OK",
                                            style: UIAlertAction.Style.cancel,
                                            handler: nil))

    self.present(alertController, animated: true, completion: nil)
    print(message)
    // TODO: send the error message + call stack through to us
    // some reporting mechanism
  }

  func updateActiveKeyboardsList() {

    // Remove all installed keyboards first -- we'll re-add them below

    while Manager.shared.removeKeyboard(at: 0) {
    }

    // Iterate through the available keyboards

    let kbList = self.keyboardList()
    for i in 0...kbList.count-1 {
      let keyboards = kbList[i].keyboards
      for kb in keyboards {
        if _loadedKeyboards.contains(kb.id) {

          // Load the .keyboard_info file and find its first language code

          var keyboardInfo: KeyboardInfo
          do {
            let kbinfoFilename: String = Bundle.main.path(forResource: kb.id, ofType: "keyboard_info", inDirectory: "Keyboards/files")!
            keyboardInfo = try KeyboardInfoParser.decode(file: kbinfoFilename)
          } catch {
            reportFatalError(message: "Failed to load keyboard info for " + kb.id+": " + error.localizedDescription)
            continue
          }

          // Preload the keyboard

          do {
            let kbPath: String = Bundle.main.path(forResource: kb.id, ofType: "js", inDirectory: "Keyboards/files")!
            let pathUrl = URL(fileURLWithPath: kbPath)
            try Manager.shared.preloadFiles(forKeyboardID: kb.id, at: [pathUrl], shouldOverwrite: true)
          } catch {
            reportFatalError(message: "Failed to load preload "+kb.id+": " + error.localizedDescription)
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

