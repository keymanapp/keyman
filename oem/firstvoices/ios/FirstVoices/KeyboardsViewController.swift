//
//  KeyboardsViewController.swift
//  FirstVoices app
//
//  License: MIT
//
//  Copyright © 2019 FirstVoices.
//
//  Created by Serkan Kurt on 17/11/2015.
//  Converted and rewritten by Marc Durdin on 15/05/2019.
//

import UIKit
import KeymanEngine

let helpLink: String = "https://help.keyman.com/keyboard/"
let tHelpButtonTag: NSInteger = 1000
let tCheckBoxTag: NSInteger = 2000

class KeyboardsViewController: UIViewController, UITableViewDelegate, UITableViewDataSource {

  @IBOutlet var tableView: UITableView!

  private var _keyboardList: FVRegionList!

  private var keyboardList: FVRegionList {
    get {
      if _keyboardList == nil {
        _keyboardList = FVRegionStorage.load()
      }
      return _keyboardList!
    }
  }

  private var _loadedKeyboards: [String] = []


  override func viewDidLoad() {
    _loadedKeyboards = FVRegionStorage.loadKeyboardListFromUserDefaults()
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
    return self.keyboardList.count
  }

  func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    let kbArray = self.keyboardList[section].keyboards
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
    return self.keyboardList[section].name
  }

  // MARK: - Table view delegate
  func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
    cell.selectionStyle = .none
    cell.accessoryType = .none
    let keyboards = (self.keyboardList[indexPath.section]).keyboards
    let kb = keyboards[indexPath.row]
    cell.textLabel!.text = kb.name
    let checkbox: CheckBox = (cell.accessoryView!.viewWithTag(tCheckBoxTag) as! CheckBox)
    checkbox.isChecked = _loadedKeyboards.contains(kb.id)
  }

  func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    //
  }

  @objc func helpButtonTapped(_ sender: Any, forEvent event: UIEvent) {
    let touches: Set<UITouch>? = event.allTouches
    let touch: UITouch = touches!.first!
    let currentTouchPosition: CGPoint = touch.location(in: self.tableView)
    let indexPath: IndexPath? = self.tableView!.indexPathForRow(at: currentTouchPosition)
    if indexPath != nil {
      let kbArray: FVKeyboardList = self.keyboardList[indexPath!.section].keyboards
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
      let keyboards = self.keyboardList[indexPath!.section].keyboards
      let kb = keyboards[indexPath!.row]

      _loadedKeyboards = _loadedKeyboards.filter { $0 != kb.id }
      if (sender as! CheckBox).isChecked {
        _loadedKeyboards += [kb.id]
      }
      FVRegionStorage.saveKeyboardListToUserDefaults(loadedKeyboards: self._loadedKeyboards)
      FVRegionStorage.updateActiveKeyboardsList(keyboardList: self.keyboardList,
                                                loadedKeyboards: self._loadedKeyboards)
    }
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

}

