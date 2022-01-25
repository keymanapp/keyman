/*
 * KeyboardsScreen.swift
 * FirstVoices app
 * 
 * License: MIT
 *
 * Copyright © 2022 FirstVoices.
 *
 * Created by Shawn Schantz on 2022-01-13.
 * 
 * Description...
 */

import UIKit

let keymanHelpSite: String = "https://help.keyman.com/keyboard/"

class KeyboardsScreen: UIViewController {
 
  @IBOutlet weak var tableView: UITableView!

  var selectedKeyboardIndex: IndexPath = IndexPath(row: 0, section: 0)
    
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

    tableView.delegate = self
    tableView.dataSource = self
  }
  
  // In a storyboard-based application, you will often want to do a little preparation before navigation
  override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
    if segue.identifier == "keyboardDetails" {
      let detailsController = segue.destination as! KeyboardDetailController
      let indexPath = self.tableView.indexPathForSelectedRow
      let keyboards = (self.keyboardList[indexPath!.section]).keyboards
      let keyboard = keyboards[indexPath!.row]
      detailsController.keyboard = keyboard
    }
  }

  override func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)
    self.navigationController!.isNavigationBarHidden = false
  }
  
  // TODO: stop using deprecated openURL method when we upgrade to iOS 10 or later
  func tableView(_ tableView: UITableView, accessoryButtonTappedForRowWith indexPath: IndexPath) {
    let keyboards: FVKeyboardList = (self.keyboardList[indexPath.section]).keyboards
    let keyboard: FVKeyboard = keyboards[indexPath.row]
    let helpUrl: URL = URL.init(string: "\(keymanHelpSite)\(keyboard.id)")!
    UIApplication.shared.openURL(helpUrl)
  }
}

extension KeyboardsScreen: UITableViewDataSource, UITableViewDelegate {

/*  func tableView(_ tableView: UITableView, willDisplayHeaderView view: UIView, forSection section: Int) {
    let headerView: UITableViewHeaderFooterView  = view as! UITableViewHeaderFooterView
    headerView.textLabel?.textColor = UIColor.darkGray
  }
*/
  func numberOfSections(in tableView: UITableView) -> Int {
    return self.keyboardList.count
  }

  func tableView(_ tableView: UITableView,
                 titleForHeaderInSection section: Int) -> String? {
    return self.keyboardList[section].name
  }

  /*
  func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    super
    self.selectedKeyboardIndex = indexPath
    //tableView.deselectRow(at: <#T##IndexPath#>, animated: <#T##Bool#>)
    //tableView.deselectRow(at: indexPath, animated: true)
    //let keyboards = (self.keyboardList[indexPath.section]).keyboards
    
    /*
    guard let cell = tableView.cellForRow(at: indexPath) else { return }
    cell.accessoryType = .checkmark
     */
    self.performSegue(withIdentifier: "keyboardDetails", sender: indexPath)
 }
 */
  
  func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    return self.keyboardList[section].keyboards.count
  }
  
  // TODO: return empty cell if it cannot be dequeued
  func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    let keyboards = (self.keyboardList[indexPath.section]).keyboards
    let keyboard = keyboards[indexPath.row]
    //let cell = tableView.dequeueReusableCell(withIdentifier: "KeyboardCell") as! KeyboardCell
    let cell = tableView.dequeueReusableCell(withIdentifier: "KeyboardCell")
      cell!.textLabel!.text = keyboard.name

    if(_loadedKeyboards.contains(keyboard.id)) {
      cell!.imageView?.isHidden = false
      print("loaded keyboard = \(keyboard.name), id = \(keyboard.id), legacyId = \(keyboard.legacyId)")
    } else {
    cell!.imageView?.isHidden = true
    }
        
    return cell!
  }
  
}
