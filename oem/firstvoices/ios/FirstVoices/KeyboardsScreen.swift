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
 * The main screen of the app containing the list (UITableView) of FirstVoices keyboards.
 * From here, users can tap on a keyboard which will cause it to transition to a detail page for that keyboard, or
 * they can tap on the info icon for a keyboard which will load the help web page for the keyboard in the browser.
 *
 */

import UIKit

let keymanHelpSite: String = "https://help.keyman.com/keyboard/"

/*
 * define protocol so that KeyboardDetailController can send message to update state of checkmark in keyboard list
 */
protocol RefreshKeyboardCheckmark {
  func refreshCheckmark()
}

class KeyboardsScreen: UIViewController, RefreshKeyboardCheckmark {
  func refreshCheckmark() {
    self.tableView.reloadRows(at: [selectedKeyboardIndex], with: UITableView.RowAnimation.none)
  }
 
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

  override func viewDidLoad() {
    super.viewDidLoad()

    tableView.delegate = self
    tableView.dataSource = self
  }
  
  /*
   * Segue to keyboard details screen.
   * Before the segue, call the details view controller and pass the state of the keyboard and
   * a reference to self to be called back to update the row if the state changes.
   */
  override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
    if segue.identifier == "keyboardDetails" {
      let detailsController = segue.destination as! KeyboardDetailController
      let indexPath = self.tableView.indexPathForSelectedRow
      let keyboards = (self.keyboardList[indexPath!.section]).keyboards
      let keyboard = keyboards[indexPath!.row]
      
      if let keyboardState: FVKeyboardState = FVKeyboardState.loadKeyboardState(keyboardId: keyboard.id) {
        detailsController.configure(delegate: self, keyboard: keyboardState)
      } else {
        print("Could not find keyboard \(keyboard.id) in available keyboards list.")
      }
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

  func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    self.selectedKeyboardIndex = indexPath
  }
  
  func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    return self.keyboardList[section].keyboards.count
  }
  
  // TODO: return empty cell if it cannot be dequeued
  func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    let keyboards = (self.keyboardList[indexPath.section]).keyboards
    let keyboard = keyboards[indexPath.row]
    let keyboardState = FVKeyboardState.loadKeyboardState(keyboardId: keyboard.id)
    
    //let cell = tableView.dequeueReusableCell(withIdentifier: "KeyboardCell") as! KeyboardCell
    let cell = tableView.dequeueReusableCell(withIdentifier: "KeyboardCell")
    cell!.textLabel!.text = keyboardState?.name

    cell!.imageView?.isHidden = !keyboardState!.isEnabled
        
    return cell!
  }
}
