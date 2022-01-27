/*
 * KeyboardDetailController.swift
 * FirstVoices app
 *
 * License: MIT
 *
 * Copyright © 2022 FirstVoices.
 *
 * Created by Shawn Schantz on 2022-01-14.
 *
 * Class responsible for the display and editing of settings for a single keyboard.
 *
 */

import UIKit

class KeyboardDetailController: UITableViewController {

  var keyboardState: FVKeyboardState? = nil
  var delegate: RefreshKeyboardCheckmark? = nil
  var lexicalModels: [FVLexicalModel] = []
    
  override func viewDidLoad() {
    super.viewDidLoad()

    if let name = self.keyboardState?.name {
      self.navigationItem.title = name
      print("Loaded details for keyboard \(name)")
    }
    
    if let languageTag = self.keyboardState?.languageTag {
      lexicalModels = FVLexicalModels.getAvailableLexicalModels(languageTag: languageTag)
    }
  }
  
/*  override func tableView(_ tableView: UITableView, willDisplayHeaderView view: UIView, forSection section: Int) {
    let headerView: UITableViewHeaderFooterView  = view as! UITableViewHeaderFooterView
    headerView.textLabel?.textColor = UIColor.darkGray
  }
*/
  
  /*
   * Used to display the values for the keyboard that was tapped and caused the segue to the detail view.
   */
  func configure(delegate: RefreshKeyboardCheckmark, keyboard: FVKeyboardState) {
    self.delegate = delegate
    self.keyboardState = keyboard
  }
  
  func saveKeyboard() {
    
  }
  
  override func numberOfSections(in tableView: UITableView) -> Int {
    return 4
  }

  override func tableView(_ tableView: UITableView,
                 titleForHeaderInSection section: Int) -> String? {
    var header = ""
    switch section {
    case 0:
      header = "Available Keyboards"
    case 1:
      header = "Language Settings"
    case 2:
      header = "Downloadable Dictionaries"
    case 3:
      header = "Keyboard Information"
    default:
      header = "Undefined"
    }
    return header
  }
  
  override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    var numberOfRows = 0;
    switch section {
    case 0:
      numberOfRows = 1
    case 1:
      numberOfRows = 2
    case 2:
      // TODO: varies
      numberOfRows = max(1, self.lexicalModels.count)
    case 3:
      numberOfRows = 1
    default:
      numberOfRows = 0
    }
    return numberOfRows
  }

  // TODO: return empty cell if it cannot be dequeued
  override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    var tableCell: UITableViewCell? = nil
    
    if indexPath.section == 3 {
      let versionCell = tableView.dequeueReusableCell(withIdentifier: "attributeCell")
      versionCell!.textLabel!.text = "Version"
      let versionText = self.keyboardState?.version
      versionCell!.detailTextLabel!.text = versionText
      tableCell = versionCell
    }
    else {
      let switchCell = tableView.dequeueReusableCell(withIdentifier: "switchCell") as! KeyboardDetailCell
      switch indexPath.section {
      case 0:
        let actionCallBack: Callback = { (enable) in
          self.keyboardState?.isEnabled = enable
          self.delegate?.refreshCheckmark()
          return true
        }
        switchCell.configure(label: (self.keyboardState?.name)!, enabled: self.keyboardState!.isEnabled,
                             callback: actionCallBack)
        tableCell = switchCell
      case 1:
        if indexPath.row == 0 {
          let actionCallBack: Callback = { (enable) in
            self.keyboardState?.suggestPredictions = enable
            return true
          }
          switchCell.configure(label: "Suggest Predictions", enabled: self.keyboardState!.suggestPredictions,
                               callback: actionCallBack)
          tableCell = switchCell
        } else if indexPath.row == 1 {
          let actionCallBack: Callback = { (enable) in
            self.keyboardState?.suggestCorrections = enable
            return true
          }
          switchCell.configure(label: "Suggest Corrections", enabled: self.keyboardState!.suggestCorrections,
                               callback: actionCallBack)
          tableCell = switchCell
       }
      case 2:
        // TODO: varies from zero to n
        if indexPath.row == 0 {
          if self.lexicalModels.isEmpty {
            let labelCell = tableView.dequeueReusableCell(withIdentifier: "labelCell")
            labelCell?.textLabel!.text = "none"
            tableCell = labelCell
         } else {
            let actionCallBack: Callback = { (enable) in
              if enable {
                print("Dictionary turned on.")
              } else {
                print("Dictionary turned off.")
              }
                return true
            }
            let availableModelName = self.lexicalModels.first!.name
            let selectedModel = (self.keyboardState?.selectedDictionary)!
            let modelEnabled = availableModelName.elementsEqual(selectedModel)
            switchCell.configure(label: self.lexicalModels.first!.name, enabled: modelEnabled,
                                 callback: actionCallBack)
            tableCell = switchCell
          }
        }
          
      default:
        let labelCell = tableView.dequeueReusableCell(withIdentifier: "labelCell")
        labelCell?.textLabel!.text = "n/a"
        tableCell = labelCell
      }
    }
    return tableCell!
  }
}
