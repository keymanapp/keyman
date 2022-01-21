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
 * Description...
 */

import UIKit

class KeyboardDetailController: UITableViewController {

  var keyboard: FVKeyboard? = nil

  override func viewDidLoad() {
    super.viewDidLoad()

    if let name = keyboard?.name {
      self.navigationItem.title = name
    }

    print("Loaded KeyboardDetailController")
  }
  
/*  override func tableView(_ tableView: UITableView, willDisplayHeaderView view: UIView, forSection section: Int) {
    let headerView: UITableViewHeaderFooterView  = view as! UITableViewHeaderFooterView
    headerView.textLabel?.textColor = UIColor.darkGray
  }
*/
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
      header = "Dictionaries"
    case 3:
      header = "Package Information"
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
      numberOfRows = 1
    case 3:
      numberOfRows = 1
    default:
      numberOfRows = 0
    }
    return numberOfRows
  }

  // TODO: return empty cell if it cannot be dequeued
  override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    if indexPath.section == 3 {
      let versionCell = tableView.dequeueReusableCell(withIdentifier: "attributeCell")
      versionCell!.textLabel!.text = "Version"
      versionCell!.detailTextLabel!.text = "1.0.0"
      return versionCell!
    }
    else {
      let switchCell = tableView.dequeueReusableCell(withIdentifier: "switchCell") as! KeyboardDetailCell
      switch indexPath.section {
      case 0:
        let labelName = "Enable \(keyboard!.name) Keyboard"
        switchCell.configure(label: labelName, enabled: false)
      case 1:
        if indexPath.row == 0 {
          switchCell.configure(label: "Enable Predictions", enabled: false)
        } else if indexPath.row == 1 {
          switchCell.configure(label: "Enable Corrections", enabled: false)
        }
      case 2:
        // TODO: varies from zero to n
        switchCell.configure(label: "Saanich", enabled: true)
      default:
        switchCell.configure(label: "Enable Keyboard", enabled: false)
      }
      
      return switchCell
    }
  }

}
