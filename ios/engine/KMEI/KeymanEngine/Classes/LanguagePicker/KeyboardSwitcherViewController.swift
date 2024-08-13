//
//  KeyboardSwitcherViewControllerTableViewController.swift
//  KeymanEngine
//
//  Created by Randy Boring on 7/26/19.
//  Copyright © 2019 SIL International. All rights reserved.
//
// This subclass is slimmed down to *just* picking (switching to) an existing keyboard.
// It replaces the original in every case but one.

import UIKit
import os.log

private let toolbarButtonTag = 100

class KeyboardSwitcherViewController: UITableViewController, UIAlertViewDelegate {
  public var userKeyboards: [InstallableKeyboard] = [InstallableKeyboard]()
  public var accessoryType: UITableViewCell.AccessoryType = .none

  override func viewDidLoad() {
    super.viewDidLoad()
    
    title = NSLocalizedString("menu-picker-title", bundle: engineBundle, comment: "")
    
    self.accessoryType = .none
    
    // remove UI that adds keyboards
    //NEEDED?
    navigationItem.rightBarButtonItem = nil
    
    os_log("didLoad: KeyboardSwitcherViewController", log: KeymanEngineLogger.ui, type: .info)
  }

  override func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)
    
    loadUserKeyboards()
    scroll(toSelectedKeyboard: false)
  }
  
  public func scroll(toSelectedKeyboard animated: Bool) {
    let index = userKeyboards.firstIndex { kb in
      return Manager.shared.currentKeyboardID == kb.fullID
    }
    
    if let index = index {
      let indexPath = IndexPath(row: index, section: 0)
      tableView.scrollToRow(at: indexPath, at: .middle, animated: animated)
      
    }
  }
  
  // MARK: - Table view data source UITableViewDataSource
  
  override func numberOfSections(in tableView: UITableView) -> Int {
    return 1
  }
  
  // MARK: - table view delegate UITableViewDelegate

  override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    return userKeyboards.count
  }
  
  override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    let cellIdentifier = "Cell"
    if let cell = tableView.dequeueReusableCell(withIdentifier: cellIdentifier) {
      return cell
    }
    
    let cell = UITableViewCell(style: .subtitle, reuseIdentifier: cellIdentifier)
    let selectionColor = UIView()
    selectionColor.backgroundColor = Colors.keyboardSelectionPrimary
    cell.selectedBackgroundView = selectionColor
    return cell
  }
  
  override func tableView(_ tableView: UITableView,
                          willDisplay cell: UITableViewCell,
                          forRowAt indexPath: IndexPath) {
    cell.selectionStyle = .none
    let kb = userKeyboards[indexPath.row]
    
    cell.textLabel?.text = kb.languageName
    cell.detailTextLabel?.text = kb.name
    cell.tag = indexPath.row
    
    if Manager.shared.currentKeyboardID == kb.fullID {
      cell.selectionStyle = .blue
      cell.isSelected = true
      cell.accessoryType = self.accessoryType
    } else {
      cell.selectionStyle = .none
      cell.isSelected = false
      cell.accessoryType = self.accessoryType
    }
  }
  
  override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    switchKeyboard(indexPath.row)
  }
  
  // MARK: - keyboard switching
  
  public func switchKeyboard(_ index: Int) {
    // Switch keyboard and register to user defaults.
    if Manager.shared.setKeyboard(userKeyboards[index]) {
      tableView.reloadData()
    }
    
    Manager.shared.dismissKeyboardPicker(self)
  }
  
  // never called but on the subclass
  public func showAddKeyboard() {
  }

  
  public func loadUserKeyboards() {
    userKeyboards = Storage.active.userDefaults.userKeyboards ?? []
    tableView.reloadData()
  }
}
