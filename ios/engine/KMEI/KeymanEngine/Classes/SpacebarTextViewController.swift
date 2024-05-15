//
//  SpacebarTextViewController.swift
//  KeymanEngine
//
//  Created by Marc Durdin on 24/6/21.
//  Copyright © 2021 SIL International. All rights reserved.
//

import UIKit

open class SpacebarTextViewController: UITableViewController {
  let items: [String] = [
    "menu-settings-spacebar-item-language",
    "menu-settings-spacebar-item-keyboard",
    "menu-settings-spacebar-item-languageKeyboard",
    "menu-settings-spacebar-item-blank"
  ]
  let itemValues: [SpacebarText] = [
    SpacebarText.LANGUAGE,
    SpacebarText.KEYBOARD,
    SpacebarText.LANGUAGE_KEYBOARD,
    SpacebarText.BLANK
  ]
  let cellIdentifier = "spacebarTextCell"
  var selection: Int
  
  override open func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)
  }
  
  override open func viewDidLoad() {
    super.viewDidLoad()
    
    title = NSLocalizedString("menu-settings-spacebar-title", bundle: engineBundle, comment: "")
    navigationItem.setHidesBackButton(false, animated: true)
    navigationItem.leftBarButtonItem?.isEnabled = true
    
    navigationController?.toolbar?.barTintColor = Colors.statusToolbar
  }
  
  public init() {
    switch Manager.shared.spacebarText {
    case SpacebarText.LANGUAGE:
      selection = 0
    case SpacebarText.KEYBOARD:
      selection = 1
    case SpacebarText.LANGUAGE_KEYBOARD:
      selection = 2
    case SpacebarText.BLANK:
      selection = 3
    }
    super.init(nibName: nil, bundle: nil)
    _ = view
  }
  
  required public init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
  
  // MARK: - Table view data source
  
  override open func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    return self.items.count
  }
  
  override public func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    
    let cell: UITableViewCell
    if let reusedCell = tableView.dequeueReusableCell(withIdentifier: cellIdentifier) {
      cell = reusedCell
    } else {
      cell = UITableViewCell(style: .default, reuseIdentifier: cellIdentifier)
    }
    cell.accessoryType = self.selection == indexPath.row ? .checkmark : .none
    cell.textLabel?.text = NSLocalizedString(items[indexPath.row], bundle: engineBundle, comment: "")
    return cell
  }
  
  override public func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    Manager.shared.spacebarText = self.itemValues[indexPath.row]
    navigationController?.popViewController(animated: true)
  }
}
