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

private let toolbarButtonTag = 100

class KeyboardSwitcherViewController: KeyboardPickerViewController {

  override func viewDidLoad() {
    super.viewDidLoad()
    
    // remove UI that adds keyboards
    navigationItem.rightBarButtonItem = nil
    
    // maybe remove, but for now, distinguish this picker via color
    navigationController?.toolbar?.barTintColor = UIColor(red: 0.8, green: 0.25,
                                                          blue: 0.5, alpha: 0.4)
    // I don't think we need to remove the download observers
    //  they just won't be used if we never start a download
    //keyboardDownloadStartedObserver = nil

    log.info("didLoad: KeyboardSwitcherViewController")
  }

  override func viewDidAppear(_ animated: Bool) {
    // remove the update button, if any
    navigationController?.toolbar?.viewWithTag(toolbarButtonTag)?.removeFromSuperview()

    log.info("didAppear: KeyboardSwitcherViewController")
  }
  
  
  // override to make it never try to update
  //NOTE: might we want it to check?
  override func checkUpdates() -> Bool {
    return false
  }
  
  override func tableView(_ tableView: UITableView,
                          accessoryButtonTappedForRowWith indexPath: IndexPath) {
    // do not show keyboard info, as it has the ability to delete it
    //NOTE: we may wish to show info without that ability, but for now, this is easier
    return
  }

  override func tableView(_ tableView: UITableView,
                          willDisplay cell: UITableViewCell,
                          forRowAt indexPath: IndexPath) {
    super.tableView(tableView, willDisplay: cell, forRowAt: indexPath)
    cell.accessoryType = .none
  }
}
