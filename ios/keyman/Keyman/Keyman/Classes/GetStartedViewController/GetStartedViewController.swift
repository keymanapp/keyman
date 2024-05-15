//
//  GetStartedViewController.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-07.
//  Copyright © 2017 SIL International. All rights reserved.
//

import KeymanEngine
import UIKit

class GetStartedViewController: UIViewController, UITableViewDelegate, UITableViewDataSource {
  var mainViewController: MainViewController!
  @IBOutlet var navItem: UINavigationItem!
  @IBOutlet var tableView: UITableView!
  
  deinit {
    NotificationCenter.default.removeObserver(self)
  }
  
  convenience init() {
    self.init(nibName: "GetStartedViewController", bundle: nil)
  }
  
  override func viewDidLoad() {
    super.viewDidLoad()
    
    NotificationCenter.default.addObserver(self, selector: #selector(self.refreshTable),
                                           name: UIApplication.didBecomeActiveNotification, object: nil)
    
    navItem.title = NSLocalizedString("menu-get-started", comment: "")
    tableView.isScrollEnabled = false
    
    let icon = UIImageView(image: #imageLiteral(resourceName: "887-notepad.png"))
    navItem.leftBarButtonItem = UIBarButtonItem(customView: icon)
    
    tableView.separatorInset = UIEdgeInsets(top: 0, left: 0, bottom: 0, right: 15)
    let frame = CGRect(x: 0, y: 0, width: tableView.frame.width, height: 63)
    
    let lineFrame = CGRect(x: 0, y: 0, width: tableView.frame.width, height: 1)
    let header = UIView(frame: lineFrame)
    header.backgroundColor = UIColor(red: 0.0, green: 0.5, blue: 1.0, alpha: 1.0)
    
    let line = UIView(frame: lineFrame)
    line.backgroundColor = UIColor(red: 0.0, green: 0.5, blue: 1.0, alpha: 1.0)
    
    let dontShowAgainSwitch = UISwitch()
    let width: CGFloat = 120
    let height: CGFloat = 63
    let x: CGFloat = (frame.width - width - dontShowAgainSwitch.frame.width) / 2.0
    let labelFrame = CGRect(x: x, y: 0, width: width, height: height)
    let label = UILabel(frame: labelFrame)
    
    label.text = NSLocalizedString("disable-get-started", comment: "")
    label.font = label.font.withSize(12.0)
    label.textAlignment = .center
    label.backgroundColor = UIColor.clear
    
    let switchFrame = CGRect(x: x + width,
                             y: (height - dontShowAgainSwitch.frame.height) / 2.0,
                             width: dontShowAgainSwitch.frame.width,
                             height: dontShowAgainSwitch.frame.height)
    dontShowAgainSwitch.frame = switchFrame
    dontShowAgainSwitch.isOn = dontShowGetStarted
    dontShowAgainSwitch.addTarget(self, action: #selector(self.switchValueChanged),
                                  for: .valueChanged)
    
    let footer = UIView(frame: frame)
    footer.addSubview(label)
    footer.addSubview(dontShowAgainSwitch)
    footer.addSubview(line)
    footer.backgroundColor = Colors.systemBackground
    
    tableView.tableHeaderView = header
    tableView.tableFooterView = footer
  }
  
  func numberOfSections(in tableView: UITableView) -> Int {
    return 3
  }
  
  func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    return 1
  }
  
  func tableView(_ tableView: UITableView,
                 cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    let cellIdentifier = "Cell"
    
    let reusableCell = tableView.dequeueReusableCell(withIdentifier: cellIdentifier)
    if let cell = reusableCell {
      return cell
    }
    
    let cell = UITableViewCell(style: .subtitle, reuseIdentifier: cellIdentifier)
    let selectionColor = UIView()
    selectionColor.backgroundColor = Colors.selectionSecondary
    cell.selectedBackgroundView = selectionColor
    cell.textLabel?.font = cell.textLabel?.font?.withSize(12.0)
    cell.detailTextLabel?.font = cell.detailTextLabel?.font?.withSize(10.0)
    return cell
  }
  
  func tableView(_ tableView: UITableView,
                 willDisplay cell: UITableViewCell,
                 forRowAt indexPath: IndexPath) {
    switch indexPath.section {
    case 0:
      cell.textLabel?.text = NSLocalizedString("tutorial-add-keyboard", comment: "")
      cell.detailTextLabel?.text = ""
      if !didAddKeyboard() {
        cell.accessoryType = .none
      } else {
        cell.accessoryType = .checkmark
      }
    case 1:
      cell.textLabel?.text = NSLocalizedString("tutorial-system-keyboard", comment: "")
      // We can expedite this via near-direct settings link.
      // So, let's add the one extra needed detail to our detail text.
      let keyboardsMenuText = NSLocalizedString("ios-settings-keyboards", comment: "")
      let enableText = String.init(format: NSLocalizedString("toggle-to-enable", comment: ""), "Keyman")
      // Probably needs to be i18n-adjusted too.
      let menuBreadcrumbing = NSLocalizedString("menu-breadcrumbing", comment: "")
      cell.detailTextLabel?.text = String.init(format: menuBreadcrumbing, keyboardsMenuText, enableText)
      
      if !AppDelegate.isKeymanEnabledSystemWide() {
        cell.accessoryType = .none
      } else {
        cell.accessoryType = .checkmark
      }
    case 2:
      cell.textLabel?.text = NSLocalizedString("tutorial-show-help", comment: "")
      cell.detailTextLabel?.text = ""
      cell.accessoryType = .detailButton
    default:
      break
    }
  }
  
  // In a xib-based application, navigation from a table can be handled in -tableView:didSelectRowAtIndexPath:
  func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    tableView.cellForRow(at: indexPath)?.isSelected = false
    performAction(for: indexPath)
  }
  
  func tableView(_ tableView: UITableView, accessoryButtonTappedForRowWith indexPath: IndexPath) {
    performAction(for: indexPath)
  }
  
  private func performAction(for indexPath: IndexPath) {
    switch indexPath.section {
    case 0:
      mainViewController.dismissGetStartedView(nil)
      mainViewController.showInstalledLanguages()
    case 1:
      if let appSettings = URL(string: UIApplication.openSettingsURLString) {
        UniversalLinks.externalLinkLauncher?(appSettings)
      } else {
        let setUpVC = SetUpViewController()
        mainViewController.present(setUpVC, animated: true, completion: nil)
      }
      
      // While it'd be nice to keep it in view, it seems to be automatically dismissed.
      // This at least helps keep the app's state properly managed.
      mainViewController.dismissGetStartedView(nil)
    case 2:
      mainViewController.dismissGetStartedView(nil)
      mainViewController.infoButtonClick(nil)
    default:
      break
    }
  }
  
  @objc func refreshTable() {
    tableView.reloadData()
  }
  
  private func didAddKeyboard() -> Bool {
    let userData = AppDelegate.activeUserDefaults()
    guard let userKeyboards = userData.userKeyboards else {
      return false
    }
    
    if userKeyboards.isEmpty {
      return false
    }
    if userKeyboards.count >= 2 {
      return true
    }
    
    let firstKB = userKeyboards[0]
    return firstKB.id != Defaults.keyboard.id || firstKB.languageID != Defaults.keyboard.languageID
  }
  
  @objc func switchValueChanged(_ sender: Any) {
    let userData = AppDelegate.activeUserDefaults()
    if let toggle = sender as? UISwitch {
      userData.set(!toggle.isOn, forKey: shouldShowGetStartedKey)
      userData.synchronize()
    }
  }
  
  private var dontShowGetStarted: Bool {
    return !shouldShowGetStarted
  }
  
  private var shouldShowGetStarted: Bool {
    let userData = AppDelegate.activeUserDefaults()
    var value: Any? = userData.object(forKey: shouldShowGetStartedKey)
    if nil == value { // new key not present, try the old key
      value = userData.object(forKey: dontShowGetStartedKey)
      if nil == value {
        userData.set(true, forKey: shouldShowGetStartedKey)
        userData.synchronize()
        return true // default to showing when no preference has been set
        // so it shows at startup
      } else {
        userData.removeObject(forKey: dontShowGetStartedKey)
        userData.set(false, forKey: shouldShowGetStartedKey)
        userData.synchronize()
        // remove the old, confusing value and set the new value
        return false
      }
    }
    return userData.bool(forKey: shouldShowGetStartedKey)
  }
}
