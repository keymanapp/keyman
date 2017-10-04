//
//  GetStartedViewController.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-07.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit

class GetStartedViewController: UIViewController, UITableViewDelegate, UITableViewDataSource {
  var mainViewController: MainViewController?
  @IBOutlet var navItem: UINavigationItem!
  @IBOutlet var tableView: UITableView!

  deinit {
    NotificationCenter.default.removeObserver(self)
  }

  override func viewDidLoad() {
    super.viewDidLoad()

    NotificationCenter.default.addObserver(self, selector: #selector(self.refreshTable),
                                           name: NSNotification.Name.UIApplicationDidBecomeActive, object: nil)

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

    label.text = "Don't show again"
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
    footer.backgroundColor = UIColor(white: 1.0, alpha: 1.0)
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
    selectionColor.backgroundColor = UIColor(red: 95.0 / 255.0, green: 196.0 / 255.0, blue: 217.0 / 255.0, alpha: 1.0)
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
      cell.textLabel?.text = "Add a keyboard for your language"
      cell.detailTextLabel?.text = ""
      if !didAddKeyboard() {
        cell.accessoryType = .none
      } else {
        cell.accessoryType = .checkmark
      }
    case 1:
      cell.textLabel?.text = "Set up Keyman as system-wide keyboard"
      cell.detailTextLabel?.text = ""
      if !AppDelegate.isKeymanEnabledSystemWide() {
        cell.accessoryType = .none
      } else {
        cell.accessoryType = .checkmark
      }
    case 2:
      cell.textLabel?.text = "More info"
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
      mainViewController?.dismissGetStartedView(nil)
      KMManager.sharedInstance().showKeyboardPicker(in: mainViewController, shouldAddKeyboard: true)
    case 1:
      mainViewController?.dismissGetStartedView(nil)
      let setUpVC = SetUpViewController()
      mainViewController?.present(setUpVC, animated: true, completion: nil)
    case 2:
      mainViewController?.dismissGetStartedView(nil)
      mainViewController?.infoButtonClick(nil)
    default:
      break
    }
  }

  @objc func refreshTable() {
    tableView.reloadData()
  }

  private func didAddKeyboard() -> Bool {
    let userData = AppDelegate.activeUserDefaults()
    let userKbs = userData.object(forKey: kKeymanUserKeyboardsListKey) as? [[AnyHashable: String]]
    guard let userKeyboards = userKbs else {
      return false
    }

    if userKeyboards.isEmpty {
      return false
    }
    if userKeyboards.count >= 2 {
      return true
    }

    let firstKB = userKeyboards[0]
    let kbID = firstKB[kKeymanKeyboardIdKey] ?? ""
    let langID = firstKB[kKeymanLanguageIdKey] ?? ""
    if (kbID == kKeymanDefaultKeyboardID) && (langID == langID) {
      return false
    } else {
      return true
    }
  }

  @objc func switchValueChanged(_ sender: Any) {
    let userData = AppDelegate.activeUserDefaults()
    if let toggle = sender as? UISwitch {
      userData.set(toggle.isOn, forKey: dontShowGetStartedKey)
      userData.synchronize()
    }
  }

  private var dontShowGetStarted: Bool {
    let userData = AppDelegate.activeUserDefaults()
    return userData.bool(forKey: dontShowGetStartedKey)
  }
}
