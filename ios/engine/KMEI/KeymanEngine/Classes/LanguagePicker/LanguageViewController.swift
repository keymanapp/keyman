//
//  LanguageViewController.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-09-15.
//  Copyright © 2017 SIL International. All rights reserved.
//
import QuartzCore
import UIKit

private let errorAlertTag = -1
private let activityViewTag = -2
private let toolbarButtonTag = 100
private let toolbarLabelTag = 101
private let toolbarActivityIndicatorTag = 102

class LanguageViewController: UITableViewController, UIAlertViewDelegate {
  private var userKeyboards: [String: InstallableKeyboard] = [:]
  private var sectionIndexTitles: [String] = []
  private var indices: [Int] = []
  private var selectedSection = 0
  private var isUpdate = false

  private var languagesUpdatedObserver: NotificationObserver?
  private var languagesDownloadFailedObserver: NotificationObserver?
  private var keyboardDownloadStartedObserver: NotificationObserver?
  private var keyboardDownloadFailedObserver: NotificationObserver?

  override func loadView() {
    super.loadView()
    if Manager.shared.currentRequest == nil && Manager.shared.languages.isEmpty {
      Manager.shared.fetchKeyboards(completionBlock: nil)
    }
    loadUserKeyboards()
  }

  override func viewDidLoad() {
    super.viewDidLoad()
    title = "Add New Keyboard"
    selectedSection = NSNotFound
    languagesUpdatedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.languagesUpdated,
      observer: self,
      function: LanguageViewController.languagesUpdated)
    languagesDownloadFailedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.languagesDownloadFailed,
      observer: self,
      function: LanguageViewController.languagesDownloadFailed)
    keyboardDownloadStartedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.keyboardDownloadStarted,
      observer: self,
      function: LanguageViewController.keyboardDownloadStarted)
    keyboardDownloadFailedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.keyboardDownloadFailed,
      observer: self,
      function: LanguageViewController.keyboardDownloadFailed)
  }

  override func viewDidAppear(_ animated: Bool) {
    super.viewDidAppear(animated)
    navigationController?.setToolbarHidden(true, animated: true)
    // if no rows to show yet, show a loading indicator
    if numberOfSections(in: tableView) == 0 {
      showActivityView()
    }
  }

  override func numberOfSections(in tableView: UITableView) -> Int {
    return Manager.shared.languages.count
  }

  override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    return 1
  }

  override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    let cellIdentifierType1 = "CellType1"
    let cellIdentifierType2 = "CellType2"
    let keyboards = Manager.shared.languages[indexPath.section].keyboards!
    let cellIdentifier = (keyboards.count < 2) ? cellIdentifierType1 : cellIdentifierType2
    let cell: UITableViewCell
    if let reusedCell = tableView.dequeueReusableCell(withIdentifier: cellIdentifier) {
      cell = reusedCell
    } else {
      if keyboards.count < 2 {
        cell = KeyboardNameTableViewCell(style: .subtitle, reuseIdentifier: cellIdentifier)
        let selectionColor = UIView()
        selectionColor.backgroundColor = UIColor(red: 204.0 / 255.0, green: 136.0 / 255.0,
                                                 blue: 34.0 / 255.0, alpha: 1.0)
        cell.selectedBackgroundView = selectionColor
      } else {
        cell = UITableViewCell(style: .default, reuseIdentifier: cellIdentifier)
        cell.accessoryType = .disclosureIndicator
        let selectionColor = UIView()
        selectionColor.backgroundColor = UIColor(red: 204.0 / 255.0, green: 136.0 / 255.0,
                                                 blue: 34.0 / 255.0, alpha: 1.0)
        cell.selectedBackgroundView = selectionColor
      }
    }
    cell.detailTextLabel?.text = keyboards.count < 2 ? (keyboards.first?.name ?? "") : ""
    return cell
  }

  // Create sections by first letter
  override func sectionIndexTitles(for tableView: UITableView) -> [String] {
    if !sectionIndexTitles.isEmpty {
      return sectionIndexTitles
    }

    let languages = Manager.shared.languages
    sectionIndexTitles = []
    indices = []
    for (index, language) in languages.enumerated() {
      let firstLetter = String(language.name.prefix(1))
      if !sectionIndexTitles.contains(firstLetter) {
        sectionIndexTitles.append(firstLetter)
        indices.append(index)
      }
    }
    return sectionIndexTitles
  }

  override func tableView(_ tableView: UITableView, sectionForSectionIndexTitle title: String, at index: Int) -> Int {
    return indices[index]
  }

  override func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
    guard let language = Manager.shared.languages[safe: indexPath.section] else {
      return
    }

    cell.textLabel?.text = language.name

    if cell.accessoryType == .disclosureIndicator {
      return
    }
    guard let cell = cell as? KeyboardNameTableViewCell else {
      return
    }

    let languageID = language.id
    let keyboardID = language.keyboards![0].id

    if isAdded(languageID: languageID, keyboardID: keyboardID) {
      cell.accessoryType = .checkmark
      cell.isUserInteractionEnabled = false
      cell.textLabel?.isEnabled = false
      cell.detailTextLabel?.isEnabled = false
    } else {
      cell.accessoryType = .none
      cell.isUserInteractionEnabled = true
      cell.textLabel?.isEnabled = true
      cell.detailTextLabel?.isEnabled = true
    }
    let kbState = Manager.shared.stateForKeyboard(withID: keyboardID)
    cell.setKeyboardState(kbState, selected: false, defaultAccessoryType: cell.accessoryType)
  }

  override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    selectedSection = indexPath.section
    tableView.cellForRow(at: indexPath)?.isSelected = false
    if tableView.cellForRow(at: indexPath)?.accessoryType == .disclosureIndicator {
      let title = tableView.cellForRow(at: indexPath)?.textLabel?.text ?? ""
      showLanguageDetailView(title: title, languageIndex: indexPath.section)
      return
    }

    let language = Manager.shared.languages[indexPath.section]
    let keyboard = language.keyboards![0]

    let state = Manager.shared.stateForKeyboard(withID: keyboard.id)
    if state != .downloading {
      isUpdate = state != .needsDownload
      let alert = UIAlertView(title: "\(language.name): \(keyboard.name)",
        message: "Would you like to download this keyboard?", delegate: self,
        cancelButtonTitle: "Cancel", otherButtonTitles: "Download")
      alert.tag = 0
      alert.show()
    }
  }

  override func tableView(_ tableView: UITableView, accessoryButtonTappedForRowWith indexPath: IndexPath) {
    let title = tableView.cellForRow(at: indexPath)?.textLabel?.text ?? ""
    showLanguageDetailView(title: title, languageIndex: indexPath.section)
  }

  private func showLanguageDetailView(title: String, languageIndex: Int) {
    let langDetailView = LanguageDetailViewController(language: Manager.shared.languages[languageIndex])
    langDetailView.title = title
    navigationController?.pushViewController(langDetailView, animated: true)
  }

  func alertView(_ alertView: UIAlertView, clickedButtonAt buttonIndex: Int) {
    if alertView.tag == errorAlertTag {
      if !Manager.shared.languages.isEmpty {
        navigationController?.popToRootViewController(animated: true)
      }
    } else {
      // Keyboard download confirmation alert (tag is used for keyboard index).
      if buttonIndex != alertView.cancelButtonIndex {
        let language = Manager.shared.languages[selectedSection]
        let keyboard = language.keyboards![alertView.tag]
        Manager.shared.downloadKeyboard(withID: keyboard.id, languageID: language.id, isUpdate: isUpdate)
      }
    }
  }

  private func languagesUpdated() {
    dismissActivityView()
    tableView.reloadData()
    if numberOfSections(in: tableView) == 0 {
      showConnectionErrorAlert()
    }
  }

  private func languagesDownloadFailed() {
    dismissActivityView()
    showConnectionErrorAlert()
  }

  private func keyboardDownloadStarted() {
    view.isUserInteractionEnabled = false
    navigationItem.setHidesBackButton(true, animated: true)

    guard let toolbar = navigationController?.toolbar else {
      return
    }

    let labelFrame = CGRect(origin: toolbar.frame.origin,
                            size: CGSize(width: toolbar.frame.width * 0.95,
                                         height: toolbar.frame.height * 0.7))
    let label = UILabel(frame: labelFrame)
    label.backgroundColor = UIColor.clear
    label.textColor = UIColor.white
    label.textAlignment = .center
    label.center = CGPoint(x: toolbar.frame.width * 0.5, y: toolbar.frame.height * 0.5)
    label.text = "Downloading\u{2026}"
    label.autoresizingMask = [.flexibleLeftMargin, .flexibleRightMargin, .flexibleTopMargin,
                              .flexibleBottomMargin, .flexibleWidth, .flexibleHeight]
    label.tag = toolbarLabelTag

    let indicatorView = UIActivityIndicatorView(activityIndicatorStyle: .gray)
    indicatorView.center = CGPoint(x: toolbar.frame.width - indicatorView.frame.width,
                                   y: toolbar.frame.height * 0.5)
    indicatorView.autoresizingMask = [.flexibleLeftMargin, .flexibleTopMargin, .flexibleBottomMargin]
    indicatorView.tag = toolbarActivityIndicatorTag
    indicatorView.startAnimating()
    toolbar.viewWithTag(toolbarButtonTag)?.removeFromSuperview()
    toolbar.viewWithTag(toolbarLabelTag)?.removeFromSuperview()
    toolbar.viewWithTag(toolbarActivityIndicatorTag)?.removeFromSuperview()
    toolbar.addSubview(label)
    toolbar.addSubview(indicatorView)
    navigationController?.setToolbarHidden(false, animated: true)
  }

  private func keyboardDownloadFailed() {
    view.isUserInteractionEnabled = true
    navigationItem.setHidesBackButton(false, animated: true)
  }

  func showActivityView() {
    view.isUserInteractionEnabled = false
    let indicatorView = UIActivityIndicatorView(activityIndicatorStyle: .whiteLarge)
    let activityView = UIView(frame: indicatorView.bounds.insetBy(dx: -10.0, dy: -10.0))
    activityView.backgroundColor = UIColor(white: 0.5, alpha: 0.8)
    activityView.layer.cornerRadius = 6.0
    activityView.center = view.center
    activityView.tag = activityViewTag
    activityView.autoresizingMask = [.flexibleLeftMargin, .flexibleRightMargin, .flexibleTopMargin,
                                     .flexibleBottomMargin]

    indicatorView.center = CGPoint(x: activityView.bounds.size.width * 0.5, y: activityView.bounds.size.height * 0.5)
    indicatorView.startAnimating()
    activityView.addSubview(indicatorView)
    view.addSubview(activityView)
  }

  func dismissActivityView() {
    let activityView = view.viewWithTag(activityViewTag)
    activityView?.removeFromSuperview()
    view.isUserInteractionEnabled = true
  }

  func loadUserKeyboards() {
    userKeyboards = [:]
    guard let userKbList = Storage.active.userDefaults.userKeyboards else {
      userKeyboards = [:]
      return
    }

    for kb in userKbList {
      let dictKey = "\(kb.languageID)_\(kb.id)"
      userKeyboards[dictKey] = kb
    }
  }

  private func isAdded(languageID: String?, keyboardID: String?) -> Bool {
    guard let languageID = languageID, let keyboardID = keyboardID else {
      return false
    }
    return userKeyboards["\(languageID)_\(keyboardID)"] != nil
  }

  private func showConnectionErrorAlert() {
    dismissActivityView()
    let alert = UIAlertView(title: "Connection Error",
                            message: "Could not reach Keyman server. Please try again later.",
                            delegate: self, cancelButtonTitle: "OK", otherButtonTitles: "")
    alert.tag = errorAlertTag
    alert.show()
  }
}
