//
//  LanguageViewController.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-09-15.
//  Copyright © 2017 SIL International. All rights reserved.
//
import QuartzCore
import UIKit

private let activityViewTag = -2
private let toolbarButtonTag = 100
private let toolbarLabelTag = 101
private let toolbarActivityIndicatorTag = 102

class LanguageViewController: UITableViewController, UIAlertViewDelegate {
  private var userKeyboards: [String: InstallableKeyboard] = [:]
  private var userLexicalModels: [String: InstallableLexicalModel] = [:]
  private var sectionIndexTitles: [String] = []
  private var indices: [Int] = []
  private var selectedSection = 0
  private var isUpdate = false
  private var languages: [Language] = []
  private let keyboardRepository: KeyboardRepository?

  private var keyboardDownloadStartedObserver: NotificationObserver?
  private var keyboardDownloadFailedObserver: NotificationObserver?

  init(_ keyboardRepository: KeyboardRepository) {
    self.keyboardRepository = keyboardRepository
    super.init(nibName: nil, bundle: nil)
    keyboardRepository.delegate = self
  }

  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
  
  func postLanguageLoad(languageDict: [String: Language]) {
    languages = languageList(languageDict)
  }
  
  func idxOfLanguage(languageID: String) -> Int {
    let langIdx = self.languages.firstIndex(where: {
      $0.id == languageID
    }) ?? 0
    return langIdx
  }
  
  override func loadView() {
    super.loadView()
    if let languageDict = keyboardRepository?.languages {
      self.postLanguageLoad(languageDict: languageDict)
    } else {
      log.info("Fetching repository from API for keyboard download (LanguageViewController)")
      keyboardRepository?.fetch()
    }

    loadUserKeyboards()
    loadUserLexicalModels()
  }

  override func viewDidLoad() {
    super.viewDidLoad()
    title = "Add New Keyboard"
    selectedSection = NSNotFound
    keyboardDownloadStartedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.packageDownloadStarted,
      observer: self,
      function: LanguageViewController.keyboardDownloadStarted)
    keyboardDownloadFailedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.packageDownloadFailed,
      observer: self,
      function: LanguageViewController.keyboardDownloadFailed)
  }

  override func viewDidAppear(_ animated: Bool) {
    super.viewDidAppear(animated)
    navigationController?.setToolbarHidden(true, animated: true)
    // if no rows to show yet, show a loading indicator
    if numberOfSections(in: tableView) == 0 {
      showActivityView()
      log.info("didAppear: LanguageViewController, but no rows to show")
    } else {
      log.info("didAppear: LanguageViewController")
    }
  }

  // MARK: - Table view data source UITableViewDataSource

  override func numberOfSections(in tableView: UITableView) -> Int {
    return languages.count
  }

  override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    return 1
  }

  override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    let cellIdentifierType1 = "CellType1"
    let cellIdentifierType2 = "CellType2"
    let keyboards = languages[indexPath.section].keyboards!
    let cellIdentifier = (keyboards.count < 2) ? cellIdentifierType1 : cellIdentifierType2
    let cell: UITableViewCell
    if let reusedCell = tableView.dequeueReusableCell(withIdentifier: cellIdentifier) {
      cell = reusedCell
    } else {
      if keyboards.count < 2 {
        cell = KeyboardNameTableViewCell(style: .subtitle, reuseIdentifier: cellIdentifier)
        let selectionColor = UIView()
        selectionColor.backgroundColor = Colors.selectionPrimary
        cell.selectedBackgroundView = selectionColor
      } else {
        cell = UITableViewCell(style: .default, reuseIdentifier: cellIdentifier)
        cell.accessoryType = .disclosureIndicator
        let selectionColor = UIView()
        selectionColor.backgroundColor = Colors.selectionPrimary
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

    sectionIndexTitles = []
    indices = []
    for (index, language) in languages.enumerated() {
      let firstLetter = String(language.name.prefix(1)).uppercased()
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
    guard let language = languages[safe: indexPath.section] else {
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
    let kbState = ResourceDownloadManager.shared.stateForKeyboard(withID: keyboardID)
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

    let language = languages[indexPath.section]
    let keyboardIndex = 0;
    let keyboard = language.keyboards![keyboardIndex]

    let state = ResourceDownloadManager.shared.stateForKeyboard(withID: keyboard.id)
    if state != .downloading {
      isUpdate = state != .needsDownload
      let alertController = UIAlertController(title: "\(language.name): \(keyboard.name)",
        message: "Would you like to download this keyboard?",
            preferredStyle: UIAlertController.Style.alert)
      alertController.addAction(UIAlertAction(title: "Cancel",
                                              style: UIAlertAction.Style.cancel,
                                              handler: nil))
      alertController.addAction(UIAlertAction(title: "Download",
                                                style: UIAlertAction.Style.default,
                                                handler: {_ in self.downloadHandler(keyboardIndex)} ))
      self.present(alertController, animated: true, completion: nil)
    }
  }

  override func tableView(_ tableView: UITableView, accessoryButtonTappedForRowWith indexPath: IndexPath) {
    let title = tableView.cellForRow(at: indexPath)?.textLabel?.text ?? ""
    showLanguageDetailView(title: title, languageIndex: indexPath.section)
  }

  func showLanguageDetailView(title: String, languageIndex: Int) {
    let langDetailView = LanguageDetailViewController(language: languages[languageIndex])
    langDetailView.title = title
    navigationController?.pushViewController(langDetailView, animated: true)
  }
    
  func downloadHandler(_ keyboardIndex: Int) {
    let language = languages[selectedSection]
    let fullID = FullKeyboardID(keyboardID: language.keyboards![keyboardIndex].id, languageID: language.id)
    let installClosure = ResourceDownloadManager.shared.standardKeyboardInstallCompletionBlock(forFullID: fullID, withModel: true)
    ResourceDownloadManager.shared.downloadKeyboard(withID: fullID.keyboardID, languageID: fullID.languageID, isUpdate: isUpdate, completionBlock: installClosure)
  }

  private func keyboardDownloadStarted() {
    view.isUserInteractionEnabled = false
    navigationItem.setHidesBackButton(true, animated: true)
  }

  private func keyboardDownloadFailed() {
    Alerts.showDownloadErrorAlert(in: self, handler: Alerts.popToNavigationRootHandler(for: self))

    view.isUserInteractionEnabled = true
    navigationItem.setHidesBackButton(false, animated: true)
  }

  func showActivityView() {
    view.isUserInteractionEnabled = false
    let indicatorView = UIActivityIndicatorView(style: .whiteLarge)
    let activityView = UIView(frame: indicatorView.bounds.insetBy(dx: -10.0, dy: -10.0))
    activityView.backgroundColor = Colors.spinnerBackground
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
      return
    }

    for kb in userKbList {
      let dictKey = "\(kb.languageID)_\(kb.id)"
      userKeyboards[dictKey] = kb
    }
  }
    
    func loadUserLexicalModels() {
        userLexicalModels = [:]
        guard let userLmList = Storage.active.userDefaults.userLexicalModels else {
            return
        }
        
        for lm in userLmList {
            let dictKey = "\(lm.languageID)_\(lm.id)"
            userLexicalModels[dictKey] = lm
        }
    }

  private func isAdded(languageID: String?, keyboardID: String?) -> Bool {
    guard let languageID = languageID, let keyboardID = keyboardID else {
      return false
    }
    return userKeyboards["\(languageID)_\(keyboardID)"] != nil
  }

  private func showConnectionErrorAlert() {
    Alerts.showConnectionErrorAlert(in: self, handler: Alerts.popToNavigationRootHandler(for: self))
  }
}

// MARK: - KeyboardRepositoryDelegate
extension LanguageViewController: KeyboardRepositoryDelegate {
  func keyboardRepositoryDidFetch(_ repository: KeyboardRepository) {
    if let languageDict = repository.languages {
      self.postLanguageLoad(languageDict: languageDict)
    }
    self.dismissActivityView()
    self.tableView.reloadData()
    if self.numberOfSections(in: self.tableView) == 0 {
      self.showConnectionErrorAlert()
    }
  }

  func keyboardRepository(_ repository: KeyboardRepository, didFailFetch error: Error) {
    dismissActivityView()
    showConnectionErrorAlert()
  }

  private func languageList(_ languageDict: [String: Language]) -> [Language] {
    return languageDict.values.sorted { a, b -> Bool in
      a.name.localizedCaseInsensitiveCompare(b.name) == .orderedAscending
    }
  }
}

// MARK: - LexicalModelRepositoryDelegate

extension LanguageViewController: LexicalModelRepositoryDelegate {
  func lexicalModelRepositoryDidFetchList(_ repository: LexicalModelRepository) {
    if let languageDict = repository.languages {
      self.postLanguageLoad(languageDict: languageDict)
    }
    self.dismissActivityView()
    self.tableView.reloadData()
    if self.numberOfSections(in: self.tableView) == 0 {
      self.showConnectionErrorAlert()
    }
  }
  
  func lexicalModelRepository(_ repository: LexicalModelRepository, didFailFetch error: Error) {
    dismissActivityView()
    showConnectionErrorAlert()
  }
}
