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
import KeymanEngine

let keyboardsSection = 0
let languageSettingsSection = 2
let dictionarySection = 1
let keyboardInfoSection = 3

let keyboardsHeader = "Available Keyboards"
let languageSettingsHeader = "Dictionary Settings"
let dictionaryHeader = "Downloadable Dictionaries"
let keyboardInfoHeader = "Keyboard Information"

let keyboardsRowCount = 1
let languageSettingsRowCount = 2
let keyboardInfoRowCount = 1

let keyboardsRow = 0
let predictionsRow = 0
let correctionsRow = 1

let versionLabel = "Version"
let predictionLabel = "Suggest Predictions"
let correctionLabel = "Suggest Corrections"

let noDictionaryMessage = "No dictionaries available"

let attributeCellIdentifier = "attributeCell"
let switchCellIdentifier = "switchCell"
let labelCellIdentifier = "labelCell"

class KeyboardDetailController: UITableViewController {
  private var keyboardRepo = KeyboardRepository.shared
  private var settingsRepo = KeyboardSettingsRepository.shared
  private var lexicalModelRepo = LexicalModelRepository.shared

  private var keyboardState: KeyboardState? = nil
  private var delegate: RefreshKeyboardCheckmark? = nil
  private var lexicalModels: [FVLexicalModel] = []
  
  /*
  private var lexicalModelDownloadStartedObserver: NotificationObserver?
  private var lexicalModelDownloadCompletedObserver: NotificationObserver?
  private var lexicalModelDownloadFailedObserver: NotificationObserver?
*/
  
  override func viewDidLoad() {
    super.viewDidLoad()
    self.title = keyboardState?.name

    // get lexical models to display in dictionary section
    if let languageTag = self.keyboardState?.languageTag {
      lexicalModels = lexicalModelRepo.getAvailableLexicalModels(languageTag: languageTag)
    }

    /*
    lexicalModelDownloadStartedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.packageDownloadStarted,
      observer: self,
      function: KeyboardDetailController.lexicalModelDownloadStarted)
    lexicalModelDownloadCompletedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.packageDownloadCompleted,
      observer: self,
      function: KeyboardDetailController.lexicalModelDownloadCompleted)
    lexicalModelDownloadFailedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.packageDownloadFailed,
      observer: self,
      function: KeyboardDetailController.lexicalModelDownloadFailed)
     */
  }

  override func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)
  }
  
  /*
   * Used to display the values for the keyboard that was tapped, causing the segue to the detail view.
   */
  func configure(delegate: RefreshKeyboardCheckmark, keyboard: KeyboardState) {
    self.delegate = delegate
    self.keyboardState = keyboard
  }
  
  override func numberOfSections(in tableView: UITableView) -> Int {
    return 4
  }

  override func tableView(_ tableView: UITableView,
                 titleForHeaderInSection section: Int) -> String? {
    var header = ""
    switch section {
    case keyboardsSection:
      header = keyboardsHeader
    case languageSettingsSection:
      header = languageSettingsHeader
    case dictionarySection:
      header = dictionaryHeader
    case keyboardInfoSection:
      header = keyboardInfoHeader
    default:
      header = "Undefined"
    }
    return header
  }
  
  override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    var numberOfRows = 0;
    switch section {
    case keyboardsSection:
      numberOfRows = keyboardsRowCount
    case languageSettingsSection:
      numberOfRows = languageSettingsRowCount
    case dictionarySection:
      numberOfRows = max(1, self.lexicalModels.count)
    case keyboardInfoSection:
      numberOfRows = keyboardInfoRowCount
    default:
      numberOfRows = 0
    }
    return numberOfRows
  }

  override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    var tableCell: UITableViewCell? = nil
    
    switch indexPath.section {
      case keyboardsSection:
        tableCell = configureKeyboardCell()
      case languageSettingsSection:
        if indexPath.row == predictionsRow {
          tableCell = configurePredictionsSwitchCell()
        } else if indexPath.row == correctionsRow {
          tableCell = configureCorrectionsSwitchCell()
       }
      case dictionarySection:
        if self.lexicalModels.isEmpty {
          tableCell = configureNoDictionaryCell()
        } else {
          tableCell = configureDictionaryCell(index: indexPath.row)
        }
      case keyboardInfoSection:
        tableCell = configureVersionCell()

      default:
        tableCell = tableView.dequeueReusableCell(withIdentifier: "labelCell")
        tableCell?.textLabel!.text = "n/a"
      }
    return tableCell!
  }
  
  func configureKeyboardCell() -> UITableViewCell {
    let switchCell = tableView.dequeueReusableCell(withIdentifier: switchCellIdentifier) as! KeyboardDetailCell
    let keyboardAction: Callback = { (on) in
      var updatedKeyboardState = false
      var errorMessage = ""

      // call Keyman Engine to install or remove the keyboard
      if on {
        (updatedKeyboardState, errorMessage) = self.keyboardRepo.installKeyboard(keyboard: self.keyboardState!.definition)
      } else {
        updatedKeyboardState = self.keyboardRepo.removeKeyboard(keyboard: self.keyboardState!.definition)
      }
      
      if (updatedKeyboardState) {
        // update state and write to local settings storage
        self.keyboardState!.isActive = on
        self.settingsRepo.saveKeyboardState(state: self.keyboardState!)

        // update the on/off state of dependent switches
        self.updateDictionarySettingsState(animated: true)
        self.delegate?.refreshCheckmark()
      } else {
        // install or remove failed, set switch back to previous state
        let keyboardsIndex = IndexPath(row: keyboardsRow, section: keyboardsSection)
        
        if (!on) {
          errorMessage = "Keyboard deactivation failed"
        }
        self.reportError(message: errorMessage)
        
        self.updateCellSwitchState(index: keyboardsIndex,
                                   on: self.keyboardState!.isActive,
                                   animated: true,
                                   available: true)
      }
    }
    switchCell.configure(label: (self.keyboardState?.name)!, on: self.keyboardState!.isActive, available: true,
                         callback: keyboardAction)
    return switchCell
  }

  func reportError(message: String) {
    let alertController = UIAlertController(title: title, message: message,
      preferredStyle: UIAlertController.Style.alert)
    alertController.addAction(UIAlertAction(title: "OK",
                                            style: UIAlertAction.Style.cancel,
                                            handler: nil))

    self.present(alertController, animated: true, completion: nil)
    print(message)
    // TODO: send the error message + call stack through to us
    // some reporting mechanism
  }

  func configurePredictionsSwitchCell() -> UITableViewCell {
    let switchCell = tableView.dequeueReusableCell(withIdentifier: switchCellIdentifier) as! KeyboardDetailCell
    let predictionAction: Callback = { (on) in
      if self.lexicalModelRepo.applyPredictionSettings(languageId: self.keyboardState!.languageTag, modelId: self.keyboardState!.lexicalModelId!, on: on) {
        self.keyboardState!.updatePredictions(on: on)
        self.settingsRepo.saveKeyboardState(state: self.keyboardState!)
        self.updateDictionarySettingsState(animated: true)
      }
    }
    switchCell.configure(label: predictionLabel, on: self.keyboardState!.suggestPredictions, available: self.keyboardState!.canSuggestPredictions(),
                         callback: predictionAction)
    return switchCell
  }

  func configureCorrectionsSwitchCell() -> UITableViewCell {
    let switchCell = tableView.dequeueReusableCell(withIdentifier: switchCellIdentifier) as! KeyboardDetailCell
    let correctionAction: Callback = { (on) in
      if (self.lexicalModelRepo.applyCorrectionSettings(languageId: self.keyboardState!.languageTag, modelId: self.keyboardState!.lexicalModelId!, on: on)) {
        self.keyboardState?.suggestCorrections = on
        self.settingsRepo.saveKeyboardState(state: self.keyboardState!)
      }
    }
    switchCell.configure(label: correctionLabel, on: self.keyboardState!.suggestCorrections, available: self.keyboardState!.canSuggestCorrections(),
                         callback: correctionAction)
    return switchCell
  }

  func configureNoDictionaryCell() -> UITableViewCell {
    let labelCell = tableView.dequeueReusableCell(withIdentifier: labelCellIdentifier)
    labelCell?.textLabel!.text = noDictionaryMessage
    return labelCell!
  }
  
  func configureDictionaryCell(index: Int) -> UITableViewCell {
    let switchCell = tableView.dequeueReusableCell(withIdentifier: switchCellIdentifier) as! KeyboardDetailCell

    if (self.lexicalModels.isEmpty) {
      print("error, invalid cell: there are no lexical models available for this langauge")
      return configureNoDictionaryCell()
    }
    let thisDictionary = self.lexicalModels[index]
    
    let savedModel = (self.keyboardState?.lexicalModelName)!
    let modelEnabled = thisDictionary.name == savedModel

    let dictionaryAction: Callback = { (on) in
      let lexicalModelName = thisDictionary.name
      let lexicalModelId = thisDictionary.id

      if on {
        let message = "As you type in your language, the FirstVoices dictionary will provide suggestions.\n\nWould you like to install this dictionary?"
        let alert = UIAlertController(title: "\(lexicalModelName)", message: message, preferredStyle: .alert)
        
        let installAction = UIAlertAction(title: NSLocalizedString("Install", comment: "Default action"), style: .default, handler: { _ in
          ResourceDownloadManager.shared.downloadLexicalModelsForLanguageIfExists(languageID: self.keyboardState!.languageTag) { package, error in
            if error != nil {
              print("lexical model download failed: \(error!)")
            }

            print("download succeeded, installing lexical model...")
            let installed = self.lexicalModelRepo.installLexicalModel(package: package!, keyboardState: self.keyboardState!, modelId: lexicalModelId)
            if (installed) {
              self.keyboardState?.selectDictionary(lexicalModel: thisDictionary)
              self.settingsRepo.saveKeyboardState(state: self.keyboardState!)
              self.updateDictionarySettingsState(animated: true)
            }
          }
        })
        alert.addAction(installAction)
        
        let cancelAction = UIAlertAction(title: "Cancel", style: .cancel) { (action) in
          switchCell.detailSwitch.isOn = false
        }
        alert.addAction(cancelAction)
        self.present(alert, animated: true, completion: nil)
      } else {
        if self.lexicalModelRepo.disableLexicalModel(keyboardState: self.keyboardState!, modelId: lexicalModelId) {
          self.keyboardState?.clearDictionary()
          self.settingsRepo.saveKeyboardState(state: self.keyboardState!)
          self.updateDictionarySettingsState(animated: true)
        }
      }
    }

    switchCell.configure(label: thisDictionary.name, on: modelEnabled,  available: self.keyboardState!.canSelectDictionary(),
                         callback: dictionaryAction)
    return switchCell
  }

  func configureVersionCell() -> UITableViewCell {
    let versionCell = tableView.dequeueReusableCell(withIdentifier: attributeCellIdentifier)
    versionCell!.textLabel!.text = versionLabel
    let versionText = self.keyboardState?.version
    versionCell!.detailTextLabel!.text = versionText
    return versionCell!
  }
  
  func updateCellSwitchAvailability(index: IndexPath, available: Bool) {
    if let cell:KeyboardDetailCell = self.tableView.cellForRow(at: index) as? KeyboardDetailCell {
        cell.updateSwitchAvailability(available: available)
    }
  }

  func updateCellSwitchState(index: IndexPath, on: Bool, animated: Bool, available: Bool) {
    if let cell:KeyboardDetailCell = self.tableView.cellForRow(at: index) as? KeyboardDetailCell {
      cell.updateSwitchState(on: on, animated: animated, available: available)
    }
  }

  func updateDictionarySettingsState(animated: Bool) {
    let keyboardsIndex = IndexPath(row: keyboardsRow, section: keyboardsSection)
    self.updateCellSwitchState(index: keyboardsIndex,
                               on: self.keyboardState!.isActive,
                               animated: animated,
                               available: true)

    for rowNumber in 0...max(0, lexicalModels.count-1) {
      let dictionaryIndex = IndexPath(row: rowNumber, section: dictionarySection)
      self.updateCellSwitchState(index: dictionaryIndex,
                                 on: self.keyboardState!.isDictionarySelected(),
                             animated: animated,
                             available: self.keyboardState!.canSelectDictionary())
    }
    
    let predictionIndex = IndexPath(row: predictionsRow, section: languageSettingsSection)
    self.updateCellSwitchState(index: predictionIndex,
                               on: self.keyboardState!.suggestPredictions,
                               animated: animated,
                               available: self.keyboardState!.canSuggestPredictions())
    let correctionIndex = IndexPath(row: correctionsRow, section: languageSettingsSection)
    self.updateCellSwitchState(index: correctionIndex,
                               on: self.keyboardState!.suggestCorrections,
                               animated: animated,
                               available: self.keyboardState!.canSuggestCorrections())
  }
  
  /*
  private func lexicalModelDownloadStarted() {
    log.info("lexicalModelDownloadStarted: KeyboardDetailViewController")
    view.isUserInteractionEnabled = false
    
    navigationItem.setHidesBackButton(true, animated: true)
    navigationController?.setToolbarHidden(false, animated: true)
  }
  
  private func lexicalModelDownloadFailed() {
    log.info("lexicalModelDownloadFailed: KeyboardDetailViewController")
    view.isUserInteractionEnabled = true
    navigationItem.setHidesBackButton(false, animated: true)
  }
  
  private func lexicalModelDownloadCompleted() {
    log.info("lexicalModelDownloadCompleted KeyboardDetailViewController")
    
    // Actually used now.
    view.isUserInteractionEnabled = true
    navigationItem.leftBarButtonItem?.isEnabled = true
    if navigationItem.rightBarButtonItem != nil {
      navigationItem.rightBarButtonItem?.isEnabled = true
    }
    
    //navigationController?.popToRootViewController(animated: true)
  }
  */
}
