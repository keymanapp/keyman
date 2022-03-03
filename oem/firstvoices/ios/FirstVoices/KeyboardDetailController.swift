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

let keyboardsSection = 0
let languageSettingsSection = 1
let dictionarySection = 2
let keyboardInfoSection = 3

let keyboardsHeader = "Available Keyboards"
let languageSettingsHeader = "Language Settings"
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
  var keyboardRepo = KeyboardRepository.shared
  var settingsRepo = KeyboardSettingsRepository.shared
  var lexicalModelRepo = LexicalModelRepository.shared

  var keyboardState: KeyboardState? = nil
  var delegate: RefreshKeyboardCheckmark? = nil
  var lexicalModels: [FVLexicalModel] = []
    
  override func viewDidLoad() {
    super.viewDidLoad()

    // get lexical models to display in dictionary section
    if let languageTag = self.keyboardState?.languageTag {
      lexicalModels = lexicalModelRepo.getAvailableLexicalModels(languageTag: languageTag)
    }
  }

  override func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)

    // TODO: remove test code
    examineUserDefaults()
  }
  /*
   * To help with debugging, check UserDefaults state upon entering view.
   */
  func examineUserDefaults() {
    // get the dictionary
    let sharedData: UserDefaults = FVShared.userDefaults()
    var userDefaultsKey = "UserPredictionEnablementSettings";
    if let settings = sharedData.dictionary(forKey: userDefaultsKey) as? [String : Bool] {
      for (languageId, value) in settings {
        print("current settings for \(userDefaultsKey), \(languageId) = \(value)")
      }
    }
    userDefaultsKey = "UserCorrectionEnablementSettings";
    if let settings = sharedData.dictionary(forKey: userDefaultsKey) as? [String : Bool] {
      for (languageId, value) in settings {
        print("current settings for \(userDefaultsKey), \(languageId) = \(value)")
      }
    }
  }

  /*
   * Used to display the values for the keyboard that was tapped and caused the segue to the detail view.
   */
  func configure(delegate: RefreshKeyboardCheckmark, keyboard: KeyboardState) {
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
        // TODO: number of dictionaries varies from zero to n
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
    let actionCallBack: Callback = { (on) in
      var updatedKeyboardState = false
      
      // call Keyman Engine to install or remove the keyboard
      if on {
        updatedKeyboardState = self.keyboardRepo.installKeyboard(keyboard: self.keyboardState!.definition)
      } else {
        updatedKeyboardState = self.keyboardRepo.removeKeyboard(keyboard: self.keyboardState!.definition)
      }
      
      if (updatedKeyboardState) {
        // update local settings storage
        self.settingsRepo.saveKeyboardState(keyboardId: self.keyboardState!.keyboardId, active: on)

        // update the on/off state of dependent switches
        self.updateDictionarySettingsAvailability()
        self.delegate?.refreshCheckmark()
      }
    }
    switchCell.configure(label: (self.keyboardState?.name)!, on: self.keyboardState!.isActive, available: true,
                         callback: actionCallBack)
    return switchCell
  }

  func configurePredictionsSwitchCell() -> UITableViewCell {
    let switchCell = tableView.dequeueReusableCell(withIdentifier: switchCellIdentifier) as! KeyboardDetailCell
    let actionCallBack: Callback = { (on) in
      if self.lexicalModelRepo.applyPredictionSettings(languageId: self.keyboardState!.languageTag, modelId: self.keyboardState!.lexicalModelId!, on: on) {
        
        self.keyboardState!.updatePredictions(on: on)
        self.updateDictionarySettingsState()
      }
    }
    switchCell.configure(label: predictionLabel, on: self.keyboardState!.suggestPredictions, available: self.keyboardState!.canSuggestPredictions(),
                         callback: actionCallBack)
    return switchCell
  }

  func configureCorrectionsSwitchCell() -> UITableViewCell {
    let switchCell = tableView.dequeueReusableCell(withIdentifier: switchCellIdentifier) as! KeyboardDetailCell
    let actionCallBack: Callback = { (on) in
      if (self.lexicalModelRepo.applyCorrectionSettings(languageId: self.keyboardState!.languageTag, modelId: self.keyboardState!.lexicalModelId!, on: on)) {
        self.keyboardState?.suggestCorrections = on
      }
    }
    switchCell.configure(label: correctionLabel, on: self.keyboardState!.suggestCorrections, available: self.keyboardState!.canSuggestCorrections(),
                         callback: actionCallBack)
    return switchCell
  }

  func configureNoDictionaryCell() -> UITableViewCell {
    let labelCell = tableView.dequeueReusableCell(withIdentifier: labelCellIdentifier)
    labelCell?.textLabel!.text = noDictionaryMessage
    return labelCell!
  }
  
  func configureDictionaryCell(index: Int) -> UITableViewCell {
    let switchCell = tableView.dequeueReusableCell(withIdentifier: switchCellIdentifier) as! KeyboardDetailCell
    // TODO: handle multiple dictionaries instead of getting first in array
    // TODO: handle empty array
    let lexicalModel = self.lexicalModels.first!
    
    let savedModel = (self.keyboardState?.lexicalModelName)!
    let modelEnabled = lexicalModel.name.elementsEqual(savedModel)

    let actionCallBack: Callback = { (on) in
      if on {
        let lexicalModelName = lexicalModel.name
        let lexicalModelId = lexicalModel.id
        let message = "As you type in your language, the FirstVoices dictionary will provide suggestions.\n\nWould you like to install this dictionary?"
        let alert = UIAlertController(title: "\(lexicalModelName)", message: message, preferredStyle: .alert)
        
        let installAction = UIAlertAction(title: NSLocalizedString("Install", comment: "Default action"), style: .default, handler: { _ in
          
          // install = download dictionary and set default settings
          let installed = self.lexicalModelRepo.installLexicalModel(keyboardState: self.keyboardState!, modelId: lexicalModelId)
          if (installed) {
            self.keyboardState?.selectDictionary(lexicalModel: lexicalModel)
          }

          // update the state of dependent switches
          self.updateDictionarySettingsState()
        })
        alert.addAction(installAction)
        
        let cancelAction = UIAlertAction(title: "Cancel", style: .cancel) { (action) in
          switchCell.detailSwitch.isOn = false
        }
        alert.addAction(cancelAction)
        self.present(alert, animated: true, completion: nil)
      } else {
        self.keyboardState?.clearDictionary()
        
        // update the state of dependent switches
        self.updateDictionarySettingsState()
      }
    }

    switchCell.configure(label: self.lexicalModels.first!.name, on: modelEnabled,  available: self.keyboardState!.canSelectDictionary(),
                         callback: actionCallBack)
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

  func updateDictionarySettingsState() {
    for rowNumber in 0...max(0, lexicalModels.count-1) {
      let dictionaryIndex = IndexPath(row: rowNumber, section: dictionarySection)
      self.updateCellSwitchState(index: dictionaryIndex,
                                 on: self.keyboardState!.isDictionarySelected(),
                             animated: true,
                             available: self.keyboardState!.canSelectDictionary())
    }
    
    let predictionIndex = IndexPath(row: predictionsRow, section: languageSettingsSection)
    self.updateCellSwitchState(index: predictionIndex,
                               on: self.keyboardState!.suggestPredictions,
                               animated: true,
                               available: self.keyboardState!.canSuggestPredictions())
    let correctionIndex = IndexPath(row: correctionsRow, section: languageSettingsSection)
    self.updateCellSwitchState(index: correctionIndex,
                               on: self.keyboardState!.suggestCorrections,
                               animated: true,
                               available: self.keyboardState!.canSuggestCorrections())
  }

  func updateDictionarySettingsAvailability() {
    for rowNumber in 0...max(0, lexicalModels.count-1) {
      let dictionaryIndex = IndexPath(row: rowNumber, section: dictionarySection)
      self.updateCellSwitchAvailability(index: dictionaryIndex, available: self.keyboardState!.canSelectDictionary())
    }
    
    let predictionIndex = IndexPath(row: predictionsRow, section: languageSettingsSection)
    self.updateCellSwitchAvailability(index: predictionIndex, available: self.keyboardState!.canSuggestPredictions())
    let correctionIndex = IndexPath(row: correctionsRow, section: languageSettingsSection)
    self.updateCellSwitchAvailability(index: correctionIndex, available: self.keyboardState!.canSuggestCorrections())
  }
}
