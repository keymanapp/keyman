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

  var keyboardState: FVKeyboardState? = nil
  var delegate: RefreshKeyboardCheckmark? = nil
  var lexicalModels: [FVLexicalModel] = []
    
  override func viewDidLoad() {
    super.viewDidLoad()

    // get lexical models to display in dictionary section
    if let languageTag = self.keyboardState?.languageTag {
      lexicalModels = FVLexicalModels.getAvailableLexicalModels(languageTag: languageTag)
    }
  }

  override func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)

    // before the view appears, update the initial enabled/disabled state of dependent switches
    updateSwitchDetailAvailability(available: self.keyboardState!.isEnabled)
  }
  

  /*
   * Used to display the values for the keyboard that was tapped and caused the segue to the detail view.
   */
  func configure(delegate: RefreshKeyboardCheckmark, keyboard: FVKeyboardState) {
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
    let actionCallBack: Callback = { (enable) in
      self.keyboardState?.isEnabled = enable
      // update the enabled/disabled state of dependent switches
      self.updateSwitchDetailAvailability(available: enable)
      self.delegate?.refreshCheckmark()
    }
    switchCell.configure(label: (self.keyboardState?.name)!, enabled: self.keyboardState!.isEnabled,
                         callback: actionCallBack)
    return switchCell
  }

  func configurePredictionsSwitchCell() -> UITableViewCell {
    let switchCell = tableView.dequeueReusableCell(withIdentifier: switchCellIdentifier) as! KeyboardDetailCell
    let actionCallBack: Callback = { (enable) in
      self.keyboardState?.suggestPredictions = enable
    }
    switchCell.configure(label: predictionLabel, enabled: self.keyboardState!.suggestPredictions,
                         callback: actionCallBack)
    return switchCell
  }

  func configureCorrectionsSwitchCell() -> UITableViewCell {
    let switchCell = tableView.dequeueReusableCell(withIdentifier: switchCellIdentifier) as! KeyboardDetailCell
    let actionCallBack: Callback = { (enable) in
     self.keyboardState?.suggestCorrections = enable
    }
    switchCell.configure(label: correctionLabel, enabled: self.keyboardState!.suggestCorrections,
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
    let availableModelName = self.lexicalModels.first!.name
    
    let savedModel = (self.keyboardState?.selectedDictionary)!
    let modelEnabled = availableModelName.elementsEqual(savedModel)

    let actionCallBack: Callback = { (enable) in
      if enable {
        self.keyboardState?.selectedDictionary = availableModelName;
        do {
          let url = try URL(string: String(contentsOf: self.lexicalModels.first!.packageUrl))
          FVLexicalModels.downloadModel(keyboard: self.keyboardState!)
        }
        catch {
          print("FVLexicalModels.downloadModel error")
        }
      }
    }

    switchCell.configure(label: self.lexicalModels.first!.name, enabled: modelEnabled,
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
  
  func updateSwitchCell(index: IndexPath, available: Bool) {
    if let cell:KeyboardDetailCell = self.tableView.cellForRow(at: index) as? KeyboardDetailCell {
      let detailSwitch = cell.detailSwitch!
      if available {
        detailSwitch.isEnabled = true;
        detailSwitch.alpha = 1.0;
      } else {
        detailSwitch.isEnabled = false;
        detailSwitch.alpha = 0.5;
      }
    }
  }

  func updateSwitchDetailAvailability(available: Bool) {
    let predictionIndex = IndexPath(row: predictionsRow, section: languageSettingsSection)
    self.updateSwitchCell(index: predictionIndex, available: available)
    let correctionIndex = IndexPath(row: correctionsRow, section: languageSettingsSection)
    self.updateSwitchCell(index: correctionIndex, available: available)
    
    for rowNumber in 0...max(0, lexicalModels.count-1) {
      let dictionaryIndex = IndexPath(row: rowNumber, section: dictionarySection)
      self.updateSwitchCell(index: dictionaryIndex, available: available)
    }
  }
}
