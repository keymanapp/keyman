/*
 * KeyboardDetailCell.swift
 * FirstVoices app
 *
 * License: MIT
 *
 * Copyright © 2022 FirstVoices.
 *
 * Created by Shawn Schantz on 2022-01-13.
 *
 * Subclass of UITableViewCell that contains a label and switch control.
 * Executes callback closure when switch is toggled.
 *
 */

import UIKit

typealias Callback = (Bool) -> Void

class KeyboardDetailCell: UITableViewCell {
  var callback: Callback? = nil
  
  @IBOutlet weak var switchLabel: UILabel!
  @IBOutlet weak var detailSwitch: UISwitch!
  
  private var title: String = ""
  
  @IBAction func didToggle(_ sender: Any) {
    callback!(detailSwitch.isOn)
  }
  
  func configure(label: String, on: Bool, available: Bool, callback: @escaping Callback) {
    switchLabel.text = label
    self.title = label
    self.callback = callback
    detailSwitch.setOn(on, animated: false)
    self.updateSwitchAvailability(available: available)
  }
  
  func updateSwitchAvailability(available: Bool) {
    if (available) {
      detailSwitch.alpha = 1.0;
    } else {
      detailSwitch.alpha = 0.5;
    }
    detailSwitch.isEnabled = available
  }

  func updateSwitchState(on: Bool, animated: Bool, available: Bool) {
    self.updateSwitchAvailability(available: available)
    detailSwitch.setOn(on, animated: animated)
  }
}
