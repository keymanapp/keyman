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
  
  func configure(label: String, enabled: Bool, callback: @escaping Callback) {
    switchLabel.text = label
    self.title = label
    self.callback = callback
    detailSwitch.setOn(enabled, animated: false)
  }
}
