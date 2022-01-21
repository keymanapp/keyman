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
 * Description...
 */

import UIKit

class KeyboardDetailCell: UITableViewCell {
  @IBOutlet weak var switchLabel: UILabel!
  @IBOutlet weak var detailSwitch: UISwitch!
  
  func configure(label: String, enabled: Bool) {
    switchLabel.text = label
    detailSwitch.setOn(enabled, animated: false)
  }
}
