//
//  KeyboardNameTableViewCell.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-09-12.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit

private let cellSelectedTag = 4576 // arbitrary number

class KeyboardNameTableViewCell: UITableViewCell, UIAlertViewDelegate {
  var indexPath: IndexPath?

  override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
    super.init(style: style, reuseIdentifier: reuseIdentifier)
    indentationLevel = 0
  }

  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  func setKeyboardState(_ kbState: KeyboardState, selected isSelected: Bool,
                        defaultAccessoryType accessoryType: UITableViewCell.AccessoryType) {
    if kbState != .downloading {
      accessoryView = nil
    }
    self.accessoryType = accessoryType

    let selectedView = viewWithTag(cellSelectedTag)
    if isSelected {
      if selectedView == nil {
        let view = UIView(frame: CGRect(x: 0, y: 0, width: bounds.width, height: bounds.height))
        view.tag = cellSelectedTag
        view.backgroundColor = UIColor.green
        view.autoresizingMask = UIView.AutoresizingMask.flexibleWidth.union(.flexibleHeight)
        addSubview(view)
      }
    } else {
      selectedView?.removeFromSuperview()
    }
  }
}
