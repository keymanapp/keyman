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

  override init(style: UITableViewCellStyle, reuseIdentifier: String?) {
    super.init(style: style, reuseIdentifier: reuseIdentifier)
    indentationLevel = 0
  }

  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  func setKeyboardState(_ kbState: KeyboardState, selected isSelected: Bool,
                        defaultAccessoryType accessoryType: UITableViewCellAccessoryType) {
    if kbState != .downloading {
      accessoryView = nil
    }
    self.accessoryType = accessoryType

    let isSelectedView = viewWithTag(cellSelectedTag)
    if isSelected {
      if isSelectedView == nil {
        let view = UIView(frame: CGRect(x: 0, y: 0, width: bounds.width, height: bounds.height))
        view.tag = cellSelectedTag
        view.backgroundColor = UIColor.green
        view.autoresizingMask = UIViewAutoresizing.flexibleWidth.union(.flexibleHeight)
        addSubview(view)
      }
    } else {
      isSelectedView?.removeFromSuperview()
    }
  }
}
