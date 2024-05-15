//
//  DropDownListView.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-07.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit
import KeymanEngine

class DropDownListView: UIView {
  init(listItems items: [UIBarButtonItem], itemSize size: CGSize, position pos: CGPoint) {
    let frameHeight = items.count > 0 ? size.height * CGFloat(items.count) : size.height
    let frame = CGRect(x: pos.x, y: pos.y, width: size.width, height: frameHeight)
    super.init(frame: frame)
    
    backgroundColor = UIColor.clear
    let dropDownView = DropDownView(frame: CGRect.zero)
    let arrowHeight = dropDownView.arrowHeight
    dropDownView.frame = CGRect(x: 0, y: -arrowHeight, width: frame.size.width,
                                height: frame.size.height + arrowHeight)
    dropDownView.arrowPosX = frame.size.width - 29
    addSubview(dropDownView)
    
    let count: Int = items.count
    let w: CGFloat = frame.width
    let h: CGFloat = size.height
    let x: CGFloat = 0
    var y: CGFloat = 0
    
    for (index, item) in items.enumerated() {
      let button = UIButton(type: .custom)
      button.setTitleColor(Colors.labelNormal, for: .normal)
      button.setTitleColor(Colors.labelHighlighted, for: .highlighted)
      button.frame = CGRect(x: x, y: y, width: w, height: h)
      button.setTitle(item.title, for: .normal)
      addSubview(button)
      if let subviews = item.customView?.subviews, subviews.count > 0,
         let btn = subviews[0] as? UIButton {
        let icon = btn.image(for: .normal)
        let iconSelected = btn.image(for: .highlighted)
        button.setImage(icon, for: .normal)
        button.setImage(iconSelected, for: .highlighted)
        button.contentHorizontalAlignment = .left
        button.contentEdgeInsets = UIEdgeInsets(top: 0, left: 8, bottom: 0, right: 0)
        if let iconWidth = icon?.size.width {
          button.titleEdgeInsets = UIEdgeInsets(top: 0, left: h - iconWidth, bottom: 0, right: 0)
        }
        for target in btn.allTargets {
          let actions = btn.actions(forTarget: target, forControlEvent: .touchUpInside)
          actions?.forEach {
            button.addTarget(target, action: NSSelectorFromString($0), for: .touchUpInside)
          }
        }
      }
      
      if index < count - 1 {
        let seperator = UIView(frame: CGRect(x: x + 1, y: y + h, width: w - 2, height: 1))
        seperator.backgroundColor = Colors.listSeparator
        addSubview(seperator)
      }
      y += h + 1
    }
    
  }
  
  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
}
