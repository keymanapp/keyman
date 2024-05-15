//
//  SubKeysView.swift
//  KMEI
//
//  Created by Gabriel Wong on 2017-09-28.
//  Copyright © 2017 SIL International. All rights reserved.
//

import  UIKit

class SubKeysView: UIView {
  private let borderRadius: CGFloat = 5.0
  private let strokeWidth: CGFloat = 1.0
  private var baseKeyYDelta: CGFloat = 0.0
  
  private var keyFrame = CGRect.zero
  private var adjX: CGFloat = 0.0
  private var adjY: CGFloat = 0.0
  private var rows: Int = 0
  
  private let scale = UIScreen.main.scale
  private let bgColor = Colors.keyboardBackground
  private let bgColor2 = Colors.keyboardBackground
  private let borderColor = Colors.popupBorder
  
  let containerView: UIView
  
  init(keyFrame frame: CGRect, subKeys: [UIButton]) {
    let isSystemKeyboard = Manager.shared.isSystemKeyboard
    var isPortrait = true
    if isSystemKeyboard {
      isPortrait = InputViewController.isPortrait
    } else {
      isPortrait = UIDevice.current.orientation.isPortrait
    }
    
    let screenRect = UIScreen.main.bounds
    let screenWidth = screenRect.width
    let screenHeight = screenRect.height
    
    keyFrame = frame.insetBy(dx: 0, dy: 0)
    var buttonWidth: CGFloat
    var buttonHeight: CGFloat
    let marginX: CGFloat = 4.0
    let marginY: CGFloat = 3.0
    
    let maxButtonSize = CGSize(width: keyFrame.width * 0.9, height: keyFrame.height * 0.9)
    let minButtonSize: CGSize
    if UIDevice.current.userInterfaceIdiom == .pad {
      if isPortrait {
        minButtonSize = CGSize(width: keyFrame.width * 0.65, height: keyFrame.height * 0.65)
      } else {
        minButtonSize = CGSize(width: keyFrame.width * 0.55, height: keyFrame.height * 0.55)
      }
    } else {
      if isPortrait {
        minButtonSize = CGSize(width: keyFrame.width * 0.75, height: keyFrame.height * 0.75)
      } else {
        minButtonSize = CGSize(width: keyFrame.width * 0.675, height: keyFrame.height * 0.675)
      }
    }
    
    let kbHeight = Manager.shared.inputViewController.kmwHeight
    let maxContainerHeight = (screenHeight - kbHeight) + keyFrame.origin.y - strokeWidth
    
    var columns = Int((screenWidth - marginX) / (maxButtonSize.width + marginX))
    rows = subKeys.count / columns
    if subKeys.count % columns != 0 {
      rows += 1
    }
    
    if subKeys.count % rows == 0 {
      columns = subKeys.count / rows
    } else {
      let s = (columns * rows - subKeys.count) / 2
      columns -= s / (rows - 1)
    }
    
    buttonWidth = maxButtonSize.width
    buttonHeight = (maxContainerHeight - CGFloat(rows + 1) * marginY) / CGFloat(rows)
    buttonHeight = CGFloat.minimum(buttonHeight, maxButtonSize.height)
    buttonHeight = CGFloat.maximum(buttonHeight, minButtonSize.height)
    
    let cx: CGFloat = 1.5
    let containerWidth = (columns < 2 ? cx : CGFloat(columns)) * (buttonWidth + marginX) + marginX
    let containerHeight = (CGFloat(rows) * (buttonHeight + marginY)) + marginY
    let containerFrame = CGRect(x: strokeWidth, y: strokeWidth, width: containerWidth, height: containerHeight)
    let viewWidth = containerWidth + 2 * strokeWidth
    let baseHeight = keyFrame.size.height + marginY
    let viewHeight = baseHeight + containerHeight - marginY + strokeWidth
    var viewPosX = keyFrame.origin.x - (viewWidth - keyFrame.size.width) / 2.0
    var viewPosY = keyFrame.origin.y - (viewHeight - keyFrame.size.height + strokeWidth)
    
    // Ensure that the system keyboard doesn't try to display off the top of the keyboard's view area.
    if viewPosY < 0 && isSystemKeyboard {
      baseKeyYDelta = viewPosY // We'll need this to adjust the visualization.
      viewPosY = 0
    }
    
    adjX = 0
    adjY = 0
    
    if viewPosX < 0 {
      if (keyFrame.origin.x - borderRadius * 1.25) < 0 {
        adjX = keyFrame.origin.x - viewPosX
        viewPosX = keyFrame.origin.x
      } else {
        adjX = -viewPosX
        viewPosX = 0
      }
    } else if (viewPosX + viewWidth) > screenWidth {
      if ((keyFrame.origin.x + keyFrame.size.width) + borderRadius * 1.25) > screenWidth {
        adjX = viewPosX - keyFrame.origin.x
        viewPosX = keyFrame.origin.x + 2 * adjX
      } else {
        adjX = (screenWidth - viewWidth) - viewPosX
        viewPosX = adjX + viewPosX
      }
    }
    containerView = UIView(frame: containerFrame)
    containerView.backgroundColor = UIColor.clear
    
    super.init(frame: CGRect(x: viewPosX, y: viewPosY, width: viewWidth, height: viewHeight))
    
    super.backgroundColor = UIColor.clear
    //isUserInteractionEnabled = false
    
    addSubview(containerView)
    let fontSize = buttonHeight * (UIDevice.current.userInterfaceIdiom == .pad ? 0.4 : 0.5)
    
    for button in subKeys {
      let i = button.tag
      let buttonRow = CGFloat(i / columns)
      let buttonFrame = CGRect(x: marginX + CGFloat(i % columns) * (buttonWidth + marginX),
                               y: (buttonRow * buttonHeight) + (buttonRow + 1) * marginY,
                               width: buttonWidth, height: buttonHeight)
      button.frame = buttonFrame
      if subKeys.count == 1 {
        button.center = containerView.center
      }
      
      button.titleLabel?.minimumScaleFactor = 0.25
      button.titleLabel?.adjustsFontSizeToFitWidth = true
      button.titleLabel?.font = button.titleLabel?.font.withSize(fontSize)
      containerView.addSubview(button)
    }
    
  }
  
  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
  
  override func draw(_ rect: CGRect) {
    drawKeyPreviewUp(rect)
  }
  
  private func drawKeyPreviewUp(_ rect: CGRect) {
    let keyWidth = keyFrame.size.width
    let keyHeight = keyFrame.size.height + adjY
    let viewWidth = rect.size.width
    let viewHeight = rect.size.height
    
    guard let context = UIGraphicsGetCurrentContext() else {
      return
    }
    UIGraphicsPushContext(context)
    context.setLineJoin(.round)
    context.setLineWidth(strokeWidth * 2)
    context.setStrokeColor(borderColor.cgColor)
    context.setFillColor(bgColor.cgColor)
    
    let keyLeftUnrounded = (viewWidth - keyWidth) / 2.0 + strokeWidth - adjX
    let keyRightUnrounded = (viewWidth - keyWidth) / 2.0 + keyWidth - strokeWidth - adjX
    var keyLeft = (keyLeftUnrounded * 10).rounded(.up) / 10
    var keyRight = (keyRightUnrounded * 10).rounded(.down) / 10
    
    let midY = viewHeight - keyHeight
    var viewLeft = strokeWidth
    var viewRight = viewWidth - strokeWidth
    let viewTop = strokeWidth
    
    var viewBottom = viewHeight - strokeWidth * 0.75 - (CGFloat(rows - 1) * 0.25)
    viewBottom += baseKeyYDelta
    
    let showBaseKeyPopup = (viewBottom > midY)
    
    if scale == 3.0 {
      viewBottom -= CGFloat(3 - rows) * 0.4
    } else if scale == 2.0 {
      viewBottom -= CGFloat(3 - rows) * 0.1
    }
    
    let viewMid = viewWidth / 2.0
    let r0 = borderRadius - strokeWidth
    let r1 = borderRadius - strokeWidth
    let r2 = borderRadius * 1.25 - strokeWidth
    
    if viewLeft.rounded() == keyLeft.rounded() || viewLeft.rounded(.down) == keyLeft.rounded(.down) {
      keyLeft = CGFloat.minimum(viewLeft, keyLeft)
      viewLeft = keyLeft
    }
    
    if viewRight.rounded() == keyRight.rounded() || viewRight.rounded(.down) == keyRight.rounded(.down) {
      keyRight = CGFloat.maximum(viewRight, keyRight)
      viewRight = keyRight
    }
    
    context.beginPath()
    context.move(to: CGPoint(x: viewMid, y: viewTop))
    context.addArc(tangent1End: CGPoint(x: viewLeft, y: viewTop),
                   tangent2End: CGPoint(x: viewLeft, y: midY), radius: r1)
    if showBaseKeyPopup {
      context.addArc(tangent1End: CGPoint(x: viewLeft, y: midY),
                     tangent2End: CGPoint(x: keyLeft, y: midY), radius: r1)
      context.addArc(tangent1End: CGPoint(x: keyLeft, y: midY),
                     tangent2End: CGPoint(x: keyLeft, y: viewBottom), radius: r2)
      context.addArc(tangent1End: CGPoint(x: keyLeft, y: viewBottom),
                     tangent2End: CGPoint(x: keyRight, y: viewBottom), radius: r0)
      context.addArc(tangent1End: CGPoint(x: keyRight, y: viewBottom),
                     tangent2End: CGPoint(x: keyRight, y: midY), radius: r0)
      context.addArc(tangent1End: CGPoint(x: keyRight, y: midY),
                     tangent2End: CGPoint(x: viewRight, y: midY), radius: r2)
    } else {
      context.addArc(tangent1End: CGPoint(x: viewLeft, y: midY),
                     tangent2End: CGPoint(x: viewRight, y: midY), radius: r0)
    }
    context.addArc(tangent1End: CGPoint(x: viewRight, y: midY),
                   tangent2End: CGPoint(x: viewRight, y: viewTop), radius: r1)
    context.addArc(tangent1End: CGPoint(x: viewRight, y: viewTop),
                   tangent2End: CGPoint(x: viewMid, y: viewTop), radius: r1)
    context.closePath()
    context.drawPath(using: .stroke)
    
    context.beginPath()
    context.move(to: CGPoint(x: viewMid, y: viewTop))
    context.addArc(tangent1End: CGPoint(x: viewLeft, y: viewTop),
                   tangent2End: CGPoint(x: viewLeft, y: midY), radius: r1)
    if showBaseKeyPopup {
      context.addArc(tangent1End: CGPoint(x: viewLeft, y: midY),
                     tangent2End: CGPoint(x: keyLeft, y: midY), radius: r1)
      context.addArc(tangent1End: CGPoint(x: keyLeft, y: midY),
                     tangent2End: CGPoint(x: keyLeft, y: viewBottom), radius: r2)
      context.addArc(tangent1End: CGPoint(x: keyLeft, y: viewBottom),
                     tangent2End: CGPoint(x: keyRight, y: viewBottom), radius: r0)
      context.addArc(tangent1End: CGPoint(x: keyRight, y: viewBottom),
                     tangent2End: CGPoint(x: keyRight, y: midY), radius: r0)
      context.addArc(tangent1End: CGPoint(x: keyRight, y: midY),
                     tangent2End: CGPoint(x: viewRight, y: midY), radius: r2)
    } else {
      context.addArc(tangent1End: CGPoint(x: viewLeft, y: midY),
                     tangent2End: CGPoint(x: viewRight, y: midY), radius: r0)
    }
    context.addArc(tangent1End: CGPoint(x: viewRight, y: midY),
                   tangent2End: CGPoint(x: viewRight, y: viewTop), radius: r1)
    context.addArc(tangent1End: CGPoint(x: viewRight, y: viewTop),
                   tangent2End: CGPoint(x: viewMid, y: viewTop), radius: r1)
    context.closePath()
    context.clip()
    
    let colorSpace = CGColorSpaceCreateDeviceRGB()
    let gradientColors = [bgColor.cgColor, bgColor2.cgColor] as CFArray
    let gradientLocations: [CGFloat] = [0, 1]
    let gradient = CGGradient(colorsSpace: colorSpace, colors: gradientColors, locations: gradientLocations)!
    
    let startPoint = CGPoint(x: rect.midX, y: rect.minY)
    let endPoint = CGPoint(x: rect.midX, y: rect.maxY)
    
    context.drawLinearGradient(gradient, start: startPoint, end: endPoint, options: [])
    UIGraphicsPopContext()
  }
}
