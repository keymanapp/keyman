//
//  KeyPreviewView.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-05.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit

class KeyPreviewView: UIView {
  private let keyFrame: CGRect
  private let strokeWidth: CGFloat = 1.0
  private let borderRadius: CGFloat = 5.0
  private let adjX: CGFloat
  private let adjY: CGFloat
  
  private let borderColor = Colors.popupBorder
  private var bgColor = Colors.keyboardBackground
  private let bgColor2 = Colors.keyboardBackground
  private let label: UILabel
  
  override init(frame: CGRect) {
    let isPortrait: Bool
    if Manager.shared.isSystemKeyboard {
      isPortrait = InputViewController.isPortrait
    } else {
      isPortrait = UIDevice.current.orientation.isPortrait
    }
    
    keyFrame = frame
    let viewWidth = keyFrame.width * (isPortrait ? 1.6 : 1.3)
    let viewHeight = keyFrame.size.height * 2.1
    
    var viewPosX = keyFrame.origin.x - (viewWidth - keyFrame.width) / 2.0
    let viewPosY = keyFrame.origin.y - (viewHeight - keyFrame.height)
    
    adjY = 0
    
    if viewPosX < 0 {
      adjX = keyFrame.origin.x - viewPosX
      viewPosX = keyFrame.origin.x
    } else if (viewPosX + viewWidth) > UIScreen.main.bounds.width {
      adjX = viewPosX - keyFrame.origin.x
      viewPosX = keyFrame.origin.x + 2 * adjX
    } else {
      adjX = 0
    }
    
    let m = strokeWidth * 2
    let fontSize = viewHeight * 0.35
    let labelFrame = CGRect(x: m, y: m, width: viewWidth - 2 * m, height: viewHeight * 0.5 - m)
    label = UILabel(frame: labelFrame)
    label.numberOfLines = 1
    label.minimumScaleFactor = 0.25
    label.adjustsFontSizeToFitWidth = true
    label.textAlignment = .center
    label.font = UIFont.systemFont(ofSize: fontSize)
    label.shadowColor = UIColor.lightText
    label.shadowOffset = CGSize(width: 1.0, height: 1.0)
    label.backgroundColor = UIColor.clear
    label.textColor = Colors.keyText
    label.text = ""
    
    super.init(frame: CGRect(x: viewPosX, y: viewPosY, width: viewWidth, height: viewHeight))
    
    super.backgroundColor = UIColor.clear
    isUserInteractionEnabled = false
    addSubview(label)
  }
  
  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
  
  override func draw(_ rect: CGRect) {
    drawKeyPreviewUp(rect)
  }
  
  private func drawKeyPreviewUp(_ rect: CGRect) {
    let keyWidth = keyFrame.width
    let viewWidth = rect.width
    let viewHeight = rect.height
    
    guard let context = UIGraphicsGetCurrentContext() else {
      return
    }
    UIGraphicsPushContext(context)
    context.setLineJoin(.round)
    context.setLineWidth(strokeWidth * 2)
    context.setStrokeColor(borderColor.cgColor)
    context.setFillColor(bgColor.cgColor)
    
    let keyLeft = (viewWidth - keyWidth) / 2.0 + strokeWidth - adjX
    let keyRight = (viewWidth - keyWidth) / 2.0 + keyWidth - strokeWidth - adjX
    let midY1 = viewHeight * 3.0 / 5.0
    let midY2 = viewHeight / 2.0
    let viewLeft = strokeWidth
    let viewRight = viewWidth - strokeWidth
    let viewTop = strokeWidth
    let viewBottom = viewHeight - strokeWidth * 1.25
    let viewMid: CGFloat = viewWidth / 2.0
    let r0 = borderRadius - strokeWidth
    let r1 = borderRadius - strokeWidth
    let r2 = borderRadius * 1.25 - strokeWidth
    
    context.beginPath()
    context.move(to: CGPoint(x: viewMid, y: viewTop))
    context.addArc(tangent1End: CGPoint(x: viewLeft, y: viewTop),
                   tangent2End: CGPoint(x: viewLeft, y: midY2), radius: r1)
    context.addArc(tangent1End: CGPoint(x: viewLeft, y: midY2),
                   tangent2End: CGPoint(x: keyLeft, y: midY1), radius: r2)
    context.addArc(tangent1End: CGPoint(x: keyLeft, y: midY1),
                   tangent2End: CGPoint(x: keyLeft, y: viewBottom), radius: r2)
    context.addArc(tangent1End: CGPoint(x: keyLeft, y: viewBottom),
                   tangent2End: CGPoint(x: keyRight, y: viewBottom), radius: r0)
    context.addArc(tangent1End: CGPoint(x: keyRight, y: viewBottom),
                   tangent2End: CGPoint(x: keyRight, y: midY1), radius: r0)
    context.addArc(tangent1End: CGPoint(x: keyRight, y: midY1),
                   tangent2End: CGPoint(x: viewRight, y: midY2), radius: r2)
    context.addArc(tangent1End: CGPoint(x: viewRight, y: midY2),
                   tangent2End: CGPoint(x: viewRight, y: viewTop), radius: r2)
    context.addArc(tangent1End: CGPoint(x: viewRight, y: viewTop),
                   tangent2End: CGPoint(x: viewMid, y: viewTop), radius: r1)
    context.closePath()
    context.drawPath(using: .stroke)
    
    context.beginPath()
    context.move(to: CGPoint(x: viewMid, y: viewTop))
    context.addArc(tangent1End: CGPoint(x: viewLeft, y: viewTop),
                   tangent2End: CGPoint(x: viewLeft, y: midY2), radius: r1)
    context.addArc(tangent1End: CGPoint(x: viewLeft, y: midY2),
                   tangent2End: CGPoint(x: keyLeft, y: midY1), radius: r2)
    context.addArc(tangent1End: CGPoint(x: keyLeft, y: midY1),
                   tangent2End: CGPoint(x: keyLeft, y: viewBottom), radius: r2)
    context.addArc(tangent1End: CGPoint(x: keyLeft, y: viewBottom),
                   tangent2End: CGPoint(x: keyRight, y: viewBottom), radius: r0)
    context.addArc(tangent1End: CGPoint(x: keyRight, y: viewBottom),
                   tangent2End: CGPoint(x: keyRight, y: midY1), radius: r0)
    context.addArc(tangent1End: CGPoint(x: keyRight, y: midY1),
                   tangent2End: CGPoint(x: viewRight, y: midY2), radius: r2)
    context.addArc(tangent1End: CGPoint(x: viewRight, y: midY2),
                   tangent2End: CGPoint(x: viewRight, y: viewTop), radius: r2)
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
  
  func setLabelText(_ text: String) {
    label.text = text
  }
  
  func setLabelFont(_ fontName: String?) {
    let fontSize = label.font.pointSize
    if let fontName = fontName {
      label.font = UIFont(name: fontName, size: fontSize)
    } else {
      label.font = UIFont.systemFont(ofSize: fontSize)
    }
  }
  
  override var backgroundColor: UIColor? {
    get {
      return super.backgroundColor
    }
    
    set(color) {
      super.backgroundColor = UIColor.clear
      if let color = color {
        bgColor = color
      }
    }
  }
}
