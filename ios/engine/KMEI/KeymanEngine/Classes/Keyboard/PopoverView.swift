//
//  PopoverView.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-09-11.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit

class PopoverView: UIView {
  let strokeWidth: CGFloat = 2.0
  let arrowWidth: CGFloat = 21.0
  let arrowHeight: CGFloat = 7.0
  let borderRadius: CGFloat = 5.0
  var borderColor = Colors.popupBorder
  
  // A default color.  This class's current only use will override these values.
  private var bgColor = Colors.systemBackground
  var backgroundColor2 = Colors.systemBackground
  private var _arrowPosX: CGFloat = 0.0
  
  override init(frame: CGRect) {
    super.init(frame: frame)
    super.backgroundColor = UIColor.clear
  }
  
  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
  
  override func draw(_ rect: CGRect) {
    guard let context = UIGraphicsGetCurrentContext() else {
      return
    }
    UIGraphicsPushContext(context)
    let currentFrame = bounds
    
    context.setLineJoin(CGLineJoin.round)
    context.setLineWidth(strokeWidth)
    context.setStrokeColor(borderColor.cgColor)
    context.setFillColor(bgColor.cgColor)
    
    context.beginPath()
    let arrowPosY: CGFloat = currentFrame.size.height - (arrowHeight + strokeWidth)
    context.move(to: CGPoint(x: borderRadius + strokeWidth, y: arrowPosY))
    context.addLine(to: CGPoint(x: arrowPosX - arrowWidth / 2.0, y: arrowPosY))
    context.addLine(to: CGPoint(x: arrowPosX, y: currentFrame.size.height - strokeWidth))
    context.addLine(to: CGPoint(x: arrowPosX + arrowWidth / 2.0, y: arrowPosY))
    context.addArc(tangent1End: CGPoint(x: currentFrame.size.width - strokeWidth,
                                        y: arrowPosY),
                   tangent2End: CGPoint(x: currentFrame.size.width - strokeWidth,
                                        y: strokeWidth),
                   radius: borderRadius - strokeWidth)
    context.addArc(tangent1End: CGPoint(x: currentFrame.size.width - strokeWidth,
                                        y: strokeWidth),
                   tangent2End: CGPoint(x: round(currentFrame.size.width / 2.0 +
                                                 arrowWidth / 2.0) - strokeWidth,
                                        y: strokeWidth),
                   radius: borderRadius - strokeWidth)
    context.addArc(tangent1End: CGPoint(x: strokeWidth, y: strokeWidth),
                   tangent2End: CGPoint(x: strokeWidth, y: arrowPosY),
                   radius: borderRadius - strokeWidth)
    context.addArc(tangent1End: CGPoint(x: strokeWidth, y: arrowPosY),
                   tangent2End: CGPoint(x: currentFrame.size.width - strokeWidth,
                                        y: arrowPosY),
                   radius: borderRadius - strokeWidth)
    context.closePath()
    context.drawPath(using: CGPathDrawingMode.fillStroke)
    
    context.beginPath()
    context.move(to: CGPoint(x: borderRadius + strokeWidth, y: arrowPosY))
    context.addLine(to: CGPoint(x: arrowPosX - arrowWidth / 2.0, y: arrowPosY))
    context.addLine(to: CGPoint(x: arrowPosX, y: currentFrame.size.height - strokeWidth))
    context.addLine(to: CGPoint(x: arrowPosX + arrowWidth / 2.0, y: arrowPosY))
    context.addArc(tangent1End: CGPoint(x: currentFrame.size.width - strokeWidth,
                                        y: arrowPosY),
                   tangent2End: CGPoint(x: currentFrame.size.width - strokeWidth,
                                        y: strokeWidth),
                   radius: borderRadius - strokeWidth)
    context.addArc(tangent1End: CGPoint(x: currentFrame.size.width - strokeWidth,
                                        y: strokeWidth),
                   tangent2End: CGPoint(x: round(currentFrame.size.width / 2.0 +
                                                 arrowWidth / 2.0) - strokeWidth,
                                        y: strokeWidth),
                   radius: borderRadius - strokeWidth)
    context.addArc(tangent1End: CGPoint(x: strokeWidth, y: strokeWidth),
                   tangent2End: CGPoint(x: strokeWidth, y: arrowPosY),
                   radius: borderRadius - strokeWidth)
    context.addArc(tangent1End: CGPoint(x: strokeWidth, y: arrowPosY),
                   tangent2End: CGPoint(x: currentFrame.size.width - strokeWidth,
                                        y: arrowPosY),
                   radius: borderRadius - strokeWidth)
    context.closePath()
    context.clip()
    
    let colorSpace = CGColorSpaceCreateDeviceRGB()
    let gradientColors = [bgColor.cgColor, backgroundColor2.cgColor] as CFArray
    let gradientLocations: [CGFloat] = [0, 1]
    let gradient = CGGradient(colorsSpace: colorSpace, colors: gradientColors,
                              locations: gradientLocations)!
    
    let startPoint = CGPoint(x: rect.midX, y: rect.minY)
    let endPoint = CGPoint(x: rect.midX, y: rect.maxY)
    
    context.drawLinearGradient(gradient, start: startPoint, end: endPoint, options: [])
    UIGraphicsPopContext()
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
  
  var arrowPosX: CGFloat {
    get {
      return _arrowPosX
    }
    
    set(posX) {
      let currentFrame = bounds
      if posX < (arrowWidth / 2.0 + borderRadius + strokeWidth) {
        _arrowPosX = arrowWidth / 2.0 + borderRadius + strokeWidth
      } else if posX > (currentFrame.size.width - arrowWidth / 2.0 - borderRadius - strokeWidth) {
        _arrowPosX = currentFrame.size.width - arrowWidth / 2.0 - borderRadius - strokeWidth
      } else {
        _arrowPosX = posX
      }
      setNeedsDisplay()
    }
  }
}
