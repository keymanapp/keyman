//
//  DropDownView.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-07.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit

class DropDownView: UIView {
  let strokeWidth: CGFloat = 2.0
  let borderRadius: CGFloat = 5.0
  let arrowWidth: CGFloat = 21.0
  let arrowHeight: CGFloat = 7.0
  let borderColor = UIColor.lightGray

  var arrowX: CGFloat
  var bgColor = UIColor(white: 1.0, alpha: 1.0)
  var backgroundColor2 = UIColor(white: 1.0, alpha: 1.0)

  override init(frame: CGRect) {
    arrowX = frame.width / 2.0
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
    let arrowPosY = arrowHeight + strokeWidth
    context.move(to: CGPoint(x: borderRadius + strokeWidth, y: arrowPosY))
    context.addLine(to: CGPoint(x: arrowPosX - arrowWidth / 2.0, y: arrowPosY))
    context.addLine(to: CGPoint(x: arrowPosX, y: strokeWidth))
    context.addLine(to: CGPoint(x: arrowPosX + arrowWidth / 2.0, y: arrowPosY))
    context.addArc(tangent1End: CGPoint(x: currentFrame.width - strokeWidth, y: arrowPosY),
        tangent2End: CGPoint(x: currentFrame.width - strokeWidth, y: currentFrame.height - strokeWidth),
        radius: borderRadius - strokeWidth)
    context.addArc(tangent1End: CGPoint(x: currentFrame.width - strokeWidth,
            y: currentFrame.height - strokeWidth),
        tangent2End: CGPoint(x: round(currentFrame.width / 2.0 + arrowWidth / 2.0) - strokeWidth,
            y: currentFrame.height - strokeWidth),
        radius: borderRadius - strokeWidth)
    context.addArc(tangent1End: CGPoint(x: strokeWidth, y: currentFrame.height - strokeWidth),
        tangent2End: CGPoint(x: strokeWidth, y: arrowPosY),
        radius: borderRadius - strokeWidth)
    context.addArc(tangent1End: CGPoint(x: strokeWidth, y: arrowPosY),
        tangent2End: CGPoint(x: currentFrame.width - strokeWidth, y: arrowPosY),
        radius: borderRadius - strokeWidth)
    context.closePath()
    context.drawPath(using: CGPathDrawingMode.fillStroke)

    context.beginPath()
    context.move(to: CGPoint(x: borderRadius + strokeWidth, y: arrowPosY))
    context.addLine(to: CGPoint(x: arrowPosX - arrowWidth / 2.0, y: arrowPosY))
    context.addLine(to: CGPoint(x: arrowPosX, y: strokeWidth))
    context.addLine(to: CGPoint(x: arrowPosX + arrowWidth / 2.0, y: arrowPosY))
    context.addArc(tangent1End: CGPoint(x: currentFrame.width - strokeWidth, y: arrowPosY),
        tangent2End: CGPoint(x: currentFrame.width - strokeWidth, y: currentFrame.height - strokeWidth),
        radius: borderRadius - strokeWidth)
    context.addArc(tangent1End: CGPoint(x: currentFrame.width - strokeWidth,
            y: currentFrame.height - strokeWidth),
        tangent2End: CGPoint(x: round(currentFrame.width / 2.0 + arrowWidth / 2.0) - strokeWidth,
            y: currentFrame.height - strokeWidth),
        radius: borderRadius - strokeWidth)
    context.addArc(tangent1End: CGPoint(x: strokeWidth, y: currentFrame.height - strokeWidth),
        tangent2End: CGPoint(x: strokeWidth, y: arrowPosY),
        radius: borderRadius - strokeWidth)
    context.addArc(tangent1End: CGPoint(x: strokeWidth, y: arrowPosY),
        tangent2End: CGPoint(x: currentFrame.width - strokeWidth, y: arrowPosY),
        radius: borderRadius - strokeWidth)
    context.closePath()
    context.clip()

    let colorSpace = CGColorSpaceCreateDeviceRGB()
    let gradientColors = [bgColor.cgColor, backgroundColor2.cgColor] as CFArray
    let gradientLocations: [CGFloat] = [0, 1]
    guard let gradient = CGGradient(colorsSpace: colorSpace, colors: gradientColors,
        locations: gradientLocations) else {
      return
    }

    let startPoint = CGPoint(x: rect.midX, y: rect.minY)
    let endPoint = CGPoint(x: rect.midX, y: rect.maxY)

    context.drawLinearGradient(gradient, start: startPoint, end: endPoint, options: [])
    UIGraphicsPopContext()
  }

  override var backgroundColor: UIColor? {
    get {
      return UIColor.clear
    }

    set(color) {
      if let c = color {
        bgColor = c
      }
    }
  }

  var arrowPosX: CGFloat {
    get {
      return arrowX
    }

    set(x) {
      if x < (arrowWidth / 2.0 + borderRadius + strokeWidth) {
        arrowX = arrowWidth / 2.0 + borderRadius + strokeWidth
      } else if x > (bounds.width - arrowWidth / 2.0 - borderRadius - strokeWidth) {
        arrowX = bounds.width - arrowWidth / 2.0 - borderRadius - strokeWidth
      } else {
        arrowX = x
      }

      setNeedsDisplay()
    }
  }
}
