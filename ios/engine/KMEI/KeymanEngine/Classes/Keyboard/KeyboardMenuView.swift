//
//  KeyboardMenuView.swift
//  KMEI
//
//  Created by Gabriel Wong on 2017-09-28.
//  Copyright © 2017 SIL International. All rights reserved.
//

import  Foundation
import  UIKit

// Used by the system keyboard as its globe key menu.
class KeyboardMenuView: UIView, UITableViewDelegate, UITableViewDataSource, UIGestureRecognizerDelegate {
  private let bgColor = Colors.systemBackground
  private let bgColor2 = Colors.systemBackground
  private let borderColor = Colors.popupBorder
  private var borderRadius: CGFloat = 5.0
  private var strokeWidth: CGFloat = 0.75

  private var menuFrame: CGRect = .zero
  private let keyFrame: CGRect
  private let rowHeight: CGFloat
  private let fontSize: CGFloat
  private let xLength: CGFloat
  private var adjX: CGFloat = 0
  private var tableView: UITableView?
  private let closeButtonTitle: String?

  // Is populated during init from a weak ref by KeymanWebViewController,
  // and instances of this class are owned by the same.
  private weak var _inputViewController: InputViewController?
  override var inputViewController: InputViewController? {
    return _inputViewController
  }

  // TODO: Only contain the keyboards and not the close button text
  private var _tableList: [Any]?
  var tableList: [Any] {
    if let tableList = _tableList {
      return tableList
    }

    let keyboardList = Storage.active.userDefaults.userKeyboards

    let titleCloseButton: String
    if let closeButtonTitle = closeButtonTitle {
      titleCloseButton = closeButtonTitle
    } else {
      let appName = Bundle.main.object(forInfoDictionaryKey: "CFBundleDisplayName") as? String ?? "Keyman"
      let format = NSLocalizedString("keyboard-menu-exit", bundle: engineBundle, comment: "")
      titleCloseButton = String.localizedStringWithFormat(format, appName)
    }

    if let keyboardList = keyboardList {
      _tableList = keyboardList
      _tableList!.append(titleCloseButton)
    } else {
      _tableList = [Defaults.keyboard]
      _tableList!.append(titleCloseButton)
    }
    return _tableList!
  }

  init(keyFrame frame: CGRect, inputViewController: InputViewController, closeButtonTitle: String?) {
    let isPortrait: Bool
    if Manager.shared.isSystemKeyboard {
      isPortrait = InputViewController.isPortrait
    } else {
      isPortrait = UIDevice.current.orientation.isPortrait
    }

    _inputViewController = inputViewController
    self.closeButtonTitle = closeButtonTitle
    keyFrame = frame
    rowHeight = UIDevice.current.userInterfaceIdiom == .phone ? 30 : 60
    fontSize = UIDevice.current.userInterfaceIdiom == .phone ? 14 : 21
    xLength = keyFrame.size.width * (isPortrait ? 1.1 : 0.6)

    super.init(frame: inputViewController.view.frame)

    let screenWidth = UIScreen.main.bounds.width
    let maxWidth = CGFloat.minimum(getMaxWidth(), screenWidth)
    let baseHeight = keyFrame.size.height
    let containerWidth = maxWidth - strokeWidth * 2
    var containerHeight = CGFloat(tableList.count) * rowHeight

    let vHeight = Manager.shared.inputViewController.kmwHeight
    let bY = Manager.shared.inputViewController.kmwHeight - (keyFrame.origin.y + baseHeight)

    if containerHeight + baseHeight > vHeight - bY {
      let maxRows = (vHeight - baseHeight - bY) / rowHeight
      containerHeight = maxRows.rounded(.towardZero) * rowHeight
    }

    let containerFrame = CGRect(x: strokeWidth, y: strokeWidth, width: containerWidth, height: containerHeight)
    let viewWidth = maxWidth
    let viewHeight = baseHeight + containerHeight + strokeWidth
    var viewPosX = keyFrame.origin.x - (viewWidth - keyFrame.size.width) / 2.0
    let viewPosY = (keyFrame.origin.y) - (viewHeight - keyFrame.size.height)

    if viewPosX < 0 {
      if (keyFrame.origin.x - borderRadius * 1.0) < 0 {
        adjX = keyFrame.origin.x - viewPosX
        viewPosX = keyFrame.origin.x
      } else {
        adjX = -viewPosX
        viewPosX = 0
      }
    } else if (viewPosX + viewWidth) > screenWidth {
      if ((keyFrame.origin.x + keyFrame.size.width) + borderRadius * 1.0) > screenWidth {
        adjX = viewPosX - keyFrame.origin.x
        viewPosX = keyFrame.origin.x + 2 * adjX
      } else {
        adjX = (screenWidth - viewWidth) - viewPosX
        viewPosX = adjX + viewPosX
      }
    }

    menuFrame = CGRect(x: viewPosX, y: viewPosY, width: viewWidth, height: viewHeight)

    backgroundColor = UIColor(white: 0.0, alpha: 0.25)
    tag = 1
    isUserInteractionEnabled = true
    let menuView = UIView(frame: menuFrame)
    menuView.tag = 2
    menuView.backgroundColor = UIColor.clear
    tableView = UITableView(frame: containerFrame)
    tableView?.delegate = self
    tableView?.dataSource = self
    tableView?.separatorStyle = .none
    tableView?.backgroundColor = UIColor.clear
    tableView?.layer.cornerRadius = (borderRadius * 1.0 - strokeWidth)
    menuView.addSubview(tableView!)
    addSubview(menuView)
    let tapGesture = UITapGestureRecognizer(target: self, action: #selector(self.tapAction))
    tapGesture.delegate = self
    tapGesture.numberOfTapsRequired = 1
    tapGesture.numberOfTouchesRequired = 1
    addGestureRecognizer(tapGesture)

  }

  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  func flashScrollIndicators() {
    tableView?.flashScrollIndicators()
  }

  override func draw(_ rect: CGRect) {
    let keyWidth = keyFrame.width
    let keyHeight = keyFrame.height
    let viewWidth = menuFrame.width
    let viewHeight = menuFrame.height

    guard let context = UIGraphicsGetCurrentContext() else {
      return
    }
    UIGraphicsPushContext(context)
    context.setLineJoin(.round)
    context.setLineWidth(strokeWidth * 2)
    context.setStrokeColor(borderColor.cgColor)
    context.setFillColor(bgColor.cgColor)
    var keyLeft = menuFrame.origin.x + (viewWidth - keyWidth) / 2.0 + strokeWidth - adjX
    var keyRight  = menuFrame.origin.x + (viewWidth - keyWidth) / 2.0 + keyWidth - strokeWidth - adjX
    keyLeft = (keyLeft * 10).rounded(.up) / 10
    keyRight = (keyRight * 10).rounded(.down) / 10

    let midY = menuFrame.origin.y + viewHeight - keyHeight
    let viewLeft = menuFrame.origin.x + strokeWidth
    let viewRight = menuFrame.origin.x + viewWidth - strokeWidth
    let viewTop = menuFrame.origin.y + strokeWidth
    let viewBottom = menuFrame.origin.y + viewHeight - strokeWidth * 1.25
    let viewMid = menuFrame.origin.x + viewWidth / 2.0
    let r = borderRadius - strokeWidth
    let r2 = borderRadius * 1.0 - strokeWidth
    let r3 = borderRadius * 1.0 - strokeWidth

    context.beginPath()
    context.move(to: CGPoint(x: viewMid, y: viewTop))
    context.addArc(tangent1End: CGPoint(x: viewLeft, y: viewTop),
                   tangent2End: CGPoint(x: viewLeft, y: midY), radius: r2)
    context.addArc(tangent1End: CGPoint(x: viewLeft, y: midY),
                   tangent2End: CGPoint(x: keyLeft, y: midY), radius: r2)
    context.addArc(tangent1End: CGPoint(x: keyLeft, y: midY),
                   tangent2End: CGPoint(x: keyLeft, y: viewBottom), radius: r3)
    context.addArc(tangent1End: CGPoint(x: keyLeft, y: viewBottom),
                   tangent2End: CGPoint(x: keyRight, y: viewBottom), radius: r)
    context.addArc(tangent1End: CGPoint(x: keyRight, y: viewBottom),
                   tangent2End: CGPoint(x: keyRight, y: midY), radius: r)
    context.addArc(tangent1End: CGPoint(x: keyRight, y: midY),
                   tangent2End: CGPoint(x: viewRight, y: midY), radius: r3)
    context.addArc(tangent1End: CGPoint(x: viewRight, y: midY),
                   tangent2End: CGPoint(x: viewRight, y: viewTop), radius: r2)
    context.addArc(tangent1End: CGPoint(x: viewRight, y: viewTop),
                   tangent2End: CGPoint(x: viewMid, y: viewTop), radius: r2)
    context.closePath()
    context.drawPath(using: .stroke)

    context.beginPath()
    context.move(to: CGPoint(x: viewMid, y: viewTop))
    context.addArc(tangent1End: CGPoint(x: viewLeft, y: viewTop),
                   tangent2End: CGPoint(x: viewLeft, y: midY), radius: r2)
    context.addArc(tangent1End: CGPoint(x: viewLeft, y: midY),
                   tangent2End: CGPoint(x: keyLeft, y: midY), radius: r2)
    context.addArc(tangent1End: CGPoint(x: keyLeft, y: midY),
                   tangent2End: CGPoint(x: keyLeft, y: viewBottom), radius: r3)
    context.addArc(tangent1End: CGPoint(x: keyLeft, y: viewBottom),
                   tangent2End: CGPoint(x: keyRight, y: viewBottom), radius: r)
    context.addArc(tangent1End: CGPoint(x: keyRight, y: viewBottom),
                   tangent2End: CGPoint(x: keyRight, y: midY), radius: r)
    context.addArc(tangent1End: CGPoint(x: keyRight, y: midY),
                   tangent2End: CGPoint(x: viewRight, y: midY), radius: r3)
    context.addArc(tangent1End: CGPoint(x: viewRight, y: midY),
                   tangent2End: CGPoint(x: viewRight, y: viewTop), radius: r2)
    context.addArc(tangent1End: CGPoint(x: viewRight, y: viewTop),
                   tangent2End: CGPoint(x: viewMid, y: viewTop), radius: r2)
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

  override func gestureRecognizerShouldBegin(_ gestureRecognizer: UIGestureRecognizer) -> Bool {
    let point = gestureRecognizer.location(in: gestureRecognizer.view)
    guard let view = gestureRecognizer.view?.hitTest(point, with: nil) else {
      return false
    }
    return view.tag > 0
  }

  @objc func tapAction(_ sender: UITapGestureRecognizer) {
    Manager.shared.inputViewController.dismissKeyboardMenu()
  }

  func numberOfSections(in tableView: UITableView) -> Int {
    return 1
  }

  func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    return tableList.count
  }

  func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    let cellIdentifier = "Cell"
    if let cell = tableView.dequeueReusableCell(withIdentifier: cellIdentifier) {
      return cell
    }

    let cell = UITableViewCell(style: .default, reuseIdentifier: cellIdentifier)
    let selectionColor = UIView()
    selectionColor.backgroundColor = Colors.popupKeyTint
    cell.selectedBackgroundView = selectionColor
    return cell
  }

  func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
    return rowHeight
  }

  func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
    cell.textLabel?.textColor = UIColor.darkGray
    cell.textLabel?.textAlignment = .left
    cell.textLabel?.font = UIFont.systemFont(ofSize: fontSize)
    cell.selectionStyle = .default
    cell.backgroundColor = UIColor.clear
    if indexPath.row == (tableList.count - 1) {
      cell.textLabel?.text = tableList[indexPath.row] as? String
      cell.tag = indexPath.row
      cell.textLabel?.textColor = UIColor.gray
      cell.textLabel?.textAlignment = .right
      cell.isSelected = false
      cell.accessoryType = .disclosureIndicator
    } else {
      let keyboard = tableList[indexPath.row] as! InstallableKeyboard
      cell.textLabel?.text = keyboard.name
      cell.tag = indexPath.row
      if Manager.shared.currentKeyboardID == keyboard.fullID {
        cell.selectionStyle = .none
        cell.isSelected = true
        cell.accessoryType = .checkmark
      } else {
        cell.isSelected = false
        cell.accessoryType = .none
      }
    }
  }

  func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    switchKeyboard(indexPath.row)

    Manager.shared.inputViewController.dismissKeyboardMenu()
  }

  func getMaxWidth() -> CGFloat {
    var w: CGFloat = 0
    for obj in tableList {
      let text: String?
      if let kb = obj as? InstallableKeyboard {
        text = kb.name
      } else {
        text = obj as? String
      }
      let tw = getTextWidth(text ?? "")
      if tw > w {
        w = tw
      }
    }
    return w + ((UIDevice.current.userInterfaceIdiom == .phone) ? 75 : 150)
  }

  func getTextWidth(_ text: String) -> CGFloat {
    return text.size(withAttributes: [.font: UIFont.systemFont(ofSize: fontSize)]).width
  }

  func switchKeyboard(_ index: Int) {
    if index == (tableList.count - 1) {
      inputViewController?.advanceToNextInputMode()
      return
    }
    guard let kb = tableList[index] as? InstallableKeyboard else {
      return
    }
    if Manager.shared.setKeyboard(kb) {
      tableView?.reloadData()
      
      // If we allow the system keyboard to show no banners, these lines are needed
      // for properly-managed variable system keyboard height.
      //
      // It's not quite perfect yet, though - the rescale only works if the first
      // keyboard loaded does not provide suggestions.  Parking it here as it's not
      // worth pursuing further at this stage.
      inputViewController?.updateViewConstraints()
      return
    }
  }
}
