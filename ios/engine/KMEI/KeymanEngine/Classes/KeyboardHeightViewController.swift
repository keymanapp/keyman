/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * Created by Shawn Schantz on 2024-10-25.
 * 
 * View controller for adjusting the height of the keyboard
 */

import Foundation
import UIKit
import os.log

class KeyboardHeightViewController: UIViewController {
  var label: UILabel = UILabel()

  public init() {
    super.init(nibName: nil, bundle: nil)
    _ = view
  }

  override func viewDidLoad() {
    super.viewDidLoad()

    //title = NSLocalizedString("menu-settings-spacebar-title", bundle: engineBundle, comment: "")
    title = "Adjust Keyboard Height"
    navigationItem.setHidesBackButton(false, animated: true)
    navigationItem.leftBarButtonItem?.isEnabled = true

    navigationController?.toolbar?.barTintColor = Colors.statusToolbar

    let firstFrame = CGRect(x: 160, y: 240, width: 200, height: 150)
    let firstView = UIView(frame: firstFrame)
    firstView.backgroundColor = UIColor.blue
    view.addSubview(firstView)
    
    label.frame = CGRectMake(10, 10, 180, 100)
    label.backgroundColor=UIColor.white
    label.textAlignment = NSTextAlignment.center
    label.text = "adjust keyboard height"
    //label.isHidden=true
    firstView.addSubview(label)
  }
  
  override open func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)
  }
  
  required public init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
}

