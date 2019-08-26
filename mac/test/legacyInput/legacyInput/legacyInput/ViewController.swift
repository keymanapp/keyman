//
//  ViewController.swift
//  legacyInput
//
//  Created by Marc Durdin on 23/7/19.
//  Copyright © 2019 Keyman. All rights reserved.
//

import Cocoa

class ViewController: NSViewController {

  @IBOutlet weak var testField: NSTextField!
  @IBOutlet weak var logField: NSScrollView!
  @IBOutlet var logTextField: NSTextView!

  override func viewDidLoad() {
    super.viewDidLoad()

    NSEvent.addLocalMonitorForEvents(matching: .flagsChanged) {
      self.flagsChanged(with: $0)
      return $0
    }
    NSEvent.addLocalMonitorForEvents(matching: .keyDown) {
      self.keyDown(with: $0)
      return $0
    }
  }

  override var representedObject: Any? {
    didSet {
    // Update the view, if already loaded.
    }
  }

  override func keyDown(with event: NSEvent) {
    switch event.modifierFlags.intersection(.deviceIndependentFlagsMask) {
    case [.command] where event.characters == "l",
         [.command, .shift] where event.characters == "l":
      print("command-l or command-shift-l")
    default:
      break
    }

    //print(String(format:"key:%02x char:%@\n", arguments: [event.keyCode, event.characters ?? ""]))
    logTextField.string += String(format:"key:%02x char:%@\n", arguments: [event.keyCode, event.characters ?? ""])
    logTextField.scrollToEndOfDocument(nil)
  }

  override func flagsChanged(with event: NSEvent) {
    switch event.modifierFlags.intersection(.deviceIndependentFlagsMask) {
    case [.shift]:
      print("shift key is pressed")
    case [.control]:
      print("control key is pressed")
    case [.option] :
      print("option key is pressed")
    case [.command]:
      print("Command key is pressed")
    case [.control, .shift]:
      print("control-shift keys are pressed")
    case [.option, .shift]:
      print("option-shift keys are pressed")
    case [.command, .shift]:
      print("command-shift keys are pressed")
    case [.control, .option]:
      print("control-option keys are pressed")
    case [.control, .command]:
      print("control-command keys are pressed")
    case [.option, .command]:
      print("option-command keys are pressed")
    case [.shift, .control, .option]:
      print("shift-control-option keys are pressed")
    case [.shift, .control, .command]:
      print("shift-control-command keys are pressed")
    case [.control, .option, .command]:
      print("control-option-command keys are pressed")
    case [.shift, .command, .option]:
      print("shift-command-option keys are pressed")
    case [.shift, .control, .option, .command]:
      print("shift-control-option-command keys are pressed")
    default:
      print("no modifier keys are pressed")
    }
  }

}

