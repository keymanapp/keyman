//
//  KeymanTextViewDelegateProxy.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-06.
//  Copyright © 2017 SIL International. All rights reserved.
//
//  Proxies delegate messages for a KeymanTextView.
//    This allows the KeymanTextView to hook into these calls while allowing a developer to still
//    use the delegate as normal (albeit with a different name: 'keymanDelegate')
//
//  This class is required because at the time of writing, setting a UITextView as it's
//  own delegate caused an infinite loop in Apple's code, repeatedly calling the selectionDidChange callback
//    See: http://lists.apple.com/archives/cocoa-dev/2010/Feb/msg01391.html

import UIKit

public protocol KeymanTextViewDelegate: UITextViewDelegate { }

class KeymanTextViewDelegateProxy: NSObject, UITextViewDelegate {
  weak var keymanDelegate: KeymanTextViewDelegate?
  private unowned let textView: UITextViewDelegate

  init(_ textView: KeymanTextView) {
    self.textView = textView
    super.init()
  }

  // MARK: - UITextViewDelegate

  // NOTE: Return values from the KeymanTextView hooks are ignored
  func textViewShouldBeginEditing(_ textView: UITextView) -> Bool {
    _ = self.textView.textViewShouldBeginEditing?(textView)
    return keymanDelegate?.textViewShouldBeginEditing?(textView) ?? true
  }

  func textViewDidBeginEditing(_ textView: UITextView) {
    self.textView.textViewDidBeginEditing?(textView)
    keymanDelegate?.textViewDidBeginEditing?(textView)
  }

  func textViewShouldEndEditing(_ textView: UITextView) -> Bool {
    _ = self.textView.textViewShouldEndEditing?(textView)
    return keymanDelegate?.textViewShouldEndEditing?(textView) ?? true
  }

  func textViewDidEndEditing(_ textView: UITextView) {
    self.textView.textViewDidEndEditing?(textView)
    keymanDelegate?.textViewDidEndEditing?(textView)
  }

  func textView(_ textView: UITextView, shouldChangeTextIn range: NSRange, replacementText string: String) -> Bool {
    _ = self.textView.textView?(textView, shouldChangeTextIn: range, replacementText: string)
    return keymanDelegate?.textView?(textView, shouldChangeTextIn: range, replacementText: string) ?? true
  }

  func textViewDidChange(_ textView: UITextView) {
    self.textView.textViewDidChange?(textView)
    keymanDelegate?.textViewDidChange?(textView)
  }

  func textViewDidChangeSelection(_ textView: UITextView) {
    self.textView.textViewDidChangeSelection?(textView)
    keymanDelegate?.textViewDidChangeSelection?(textView)
  }

  @available(iOS 10.0, *)
  func textView(_ textView: UITextView, shouldInteractWith text: NSTextAttachment,
                in range: NSRange, interaction: UITextItemInteraction) -> Bool {
    _ = self.textView.textView?(textView, shouldInteractWith: text, in: range, interaction: interaction)
    return keymanDelegate?.textView?(textView, shouldInteractWith: text, in: range,
                                     interaction: interaction) ?? true
  }

  @available(iOS 10.0, *)
  func textView(_ textView: UITextView, shouldInteractWith url: URL,
                in range: NSRange, interaction: UITextItemInteraction) -> Bool {
    _ = self.textView.textView?(textView, shouldInteractWith: url, in: range, interaction: interaction)
    return keymanDelegate?.textView?(textView, shouldInteractWith: url, in: range,
                                     interaction: interaction) ?? true
  }
}
