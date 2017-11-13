//
//  NSNotification.Name+Notifications.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-06.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

public extension NSNotification.Name {
  public static let keymanLanguagesUpdated = Notification.Name("KeymanLanguagesUpdated")
  public static let keymanLanguagesDownloadFailed = Notification.Name("KeymanLanguagesDownloadFailed")
  public static let keymanKeyboardDownloadStarted = Notification.Name("KeymanKeyboardDownloadStarted")
  public static let keymanKeyboardDownloadCompleted = Notification.Name("KeymanKeyboardDownloadCompleted")
  public static let keymanKeyboardDownloadFailed = Notification.Name("KeymanKeyboardDownloadFailed")
  public static let keymanKeyboardChanged = Notification.Name("KeymanKeyboardChanged")
  public static let keymanKeyboardLoaded = Notification.Name("KeymanKeyboardLoaded")
  public static let keymanKeyboardPickerDismissed = Notification.Name("KeymanKeyboardPickerDismissed")

  // TODO: delete
  public static let keymanKeyboardRemoved = Notification.Name("KeymanKeyboardRemoved")

  public static let keymanSubKeysMenuWillShow = Notification.Name("KeymanSubKeysMenuWillShow")
  public static let keymanSubKeysMenuDismissed = Notification.Name("KeymanSubKeysMenuDismissed")
  public static let keymanDebugLog = Notification.Name("KeymanDebugLog")
}
