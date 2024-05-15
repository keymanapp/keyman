//
//  NotificationName.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-30.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

/// `Notification.Name` with a phantom type. `T` is the type of the data in the notification.
///
/// **See Also:** Usage with extension of `NotificationCenter`
///   - `NotificationCenter.addObserver(forName:object:queue:using:)`
///   - `NotificationCenter.post(name:object:value)`
public struct NotificationName<T> {
  /// Underlying name of the notification.
  /// - Important: For most usages, this property should not be accessed directly. Use the typed version of
  /// `NotificationCenter.addObserver(forName:object:queue:using:)` instead.
  public let name: NSNotification.Name
  
  public init(name: NSNotification.Name) {
    self.name = name
  }
  
  public init(_ string: String) {
    self.name = NSNotification.Name(string)
  }
}
