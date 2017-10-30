//
//  NotificationCenter+Typed.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-30.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

private let userInfoKey = "value"

public extension NotificationCenter {
  /// Typed version of post(name:object:userInfo:).
  /// - Parameters:
  ///   - name: The typed name of the notification.
  ///   - object: The object posting the notification.
  ///   - value: Value to be sent to observer.
  public func post<T>(name: NotificationName<T>, object: Any? = nil, value: T) {
    let userInfo = [userInfoKey: value]
    post(name: name.name, object: object, userInfo: userInfo)
  }

  /// Typed version of addObserver(forName:object:queue:using:)
  /// - Parameters:
  ///   - name: Typed name of the notification for which to register the observer.
  ///   - object: Filter for notifications sent by `object` if not `nil`.
  ///   - queue: The operation queue to which `block` should be added. If you pass `nil`, the block is run
  ///     synchronously on the positing thread.
  ///   - block: The block to be executed when the notification is received.
  /// - Returns: Observer that automatically deregisteres itself when deinitializing.
  public func addObserver<T>(forName name: NotificationName<T>,
                             object: Any? = nil,
                             queue: OperationQueue? = nil,
                             using block: @escaping (T) -> Void) -> NotificationObserver {
    let observer = addObserver(forName: name.name, object: object, queue: queue) { notification in
      if let value = notification.userInfo?[userInfoKey] as? T {
        block(value)
      } else {
        Manager.shared.kmLog("Unexpected userInfo in notification: \(String(describing: notification))",
          checkDebugPrinting: false)
      }
    }
    return NotificationObserver(observer: observer, center: self)
  }
}
