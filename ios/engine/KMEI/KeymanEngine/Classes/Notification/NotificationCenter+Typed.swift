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
  /// - Important: `block` is retained with a strong reference. Beware of a reference cycle if `block` captures `self`
  /// with a strong reference.
  /// - Parameters:
  ///   - name: Typed name of the notification for which to register the observer.
  ///   - object: Filter for notifications sent by `object` if not `nil`.
  ///   - queue: The operation queue to which `block` should be added. If you pass `nil`, the block is run
  ///     synchronously on the posting thread.
  ///   - block: The block to be executed when the notification is received.
  /// - Returns: Observer that automatically deregisters itself when deinitializing.
  /// - SeeAlso: `addObserver(name:object:queue:observer:function:)` to add an instance method with a weak reference to
  /// the object instance.
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

  /// Typed version of addObserver(forName:object:queue:using:) that registers an instance method with a weak reference
  /// to the object instance.
  /// - Parameters:
  ///   - name: Typed name of the notification for which to register the observer.
  ///   - object: Filter for notifications sent by `object` if not `nil`.
  ///   - queue: The operation queue to which `block` should be added. If you pass `nil`, the block is run
  ///     synchronously on the posting thread.
  ///   - observer: The object containing the instance method. A weak reference to `observer` is held.
  ///   - function: A static reference to the instance method that is to be called on `observer`
  ///   (eg. `Class.instanceMethod`).
  /// - Returns: Observer that automatically deregisters itself when deinitializing.
  public func addObserver<O: AnyObject, T>(forName name: NotificationName<T>,
                                           object: Any? = nil,
                                           queue: OperationQueue? = nil,
                                           observer: O,
                                           function: @escaping (O) -> (T) -> Void) -> NotificationObserver {
    let block = { [weak observer] (value: T) -> Void in
      if let observer = observer {
        function(observer)(value)
      }
    }
    return addObserver(forName: name, object: object, queue: queue, using: block)
  }

  /// Typed version of addObserver(forName:object:queue:using:) that registers an instance method with a weak reference
  /// to the object instance. This is a convenience variant for instance methods that ignore the notification value.
  /// - Parameters:
  ///   - name: Typed name of the notification for which to register the observer.
  ///   - object: Filter for notifications sent by `object` if not `nil`.
  ///   - queue: The operation queue to which `block` should be added. If you pass `nil`, the block is run
  ///     synchronously on the posting thread.
  ///   - observer: The object containing the instance method. A weak reference to `observer` is held.
  ///   - function: A static reference to the instance method that is to be called on `observer`
  ///   (eg. `Class.instanceMethod`).
  /// - Returns: Observer that automatically deregisters itself when deinitializing.
  public func addObserver<O: AnyObject, T>(forName name: NotificationName<T>,
                                           object: Any? = nil,
                                           queue: OperationQueue? = nil,
                                           observer: O,
                                           function: @escaping (O) -> () -> Void) -> NotificationObserver {
    let block = { [weak observer] (_: T) -> Void in
      if let observer = observer {
        function(observer)()
      }
    }
    return addObserver(forName: name, object: object, queue: queue, using: block)
  }
}
