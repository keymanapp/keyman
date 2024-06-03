//
//  HTTPDownloader.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-09-15.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation
import os.log

protocol HTTPDownloadDelegate: AnyObject {
  func downloadRequestStarted(_ request: HTTPDownloadRequest)
  func downloadRequestFinished(_ request: HTTPDownloadRequest)
  func downloadRequestFailed(_ request: HTTPDownloadRequest, with: Error?)
  func downloadQueueFinished(_ queue: HTTPDownloader)
  func downloadQueueCancelled(_ queue: HTTPDownloader)
}

class HTTPDownloader: NSObject {
  var queue: [HTTPDownloadRequest] = []
  // TODO: Make unowned
  /*weak*/ var handler: HTTPDownloadDelegate?  // 'weak' interferes with installs
                                               // from the app's browser.
  var currentRequest: HTTPDownloadRequest?
  var downloadSession: URLSession!
  public var userInfo: [String: Any] = [:]
  var isCancelled: Bool = false

  init(_ handler: HTTPDownloadDelegate?, session: URLSession = .shared) {
    super.init()
    self.handler = handler
    downloadSession = session
  }

  func addRequest(_ request: HTTPDownloadRequest) {
    isCancelled = false
    queue.append(request)
  }

  func popRequest() -> HTTPDownloadRequest? {
    if queue.isEmpty {
      return nil
    }
    return queue.remove(at: 0)
  }

  // Starts the queue.  The queue is managed via event messages in order to process them sequentially.
  func run() {
    isCancelled = false
    if !queue.isEmpty {
      runRequest()
    }
  }

  // Runs the next step in the queue, if appropriate.
  func runRequest() {
    // Perhaps more of an 'if', relying on 'completed' messages to continue the loop?
    if !queue.isEmpty {
      let req: HTTPDownloadRequest! = popRequest()
      currentRequest = req

      if req.typeCode == .downloadFile {
        req.task = downloadSession.downloadTask(with: req.url, completionHandler: self.downloadCompletionHandler)
        handler?.downloadRequestStarted(req)
      }
      req?.task?.resume()
    } else if !isCancelled {
      handler?.downloadQueueFinished(self)
    }
    // The next step in the queue, should it exist, will be triggered by urlSession(_, task:, didCompleteWithError:)
    // which calls this method.  Thus, this method may serve as the single source of the 'queue finished' message.
  }

  func downloadCompletionHandler(location: URL?, response: URLResponse?, error: Error?) -> Void {
    guard let currentRequest = currentRequest else {
      return
    }

    // If it's an error case
    guard let location = location else {
      // If location is `nil`, we have an error.
      // See docs at https://developer.apple.com/documentation/foundation/urlsession/1411511-downloadtask
      //
      // Force the callback onto the main thread.
      DispatchQueue.main.async {
        self.handler?.downloadRequestFailed(currentRequest, with: nil)
        self.runRequest()
      }
      return
    }

    guard error == nil else {
      // The download process itself encountered an error.
      DispatchQueue.main.async {
        self.handler?.downloadRequestFailed(currentRequest, with: error)
        self.runRequest()
      }
      return
    }

    // Successful download.
    let message = "Downloaded file \(currentRequest.url) as \(location), to be copied to \(currentRequest.destinationFile ?? "nil")"
    os_log("%{public}s", log: KeymanEngineLogger.resources, type: .debug, message)

    // If a destination file for the download has already been specified, let's go ahead and copy it over.
    if let destFile = currentRequest.destinationFile {
      let destFileUrl = URL(fileURLWithPath: destFile)
      do {

        // Need to delete the file if it already exists (e.g. in case of a previous partial download)
        if(FileManager.default.fileExists(atPath: destFileUrl.path)) {
            try FileManager.default.removeItem(at: destFileUrl)
        }

        try FileManager.default.copyItem(at: location,
                                         to: destFileUrl)
      } catch {
        os_log("Error saving the download: %{public}s", log: KeymanEngineLogger.resources, type: .error, error.localizedDescription)
      }
    }

    DispatchQueue.main.async {
      self.handler?.downloadRequestFinished(currentRequest)
      self.runRequest()
    }
  }

  func dataCompletionHandler(data: Data?, response: URLResponse?, error: Error?) -> Void {
    guard let currentRequest = currentRequest else {
      return
    }

    guard let data = data else {
      DispatchQueue.main.async {
        self.handler?.downloadRequestFailed(currentRequest, with: nil)
        self.runRequest()
      }
      return
    }
    // We only evaluate one at a time, so the 'current request' holds the data task's original request data.
    currentRequest.rawResponseData = data
    // With current internal settings, this will cache the data as we desire.

    DispatchQueue.main.async {
      self.handler?.downloadRequestFinished(currentRequest)
      self.runRequest()
    }
  }

  func cancelAllOperations() {
    while !queue.isEmpty {
      _ = popRequest()
    }

    // Done second so that there is no 'next quest' that could possibly trigger.  (This is probably overcautious.)
    if let currentRequest = currentRequest {
      currentRequest.task?.cancel()
    }
    
    self.handler?.downloadQueueCancelled(self)
    self.isCancelled = true
  }

  var requestsCount: Int {
    return queue.count
  }
}
