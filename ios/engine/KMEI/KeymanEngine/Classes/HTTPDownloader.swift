//
//  HTTPDownloader.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-09-15.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

protocol HTTPDownloadDelegate: class {
  func downloadRequestStarted(_ request: HTTPDownloadRequest)
  func downloadRequestFinished(_ request: HTTPDownloadRequest)
  func downloadRequestFailed(_ request: HTTPDownloadRequest)
  func downloadQueueFinished(_ queue: HTTPDownloader)
  func downloadQueueCancelled(_ queue: HTTPDownloader)
}

class HTTPDownloader: NSObject, URLSessionDelegate, URLSessionTaskDelegate, URLSessionDownloadDelegate,
URLSessionDataDelegate {
  var queue: [HTTPDownloadRequest] = []
  // TODO: Make unowned
  weak var handler: HTTPDownloadDelegate?
  var currentRequest: HTTPDownloadRequest?
  var downloadSession: URLSession!
  public var userInfo: [String: Any] = [:]

  init(_ handler: HTTPDownloadDelegate?) {
    super.init()
    self.handler = handler

    let config = URLSessionConfiguration.default
    downloadSession = URLSession(configuration: config, delegate: self, delegateQueue: nil)
  }

  func addRequest(_ request: HTTPDownloadRequest) {
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
        req.task = downloadSession.downloadTask(with: req.url)
        handler?.downloadRequestStarted(req)
      }
      req?.task?.resume()
    } else {
      handler?.downloadQueueFinished(self)
    }
    // The next step in the queue, should it exist, will be triggered by urlSession(_, task:, didCompleteWithError:)
    // which calls this method.  Thus, this method may serve as the single source of the 'queue finished' message.
  }

  // This is triggered before the 'didCompleteWithError' delegate method.
  func urlSession(_ session: URLSession, downloadTask: URLSessionDownloadTask,
                  didFinishDownloadingTo location: URL) {
    guard let currentRequest = currentRequest else {
      return
    }
    log.debug("Downloaded file \(currentRequest.url) as \(location), " +
      "to be copied to \(currentRequest.destinationFile ?? "nil")")

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
        log.error("Error saving the download: \(error)")
      }
    }
  }

  func urlSession(_ session: URLSession, dataTask: URLSessionDataTask,
                  willCacheResponse proposedResponse: CachedURLResponse,
                  completionHandler: @escaping (CachedURLResponse?) -> Void) {
    // We only evaluate one at a time, so the 'current request' holds the data task's original request data.
    currentRequest?.rawResponseData = proposedResponse.data
    completionHandler(proposedResponse)
    // With current internal settings, this will cache the data as we desire.
  }

  func urlSession(_ session: URLSession, task: URLSessionTask, didCompleteWithError error: Error?) {
    let req = currentRequest
    currentRequest = nil
    if task.error != nil {
      //Force the callback onto the main thread.
      DispatchQueue.main.async {
        self.handler?.downloadRequestFailed(req!)
      }
    } else {
      DispatchQueue.main.async {
        self.handler?.downloadRequestFinished(req!)
      }
    }
    DispatchQueue.main.async {
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
  }

  var requestsCount: Int {
    return queue.count
  }
}
