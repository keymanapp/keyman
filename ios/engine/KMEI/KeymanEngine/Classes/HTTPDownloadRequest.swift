//
//  HTTPDownloadRequest.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-09-15.
//  Copyright © 2017 SIL International. All rights reserved.
//

@objc enum DownloadType: Int {
  case downloadFile
  case downloadCachedData
}

class HTTPDownloadRequest: NSObject {
  @objc let url: URL
  @objc let typeCode: DownloadType
  @objc let userInfo: [String: Any]
  var task: URLSessionTask?
  @objc var destinationFile: String?
  @objc var rawResponseData: Data?
  @objc var tag: Int = 0

  @objc init(url: URL, downloadType type: DownloadType, userInfo info: [String: Any]) {
    self.url = url
    typeCode = type
    userInfo = info
    super.init()
  }

  @objc convenience init(url: URL) {
    self.init(url: url, downloadType: DownloadType.downloadFile, userInfo: [:])
  }

  @objc convenience init(url: URL, userInfo info: [String: Any]) {
    self.init(url: url, downloadType: DownloadType.downloadFile, userInfo: info)
  }

  @objc convenience init(url: URL, downloadType type: DownloadType) {
    self.init(url: url, downloadType: type, userInfo: [:])
  }

  var responseStatusCode: Int? {
    if let response = task?.response as? HTTPURLResponse {
      return response.statusCode
    }
    return nil
  }

  // TODO: Remove
  @objc var responseStatusCodeObjc: Int {
    if let code = responseStatusCode {
      return code
    }
    return -1
  }

  @objc var responseStatusMessage: String? {
    guard let statusCode = responseStatusCode else {
      return nil
    }
    return HTTPURLResponse.localizedString(forStatusCode: statusCode)
  }

  @objc var error: Error? {
    return task?.error
  }
}
