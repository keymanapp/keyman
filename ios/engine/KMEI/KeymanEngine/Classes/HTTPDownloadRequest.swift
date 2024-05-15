//
//  HTTPDownloadRequest.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-09-15.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

enum DownloadType: String {
  case downloadFile
}

class HTTPDownloadRequest: NSObject {
  let url: URL
  let typeCode: DownloadType
  // TODO: Make values of this dict properties of the class
  var userInfo: [String: Any]
  var task: URLSessionTask?
  var destinationFile: String?
  var rawResponseData: Data?
  var tag: Int = 0
  
  init(url: URL, downloadType type: DownloadType, userInfo info: [String: Any]) {
    self.url = url
    typeCode = type
    userInfo = info
    super.init()
  }
  
  convenience init(url: URL) {
    self.init(url: url, downloadType: DownloadType.downloadFile, userInfo: [:])
  }
  
  convenience init(url: URL, userInfo info: [String: Any]) {
    self.init(url: url, downloadType: DownloadType.downloadFile, userInfo: info)
  }
  
  convenience init(url: URL, downloadType type: DownloadType) {
    self.init(url: url, downloadType: type, userInfo: [:])
  }
  
  var responseStatusCode: Int? {
    if let response = task?.response as? HTTPURLResponse {
      return response.statusCode
    }
    return nil
  }
  
  var responseStatusMessage: String? {
    guard let statusCode = responseStatusCode else {
      return nil
    }
    return HTTPURLResponse.localizedString(forStatusCode: statusCode)
  }
  
  var error: Error? {
    return task?.error
  }
}
