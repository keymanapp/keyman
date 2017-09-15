//
//  KMHTTPDownloader.h
//  KeymanEngine
//
//  Created by Joshua A. Horton on 2017-07-18.
//  Copyright © 2017 SIL International. All rights reserved.
//

#import <Foundation/Foundation.h>

@class HTTPDownloadRequest;

@class KMHTTPDownloader;

@protocol KMHTTPDownloadDelegate
  - (void) downloadRequestStarted:(HTTPDownloadRequest *)  request;
  - (void) downloadRequestFinished:(HTTPDownloadRequest *) request;
  - (void) downloadRequestFailed:(HTTPDownloadRequest *)   request;
  - (void) downloadQueueFinished:(KMHTTPDownloader *)  queue;
@end

@interface KMHTTPDownloader : NSObject<NSURLSessionDelegate, NSURLSessionTaskDelegate, NSURLSessionDataDelegate> {
  // Holds the necessary HTTP requests, in order of entry.
  NSMutableArray *_queue;
  HTTPDownloadRequest *_currentRequest;
  
  NSURLSession *_downloadSession;
}
  
  @property (weak) id<KMHTTPDownloadDelegate> handler;
  
  - (id)init:(id<KMHTTPDownloadDelegate>) handler;

  - (void)addRequest:(HTTPDownloadRequest *) request;
  
  - (HTTPDownloadRequest *)popRequest;
  
  - (void)cancelAllOperations;

  - (NSUInteger)requestsCount;
  
  - (void)run;
  
  - (void)runRequest;
  
  // Facilitates tracking of the request and its purpose within the system.
  // Is not actually used to construct any part of the URL.
  @property NSDictionary *userInfo;
  
  /**
   * The following are used to interface with the NSURLSession API.
   */
  
  - (void)URLSession:(NSURLSession *)session task:(NSURLSessionTask *)task
          didCompleteWithError:(NSError *)error;
  
  // Used for NSURLSessionDownloadTasks.  Turns out those are async, though.
  - (void)URLSession:(NSURLSession *)session downloadTask:(NSURLSessionDownloadTask *) downloadTask
          didFinishDownloadingToURL:(NSURL *)location;
  
  - (void)URLSession:(NSURLSession *)session dataTask:(NSURLSessionDataTask *) dataTask
          willCacheResponse:(NSCachedURLResponse *)proposedResponse
          completionHandler:(void (^)(NSCachedURLResponse *cachedResponse)) completionHandler;

  
@end


