//
//  KMHTTPDownloader.m
//  KeymanEngine
//
//  Created by Joshua A. Horton on 2017-07-18.
//  Copyright © 2017 SIL International. All rights reserved.
//

#import "KMHTTPDownloader.h"
#import "KMManager.h"

@implementation KMHTTPDownloader
  - (id) init:(id<KMHTTPDownloadDelegate>) handler {
    self = [super init];
    
    _queue = [NSMutableArray new];
    _handler = handler;
    
    NSURLSessionConfiguration *config = [NSURLSessionConfiguration defaultSessionConfiguration];
    
    _downloadSession = [NSURLSession sessionWithConfiguration:config delegate:self delegateQueue:nil];
    
    return self;
  }
  
  - (void) addRequest:(KMHTTPDownloadRequest *)request {
    [_queue addObject:request];
  }
  
  - (KMHTTPDownloadRequest *) popRequest {
    KMHTTPDownloadRequest *req = [_queue objectAtIndex:0];
    if (req != nil) {
      [_queue removeObjectAtIndex:0];
    }
    
    return req;
  }
  
  // Starts the queue.  The queue is managed via event messages in order to process them sequentially.
- (void) run {
    if([_queue count] > 0) {
      [self runRequest];
    }
  }
  
  // Runs the next step in the queue, if appropriate.
  - (void) runRequest {
    // Perhaps more of an 'if', relying on 'completed' messages to continue the loop?
    if([_queue count] > 0) {
      KMHTTPDownloadRequest *req = [self popRequest];
      self->_currentRequest = req;
      
      if(req.typeCode == kmDownloadFile) {
        [req setTask:[_downloadSession downloadTaskWithURL:[req url]]];
        [_handler downloadRequestStarted:req];
      } else if(req.typeCode == kmDownloadCachedData) {
        [req setTask:[_downloadSession dataTaskWithURL:[req url]]];
        //[_handler dataRequestStarted:req]; // Consider implementing later, should we need it?
      }
      
      [req.task resume];
    } else {
      [_handler downloadQueueFinished:self];
    }
    
    // The next step in the queue, should it exist, will be triggered by URLSession:task:didCompleteWithError,
    // which calls this method.  Thus, this method may serve as the single source of the 'queue finished' message.
  }
  
  // This is triggered before the 'didCompleteWithError' delegate method.
  - (void)URLSession:(NSURLSession *)session downloadTask:(NSURLSessionDownloadTask *) downloadTask
          didFinishDownloadingToURL:(NSURL *)location {
    
    //NSLog(@"Downloaded file %@ as %@ -> %@.", self->_currentRequest.url, location, self->_currentRequest.destinationFile);
    [[KMManager sharedInstance] KMLog:[NSString stringWithFormat:@"Downloaded file %@ as %@, to be copied to %@",
                                       self->_currentRequest.url,
                                       location,
                                       self->_currentRequest.destinationFile]
                   checkDebugPrinting:NO];
    
    // If a destination file for the download has already been specified, let's go ahead and copy it over.
    if(self->_currentRequest.destinationFile) {
      NSError *err;
      bool result = [[NSFileManager defaultManager] copyItemAtURL:location
                                                            toURL:[[NSURL alloc] initFileURLWithPath:self->_currentRequest.destinationFile]
                                                            error:&err];
      
      if(!result) {
        [[KMManager sharedInstance] KMLog:[NSString stringWithFormat:@"Error saving the download: %@", err.description] checkDebugPrinting:NO];
      }
    }
  }
  
  - (void)URLSession:(NSURLSession *)session dataTask:(NSURLSessionDataTask *) dataTask
          willCacheResponse:(NSCachedURLResponse *)proposedResponse
          completionHandler:(void (^)(NSCachedURLResponse *cachedResponse)) completionHandler {
    
    // We only evaluate one at a time, so the 'current request' holds the data task's original request data.
    KMHTTPDownloadRequest *request = self->_currentRequest;
    request.rawResponseData = proposedResponse.data;
    
    completionHandler(proposedResponse); // With current internal settings, this will cache the data as we desire.
  }

  - (void)URLSession:(NSURLSession *)session task:(NSURLSessionTask *)task
          didCompleteWithError:(NSError *)error {
    KMHTTPDownloadRequest *req = self->_currentRequest;
    self->_currentRequest = nil;
    
    
    if(task.error) {
      //Force the callback onto the main thread.
      dispatch_async(dispatch_get_main_queue(), ^{
        [_handler downloadRequestFailed:req];
      });
    } else {
      dispatch_async(dispatch_get_main_queue(), ^{
        [_handler downloadRequestFinished:req];
      });
    }
    dispatch_async(dispatch_get_main_queue(), ^{
      [self runRequest];
    });
  }
  
  - (void)cancelAllOperations {
    while ([_queue count] > 0) {
      [self popRequest];
    }
    
    // Done second so that there is no 'next quest' that could possibly trigger.  (This is probably overcautious.)
    if(self->_currentRequest) {
      [self->_currentRequest.task cancel];
    }
  }
  
  - (NSUInteger)requestsCount {
    return [_queue count];
  }

@end
