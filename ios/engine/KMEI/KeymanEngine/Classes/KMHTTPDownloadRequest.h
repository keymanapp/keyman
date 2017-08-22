//
//  KMHTTPDownloadRequest.h
//  KeymanEngine
//
//  Created by Joshua A. Horton on 2017-07-18.
//  Copyright © 2017 SIL International. All rights reserved.
//

#import <Foundation/Foundation.h>

typedef enum {
  kmDownloadFile,
  kmDownloadCachedData
} KMDownloadType;

@interface KMHTTPDownloadRequest : NSObject
  
  - (id) initWithURL:(NSURL *)url;
  - (id) initWithURL:(NSURL *)url downloadType:(KMDownloadType) type;
  - (id) initWithURL:(NSURL *)url userInfo:(NSDictionary *) info;
  - (id) initWithURL:(NSURL *)url downloadType:(KMDownloadType) type userInfo:(NSDictionary *) info;
  
  - (NSInteger)responseStatusCode;
  - (NSString *)responseStatusMessage;
  - (NSError *)error;
  
  // The URL being requested.
  @property (readonly) NSURL *url;
  
  // Facilitates tracking of the request and its purpose within the system.
  // Is not actually used to construct any part of the URL.
  @property NSDictionary *userInfo;
  
  // The location we wish to store the downloaded file.
  @property NSString *destinationFile;
  
  // Used to store the NSURLSessionTask corresponding to the request once it has been activated.
  @property NSURLSessionTask *task;
  
  // Maintained for legacy reasons, as the value is used to track specific data for managing notifications.
  @property NSInteger tag;

  // Used to pass along data from "CachedData" requests.
  @property NSData *rawResponseData;

  // Indicates the type of download request this represents.
  @property KMDownloadType typeCode;
@end
