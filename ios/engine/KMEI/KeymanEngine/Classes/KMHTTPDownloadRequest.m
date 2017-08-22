//
//  KMHTTPDownloadRequest.m
//  KeymanEngine
//
//  Created by Joshua A. Horton on 2017-07-18.
//  Copyright © 2017 SIL International. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "KMHTTPDownloadRequest.h"

@implementation KMHTTPDownloadRequest
  - (id) initWithURL:(NSURL *)url {
    return [self initWithURL:url downloadType:kmDownloadFile userInfo:nil];
  }
  
  - (id) initWithURL:(NSURL *)url userInfo:(NSDictionary *) info {
    return [self initWithURL:url downloadType:kmDownloadFile userInfo:info];
  }
  
  - (id) initWithURL:(NSURL *)url downloadType:(KMDownloadType) type {
    return [self initWithURL:url downloadType:type userInfo:nil];
  }

  - (id) initWithURL:(NSURL *)url downloadType:(KMDownloadType) type userInfo:(NSDictionary *) info {
    self = [super init];
    if (self) {
      _url = url;
      _typeCode = type;
      _userInfo = info;
    }
    return self;
  }
  
  - (NSInteger)responseStatusCode {
    if([_task.response isKindOfClass:[NSHTTPURLResponse class]]) {
      NSHTTPURLResponse * resp = (NSHTTPURLResponse *)_task.response;
      return resp.statusCode;
    } else {
      [NSException raise:@"This download request didn't use the HTTP or HTTPS protocol!  No response code is available." format:@"No response code available."];
      return -1;
    }
  }
  
  - (NSString *)responseStatusMessage {
    return [NSHTTPURLResponse localizedStringForStatusCode:[self responseStatusCode]];
  }
  
  - (NSError *)error {
    return _task.error;
  }

@end
