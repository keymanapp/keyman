//
//  SimpleTestClient.h
//  Keyman4MacIM
//
//  Created by tom on 6/22/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#ifndef SimpleTestClient_h
#define SimpleTestClient_h
#import <Foundation/Foundation.h>
#import "KMInputMethodEventHandler.h"

@interface SimpleTestClient : NSObject
- (instancetype)init;
- (instancetype)initWithString:(NSString *)contents;
- (void)handeEventIfNotHandledBy: (KMInputMethodEventHandler *) im event:(NSEvent *)event;
@end

#endif /* SimpleTestClient_h */
