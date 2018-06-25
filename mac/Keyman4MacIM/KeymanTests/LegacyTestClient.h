//
//  LegacyTestClient.h
//  Keyman4MacIM
//
//  Created by tom on 6/22/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#ifndef LegacyTestClient_h
#define LegacyTestClient_h
#import <Foundation/Foundation.h>
#import "KMInputMethodEventHandler.h"

@interface LegacyTestClient : NSObject
- (instancetype)init;
- (instancetype)initWithString:(NSString *)contents;
- (void)handleEventIfNotHandledBy: (KMInputMethodEventHandler *) im event:(NSEvent *)event;
- (NSString *) getResult;

- (void)insertTextProtected:(id)string replacementRange:(NSRange)replacementRange;
- (NSString *)substringFromRangeProtected:(NSRange)range;
@end

#endif /* LegacyTestClient_h */
