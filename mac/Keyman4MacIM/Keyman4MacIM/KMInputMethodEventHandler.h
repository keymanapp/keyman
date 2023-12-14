//
//  KMInputMethodEventHandler.h
//  Keyman4MacIM
//
//  Created by Tom Bogle on 11/22/17.
//  Copyright © 2017 SIL International. All rights reserved.
//

#ifndef KMInputMethodEventHandler_h
#define KMInputMethodEventHandler_h

#import <Foundation/Foundation.h>
#import "KMInputMethodAppDelegate.h"

@interface KMInputMethodEventHandler : NSObject

- (instancetype)initWithClient:(NSString *)clientAppId client:(id)sender;
- (BOOL)handleEvent:(NSEvent *)event client:(id)sender;
- (void)handleBackspace:(NSEvent *)event;
- (void)deactivate;

@end

#endif /* KMInputMethodEventHandler_h */
