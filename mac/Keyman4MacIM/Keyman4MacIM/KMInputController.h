//
//  KMInputController.h
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 29/01/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <InputMethodKit/InputMethodKit.h>
#import <Carbon/Carbon.h>

@interface KMInputController : IMKInputController

- (void)menuAction:(id)sender;
- (void)handleBackspace:(NSEvent *)event;
- (void)changeClients:(NSString *)clientAppId;
- (void)deactivateClient;
@end
