//
//  KMInputMethodEventHandlerProtected.h
//  Keyman4MacIM
//
//  Created by tom on 1/5/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#ifndef KMInputMethodEventHandlerProtected_h
#define KMInputMethodEventHandlerProtected_h

@interface KMInputMethodEventHandler ()

- (void)handleCommand:(NSEvent *)event;
- (KMInputMethodAppDelegate *)appDelegate;
@end

#endif /* KMInputMethodEventHandlerProtected_h */
