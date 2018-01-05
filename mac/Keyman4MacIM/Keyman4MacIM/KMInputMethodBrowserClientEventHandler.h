//
//  KMInputMethodBrowserClientEventHandler.h
//  Keyman4MacIM
// This class is used for browser clients because certain javascript-based websites, such as Google Docs) do
// not properly deal with calls to insertText calls that replace a range of characters. So for these "legacy"
// apps, we post one or more deletes (i.e., backspace), followed by a special code that tells us we're now
// ready to insert the composed text .
//
//  Created by tom on 1/5/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#ifndef KMInputMethodLegacyClientEventHandler_h
#define KMInputMethodLegacyClientEventHandler_h
#import "KMInputMethodEventHandler.h"

@interface KMInputMethodBrowserClientEventHandler : KMInputMethodEventHandler
@end

#endif /* KMInputMethodLegacyClientEventHandler_h */
