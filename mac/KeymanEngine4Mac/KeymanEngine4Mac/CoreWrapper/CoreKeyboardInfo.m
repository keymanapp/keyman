/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * Created by Shawn Schantz on 2024-09-23.
 * 
 * Description...
 */

#import "CoreKeyboardInfo.h"
#import "CoreKey.h"

@implementation CoreKeyboardInfo
-(instancetype)init:(NSString*)id keyArray:(NSArray*) keys {
  self = [super init];
  if (self) {
    _keyboardId = id;
    _keyArray = keys;
  }
  return self;
}

-(void)checkModifiers {
  for (CoreKey *key in _keyArray) {
    if (key.keyModifiers) {
      _containsLeftAlt = false;
      _containsRightAlt = true;
    }
  }
}

@end
