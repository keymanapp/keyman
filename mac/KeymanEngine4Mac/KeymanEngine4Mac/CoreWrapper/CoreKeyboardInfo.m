/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * Created by Shawn Schantz on 2024-09-23.
 * 
 * A class to contain the information about the Keyman Keyboard
 * provided by calling km_core_keyboard_get_key_list.
 */

#import "CoreKeyboardInfo.h"
#import "CoreKey.h"

@implementation CoreKeyboardInfo
-(instancetype)init:(NSString*)id keyArray:(NSArray*) keys {
  self = [super init];
  if (self) {
    _keyboardId = id;
    _keyArray = keys;
    _containsAlt = false;
    _containsLeftAlt = false;
    _containsRightAlt = false;
    [self checkModifiers];
  }
  return self;
}

/**
 * loop through the entire key list and track whether any keys have the specified modifiers
 */
-(void)checkModifiers {
  for (CoreKey *key in _keyArray) {
    if (key.hasRightAlt) {
      _containsRightAlt = true;
    } else if (key.hasLeftAlt) {
      _containsLeftAlt = true;
    } else if (key.hasAlt) {
      _containsAlt = true;
    }
  }
}

- (NSString *)description {
  NSString *format = @"<%@: %p, keyboardId: %@ key count: %d containsAlt: %d containsLeftAlt: %d containsRightAlt: %d>";
  NSString *str = [NSString stringWithFormat:format,
                   self.className, self, self.keyboardId, self.keyArray.count, self.containsAlt, self.containsLeftAlt, self.containsRightAlt];
  return str;
}

@end
