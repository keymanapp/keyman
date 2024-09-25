/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * Created by Shawn Schantz on 2024-09-20.
 * 
 * A value object to represent a keycode and modifier pair.
 * This corresponds to the km_core_keyboard_key struct returned by Core.
 */

#import "CoreKey.h"
#import "keyman_core_api_vkeys.h"
#import "KMELogs.h"

@implementation CoreKey
-(instancetype)init:(NSUInteger)code modifiers:(NSUInteger)modifiers {
  self = [super init];
  if (self) {
    _keyCode = code;
    _keyModifiers = modifiers;
  }
  return self;
}

-(BOOL)hasLeftAlt {
  int result = (self.keyModifiers & KM_CORE_MODIFIER_LALT);
  //os_log_info([KMELogs coreLog], "hasLeftAlt, keyModifiers: 0x%lX KM_CORE_MODIFIER_LALT: 0x%X (keyModifiers & KM_CORE_MODIFIER_LALT): 0x%X", (unsigned long)self.keyModifiers, KM_CORE_MODIFIER_LALT, result);
  return (result == KM_CORE_MODIFIER_LALT);
}

-(BOOL)hasRightAlt {
  return ((self.keyModifiers & KM_CORE_MODIFIER_RALT) == KM_CORE_MODIFIER_RALT);
}

-(BOOL)hasAlt {
  return ((self.keyModifiers & KM_CORE_MODIFIER_ALT) == KM_CORE_MODIFIER_ALT);
}

- (NSString *)description {
  NSString *format = @"<%@: %p, keyCode: %d keyModifiers: 0x%X hasLeftAlt: %d hasRightAlt: %d hasAlt: %d>";
  NSString *str = [NSString stringWithFormat:format,
                   self.className, self, self.keyCode, self.keyModifiers, self.hasLeftAlt, self.hasRightAlt, self.hasAlt];
  return str;
}

@end
