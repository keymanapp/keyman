/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * Created by Shawn Schantz on 2024-09-20.
 * 
 * A value object to represent a keycode and modifier pair.
 * This corresponds to the km_core_keyboard_key struct returned by Core.
 */

#import "CoreKey.h"

@implementation CoreKey
-(instancetype)init:(NSUInteger)code modifiers:(NSUInteger)modifiers {
  self = [super init];
  if (self) {
    _keyCode = code;
    _keyModifiers = modifiers;
  }
  return self;
}


@end
