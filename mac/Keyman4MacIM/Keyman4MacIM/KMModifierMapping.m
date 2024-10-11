/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * Created by Shawn Schantz on 2024-09-25.
 * 
 * Contains the logic that determines whether the right and left
 * options keys both cause the same character to be generated or
 * whether each option key results in a unique key combination.
 */

#import "KMModifierMapping.h"
#import <KeymanEngine4Mac/KeymanEngine4Mac.h>
#import "KMLogs.h"

@interface KMModifierMapping ()
@property (nonatomic, strong) CoreKeyboardInfo *keyboardInfo;
@end

@implementation KMModifierMapping

const NSEventModifierFlags LEFT_OPTION_MASK = 0x80120;
const NSEventModifierFlags RIGHT_OPTION_MASK = 0x80140;

+(BOOL)containsLeftOptionFlag:(NSEventModifierFlags)modifiers {
  return (modifiers & LEFT_OPTION_MASK) == LEFT_OPTION_MASK;
}

-(instancetype)init:(CoreKeyboardInfo*)keyboardInfo {
  self = [super init];
  if (self) {
    _keyboardInfo = keyboardInfo;
  }
  return self;
}

-(BOOL)optionKeysUnused {
  return !(self.keyboardInfo.containsRightAlt ||
           self.keyboardInfo.containsLeftAlt ||
           self.keyboardInfo.containsAlt);
}

-(BOOL)bothOptionKeysGenerateRightAlt {
  return (self.keyboardInfo.containsRightAlt || self.keyboardInfo.containsAlt) && !self.keyboardInfo.containsLeftAlt;
}

/**
 * If necessary, change the modifiers so that it appears that the right option key was pressed instead of the
 * left option key. This will trigger keyboard rules that are defined for right option because both option keys
 * usually have the same meaning on the Mac.
 */
- (NSEventModifierFlags) adjustModifiers:(NSEventModifierFlags)originalModifiers {
  
  NSEventModifierFlags newModifiers = originalModifiers;

  if (self.bothOptionKeysGenerateRightAlt) {
    // if original includes left option key, then replace it with the right option key
    if ([KMModifierMapping containsLeftOptionFlag:originalModifiers]) {
      
      // clear all left option bits
      newModifiers = originalModifiers & ~LEFT_OPTION_MASK;

      // set all right option bits
      newModifiers = newModifiers | RIGHT_OPTION_MASK;
      os_log_debug([KMLogs eventsLog], "adjustModifiers, changing from originalModifiers: 0x%lX to newModifiers: 0x%lX", originalModifiers, newModifiers);
    }
  } else {
    os_log_debug([KMLogs eventsLog], "adjustModifiers, retaining originalModifiers: 0x%lX", (unsigned long)originalModifiers);
  }
  return newModifiers;
}

@end
