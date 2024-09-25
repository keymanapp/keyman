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

@interface KMModifierMapping ()
@property (nonatomic, strong) CoreKeyboardInfo *keyboardInfo;
@end

@implementation KMModifierMapping

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

@end
