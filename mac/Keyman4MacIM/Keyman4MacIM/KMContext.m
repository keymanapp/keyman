/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * KMContext.m
 * Keyman
 * 
 * Created by Shawn Schantz on 2023-05-11.
 * 
 * Tracks the state of the context in the client application to the best of our ability.
 * Needed for storing and managing marker actions returned from Keyman Core.
 * Also useful for applications that do not tell us about their context through the two methods:
 * selectedRange and attributedSubstringFromRange
 */

#import "KMContext.h"
#include <Carbon/Carbon.h> /* For kVK_ constants. */

NSUInteger const MAXIMUM_CONTEXT_LENGTH = 60;         // KM_KBP_IT_CHAR

@interface KMContext()

@property (readonly) NSMutableString *context;

@end

@implementation KMContext

-(instancetype)init {
  self = [super init];
  if (self) {
    _context = [[NSMutableString alloc] initWithCapacity:MAXIMUM_CONTEXT_LENGTH];
  }
  return self;
}

-(instancetype)initWithString:(NSString*)initialContext {
  self = [self init];
  if (self) {
    NSString *maximumContext = nil;
    // copy up to MAXIMUM_CONTEXT_LENGTH from the end of the context
    NSUInteger initialLength = [initialContext length];
    if (initialLength > MAXIMUM_CONTEXT_LENGTH) {
      maximumContext = [initialContext substringFromIndex:initialLength-MAXIMUM_CONTEXT_LENGTH];
    } else {
      maximumContext = initialContext;
    }

    [_context setString:maximumContext];
  }
  return self;
}

-(NSString*)currentContext {
  return (NSString*)_context;
}

-(void)clearContext {
  _context.string = @"";
}

-(void)appendMarker {
  #define UC_NONCHARACTER             0xFFFF
  NSString *markerString = [NSString stringWithFormat:@"%C", UC_NONCHARACTER];
  [self.context appendString:markerString];
}

/**
 * Deletes the last complete unicode character in the context, if the string is not empty.
 * Checks for actual size and handles BMP values and surrogate pairs.
 */
-(void)deleteLastUnicodeCharacter {
  NSUInteger contextLength = self.context.length;
  if (contextLength > 0) {
    NSUInteger characterIndex = contextLength-1;
    NSRange characterRange = [self.context rangeOfComposedCharacterSequenceAtIndex:characterIndex];
    NSLog(@"deleteLastUnicodeCharacter, index = %lu, rangeOfLastCharacter = %@\n", characterIndex, NSStringFromRange(characterRange));
    [self.context deleteCharactersInRange:characterRange];
  }
}

-(void)appendString:(NSString*)string {
  [self.context appendString:string];
}

-(void)applyAction:(CoreAction*)action keyDownEvent:(nonnull NSEvent *)event {

  switch(action.actionType) {
    case MarkerAction:
      [self appendMarker];
      break;
    case CharacterAction:
      [self appendString:action.content];
      break;
    case BackspaceAction:
      [self deleteLastUnicodeCharacter];
      break;
    case EmitKeystrokeAction:
      if(event.keyCode == kVK_Delete) {
        [self deleteLastUnicodeCharacter];
      } else {
        [self appendString:event.characters];
      }
      break;
    case InvalidateContextAction:
      [self clearContext];
      break;
    default:
      NSLog(@"CachedContext applyAction, action not applied to context %@\n", action.typeName.description);
      break;
  }
}

@end
