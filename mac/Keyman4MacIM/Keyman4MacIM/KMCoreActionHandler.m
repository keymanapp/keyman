/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * KMCoreActionHandler.m
 * Keyman
 * 
 * Created by Shawn Schantz on 2023-04-21.
 * 
 * Processes an NSArray of CoreAction objects and applies them to the client application.
 * May call insertText and attributedSubstringFromRange if available, may also send events.
 * Responsible for applying changes in the correct order so that the correct output is created.
 *
 * To apply the changes in the correct order, this class identifies the action pattern.
 * Some patterns require a different strategy to preserve the order.
 *
 */

#import "KMInputMethodEventHandler.h"
#import "KMCoreActionHandler.h"

@interface KMCoreActionHandler ()

typedef enum {BackspacesOnly,
  CharactersOnly,
  BackspaceBeforeCharacter,
  CharacterBeforeBackspace,
  Unexceptional
} ActionPattern;

@property (readonly) NSArray *actions;
@property (readonly) NSEvent *event;
@property (readonly) id client;
@property (readonly) ActionPattern pattern;
@property int backspaceCount;

@end

@implementation KMCoreActionHandler

-(instancetype)initWithActions:(NSArray*)actions event: (NSEvent *)event client:(id) client  {
  self = [super init];
  if (self) {
    _actions = actions;
    _event = event;
    _client = client;
    _pattern = [self evaluateActionPattern:actions];
    _backspacesToInsert = 0;
    _backspaceEventsToGenerate = 0;
    _textToInsert = @"";
    _textEventsToGenerate = @"";
  }
  return self;
}

/*
 * Looking for interesting patterns of backspaces and characters that require
 * special handling to avoid race conditions.
 */
-(ActionPattern)evaluateActionPattern:(NSArray*)actions {
  ActionPattern actionPattern = Unexceptional;
  BOOL containsCharacters = NO;
  BOOL containsBackspaces = NO;
  BOOL backspaceFirst = NO;
  BOOL characterFirst = NO;
  //TODO: track character count?
  
  for (CoreAction *action in [actions objectEnumerator])
  {
    if (action.actionType==CharacterAction) {
      containsCharacters = YES;
      if (!containsBackspaces) {
        characterFirst = YES;
      }
    } else if (action.actionType==BackspaceAction) {
      containsBackspaces = YES;
      self.backspaceCount++;
      if (!containsCharacters) {
        backspaceFirst = YES;
      }
    }
  }

  if (containsCharacters) {
    if (containsBackspaces) {
      // contains both characters and backspaces, now determine order
      if (characterFirst) {
        actionPattern = CharacterBeforeBackspace;
      } else if (backspaceFirst) {
        actionPattern = BackspaceBeforeCharacter;
      }
    } else {
      actionPattern = CharactersOnly;
    }
  } else if (containsBackspaces) {
    actionPattern = BackspacesOnly;
  }
  
  return actionPattern;
}

//TODO: check for mutliple backspaces
-(BOOL)isSinglePassThroughBackspace {
  BOOL isPassThrough =
  (self.event.keyCode == kVK_Delete)
  && (self.pattern == BackspacesOnly)
  && (self.backspaceCount == 1);
  return isPassThrough;
}


/*
 * Returns YES if we have applied an event to the client.
 * If NO, then (passthrough case) we are instructing the OS to apply the original event.
 * If any of the returned actions result in a change to the client, then return YES.
 * Changes to the client may be executed with insertText or by generating a KeyDown event.
 */
-(BOOL)handleActions {
  BOOL handledEvent = NO;
  
  NSLog(@"***SGS handleActions invoked, actions.count = %lu ", (unsigned long)self.actions.count);
  NSLog(@"event = %@", self.event);
  NSLog(@"client = %@", self.client);
  
  if (self.pattern == CharactersOnly) {
    NSLog(@"***SGS handleActions CharactersOnly");
    handledEvent = [self applyCharacterActions:0];
  } else if (self.pattern == BackspacesOnly) {
    NSLog(@"***SGS handleActions BackspacesOnly");
    if ([self isSinglePassThroughBackspace]) {
      NSLog(@"Identified pass-through backspace");
      // return NO to let backspace pass through
      handledEvent = NO;
    } else {
      // must generate backspace event(s)
      handledEvent = [self applyIsolatedBackspaceActions:self.backspaceCount];
    }
  } else if (self.pattern == BackspaceBeforeCharacter) {
    NSLog(@"***SGS handleActions BackspaceBeforeCharacter");
    handledEvent = [self applyCharacterActions:self.backspaceCount];
  } else {
    NSLog(@"***SGS Unimplemented pattern***");
  }
  
  return handledEvent;
}
  
/*
-(BOOL)doSomething {
  NSArray *actions = self.actions;
  BOOL handledEvent;
  BOOL consumedEvent;

  for (CoreAction *action in actions) {
    switch (action.actionType)
    {
      case CharacterAction: {
        BOOL consumedEvent  = [self applyCharacterActions];
        handledEvent = handledEvent || consumedEvent;
        break;
      }
      case EmitKeystrokeAction: {
        BOOL consumedEvent  = [self applyEmitKeystrokeAction:action event:self.event client:self.client];
        handledEvent = handledEvent || consumedEvent;
      break;
     }
      case BackspaceAction: {
        if ([self isPassThroughBackspace]) {
          NSLog(@"Identified pass-through backspace");
          // return NO to let backspace pass through
         handledEvent = NO;
        } else {
          NSLog(@"Applying backspace");
         BOOL consumedEvent  = [self applyBackspaceActions];
          handledEvent = handledEvent || consumedEvent;
        }
        break;
      }
      case EndAction: {
        NSLog(@"reached EndAction");
        break;
      }
      default: {
        NSLog(@"***SGS applyKeymanCoreActions, unhandled action of type %u", action.actionType);
        break;
      }
    }
  }

  return handledEvent;
}
*/
-(NSString*)collectOutputText {
  NSMutableString *output = [[NSMutableString alloc]init];
  for (CoreAction *action in [self.actions objectEnumerator]) {
    if (action.actionType==CharacterAction) {
      [output appendString:action.content];
    }
  }
  return output;
}

-(BOOL)applyCharacterActions:(int)replacementCount  {
  NSString *outputText = [self collectOutputText];
  NSRange selectionRange = [self.client selectedRange];
  NSRange contextRange = NSMakeRange(0, selectionRange.location);
  NSAttributedString *context = [self.client attributedSubstringFromRange:contextRange];
  NSLog(@"***SGS applyCharacterActions, replacementCount=%d, selectionRange.location=%lu", replacementCount, selectionRange.location);

  NSRange replacementRange;
  if (replacementCount > 0) {
    replacementRange = NSMakeRange(selectionRange.location-replacementCount, replacementCount);
  } else {
    replacementRange = NSMakeRange(NSNotFound, NSNotFound);
  }
  NSLog(@"***SGS applyCharacterActions, insertText %@ in replacementRange.start=%lu, replacementRange.length=%lu", outputText, (unsigned long)replacementRange.location, (unsigned long)replacementRange.length);
  
  [self.client insertText:outputText replacementRange:replacementRange];
  return YES;
}

/*
 * No way to apply backspaces directly, so just set counter for backspace events to be generated.
 */
-(BOOL)applyIsolatedBackspaceActions:(int)count  {
  NSLog(@"***SGS applyIsolatedBackspaceActions");
  self.backspaceEventsToGenerate = count;
  return YES;
}

-(BOOL)applyInvalidateContextAction:(CoreAction*)action event: (NSEvent *)event client:(id) client  {
  NSLog(@"insertText for action %@", action.content);
  return NO;
}

-(BOOL)applyEmitKeystrokeAction:(CoreAction*)action event: (NSEvent *)event client:(id) client  {
  NSLog(@"applyEmitKeystrokeAction for event with keycode: %hu", event.keyCode);
    
  // For other events that the Keyman engine does not have rules, just apply context changes
  // and let client handle the event
  NSString* charactersToAppend = nil;
  BOOL updateEngineContext = YES;
  unsigned short keyCode = event.keyCode;
  switch (keyCode) {
      case kVK_Delete:
          NSLog(@"***SGS applyEmitKeystrokeAction kVK_Return");
          // [self processUnhandledDeleteBack: sender updateEngineContext: &updateEngineContext];
          //[self.keySender sendBackspace:1];
          break;

      case kVK_LeftArrow:
      case kVK_RightArrow:
      case kVK_UpArrow:
      case kVK_DownArrow:
      case kVK_Home:
      case kVK_End:
      case kVK_PageUp:
      case kVK_PageDown:
          //_contextOutOfDate = YES;
          //updateEngineContext = NO;
          break;

      case kVK_Return:
      case kVK_ANSI_KeypadEnter:
          NSLog(@"***SGS applyEmitKeystrokeAction kVK_Return not handled");
          //charactersToAppend = @"\n";
          //[client insertText:charactersToAppend replacementRange:NSMakeRange(NSNotFound, NSNotFound)];
         break;

      default:
          {
              // NOTE: Although ch is usually the same as keyCode, when the option key is depressed (and
              // perhaps in some other cases) it may not be (keyCode can be 0). Likewise, the option key
              // can generate more than one character in event.characters.
              unichar ch = [event.characters characterAtIndex:0];
              if (keyCode < 0x33 || (ch >= 0x2A && ch <= 0x39)) { // Main keys, Numpad char range, normal punctuation
                  charactersToAppend = event.characters;
              }
              else {
                  // Other keys
              }
          }
          break;
  }
  /*
  if (charactersToAppend != nil) {
      if ([self.AppDelegate debugMode]) {
          NSLog(@"Adding \"%@\" to context buffer", charactersToAppend);
      }
      [self.contextBuffer appendString:charactersToAppend];
      if (_legacyMode) {
          _previousSelRange.location += charactersToAppend.length;
          _previousSelRange.length = 0;
      }
  }

  if (updateEngineContext) {
      [self.kme setContextBuffer:self.contextBuffer];
  }
   */
  return NO;
}
  

@end
