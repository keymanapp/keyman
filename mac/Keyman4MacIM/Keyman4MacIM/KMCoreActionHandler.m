/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * KMCoreActionHandler.m
 * Keyman
 * 
 * Created by Shawn Schantz on 2023-04-21.
 * 
 * Processes an NSArray of CoreAction objects and determines how they should be
 * applied to the client application. Creates a KMActionHandlerResult object to instruct
 * Keyman how to apply the changes to the client to conform to the CoreAction
 * objects.
 *
 * To apply the changes in the correct order in the client application, the
 * pattern in which the actions occur are analyzed and classified with the
 * ActionPattern enum. Some patterns require a different strategy to preserve
 * the order.
 */

#import "KMInputMethodEventHandler.h"
#import "KMCoreActionHandler.h"

@interface KMCoreActionHandler ()

typedef enum {
  BackspacesOnly,
  CharactersOnly,
  BackspaceBeforeCharacter,
  CharacterBeforeBackspace,
  None
} ClientTextOutputPattern;

@property (readonly) NSArray *actions;
@property (readonly) unsigned short keyCode;        /* device-independent key number */
@property (readonly) ClientTextOutputPattern pattern;
@property int backspaceCount;
@property BOOL useEvents;
@property BOOL emitKeystroke;
@end

@implementation KMCoreActionHandler

-(KMInputMethodAppDelegate *)appDelegate {
  return (KMInputMethodAppDelegate *)[NSApp delegate];
}

-(instancetype)initWithActions:(NSArray*)actions keyCode: (unsigned short)keyCode  {
  self = [super init];
  if (self) {
    _actions = actions;
    _keyCode = keyCode;
    _backspaceCount = 0;
    _emitKeystroke = NO;
  }
  return self;
}

/*
 * Loop through the actions and identify which pattern of which actions of type
 * CharacterAction and CharacterBackspaceAction occur. We use this pattern to
 * determine how to apply changes to the client application.
 */
-(ClientTextOutputPattern)analyzeActions:(NSArray*)actions {
  BOOL containsCharacters = NO;
  BOOL containsBackspaces = NO;
  BOOL backspaceFirst = NO;
  BOOL characterFirst = NO;
  // TODO: adjust backspace count for code points with more than one code unit?
  // TODO: backspace event and replacement range must be tested for surrogate pairs
  
  // first, loop through the actions to summarize the contents
  // only looking at characters and backspaces of characters
  for (CoreAction *action in [actions objectEnumerator]) {
    if (action.isCharacter) {
      containsCharacters = YES;
      if (!containsBackspaces) {
        characterFirst = YES;
      }
    } else if (action.isCharacterBackspace) {
      containsBackspaces = YES;
      self.backspaceCount++;
      if (!containsCharacters) {
        backspaceFirst = YES;
      }
    }
  }

  [self.appDelegate logDebugMessage:@"analyzeActions, containsCharacters = %d, containsBackspaces = %d ", containsCharacters,  containsBackspaces];

  ClientTextOutputPattern actionPattern = None;
  // second, asssign an actionPattern type based on what we found
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

-(BOOL)isSinglePassThroughBackspace {
  BOOL isPassThrough =
  (self.keyCode == kVK_Delete)
  && (self.pattern == BackspacesOnly)
  && (self.backspaceCount == 1);
  return isPassThrough;
}

/*
 * Returns result object to instruct Input Method how to apply changes to client text.
 */
-(KMActionHandlerResult*)handleActions {
  KMActionHandlerResult *result = nil;
  
  [self.appDelegate logDebugMessage:@"handleActions invoked, actions.count = %lu ", (unsigned long)self.actions.count];
  
  // examine the pattern of actions for required text inserted and deleted
  _pattern = [self analyzeActions:self.actions];

  // use the ClientTextOutputPattern to create the result object
  switch (self.pattern) {
    case CharactersOnly:
      result = [self buildResultForCharactersOnly];
      break;
    case BackspacesOnly:
      if ([self isSinglePassThroughBackspace]) {
        result = [self buildResultForSinglePassThroughBackspaceNoText];
      } else {
        result = [self buildResultForMultipleBackspacesNoText];
      }
      break;
    case BackspaceBeforeCharacter:
      result = [self buildResultForBackspacesBeforeText];
      break;
    case None:
      result = [self buildResultForNoCharactersOrBackspaces];
      break;
    default:
      NSLog(@"KMCoreActionHandler handleActions(), Unimplemented pattern***");
  }

  return result;
}

/**
 * The event is marked as handled and the result includes the new text string to insert in the client
 */
-(KMActionHandlerResult*)buildResultForCharactersOnly {
  [self.appDelegate logDebugMessage:@"buildResultForCharactersOnly"];
  return [[KMActionHandlerResult alloc] initForActions:self.actions handledEvent:YES backspaceCount:0 textToInsert:[self collectOutputText]];
}

/**
 * Mark the event as NOT handled, we simply let the original backspace event pass through to the client without manipulating the client text
 */
-(KMActionHandlerResult*)buildResultForSinglePassThroughBackspaceNoText {
  [self.appDelegate logDebugMessage:@"buildResultForSinglePassThroughBackspaceNoText"];
  return [[KMActionHandlerResult alloc] initForActions:self.actions handledEvent:NO backspaceCount:0 textToInsert:@""];
}

/**
 * Multiple backspaces with no text will be handled by generating events
 */
-(KMActionHandlerResult*)buildResultForMultipleBackspacesNoText {
  [self.appDelegate logDebugMessage:@"buildResultForMultipleBackspacesNoText"];
  return [[KMActionHandlerResult alloc] initForActions:self.actions handledEvent:YES backspaceCount:self.backspaceCount textToInsert:@""];
}

/**
 * For backspaces needed before text, we will insert after backspaces are completed
 */
-(KMActionHandlerResult*)buildResultForBackspacesBeforeText {
  [self.appDelegate logDebugMessage:@"buildResultForBackspacesBeforeText"];
  return [[KMActionHandlerResult alloc] initForActions:self.actions handledEvent:YES backspaceCount:self.backspaceCount textToInsert:[self collectOutputText]];
}

/**
 * For case with no actions of type CharacterAction or CharacterBackspaceAction
 */
-(KMActionHandlerResult*)buildResultForNoCharactersOrBackspaces {
  [self.appDelegate logDebugMessage:@"buildResultForNoCharactersOrBackspaces"];
  return [[KMActionHandlerResult alloc] initForActions:self.actions handledEvent:NO backspaceCount:0 textToInsert:@""];
}


-(NSString*)collectOutputText {
  NSMutableString *output = [[NSMutableString alloc]init];
  for (CoreAction *action in [self.actions objectEnumerator]) {
    if (action.actionType==CharacterAction) {
      [output appendString:action.content];
    }
  }
  return output;
}

@end

@implementation KMActionHandlerResult
-(instancetype)initForActions:(NSArray*)actions handledEvent:(BOOL)handledEvent backspaceCount:(int)backspaces textToInsert:(NSString*)text {
  self = [super init];
  if (self) {
    _handledEvent = handledEvent;
    _backspaceCount = backspaces;
    _textToInsert = text;
    _operations = [self populateOperationListForActions:actions];
  }
  return self;
}

-(NSArray*) populateOperationListForActions:(NSArray*)actions {
  NSMutableArray* operationsList = [NSMutableArray arrayWithCapacity:actions.count];
  BOOL addedCompositeOperation = NO;
  
  for (CoreAction *action in [actions objectEnumerator])
  {
    switch(action.actionType) {
      case CharacterAction:
      case CharacterBackspaceAction:
        /**
         * only create the composite operation if we are handling the event, and, then, only if we haven't already created one yet
         */
        if(self.handledEvent) {
          if(!addedCompositeOperation) {
            KMActionOperation *operation = [[KMActionOperation alloc] initForCompositeAction:self.textToInsert backspaceCount:self.backspaceCount];
            [operationsList addObject:operation];
            addedCompositeOperation = YES;
          }
        } else {
          /**
           * if we are not handling the event (letting backspace passthrough), then just make an operation for the action
           * so that it can be used to update the context
           */
          KMActionOperation *operation = [[KMActionOperation alloc] initForSimpleAction:action];
          [operationsList addObject:operation];
        }
        break;
      case MarkerAction:
      case MarkerBackspaceAction:
      case AlertAction:
      case PersistOptionAction:
      case EmitKeystrokeAction:
      case InvalidateContextAction:
      case CapsLockAction: {
        KMActionOperation *operation = [[KMActionOperation alloc] initForSimpleAction:action];
        [operationsList addObject:operation];
        break;
      }
      case EndAction:
        // this should never happen as it has been removed during optimization
        break;
    }
  }

  return [operationsList copy];
}

@end

@implementation KMActionOperation

-(instancetype)initForSimpleAction:(CoreAction*)action {
  self = [super init];
  if (self) {
    _action = action;
    _textToInsert = @"";
    _backspaceCount = 0;
    _isForSimpleAction = YES;
    _isForCompositeAction = NO;
  }
  return self;
}

-(instancetype)initForCompositeAction:(NSString*)textToInsert backspaceCount:(int)backspaces {
  self = [super init];
  if (self) {
    _action = nil;
    _textToInsert = textToInsert;
    _backspaceCount = backspaces;
    _isForSimpleAction = NO;
    _isForCompositeAction = YES;
  }
  return self;
}

-(NSString *)description
{
  return [[NSString alloc] initWithFormat: @"%@ %@ textToInsert: %@ backspaces: %d", self.isForSimpleAction?@"simpleAction":@"compositeAction", self.action, self.textToInsert, self.backspaceCount];
}

-(BOOL)hasTextToInsert {
  return (self.textToInsert.length > 0);
}

-(BOOL)hasBackspaces {
  return (self.backspaceCount > 0);
}

-(BOOL)isBackspaceOnlyScenario {
  return (self.hasBackspaces && !self.hasTextToInsert);
}

-(BOOL)isTextOnlyScenario {
  return (!self.hasBackspaces && self.hasTextToInsert);
}

-(BOOL)isTextAndBackspaceScenario {
  return (self.hasBackspaces && self.hasTextToInsert);
}

@end
