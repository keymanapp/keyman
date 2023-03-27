/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * CoreAction.m
 * CoreTesterApp
 * 
 * Created by Shawn Schantz on 2023-02-21.
 * 
 * A value object representing a single action returned by Keyman Core. A list
 * of Actions are returned by Core when processing a key down event and are
 * applied by the platform code to update the context.
 */

#import "CoreAction.h"
#import "KMEngine.h"  // included for Q_STR etc.

/*
// constants for compatibility with Keyman for Mac action representation
NSString *const Q_STR = @"Q_STR";         // KM_KBP_IT_CHAR
NSString *const Q_BACK = @"Q_BACK";       // KM_KBP_IT_BACK
NSString *const Q_DEADKEY = @"Q_DEADKEY"; // KM_KBP_IT_MARKER
NSString *const Q_NUL = @"Q_NUL";         // KM_KBP_IT_END?
NSString *const Q_BEEP = @"Q_BEEP";       // KM_KBP_IT_ALERT
NSString *const Q_RETURN = @"Q_RETURN";   // KM_KBP_IT_EMIT_KEYSTROKE?
NSString *const Q_SAVEOPT = @"Q_SAVEOPT"; // KM_KBP_IT_PERSIST_OPT
// none defined for KM_KBP_IT_INVALIDATE_CONTEXT, KM_KBP_IT_CAPSLOCK
*/

@implementation CoreAction

/*
 * Designated initializer
 */
-(instancetype)initWithType: (ActionType)type actionContent:(NSString*)content backspaceCount:(int)backspaceCount {
  self = [super init];
  if (self) {
    self->_actionType = type;
    switch (type)
    {
      case EndAction: {
        self->_typeName = @"End";
        break;
      }
      case CharacterAction: {
        self->_typeName = @"Character";
        self->_content = content;
        break;
      }
      case MarkerAction: {
        self->_typeName = @"Marker";
        break;
      }
      case AlertAction: {
        self->_typeName = @"Alert";
        break;
      }
      case BackspaceAction: {
        self->_typeName = @"Backspace";
        self->_backspaceCount = backspaceCount;
        break;
      }
      case PersistOptionAction: {
        self->_typeName = @"Persist Option";
        break;
      }
      case EmitKeystrokeAction: {
        self->_typeName = @"Emit Keystroke";
        break;
      }
      case InvalidateContextAction: {
        self->_typeName = @"Invalidate Context";
        break;
      }
      case CapsLockAction: {
        self->_typeName = @"Caps Lock";
        break;
      }
      default: {
        self->_typeName = @"Unknown";
      }
    }
  }
  return self;
}

/*
 * This is implemented so that there is no path to init a CoreAction object without
 * going through the designated initializer, initWithType. It blocks a direct call to super init which
 * would result in uninitialized fields. This init results in a hard-coded AlertAction and
 * should not be used.
 */
-(instancetype)init {
  return [self initWithType:AlertAction actionContent:@"" backspaceCount:0];
}

-(instancetype)initWithActionStruct:(km_kbp_action_item*)actionStruct coreHelper:(CoreHelper*)helper {
    switch (actionStruct->type)
    {
      case KM_KBP_IT_END: {
        self = [self initWithType: EndAction actionContent:@"" backspaceCount:0];
        break;
      }
      case KM_KBP_IT_CHAR: {
        NSString *characterString = [helper utf32ValueToString:actionStruct->character];
        self = [self initWithType: CharacterAction actionContent:characterString backspaceCount:0];
        NSLog(@"actionStruct->character decimal: %u, hex: %X", actionStruct->character, actionStruct->character);
        NSLog(@"converted unicode string: '%@'", characterString);
        break;
      }
      case KM_KBP_IT_MARKER: {
        self = [self initWithType: MarkerAction actionContent:@"" backspaceCount:0];
        break;
      }
      case KM_KBP_IT_ALERT: {
        self = [self initWithType: AlertAction actionContent:@"" backspaceCount:0];
        break;
      }
      case KM_KBP_IT_BACK: {
        self = [self initWithType: BackspaceAction actionContent:@"" backspaceCount:1];
        break;
      }
      case KM_KBP_IT_PERSIST_OPT: {
        self = [self initWithType: PersistOptionAction actionContent:@"" backspaceCount:0];
        break;
      }
      case KM_KBP_IT_EMIT_KEYSTROKE: {
        self = [self initWithType: EmitKeystrokeAction actionContent:@"" backspaceCount:0];
        break;
      }
      case KM_KBP_IT_INVALIDATE_CONTEXT: {
        self = [self initWithType: InvalidateContextAction actionContent:@"" backspaceCount:0];
        break;
      }
      case KM_KBP_IT_CAPSLOCK: {
        self = [self initWithType: CapsLockAction actionContent:@"" backspaceCount:0];
        break;
      }
      default: {
        self->_typeName = @"Unknown";
        NSLog(@"unrecognized type of km_kbp_action_item = %u\n", actionStruct->type);
      }
  }
  return self;
}

-(instancetype)initCharacterAction:(NSString*)content {
  self = [self initWithType: CharacterAction actionContent:content backspaceCount:0];
  return self;
}

-(instancetype)initBackspaceAction:(int)count {
  self = [self initWithType: BackspaceAction actionContent:@"" backspaceCount:count];
  return self;
}

-(NSString *)description
{
  NSString *actionDescription = @"";
  if (self.actionType == CharacterAction) {
    const unichar unicodeChar = [_content characterAtIndex:0];
    actionDescription = [[NSString alloc] initWithFormat:@"%u / 0x%X : '%@'", unicodeChar, unicodeChar, _content];
  }
  return actionDescription;
}

/*
 The legacy Keyman for Mac code represents each action returned from Keyman
 Engine as an NSDictionary. CoreWrapper returns CoreAction objects. This method
 converts a CoreAction object as an array of NSDictionary objects. This allows
 us to replace the key processing code in Keyman Engine with Keyman Core and
 CoreWrapper without rewriting the Keyman Input Method code consuming the
 actions.
 */
-(NSDictionary*) legacyDictionaryActionForActionObject:(CoreAction*)action {
  NSDictionary *actionMap = nil;
  
  switch (action.actionType)
  {
    case CharacterAction: {
      actionMap = [[NSDictionary alloc] initWithObjectsAndKeys:action.content, Q_STR, nil];
      break;
    }
    case MarkerAction: {
      actionMap = [[NSDictionary alloc] initWithObjectsAndKeys:[NSNumber numberWithUnsignedInteger:1], Q_DEADKEY, nil];
      break;
    }
    case AlertAction: {
      actionMap = [[NSDictionary alloc] initWithObjectsAndKeys:@"", Q_BEEP, nil];
      break;
    }
    case BackspaceAction: {
      actionMap = [[NSDictionary alloc] initWithObjectsAndKeys:[NSNumber numberWithUnsignedInteger:1], Q_BACK, nil];
      break;
    }
    // TODO: implement Persist Options
    case PersistOptionAction: {
      actionMap = [[NSDictionary alloc] initWithObjectsAndKeys:[NSNumber numberWithUnsignedInteger:1], Q_SAVEOPT, nil];
      break;
    }
    case EmitKeystrokeAction: {
      actionMap = [[NSDictionary alloc] initWithObjectsAndKeys:@"", Q_RETURN, nil];
      break;
    }
    case EndAction: {
      // purposely return nil for EndAction, as it is not needed by the legacy Keyman for Mac code
      break;
    }
    default: {
    }
  }

  return actionMap;
}


@end

