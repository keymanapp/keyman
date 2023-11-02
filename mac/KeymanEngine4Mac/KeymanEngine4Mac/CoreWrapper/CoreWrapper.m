/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * CoreWrapper.m
 * Keyman
 * 
 * Created by Shawn Schantz on 2022-12-12.
 * 
 * A class to wrap an instance of a Keyman Core keyboard (a km_core_keyboard
 * object) and its associated objects. A new CoreWrapper is created whenever
 * Keyman for Mac switches to a new keyboard. The CoreWrapper allows Keyman
 * Engine to be mostly unaware of Keyman Core.
 */

#import "CoreWrapper.h"
#import "keyman_core_api.h"
#import "keyman_core_api_consts.h"

@interface CoreWrapper()

@property (readonly) km_core_keyboard * coreKeyboard;
@property (readonly) km_core_state *coreState;
@property (weak, nonatomic, readonly) CoreHelper *coreHelper;

@end

@implementation CoreWrapper

const int CORE_ENVIRONMENT_ARRAY_LENGTH = 6;

-(instancetype)initWithHelper:(CoreHelper*)helper kmxFilePath:(nullable NSString*) path {
  self = [super init];
  if (self) {
    _coreHelper = helper;
    
    // if the kmxFilePath has been provided, then load the keyboard now
    if (path != nil) {
      [self changeKeyboardWithKmxFilePath: path];
    }
  }
  return self;
}

-(void)clearCoreContext {
  [self clearContextUsingCore];
}
-(void)changeKeyboardWithKmxFilePath:(NSString*) path {
  if (path != nil) {
    @try {
      [self loadKeyboardUsingCore: path];
      [self readKeyboardAttributesUsingCore];
      [self createKeyboardStateUsingCore];
    }
    @catch (NSException *exception) {
      if (self.coreState) {
        km_core_state_dispose(self.coreState);
        _coreState = nil;
      }
      if (self.coreKeyboard) {
        km_core_keyboard_dispose(self.coreKeyboard);
        _coreKeyboard = nil;
      }
      //TODO: use NSError instead of NSException?
      @throw;
    }
  }
}

-(void) dealloc{
  if (self.coreState) {
    km_core_state_dispose(self.coreState);
  }
  if (self.coreKeyboard) {
    km_core_keyboard_dispose(self.coreKeyboard);
  }
  [self.coreHelper logDebugMessage:@"CoreWrapper dealloc called."];
}

-(void)loadKeyboardUsingCore:(NSString*) path {
  km_core_path_name keyboardPath = [path UTF8String];
  km_core_status result = km_core_keyboard_load(keyboardPath, &_coreKeyboard);
  
  if (result != KM_CORE_STATUS_OK) {
    NSString *message = [NSString stringWithFormat:@"Unexpected Keyman Core result: %u", result];
    [NSException raise:@"LoadKeyboardException" format:@"%@", message];
  }
}

-(void)readKeyboardAttributesUsingCore {
  if (self.coreKeyboard != nil) {
    const km_core_keyboard_attrs * keyboardAttributes;
    km_core_status result = km_core_keyboard_get_attrs(self.coreKeyboard, &keyboardAttributes);

    if (result==KM_CORE_STATUS_OK) {
      _keyboardVersion = [self.coreHelper createNSStringFromUnicharString:keyboardAttributes->version_string];
      _keyboardId = [self.coreHelper createNSStringFromUnicharString:keyboardAttributes->id];
      [self.coreHelper logDebugMessage:@"keyboardVersion = %@\n, keyboardId  = %@\n", _keyboardVersion, _keyboardId];
    } else {
      NSLog(@"km_core_keyboard_get_attrs() failed with result = %u\n", result );
    }
  }
}

-(void)createKeyboardStateUsingCore {
  km_core_status result = KM_CORE_STATUS_OK;
  
  // TODO: create once
  // create option list
  km_core_option_item coreEnvironment[CORE_ENVIRONMENT_ARRAY_LENGTH] = {0};

  if([CoreWrapper setupCoreEnvironment:coreEnvironment]) {
    // create state using keyboard and option list
    result = km_core_state_create(self.coreKeyboard, coreEnvironment, &_coreState);

    if (result != KM_CORE_STATUS_OK) {
      NSString *message = [NSString stringWithFormat:@"Unexpected Keyman Core result: %u", result];
      [NSException raise:@"CreateKeyboardStateException" format:@"%@", message];
    }
  } else {
      NSLog(@"CoreWrapper, Unable to set environment options for keyboard" );
  }
}

-(NSArray*)processEvent:(nonnull NSEvent *)event {
  NSEventModifierFlags modifiers = event.modifierFlags;
  
  // process key down events only
  if (event.type != NSEventTypeKeyDown) {
      return nil;
  }
  // do not process command keys
  if (modifiers & NSEventModifierFlagCommand) {
      return nil;
  }
  
  NSArray* actions = [self processMacVirtualKey:event.keyCode
                      withModifiers:modifiers
                        withKeyDown:YES];
  
  return [self.coreHelper optimizeActionArray:actions];
}

-(NSArray*)processMacVirtualKey:(unsigned short)macKeyCode
            withModifiers:(NSEventModifierFlags)modifiers
             withKeyDown:(BOOL) isKeyDown {
  NSArray *actions = nil;
  uint16_t windowsKeyCode = [self.coreHelper macVirtualKeyToWindowsVirtualKey:macKeyCode];
  uint32_t modifierState = [self.coreHelper macToKeymanModifier:modifiers];
  uint8_t keyDown = (isKeyDown) ? 1 : 0;

  if ([self processVirtualKey:windowsKeyCode
                        withModifier:modifierState
                  withKeyDown:keyDown]) {
    actions = [self loadActionsUsingCore];
  }
  return actions;
}

-(BOOL)processVirtualKey:(uint16_t)keyCode
            withModifier:(uint16_t)modifierState
             withKeyDown:(uint8_t) isKeyDown {

  km_core_status result = km_core_process_event(self.coreState, keyCode, modifierState, isKeyDown, KM_CORE_EVENT_FLAG_DEFAULT);

  if (result!=KM_CORE_STATUS_OK) {
    [self.coreHelper logDebugMessage:@"km_core_process_event() result = %u\n", result];
  }

  return (result==KM_CORE_STATUS_OK);
}

-(NSArray*)loadActionsForLastKeyProcessed {
  return [self loadActionsUsingCore];
}

-(NSArray*)loadActionsUsingCore {
  size_t actionCount = 0;
  km_core_action_item const * actionList =
      km_core_state_action_items(self.coreState, &actionCount);
  
  NSMutableArray *eventArray = [NSMutableArray arrayWithCapacity:actionCount];

  for (int i = 0; i < actionCount; i++) {
    km_core_action_item action = actionList[i];
    CoreAction *coreAction = [self createCoreActionForActionStruct:&action];
    [eventArray insertObject:coreAction atIndex:i];
  }
  
  return eventArray;
}

-(CoreAction*)createCoreActionForActionStruct:(km_core_action_item*)actionStruct {
  CoreAction* action = nil;
    switch (actionStruct->type)
    {
      case KM_CORE_IT_END: {
        action = [[CoreAction alloc] initWithType: EndAction actionContent:@"" backspaceCount:0 key:@"" value:@"" scope:0];
        break;
      }
      case KM_CORE_IT_CHAR: {
        NSString *characterString = [self.coreHelper utf32ValueToString:actionStruct->character];
        action = [[CoreAction alloc] initWithType: CharacterAction actionContent:characterString backspaceCount:0 key:@"" value:@"" scope:0];
        [self.coreHelper logDebugMessage:@"createCoreActionForActionStruct actionStruct->character decimal: %u, hex: %X", actionStruct->character, actionStruct->character];
        [self.coreHelper logDebugMessage:@"createCoreActionForActionStruct converted unicode string: '%@' length=%lu", characterString, characterString.length];
        break;
      }
      case KM_CORE_IT_MARKER: {
        action = [[CoreAction alloc] initWithType: MarkerAction actionContent:@"" backspaceCount:0 key:@"" value:@"" scope:0];
        break;
      }
      case KM_CORE_IT_ALERT: {
        action = [[CoreAction alloc] initWithType: AlertAction actionContent:@"" backspaceCount:0 key:@"" value:@"" scope:0];
        break;
      }
      case KM_CORE_IT_BACK: {
        km_core_backspace_item backspace = actionStruct->backspace;
        
        if (backspace.expected_type == KM_CORE_BT_CHAR) {
          NSString *charString = [self.coreHelper utf32ValueToString:backspace.expected_value];
          [self.coreHelper logDebugMessage:@"createCoreActionForActionStruct charString = %@", charString];
          action = [[CoreAction alloc] initCharacterBackspaceAction:charString];
          [self.coreHelper logDebugMessage:@"createCoreActionForActionStruct converted character backspace, expected value =%lu, expected type =%u", backspace.expected_value, backspace.expected_type];
        } else if(backspace.expected_type == KM_CORE_BT_MARKER) {
          action = [[CoreAction alloc] initMarkerBackspaceAction:actionStruct->backspace.expected_value];
          [self.coreHelper logDebugMessage:@"createCoreActionForActionStruct converted marker backspace, expected value =%lu, expected type =%u", backspace.expected_value, backspace.expected_type];
        } else {
          [self.coreHelper logDebugMessage:@"createCoreActionForActionStruct did not convert unknown backspace, expected value =%lu, expected type =%u", backspace.expected_value, backspace.expected_type];
        }
        break;
      }
      case KM_CORE_IT_PERSIST_OPT: {
        [self.coreHelper logDebugMessage:@"***createCoreActionForActionStruct Persist Options encountered."];
        km_core_option_item const * option = actionStruct->option;
        NSString *keyString = [self.coreHelper createNSStringFromUnicharString:option->key];
        NSString *valueString = [self.coreHelper createNSStringFromUnicharString:option->value];
        
        [self.coreHelper logDebugMessage:@"***createCoreActionForActionStruct converted Persist Options, key = %@, value = %@, scope = %d", keyString, valueString, option->scope];
        
        action = [[CoreAction alloc] initPersistOptionAction:keyString value:valueString scope:option->scope];
        break;
      }
      case KM_CORE_IT_EMIT_KEYSTROKE: {
        action = [[CoreAction alloc] initWithType: EmitKeystrokeAction actionContent:@"" backspaceCount:0 key:@"" value:@"" scope:0];
        break;
      }
      case KM_CORE_IT_INVALIDATE_CONTEXT: {
        action = [[CoreAction alloc] initWithType: InvalidateContextAction actionContent:@"" backspaceCount:0 key:@"" value:@"" scope:0];
        break;
      }
      case KM_CORE_IT_CAPSLOCK: {
        action = [[CoreAction alloc] initWithType: CapsLockAction actionContent:@"" backspaceCount:0 key:@"" value:@"" scope:0];
        break;
      }
      default: {
        NSLog(@"createCoreActionForActionStruct unrecognized type of km_core_action_item = %u\n", actionStruct->type);
      }
  }
  return action;
}

-(NSString *)getContextAsStringUsingCore {
  km_core_context * context =  km_core_state_context(self.coreState);
  
  km_core_context_item * contextItemsArray = nil;
  size_t contextLength = km_core_context_length(context);
  
  NSMutableString *contextString = [[NSMutableString alloc]init];

  if (contextLength==0) {
    [self.coreHelper logDebugMessage:@"CoreWrapper getContextAsStringUsingCore, context is empty."];
  } else {
    km_core_status result = km_core_context_get(context, &contextItemsArray);
    if (result==KM_CORE_STATUS_OK) {
      for (int i = 0; i < contextLength; i++) {
        km_core_context_item contextItem = contextItemsArray[i];
        if (contextItem.type == KM_CORE_CT_CHAR) {
          NSString *unicodeString = [self.coreHelper utf32ValueToString:contextItem.character];
          [contextString appendString:unicodeString];
        }
      }
    }
  }
  NSString *immutableString = [NSString stringWithString:contextString];
  
  // dispose of context items array
  if (contextItemsArray) {
    km_core_context_items_dispose(contextItemsArray);
  }

  [self.coreHelper logDebugMessage:@"CoreWrapper getContextAsStringUsingCore = %@", immutableString];
  return immutableString;
}

-(void)clearContextUsingCore {
  km_core_context * coreContext =  km_core_state_context(self.coreState);
  km_core_context_clear(coreContext);
  [self.coreHelper logDebugMessage:@"km_core_context_clear called"];
}

-(void)setContextIfNeeded:(NSString*)context {
  unichar const * unicharContext = [self.coreHelper createUnicharStringFromNSString:context];
  km_core_status result = km_core_state_context_set_if_needed(self.coreState, unicharContext);
  [self.coreHelper logDebugMessage:@"CoreWrapper setContextIfNeeded, context=%@, km_core_state_context_set_if_needed result=%i", context, result];
}

-(void)setContext:(NSString*)context {
  if (context.length == 0) {
    [self clearContextUsingCore];
  } else {
    char const *coreString = [context cStringUsingEncoding:NSUTF8StringEncoding];
    km_core_context_item *contextItemArray;
    
    // create array of context items
    km_core_status result = km_core_context_items_from_utf8(coreString, &contextItemArray);
    [self.coreHelper logDebugMessage:@"km_core_context_items_from_utf8, result=%i", result];
    
    // set the context in core using the array
    km_core_context * coreContext =  km_core_state_context(self.coreState);
    km_core_context_set(coreContext, contextItemArray);
    // dispose
    km_core_context_items_dispose(contextItemArray);
  }
}


-(NSString*)context {
  return [self getContextAsStringUsingCore];
}

//TODO: create and save as static
+(BOOL)setupCoreEnvironment:(km_core_option_item *) coreOptionArray {
  coreOptionArray[0].scope = KM_CORE_OPT_ENVIRONMENT;
  coreOptionArray[0].key = KM_CORE_KMX_ENV_BASELAYOUT;
  coreOptionArray[0].value = u"kbdus.dll";   // const char16_t*, encoded as UTF-16

  coreOptionArray[1].scope = KM_CORE_OPT_ENVIRONMENT;
  coreOptionArray[1].key = KM_CORE_KMX_ENV_BASELAYOUTALT;
  coreOptionArray[1].value = u"en-US";   // const char16_t*, encoded as UTF-16

  coreOptionArray[2].scope = KM_CORE_OPT_ENVIRONMENT;
  coreOptionArray[2].key = KM_CORE_KMX_ENV_SIMULATEALTGR;
  coreOptionArray[2].value = u"0";   // const char16_t*, encoded as UTF-16

  coreOptionArray[3].scope = KM_CORE_OPT_ENVIRONMENT;
  coreOptionArray[3].key = KM_CORE_KMX_ENV_BASELAYOUTGIVESCTRLRALTFORRALT;
  coreOptionArray[3].value = u"0";   // const char16_t*, encoded as UTF-16

  coreOptionArray[4].scope = KM_CORE_OPT_ENVIRONMENT;
  coreOptionArray[4].key = KM_CORE_KMX_ENV_PLATFORM;
  coreOptionArray[4].value = u"mac macos macosx hardware desktop native";   // const char16_t*, encoded as UTF-16
  
  coreOptionArray[5] = (km_core_option_item) {0};
  
  return TRUE;
}

-(BOOL)setOptionsForCore: (NSString *) key value:(NSString *) value {
  [self.coreHelper logDebugMessage:@"setOptionsForCore, key = %@, value = %@", key, value];
  
  // array of length 2, second item is terminating null struct
  km_core_option_item option[2] = {0};
  option[0].key = [self.coreHelper createUnicharStringFromNSString: key];
  option[0].value = [self.coreHelper createUnicharStringFromNSString: value];
  option[0].scope = KM_CORE_OPT_KEYBOARD;
  
  km_core_status result = km_core_state_options_update(self.coreState, &option[0]);
  [self.coreHelper logDebugMessage:@"setOptionsForCore, km_core_state_options_update result = %d", result];
  
  return (result==KM_CORE_STATUS_OK);
}

-(void)readCoreOptions: (km_core_cp const *) key {
  km_core_cp const * valueFromCore = nil;
  km_core_status result =
  km_core_state_option_lookup(self.coreState,
                             KM_CORE_OPT_KEYBOARD,
                             key,
                             &valueFromCore);
  if (result == KM_CORE_STATUS_OK) {
    if (valueFromCore) {
      [self.coreHelper logDebugMessage:@"km_core_state_option_lookup successful, current value set in core= %@", [self.coreHelper createNSStringFromUnicharString:valueFromCore]];
    } else {
      [self.coreHelper logDebugMessage:@"km_core_state_option_lookup returned nil"];
    }
  } else {
    if (valueFromCore) {
      [self.coreHelper logDebugMessage:@"km_core_state_option_lookup failed, result = %d", result];
    } else {
      [self.coreHelper logDebugMessage:@"km_core_state_option_lookup returned nil, result = %d", result];
    }
  }

}

@end
