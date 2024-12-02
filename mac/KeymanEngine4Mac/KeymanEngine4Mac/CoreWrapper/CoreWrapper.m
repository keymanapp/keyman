/*
 * Keyman is copyright (C) SIL International. MIT License.
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
#import "KMELogs.h"

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

-(CoreKeyboardInfo*) getKeyboardInfoForKmxFile:(NSString*)kmxFile {
  NSArray *keyArray = [self getKeyArray];
  CoreKeyboardInfo* info = [[CoreKeyboardInfo alloc] init:kmxFile keyArray: keyArray];
  return info;
}

/**
 * Get the list of Keys supported by the keyboard
 */
-(NSArray*)getKeyArray {
  NSMutableArray *keyArray = [[NSMutableArray alloc] initWithCapacity:0];
  km_core_keyboard_key *keyList;

  km_core_status result = km_core_keyboard_get_key_list(self.coreKeyboard, &keyList);

  for(km_core_keyboard_key* keyPtr = keyList; (keyPtr->key) != 0; keyPtr++) {
    uint16_t key = keyPtr->key;
    uint32_t modifier_flag = keyPtr->modifier_flag;
    
    if(key != 0) {
      CoreKey* coreKey = [[CoreKey alloc] init: key modifiers: modifier_flag];
      [keyArray addObject:coreKey];

      //os_log_debug([KMELogs coreLog], "key %d modifiers 0x%X coreKey %{public}@", key, modifier_flag, coreKey);
    }
  }

  km_core_keyboard_key_list_dispose(keyList);
  
  os_log_debug([KMELogs coreLog], "getKeyList returning %lu keys", (unsigned long)keyArray.count);

  return keyArray;
}

-(void) dealloc{
  if (self.coreState) {
    km_core_state_dispose(self.coreState);
  }
  if (self.coreKeyboard) {
    km_core_keyboard_dispose(self.coreKeyboard);
  }
  os_log_debug([KMELogs coreLog], "CoreWrapper dealloc called");
}

-(void)loadKeyboardUsingCore:(NSString*) path {
  km_core_path_name keyboardPath = [path UTF8String];
  NSError* dataError = nil;
  NSData *data = [NSData dataWithContentsOfFile:path options:0 error:&dataError];
  
  if (dataError != nil) {
    os_log_error([KMELogs coreLog], "loadKeyboardUsingCore, path: %{public}@\n dataError: %{public}@", path, dataError);
  } else {
    NSUInteger dataLength = data.length;
    os_log_info([KMELogs coreLog], "loadKeyboardUsingCore, path: %{public}@\n dataLength: %lu", path, dataLength);
    
    km_core_status result = km_core_keyboard_load_from_blob(keyboardPath,
                                                   data.bytes, dataLength, &_coreKeyboard);
    if (result != KM_CORE_STATUS_OK) {
      NSString *message = [NSString stringWithFormat:@"Unexpected Keyman Core result: %u", result];
      os_log_error([KMELogs coreLog], "loadKeyboardUsingCore, path: %{public}@\n core result: %{public}@", path, message);
      [NSException raise:@"LoadKeyboardException" format:@"%@", message];
    }
  }
}

-(void)readKeyboardAttributesUsingCore {
  if (self.coreKeyboard != nil) {
    const km_core_keyboard_attrs * keyboardAttributes;
    km_core_status result = km_core_keyboard_get_attrs(self.coreKeyboard, &keyboardAttributes);
    
    if (result==KM_CORE_STATUS_OK) {
      _keyboardVersion = [self.coreHelper createNSStringFromUnicharString:keyboardAttributes->version_string];
      _keyboardId = [self.coreHelper createNSStringFromUnicharString:keyboardAttributes->id];
      os_log_debug([KMELogs coreLog], "readKeyboardAttributesUsingCore, keyboardVersion: %{public}@, keyboardId: %{public}@\n", _keyboardVersion, _keyboardId);
    } else {
      os_log_error([KMELogs coreLog], "km_core_keyboard_get_attrs() failed with result = %u\n", result);
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
    os_log_error([KMELogs coreLog], "CoreWrapper, Unable to set environment options for keyboard");
  }
}

-(CoreKeyOutput*)processEvent:(nonnull NSEvent *)event {
  NSEventModifierFlags modifiers = event.modifierFlags;
  
  // process key down events only
  if (event.type != NSEventTypeKeyDown) {
    return nil;
  }
  // do not process command keys
  if (modifiers & NSEventModifierFlagCommand) {
    return nil;
  }
  
  return [self processMacVirtualKey:event.keyCode
                      withModifiers:modifiers withKeyDown:YES];
}

-(CoreKeyOutput*)processMacVirtualKey:(unsigned short)macKeyCode
                        withModifiers:(NSEventModifierFlags)modifiers
                          withKeyDown:(BOOL) isKeyDown {
  CoreKeyOutput *output = nil;
  uint16_t windowsKeyCode = [self.coreHelper macVirtualKeyToWindowsVirtualKey:macKeyCode];
  uint32_t modifierState = [self.coreHelper macToKeymanModifier:modifiers];
  uint8_t keyDown = (isKeyDown) ? 1 : 0;
  
  if ([self processVirtualKey:windowsKeyCode
                 withModifier:modifierState
                  withKeyDown:keyDown]) {
    output = [self loadOutputForLastKeyProcessed];
    os_log_debug([KMELogs coreLog], "processMacVirtualKey for macKeyCode: %d / 0x%X, core output: %{public}@", macKeyCode, macKeyCode, output);
  }
  return output;
}

-(BOOL)processVirtualKey:(uint16_t)keyCode
            withModifier:(uint16_t)modifierState
             withKeyDown:(uint8_t) isKeyDown {
  
  km_core_status result = km_core_process_event(self.coreState, keyCode, modifierState, isKeyDown, KM_CORE_EVENT_FLAG_DEFAULT);
  
  if (result!=KM_CORE_STATUS_OK) {
    os_log_error([KMELogs coreLog], "km_core_process_event() result = %u\n", result);
  }
  
  return (result==KM_CORE_STATUS_OK);
}

-(CoreKeyOutput*)loadOutputForLastKeyProcessed {
  return [self loadActionStructUsingCore];
}

-(CoreKeyOutput*)loadActionStructUsingCore {
  km_core_actions * actions = km_core_state_get_actions(self.coreState);
  CoreKeyOutput *output = [self createCoreKeyOutputForActionsStruct:actions];
  return output;
}

-(CoreKeyOutput*) createCoreKeyOutputForActionsStruct:(km_core_actions*)actions {
  NSString* text = [self.coreHelper utf32CStringToString:actions->output];
  NSDictionary* options = [self convertOptionsArray:actions->persist_options];
  CapsLockState capsLock = [self convertCapsLockState:actions->new_caps_lock_state];
  NSString* deletedText = [self.coreHelper utf32CStringToString:actions->deleted_context];
  
  CoreKeyOutput* coreKeyOutput = [[CoreKeyOutput alloc] init: actions->code_points_to_delete textToDelete:deletedText textToInsert:text optionsToPersist:options alert:actions->do_alert emitKeystroke:actions->emit_keystroke capsLockState:capsLock];

  return coreKeyOutput;
}

-(NSDictionary*)convertOptionsArray:(km_core_option_item*)options {
  NSMutableDictionary* optionsDictionary = nil;
  if (options) {
    optionsDictionary = [[NSMutableDictionary alloc] init];
    for (; options->key != 0; ++options) {
      unichar const * optionsKey = options->key;
      unichar const * valueKey = options->value;
      
      NSString *key = [self.coreHelper createNSStringFromUnicharString:optionsKey];
      NSString *value = [self.coreHelper createNSStringFromUnicharString:valueKey];
      [optionsDictionary setObject:value forKey:key];
    }
  }
  return optionsDictionary;
}

-(CapsLockState)convertCapsLockState:(km_core_caps_state)capsState {
  CapsLockState capsLock = Unchanged;
  switch(capsState) {
    case KM_CORE_CAPS_UNCHANGED:
      capsLock = Unchanged;
      break;
    case KM_CORE_CAPS_OFF:
      capsLock = Off;
      break;
    case KM_CORE_CAPS_ON:
      capsLock = On;
      break;
    default:
      capsLock = Unchanged;
      break;
  }
  return capsLock;
}

-(void)clearContextUsingCore {
  km_core_state_context_clear(self.coreState);
  os_log_debug([KMELogs coreLog], "km_core_state_context_clear called");
}

-(void)setContextIfNeeded:(NSString*)context {
  unichar const * unicharContext = [self.coreHelper createUnicharStringFromNSString:context];
  km_core_status result = km_core_state_context_set_if_needed(self.coreState, unicharContext);
  os_log_debug([KMELogs coreLog], "setContextIfNeeded, context=%{public}@, km_core_state_context_set_if_needed result=%d", context, result);
}

-(NSString*)contextDebug {
  km_core_cu * context = km_core_state_context_debug(self.coreState, KM_CORE_DEBUG_CONTEXT_CACHED);
  NSString *debugString = [self.coreHelper createNSStringFromUnicharString:context];
  km_core_cu_dispose(context);
  
  os_log_debug([KMELogs coreLog], "contextDebug = %{public}@", debugString);
  return debugString;
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
  os_log_debug([KMELogs coreLog], "setOptionsForCore, key = %@, value = %@", key, value);

  // array of length 2, second item is terminating null struct
  km_core_option_item option[2] = {0};
  option[0].key = [self.coreHelper createUnicharStringFromNSString: key];
  option[0].value = [self.coreHelper createUnicharStringFromNSString: value];
  option[0].scope = KM_CORE_OPT_KEYBOARD;
  
  km_core_status result = km_core_state_options_update(self.coreState, &option[0]);
  os_log_debug([KMELogs coreLog], "setOptionsForCore, km_core_state_options_update result = %d", result);

  return (result==KM_CORE_STATUS_OK);
}

-(void)readCoreOptions: (km_core_cu const *) key {
  km_core_cu const * valueFromCore = nil;
  km_core_status result =
  km_core_state_option_lookup(self.coreState,
                              KM_CORE_OPT_KEYBOARD,
                              key,
                              &valueFromCore);
  if (result == KM_CORE_STATUS_OK) {
    if (valueFromCore) {
      os_log_debug([KMELogs coreLog], "km_core_state_option_lookup successful, current value set in core= %@", [self.coreHelper createNSStringFromUnicharString:valueFromCore]);
    } else {
      os_log_debug([KMELogs coreLog], "km_core_state_option_lookup returned nil");
    }
  } else {
    if (valueFromCore) {
      os_log_debug([KMELogs coreLog], "km_core_state_option_lookup failed, result = %d", result);
    } else {
      os_log_debug([KMELogs coreLog], "km_core_state_option_lookup returned nil, result = %d", result);
    }
  }
  
}

@end
