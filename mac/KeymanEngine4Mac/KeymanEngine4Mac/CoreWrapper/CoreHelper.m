/**
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * CoreHelper.m
 * Keyman
 *
 * Created by Shawn Schantz on 2023-02-14.
 *
 * A class to encapsulate type conversions between Keyman Core C++ code and
 *  Keyman Engine for Mac Objective C code.
 *
 * Only a single instance of this class is created, and it is injected at init
 * time to the other classes like CoreWrapper and CoreAction that use it. The
 * only data it contains is reference data, and it should never change state.
 */

#import "CoreHelper.h"
#import "CoreAction.h"
#import "keyman_core_api.h"
#import "MacVKCodes.h"
#import "WindowsVKCodes.h"
#import "KMELogs.h"

const int VIRTUAL_KEY_ARRAY_SIZE = 0x80;
UInt32 VirtualKeyMap[VIRTUAL_KEY_ARRAY_SIZE];

@interface CoreHelper()
@end


@implementation CoreHelper

-(unichar const *) createUnicharStringFromNSString:(NSString *)string {
  NSString *nullTerminatedString = [string stringByAppendingString:@"\0"];
  if (![nullTerminatedString canBeConvertedToEncoding:NSUTF16LittleEndianStringEncoding]) {
    os_log_debug([KMELogs coreLog], "createUnicharStringFromNSString, canBeConvertedToEncoding false for NSUTF16LittleEndianStringEncoding");
    return nil;
  }
  
  NSData *data = [nullTerminatedString dataUsingEncoding:NSUTF16LittleEndianStringEncoding];
  return (unichar const *) data.bytes;
}

-(NSString *) createNSStringFromUnicharString:(unichar const *)string {
  return [[NSString alloc] initWithCharacters:string length:[self unicharStringLength:string]];
}

-(unsigned long long) unicharStringLength:(unichar const *)string {
  unsigned long long length = 0llu;
  if(NULL == string) return length;
  
  while('\0' != string[length])
    length++;
  
  return length;
}

/**
 * Return the string length (number of unicode scalar, or UTF32, values) excluding the terminating null.
 */
-(unsigned long) scalarValueStringLength:(UTF32Char const *)string {
  unsigned long length = 0lu;
  if(NULL == string) {
    return length;
  }
  while('\0' != string[length]) {
    length++;
  }
  return length;
}

-(instancetype)init {
  self = [super init];
  if (self) {
    [self initVirtualKeyMapping];
  }
  return self;
}

-(unsigned short) macVirtualKeyToWindowsVirtualKey:(unsigned short) keyCode {
  if ((keyCode<0) || (keyCode>=VIRTUAL_KEY_ARRAY_SIZE)) {
    return 0;
  } else {
    return VirtualKeyMap[keyCode];
  }
}

-(UTF32Char)macToKeymanModifier:(NSEventModifierFlags)modifiers {
  UTF32Char keymanModifiers = 0;
  if ([self isShiftKey:modifiers]) {
    keymanModifiers |= KM_CORE_MODIFIER_SHIFT;
  }
  
  if([self isLeftOptionKey:modifiers]) {
    keymanModifiers |= KM_CORE_MODIFIER_LALT;
  } else if([self isRightOptionKey:modifiers]) {
    keymanModifiers |= KM_CORE_MODIFIER_RALT;
  } else if ([self isOptionKey:modifiers]) {
    keymanModifiers |= KM_CORE_MODIFIER_ALT;
  }
  
  if ([self isLeftControlKey:modifiers]) {
    keymanModifiers |= KM_CORE_MODIFIER_LCTRL;
  } else if ([self isRightControlKey:modifiers]) {
    keymanModifiers |= KM_CORE_MODIFIER_RCTRL;
  } else if ([self isControlKey:modifiers]) {
    keymanModifiers |= KM_CORE_MODIFIER_CTRL;
  }
  
  if ([self isCapsLockKey:modifiers]) {
    keymanModifiers |= KM_CORE_MODIFIER_CAPS;
  }
  // setting NOCAPS in Core breaks keyboards like EuroLatin and Amharic
  // apparently better to simply leave the CAPS bit clear
  /*else {
   keymanModifiers |= KM_CORE_MODIFIER_NOCAPS;
   }*/
  
  os_log_debug([KMELogs coreLog], "macToKeymanModifier, macModifiers: %lu / ox%lX keymanModifiers: %d / 0x%X", (unsigned long)modifiers, modifiers, keymanModifiers, keymanModifiers);
  return keymanModifiers;
}

-(BOOL)isShiftKey:(NSEventModifierFlags) modifiers {
  return (modifiers & NSEventModifierFlagShift) == NSEventModifierFlagShift;
}

-(BOOL)isCapsLockKey:(NSEventModifierFlags) modifiers {
  return (modifiers & NSEventModifierFlagCapsLock) == NSEventModifierFlagCapsLock;
}

-(BOOL)isLeftControlKey:(NSEventModifierFlags) modifiers {
  return ((modifiers & MK_LEFT_CTRL_MASK) == MK_LEFT_CTRL_MASK);
}

-(BOOL)isRightControlKey:(NSEventModifierFlags) modifiers {
  return ((modifiers & MK_RIGHT_CTRL_MASK) == MK_RIGHT_CTRL_MASK);
}

-(BOOL)isControlKey:(NSEventModifierFlags) modifiers {
  return (modifiers & NSEventModifierFlagControl) == NSEventModifierFlagControl;
}

-(BOOL)isLeftOptionKey:(NSEventModifierFlags) modifiers {
  return ((modifiers & MK_LEFT_ALT_MASK) == MK_LEFT_ALT_MASK);
}

-(BOOL)isRightOptionKey:(NSEventModifierFlags) modifiers {
  return ((modifiers & MK_RIGHT_ALT_MASK) == MK_RIGHT_ALT_MASK);
}

-(BOOL)isOptionKey:(NSEventModifierFlags) modifiers {
  return (modifiers & NSEventModifierFlagOption) == NSEventModifierFlagOption;
}

/**
 * Converts a UTF-32 unicode scalar value to an NSString.
 * Use this method to convert values from Keyman Core of type km_core_usv, equivalent to uint32_t
 */
-(NSString*)utf32ValueToString:(UTF32Char)scalarValue {
  NSData * characterData = [[NSData alloc] initWithBytes:&scalarValue length:sizeof(scalarValue)];
  
  NSString *characterString=[[NSString alloc] initWithBytes:[characterData bytes] length:[characterData length] encoding:NSUTF32LittleEndianStringEncoding];
  os_log_debug([KMELogs coreLog], "utf32ValueToString data: '%@', string: %@", characterData, characterString);
  return characterString;
}

/**
 * Converts a null-terminated string of UTF-32 unicode scalar values to an NSString.
 */
-(NSString*)utf32CStringToString:(UTF32Char*)utf32CString {
  NSString *characterString = nil;
  long utf32StringLength = [self scalarValueStringLength:utf32CString];
  
  if (utf32StringLength > 0) {
    characterString=[[NSString alloc] initWithBytes:utf32CString length:utf32StringLength*4 encoding:NSUTF32LittleEndianStringEncoding];
  }
  
  return characterString;
}


// TODO: alphabetize instead of using whatever this order is

// Creates a VK map to convert Mac VK codes to Windows VK codes
- (void)initVirtualKeyMapping {
  VirtualKeyMap[MVK_A] = VK_KEY_A;                        // A
  VirtualKeyMap[MVK_S] = VK_KEY_S;                        // S
  VirtualKeyMap[MVK_D] = VK_KEY_D;                        // D
  VirtualKeyMap[MVK_F] = VK_KEY_F;                        // F
  VirtualKeyMap[MVK_H] = VK_KEY_H;                        // H
  VirtualKeyMap[MVK_G] = VK_KEY_G;                        // G
  VirtualKeyMap[MVK_Z] = VK_KEY_Z;                        // Z
  VirtualKeyMap[MVK_X] = VK_KEY_X;                        // X
  VirtualKeyMap[MVK_C] = VK_KEY_C;                        // C
  VirtualKeyMap[MVK_V] = VK_KEY_V;                        // V
  //    0x0A  = nil
  VirtualKeyMap[MVK_B] = VK_KEY_B;                        // B
  VirtualKeyMap[MVK_Q] = VK_KEY_Q;                        // Q
  VirtualKeyMap[MVK_W] = VK_KEY_W;                        // W
  VirtualKeyMap[MVK_E] = VK_KEY_E;                        // E
  VirtualKeyMap[MVK_R] = VK_KEY_R;                        // R
  VirtualKeyMap[MVK_Y] = VK_KEY_Y;                        // Y
  VirtualKeyMap[MVK_T] = VK_KEY_T;                        // T
  VirtualKeyMap[MVK_1] = VK_KEY_1;                        // 1
  VirtualKeyMap[MVK_2] = VK_KEY_2;                        // 2
  VirtualKeyMap[MVK_3] = VK_KEY_3;                        // 3
  VirtualKeyMap[MVK_4] = VK_KEY_4;                        // 4
  VirtualKeyMap[MVK_6] = VK_KEY_6;                        // 6
  VirtualKeyMap[MVK_5] = VK_KEY_5;                        // 5
  VirtualKeyMap[MVK_EQUAL] = VK_EQUAL;                    // =
  VirtualKeyMap[MVK_9] = VK_KEY_9;                        // 9
  VirtualKeyMap[MVK_7] = VK_KEY_7;                        // 7
  VirtualKeyMap[MVK_MINUS] = VK_MINUS;                    // -
  VirtualKeyMap[MVK_8] = VK_KEY_8;                        // 8
  VirtualKeyMap[MVK_0] = VK_KEY_0;                        // 0
  VirtualKeyMap[MVK_RIGHT_BRACKET] = VK_RIGHT_BRACKET;    // ]
  VirtualKeyMap[MVK_O] = VK_KEY_O;                        // O
  VirtualKeyMap[MVK_U] = VK_KEY_U;                        // U
  VirtualKeyMap[MVK_LEFT_BRACKET] = VK_LEFT_BRACKET;      // [
  VirtualKeyMap[MVK_I] = VK_KEY_I;                        // I
  VirtualKeyMap[MVK_P] = VK_KEY_P;                        // P
  VirtualKeyMap[MVK_ENTER] = VK_ENTER;                    // <Enter>
  VirtualKeyMap[MVK_L] = VK_KEY_L;                        // L
  VirtualKeyMap[MVK_J] = VK_KEY_J;                        // J
  VirtualKeyMap[MVK_QUOTE] = VK_QUOTE;                    // '
  VirtualKeyMap[MVK_K] = VK_KEY_K;                        // K
  VirtualKeyMap[MVK_SEMICOLON] = VK_SEMICOLON;            // ;
  VirtualKeyMap[MVK_BACKSLASH] = VK_BACKSLASH;            // '\'
  VirtualKeyMap[MVK_COMMA] = VK_COMMA;                    // ,
  VirtualKeyMap[MVK_SLASH] = VK_SLASH;                    // '/'
  VirtualKeyMap[MVK_N] = VK_KEY_N;                        // N
  VirtualKeyMap[MVK_M] = VK_KEY_M;                        // M
  VirtualKeyMap[MVK_PERIOD] = VK_PERIOD;                  // .
  VirtualKeyMap[MVK_TAB] = VK_TAB;                        // <Tab>
  VirtualKeyMap[MVK_SPACE] = VK_SPACE;                    // <Space>
  VirtualKeyMap[MVK_GRAVE] = VK_GRAVE;                    // `
  VirtualKeyMap[MVK_BACKSPACE] = VK_BACKSPACE;            // <Backspace>
  VirtualKeyMap[MVK_OEM102] = VK_OEM_102;                 // 102nd key (ISO keyboards)
  
  // Num Pad
  VirtualKeyMap[0x41] = VK_NUMPAD_DECIMAL;                // Decimal .
  VirtualKeyMap[0x43] = VK_NUMPAD_MULTIPLY;               // Multiply *
  VirtualKeyMap[0x45] = VK_NUMPAD_ADD;                    // Add +
  VirtualKeyMap[0x47] = 0x00;                             // <Clear>      *** Undefined ***
  VirtualKeyMap[0x4B] = VK_NUMPAD_DIVIDE;                 // Divide /
  VirtualKeyMap[0x4C] = 0x00;                             // <Enter>      *** Undefined ***
  VirtualKeyMap[0x4E] = VK_NUMPAD_SUBTRACT;               // Subtract -
  VirtualKeyMap[0x51] = 0x00;                             // Equal =      *** Undefined ***
  VirtualKeyMap[0x52] = VK_NUMPAD0;                       // 0
  VirtualKeyMap[0x53] = VK_NUMPAD1;                       // 1
  VirtualKeyMap[0x54] = VK_NUMPAD2;                       // 2
  VirtualKeyMap[0x55] = VK_NUMPAD3;                       // 3
  VirtualKeyMap[0x56] = VK_NUMPAD4;                       // 4
  VirtualKeyMap[0x57] = VK_NUMPAD5;                       // 5
  VirtualKeyMap[0x58] = VK_NUMPAD6;                       // 6
  VirtualKeyMap[0x59] = VK_NUMPAD7;                       // 7
  VirtualKeyMap[0x5B] = VK_NUMPAD8;                       // 8
  VirtualKeyMap[0x5C] = VK_NUMPAD9;                       // 9
}

@end


