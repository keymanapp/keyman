/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * CoreHelper.m
 * CoreTesterApp
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
#import "keyboardprocessor.h"
#import "KMEngine.h"  // included for VKMap
#import "ActionArrayOptimizer.h"
#import "MacVKCodes.h"

// TODO move VKMap here from KMEngine
const int VIRTUAL_KEY_ARRAY_SIZE = 0x80;
//uint32_t VirtualKeyMap[VIRTUAL_KEY_ARRAY_SIZE];

@interface CoreHelper()
@property (strong, nonatomic, readonly) ActionArrayOptimizer *optimizer;
@end


@implementation CoreHelper

-(instancetype)init {
  self = [super init];
  if (self) {
    _optimizer = [[ActionArrayOptimizer alloc] init];
    //[self setVKMapping];
    }
    return self;
}

-(unsigned short) macVirtualKeyToWindowsVirtualKey:(unsigned short) keyCode {
  if ((keyCode<0) || (keyCode>=VIRTUAL_KEY_ARRAY_SIZE)) {
    return 0;
  } else {
    return VKMap[keyCode];
  }
}

/*
 The legacy Keyman for Mac code represents each action returned from Keyman
 Engine as an NSDictionary. CoreWrapper returns CoreAction objects. This method
 converts an array of CoreAction objects to an array of NSDictionary objects.
 This allows us to replace the key processing code in Keyman Engine with Keyman
 Core and CoreWrapper without rewriting the Keyman Input Method code consuming
 the actions.

 Note that the CoreAction of type EndAction produced by Keyman Core is not
 expected by Keyman for Mac and is removed during this conversion.
 */
-(NSArray*)actionObjectArrayToLegacyActionMapArray:(NSArray*)actionArray {

  // legacy code expects the map array to be optimized, so we can do that before converting to NSDictionary
  NSArray *optimizedArray = [self.optimizer actionArrayToOptimizedActionArray:actionArray];
  
  NSMutableArray *mapArray = [[NSMutableArray alloc] init];
  for (CoreAction *action in optimizedArray) {
    NSLog(@"%@\n", action.typeName.description);
    NSDictionary *actionMap = [action legacyDictionaryActionForActionObject:action];
    if (actionMap) {
      [mapArray addObject:actionMap];
    }
  }
  
  return mapArray;
}

-(UInt32)macToKeymanModifier:(NSEventModifierFlags)modifiers {
  UInt32 keymanModifiers = 0;
  if([self isShiftKey:modifiers]) {
    keymanModifiers |= KM_KBP_MODIFIER_SHIFT;
  }
  if([self isLeftControlKey:modifiers]) {
    keymanModifiers |= KM_KBP_MODIFIER_LCTRL;
  }
  if([self isRightControlKey:modifiers]) {
    keymanModifiers |= KM_KBP_MODIFIER_RCTRL;
  }
  if([self isLeftOptionKey:modifiers]) {
    keymanModifiers |= KM_KBP_MODIFIER_LALT;
  }
  if([self isRightOptionKey:modifiers]) {
    keymanModifiers |= KM_KBP_MODIFIER_RALT;
  }

  NSLog(@"macToKeymanModifier result  = %u", (unsigned int)keymanModifiers);
  return keymanModifiers;
}

-(BOOL)isShiftKey:(NSEventModifierFlags) modifiers {
  return (modifiers & NSEventModifierFlagShift) == NSEventModifierFlagShift;
}

-(BOOL)isLeftControlKey:(NSEventModifierFlags) modifiers {
  return ((modifiers & MK_LEFT_CTRL_MASK) == MK_LEFT_CTRL_MASK);
}

-(BOOL)isRightControlKey:(NSEventModifierFlags) modifiers {
  return ((modifiers & MK_RIGHT_CTRL_MASK) == MK_RIGHT_CTRL_MASK);
}

-(BOOL)isLeftOptionKey:(NSEventModifierFlags) modifiers {
  return ((modifiers & MK_LEFT_ALT_MASK) == MK_LEFT_ALT_MASK);
}

-(BOOL)isRightOptionKey:(NSEventModifierFlags) modifiers {
  return ((modifiers & MK_RIGHT_ALT_MASK) == MK_RIGHT_ALT_MASK);
}

/*
 Converts a UTF-32 unicode scalar value to an NSString.
 Use this method to convert values from Keyman Core of type km_kbp_usv, equivalent to uint32_t
 */
-(NSString*)utf32ValueToString:(UInt32)scalarValue {
  NSData * characterData = [[NSData alloc] initWithBytes:&scalarValue length:sizeof(scalarValue)];
  NSLog(@"characterData: '%@'", characterData);

  NSString *characterString=[[NSString alloc] initWithBytes:[characterData bytes] length:[characterData length] encoding:NSUTF32LittleEndianStringEncoding];
  return characterString;
}


/*
// Creates a VK map to convert Mac VK codes to Windows VK codes
- (void)setVKMapping {
    VKMap[MVK_A] = VK_KEY_A;                        // A
    VKMap[MVK_S] = VK_KEY_S;                        // S
    VKMap[MVK_D] = VK_KEY_D;                        // D
    VKMap[MVK_F] = VK_KEY_F;                        // F
    VKMap[MVK_H] = VK_KEY_H;                        // H
    VKMap[MVK_G] = VK_KEY_G;                        // G
    VKMap[MVK_Z] = VK_KEY_Z;                        // Z
    VKMap[MVK_X] = VK_KEY_X;                        // X
    VKMap[MVK_C] = VK_KEY_C;                        // C
    VKMap[MVK_V] = VK_KEY_V;                        // V
    //    0x0A  = nil
    VKMap[MVK_B] = VK_KEY_B;                        // B
    VKMap[MVK_Q] = VK_KEY_Q;                        // Q
    VKMap[MVK_W] = VK_KEY_W;                        // W
    VKMap[MVK_E] = VK_KEY_E;                        // E
    VKMap[MVK_R] = VK_KEY_R;                        // R
    VKMap[MVK_Y] = VK_KEY_Y;                        // Y
    VKMap[MVK_T] = VK_KEY_T;                        // T
    VKMap[MVK_1] = VK_KEY_1;                        // 1
    VKMap[MVK_2] = VK_KEY_2;                        // 2
    VKMap[MVK_3] = VK_KEY_3;                        // 3
    VKMap[MVK_4] = VK_KEY_4;                        // 4
    VKMap[MVK_6] = VK_KEY_6;                        // 6
    VKMap[MVK_5] = VK_KEY_5;                        // 5
    VKMap[MVK_EQUAL] = VK_EQUAL;                    // =
    VKMap[MVK_9] = VK_KEY_9;                        // 9
    VKMap[MVK_7] = VK_KEY_7;                        // 7
    VKMap[MVK_MINUS] = VK_MINUS;                    // -
    VKMap[MVK_8] = VK_KEY_8;                        // 8
    VKMap[MVK_0] = VK_KEY_0;                        // 0
    VKMap[MVK_RIGHT_BRACKET] = VK_RIGHT_BRACKET;    // ]
    VKMap[MVK_O] = VK_KEY_O;                        // O
    VKMap[MVK_U] = VK_KEY_U;                        // U
    VKMap[MVK_LEFT_BRACKET] = VK_LEFT_BRACKET;      // [
    VKMap[MVK_I] = VK_KEY_I;                        // I
    VKMap[MVK_P] = VK_KEY_P;                        // P
    VKMap[MVK_ENTER] = VK_ENTER;                    // <Enter>
    VKMap[MVK_L] = VK_KEY_L;                        // L
    VKMap[MVK_J] = VK_KEY_J;                        // J
    VKMap[MVK_QUOTE] = VK_QUOTE;                    // '
    VKMap[MVK_K] = VK_KEY_K;                        // K
    VKMap[MVK_SEMICOLON] = VK_SEMICOLON;            // ;
    VKMap[MVK_BACKSLASH] = VK_BACKSLASH;            // '\'
    VKMap[MVK_COMMA] = VK_COMMA;                    // ,
    VKMap[MVK_SLASH] = VK_SLASH;                    // '/'
    VKMap[MVK_N] = VK_KEY_N;                        // N
    VKMap[MVK_M] = VK_KEY_M;                        // M
    VKMap[MVK_PERIOD] = VK_PERIOD;                  // .
    VKMap[MVK_TAB] = VK_TAB;                        // <Tab>
    VKMap[MVK_SPACE] = VK_SPACE;                    // <Space>
    VKMap[MVK_GRAVE] = VK_GRAVE;                    // `
    VKMap[MVK_BACKSPACE] = VK_BACKSPACE;            // <Backspace>
    VKMap[MVK_OEM102] = VK_OEM_102;                 // 102nd key (ISO keyboards)

    // Num Pad
    VKMap[0x41] = VK_NUMPAD_DECIMAL;                // Decimal .
    VKMap[0x43] = VK_NUMPAD_MULTIPLY;               // Multiply *
    VKMap[0x45] = VK_NUMPAD_ADD;                    // Add +
    VKMap[0x47] = 0x00;                             // <Clear>      *** Undefined ***
    VKMap[0x4B] = VK_NUMPAD_DIVIDE;                 // Divide /
    VKMap[0x4C] = 0x00;                             // <Enter>      *** Undefined ***
    VKMap[0x4E] = VK_NUMPAD_SUBTRACT;               // Subtract -
    VKMap[0x51] = 0x00;                             // Equal =      *** Undefined ***
    VKMap[0x52] = VK_NUMPAD0;                       // 0
    VKMap[0x53] = VK_NUMPAD1;                       // 1
    VKMap[0x54] = VK_NUMPAD2;                       // 2
    VKMap[0x55] = VK_NUMPAD3;                       // 3
    VKMap[0x56] = VK_NUMPAD4;                       // 4
    VKMap[0x57] = VK_NUMPAD5;                       // 5
    VKMap[0x58] = VK_NUMPAD6;                       // 6
    VKMap[0x59] = VK_NUMPAD7;                       // 7
    VKMap[0x5B] = VK_NUMPAD8;                       // 8
    VKMap[0x5C] = VK_NUMPAD9;                       // 9
}
*/

@end


