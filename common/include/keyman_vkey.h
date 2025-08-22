/*
 * Keyman is copyright (C) SIL International. MIT License.
 * Create Date: 16 Jul 2025
 * Author:      Sabine Schmitt
 * VKCodes for keyman
 */

namespace km_vk {

#define VK_LBUTTON          0x01
#define VK_RBUTTON          0x02
#define VK_CANCEL           0x03
#define VK_MBUTTON          0x04

#define VK_BACKSPACE        0x08
#define VK_BACK             0x08
#define VK_TAB              0x09

#define VK_CLEAR            0x0C
#define VK_RETURN           0x0D
#define VK_ENTER            0x0D

#define VK_SHIFT            0x10
#define VK_CONTROL          0x11
#define VK_MENU             0x12
#define VK_PAUSE            0x13
#define VK_CAPITAL          0x14
#define VK_KANA             0x15
#define VK_HANGEUL          0x15
#define VK_HANGUL           0x15
#define VK_IME_ON           0x16
#define VK_JUNJA            0x17
#define VK_FINAL            0x18
#define VK_HANJA            0x19
#define VK_KANJI            0x19
#define VK_IME_OFF          0x1A
#define VK_ESCAPE           0x1B
#define VK_CONVERT          0x1C
#define VK_NONCONVERT       0x1D
#define VK_ACCEPT           0x1E
#define VK_MODECHANGE       0x1F
#define VK_SPACE            0x20
#define VK_PRIOR            0x21
#define VK_NEXT             0x22
#define VK_END              0x23
#define VK_HOME             0x24
#define VK_LEFT             0x25
#define VK_UP               0x26
#define VK_RIGHT            0x27
#define VK_DOWN             0x28
#define VK_SELECT           0x29
#define VK_PRINT            0x2A
#define VK_EXECUTE          0x2B
#define VK_SNAPSHOT         0x2C
#define VK_INSERT           0x2D
//#define VK_DECIMAL          0x2E  // _S2 if I use it here kmdecomp complains because of duplicate macro definition here and in WinUser.h

#define VK_KEY_0            0x30
#define VK_KEY_1            0x31
#define VK_KEY_2            0x32
#define VK_KEY_3            0x33
#define VK_KEY_4            0x34
#define VK_KEY_5            0x35
#define VK_KEY_6            0x36
#define VK_KEY_7            0x37
#define VK_KEY_8            0x38
#define VK_KEY_9            0x39

#define VK_KEY_A            0x41
#define VK_KEY_B            0x42
#define VK_KEY_C            0x43
#define VK_KEY_D            0x44
#define VK_KEY_E            0x45
#define VK_KEY_F            0x46
#define VK_KEY_G            0x47
#define VK_KEY_H            0x48
#define VK_KEY_I            0x49
#define VK_KEY_J            0x4A
#define VK_KEY_K            0x4B
#define VK_KEY_L            0x4C
#define VK_KEY_M            0x4D
#define VK_KEY_N            0x4E
#define VK_KEY_O            0x4F
#define VK_KEY_P            0x50
#define VK_KEY_Q            0x51
#define VK_KEY_R            0x52
#define VK_KEY_S            0x53
#define VK_KEY_T            0x54
#define VK_KEY_U            0x55
#define VK_KEY_V            0x56
#define VK_KEY_W            0x57
#define VK_KEY_X            0x58
#define VK_KEY_Y            0x59
#define VK_KEY_Z            0x5A

#define VK_NUMPAD0          0x60
#define VK_NUMPAD1          0x61
#define VK_NUMPAD2          0x62
#define VK_NUMPAD3          0x63
#define VK_NUMPAD4          0x64
#define VK_NUMPAD5          0x65
#define VK_NUMPAD6          0x66
#define VK_NUMPAD7          0x67
#define VK_NUMPAD8          0x68
#define VK_NUMPAD9          0x69
#define VK_NUMPAD_MULTIPLY  0x6A
#define VK_NUMPAD_ADD       0x6B

#define VK_NUMPAD_SUBTRACT  0x6D
#define VK_NUMPAD_DECIMAL   0x6E
#define VK_NUMPAD_DIVIDE    0x6F
#define VK_DIVIDE           0x6F

#define VK_NUMLOCK          0x90

#define VK_LSHIFT           0xA0
#define VK_RSHIFT           0xA1
#define VK_LCONTROL         0xA2
#define VK_RCONTROL         0xA3
#define VK_LMENU            0xA4
#define VK_RMENU            0xA5

#define VK_OEM_1            0xBA  
#define VK_COLON            0xBA
#define VK_SEMICOLON        0xBA
#define VK_OEM_PLUS         0xBB
#define VK_EQUAL            0xBB
#define VK_OEM_COMMA        0xBC
#define VK_COMMA            0xBC
#define VK_OEM_MINUS        0xBD
#define VK_HYPHEN           0xBD
#define VK_MINUS            0xBD
#define VK_OEM_PERIOD       0xBE
#define VK_PERIOD           0xBE
#define VK_OEM_2            0xBF
#define VK_SLASH            0xBF
#define VK_OEM_3            0xC0
#define VK_ACCENT           0xC0
#define VK_GRAVE            0xC0
#define VK_LEFT_BRACKET     0xDB
#define VK_LBRKT            0xDB
#define VK_OEM_4            0xDB
#define VK_BACKSLASH        0xDC
#define VK_BKSLASH          0xDC
#define VK_OEM_5            0xDC
#define VK_RIGHT_BRACKET    0xDD
#define VK_RBRKT            0xDD
#define VK_OEM_6            0xDD
#define VK_QUOTE            0xDE
#define VK_OEM_7            0xDE
#define VK_xDF              0xDF
#define VK_OEM_8            0xDF
#define VK_OEM_102          0xE2
#define VK_OEM_CLEAR        0xFE

/*
 * VK__MAX _S2 defines the highest virtual key code defined in the system = 0xFF.  Custom VK codes start at 256
 */
#define VK__MAX 255


}  // namespace km_vk