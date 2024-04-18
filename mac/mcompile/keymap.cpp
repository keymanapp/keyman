#include "keymap.h"



std::u16string get_character_From_Keycode(int dk, int ch , int shiftstate) {

  std::u16string character;
  std::vector<int> keyvals;
  keyvals.push_back(dk);
  keyvals.push_back(ch);

  TISInputSourceRef source = TISCopyCurrentKeyboardInputSource();
  CFDataRef layout_data = static_cast<CFDataRef>((TISGetInputSourceProperty(source, kTISPropertyUnicodeKeyLayoutData)));
  const UCKeyboardLayout* keyboard_layout = reinterpret_cast<const UCKeyboardLayout*>(CFDataGetBytePtr(layout_data));

  if (layout_data)
    character = get_character_From_Keycode(keyvals, shiftstate, keyboard_layout);

  return character;
}

std::u16string get_character_From_Keycode(std::vector<int> keyval, int shiftstate,const UCKeyboardLayout* keyboard_layout ) {  
  char16_t ch_array[3] = {L'\0'};
  UInt32 deadkeystate             = 0;
  UniCharCount maxStringlength    = 5;
  UniCharCount actualStringlength = 0;
  UniChar unicodeString[maxStringlength];
  OSStatus status;

  for ( int i =0; i < keyval.size(); i++) {

    status = UCKeyTranslate(keyboard_layout, keyval[i] ,kUCKeyActionDown, shiftstate, LMGetKbdType(), 0, &deadkeystate, maxStringlength, &actualStringlength, unicodeString );
    if( shiftstate %2 == 1 )                        // uneven:  seperate char -> `e
      ch_array[i] = (char16_t) unicodeString[0];
    else
      ch_array[0] = (char16_t) unicodeString[0];    // even:    combine char -> è
  }

  std::u16string  returnString(ch_array);
  return returnString;
}



//--------------------------

void fun3() {std::cout << "Hier ist fun3 von keymap.cpp...\n";}