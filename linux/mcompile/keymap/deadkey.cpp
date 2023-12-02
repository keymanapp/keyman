#include "keymap.h"
#include "deadkey.h"

#include <xkbcommon/xkbcommon.h>

int  createDK_ComposeTable(v_dw_2D & dk_ComposeTable){

  // values taken from: https://help.ubuntu.com/community/GtkComposeTable
  //dk_ComposeTable[i][0] : First
  //dk_ComposeTable[i][1] : Second
  //dk_ComposeTable[i][2] : Third
  //dk_ComposeTable[i][3] : Character
  //dk_ComposeTable[i][4] : Unicode-Value

  v_dw_1D line;

	// Diacritics and punctuation
    line.push_back(convertNamesToIntegerValue(L"apostrophe"));
    line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
    line.push_back(convertNamesToIntegerValue(L"acute accent"));
    line.push_back(0x00B4);
  	dk_ComposeTable.push_back(line); 
  	line.clear();


	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"space"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Apostrophe"));
	line.push_back(0x27); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"space"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Apostrophe"));
	line.push_back(0x27); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"parenleft"));
	line.push_back(convertNamesToIntegerValue(L"space"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Breve"));
	line.push_back(0x02D8); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"space"));
	line.push_back(convertNamesToIntegerValue(L"parenleft"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Breve"));
	line.push_back(0x02D8); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"space"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Caron"));
	line.push_back(0x02C7); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"space"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Caron"));
	line.push_back(0x02C7); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Cedilla"));
	line.push_back(0x00B8); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"space"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Circumflex accent"));
	line.push_back(0x005E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L"space"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Circumflex accent"));
	line.push_back(0x005E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"space"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Circumflex accent"));
	line.push_back(0x005E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"space"));
	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Circumflex accent"));
	line.push_back(0x005E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Diaeresis"));
	line.push_back(0x00A8); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L"space"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Grave accent"));
	line.push_back(0x60); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"space"));
	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Grave accent"));
	line.push_back(0x60); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"em dash —"));
	line.push_back(0x2014); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"en dash –"));
	line.push_back(0x2013); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"exclam"));
	line.push_back(convertNamesToIntegerValue(L"exclam"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"inverted exclamation mark"));
	line.push_back(0x00A1); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"question"));
	line.push_back(convertNamesToIntegerValue(L"question"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"inverted question mark"));
	line.push_back(0x00BF); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"exclam"));
	line.push_back(convertNamesToIntegerValue(L"question"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"interrobang"));
	line.push_back(0x203D); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"parenleft"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"left Curly Bracket"));
	line.push_back(0x007B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"parenleft"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"left Curly Bracket"));
	line.push_back(0x007B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"left single Quotation Mark"));
	line.push_back(0x2018); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"left single Quotation Mark"));
	line.push_back(0x2018); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"parenleft"));
	line.push_back(convertNamesToIntegerValue(L"parenleft"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"left Square Bracket"));
	line.push_back(0x005B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"left pointing Double Angle Quotation Mark"));
	line.push_back(0x00AB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Macron"));
	line.push_back(0x00AF); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Macron"));
	line.push_back(0x00AF); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Macron"));
	line.push_back(0x00AF); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Macron"));
	line.push_back(0x00AF); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Macron"));
	line.push_back(0x00AF); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"space"));
	line.push_back(convertNamesToIntegerValue(L"space"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Non-breaking Space"));
	line.push_back(0x00A0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"parenright"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"right Curly Bracket"));
	line.push_back(0x007D); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"parenright"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"right Curly Bracket"));
	line.push_back(0x007D); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"right single Quotation Mark"));
	line.push_back(0x2019); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"right single Quotation Mark"));
	line.push_back(0x2019); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"parenright"));
	line.push_back(convertNamesToIntegerValue(L"parenright"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"right Square Bracket"));
	line.push_back(0x005D); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"right pointing Double Angle Quotation Mark"));
	line.push_back(0x00BB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"space"));
	line.push_back(convertNamesToIntegerValue(L"SoftHyphen (word break)"));
	line.push_back(0x00AD); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	// Currency
	line.push_back(convertNamesToIntegerValue(L"bar"));
	line.push_back(convertNamesToIntegerValue(L"C"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Cent sign"));
	line.push_back(0x00A2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"bar"));
	line.push_back(convertNamesToIntegerValue(L"c"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Cent sign"));
	line.push_back(0x00A2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"C"));
	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Cent sign"));
	line.push_back(0x00A2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"c"));
	line.push_back(convertNamesToIntegerValue(L"bar"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Cent sign"));
	line.push_back(0x00A2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"c"));
	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Cent sign"));
	line.push_back(0x00A2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"C"));
	line.push_back(convertNamesToIntegerValue(L"bar"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Cent sign"));
	line.push_back(0x00A2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L"C"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Cent sign"));
	line.push_back(0x00A2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L"c"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Cent sign"));
	line.push_back(0x00A2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"C"));
	line.push_back(convertNamesToIntegerValue(L"equal"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Euro sign"));
	line.push_back(0x20AC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"c"));
	line.push_back(convertNamesToIntegerValue(L"equal"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Euro sign"));
	line.push_back(0x20AC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L"equal"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Euro sign"));
	line.push_back(0x20AC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L"equal"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Euro sign"));
	line.push_back(0x20AC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"equal"));
	line.push_back(convertNamesToIntegerValue(L"C"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Euro sign"));
	line.push_back(0x20AC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"equal"));
	line.push_back(convertNamesToIntegerValue(L"c"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Euro sign"));
	line.push_back(0x20AC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"equal"));
	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Euro sign"));
	line.push_back(0x20AC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"equal"));
	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Euro sign"));
	line.push_back(0x20AC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"equal"));
	line.push_back(convertNamesToIntegerValue(L"L"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Lira sign"));
	line.push_back(0x00A3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"L"));
	line.push_back(convertNamesToIntegerValue(L"equal"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Lira sign"));
	line.push_back(0x00A3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"l"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Pound sign"));
	line.push_back(0x00A3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"L"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Pound sign"));
	line.push_back(0x00A3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"L"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Pound sign"));
	line.push_back(0x00A3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"l"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Pound sign"));
	line.push_back(0x00A3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"R"));
	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Rupee sign"));
	line.push_back(0x20a8); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"equal"));
	line.push_back(convertNamesToIntegerValue(L"Y"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Yen sign"));
	line.push_back(0x00A5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"equal"));
	line.push_back(convertNamesToIntegerValue(L"y"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Yen sign"));
	line.push_back(0x00A5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"Y"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Yen sign"));
	line.push_back(0x00A5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"y"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Yen sign"));
	line.push_back(0x00A5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Y"));
	line.push_back(convertNamesToIntegerValue(L"equal"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Yen sign"));
	line.push_back(0x00A5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Y"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Yen sign"));
	line.push_back(0x00A5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"y"));
	line.push_back(convertNamesToIntegerValue(L"equal"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Yen sign"));
	line.push_back(0x00A5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"y"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Yen sign"));
	line.push_back(0x00A5); 
	dk_ComposeTable.push_back(line);  
	line.clear();


	// Vulgar Fractions
	line.push_back(convertNamesToIntegerValue(L"1"));
	line.push_back(convertNamesToIntegerValue(L"2"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"One-half"));
	line.push_back(0x00BD); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"1"));
	line.push_back(convertNamesToIntegerValue(L"3"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"One-third"));
	line.push_back(0x2153); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"2"));
	line.push_back(convertNamesToIntegerValue(L"3"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Two-thirds"));
	line.push_back(0x2154); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"1"));
	line.push_back(convertNamesToIntegerValue(L"4"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"One-quarter"));
	line.push_back(0x00BC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"3"));
	line.push_back(convertNamesToIntegerValue(L"4"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Three-quarters"));
	line.push_back(0x00BE); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"1"));
	line.push_back(convertNamesToIntegerValue(L"5"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"One-fifth"));
	line.push_back(0x2155); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"2"));
	line.push_back(convertNamesToIntegerValue(L"5"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Two-fifths"));
	line.push_back(0x2156); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"3"));
	line.push_back(convertNamesToIntegerValue(L"5"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Three-fifths"));
	line.push_back(0x2157); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"4"));
	line.push_back(convertNamesToIntegerValue(L"5"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Four-fifths"));
	line.push_back(0x2158); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"1"));
	line.push_back(convertNamesToIntegerValue(L"6"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"One-sixth"));
	line.push_back(0x2159); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"1"));
	line.push_back(convertNamesToIntegerValue(L"8"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"One-eighth"));
	line.push_back(0x215B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"3"));
	line.push_back(convertNamesToIntegerValue(L"8"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Three-eighths"));
	line.push_back(0x215C); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"5"));
	line.push_back(convertNamesToIntegerValue(L"8"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Five-eighths"));
	line.push_back(0x215D); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	// Mathematical Symbols and Signs
	line.push_back(convertNamesToIntegerValue(L"7"));
	line.push_back(convertNamesToIntegerValue(L"8"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Seven-eighths"));
	line.push_back(0x215E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"backslash"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Backslash bar"));
	line.push_back(0x233f); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"backslash"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Backslash bar"));
	line.push_back(0x233f); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"0"));
	line.push_back(convertNamesToIntegerValue(L"C"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Copyright sign"));
	line.push_back(0x00A9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"0"));
	line.push_back(convertNamesToIntegerValue(L"c"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Copyright sign"));
	line.push_back(0x00A9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L"C"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Copyright sign"));
	line.push_back(0x00A9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L"c"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Copyright sign"));
	line.push_back(0x00A9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L"C"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Copyright sign"));
	line.push_back(0x00A9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L"c"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Copyright sign"));
	line.push_back(0x00A9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"C"));
	line.push_back(convertNamesToIntegerValue(L"0"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Copyright sign"));
	line.push_back(0x00A9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"C"));
	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Copyright sign"));
	line.push_back(0x00A9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"C"));
	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Copyright sign"));
	line.push_back(0x00A9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"c"));
	line.push_back(convertNamesToIntegerValue(L"0"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Copyright sign"));
	line.push_back(0x00A9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"c"));
	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Copyright sign"));
	line.push_back(0x00A9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"c"));
	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Copyright sign"));
	line.push_back(0x00A9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"parenleft"));
	line.push_back(convertNamesToIntegerValue(L"c"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Copyright sign"));
	line.push_back(0x00A9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Degree sign"));
	line.push_back(0x00B0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"0"));
	line.push_back(convertNamesToIntegerValue(L"asterisk"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Degree sign"));
	line.push_back(0x00B0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asterisk"));
	line.push_back(convertNamesToIntegerValue(L"0"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Degree sign"));
	line.push_back(0x00B0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Diamond operator"));
	line.push_back(0x22c4); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Diamond operator"));
	line.push_back(0x22c4); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"colon"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Division sign"));
	line.push_back(0x00F7); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"colon"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Division sign"));
	line.push_back(0x00F7); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"feminine ordinal indicator"));
	line.push_back(0x00AA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"feminine ordinal indicator"));
	line.push_back(0x00AA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"feminine ordinal indicator"));
	line.push_back(0x00AA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"feminine ordinal indicator"));
	line.push_back(0x00AA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"equal"));
	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Identical to"));
	line.push_back(0x2261); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"8"));
	line.push_back(convertNamesToIntegerValue(L"8"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Infinity"));
	line.push_back(0x221e); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"equal"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Less-than or equal to"));
	line.push_back(0x2264); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L"equal"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Greater-than or equal to"));
	line.push_back(0x2265); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"masculine ordinal indicator"));
	line.push_back(0x00BA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"masculine ordinal indicator"));
	line.push_back(0x00BA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"masculine ordinal indicator"));
	line.push_back(0x00BA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"masculine ordinal indicator"));
	line.push_back(0x00BA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Micro sign"));
	line.push_back(0x00B5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Micro sign"));
	line.push_back(0x00B5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Micro sign"));
	line.push_back(0x00B5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Micro sign"));
	line.push_back(0x00B5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Middle·Dot"));
	line.push_back(0x00B7); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Middle·Dot"));
	line.push_back(0x00B7); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"x"));
	line.push_back(convertNamesToIntegerValue(L"x"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Multiplication sign"));
	line.push_back(0x00D7); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L"equal"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Not equal to"));
	line.push_back(0x2260); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"equal"));
	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Not equal to"));
	line.push_back(0x2260); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L"equal"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Not equal to"));
	line.push_back(0x2260); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Not sign"));
	line.push_back(0x00AC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Not sign"));
	line.push_back(0x00AC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"plus"));
	line.push_back(convertNamesToIntegerValue(L"plus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Number sign (Octothorpe)"));
	line.push_back(0x23); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"exclam"));
	line.push_back(convertNamesToIntegerValue(L"P"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Pilcrow sign"));
	line.push_back(0x00B6); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"exclam"));
	line.push_back(convertNamesToIntegerValue(L"p"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Pilcrow sign"));
	line.push_back(0x00B6); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"p"));
	line.push_back(convertNamesToIntegerValue(L"exclam"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Pilcrow sign"));
	line.push_back(0x00B6); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"P"));
	line.push_back(convertNamesToIntegerValue(L"exclam"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Pilcrow sign"));
	line.push_back(0x00B6); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"plus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"plusminus sign"));
	line.push_back(0x00B1); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"plus"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"plusminus sign"));
	line.push_back(0x00B1); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L"R"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Registered sign"));
	line.push_back(0x00AE); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L"r"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Registered sign"));
	line.push_back(0x00AE); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L"R"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Registered sign"));
	line.push_back(0x00AE); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L"r"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Registered sign"));
	line.push_back(0x00AE); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"parenleft"));
	line.push_back(convertNamesToIntegerValue(L"r"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Registered sign"));
	line.push_back(0x00AE); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Slash bar"));
	line.push_back(0x233f); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Slash bar"));
	line.push_back(0x233f); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"T"));
	line.push_back(convertNamesToIntegerValue(L"M"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Trademark sign"));
	line.push_back(0x2122); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"T"));
	line.push_back(convertNamesToIntegerValue(L"m"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Trademark sign"));
	line.push_back(0x2122); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"t"));
	line.push_back(convertNamesToIntegerValue(L"M"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Trademark sign"));
	line.push_back(0x2122); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"t"));
	line.push_back(convertNamesToIntegerValue(L"m"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Trademark sign"));
	line.push_back(0x2122); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"reverse Solidus"));
	line.push_back(0x005C); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"reverse Solidus"));
	line.push_back(0x005C); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"reverse Solidus"));
	line.push_back(0x005C); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"0"));
	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Section sign"));
	line.push_back(0x00A7); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"0"));
	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Section sign"));
	line.push_back(0x00A7); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"exclam"));
	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Section sign"));
	line.push_back(0x00A7); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"exclam"));
	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Section sign"));
	line.push_back(0x00A7); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Section sign"));
	line.push_back(0x00A7); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Section sign"));
	line.push_back(0x00A7); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L"0"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Section sign"));
	line.push_back(0x00A7); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L"exclam"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Section sign"));
	line.push_back(0x00A7); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L"0"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Section sign"));
	line.push_back(0x00A7); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Section sign"));
	line.push_back(0x00A7); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Section sign"));
	line.push_back(0x00A7); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L"exclam"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Section sign"));
	line.push_back(0x00A7); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"v"));
	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Square root"));
	line.push_back(0x221a); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L"v"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Square root"));
	line.push_back(0x221a); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"bracketleft"));
	line.push_back(convertNamesToIntegerValue(L"bracketright"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"squish quad"));
	line.push_back(0x2337); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"0"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Superscript Zero"));
	line.push_back(0x2070); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"0"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Superscript Zero"));
	line.push_back(0x2070); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"1"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Superscript One"));
	line.push_back(0x00B9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"1"));
	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Superscript One"));
	line.push_back(0x00B9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"1"));
	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Superscript One"));
	line.push_back(0x00B9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"1"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Superscript One"));
	line.push_back(0x00B9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L"1"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Superscript One"));
	line.push_back(0x00B9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L"1"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Superscript One"));
	line.push_back(0x00B9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"2"));
	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Superscript Two"));
	line.push_back(0x00B2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"2"));
	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Superscript Two"));
	line.push_back(0x00B2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"2"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Superscript Two"));
	line.push_back(0x00B2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"2"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Superscript Two"));
	line.push_back(0x00B2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L"2"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Superscript Two"));
	line.push_back(0x00B2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L"2"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Superscript Two"));
	line.push_back(0x00B2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"3"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Superscript Three"));
	line.push_back(0x00B3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"3"));
	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Superscript Three"));
	line.push_back(0x00B3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"3"));
	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Superscript Three"));
	line.push_back(0x00B3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"3"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Superscript Three"));
	line.push_back(0x00B3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L"3"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Superscript Three"));
	line.push_back(0x00B3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L"3"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Superscript Three"));
	line.push_back(0x00B3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciiTilde (~)"));
	line.push_back(convertNamesToIntegerValue(L"space"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Tilde"));
	line.push_back(0x007E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"space"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Tilde"));
	line.push_back(0x007E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"space"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Tilde"));
	line.push_back(0x007E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"space"));
	line.push_back(convertNamesToIntegerValue(L"asciiTilde"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"Tilde"));
	line.push_back(0x007E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"vertical line"));
	line.push_back(0x007C); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"l"));
	line.push_back(convertNamesToIntegerValue(L"v"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"vertical line"));
	line.push_back(0x007C); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"L"));
	line.push_back(convertNamesToIntegerValue(L"V"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"vertical line"));
	line.push_back(0x007C); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"vertical line"));
	line.push_back(0x007C); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"v"));
	line.push_back(convertNamesToIntegerValue(L"l"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"vertical line"));
	line.push_back(0x007C); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"V"));
	line.push_back(convertNamesToIntegerValue(L"L"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"vertical line"));
	line.push_back(0x007C); 
	dk_ComposeTable.push_back(line);  
	line.clear();
	
	// Latin majuscules
	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with acute"));
	line.push_back(0x00C1); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with acute"));
	line.push_back(0x00C1); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with acute"));
	line.push_back(0x00C1); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with acute"));
	line.push_back(0x00C1); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L"parenleft"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with breve"));
	line.push_back(0x102); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"parenleft"));
	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with breve"));
	line.push_back(0x102); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with circumflex"));
	line.push_back(0x00C2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with circumflex"));
	line.push_back(0x00C2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with circumflex"));
	line.push_back(0x00C2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with circumflex"));
	line.push_back(0x00C2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with diaeresis"));
	line.push_back(0x00C4); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with diaeresis"));
	line.push_back(0x00C4); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with diaeresis"));
	line.push_back(0x00C4); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with diaeresis"));
	line.push_back(0x00C4); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with grave"));
	line.push_back(0x00C0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with grave"));
	line.push_back(0x00C0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with ogonek"));
	line.push_back(0x104); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with ogonek"));
	line.push_back(0x104); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with ring above"));
	line.push_back(0x00C5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L"asterisk"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with ring above"));
	line.push_back(0x00C5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asterisk"));
	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with ring above"));
	line.push_back(0x00C5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L"asciiTilde"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with tilde"));
	line.push_back(0x00C3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with tilde"));
	line.push_back(0x00C3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciiTilde"));
	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with tilde"));
	line.push_back(0x00C3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital A with tilde"));
	line.push_back(0x00C3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"A"));
	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital AE ligature"));
	line.push_back(0x00C6); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"B"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital B with dot above"));
	line.push_back(0x100); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"B"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital B with dot above"));
	line.push_back(0x100); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"C"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital C with acute"));
	line.push_back(0x106); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"C"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital C with acute"));
	line.push_back(0x106); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"C"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital C with caron"));
	line.push_back(0x010C); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"C"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital C with caron"));
	line.push_back(0x010C); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"C"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital C with cedilla"));
	line.push_back(0x00C7); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"C"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital C with cedilla"));
	line.push_back(0x00C7); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"C"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital C with dot above"));
	line.push_back(0x010A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"C"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital C with dot above"));
	line.push_back(0x010A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"D"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital D with caron"));
	line.push_back(0x010E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"D"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital D with caron"));
	line.push_back(0x010E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"D"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital D with dot above"));
	line.push_back(0x1E0A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"D"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital D with dot above"));
	line.push_back(0x1E0A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"D"));
	line.push_back(convertNamesToIntegerValue(L"H"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Eth"));
	line.push_back(0x00D0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"D"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital D with stroke"));
	line.push_back(0x110); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"D"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital D with stroke"));
	line.push_back(0x110); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with acute"));
	line.push_back(0x00C9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with acute"));
	line.push_back(0x00C9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with acute"));
	line.push_back(0x00C9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with acute"));
	line.push_back(0x00C9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with caron"));
	line.push_back(0x011A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with caron"));
	line.push_back(0x011A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with circumflex"));
	line.push_back(0x00CA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with circumflex"));
	line.push_back(0x00CA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with circumflex"));
	line.push_back(0x00CA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with circumflex"));
	line.push_back(0x00CA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with diaeresis"));
	line.push_back(0x00CB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with diaeresis"));
	line.push_back(0x00CB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with diaeresis"));
	line.push_back(0x00CB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with diaeresis"));
	line.push_back(0x00CB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with dot above"));
	line.push_back(0x116); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with dot above"));
	line.push_back(0x116); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with grave"));
	line.push_back(0x00C8); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with grave"));
	line.push_back(0x00C8); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with macron"));
	line.push_back(0x112); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with macron"));
	line.push_back(0x112); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with macron"));
	line.push_back(0x112); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with macron"));
	line.push_back(0x112); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with ogonek"));
	line.push_back(0x118); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital E with ogonek"));
	line.push_back(0x118); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"N"));
	line.push_back(convertNamesToIntegerValue(L"G"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital ENG"));
	line.push_back(0x014A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"F"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital F with dot above"));
	line.push_back(0x1E1E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"F"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital F with dot above"));
	line.push_back(0x1E1E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"breve"));
	line.push_back(convertNamesToIntegerValue(L"G"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital G with breve"));
	line.push_back(0x011E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"G"));
	line.push_back(convertNamesToIntegerValue(L"breve"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital G with breve"));
	line.push_back(0x011E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"G"));
	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital G with breve"));
	line.push_back(0x011E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"G"));
	line.push_back(convertNamesToIntegerValue(L"parenleft"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital G with breve"));
	line.push_back(0x011E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"parenleft"));
	line.push_back(convertNamesToIntegerValue(L"G"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital G with breve"));
	line.push_back(0x011E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"G"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital G with cedilla"));
	line.push_back(0x122); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"G"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital G with cedilla"));
	line.push_back(0x122); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"G"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital G with dot above"));
	line.push_back(0x120); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"G"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital G with dot above"));
	line.push_back(0x120); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with acute"));
	line.push_back(0x00CD); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with acute"));
	line.push_back(0x00CD); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with acute"));
	line.push_back(0x00CD); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with acute"));
	line.push_back(0x00CD); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with circumflex"));
	line.push_back(0x00CE); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with circumflex"));
	line.push_back(0x00CE); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with circumflex"));
	line.push_back(0x00CE); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with circumflex"));
	line.push_back(0x00CE); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with diaeresis"));
	line.push_back(0x00CF); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with diaeresis"));
	line.push_back(0x00CF); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with diaeresis"));
	line.push_back(0x00CF); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with diaeresis"));
	line.push_back(0x00CF); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with dot above"));
	line.push_back(0x130); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with dot above"));
	line.push_back(0x130); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with grave"));
	line.push_back(0x00CC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with grave"));
	line.push_back(0x00CC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with macron"));
	line.push_back(0x012A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with macron"));
	line.push_back(0x012A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with macron"));
	line.push_back(0x012A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with macron"));
	line.push_back(0x012A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with ogonek"));
	line.push_back(0x012E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with ogonek"));
	line.push_back(0x012E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciiTilde"));
	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with tilde"));
	line.push_back(0x128); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"I"));
	line.push_back(convertNamesToIntegerValue(L"asciiTilde"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital I with tilde"));
	line.push_back(0x128); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"K"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital K with cedilla"));
	line.push_back(0x136); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"K"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital K with cedilla"));
	line.push_back(0x136); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"L"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital L with acute"));
	line.push_back(0x139); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"L"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital L with acute"));
	line.push_back(0x139); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"L"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital L with caron"));
	line.push_back(0x013D); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"L"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital L with caron"));
	line.push_back(0x013D); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"L"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital L with cedilla"));
	line.push_back(0x013B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"L"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital L with cedilla"));
	line.push_back(0x013B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"L"));
	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital L with stroke"));
	line.push_back(0x141); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L"L"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital L with stroke"));
	line.push_back(0x141); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"M"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital M with dot above"));
	line.push_back(0x1E40); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"M"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital M with dot above"));
	line.push_back(0x1E40); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"N"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital N with acute"));
	line.push_back(0x143); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"N"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital N with acute"));
	line.push_back(0x143); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"N"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital N with caron"));
	line.push_back(0x147); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"N"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital N with caron"));
	line.push_back(0x147); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"N"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital N with cedilla"));
	line.push_back(0x145); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"N"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital N with cedilla"));
	line.push_back(0x145); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciiTilde"));
	line.push_back(convertNamesToIntegerValue(L"N"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital N with tilde"));
	line.push_back(0x00D1); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"N"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital N with tilde"));
	line.push_back(0x00D1); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"N"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital N with tilde"));
	line.push_back(0x00D1); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"N"));
	line.push_back(convertNamesToIntegerValue(L"asciiTilde"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital N with tilde"));
	line.push_back(0x00D1); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L"E"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital ligature OE"));
	line.push_back(0x152); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital O with acute"));
	line.push_back(0x00D3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital O with acute"));
	line.push_back(0x00D3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital O with acute"));
	line.push_back(0x00D3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital O with acute"));
	line.push_back(0x00D3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital O with circumflex"));
	line.push_back(0x00D4); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital O with circumflex"));
	line.push_back(0x00D4); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital O with circumflex"));
	line.push_back(0x00D4); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital O with circumflex"));
	line.push_back(0x00D4); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital O with diaeresis"));
	line.push_back(0x00D6); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital O with diaeresis"));
	line.push_back(0x00D6); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital O with diaeresis"));
	line.push_back(0x00D6); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital O with diaeresis"));
	line.push_back(0x00D6); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital O with grave"));
	line.push_back(0x00D2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital O with grave"));
	line.push_back(0x00D2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital O with stroke"));
	line.push_back(0x00D8); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital O with stroke"));
	line.push_back(0x00D8); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciiTilde"));
	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital O with tilde"));
	line.push_back(0x00D5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital O with tilde"));
	line.push_back(0x00D5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital O with tilde"));
	line.push_back(0x00D5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"O"));
	line.push_back(convertNamesToIntegerValue(L"asciiTilde"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital O with tilde"));
	line.push_back(0x00D5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"P"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital P with dot above"));
	line.push_back(0x1E56); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"P"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital P with dot above"));
	line.push_back(0x1E56); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"R"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital R with acute"));
	line.push_back(0x154); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"R"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital R with acute"));
	line.push_back(0x154); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"R"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital R with caron"));
	line.push_back(0x158); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"R"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital R with caron"));
	line.push_back(0x158); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"R"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital R with cedilla"));
	line.push_back(0x156); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"R"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital R with cedilla"));
	line.push_back(0x156); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital S with acute"));
	line.push_back(0x015A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital S with acute"));
	line.push_back(0x015A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital S with caron"));
	line.push_back(0x160); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital S with caron"));
	line.push_back(0x160); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"cedilla"));
	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital S with cedilla"));
	line.push_back(0x015E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital S with cedilla"));
	line.push_back(0x015E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital S with cedilla"));
	line.push_back(0x015E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L"cedilla"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital S with cedilla"));
	line.push_back(0x015E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital S with dot above"));
	line.push_back(0x1E60); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"S"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital S with dot above"));
	line.push_back(0x1E60); 
	dk_ComposeTable.push_back(line);  
	line.clear();



	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"T"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital T with caron"));
	line.push_back(0x164); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"T"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital T with caron"));
	line.push_back(0x164); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"T"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital T with dot above"));
	line.push_back(0x1E6A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"T"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital T with dot above"));
	line.push_back(0x1E6A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L"T"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital T with stroke"));
	line.push_back(0x166); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"T"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital T with stroke"));
	line.push_back(0x166); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"T"));
	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital T with stroke"));
	line.push_back(0x166); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"T"));
	line.push_back(convertNamesToIntegerValue(L"H"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital THORN"));
	line.push_back(0x00DE); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with acute"));
	line.push_back(0x00DA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with acute"));
	line.push_back(0x00DA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with acute"));
	line.push_back(0x00DA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with acute"));
	line.push_back(0x00DA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with circumflex"));
	line.push_back(0x00DB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with circumflex"));
	line.push_back(0x00DB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with circumflex"));
	line.push_back(0x00DB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with circumflex"));
	line.push_back(0x00DB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with diaeresis"));
	line.push_back(0x00DC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with diaeresis"));
	line.push_back(0x00DC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with diaeresis"));
	line.push_back(0x00DC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with diaeresis"));
	line.push_back(0x00DC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with grave"));
	line.push_back(0x00D9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with grave"));
	line.push_back(0x00D9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with macron"));
	line.push_back(0x016A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with macron"));
	line.push_back(0x016B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with macron"));
	line.push_back(0x016B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with macron"));
	line.push_back(0x016A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with ogonek"));
	line.push_back(0x172); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with ogonek"));
	line.push_back(0x172); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asterisk"));
	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with ring above"));
	line.push_back(0x016E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L"asterisk"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with ring above"));
	line.push_back(0x016E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciiTilde"));
	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with tilde"));
	line.push_back(0x168); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L"asciiTilde"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital U with tilde"));
	line.push_back(0x168); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"W"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital W with circumflex"));
	line.push_back(0x174); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"W"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital W with circumflex"));
	line.push_back(0x174); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L"Y"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Y with acute"));
	line.push_back(0x00DD); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"Y"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Y with acute"));
	line.push_back(0x00DD); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Y"));
	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Y with acute"));
	line.push_back(0x00DD); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Y"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Y with acute"));
	line.push_back(0x00DD); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"Y"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Y with circumflex"));
	line.push_back(0x176); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Y"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Y with circumflex"));
	line.push_back(0x176); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L"Y"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Y with diaeresis"));
	line.push_back(0x178); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"Y"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Y with diaeresis"));
	line.push_back(0x178); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Y"));
	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Y with diaeresis"));
	line.push_back(0x178); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Y"));
	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Y with diaeresis"));
	line.push_back(0x178); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"Z"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Z with acute"));
	line.push_back(0x179); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Z"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Z with acute"));
	line.push_back(0x179); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"Z"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Z with caron"));
	line.push_back(0x017D); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"v"));
	line.push_back(convertNamesToIntegerValue(L"Z"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Z with caron"));
	line.push_back(0x017D); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Z"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Z with caron"));
	line.push_back(0x017D); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"Z"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Z with dot above"));
	line.push_back(0x017B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Z"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Z with dot above"));
	line.push_back(0x017B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	// Latin minuscule
	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with acute"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with acute"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with acute"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with acute"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L"parenleft"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with breve"));
	line.push_back(0x103); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"parenleft"));
	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with breve"));
	line.push_back(0x103); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with circumflex"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with circumflex"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with circumflex"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with circumflex"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with diaeresis"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with diaeresis"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with diaeresis"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with diaeresis"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with grave"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with grave"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with ogonek"));
	line.push_back(0x105); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with ogonek"));
	line.push_back(0x105); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L"asterisk"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with ring above"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with ring above"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asterisk"));
	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with ring above"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L"asciiTilde"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with tilde"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with tilde"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciiTilde"));
	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with tilde"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small A with tilde"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"a"));
	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small AE ligature"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"b"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small B with dot above"));
	line.push_back(0x1000); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"b"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small B with dot above"));
	line.push_back(0x1000); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"c"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small C with acute"));
	line.push_back(0x107); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"c"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small C with acute"));
	line.push_back(0x107); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"c"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small C with caron"));
	line.push_back(0x010D); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"c"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small C with caron"));
	line.push_back(0x010D); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"c"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small C with cedilla"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"c"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small C with cedilla"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"c"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small C with dot above"));
	line.push_back(0x010B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"c"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small C with dot above"));
	line.push_back(0x010B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"d"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small D with caron"));
	line.push_back(0x010F); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"d"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small D with caron"));
	line.push_back(0x010F); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"d"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small D with dot above"));
	line.push_back(0x1E0B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"d"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small D with dot above"));
	line.push_back(0x1E0B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"d"));
	line.push_back(convertNamesToIntegerValue(L"h"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Eth"));
	line.push_back(0x00F0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"d"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small D with stroke"));
	line.push_back(0x111); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"d"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small D with stroke"));
	line.push_back(0x111); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small dotless I"));
	line.push_back(0x131); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small dotless I"));
	line.push_back(0x131); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with acute"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with acute"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with acute"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with acute"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with caron"));
	line.push_back(0x011B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with caron"));
	line.push_back(0x011B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with circumflex"));
	line.push_back(0x00EA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with circumflex"));
	line.push_back(0x00EA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with circumflex"));
	line.push_back(0x00EA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with circumflex"));
	line.push_back(0x00EA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with diaeresis"));
	line.push_back(0x00EB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with diaeresis"));
	line.push_back(0x00EB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with diaeresis"));
	line.push_back(0x00EB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with diaeresis"));
	line.push_back(0x00EB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with dot above"));
	line.push_back(0x117); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with dot above"));
	line.push_back(0x117); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with grave"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with grave"));
	line.push_back(0x0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with macron"));
	line.push_back(0x113); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with macron"));
	line.push_back(0x113); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with macron"));
	line.push_back(0x113); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with macron"));
	line.push_back(0x113); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with ogonek"));
	line.push_back(0x119); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small E with ogonek"));
	line.push_back(0x119); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"n"));
	line.push_back(convertNamesToIntegerValue(L"g"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small ENG"));
	line.push_back(0x014B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"f"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small F with dot above"));
	line.push_back(0x1E1F); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"f"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small F with dot above"));
	line.push_back(0x1E1F); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"breve"));
	line.push_back(convertNamesToIntegerValue(L"g"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small G with breve"));
	line.push_back(0x011F); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"g"));
	line.push_back(convertNamesToIntegerValue(L"breve"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small G with breve"));
	line.push_back(0x011F); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"g"));
	line.push_back(convertNamesToIntegerValue(L"parenleft"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small G with breve"));
	line.push_back(0x011F); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"g"));
	line.push_back(convertNamesToIntegerValue(L"U"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small G with breve"));
	line.push_back(0x011F); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"parenleft"));
	line.push_back(convertNamesToIntegerValue(L"g"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small G with breve"));
	line.push_back(0x011F); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"g"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small G with cedilla"));
	line.push_back(0x123); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"g"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small G with cedilla"));
	line.push_back(0x123); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"g"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small G with dot above"));
	line.push_back(0x121); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"g"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small G with dot above"));
	line.push_back(0x121); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with acute"));
	line.push_back(0x00ED); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with acute"));
	line.push_back(0x00ED); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with acute"));
	line.push_back(0x00ED); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with acute"));
	line.push_back(0x00ED); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with circumflex"));
	line.push_back(0x00EE); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with circumflex"));
	line.push_back(0x00EE); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with circumflex"));
	line.push_back(0x00EE); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with circumflex"));
	line.push_back(0x00EE); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with diaeresis"));
	line.push_back(0x00EF); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with diaeresis"));
	line.push_back(0x00EF); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with diaeresis"));
	line.push_back(0x00EF); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with diaeresis"));
	line.push_back(0x00EF); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with grave"));
	line.push_back(0x00EC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with grave"));
	line.push_back(0x00EC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with macron"));
	line.push_back(0x012B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with macron"));
	line.push_back(0x012B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with macron"));
	line.push_back(0x012B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with macron"));
	line.push_back(0x012B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with ogonek"));
	line.push_back(0x012F); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with ogonek"));
	line.push_back(0x012F); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciiTilde"));
	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with tilde"));
	line.push_back(0x129); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"i"));
	line.push_back(convertNamesToIntegerValue(L"asciiTilde"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small I with tilde"));
	line.push_back(0x129); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"k"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small K with cedilla"));
	line.push_back(0x137); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"k"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small K with cedilla"));
	line.push_back(0x137); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"k"));
	line.push_back(convertNamesToIntegerValue(L"k"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small KRA"));
	line.push_back(0x138); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"l"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small L with acute"));
	line.push_back(0x013A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"l"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small L with acute"));
	line.push_back(0x013A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"l"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small L with caron"));
	line.push_back(0x013E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"l"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small L with caron"));
	line.push_back(0x013E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"l"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small L with cedilla"));
	line.push_back(0x013C); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"l"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small L with cedilla"));
	line.push_back(0x013C); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"l"));
	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small L with stroke"));
	line.push_back(0x142); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L"l"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small L with stroke"));
	line.push_back(0x142); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L"e"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small ligature OE"));
	line.push_back(0x153); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"m"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small M with dot above"));
	line.push_back(0x1E41); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"m"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small M with dot above"));
	line.push_back(0x1E41); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"n"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small N with acute"));
	line.push_back(0x144); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"n"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small N with acute"));
	line.push_back(0x144); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"n"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small N with caron"));
	line.push_back(0x148); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"n"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small N with caron"));
	line.push_back(0x148); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"n"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small N with cedilla"));
	line.push_back(0x146); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"n"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small N with cedilla"));
	line.push_back(0x146); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciiTilde"));
	line.push_back(convertNamesToIntegerValue(L"n"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small N with tilde"));
	line.push_back(0x00F1); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"n"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small N with tilde"));
	line.push_back(0x00F1); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"n"));
	line.push_back(convertNamesToIntegerValue(L"asciiTilde"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small N with tilde"));
	line.push_back(0x00F1); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"n"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small N with tilde"));
	line.push_back(0x00F1); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small O with acute"));
	line.push_back(0x00F3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small O with acute"));
	line.push_back(0x00F3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small O with acute"));
	line.push_back(0x00F3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small O with acute"));
	line.push_back(0x00F3); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small O with circumflex"));
	line.push_back(0x00F4); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small O with circumflex"));
	line.push_back(0x00F4); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small O with circumflex"));
	line.push_back(0x00F4); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small O with circumflex"));
	line.push_back(0x00F4); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small O with diaeresis"));
	line.push_back(0x00F6); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small O with diaeresis"));
	line.push_back(0x00F6); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small O with diaeresis"));
	line.push_back(0x00F6); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small O with diaeresis"));
	line.push_back(0x00F6); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small O with grave"));
	line.push_back(0x00F2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small O with grave"));
	line.push_back(0x00F2); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small O with stroke"));
	line.push_back(0x00F8); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small O with stroke"));
	line.push_back(0x00F8); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciiTilde"));
	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small O with tilde"));
	line.push_back(0x00F5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small O with tilde"));
	line.push_back(0x00F5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small O with tilde"));
	line.push_back(0x00F5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"o"));
	line.push_back(convertNamesToIntegerValue(L"asciiTilde"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small O with tilde"));
	line.push_back(0x00F5); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"p"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small P with dot above"));
	line.push_back(0x1E57); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"p"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small P with dot above"));
	line.push_back(0x1E57); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"r"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small R with acute"));
	line.push_back(0x155); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"r"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small R with acute"));
	line.push_back(0x155); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"r"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small R with caron"));
	line.push_back(0x159); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"r"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small R with caron"));
	line.push_back(0x159); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"r"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small R with cedilla"));
	line.push_back(0x157); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"r"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small R with cedilla"));
	line.push_back(0x157); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small S with acute"));
	line.push_back(0x015B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small S with acute"));
	line.push_back(0x015B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small S with caron"));
	line.push_back(0x161); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small S with caron"));
	line.push_back(0x161); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"cedilla"));
	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small S with cedilla"));
	line.push_back(0x015F); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small S with cedilla"));
	line.push_back(0x015F); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small S with cedilla"));
	line.push_back(0x015F); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L"cedilla"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small S with cedilla"));
	line.push_back(0x015F); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small S with dot above"));
	line.push_back(0x1E61); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small S with dot above"));
	line.push_back(0x1E61); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L"s"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small SHARP S"));
	line.push_back(0x00DF); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"t"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small T with caron"));
	line.push_back(0x165); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"t"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small T with caron"));
	line.push_back(0x165); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"t"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small T with dot above"));
	line.push_back(0x1E6B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"t"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small T with dot above"));
	line.push_back(0x1E6B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L"t"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small T with stroke"));
	line.push_back(0x167); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"t"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small T with stroke"));
	line.push_back(0x167); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"t"));
	line.push_back(convertNamesToIntegerValue(L"slash"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small T with stroke"));
	line.push_back(0x167); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"t"));
	line.push_back(convertNamesToIntegerValue(L"h"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small THORN"));
	line.push_back(0x00FE); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with acute"));
	line.push_back(0x00FA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with acute"));
	line.push_back(0x00FA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with acute"));
	line.push_back(0x00FA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with acute"));
	line.push_back(0x00FA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with caron"));
	line.push_back(0x01D4); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with circumflex"));
	line.push_back(0x00FB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with circumflex"));
	line.push_back(0x00FB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with circumflex"));
	line.push_back(0x00FB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L"greater"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with circumflex"));
	line.push_back(0x00FB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with diaeresis"));
	line.push_back(0x00FC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with diaeresis"));
	line.push_back(0x00FC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with diaeresis"));
	line.push_back(0x00FC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with diaeresis"));
	line.push_back(0x00FC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with grave"));
	line.push_back(0x00F9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L"grave"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with grave"));
	line.push_back(0x00F9); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with macron"));
	line.push_back(0x016B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L"minus"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with macron"));
	line.push_back(0x016B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with macron"));
	line.push_back(0x016B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"underscore"));
	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with macron"));
	line.push_back(0x016B); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with ogonek"));
	line.push_back(0x173); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L"comma"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with ogonek"));
	line.push_back(0x173); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asterisk"));
	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with ring above"));
	line.push_back(0x016F); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L"asterisk"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with ring above"));
	line.push_back(0x016F); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciiTilde"));
	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with tilde"));
	line.push_back(0x169); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"u"));
	line.push_back(convertNamesToIntegerValue(L"asciiTilde"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small U with tilde"));
	line.push_back(0x169); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"w"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small W with circumflex"));
	line.push_back(0x175); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"w"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small W with circumflex"));
	line.push_back(0x175); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L"y"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Y with acute"));
	line.push_back(0x00FD); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"y"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Y with acute"));
	line.push_back(0x00FD); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"y"));
	line.push_back(convertNamesToIntegerValue(L"acute"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Y with acute"));
	line.push_back(0x00FD); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"y"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Y with acute"));
	line.push_back(0x00FD); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L"y"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Y with circumflex"));
	line.push_back(0x177); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"y"));
	line.push_back(convertNamesToIntegerValue(L"asciicircum"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Y with circumflex"));
	line.push_back(0x177); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L"y"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Y with diaeresis"));
	line.push_back(0x00FF); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"y"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Y with diaeresis"));
	line.push_back(0x00FF); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"y"));
	line.push_back(convertNamesToIntegerValue(L"diaeresis"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Y with diaeresis"));
	line.push_back(0x00FF); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"y"));
	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Y with diaeresis"));
	line.push_back(0x00FF); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"z"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Z with acute"));
	line.push_back(0x017A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"z"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Z with acute"));
	line.push_back(0x017A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L"z"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Z with caron"));
	line.push_back(0x017E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"v"));
	line.push_back(convertNamesToIntegerValue(L"z"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Z with caron"));
	line.push_back(0x017E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"z"));
	line.push_back(convertNamesToIntegerValue(L"less"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Z with caron"));
	line.push_back(0x017E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L"z"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Z with dot above"));
	line.push_back(0x017C); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"z"));
	line.push_back(convertNamesToIntegerValue(L"period"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Z with dot above"));
	line.push_back(0x017C); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	// Greek
	/*line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"space"));
	line.push_back(convertNamesToIntegerValue(L"dialytika tonos"));
	line.push_back(0x385); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"space"));
	line.push_back(convertNamesToIntegerValue(L"dialytika tonos"));
	line.push_back(0x385); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"Greek_Alpha"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Alpha with tonos"));
	line.push_back(0x386); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Greek_Alpha"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Alpha with tonos"));
	line.push_back(0x386); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"Greek_Epsilon"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Epsilon with tonos"));
	line.push_back(0x388); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Greek_Epsilon"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Epsilon with tonos"));
	line.push_back(0x388); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"Greek_Eta"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Eta with tonos"));
	line.push_back(0x389); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Greek_Eta"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Eta with tonos"));
	line.push_back(0x389); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Greek_Iota"));
	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Iota with dialytika"));
	line.push_back(0x03AA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"Greek_Iota"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Iota with dialytika"));
	line.push_back(0x03AA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"Greek_Iota"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Iota with tonos"));
	line.push_back(0x038A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Greek_Iota"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Iota with tonos"));
	line.push_back(0x038A); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"Greek_Omicron"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Omicron with tonos"));
	line.push_back(0x038C); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Greek_Omicron"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Omicron with tonos"));
	line.push_back(0x038C); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Greek_Upsilon"));
	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Upsilon with dialytika"));
	line.push_back(0x03AB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"Greek_Upsilon"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Upsilon with dialytika"));
	line.push_back(0x03AB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"Greek_Upsilon"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Upsilon with tonos"));
	line.push_back(0x038E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Greek_Upsilon"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Upsilon with tonos"));
	line.push_back(0x038E); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"Greek_Omega"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Omega with tonos"));
	line.push_back(0x038F); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Greek_Omega"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"capital Omega with tonos"));
	line.push_back(0x038F); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"Greek_Alpha"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Alpha with tonos"));
	line.push_back(0x03AC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Greek_Alpha"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Alpha with tonos"));
	line.push_back(0x03AC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"Greek_Epsilon"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Epsilon with tonos"));
	line.push_back(0x03AD); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Greek_Epsilon"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Epsilon with tonos"));
	line.push_back(0x03AD); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"Greek_eta"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Eta with tonos"));
	line.push_back(0x03AE); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Greek_eta"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Eta with tonos"));
	line.push_back(0x03AE); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"Greek_iota"));
	line.push_back(convertNamesToIntegerValue(L"small Iota with dialytika and tonos"));
	line.push_back(0x390); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"Greek_iota"));
	line.push_back(convertNamesToIntegerValue(L"small Iota with dialytika and tonos"));
	line.push_back(0x390); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Greek_iota"));
	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Iota with dialytika"));
	line.push_back(0x03CA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"Greek_iota"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Iota with dialytika"));
	line.push_back(0x03CA); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"Greek_iota"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Iota with tonos"));
	line.push_back(0x03AF); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Greek_iota"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Iota with tonos"));
	line.push_back(0x03AF); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"Greek_oMicron"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Omicron with tonos"));
	line.push_back(0x03CC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Greek_Omicron"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Omicron with tonos"));
	line.push_back(0x03CC); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"Greek_upsilon"));
	line.push_back(convertNamesToIntegerValue(L"small Upsilon with dialytika and tonos"));
	line.push_back(0x03B0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"Greek_upsilon"));
	line.push_back(convertNamesToIntegerValue(L"small Upsilon with dialytika and tonos"));
	line.push_back(0x03B0); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Greek_upsilon"));
	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Upsilon with dialytika"));
	line.push_back(0x03CB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"quotedbl"));
	line.push_back(convertNamesToIntegerValue(L"Greek_upsilon"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Upsilon with dialytika"));
	line.push_back(0x03CB); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"Greek_upsilon"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Upsilon with tonos"));
	line.push_back(0x03CD); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Greek_upsilon"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Upsilon with tonos"));
	line.push_back(0x03CD); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L"Greek_omega"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Omega with tonos"));
	line.push_back(0x03CE); 
	dk_ComposeTable.push_back(line);  
	line.clear();

	line.push_back(convertNamesToIntegerValue(L"Greek_omega"));
	line.push_back(convertNamesToIntegerValue(L"apostrophe"));
	line.push_back(convertNamesToIntegerValue(L""));
	line.push_back(convertNamesToIntegerValue(L"small Omega with tonos"));
	line.push_back(0x03CE); 
	dk_ComposeTable.push_back(line);  
	line.clear();*/
    
  return 0;
}


KMX_DWORD find_ComposedCharacter(v_dw_2D * p_dk_ComposeTable, KMX_DWORD first, KMX_DWORD second , KMX_DWORD third ) {
	
  v_dw_2D  dk_ComposeTable = * p_dk_ComposeTable;

  for ( int i =0; i< (dk_ComposeTable).size()-1; i++) {
	if (( (KMX_DWORD) dk_ComposeTable[i][0] == first) && ( (KMX_DWORD) dk_ComposeTable[i][1] == second) && ( (KMX_DWORD) dk_ComposeTable[i][2] == third) )
	  return (KMX_DWORD) dk_ComposeTable[i][4];
  }
  return 0;   // _S2 what to return if not found?
}

