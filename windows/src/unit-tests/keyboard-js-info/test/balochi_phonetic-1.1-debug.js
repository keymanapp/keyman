
KeymanWeb.KR(new Keyboard_balochi_phonetic());

function Keyboard_balochi_phonetic()
{
  
  this.KI="Keyboard_balochi_phonetic";
  this.KN="Balochi Phonetic";
  this.KV={F:' 1em "Tahoma"',K102:0};
  this.KDU=1;
  this.KLS={
    "shift": ["ٔ","","","","","","‌","","","(",")","«","»","","","","ي","أ","ۓ","ڑ","ٹ","","ۀ","ً","ْ","ّ","{","}","ں","","","","آ","ش","ڈ","ث","ق","ص","ض","[","]","","","","","","","","","ژ","خ","ط","ظ","ة","ع","غ","‹","›","؟","","","","","",""],
    "default": ["ء","1","2","3","4","5","6","7","8","9","0","","","","","","ئ","و","ے","ر","ت","ی","ۆ","ێ","ُ","پ","َ","ِ","","","","","ا","س","د","ف","گ","ه","ج","ک","ل","؛","","","","","","","","ز","ح","چ","ـ","ب","ن","م","،",".","","","","","","",""]
  };
  this.KV.BK=(function(x){
    var
      empty=Array.apply(null, Array(65)).map(String.prototype.valueOf,""),
      result=[], v, i,
      modifiers=['default','shift','ctrl','shift-ctrl','alt','shift-alt','ctrl-alt','shift-ctrl-alt'];
    for(i=modifiers.length-1;i>=0;i--) {
      v = x[modifiers[i]];
      if(v || result.length > 0) {
        result=(v ? v : empty).slice().concat(result);
      }
    }
    return result;
  })(this.KLS);
  this.KH="This is a phonetically organized Balochi keyboard.";
  this.KM=0;
  this.KBVER="1.1";
  this.KMBM=0x0010;
  this.KRTL=1;
  this.KVKL={
  "tablet": {
    "font": "Tahoma",
    "layer": [
      {
        "id": "default",
        "row": [
          {
            "id": 1,
            "key": [
              {
                "id": "K_1",
                "text": "1"
              },
              {
                "id": "K_2",
                "text": "2"
              },
              {
                "id": "K_3",
                "text": "3"
              },
              {
                "id": "K_4",
                "text": "4"
              },
              {
                "id": "K_5",
                "text": "5"
              },
              {
                "id": "K_6",
                "text": "6"
              },
              {
                "id": "K_7",
                "text": "7"
              },
              {
                "id": "K_8",
                "text": "8"
              },
              {
                "id": "K_9",
                "text": "9"
              },
              {
                "id": "K_0",
                "text": "0"
              },
              {
                "id": "K_HYPHEN",
                "text": ""
              },
              {
                "id": "K_EQUAL",
                "text": ""
              },
              {
                "id": "K_BKSP",
                "text": "*BkSp*",
                "width": "100",
                "sp": "1"
              }
            ]
          },
          {
            "id": 2,
            "key": [
              {
                "id": "K_Q",
                "text": "ئ",
                "pad": 75
              },
              {
                "id": "K_W",
                "text": "و"
              },
              {
                "id": "K_E",
                "text": "ے"
              },
              {
                "id": "K_R",
                "text": "ر"
              },
              {
                "id": "K_T",
                "text": "ت"
              },
              {
                "id": "K_Y",
                "text": "ی"
              },
              {
                "id": "K_U",
                "text": "ۆ"
              },
              {
                "id": "K_I",
                "text": "ێ"
              },
              {
                "id": "K_O",
                "text": "ُ"
              },
              {
                "id": "K_P",
                "text": "پ"
              },
              {
                "id": "K_LBRKT",
                "text": "َ"
              },
              {
                "id": "K_RBRKT",
                "text": "ِ"
              },
              {
                "id": "T_new_26",
                "text": "",
                "width": "10",
                "sp": "10"
              }
            ]
          },
          {
            "id": 3,
            "key": [
              {
                "id": "K_BKQUOTE",
                "text": "ء"
              },
              {
                "id": "K_A",
                "text": "ا"
              },
              {
                "id": "K_S",
                "text": "س"
              },
              {
                "id": "K_D",
                "text": "د"
              },
              {
                "id": "K_F",
                "text": "ف"
              },
              {
                "id": "K_G",
                "text": "گ"
              },
              {
                "id": "K_H",
                "text": "ه"
              },
              {
                "id": "K_J",
                "text": "ج"
              },
              {
                "id": "K_K",
                "text": "ک"
              },
              {
                "id": "K_L",
                "text": "ل"
              },
              {
                "id": "K_COLON",
                "text": "؛"
              },
              {
                "id": "K_QUOTE",
                "text": ""
              },
              {
                "id": "K_BKSLASH",
                "text": ""
              }
            ]
          },
          {
            "id": 4,
            "key": [
              {
                "id": "K_SHIFT",
                "text": "*Shift*",
                "width": "160",
                "sp": "1",
                "nextlayer": "shift"
              },
              {
                "id": "K_Z",
                "text": "ز"
              },
              {
                "id": "K_X",
                "text": "ح"
              },
              {
                "id": "K_C",
                "text": "چ"
              },
              {
                "id": "K_V",
                "text": "ـ"
              },
              {
                "id": "K_B",
                "text": "ب"
              },
              {
                "id": "K_N",
                "text": "ن"
              },
              {
                "id": "K_M",
                "text": "م"
              },
              {
                "id": "K_COMMA",
                "text": "،"
              },
              {
                "id": "K_PERIOD",
                "text": "."
              },
              {
                "id": "K_SLASH",
                "text": ""
              },
              {
                "id": "K_ENTER",
                "text": "*Enter*",
                "width": "145",
                "sp": "1"
              }
            ]
          },
          {
            "id": 5,
            "key": [
              {
                "id": "K_LCONTROL",
                "text": "*Ctrl*",
                "width": "130",
                "sp": "1"
              },
              {
                "id": "K_LOPT",
                "text": "*Menu*",
                "width": "140",
                "sp": "1"
              },
              {
                "id": "K_LALT",
                "text": "*Alt*",
                "width": "130",
                "sp": "1"
              },
              {
                "id": "K_SPACE",
                "text": "",
                "width": "675",
                "sp": "0"
              },
              {
                "id": "K_RALT",
                "text": "*AltGr*",
                "width": "130",
                "sp": "1"
              },
              {
                "id": "K_RCONTROL",
                "text": "*Ctrl*",
                "width": "130",
                "sp": "1"
              }
            ]
          }
        ]
      },
      {
        "id": "shift",
        "row": [
          {
            "id": 1,
            "key": [
              {
                "id": "K_1",
                "text": ""
              },
              {
                "id": "K_2",
                "text": ""
              },
              {
                "id": "K_3",
                "text": ""
              },
              {
                "id": "K_4",
                "text": ""
              },
              {
                "id": "K_5",
                "text": ""
              },
              {
                "id": "K_6",
                "text": "‌"
              },
              {
                "id": "K_7",
                "text": ""
              },
              {
                "id": "K_8",
                "text": ""
              },
              {
                "id": "K_9",
                "text": ")"
              },
              {
                "id": "K_0",
                "text": "("
              },
              {
                "id": "K_HYPHEN",
                "text": "»"
              },
              {
                "id": "K_EQUAL",
                "text": "«"
              },
              {
                "id": "K_BKSP",
                "text": "*BkSp*",
                "width": "100",
                "sp": "1"
              }
            ]
          },
          {
            "id": 2,
            "key": [
              {
                "id": "K_Q",
                "text": "ي",
                "pad": 75
              },
              {
                "id": "K_W",
                "text": "أ"
              },
              {
                "id": "K_E",
                "text": "ۓ"
              },
              {
                "id": "K_R",
                "text": "ڑ"
              },
              {
                "id": "K_T",
                "text": "ٹ"
              },
              {
                "id": "K_Y",
                "text": ""
              },
              {
                "id": "K_U",
                "text": "ۀ"
              },
              {
                "id": "K_I",
                "text": "ً"
              },
              {
                "id": "K_O",
                "text": "ْ"
              },
              {
                "id": "K_P",
                "text": "ّ"
              },
              {
                "id": "K_LBRKT",
                "text": "}"
              },
              {
                "id": "K_RBRKT",
                "text": "{"
              },
              {
                "id": "T_new_702",
                "text": "",
                "width": "10",
                "sp": "10"
              }
            ]
          },
          {
            "id": 3,
            "key": [
              {
                "id": "K_BKQUOTE",
                "text": "ٔ"
              },
              {
                "id": "K_A",
                "text": "آ"
              },
              {
                "id": "K_S",
                "text": "ش"
              },
              {
                "id": "K_D",
                "text": "ڈ"
              },
              {
                "id": "K_F",
                "text": "ث"
              },
              {
                "id": "K_G",
                "text": "ق"
              },
              {
                "id": "K_H",
                "text": "ص"
              },
              {
                "id": "K_J",
                "text": "ض"
              },
              {
                "id": "K_K",
                "text": "]"
              },
              {
                "id": "K_L",
                "text": "["
              },
              {
                "id": "K_COLON",
                "text": ""
              },
              {
                "id": "K_QUOTE",
                "text": ""
              },
              {
                "id": "K_BKSLASH",
                "text": "ں"
              }
            ]
          },
          {
            "id": 4,
            "key": [
              {
                "id": "K_SHIFT",
                "text": "*Shift*",
                "width": "160",
                "sp": "2",
                "nextlayer": "default"
              },
              {
                "id": "K_Z",
                "text": "ژ"
              },
              {
                "id": "K_X",
                "text": "خ"
              },
              {
                "id": "K_C",
                "text": "ط"
              },
              {
                "id": "K_V",
                "text": "ظ"
              },
              {
                "id": "K_B",
                "text": "ة"
              },
              {
                "id": "K_N",
                "text": "ع"
              },
              {
                "id": "K_M",
                "text": "غ"
              },
              {
                "id": "K_COMMA",
                "text": "›"
              },
              {
                "id": "K_PERIOD",
                "text": "‹"
              },
              {
                "id": "K_SLASH",
                "text": "؟"
              },
              {
                "id": "K_ENTER",
                "text": "*Enter*",
                "width": "145",
                "sp": "1"
              }
            ]
          },
          {
            "id": 5,
            "key": [
              {
                "id": "K_LCONTROL",
                "text": "*Ctrl*",
                "width": "130",
                "sp": "1"
              },
              {
                "id": "K_LOPT",
                "text": "*Menu*",
                "width": "140",
                "sp": "1"
              },
              {
                "id": "K_LALT",
                "text": "*Alt*",
                "width": "130",
                "sp": "1"
              },
              {
                "id": "K_SPACE",
                "text": "",
                "width": "675",
                "sp": "0"
              },
              {
                "id": "K_RALT",
                "text": "*AltGr*",
                "width": "130",
                "sp": "1"
              },
              {
                "id": "K_RCONTROL",
                "text": "*Ctrl*",
                "width": "130",
                "sp": "1"
              }
            ]
          }
        ]
      }
    ]
  },
  "phone": {
    "font": "Tahoma",
    "layer": [
      {
        "id": "default",
        "row": [
          {
            "id": 1,
            "key": [
              {
                "id": "K_Q",
                "text": "ئ",
                "sk": [
                  {
                    "text": "ی",
                    "id": "U_06CC"
                  },
                  {
                    "text": "ێ",
                    "id": "U_06CE"
                  }
                ]
              },
              {
                "id": "K_W",
                "text": "و",
                "sk": [
                  {
                    "text": "ۆ",
                    "id": "U_06C6"
                  }
                ]
              },
              {
                "id": "K_E",
                "text": "ے",
                "sk": [
                  {
                    "text": "ۓ",
                    "id": "U_06D3"
                  }
                ]
              },
              {
                "id": "K_R",
                "text": "ر",
                "sk": [
                  {
                    "text": "ژ",
                    "id": "U_0698"
                  },
                  {
                    "text": "ز",
                    "id": "U_0632"
                  },
                  {
                    "text": "ڑ",
                    "id": "U_0691"
                  }
                ]
              },
              {
                "id": "K_T",
                "text": "ت",
                "sk": [
                  {
                    "text": "ث",
                    "id": "U_062B"
                  },
                  {
                    "text": "ٹ",
                    "id": "U_0679"
                  }
                ]
              },
              {
                "id": "K_Y",
                "text": "ی",
                "sk": [
                  {
                    "text": "ئ",
                    "id": "U_0626"
                  },
                  {
                    "text": "ێ",
                    "id": "U_06CC"
                  }
                ]
              },
              {
                "id": "K_U",
                "text": "ۆ",
                "sk": [
                  {
                    "text": "و",
                    "id": "U_0648"
                  }
                ]
              },
              {
                "id": "K_I",
                "text": "ێ",
                "sk": [
                  {
                    "text": "ئ",
                    "id": "U_0626"
                  },
                  {
                    "text": "ی",
                    "id": "U_06CC"
                  }
                ]
              },
              {
                "id": "K_O",
                "text": "ُ",
                "sk": [
                  {
                    "text": "ً",
                    "id": "U_064B"
                  },
                  {
                    "text": "ٔ",
                    "id": "U_0658"
                  },
                  {
                    "text": "ْ",
                    "id": "U_0652"
                  },
                  {
                    "text": "ّ",
                    "id": "U_0651"
                  },
                  {
                    "text": "ِ",
                    "id": "U_0650"
                  },
                  {
                    "text": "َ",
                    "id": "U_064E"
                  }
                ]
              },
              {
                "id": "K_P",
                "text": "پ",
                "sk": [
                  {
                    "text": "ب",
                    "id": "U_0628"
                  }
                ]
              }
            ]
          },
          {
            "id": 2,
            "key": [
              {
                "id": "K_A",
                "text": "ا",
                "pad": "50",
                "sk": [
                  {
                    "text": "أ",
                    "id": "U_0623"
                  },
                  {
                    "text": "آ",
                    "id": "U_0622"
                  }
                ]
              },
              {
                "id": "K_S",
                "text": "س",
                "sk": [
                  {
                    "text": "ش",
                    "id": "U_0634"
                  }
                ]
              },
              {
                "id": "K_D",
                "text": "د",
                "sk": [
                  {
                    "text": "ذ",
                    "id": "U_0630"
                  },
                  {
                    "text": "ڈ",
                    "id": "U_0688"
                  }
                ]
              },
              {
                "id": "K_F",
                "text": "ف",
                "sk": [
                  {
                    "text": "ق",
                    "id": "U_0642"
                  }
                ]
              },
              {
                "id": "K_G",
                "text": "گ"
              },
              {
                "id": "K_H",
                "text": "ه",
                "sk": [
                  {
                    "text": "ة",
                    "id": "U_0629"
                  },
                  {
                    "text": "ۀ",
                    "id": "U_06C0"
                  }
                ]
              },
              {
                "id": "K_J",
                "text": "ج",
                "sk": [
                  {
                    "text": "خ",
                    "id": "U_062E"
                  },
                  {
                    "text": "ح",
                    "id": "U_062D"
                  },
                  {
                    "text": "چ",
                    "id": "U_0686"
                  }
                ]
              },
              {
                "id": "K_K",
                "text": "ک"
              },
              {
                "id": "K_L",
                "text": "ل"
              },
              {
                "id": "T_new_77",
                "text": "",
                "width": "10",
                "sp": "10"
              }
            ]
          },
          {
            "id": 3,
            "key": [
              {
                "id": "K_SHIFT",
                "text": "*Shift*",
                "sp": "1",
                "nextlayer": "shift"
              },
              {
                "id": "K_Z",
                "text": "ز",
                "sk": [
                  {
                    "text": "ژ",
                    "id": "U_0698"
                  },
                  {
                    "text": "ڑ",
                    "id": "U_0691"
                  },
                  {
                    "text": "ر",
                    "id": "U_0631"
                  }
                ]
              },
              {
                "id": "K_X",
                "text": "ح",
                "sk": [
                  {
                    "text": "چ",
                    "id": "U_0686"
                  },
                  {
                    "text": "ج",
                    "id": "U_062C"
                  },
                  {
                    "text": "خ",
                    "id": "U_062E"
                  }
                ]
              },
              {
                "id": "K_C",
                "text": "چ",
                "sk": [
                  {
                    "text": "خ",
                    "id": "U_062E"
                  },
                  {
                    "text": "ح",
                    "id": "U_062D"
                  },
                  {
                    "text": "ج",
                    "id": "U_062C"
                  }
                ]
              },
              {
                "id": "K_V",
                "text": "ـ"
              },
              {
                "id": "K_B",
                "text": "ب",
                "sk": [
                  {
                    "text": "پ",
                    "id": "U_067E"
                  }
                ]
              },
              {
                "id": "K_N",
                "text": "ن",
                "sk": [
                  {
                    "text": "ں",
                    "id": "U_06BA"
                  }
                ]
              },
              {
                "id": "K_M",
                "text": "م"
              },
              {
                "id": "K_PERIOD",
                "text": ".",
                "sk": [
                  {
                    "text": "'",
                    "id": "K_QUOTE"
                  },
                  {
                    "text": "\"",
                    "id": "K_QUOTE",
                    "layer": "shift"
                  },
                  {
                    "text": "‹",
                    "id": "U_203A"
                  },
                  {
                    "text": "›",
                    "id": "U_2039"
                  },
                  {
                    "text": "«",
                    "id": "U_00BB"
                  },
                  {
                    "text": "»",
                    "id": "U_00AB"
                  },
                  {
                    "text": "؛",
                    "id": "U_061B"
                  },
                  {
                    "text": ":",
                    "id": "K_COLON",
                    "layer": "shift"
                  },
                  {
                    "text": "!",
                    "id": "K_1",
                    "layer": "shift"
                  },
                  {
                    "text": "؟",
                    "id": "U_061F",
                    "layer": "shift"
                  },
                  {
                    "text": "،",
                    "id": "U_060C"
                  }
                ]
              },
              {
                "id": "K_BKSP",
                "text": "*BkSp*",
                "width": "100",
                "sp": "1"
              }
            ]
          },
          {
            "id": 4,
            "key": [
              {
                "id": "K_NUMLOCK",
                "text": "*123*",
                "width": "150",
                "sp": "1",
                "nextlayer": "numeric"
              },
              {
                "id": "K_LOPT",
                "text": "*Menu*",
                "width": "120",
                "sp": "1"
              },
              {
                "id": "K_SPACE",
                "text": "",
                "width": "610",
                "sp": "0"
              },
              {
                "id": "K_ENTER",
                "text": "*Enter*",
                "width": "150",
                "sp": "1"
              }
            ]
          }
        ]
      },
      {
        "id": "shift",
        "row": [
          {
            "id": 1,
            "key": [
              {
                "id": "K_Q",
                "text": "ي"
              },
              {
                "id": "K_W",
                "text": "أ"
              },
              {
                "id": "K_E",
                "text": "ۓ"
              },
              {
                "id": "K_R",
                "text": "ڑ"
              },
              {
                "id": "K_T",
                "text": "ٹ"
              },
              {
                "id": "K_Y",
                "text": "ـ"
              },
              {
                "id": "K_U",
                "text": "ۀ"
              },
              {
                "id": "K_I",
                "text": "ً"
              },
              {
                "id": "K_O",
                "text": "ْ"
              },
              {
                "id": "K_P",
                "text": "ّ"
              }
            ]
          },
          {
            "id": 2,
            "key": [
              {
                "id": "K_A",
                "text": "آ",
                "pad": "50"
              },
              {
                "id": "K_S",
                "text": "ش"
              },
              {
                "id": "K_D",
                "text": "ڈ"
              },
              {
                "id": "K_F",
                "text": "ث"
              },
              {
                "id": "K_G",
                "text": "ق"
              },
              {
                "id": "K_H",
                "text": "ص"
              },
              {
                "id": "K_J",
                "text": "ض"
              },
              {
                "id": "K_K",
                "text": "[",
                "sk": [
                  {
                    "text": "(",
                    "id": "U_0029"
                  },
                  {
                    "text": "{",
                    "id": "U_007D"
                  }
                ]
              },
              {
                "id": "K_L",
                "text": "]",
                "sk": [
                  {
                    "text": ")",
                    "id": "U_0028"
                  },
                  {
                    "text": "}",
                    "id": "U_007B"
                  }
                ]
              },
              {
                "id": "T_new_202",
                "text": "",
                "width": "10",
                "sp": "10"
              }
            ]
          },
          {
            "id": 3,
            "key": [
              {
                "id": "K_SHIFT",
                "text": "*Shift*",
                "sp": "2",
                "nextlayer": "default"
              },
              {
                "id": "K_Z",
                "text": "ژ"
              },
              {
                "id": "K_X",
                "text": "خ"
              },
              {
                "id": "K_C",
                "text": "ط"
              },
              {
                "id": "K_V",
                "text": "ظ"
              },
              {
                "id": "K_B",
                "text": "ة"
              },
              {
                "id": "K_N",
                "text": "ع"
              },
              {
                "id": "K_M",
                "text": "غ"
              },
              {
                "id": "K_PERIOD",
                "text": "،"
              },
              {
                "id": "K_BKSP",
                "text": "*BkSp*",
                "sp": "1"
              }
            ]
          },
          {
            "id": 4,
            "key": [
              {
                "id": "K_NUMLOCK",
                "text": "*123*",
                "width": "150",
                "sp": "1",
                "nextlayer": "numeric"
              },
              {
                "id": "K_LOPT",
                "text": "*Menu*",
                "width": "120",
                "sp": "1"
              },
              {
                "id": "K_SPACE",
                "text": "",
                "width": "610",
                "sp": "0"
              },
              {
                "id": "K_ENTER",
                "text": "*Enter*",
                "width": "150",
                "sp": "1"
              }
            ]
          }
        ]
      },
      {
        "id": "numeric",
        "row": [
          {
            "id": 1,
            "key": [
              {
                "id": "K_1",
                "text": "1",
                "sk": [
                  {
                    "text": "۱",
                    "id": "U_06F1"
                  }
                ]
              },
              {
                "id": "K_2",
                "text": "2",
                "sk": [
                  {
                    "text": "۲",
                    "id": "U_06F2"
                  }
                ]
              },
              {
                "id": "K_3",
                "text": "3",
                "sk": [
                  {
                    "text": "۳",
                    "id": "U_06F3"
                  }
                ]
              },
              {
                "id": "K_4",
                "text": "4",
                "sk": [
                  {
                    "text": "۴",
                    "id": "U_06F4"
                  }
                ]
              },
              {
                "id": "K_5",
                "text": "5",
                "sk": [
                  {
                    "text": "۵",
                    "id": "U_06F5"
                  }
                ]
              },
              {
                "id": "K_6",
                "text": "6",
                "sk": [
                  {
                    "text": "۶",
                    "id": "U_06F6"
                  }
                ]
              },
              {
                "id": "K_7",
                "text": "7",
                "sk": [
                  {
                    "text": "۷",
                    "id": "U_06F7"
                  }
                ]
              },
              {
                "id": "K_8",
                "text": "8",
                "sk": [
                  {
                    "text": "۸",
                    "id": "U_06F8"
                  }
                ]
              },
              {
                "id": "K_9",
                "text": "9",
                "sk": [
                  {
                    "text": "۹",
                    "id": "U_06F9"
                  }
                ]
              },
              {
                "id": "K_0",
                "text": "0",
                "sk": [
                  {
                    "text": "۰",
                    "id": "U_06F0"
                  }
                ]
              }
            ]
          },
          {
            "id": 2,
            "key": [
              {
                "id": "K_4",
                "text": "",
                "pad": "50",
                "layer": "shift"
              },
              {
                "id": "K_2",
                "text": "",
                "layer": "shift"
              },
              {
                "id": "K_3",
                "text": "",
                "layer": "shift"
              },
              {
                "id": "K_5",
                "text": "",
                "layer": "shift"
              },
              {
                "id": "K_6",
                "text": "‌",
                "layer": "shift"
              },
              {
                "id": "K_HYPHEN",
                "text": "",
                "layer": "shift"
              },
              {
                "id": "K_EQUAL",
                "text": "",
                "layer": "default"
              },
              {
                "id": "K_BKSLASH",
                "text": "",
                "layer": "shift"
              },
              {
                "id": "K_BKSLASH",
                "text": "",
                "layer": "default"
              },
              {
                "id": "T_new_236",
                "text": "",
                "width": "10",
                "sp": "10"
              }
            ]
          },
          {
            "id": 3,
            "key": [
              {
                "id": "K_LBRKT",
                "text": "",
                "pad": "110"
              },
              {
                "id": "K_9",
                "text": "",
                "layer": "shift"
              },
              {
                "id": "K_0",
                "text": "",
                "layer": "shift"
              },
              {
                "id": "K_RBRKT",
                "text": ""
              },
              {
                "id": "K_EQUAL",
                "text": "",
                "layer": "shift"
              },
              {
                "id": "K_HYPHEN",
                "text": ""
              },
              {
                "id": "K_8",
                "text": "",
                "layer": "shift"
              },
              {
                "id": "K_SLASH",
                "text": ""
              },
              {
                "id": "K_BKSP",
                "text": "*BkSp*",
                "width": "100",
                "sp": "1"
              }
            ]
          },
          {
            "id": 4,
            "key": [
              {
                "id": "K_LOWER",
                "text": "*abc*",
                "width": "150",
                "sp": "1",
                "nextlayer": "default"
              },
              {
                "id": "K_LOPT",
                "text": "*Menu*",
                "width": "120",
                "sp": "1"
              },
              {
                "id": "K_SPACE",
                "text": "",
                "width": "610",
                "sp": "0"
              },
              {
                "id": "K_ENTER",
                "text": "*Enter*",
                "width": "150",
                "sp": "1"
              }
            ]
          }
        ]
      }
    ]
  }
}
;
  this.s_dkf005c=" 0123456789";
  this.s_dkt005c="\\۰۱۲۳۴۵۶۷۸۹";
  this.KVER="10.0.967.0";
  this.gs=function(t,e) {
    return this.g_main(t,e);
  };
  this.g_main=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, 0x4010, 0x39)) {   // Line 30
      r=m=1;
      k.KO(0,t,")");
    }
    else if(k.KKM(e, 0x4010, 0x30)) {   // Line 33
      r=m=1;
      k.KO(0,t,"(");
    }
    else if(k.KKM(e, 0x4010, 0xBB)) {   // Line 37
      r=m=1;
      k.KO(0,t,"«");
    }
    else if(k.KKM(e, 0x4000, 0xBC)) {   // Line 79
      r=m=1;
      k.KO(0,t,"،");
    }
    else if(k.KKM(e, 0x4000, 0xBE)) {   // Line 78
      r=m=1;
      k.KO(0,t,".");
    }
    else if(k.KKM(e, 0x4000, 0x30)) {   // Line 52
      r=m=1;
      k.KO(0,t,"0");
    }
    else if(k.KKM(e, 0x4000, 0x31)) {   // Line 61
      r=m=1;
      k.KO(0,t,"1");
    }
    else if(k.KKM(e, 0x4000, 0x32)) {   // Line 60
      r=m=1;
      k.KO(0,t,"2");
    }
    else if(k.KKM(e, 0x4000, 0x33)) {   // Line 59
      r=m=1;
      k.KO(0,t,"3");
    }
    else if(k.KKM(e, 0x4000, 0x34)) {   // Line 58
      r=m=1;
      k.KO(0,t,"4");
    }
    else if(k.KKM(e, 0x4000, 0x35)) {   // Line 57
      r=m=1;
      k.KO(0,t,"5");
    }
    else if(k.KKM(e, 0x4000, 0x36)) {   // Line 56
      r=m=1;
      k.KO(0,t,"6");
    }
    else if(k.KKM(e, 0x4000, 0x37)) {   // Line 55
      r=m=1;
      k.KO(0,t,"7");
    }
    else if(k.KKM(e, 0x4000, 0x38)) {   // Line 54
      r=m=1;
      k.KO(0,t,"8");
    }
    else if(k.KKM(e, 0x4000, 0x39)) {   // Line 53
      r=m=1;
      k.KO(0,t,"9");
    }
    else if(k.KKM(e, 0x4000, 0xBA)) {   // Line 93
      r=m=1;
      k.KO(0,t,"؛");
    }
    else if(k.KKM(e, 0x4010, 0xBC)) {   // Line 42
      r=m=1;
      k.KO(0,t,"›");
    }
    else if(k.KKM(e, 0x4010, 0xBE)) {   // Line 41
      r=m=1;
      k.KO(0,t,"‹");
    }
    else if(k.KKM(e, 0x4010, 0xBF)) {   // Line 34
      r=m=1;
      k.KO(0,t,"؟");
    }
    else if(k.KKM(e, 0x4010, 0x41)) {   // Line 77
      r=m=1;
      k.KO(0,t,"آ");
    }
    else if(k.KKM(e, 0x4010, 0x42)) {   // Line 45
      r=m=1;
      k.KO(0,t,"ة");
    }
    else if(k.KKM(e, 0x4010, 0x43)) {   // Line 72
      r=m=1;
      k.KO(0,t,"ط");
    }
    else if(k.KKM(e, 0x4010, 0x44)) {   // Line 63
      r=m=1;
      k.KO(0,t,"ڈ");
    }
    else if(k.KKM(e, 0x4010, 0x45)) {   // Line 50
      r=m=1;
      k.KO(0,t,"ۓ");
    }
    else if(k.KKM(e, 0x4010, 0x46)) {   // Line 49
      r=m=1;
      k.KO(0,t,"ث");
    }
    else if(k.KKM(e, 0x4010, 0x47)) {   // Line 75
      r=m=1;
      k.KO(0,t,"ق");
    }
    else if(k.KKM(e, 0x4010, 0x48)) {   // Line 29
      r=m=1;
      k.KO(0,t,"ص");
    }
    else if(k.KKM(e, 0x4010, 0x49)) {   // Line 39
      r=m=1;
      k.KO(0,t,"ً");
    }
    else if(k.KKM(e, 0x4010, 0x4A)) {   // Line 28
      r=m=1;
      k.KO(0,t,"ض");
    }
    else if(k.KKM(e, 0x4010, 0x4B)) {   // Line 35
      r=m=1;
      k.KO(0,t,"]");
    }
    else if(k.KKM(e, 0x4010, 0x4C)) {   // Line 43
      r=m=1;
      k.KO(0,t,"[");
    }
    else if(k.KKM(e, 0x4010, 0x4D)) {   // Line 47
      r=m=1;
      k.KO(0,t,"غ");
    }
    else if(k.KKM(e, 0x4010, 0x4E)) {   // Line 48
      r=m=1;
      k.KO(0,t,"ع");
    }
    else if(k.KKM(e, 0x4010, 0x4F)) {   // Line 23
      r=m=1;
      k.KO(0,t,"ْ");
    }
    else if(k.KKM(e, 0x4010, 0x50)) {   // Line 26
      r=m=1;
      k.KO(0,t,"ّ");
    }
    else if(k.KKM(e, 0x4010, 0x51)) {   // Line 51
      r=m=1;
      k.KO(0,t,"ي");
    }
    else if(k.KKM(e, 0x4010, 0x52)) {   // Line 68
      r=m=1;
      k.KO(0,t,"ڑ");
    }
    else if(k.KKM(e, 0x4010, 0x53)) {   // Line 76
      r=m=1;
      k.KO(0,t,"ش");
    }
    else if(k.KKM(e, 0x4010, 0x54)) {   // Line 70
      r=m=1;
      k.KO(0,t,"ٹ");
    }
    else if(k.KKM(e, 0x4010, 0x55)) {   // Line 44
      r=m=1;
      k.KO(0,t,"ۀ");
    }
    else if(k.KKM(e, 0x4010, 0x56)) {   // Line 71
      r=m=1;
      k.KO(0,t,"ظ");
    }
    else if(k.KKM(e, 0x4010, 0x57)) {   // Line 25
      r=m=1;
      k.KO(0,t,"أ");
    }
    else if(k.KKM(e, 0x4010, 0x58)) {   // Line 73
      r=m=1;
      k.KO(0,t,"خ");
    }
    else if(k.KKM(e, 0x4010, 0x5A)) {   // Line 74
      r=m=1;
      k.KO(0,t,"ژ");
    }
    else if(k.KKM(e, 0x4000, 0xDB)) {   // Line 67
      r=m=1;
      k.KO(0,t,"َ");
    }
    else if(k.KKM(e, 0x4000, 0xDC)) {   // Line 20
      r=m=1;
      k.KDO(0,t,0);
    }
    else if(k.KKM(e, 0x4000, 0xDD)) {   // Line 66
      r=m=1;
      k.KO(0,t,"ِ");
    }
    else if(k.KKM(e, 0x4010, 0x36)) {   // Line 69
      r=m=1;
      k.KO(0,t,"‌");
    }
    else if(k.KKM(e, 0x4010, 0xBD)) {   // Line 38
      r=m=1;
      k.KO(0,t,"»");
    }
    else if(k.KKM(e, 0x4000, 0xC0)) {   // Line 24
      r=m=1;
      k.KO(0,t,"ء");
    }
    else if(k.KKM(e, 0x4000, 0x41)) {   // Line 101
      r=m=1;
      k.KO(0,t,"ا");
    }
    else if(k.KKM(e, 0x4000, 0x42)) {   // Line 82
      r=m=1;
      k.KO(0,t,"ب");
    }
    else if(k.KKM(e, 0x4000, 0x43)) {   // Line 83
      r=m=1;
      k.KO(0,t,"چ");
    }
    else if(k.KKM(e, 0x4000, 0x44)) {   // Line 99
      r=m=1;
      k.KO(0,t,"د");
    }
    else if(k.KKM(e, 0x4000, 0x45)) {   // Line 90
      r=m=1;
      k.KO(0,t,"ے");
    }
    else if(k.KKM(e, 0x4000, 0x46)) {   // Line 98
      r=m=1;
      k.KO(0,t,"ف");
    }
    else if(k.KKM(e, 0x4000, 0x47)) {   // Line 64
      r=m=1;
      k.KO(0,t,"گ");
    }
    else if(k.KKM(e, 0x4000, 0x48)) {   // Line 97
      r=m=1;
      k.KO(0,t,"ه");
    }
    else if(k.KKM(e, 0x4000, 0x49)) {   // Line 62
      r=m=1;
      k.KO(0,t,"ێ");
    }
    else if(k.KKM(e, 0x4000, 0x4A)) {   // Line 96
      r=m=1;
      k.KO(0,t,"ج");
    }
    else if(k.KKM(e, 0x4000, 0x4B)) {   // Line 95
      r=m=1;
      k.KO(0,t,"ک");
    }
    else if(k.KKM(e, 0x4000, 0x4C)) {   // Line 94
      r=m=1;
      k.KO(0,t,"ل");
    }
    else if(k.KKM(e, 0x4000, 0x4D)) {   // Line 80
      r=m=1;
      k.KO(0,t,"م");
    }
    else if(k.KKM(e, 0x4000, 0x4E)) {   // Line 81
      r=m=1;
      k.KO(0,t,"ن");
    }
    else if(k.KKM(e, 0x4000, 0x4F)) {   // Line 22
      r=m=1;
      k.KO(0,t,"ُ");
    }
    else if(k.KKM(e, 0x4000, 0x50)) {   // Line 86
      r=m=1;
      k.KO(0,t,"پ");
    }
    else if(k.KKM(e, 0x4000, 0x51)) {   // Line 92
      r=m=1;
      k.KO(0,t,"ئ");
    }
    else if(k.KKM(e, 0x4000, 0x52)) {   // Line 89
      r=m=1;
      k.KO(0,t,"ر");
    }
    else if(k.KKM(e, 0x4000, 0x53)) {   // Line 100
      r=m=1;
      k.KO(0,t,"س");
    }
    else if(k.KKM(e, 0x4000, 0x54)) {   // Line 88
      r=m=1;
      k.KO(0,t,"ت");
    }
    else if(k.KKM(e, 0x4000, 0x55)) {   // Line 65
      r=m=1;
      k.KO(0,t,"ۆ");
    }
    else if(k.KKM(e, 0x4000, 0x56)) {   // Line 46
      r=m=1;
      k.KO(0,t,"ـ");
    }
    else if(k.KKM(e, 0x4000, 0x57)) {   // Line 91
      r=m=1;
      k.KO(0,t,"و");
    }
    else if(k.KKM(e, 0x4000, 0x58)) {   // Line 84
      r=m=1;
      k.KO(0,t,"ح");
    }
    else if(k.KKM(e, 0x4000, 0x59)) {   // Line 87
      r=m=1;
      k.KO(0,t,"ی");
    }
    else if(k.KKM(e, 0x4000, 0x5A)) {   // Line 85
      r=m=1;
      k.KO(0,t,"ز");
    }
    else if(k.KKM(e, 0x4010, 0xDB)) {   // Line 32
      r=m=1;
      k.KO(0,t,"}");
    }
    else if(k.KKM(e, 0x4010, 0xDC)) {   // Line 36
      r=m=1;
      k.KO(0,t,"ں");
    }
    else if(k.KKM(e, 0x4010, 0xDD)) {   // Line 31
      r=m=1;
      k.KO(0,t,"{");
    }
    else if(k.KKM(e, 0x4010, 0xC0)) {   // Line 27
      r=m=1;
      k.KO(0,t,"ٔ");
    }
    if(m) {
    
      r=this.g_deadkeys(t,e);
    }
    return r;
  };
  this.g_deadkeys=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
    if(k.KDM(1,t,0)&&k.KA(0,k.KC(1,1,t),this.s_dkf005c)) {   // Line 110
      m=1;
      k.KIO(1,this.s_dkt005c,1,t);
    }
    return r;
  };
}
