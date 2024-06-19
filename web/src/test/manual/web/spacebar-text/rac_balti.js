
KeymanWeb.KR(new Keyboard_rac_balti());

function Keyboard_rac_balti()
{
  
  this.KI="Keyboard_rac_balti";
  this.KN="Rachitrali-Balti";
  this.KMINVER="9.0";
  this.KV={F:' 1em "Scheherazade"',K102:1};
  this.KDU=0;
  this.KV.KLS={
    "default": ["ڃ","۱","۲","۳","۴","۵","۶","۷","۸","۹","۰","-","=","","","","ق","و","ع","ر","ت","ے","ء","ی","ہ","پ","ڇ","ڗ","ݜ","","","","ا","س","د","ف","گ","ح","ج","ک","ل","؛","'","","","","","","\\","ز","ش","چ","ط","ب","ن","م","،","۔","/","","","","","",""],
    "shift": ["ٸ","1","2","3","4","5","6","7","8","9","0","_","+","","",""," کٔ","ّ","ٰ","ڑ","ٹ","َ","ئ","ِ","ۃ","ُ","ݩ","\\","|","","","","آ","ص","ڈ","ݨ","غ","ھ","ض","خ","ٔ",":","\"","","","","","","|","ذ","ژ","ث","ظ","ً","ں","٬","<",">","؟","","","","","",""],
    "ctrl-alt": ["","!","@","#","","%","","&","*","(",")","ْ","","","","","؂","ٖ","ؓ","ؔ","؁","ٔ","ؑ","","","","","","\\","","","","ﷲ","ؐ","ﷺ","","","ؒ","ﷻ","<",">","","","","","","","","","","؃","﷽","؀","","","","","|","/","","","","","",""]
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
  })(this.KV.KLS);
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=0x0070;
  this.KRTL=1;
  this.KVKL={
  "tablet": {
    "layer": [
      {
        "id": "default",
        "row": [
          {
            "id": 1,
            "key": [
              {
                "id": "K_1",
                "text": "۱"
              },
              {
                "id": "K_2",
                "text": "۲"
              },
              {
                "id": "K_3",
                "text": "۳"
              },
              {
                "id": "K_4",
                "text": "۴"
              },
              {
                "id": "K_5",
                "text": "۵"
              },
              {
                "id": "K_6",
                "text": "۶"
              },
              {
                "id": "K_7",
                "text": "۷"
              },
              {
                "id": "K_8",
                "text": "۸"
              },
              {
                "id": "K_9",
                "text": "۹"
              },
              {
                "id": "K_0",
                "text": "۰"
              },
              {
                "id": "K_HYPHEN",
                "text": "-"
              },
              {
                "id": "K_EQUAL",
                "text": "="
              }
            ]
          },
          {
            "id": 2,
            "key": [
              {
                "id": "K_Q",
                "text": "ق",
                "pad": ""
              },
              {
                "id": "K_W",
                "text": "و"
              },
              {
                "id": "K_E",
                "text": "ع"
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
                "text": "ے"
              },
              {
                "id": "K_U",
                "text": "ء"
              },
              {
                "id": "K_I",
                "text": "ی"
              },
              {
                "id": "K_O",
                "text": "ہ"
              },
              {
                "id": "K_P",
                "text": "پ"
              },
              {
                "id": "K_LBRKT",
                "text": "ڇ"
              },
              {
                "id": "K_RBRKT",
                "text": "ڗ"
              }
            ]
          },
          {
            "id": 3,
            "key": [
              {
                "id": "K_BKQUOTE",
                "text": "ڃ"
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
                "text": "ح"
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
                "text": "'"
              }
            ]
          },
          {
            "id": 4,
            "key": [
              {
                "id": "K_SHIFT",
                "text": "*Shift*",
                "width": "",
                "sp": "1",
                "nextlayer": "shift"
              },
              {
                "id": "K_Z",
                "text": "ز"
              },
              {
                "id": "K_X",
                "text": "ش"
              },
              {
                "id": "K_C",
                "text": "چ"
              },
              {
                "id": "K_V",
                "text": "ط"
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
                "text": "۔"
              },
              {
                "id": "K_BKSLASH",
                "text": "ݜ"
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
            "id": 5,
            "key": [
              {
                "id": "K_LCONTROL",
                "text": "*Symbol*",
                "width": "",
                "sp": "1",
                "nextlayer": "symbols"
              },
              {
                "id": "K_LOPT",
                "text": "*Menu*",
                "width": "",
                "sp": "1",
                "nextlayer": "shift"
              },
              {
                "id": "K_SPACE",
                "text": "",
                "width": "930"
              },
              {
                "id": "K_ENTER",
                "text": "*Enter*",
                "width": "110",
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
              }
            ]
          },
          {
            "id": 2,
            "key": [
              {
                "id": "K_Q",
                "text": "کٔ",
                "pad": ""
              },
              {
                "id": "K_W",
                "text": "ّ"
              },
              {
                "id": "K_E",
                "text": "ٰ"
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
                "text": "َ"
              },
              {
                "id": "K_U",
                "text": "ئ"
              },
              {
                "id": "K_I",
                "text": "ِ"
              },
              {
                "id": "K_O",
                "text": "ۃ"
              },
              {
                "id": "K_P",
                "text": "ُ"
              }
            ]
          },
          {
            "id": 3,
            "key": [
              {
                "id": "K_BKQUOTE",
                "text": "ٸ"
              },
              {
                "id": "K_A",
                "text": "آ"
              },
              {
                "id": "K_S",
                "text": "ص"
              },
              {
                "id": "K_D",
                "text": "ڈ"
              },
              {
                "id": "K_F",
                "text": "ݨ"
              },
              {
                "id": "K_G",
                "text": "غ"
              },
              {
                "id": "K_H",
                "text": "ھ"
              },
              {
                "id": "K_J",
                "text": "ض"
              },
              {
                "id": "K_K",
                "text": "خ"
              },
              {
                "id": "K_L",
                "text": "ٔ"
              }
            ]
          },
          {
            "id": 4,
            "key": [
              {
                "id": "K_SHIFT",
                "text": "*Shift*",
                "width": "",
                "sp": "1",
                "nextlayer": "default"
              },
              {
                "id": "K_Z",
                "text": "ذ"
              },
              {
                "id": "K_X",
                "text": "ژ"
              },
              {
                "id": "K_C",
                "text": "ث"
              },
              {
                "id": "K_V",
                "text": "ظ"
              },
              {
                "id": "K_B",
                "text": "ً"
              },
              {
                "id": "K_N",
                "text": "ں"
              },
              {
                "id": "K_LBRKT",
                "text": "ݩ"
              },
              {
                "id": "K_SLASH",
                "text": "؟"
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
            "id": 5,
            "key": [
              {
                "id": "K_LCONTROL",
                "text": "*Symbol*",
                "width": "",
                "sp": "1",
                "nextlayer": "symbols"
              },
              {
                "id": "K_LOPT",
                "text": "*Menu*",
                "width": "",
                "sp": "1"
              },
              {
                "id": "K_SPACE",
                "text": "",
                "width": "705"
              },
              {
                "id": "K_ENTER",
                "text": "*Enter*",
                "width": "125",
                "sp": "1"
              }
            ]
          }
        ]
      },
      {
        "id": "symbols",
        "row": [
          {
            "id": 1,
            "key": [
              {
                "id": "U_0021",
                "text": "!"
              },
              {
                "id": "U_0040",
                "text": "@"
              },
              {
                "id": "U_0023",
                "text": "#"
              },
              {
                "id": "U_0025",
                "text": "%"
              },
              {
                "id": "U_0026",
                "text": "&"
              },
              {
                "id": "U_002A",
                "text": "*"
              },
              {
                "id": "U_0028",
                "text": "("
              },
              {
                "id": "U_0029",
                "text": ")"
              },
              {
                "id": "U_005F",
                "text": "_"
              },
              {
                "id": "U_002B",
                "text": "+"
              }
            ]
          },
          {
            "id": 2,
            "key": [
              {
                "id": "U_0602",
                "text": "؂",
                "pad": ""
              },
              {
                "id": "U_0656",
                "text": "ٖ"
              },
              {
                "id": "U_0613",
                "text": "ؓ"
              },
              {
                "id": "U_0614",
                "text": "ؔ"
              },
              {
                "id": "U_0601",
                "text": "؁"
              },
              {
                "id": "U_0654",
                "text": "ٔ"
              },
              {
                "id": "U_0611",
                "text": "ؑ"
              },
              {
                "id": "U_0652",
                "text": "ْ"
              },
              {
                "id": "U_007B",
                "text": "{"
              },
              {
                "id": "U_007D",
                "text": "}"
              }
            ]
          },
          {
            "id": 3,
            "key": [
              {
                "id": "U_FDF2",
                "text": "ﷲ"
              },
              {
                "id": "U_0610",
                "text": "ؐ"
              },
              {
                "id": "U_0612",
                "text": "ؒ"
              },
              {
                "id": "U_FDFB",
                "text": "ﷻ"
              },
              {
                "id": "U_0603",
                "text": "؃"
              },
              {
                "id": "U_FDFD",
                "text": "﷽"
              },
              {
                "id": "U_FDFA",
                "text": "ﷺ"
              },
              {
                "id": "U_0600",
                "text": "؀"
              },
              {
                "id": "U_003C",
                "text": "<"
              },
              {
                "id": "U_003E",
                "text": ">"
              }
            ]
          },
          {
            "id": 4,
            "key": [
              {
                "id": "K_SHIFT",
                "text": "*Shift*",
                "width": "",
                "sp": "1",
                "nextlayer": "shift"
              },
              {
                "id": "U_003A",
                "text": ":"
              },
              {
                "id": "U_0022",
                "text": "\""
              },
              {
                "id": "U_002F",
                "text": "/"
              },
              {
                "id": "U_007C",
                "text": "|"
              },
              {
                "id": "U_005C",
                "text": "\\"
              },
              {
                "id": "U_002C",
                "text": ","
              },
              {
                "id": "U_005B",
                "text": "["
              },
              {
                "id": "U_005D",
                "text": "]"
              },
              {
                "id": "K_BKSP",
                "text": "*BkSp*",
                "width": "",
                "sp": "1"
              }
            ]
          },
          {
            "id": 5,
            "key": [
              {
                "id": "K_LCONTROL",
                "text": "*abc*",
                "width": "",
                "sp": "1",
                "nextlayer": "default"
              },
              {
                "id": "K_LOPT",
                "text": "*Menu*",
                "width": "",
                "sp": "1"
              },
              {
                "id": "K_SPACE",
                "text": "",
                "pad": "",
                "width": "705"
              },
              {
                "id": "K_ENTER",
                "text": "*Enter*",
                "width": "125",
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
  this.KVER="10.0.1105.0";
  this.gs=function(t,e) {
    return this.g_main(t,e);
  };
  this.g_main=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, 0x4000, 0xE2)) {   // Line 32
      r=m=1;
      k.KO(0,t,"\\");
    }
    else if(k.KKM(e, 0x4010, 0xE2)) {   // Line 99
      r=m=1;
      k.KO(0,t,"|");
    }
    else if(k.KKM(e, 0x4010, 0x31)) {   // Line 126
      r=m=1;
      k.KO(0,t,"1");
    }
    else if(k.KKM(e, 0x4010, 0xDE)) {   // Line 100
      r=m=1;
      k.KO(0,t,"\"");
    }
    else if(k.KKM(e, 0x4010, 0x33)) {   // Line 124
      r=m=1;
      k.KO(0,t,"3");
    }
    else if(k.KKM(e, 0x4010, 0x34)) {   // Line 123
      r=m=1;
      k.KO(0,t,"4");
    }
    else if(k.KKM(e, 0x4010, 0x35)) {   // Line 122
      r=m=1;
      k.KO(0,t,"5");
    }
    else if(k.KKM(e, 0x4010, 0x37)) {   // Line 120
      r=m=1;
      k.KO(0,t,"7");
    }
    else if(k.KKM(e, 0x4000, 0xDE)) {   // Line 51
      r=m=1;
      k.KO(0,t,"'");
    }
    else if(k.KKM(e, 0x4010, 0x39)) {   // Line 118
      r=m=1;
      k.KO(0,t,"9");
    }
    else if(k.KKM(e, 0x4010, 0x30)) {   // Line 117
      r=m=1;
      k.KO(0,t,"0");
    }
    else if(k.KKM(e, 0x4010, 0x38)) {   // Line 119
      r=m=1;
      k.KO(0,t,"8");
    }
    else if(k.KKM(e, 0x4010, 0xBB)) {   // Line 115
      r=m=1;
      k.KO(0,t,"+");
    }
    else if(k.KKM(e, 0x4000, 0xBC)) {   // Line 43
      r=m=1;
      k.KO(0,t,"،");
    }
    else if(k.KKM(e, 0x4060, 0xBD)) {   // Line 36
      r=m=1;
      k.KO(0,t,"ْ");
    }
    else if(k.KKM(e, 0x4000, 0xBD)) {   // Line 74
      r=m=1;
      k.KO(0,t,"-");
    }
    else if(k.KKM(e, 0x4060, 0xBE)) {   // Line 24
      r=m=1;
      k.KO(0,t,"|");
    }
    else if(k.KKM(e, 0x4000, 0xBE)) {   // Line 42
      r=m=1;
      k.KO(0,t,"۔");
    }
    else if(k.KKM(e, 0x4060, 0xBF)) {   // Line 25
      r=m=1;
      k.KO(0,t,"/");
    }
    else if(k.KKM(e, 0x4000, 0xBF)) {   // Line 40
      r=m=1;
      k.KO(0,t,"/");
    }
    else if(k.KKM(e, 0x4000, 0x30)) {   // Line 75
      r=m=1;
      k.KO(0,t,"۰");
    }
    else if(k.KKM(e, 0x4060, 0x30)) {   // Line 143
      r=m=1;
      k.KO(0,t,")");
    }
    else if(k.KKM(e, 0x4000, 0x31)) {   // Line 84
      r=m=1;
      k.KO(0,t,"۱");
    }
    else if(k.KKM(e, 0x4060, 0x31)) {   // Line 150
      r=m=1;
      k.KO(0,t,"!");
    }
    else if(k.KKM(e, 0x4000, 0x32)) {   // Line 83
      r=m=1;
      k.KO(0,t,"۲");
    }
    else if(k.KKM(e, 0x4060, 0x32)) {   // Line 149
      r=m=1;
      k.KO(0,t,"@");
    }
    else if(k.KKM(e, 0x4000, 0x33)) {   // Line 82
      r=m=1;
      k.KO(0,t,"۳");
    }
    else if(k.KKM(e, 0x4060, 0x33)) {   // Line 148
      r=m=1;
      k.KO(0,t,"#");
    }
    else if(k.KKM(e, 0x4000, 0x34)) {   // Line 81
      r=m=1;
      k.KO(0,t,"۴");
    }
    else if(k.KKM(e, 0x4000, 0x35)) {   // Line 80
      r=m=1;
      k.KO(0,t,"۵");
    }
    else if(k.KKM(e, 0x4060, 0x35)) {   // Line 147
      r=m=1;
      k.KO(0,t,"%");
    }
    else if(k.KKM(e, 0x4000, 0x36)) {   // Line 79
      r=m=1;
      k.KO(0,t,"۶");
    }
    else if(k.KKM(e, 0x4000, 0x37)) {   // Line 78
      r=m=1;
      k.KO(0,t,"۷");
    }
    else if(k.KKM(e, 0x4060, 0x37)) {   // Line 146
      r=m=1;
      k.KO(0,t,"&");
    }
    else if(k.KKM(e, 0x4000, 0x38)) {   // Line 77
      r=m=1;
      k.KO(0,t,"۸");
    }
    else if(k.KKM(e, 0x4060, 0x38)) {   // Line 145
      r=m=1;
      k.KO(0,t,"*");
    }
    else if(k.KKM(e, 0x4000, 0x39)) {   // Line 76
      r=m=1;
      k.KO(0,t,"۹");
    }
    else if(k.KKM(e, 0x4060, 0x39)) {   // Line 144
      r=m=1;
      k.KO(0,t,"(");
    }
    else if(k.KKM(e, 0x4010, 0xBA)) {   // Line 101
      r=m=1;
      k.KO(0,t,":");
    }
    else if(k.KKM(e, 0x4000, 0xBA)) {   // Line 52
      r=m=1;
      k.KO(0,t,"؛");
    }
    else if(k.KKM(e, 0x4010, 0xBC)) {   // Line 91
      r=m=1;
      k.KO(0,t,"<");
    }
    else if(k.KKM(e, 0x4000, 0xBB)) {   // Line 73
      r=m=1;
      k.KO(0,t,"=");
    }
    else if(k.KKM(e, 0x4010, 0xBE)) {   // Line 37
      r=m=1;
      k.KO(0,t,">");
    }
    else if(k.KKM(e, 0x4010, 0xBF)) {   // Line 88
      r=m=1;
      k.KO(0,t,"؟");
    }
    else if(k.KKM(e, 0x4010, 0x32)) {   // Line 125
      r=m=1;
      k.KO(0,t,"2");
    }
    else if(k.KKM(e, 0x4010, 0x41)) {   // Line 107
      r=m=1;
      k.KO(0,t,"آ");
    }
    else if(k.KKM(e, 0x4010, 0x42)) {   // Line 94
      r=m=1;
      k.KO(0,t,"ً");
    }
    else if(k.KKM(e, 0x4010, 0x43)) {   // Line 96
      r=m=1;
      k.KO(0,t,"ث");
    }
    else if(k.KKM(e, 0x4010, 0x44)) {   // Line 105
      r=m=1;
      k.KO(0,t,"ڈ");
    }
    else if(k.KKM(e, 0x4010, 0x45)) {   // Line 114
      r=m=1;
      k.KO(0,t,"ٰ");
    }
    else if(k.KKM(e, 0x4010, 0x46)) {   // Line 30
      r=m=1;
      k.KO(0,t,"ݨ");
    }
    else if(k.KKM(e, 0x4010, 0x47)) {   // Line 89
      r=m=1;
      k.KO(0,t,"غ");
    }
    else if(k.KKM(e, 0x4010, 0x48)) {   // Line 104
      r=m=1;
      k.KO(0,t,"ھ");
    }
    else if(k.KKM(e, 0x4010, 0x49)) {   // Line 111
      r=m=1;
      k.KO(0,t,"ِ");
    }
    else if(k.KKM(e, 0x4010, 0x4A)) {   // Line 103
      r=m=1;
      k.KO(0,t,"ض");
    }
    else if(k.KKM(e, 0x4010, 0x4B)) {   // Line 102
      r=m=1;
      k.KO(0,t,"خ");
    }
    else if(k.KKM(e, 0x4010, 0x4C)) {   // Line 22
      r=m=1;
      k.KO(0,t,"ٔ");
    }
    else if(k.KKM(e, 0x4010, 0x4D)) {   // Line 92
      r=m=1;
      k.KO(0,t,"٬");
    }
    else if(k.KKM(e, 0x4010, 0x4E)) {   // Line 93
      r=m=1;
      k.KO(0,t,"ں");
    }
    else if(k.KKM(e, 0x4010, 0x4F)) {   // Line 110
      r=m=1;
      k.KO(0,t,"ۃ");
    }
    else if(k.KKM(e, 0x4010, 0x50)) {   // Line 109
      r=m=1;
      k.KO(0,t,"ُ");
    }
    else if(k.KKM(e, 0x4010, 0x51)) {   // Line 23
      r=m=1;
      k.KO(0,t,"کٔ");
    }
    else if(k.KKM(e, 0x4010, 0x52)) {   // Line 39
      r=m=1;
      k.KO(0,t,"ڑ");
    }
    else if(k.KKM(e, 0x4010, 0x53)) {   // Line 106
      r=m=1;
      k.KO(0,t,"ص");
    }
    else if(k.KKM(e, 0x4010, 0x54)) {   // Line 113
      r=m=1;
      k.KO(0,t,"ٹ");
    }
    else if(k.KKM(e, 0x4010, 0x55)) {   // Line 112
      r=m=1;
      k.KO(0,t,"ئ");
    }
    else if(k.KKM(e, 0x4010, 0x56)) {   // Line 95
      r=m=1;
      k.KO(0,t,"ظ");
    }
    else if(k.KKM(e, 0x4010, 0x57)) {   // Line 26
      r=m=1;
      k.KO(0,t,"ّ");
    }
    else if(k.KKM(e, 0x4010, 0x58)) {   // Line 97
      r=m=1;
      k.KO(0,t,"ژ");
    }
    else if(k.KKM(e, 0x4010, 0x59)) {   // Line 38
      r=m=1;
      k.KO(0,t,"َ");
    }
    else if(k.KKM(e, 0x4010, 0x5A)) {   // Line 98
      r=m=1;
      k.KO(0,t,"ذ");
    }
    else if(k.KKM(e, 0x4000, 0xDB)) {   // Line 63
      r=m=1;
      k.KO(0,t,"ڇ");
    }
    else if(k.KKM(e, 0x4060, 0xDC)) {   // Line 35
      r=m=1;
      k.KO(0,t,"\\");
    }
    else if(k.KKM(e, 0x4000, 0xDC)) {   // Line 41
      r=m=1;
      k.KO(0,t,"ݜ");
    }
    else if(k.KKM(e, 0x4000, 0xDD)) {   // Line 62
      r=m=1;
      k.KO(0,t,"ڗ");
    }
    else if(k.KKM(e, 0x4010, 0x36)) {   // Line 121
      r=m=1;
      k.KO(0,t,"6");
    }
    else if(k.KKM(e, 0x4010, 0xBD)) {   // Line 116
      r=m=1;
      k.KO(0,t,"_");
    }
    else if(k.KKM(e, 0x4000, 0xC0)) {   // Line 85
      r=m=1;
      k.KO(0,t,"ڃ");
    }
    else if(k.KKM(e, 0x4000, 0x41)) {   // Line 61
      r=m=1;
      k.KO(0,t,"ا");
    }
    else if(k.KKM(e, 0x4060, 0x41)) {   // Line 136
      r=m=1;
      k.KO(0,t,"ﷲ");
    }
    else if(k.KKM(e, 0x4000, 0x42)) {   // Line 46
      r=m=1;
      k.KO(0,t,"ب");
    }
    else if(k.KKM(e, 0x4000, 0x43)) {   // Line 48
      r=m=1;
      k.KO(0,t,"چ");
    }
    else if(k.KKM(e, 0x4060, 0x43)) {   // Line 131
      r=m=1;
      k.KO(0,t,"﷽");
    }
    else if(k.KKM(e, 0x4060, 0x44)) {   // Line 34
      r=m=1;
      k.KO(0,t,"ﷺ");
    }
    else if(k.KKM(e, 0x4000, 0x44)) {   // Line 59
      r=m=1;
      k.KO(0,t,"د");
    }
    else if(k.KKM(e, 0x4000, 0x45)) {   // Line 70
      r=m=1;
      k.KO(0,t,"ع");
    }
    else if(k.KKM(e, 0x4060, 0x45)) {   // Line 141
      r=m=1;
      k.KO(0,t,"ؓ");
    }
    else if(k.KKM(e, 0x4000, 0x46)) {   // Line 58
      r=m=1;
      k.KO(0,t,"ف");
    }
    else if(k.KKM(e, 0x4000, 0x47)) {   // Line 57
      r=m=1;
      k.KO(0,t,"گ");
    }
    else if(k.KKM(e, 0x4000, 0x48)) {   // Line 56
      r=m=1;
      k.KO(0,t,"ح");
    }
    else if(k.KKM(e, 0x4060, 0x48)) {   // Line 134
      r=m=1;
      k.KO(0,t,"ؒ");
    }
    else if(k.KKM(e, 0x4000, 0x49)) {   // Line 66
      r=m=1;
      k.KO(0,t,"ی");
    }
    else if(k.KKM(e, 0x4000, 0x4A)) {   // Line 55
      r=m=1;
      k.KO(0,t,"ج");
    }
    else if(k.KKM(e, 0x4060, 0x4A)) {   // Line 133
      r=m=1;
      k.KO(0,t,"ﷻ");
    }
    else if(k.KKM(e, 0x4060, 0x4B)) {   // Line 28
      r=m=1;
      k.KO(0,t,"<");
    }
    else if(k.KKM(e, 0x4000, 0x4B)) {   // Line 54
      r=m=1;
      k.KO(0,t,"ک");
    }
    else if(k.KKM(e, 0x4060, 0x4C)) {   // Line 27
      r=m=1;
      k.KO(0,t,">");
    }
    else if(k.KKM(e, 0x4000, 0x4C)) {   // Line 53
      r=m=1;
      k.KO(0,t,"ل");
    }
    else if(k.KKM(e, 0x4000, 0x4D)) {   // Line 44
      r=m=1;
      k.KO(0,t,"م");
    }
    else if(k.KKM(e, 0x4000, 0x4E)) {   // Line 45
      r=m=1;
      k.KO(0,t,"ن");
    }
    else if(k.KKM(e, 0x4000, 0x4F)) {   // Line 65
      r=m=1;
      k.KO(0,t,"ہ");
    }
    else if(k.KKM(e, 0x4000, 0x50)) {   // Line 64
      r=m=1;
      k.KO(0,t,"پ");
    }
    else if(k.KKM(e, 0x4000, 0x51)) {   // Line 72
      r=m=1;
      k.KO(0,t,"ق");
    }
    else if(k.KKM(e, 0x4060, 0x51)) {   // Line 142
      r=m=1;
      k.KO(0,t,"؂");
    }
    else if(k.KKM(e, 0x4000, 0x52)) {   // Line 69
      r=m=1;
      k.KO(0,t,"ر");
    }
    else if(k.KKM(e, 0x4060, 0x52)) {   // Line 140
      r=m=1;
      k.KO(0,t,"ؔ");
    }
    else if(k.KKM(e, 0x4000, 0x53)) {   // Line 60
      r=m=1;
      k.KO(0,t,"س");
    }
    else if(k.KKM(e, 0x4060, 0x53)) {   // Line 135
      r=m=1;
      k.KO(0,t,"ؐ");
    }
    else if(k.KKM(e, 0x4000, 0x54)) {   // Line 68
      r=m=1;
      k.KO(0,t,"ت");
    }
    else if(k.KKM(e, 0x4060, 0x54)) {   // Line 139
      r=m=1;
      k.KO(0,t,"؁");
    }
    else if(k.KKM(e, 0x4000, 0x55)) {   // Line 33
      r=m=1;
      k.KO(0,t,"ء");
    }
    else if(k.KKM(e, 0x4060, 0x55)) {   // Line 137
      r=m=1;
      k.KO(0,t,"ؑ");
    }
    else if(k.KKM(e, 0x4000, 0x56)) {   // Line 47
      r=m=1;
      k.KO(0,t,"ط");
    }
    else if(k.KKM(e, 0x4060, 0x56)) {   // Line 130
      r=m=1;
      k.KO(0,t,"؀");
    }
    else if(k.KKM(e, 0x4000, 0x57)) {   // Line 71
      r=m=1;
      k.KO(0,t,"و");
    }
    else if(k.KKM(e, 0x4060, 0x57)) {   // Line 129
      r=m=1;
      k.KO(0,t,"ٖ");
    }
    else if(k.KKM(e, 0x4000, 0x58)) {   // Line 49
      r=m=1;
      k.KO(0,t,"ش");
    }
    else if(k.KKM(e, 0x4060, 0x58)) {   // Line 132
      r=m=1;
      k.KO(0,t,"؃");
    }
    else if(k.KKM(e, 0x4000, 0x59)) {   // Line 67
      r=m=1;
      k.KO(0,t,"ے");
    }
    else if(k.KKM(e, 0x4060, 0x59)) {   // Line 138
      r=m=1;
      k.KO(0,t,"ٔ");
    }
    else if(k.KKM(e, 0x4000, 0x5A)) {   // Line 50
      r=m=1;
      k.KO(0,t,"ز");
    }
    else if(k.KKM(e, 0x4010, 0xDB)) {   // Line 31
      r=m=1;
      k.KO(0,t,"ݩ");
    }
    else if(k.KKM(e, 0x4010, 0xDC)) {   // Line 29
      r=m=1;
      k.KO(0,t,"|");
    }
    else if(k.KKM(e, 0x4010, 0xDD)) {   // Line 108
      r=m=1;
      k.KO(0,t,"\\");
    }
    else if(k.KKM(e, 0x4010, 0xC0)) {   // Line 90
      r=m=1;
      k.KO(0,t,"ٸ");
    }
    return r;
  };
}
