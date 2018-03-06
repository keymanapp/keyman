if(typeof keyman === 'undefined') {
  console.log('Keyboard requires KeymanWeb 10.0 or later');
  if(typeof tavultesoft !== 'undefined') tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");
} else {
KeymanWeb.KR(new Keyboard_khmer_angkor());
}
function Keyboard_khmer_angkor()
{
  var modCodes = keyman.osk.modifierCodes;
  var keyCodes = keyman.osk.keyCodes;

  this.KI="Keyboard_khmer_angkor";
  this.KN="Khmer Angkor";
  this.KMINVER="10.0";
  this.KV={F:' 1em "Khmer OS"',K102:0};
  this.KDU=0;
  this.KV.KLS={
    "rightalt": ["‍","‌","@","៑","$","€","៙","៚","*","{","}","≈","៎","","","","ៜ","៝","ឯ","ឫ","ឨ","[","]","ឦ","ឱ","ឰ","ឩ","ឳ","\\","","","","+","-","×","÷",":","‘","’","ឝ","៘","៖","ៈ","","","","","","","<",">","","","ឞ",";","៓",",",".","/","","","","","",""],
    "rightalt-shift": ["","៱","៲","៳","៴","៵","៶","៷","៸","៹","៰","","","","","","᧠","᧡","᧢","᧣","᧤","᧥","᧦","᧧","᧨","᧩","᧪","᧫","","","","","᧬","᧭","᧮","᧯","᧰","᧱","᧲","᧳","᧴","᧵","᧶","","","","","","","᧷","᧸","᧹","᧺","᧻","᧼","᧽","᧾","᧿","","","","","","",""],
    "leftctrl-leftalt": ["","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","‍"],
    "default": ["«","១","២","៣","៤","៥","៦","៧","៨","៩","០","ឥ","ឲ","","","","ឆ","ឹ","េ","រ","ត","យ","ុ","ិ","ោ","ផ","ៀ","ឪ","ឮ","","","","ា","ស","ដ","ថ","ង","ហ","្","ក","ល","ើ","់","","","","","","","ឋ","ខ","ច","វ","ប","ន","ម","ុំ","។","៊","","","","","","​"],
    "shift": ["»","!","ៗ","\"","៛","%","៍","័","៏","(",")","៌","=","","","","ឈ","ឺ","ែ","ឬ","ទ","ួ","ូ","ី","ៅ","ភ","ឿ","ឧ","ឭ","","","","ាំ","ៃ","ឌ","ធ","អ","ះ","ញ","គ","ឡ","ោះ","៉","","","","","","","ឍ","ឃ","ជ","េះ","ព","ណ","ំ","ុះ","៕","?","","","","",""," "]
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
  this.KMBM=modCodes.RALT | modCodes.SHIFT /* 0x0018 */;
  this.KVKD="T_17D2_1780 T_17D2_1781 T_17D2_1782 T_17D2_1783 T_17D2_1784 T_17D2_1785 T_17D2_1786 T_17D2_1787 T_17D2_1788 T_17D2_1789 T_17D2_178A T_17D2_178B T_17D2_178C T_17D2_178D T_17D2_178E T_17D2_178F T_17D2_1790 T_17D2_1791 T_17D2_1792 T_17D2_1793 T_17D2_1794 T_17D2_1795 T_17D2_1796 T_17D2_1797 T_17D2_1798 T_17D2_1799 T_17D2_179A T_17D2_179B T_17D2_179C T_17D2_179D T_17D2_179E T_17D2_179F T_17D2_17A0 T_17D2_17A1 T_17D2_17A2 U_0030 U_0031 U_0032 U_0033 U_0034 U_0035 U_0036 U_0037 U_0038 U_0039";
  this.KVKL={
  "tablet": {
    "font": "Khmer OS",
    "layer": [
      {
        "id": "default",
        "row": [
          {
            "id": 1,
            "key": [
              {
                "id": "K_Q",
                "text": "ឆ",
                "pad": "0",
                "sk": [
                  {
                    "text": "ឈ",
                    "id": "K_Q",
                    "layer": "shift"
                  },
                  {
                    "text": "្ឆ",
                    "id": "T_17D2_1786"
                  },
                  {
                    "text": "្ឈ",
                    "id": "T_17D2_1788"
                  }
                ]
              },
              {
                "id": "K_W",
                "text": "ឹ",
                "sk": [
                  {
                    "text": "ឺ",
                    "id": "K_W",
                    "layer": "shift"
                  }
                ]
              },
              {
                "id": "K_E",
                "text": "េ",
                "sk": [
                  {
                    "text": "ែ",
                    "id": "K_E",
                    "layer": "shift"
                  },
                  {
                    "text": "ៃ",
                    "id": "K_S",
                    "layer": "shift"
                  },
                  {
                    "id": "K_V",
                    "text": "េះ",
                    "layer": "shift"
                  },
                  {
                    "text": "ឯ",
                    "id": "U_17AF"
                  },
                  {
                    "text": "ឰ",
                    "id": "U_17B0"
                  }
                ]
              },
              {
                "id": "K_R",
                "text": "រ",
                "sk": [
                  {
                    "text": "្រ",
                    "id": "T_17D2_179A"
                  },
                  {
                    "text": "ឫ",
                    "id": "U_17AB"
                  },
                  {
                    "text": "ឬ",
                    "id": "U_17AC"
                  }
                ]
              },
              {
                "id": "K_T",
                "text": "ត",
                "sk": [
                  {
                    "text": "ទ",
                    "id": "K_T",
                    "layer": "shift"
                  },
                  {
                    "text": "្ត",
                    "id": "T_17D2_178F"
                  },
                  {
                    "text": "្ទ",
                    "id": "T_17D2_1791",
                    "layer": "default"
                  }
                ]
              },
              {
                "id": "K_Y",
                "text": "យ",
                "sk": [
                  {
                    "text": "្យ",
                    "id": "T_17D2_1799"
                  }
                ]
              },
              {
                "id": "K_U",
                "text": "ុ",
                "sk": [
                  {
                    "text": "ូ",
                    "id": "K_U",
                    "layer": "shift"
                  },
                  {
                    "text": "ួ",
                    "id": "K_Y",
                    "layer": "shift"
                  },
                  {
                    "text": "ឧ",
                    "id": "U_17A7"
                  },
                  {
                    "text": "ឪ",
                    "id": "U_17AA",
                    "layer": "shift"
                  },
                  {
                    "text": "ឩ",
                    "id": "U_17A9",
                    "layer": "shift"
                  },
                  {
                    "text": "ឨ",
                    "id": "U_17A8"
                  }
                ]
              },
              {
                "id": "K_I",
                "text": "ិ",
                "sk": [
                  {
                    "text": "ី",
                    "id": "K_I",
                    "layer": "shift"
                  },
                  {
                    "text": "ឥ",
                    "id": "U_17A5"
                  },
                  {
                    "text": "ឦ",
                    "id": "U_17A6",
                    "layer": "shift"
                  }
                ]
              },
              {
                "id": "K_O",
                "text": "ោ",
                "sk": [
                  {
                    "text": "ៅ",
                    "id": "K_O",
                    "layer": "shift"
                  },
                  {
                    "text": "ៀ",
                    "id": "K_LBRKT"
                  },
                  {
                    "text": "ឿ",
                    "id": "K_LBRKT",
                    "layer": "shift"
                  },
                  {
                    "id": "K_COLON",
                    "text": "ោះ",
                    "layer": "shift"
                  },
                  {
                    "text": "ឱ",
                    "id": "U_17B1"
                  },
                  {
                    "text": "ឲ",
                    "id": "U_17B2"
                  },
                  {
                    "text": "ឳ",
                    "id": "U_17B3",
                    "layer": "shift"
                  }
                ]
              },
              {
                "id": "K_P",
                "text": "ផ",
                "sk": [
                  {
                    "text": "ភ",
                    "id": "K_P",
                    "layer": "shift"
                  },
                  {
                    "text": "្ផ",
                    "id": "T_17D2_1795"
                  },
                  {
                    "text": "្ភ",
                    "id": "T_17D2_1797",
                    "layer": "default"
                  }
                ]
              },
              {
                "id": "T_new_645",
                "text": "",
                "width": "25",
                "sp": "10"
              }
            ]
          },
          {
            "id": 2,
            "key": [
              {
                "id": "K_A",
                "text": "ា",
                "pad": "30",
                "width": "100",
                "sk": [
                  {
                    "text": "ាំ",
                    "id": "K_A",
                    "layer": "shift"
                  }
                ]
              },
              {
                "id": "K_S",
                "text": "ស",
                "sk": [
                  {
                    "text": "្ស",
                    "id": "T_17D2_179F"
                  },
                  {
                    "text": "ឝ",
                    "id": "U_179D"
                  },
                  {
                    "text": "ឞ",
                    "id": "U_179E"
                  }
                ]
              },
              {
                "id": "K_D",
                "text": "ដ",
                "sk": [
                  {
                    "text": "ឌ",
                    "id": "K_D",
                    "layer": "shift"
                  },
                  {
                    "text": "្ដ",
                    "id": "T_17D2_178A"
                  },
                  {
                    "text": "្ឌ",
                    "id": "T_17D2_178C",
                    "layer": "default"
                  }
                ]
              },
              {
                "id": "K_F",
                "text": "ថ",
                "sk": [
                  {
                    "text": "ធ",
                    "id": "K_F",
                    "layer": "shift"
                  },
                  {
                    "text": "្ថ",
                    "id": "T_17D2_1790"
                  },
                  {
                    "text": "្ធ",
                    "id": "T_17D2_1792",
                    "layer": "default"
                  }
                ]
              },
              {
                "id": "K_G",
                "text": "ង",
                "sk": [
                  {
                    "text": "អ",
                    "id": "K_G",
                    "layer": "shift"
                  },
                  {
                    "text": "្ង",
                    "id": "T_17D2_1784"
                  },
                  {
                    "text": "្អ",
                    "id": "T_17D2_17A2",
                    "layer": "default"
                  }
                ]
              },
              {
                "id": "K_H",
                "text": "ហ",
                "sk": [
                  {
                    "text": "្ហ",
                    "id": "T_17D2_17A0"
                  },
                  {
                    "text": "ះ",
                    "id": "K_H",
                    "layer": "shift"
                  },
                  {
                    "text": "ៈ",
                    "id": "U_17C8"
                  }
                ]
              },
              {
                "id": "K_J",
                "text": "ញ",
                "layer": "shift",
                "sk": [
                  {
                    "text": "្ញ",
                    "id": "T_17D2_1789"
                  }
                ]
              },
              {
                "id": "K_K",
                "text": "ក",
                "sk": [
                  {
                    "text": "គ",
                    "id": "K_K",
                    "layer": "shift"
                  },
                  {
                    "text": "្ក",
                    "id": "T_17D2_1780"
                  },
                  {
                    "text": "្គ",
                    "id": "T_17D2_1782"
                  }
                ]
              },
              {
                "id": "K_L",
                "text": "ល",
                "sk": [
                  {
                    "text": "ឡ",
                    "id": "K_L",
                    "layer": "shift"
                  },
                  {
                    "text": "្ល",
                    "id": "T_17D2_179B"
                  },
                  {
                    "text": "ឭ",
                    "id": "U_17AD"
                  },
                  {
                    "text": "ឮ",
                    "id": "U_17AE"
                  }
                ]
              },
              {
                "id": "K_COLON",
                "text": "ើ"
              }
            ]
          },
          {
            "id": 3,
            "key": [
              {
                "id": "K_Z",
                "text": "ឋ",
                "pad": "40",
                "sk": [
                  {
                    "text": "ឍ",
                    "id": "K_Z",
                    "layer": "shift"
                  },
                  {
                    "text": "្ឋ",
                    "id": "T_17D2_178B"
                  },
                  {
                    "text": "្ឍ",
                    "id": "T_17D2_178D",
                    "layer": "default"
                  }
                ]
              },
              {
                "id": "K_X",
                "text": "ខ",
                "sk": [
                  {
                    "text": "ឃ",
                    "id": "K_X",
                    "layer": "shift"
                  },
                  {
                    "text": "្ខ",
                    "id": "T_17D2_1781"
                  },
                  {
                    "text": "្ឃ",
                    "id": "T_17D2_1783",
                    "layer": "default"
                  }
                ]
              },
              {
                "id": "K_C",
                "text": "ច",
                "sk": [
                  {
                    "text": "ជ",
                    "id": "K_C",
                    "layer": "shift"
                  },
                  {
                    "text": "្ច",
                    "id": "T_17D2_1785"
                  },
                  {
                    "text": "្ជ",
                    "id": "T_17D2_1787",
                    "layer": "default"
                  }
                ]
              },
              {
                "id": "K_V",
                "text": "វ",
                "sk": [
                  {
                    "text": "្វ",
                    "id": "T_17D2_179C"
                  }
                ]
              },
              {
                "id": "K_B",
                "text": "ប",
                "sk": [
                  {
                    "text": "ព",
                    "id": "K_B",
                    "layer": "shift"
                  },
                  {
                    "text": "្ប",
                    "id": "T_17D2_1794"
                  },
                  {
                    "text": "្ព",
                    "id": "T_17D2_1796",
                    "layer": "default"
                  }
                ]
              },
              {
                "id": "K_N",
                "text": "ន",
                "sk": [
                  {
                    "text": "ណ",
                    "id": "K_N",
                    "layer": "shift"
                  },
                  {
                    "text": "្ន",
                    "id": "T_17D2_1793"
                  },
                  {
                    "text": "្ណ",
                    "id": "T_17D2_178E",
                    "layer": "default"
                  }
                ]
              },
              {
                "id": "K_M",
                "text": "ម",
                "sk": [
                  {
                    "text": "្ម",
                    "id": "T_17D2_1798"
                  },
                  {
                    "id": "K_M",
                    "text": "ំ",
                    "layer": "shift"
                  }
                ]
              },
              {
                "id": "K_COMMA",
                "text": "ុំ",
                "sk": [
                  {
                    "id": "K_COMMA",
                    "text": "ុះ",
                    "layer": "shift"
                  },
                  {
                    "id": "K_6",
                    "text": "៍",
                    "layer": "shift"
                  },
                  {
                    "id": "K_7",
                    "text": "័",
                    "layer": "shift"
                  },
                  {
                    "id": "K_8",
                    "text": "៏",
                    "layer": "shift"
                  },
                  {
                    "id": "K_HYPHEN",
                    "text": "៌",
                    "layer": "shift"
                  },
                  {
                    "id": "U_17D1",
                    "text": "៑",
                    "layer": "shift"
                  },
                  {
                    "id": "U_17DD",
                    "text": "៝",
                    "layer": "shift"
                  },
                  {
                    "id": "U_17CE",
                    "text": "៎",
                    "layer": "shift"
                  }
                ]
              },
              {
                "id": "K_QUOTE",
                "text": "់",
                "width": "90",
                "sk": [
                  {
                    "text": "៉",
                    "id": "K_QUOTE",
                    "layer": "shift"
                  },
                  {
                    "text": "៊",
                    "id": "K_SLASH"
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
                "text": "១២៣",
                "width": "130",
                "sp": "1",
                "nextlayer": "numeric"
              },
              {
                "id": "K_LOPT",
                "text": "*Menu*",
                "width": "140",
                "sp": "1"
              },
              {
                "id": "K_SPACE",
                "text": "​",
                "width": "550",
                "sp": "0",
                "sk": [
                  {
                    "text": " ",
                    "id": "U_0020",
                    "layer": "default"
                  }
                ]
              },
              {
                "id": "K_PERIOD",
                "text": "។",
                "sk": [
                  {
                    "text": "៕",
                    "id": "K_PERIOD",
                    "layer": "shift"
                  },
                  {
                    "text": "!",
                    "id": "U_0021"
                  },
                  {
                    "text": "?",
                    "id": "U_003F"
                  }
                ]
              },
              {
                "id": "K_ENTER",
                "text": "*Enter*",
                "width": "130",
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
                "text": "១",
                "pad": "0",
                "sk": [
                  {
                    "text": "1",
                    "id": "U_0031"
                  }
                ]
              },
              {
                "id": "K_2",
                "text": "២",
                "sk": [
                  {
                    "text": "2",
                    "id": "U_0032"
                  }
                ]
              },
              {
                "id": "K_3",
                "text": "៣",
                "sk": [
                  {
                    "text": "3",
                    "id": "U_0033"
                  }
                ]
              },
              {
                "id": "K_4",
                "text": "៤",
                "sk": [
                  {
                    "text": "4",
                    "id": "U_0034"
                  }
                ]
              },
              {
                "id": "K_5",
                "text": "៥",
                "sk": [
                  {
                    "text": "5",
                    "id": "U_0035"
                  }
                ]
              },
              {
                "id": "K_6",
                "text": "៦",
                "sk": [
                  {
                    "text": "6",
                    "id": "U_0036"
                  }
                ]
              },
              {
                "id": "K_7",
                "text": "៧",
                "sk": [
                  {
                    "text": "7",
                    "id": "U_0037"
                  }
                ]
              },
              {
                "id": "K_8",
                "text": "៨",
                "sk": [
                  {
                    "text": "8",
                    "id": "U_0038"
                  }
                ]
              },
              {
                "id": "K_9",
                "text": "៩",
                "sk": [
                  {
                    "text": "9",
                    "id": "U_0039"
                  }
                ]
              },
              {
                "id": "K_0",
                "text": "០",
                "sk": [
                  {
                    "text": "0",
                    "id": "U_0030"
                  },
                  {
                    "text": "៓",
                    "id": "U_17D3"
                  }
                ]
              },
              {
                "id": "T_new_5912",
                "text": "",
                "width": "25",
                "sp": "10"
              }
            ]
          },
          {
            "id": 2,
            "key": [
              {
                "id": "U_0040",
                "text": "@",
                "pad": "30",
                "sk": [
                  {
                    "text": "©",
                    "id": "U_00A9"
                  },
                  {
                    "text": "®",
                    "id": "U_00AE"
                  }
                ]
              },
              {
                "id": "U_0023",
                "text": "#",
                "sk": [
                  {
                    "text": "№",
                    "id": "U_2116"
                  },
                  {
                    "text": "~",
                    "id": "U_007E"
                  }
                ]
              },
              {
                "id": "U_17DB",
                "text": "៛",
                "sk": [
                  {
                    "text": "$",
                    "id": "U_0024"
                  },
                  {
                    "text": "฿",
                    "id": "U_0E3F"
                  },
                  {
                    "text": "¢",
                    "id": "U_00A2"
                  },
                  {
                    "text": "£",
                    "id": "U_00A3"
                  },
                  {
                    "text": "¥",
                    "id": "U_00A5"
                  }
                ]
              },
              {
                "id": "U_0026",
                "text": "&"
              },
              {
                "id": "U_0025",
                "text": "%",
                "sk": [
                  {
                    "text": "‰",
                    "id": "U_2030"
                  },
                  {
                    "text": "‱",
                    "id": "U_2031"
                  }
                ]
              },
              {
                "id": "U_002B",
                "text": "+",
                "sk": [
                  {
                    "text": "-",
                    "id": "U_002D"
                  },
                  {
                    "text": "×",
                    "id": "U_00D7"
                  },
                  {
                    "text": "÷",
                    "id": "U_00F7"
                  },
                  {
                    "text": "±",
                    "id": "U_00B1"
                  }
                ]
              },
              {
                "id": "U_003D",
                "text": "=",
                "sk": [
                  {
                    "text": "_",
                    "id": "U_005F"
                  },
                  {
                    "text": "≠",
                    "id": "U_2260"
                  }
                ]
              },
              {
                "id": "U_002A",
                "text": "*",
                "sk": [
                  {
                    "text": "^",
                    "id": "U_005E"
                  }
                ]
              },
              {
                "id": "U_003F",
                "text": "?",
                "sk": [
                  {
                    "text": "¿",
                    "id": "U_00BF"
                  }
                ]
              },
              {
                "id": "U_0021",
                "text": "!",
                "sk": [
                  {
                    "text": "¡",
                    "id": "U_00A1"
                  }
                ]
              }
            ]
          },
          {
            "id": 3,
            "key": [
              {
                "id": "K_SYMBOLS",
                "text": "᧠᧡᧢",
                "sp": "1",
                "nextlayer": "lunar_date"
              },
              {
                "id": "U_00AB",
                "text": "«",
                "pad": "",
                "sk": [
                  {
                    "text": "»",
                    "id": "U_00BB"
                  },
                  {
                    "text": "‘",
                    "id": "U_2018"
                  },
                  {
                    "text": "’",
                    "id": "U_2019"
                  },
                  {
                    "text": "“",
                    "id": "U_201C"
                  },
                  {
                    "text": "”",
                    "id": "U_201D"
                  }
                ]
              },
              {
                "id": "U_002F",
                "text": "/",
                "sk": [
                  {
                    "text": "\\",
                    "id": "U_005C"
                  },
                  {
                    "text": "|",
                    "id": "U_007C"
                  },
                  {
                    "text": "¦",
                    "id": "U_00A6"
                  }
                ]
              },
              {
                "id": "U_0028",
                "text": "(",
                "sk": [
                  {
                    "text": ")",
                    "id": "U_0029"
                  },
                  {
                    "text": "[",
                    "id": "U_005B"
                  },
                  {
                    "text": "]",
                    "id": "U_005D"
                  },
                  {
                    "text": "{",
                    "id": "U_007B"
                  },
                  {
                    "text": "}",
                    "id": "U_007D"
                  }
                ]
              },
              {
                "id": "U_17D9",
                "text": "៙",
                "sk": [
                  {
                    "text": "៚",
                    "id": "U_17DA"
                  },
                  {
                    "text": "ៜ",
                    "id": "U_17DC"
                  },
                  {
                    "text": "§",
                    "id": "U_00A7"
                  },
                  {
                    "text": "Ø",
                    "id": "U_00D8"
                  }
                ]
              },
              {
                "id": "U_17D7",
                "text": "ៗ"
              },
              {
                "id": "U_17D8",
                "text": "៘"
              },
              {
                "id": "U_003C",
                "text": "<",
                "sk": [
                  {
                    "text": "≤",
                    "id": "U_2264"
                  },
                  {
                    "text": ">",
                    "id": "U_003E"
                  },
                  {
                    "text": "≥",
                    "id": "U_2265"
                  }
                ]
              },
              {
                "id": "U_17D6",
                "text": "៖",
                "sk": [
                  {
                    "text": ":",
                    "id": "U_003A"
                  },
                  {
                    "text": ";",
                    "id": "U_003B"
                  },
                  {
                    "text": "…",
                    "id": "U_2026"
                  }
                ]
              },
              {
                "id": "K_BKSP",
                "text": "*BkSp*",
                "width": "125",
                "sp": "1"
              }
            ]
          },
          {
            "id": 4,
            "key": [
              {
                "id": "K_LCONTROL",
                "text": "កខគ",
                "width": "130",
                "sp": "1",
                "nextlayer": "default"
              },
              {
                "id": "K_LOPT",
                "text": "*Menu*",
                "width": "140",
                "sp": "1"
              },
              {
                "id": "K_SPACE",
                "text": "​",
                "width": "550",
                "sp": "0",
                "layer": "shift",
                "sk": []
              },
              {
                "id": "K_PERIOD",
                "text": "។",
                "sk": [
                  {
                    "text": "៕",
                    "id": "K_PERIOD",
                    "layer": "shift"
                  },
                  {
                    "text": "!",
                    "id": "U_0021"
                  },
                  {
                    "text": "?",
                    "id": "U_003F"
                  }
                ]
              },
              {
                "id": "K_ENTER",
                "text": "*Enter*",
                "width": "130",
                "sp": "1"
              }
            ]
          }
        ]
      },
      {
        "id": "lunar_date",
        "row": [
          {
            "id": 1,
            "key": [
              {
                "id": "U_17F1",
                "text": "៱",
                "pad": "0"
              },
              {
                "id": "U_17F2",
                "text": "៲"
              },
              {
                "id": "U_17F3",
                "text": "៳"
              },
              {
                "id": "U_17F4",
                "text": "៴"
              },
              {
                "id": "U_17F5",
                "text": "៵"
              },
              {
                "id": "U_17F6",
                "text": "៶"
              },
              {
                "id": "U_17F7",
                "text": "៷"
              },
              {
                "id": "U_17F8",
                "text": "៸"
              },
              {
                "id": "U_17F9",
                "text": "៹"
              },
              {
                "id": "U_17F0",
                "text": "៰"
              },
              {
                "id": "T_new_5912",
                "text": "",
                "width": "25",
                "sp": "10"
              }
            ]
          },
          {
            "id": 2,
            "key": [
              {
                "id": "U_19E0",
                "text": "᧠",
                "pad": "30",
                "sk": [
                  {
                    "text": "᧰",
                    "id": "U_19F0"
                  }
                ]
              },
              {
                "id": "U_19E1",
                "text": "᧡",
                "sk": [
                  {
                    "text": "᧱",
                    "id": "U_19F1"
                  }
                ]
              },
              {
                "id": "U_19E2",
                "text": "᧢",
                "sk": [
                  {
                    "text": "᧲",
                    "id": "U_19F2"
                  }
                ]
              },
              {
                "id": "U_19E3",
                "text": "᧣",
                "sk": [
                  {
                    "text": "᧳",
                    "id": "U_19F3"
                  }
                ]
              },
              {
                "id": "U_19E4",
                "text": "᧤",
                "sk": [
                  {
                    "text": "᧴",
                    "id": "U_19F4"
                  }
                ]
              },
              {
                "id": "U_19E5",
                "text": "᧥",
                "sk": [
                  {
                    "text": "᧵",
                    "id": "U_19F5"
                  }
                ]
              },
              {
                "id": "U_19E6",
                "text": "᧦",
                "sk": [
                  {
                    "text": "᧶",
                    "id": "U_19F6"
                  }
                ]
              },
              {
                "id": "U_19E7",
                "text": "᧧",
                "sk": [
                  {
                    "text": "᧷",
                    "id": "U_19F7"
                  }
                ]
              },
              {
                "id": "U_19E8",
                "text": "᧨",
                "sk": [
                  {
                    "text": "᧸",
                    "id": "U_19F8"
                  }
                ]
              },
              {
                "id": "U_19E9",
                "text": "᧩",
                "sk": [
                  {
                    "text": "᧹",
                    "id": "U_19F9"
                  }
                ]
              }
            ]
          },
          {
            "id": 3,
            "key": [
              {
                "id": "K_NUMLOCK",
                "text": "១២៣",
                "sp": "1",
                "nextlayer": "numeric"
              },
              {
                "id": "U_19EA",
                "text": "᧪",
                "sk": [
                  {
                    "text": "᧺",
                    "id": "U_19FA"
                  }
                ]
              },
              {
                "id": "U_19EB",
                "text": "᧫",
                "pad": "",
                "sk": [
                  {
                    "text": "᧻",
                    "id": "U_19FB"
                  }
                ]
              },
              {
                "id": "U_19EC",
                "text": "᧬",
                "sk": [
                  {
                    "text": "᧼",
                    "id": "U_19FC"
                  }
                ]
              },
              {
                "id": "U_19ED",
                "text": "᧭",
                "sk": [
                  {
                    "text": "᧽",
                    "id": "U_19FD"
                  }
                ]
              },
              {
                "id": "U_19EE",
                "text": "᧮",
                "sk": [
                  {
                    "text": "᧾",
                    "id": "U_19FE"
                  }
                ]
              },
              {
                "id": "U_19EF",
                "text": "᧯",
                "sk": [
                  {
                    "text": "᧿",
                    "id": "U_19FF"
                  }
                ]
              },
              {
                "id": "U_17D9",
                "text": "៙"
              },
              {
                "id": "U_17DA",
                "text": "៚"
              },
              {
                "id": "K_BKSP",
                "text": "*BkSp*",
                "width": "125",
                "sp": "1"
              }
            ]
          },
          {
            "id": 4,
            "key": [
              {
                "id": "K_LCONTROL",
                "text": "កខគ",
                "width": "130",
                "sp": "1",
                "nextlayer": "default"
              },
              {
                "id": "K_LOPT",
                "text": "*Menu*",
                "width": "140",
                "sp": "1"
              },
              {
                "id": "K_SPACE",
                "text": "​",
                "width": "550",
                "sp": "0",
                "layer": "shift",
                "sk": []
              },
              {
                "id": "K_PERIOD",
                "text": "។",
                "sk": [
                  {
                    "text": "៕",
                    "id": "K_PERIOD",
                    "layer": "shift"
                  },
                  {
                    "text": "!",
                    "id": "U_0021"
                  },
                  {
                    "text": "?",
                    "id": "U_003F"
                  }
                ]
              },
              {
                "id": "K_ENTER",
                "text": "*Enter*",
                "width": "130",
                "sp": "1"
              }
            ]
          }
        ]
      }
    ],
    "fontsize": ""
  }
}
;
  this.s_c_key="...................................";
  this.s_c_out="កខគឃងចឆជឈញដឋឌឍណតថទធនបផពភមយរលវសហឡអឝឞ";
  this.s_v_gen_key="................";
  this.s_v_gen="ាិីឹឺុូួើឿៀេែៃោៅ";
  this.s_v_pseudo_key="...";
  this.s_v_pseudo="ំះៈ";
  this.s_v_key="...................";
  this.s_v_out="ាិីឹឺុូួើឿៀេែៃោៅំះៈ";
  this.s_v_any="ាិីឹឺុូួើឿៀេែៃោៅំះៈ";
  this.s_v_combo_R="េោុិីឹែ";
  this.s_v_combo_N="ាុ";
  this.s_v_combo="េោុិីឹែាុ";
  this.s_ind_v_key="...............";
  this.s_ind_v_out="ឥឦឧឨឩឪឫឬឭឮឯឰឱឲឳ";
  this.s_diacritic_key="...........";
  this.s_diacritic_out="់័៌៏៍ៈ៎៑៝ៜ្";
  this.s_c_shifter_key="..";
  this.s_c_shifter="៉៊";
  this.s_punct_key="........";
  this.s_punct_out="។៕៖ៗ៘៙៚៓";
  this.s_latin_punct_key="..................................";
  this.s_latin_punct_out="«»()!\"%=?{}\\@*,×./[]‍‌+-÷:≈‘’;<>#&";
  this.s_spaces_key="...";
  this.s_spaces_out="​ ‍";
  this.s_currency_key="...";
  this.s_currency_out="៛$€";
  this.s_digit_key="..........";
  this.s_digit_out="០១២៣៤៥៦៧៨៩";
  this.s_lek_attak_key="..........";
  this.s_lek_attak_out="៰៱៲៳៴៵៶៷៸៹";
  this.s_lunar_date_key="................................";
  this.s_lunar_date_out="᧬᧻᧹᧮᧢᧯᧰᧱᧧᧲᧳᧴᧽᧼᧨᧩᧠᧣᧭᧤᧦᧺᧡᧸᧥᧷᧵᧾᧿᧪᧫᧶";
  this.s_input_subcons="...................................";
  this.s_subcons="កខគឃងចឆជឈញដឋឌឍណតថទធនបផពភមយរលវឝឞសហឡអ";
  this.s_arabic_digit_key="..........";
  this.s_arabic_digit_out="0123456789";
  this.s_v_above="ិីឹឺើ";
  this.s_shiftable_c_1st="សហអ";
  this.s_shiftable_c_2nd="ងញមយរវ";
  this.s_c_2nd_combo_LO="យមងបវ";
  this.s_c_2nd_combo_MO="យលងរ";
  this.s_c_1st_combo_LO="បហអ";
  this.s_c_1st_combo_MO="ហសអ";
  this.s_c_combo_SA="បយលមនញងរវអ";
  this.s_c_combo_QA="ឆឈបផតទ";
  this.s_c_combo_HA="វឣ";
  this.s64="touch";
  this.s65="web";
  this.KVER="10.0.700.0";
  this.gs=function(t,e) {
    return this.g_main(t,e);
  };
  this.g_main=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_BKSP /* 0x08 */)&&k.KIFS(31,this.s64,t)&&k.KCM(2,t,"្",1)&&k.KA(1,k.KC(1,1,t),this.s_c_out)) {   // Line 316
      r=m=1;
      k.KO(2,t,"");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_BKSP /* 0x08 */)&&k.KIFS(31,this.s65,t)&&k.KCM(2,t,"្",1)&&k.KA(1,k.KC(1,1,t),this.s_c_out)) {   // Line 317
      r=m=1;
      k.KO(2,t,"");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_BKSP /* 0x08 */)&&k.KA(0,k.KC(2,1,t),this.s_v_combo_N)&&k.KCM(1,t,"ំ",1)) {   // Line 246
      r=m=1;
      k.KO(2,t,"");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_BKSP /* 0x08 */)&&k.KA(0,k.KC(2,1,t),this.s_v_combo_R)&&k.KCM(1,t,"ះ",1)) {   // Line 247
      r=m=1;
      k.KO(2,t,"");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_K /* 0x4B */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ឝ");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_B /* 0x42 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ឞ");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_QUOTE /* 0xDE */)) {   // Line 210
      r=m=1;
      k.KO(0,t,"ៈ");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_I /* 0x49 */)) {   // Line 211
      r=m=1;
      k.KO(0,t,"ឦ");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_LBRKT /* 0xDB */)) {   // Line 211
      r=m=1;
      k.KO(0,t,"ឩ");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_R /* 0x52 */)) {   // Line 211
      r=m=1;
      k.KO(0,t,"ឫ");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_E /* 0x45 */)) {   // Line 211
      r=m=1;
      k.KO(0,t,"ឯ");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_P /* 0x50 */)) {   // Line 211
      r=m=1;
      k.KO(0,t,"ឰ");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_O /* 0x4F */)) {   // Line 211
      r=m=1;
      k.KO(0,t,"ឱ");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_RBRKT /* 0xDD */)) {   // Line 211
      r=m=1;
      k.KO(0,t,"ឳ");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_T /* 0x54 */)) {   // Line 211
      r=m=1;
      k.KO(0,t,"ឨ");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_EQUAL /* 0xBB */)) {   // Line 212
      r=m=1;
      k.KO(0,t,"៎");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_3 /* 0x33 */)) {   // Line 212
      r=m=1;
      k.KO(0,t,"៑");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_W /* 0x57 */)) {   // Line 212
      r=m=1;
      k.KO(0,t,"៝");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_Q /* 0x51 */)) {   // Line 212
      r=m=1;
      k.KO(0,t,"ៜ");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_QUOTE /* 0xDE */)) {   // Line 212
      r=m=1;
      k.KO(0,t,"ៈ");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_COLON /* 0xBA */)) {   // Line 214
      r=m=1;
      k.KO(0,t,"៖");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_L /* 0x4C */)) {   // Line 214
      r=m=1;
      k.KO(0,t,"៘");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_7 /* 0x37 */)) {   // Line 214
      r=m=1;
      k.KO(0,t,"៚");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_M /* 0x4D */)) {   // Line 214
      r=m=1;
      k.KO(0,t,"៓");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_6 /* 0x36 */)) {   // Line 214
      r=m=1;
      k.KO(0,t,"៙");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_X /* 0x58 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,">");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_BKSLASH /* 0xDC */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"\\");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_S /* 0x53 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"-");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_8 /* 0x38 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"*");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_COMMA /* 0xBC */)) {   // Line 215
      r=m=1;
      k.KO(0,t,",");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_D /* 0x44 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"×");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_A /* 0x41 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"+");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_PERIOD /* 0xBE */)) {   // Line 215
      r=m=1;
      k.KO(0,t,".");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_C /* 0x43 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"#");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_V /* 0x56 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"&");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_SLASH /* 0xBF */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"/");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_Y /* 0x59 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"[");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_U /* 0x55 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"]");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_BKQUOTE /* 0xC0 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"‍");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_1 /* 0x31 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"‌");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_9 /* 0x39 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"{");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_F /* 0x46 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"÷");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_G /* 0x47 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,":");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_HYPHEN /* 0xBD */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"≈");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_H /* 0x48 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"‘");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_J /* 0x4A */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"’");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_N /* 0x4E */)) {   // Line 215
      r=m=1;
      k.KO(0,t,";");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_0 /* 0x30 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"}");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_Z /* 0x5A */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"<");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_2 /* 0x32 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"@");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_4 /* 0x34 */)) {   // Line 216
      r=m=1;
      k.KO(0,t,"$");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_5 /* 0x35 */)) {   // Line 216
      r=m=1;
      k.KO(0,t,"€");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_2 /* 0x32 */)) {   // Line 218
      r=m=1;
      k.KO(0,t,"៲");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_1 /* 0x31 */)) {   // Line 218
      r=m=1;
      k.KO(0,t,"៱");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_0 /* 0x30 */)) {   // Line 218
      r=m=1;
      k.KO(0,t,"៰");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_3 /* 0x33 */)) {   // Line 218
      r=m=1;
      k.KO(0,t,"៳");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_6 /* 0x36 */)) {   // Line 218
      r=m=1;
      k.KO(0,t,"៶");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_7 /* 0x37 */)) {   // Line 218
      r=m=1;
      k.KO(0,t,"៷");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_8 /* 0x38 */)) {   // Line 218
      r=m=1;
      k.KO(0,t,"៸");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_9 /* 0x39 */)) {   // Line 218
      r=m=1;
      k.KO(0,t,"៹");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_5 /* 0x35 */)) {   // Line 218
      r=m=1;
      k.KO(0,t,"៵");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_4 /* 0x34 */)) {   // Line 218
      r=m=1;
      k.KO(0,t,"៴");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_E /* 0x45 */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧢");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_F /* 0x46 */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧯");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_A /* 0x41 */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧬");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_D /* 0x44 */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧮");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_C /* 0x43 */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧹");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_QUOTE /* 0xDE */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧶");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_G /* 0x47 */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧰");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_RBRKT /* 0xDD */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧫");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_LBRKT /* 0xDB */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧪");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_H /* 0x48 */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧱");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_PERIOD /* 0xBE */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧿");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_I /* 0x49 */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧧");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_J /* 0x4A */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧲");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_K /* 0x4B */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧳");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_L /* 0x4C */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧴");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_COMMA /* 0xBC */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧾");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_M /* 0x4D */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧽");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_N /* 0x4E */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧼");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_COLON /* 0xBA */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧵");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_Z /* 0x5A */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧷");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_Y /* 0x59 */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧥");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_X /* 0x58 */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧸");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_O /* 0x4F */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧨");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_W /* 0x57 */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧡");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_V /* 0x56 */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧺");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_U /* 0x55 */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧦");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_P /* 0x50 */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧩");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_Q /* 0x51 */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧠");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_R /* 0x52 */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧣");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_S /* 0x53 */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧭");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_T /* 0x54 */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧤");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_B /* 0x42 */)) {   // Line 219
      r=m=1;
      k.KO(0,t,"᧻");
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_SPACE /* 0x20 */)) {   // Line 220
      r=m=1;
      k.KO(0,t,"‍");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x10C)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ឌ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x10D)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ឍ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x10E)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ណ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x10F)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ត");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x10A)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ដ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x109)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ញ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x108)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ឈ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x107)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ជ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x106)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ឆ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x105)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ច");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x122)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្អ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x104)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ង");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x103)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ឃ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x102)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្គ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x111)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ទ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x101)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ខ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x100)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ក");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x112)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ធ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x113)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ន");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x114)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ប");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x115)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ផ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x110)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ថ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x10B)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ឋ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x116)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ព");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x121)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ឡ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x117)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ភ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x120)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ហ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x11F)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ស");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x11E)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ឞ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x11D)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ឝ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x118)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ម");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x11C)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្វ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x119)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្យ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x11A)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្រ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x11B)) {   // Line 312
      r=m=1;
      k.KO(0,t,"្ល");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_SPACE /* 0x20 */)) {   // Line 220
      r=m=1;
      k.KO(0,t," ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_SPACE /* 0x20 */)) {   // Line 220
      r=m=1;
      k.KO(0,t,"​");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_1 /* 0x31 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"!");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_QUOTE /* 0xDE */)&&k.KA(0,k.KC(3,1,t),this.s_c_combo_QA)&&k.KCM(2,t,"្អ",2)) {   // Line 261
      r=m=1;
      k.KO(-1,t,"៉");
      k.KB(t);
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_QUOTE /* 0xDE */)&&k.KCM(3,t,"ល្",2)&&k.KA(2,k.KC(1,1,t),this.s_c_1st_combo_LO)) {   // Line 262
      r=m=1;
      k.KO(-1,t,"៉");
      k.KB(t);
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_QUOTE /* 0xDE */)&&k.KCM(3,t,"ម្",2)&&k.KA(2,k.KC(1,1,t),this.s_c_1st_combo_MO)) {   // Line 263
      r=m=1;
      k.KO(-1,t,"៉");
      k.KB(t);
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_QUOTE /* 0xDE */)&&k.KCM(3,t,"ស្",2)&&k.KA(2,k.KC(1,1,t),this.s_c_combo_SA)) {   // Line 264
      r=m=1;
      k.KO(-1,t,"៉");
      k.KB(t);
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_QUOTE /* 0xDE */)&&k.KA(0,k.KC(3,1,t),this.s_c_combo_HA)&&k.KCM(2,t,"្ហ",2)) {   // Line 265
      r=m=1;
      k.KO(-1,t,"៉");
      k.KB(t);
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_QUOTE /* 0xDE */)&&k.KCM(3,t,"អ្ង",3)) {   // Line 266
      r=m=1;
      k.KO(-1,t,"៉");
      k.KB(t);
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_QUOTE /* 0xDE */)&&k.KCM(3,t,"អ្វ",3)) {   // Line 267
      r=m=1;
      k.KO(-1,t,"៉");
      k.KB(t);
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_QUOTE /* 0xDE */)&&k.KA(0,k.KC(1,1,t),this.s_c_shifter)) {   // Line 236
      r=m=1;
      k.KB(t);
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_QUOTE /* 0xDE */)&&k.KA(0,k.KC(1,1,t),this.s_shiftable_c_1st)) {   // Line 256
      r=m=1;
      k.KO(-1,t,"៉");
      k.KB(t);
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_QUOTE /* 0xDE */)) {   // Line 213
      r=m=1;
      k.KO(0,t,"៉");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_3 /* 0x33 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"\"");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_4 /* 0x34 */)) {   // Line 216
      r=m=1;
      k.KO(0,t,"៛");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_5 /* 0x35 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"%");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_7 /* 0x37 */)) {   // Line 212
      r=m=1;
      k.KO(0,t,"័");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_QUOTE /* 0xDE */)&&k.KCM(2,t,"្",1)&&k.KA(1,k.KC(1,1,t),this.s_c_out)) {   // Line 235
      r=m=1;
      k.KB(t);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_QUOTE /* 0xDE */)&&k.KA(0,k.KC(1,1,t),this.s_v_gen)) {   // Line 232
      r=m=1;
      k.KB(t);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_QUOTE /* 0xDE */)&&k.KA(0,k.KC(1,1,t),this.s_v_pseudo)) {   // Line 233
      r=m=1;
      k.KB(t);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_QUOTE /* 0xDE */)&&k.KA(0,k.KC(1,1,t),this.s_c_shifter)) {   // Line 234
      r=m=1;
      k.KB(t);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_QUOTE /* 0xDE */)) {   // Line 212
      r=m=1;
      k.KO(0,t,"់");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_9 /* 0x39 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"(");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_0 /* 0x30 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,")");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_8 /* 0x38 */)) {   // Line 212
      r=m=1;
      k.KO(0,t,"៏");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_EQUAL /* 0xBB */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"=");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_COMMA /* 0xBC */)) {   // Line 227
      r=m=1;
      k.KO(0,t,"ុំ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_HYPHEN /* 0xBD */)) {   // Line 211
      r=m=1;
      k.KO(0,t,"ឥ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_PERIOD /* 0xBE */)) {   // Line 214
      r=m=1;
      k.KO(0,t,"។");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_SLASH /* 0xBF */)&&k.KCM(3,t,"ល្",2)&&k.KA(2,k.KC(1,1,t),this.s_c_2nd_combo_LO)) {   // Line 271
      r=m=1;
      k.KO(-1,t,"៊");
      k.KB(t);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_SLASH /* 0xBF */)&&k.KCM(3,t,"ម្",2)&&k.KA(2,k.KC(1,1,t),this.s_c_2nd_combo_MO)) {   // Line 272
      r=m=1;
      k.KO(-1,t,"៊");
      k.KB(t);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_SLASH /* 0xBF */)&&k.KA(0,k.KC(1,1,t),this.s_c_shifter)) {   // Line 236
      r=m=1;
      k.KB(t);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_SLASH /* 0xBF */)&&k.KA(0,k.KC(1,1,t),this.s_shiftable_c_2nd)) {   // Line 257
      r=m=1;
      k.KO(-1,t,"៊");
      k.KB(t);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_SLASH /* 0xBF */)) {   // Line 213
      r=m=1;
      k.KO(0,t,"៊");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_0 /* 0x30 */)) {   // Line 217
      r=m=1;
      k.KO(0,t,"០");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_1 /* 0x31 */)) {   // Line 217
      r=m=1;
      k.KO(0,t,"១");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_2 /* 0x32 */)) {   // Line 217
      r=m=1;
      k.KO(0,t,"២");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_3 /* 0x33 */)) {   // Line 217
      r=m=1;
      k.KO(0,t,"៣");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_4 /* 0x34 */)) {   // Line 217
      r=m=1;
      k.KO(0,t,"៤");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_5 /* 0x35 */)) {   // Line 217
      r=m=1;
      k.KO(0,t,"៥");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_6 /* 0x36 */)) {   // Line 217
      r=m=1;
      k.KO(0,t,"៦");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_7 /* 0x37 */)) {   // Line 217
      r=m=1;
      k.KO(0,t,"៧");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_8 /* 0x38 */)) {   // Line 217
      r=m=1;
      k.KO(0,t,"៨");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_9 /* 0x39 */)) {   // Line 217
      r=m=1;
      k.KO(0,t,"៩");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_COLON /* 0xBA */)) {   // Line 226
      r=m=1;
      k.KO(0,t,"ោះ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_COLON /* 0xBA */)) {   // Line 210
      r=m=1;
      k.KO(0,t,"ើ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_COMMA /* 0xBC */)) {   // Line 228
      r=m=1;
      k.KO(0,t,"ុះ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_EQUAL /* 0xBB */)) {   // Line 211
      r=m=1;
      k.KO(0,t,"ឲ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_PERIOD /* 0xBE */)) {   // Line 214
      r=m=1;
      k.KO(0,t,"៕");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_SLASH /* 0xBF */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"?");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_2 /* 0x32 */)) {   // Line 214
      r=m=1;
      k.KO(0,t,"ៗ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_A /* 0x41 */)) {   // Line 224
      r=m=1;
      k.KO(0,t,"ាំ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_B /* 0x42 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ព");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_C /* 0x43 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ជ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_D /* 0x44 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ឌ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_E /* 0x45 */)) {   // Line 210
      r=m=1;
      k.KO(0,t,"ែ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_F /* 0x46 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ធ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_G /* 0x47 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"អ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_H /* 0x48 */)&&k.KCM(1,t,"ះ",1)) {   // Line 240
      r=m=1;
      k.KO(1,t,"ៈ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_H /* 0x48 */)&&k.KCM(1,t,"ៈ",1)) {   // Line 241
      r=m=1;
      k.KO(1,t,"ះ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_H /* 0x48 */)) {   // Line 210
      r=m=1;
      k.KO(0,t,"ះ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_I /* 0x49 */)) {   // Line 210
      r=m=1;
      k.KO(0,t,"ី");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_J /* 0x4A */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ញ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_K /* 0x4B */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"គ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_L /* 0x4C */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ឡ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_M /* 0x4D */)) {   // Line 210
      r=m=1;
      k.KO(0,t,"ំ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_N /* 0x4E */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ណ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_O /* 0x4F */)) {   // Line 210
      r=m=1;
      k.KO(0,t,"ៅ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_P /* 0x50 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ភ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_Q /* 0x51 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ឈ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_R /* 0x52 */)) {   // Line 211
      r=m=1;
      k.KO(0,t,"ឬ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_S /* 0x53 */)) {   // Line 210
      r=m=1;
      k.KO(0,t,"ៃ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_T /* 0x54 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ទ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_U /* 0x55 */)) {   // Line 210
      r=m=1;
      k.KO(0,t,"ូ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_V /* 0x56 */)) {   // Line 225
      r=m=1;
      k.KO(0,t,"េះ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_W /* 0x57 */)) {   // Line 210
      r=m=1;
      k.KO(0,t,"ឺ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_X /* 0x58 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ឃ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_Y /* 0x59 */)) {   // Line 210
      r=m=1;
      k.KO(0,t,"ួ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_Z /* 0x5A */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ឍ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_LBRKT /* 0xDB */)) {   // Line 210
      r=m=1;
      k.KO(0,t,"ៀ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_BKSLASH /* 0xDC */)) {   // Line 211
      r=m=1;
      k.KO(0,t,"ឮ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_RBRKT /* 0xDD */)) {   // Line 211
      r=m=1;
      k.KO(0,t,"ឪ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_6 /* 0x36 */)) {   // Line 212
      r=m=1;
      k.KO(0,t,"៍");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_HYPHEN /* 0xBD */)) {   // Line 212
      r=m=1;
      k.KO(0,t,"៌");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_BKQUOTE /* 0xC0 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"«");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_A /* 0x41 */)) {   // Line 210
      r=m=1;
      k.KO(0,t,"ា");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_B /* 0x42 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ប");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_C /* 0x43 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ច");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_D /* 0x44 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ដ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_E /* 0x45 */)) {   // Line 210
      r=m=1;
      k.KO(0,t,"េ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_F /* 0x46 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ថ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_G /* 0x47 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ង");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_H /* 0x48 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ហ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_I /* 0x49 */)) {   // Line 210
      r=m=1;
      k.KO(0,t,"ិ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_J /* 0x4A */)) {   // Line 212
      r=m=1;
      k.KO(0,t,"្");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_K /* 0x4B */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ក");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_L /* 0x4C */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ល");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_M /* 0x4D */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ម");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_N /* 0x4E */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ន");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_O /* 0x4F */)) {   // Line 210
      r=m=1;
      k.KO(0,t,"ោ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_P /* 0x50 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ផ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Q /* 0x51 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ឆ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_R /* 0x52 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"រ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_S /* 0x53 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ស");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_T /* 0x54 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ត");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_U /* 0x55 */)&&k.KA(0,k.KC(3,1,t),this.s_shiftable_c_1st)&&k.KCM(2,t,"ាំ",2)) {   // Line 251
      r=m=1;
      k.KIO(3,this.s_shiftable_c_1st,1,t);
      k.KO(-1,t,"៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_U /* 0x55 */)&&k.KA(0,k.KC(3,1,t),this.s_shiftable_c_2nd)&&k.KCM(2,t,"ាំ",2)) {   // Line 252
      r=m=1;
      k.KIO(3,this.s_shiftable_c_2nd,1,t);
      k.KO(-1,t,"៉");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_U /* 0x55 */)) {   // Line 210
      r=m=1;
      k.KO(0,t,"ុ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_V /* 0x56 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"វ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_W /* 0x57 */)) {   // Line 210
      r=m=1;
      k.KO(0,t,"ឹ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_X /* 0x58 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ខ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Y /* 0x59 */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"យ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Z /* 0x5A */)) {   // Line 209
      r=m=1;
      k.KO(0,t,"ឋ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_LBRKT /* 0xDB */)) {   // Line 210
      r=m=1;
      k.KO(0,t,"ឿ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_BKSLASH /* 0xDC */)) {   // Line 211
      r=m=1;
      k.KO(0,t,"ឭ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_RBRKT /* 0xDD */)) {   // Line 211
      r=m=1;
      k.KO(0,t,"ឧ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_BKQUOTE /* 0xC0 */)) {   // Line 215
      r=m=1;
      k.KO(0,t,"»");
    }
    if(m) {
    
      r=this.g_normalise(t,e);
    }
    return r;
  };
  this.g_normalise=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
    if(k.KA(0,k.KC(7,1,t),this.s_c_combo_QA)&&k.KCM(6,t,"្អុំាំ",6)) {   // Line 422
      m=1;
      k.KIO(7,this.s_c_combo_QA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"អ៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(7,t,"ល្",2)&&k.KA(2,k.KC(5,1,t),this.s_c_1st_combo_LO)&&k.KCM(4,t,"ុំាំ",4)) {   // Line 427
      m=1;
      k.KO(7,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_LO,3,t);
      k.KO(-1,t,"៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(7,t,"ម្",2)&&k.KA(2,k.KC(5,1,t),this.s_c_1st_combo_MO)&&k.KCM(4,t,"ុំាំ",4)) {   // Line 432
      m=1;
      k.KO(7,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_MO,3,t);
      k.KO(-1,t,"៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(7,t,"ស្",2)&&k.KA(2,k.KC(5,1,t),this.s_c_combo_SA)&&k.KCM(4,t,"ុំាំ",4)) {   // Line 437
      m=1;
      k.KO(7,t,"ស");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_combo_SA,3,t);
      k.KO(-1,t,"៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KA(0,k.KC(7,1,t),this.s_c_combo_HA)&&k.KCM(6,t,"្ហុំាំ",6)) {   // Line 442
      m=1;
      k.KIO(7,this.s_c_combo_HA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"ហ៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(7,t,"អ្ងុំាំ",7)) {   // Line 447
      m=1;
      k.KO(7,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"ង៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(7,t,"អ្វុំាំ",7)) {   // Line 452
      m=1;
      k.KO(7,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"វ៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(7,t,"ល្",2)&&k.KA(2,k.KC(5,1,t),this.s_c_2nd_combo_LO)&&k.KCM(4,t,"ុំាំ",4)) {   // Line 459
      m=1;
      k.KO(7,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_LO,3,t);
      k.KO(-1,t,"៉");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(7,t,"ម្",2)&&k.KA(2,k.KC(5,1,t),this.s_c_2nd_combo_MO)&&k.KCM(4,t,"ុំាំ",4)) {   // Line 464
      m=1;
      k.KO(7,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_MO,3,t);
      k.KO(-1,t,"៉");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"្ដ",2)&&k.KA(2,k.KC(4,1,t),this.s_v_combo_N)&&k.KCM(3,t,"ំ្រ",3)) {   // Line 393
      m=1;
      k.KO(6,t,"្ត");
      k.KO(-1,t,"្");
      k.KO(-1,t,"រ");
      k.KIO(-1,this.s_v_combo_N,3,t);
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"្ដ",2)&&k.KA(2,k.KC(4,1,t),this.s_v_combo_R)&&k.KCM(3,t,"ះ្រ",3)) {   // Line 394
      m=1;
      k.KO(6,t,"្ត");
      k.KO(-1,t,"្");
      k.KO(-1,t,"រ");
      k.KIO(-1,this.s_v_combo_R,3,t);
      k.KO(-1,t,"ះ");
    }
    else if(k.KCM(6,t,"្រ",2)&&k.KA(2,k.KC(4,1,t),this.s_v_combo_N)&&k.KCM(3,t,"ំ្ដ",3)) {   // Line 397
      m=1;
      k.KO(6,t,"្ត");
      k.KO(-1,t,"្");
      k.KO(-1,t,"រ");
      k.KIO(-1,this.s_v_combo_N,3,t);
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"្រ",2)&&k.KA(2,k.KC(4,1,t),this.s_v_combo_R)&&k.KCM(3,t,"ះ្ដ",3)) {   // Line 398
      m=1;
      k.KO(6,t,"្ត");
      k.KO(-1,t,"្");
      k.KO(-1,t,"រ");
      k.KIO(-1,this.s_v_combo_R,3,t);
      k.KO(-1,t,"ះ");
    }
    else if(k.KCM(6,t,"្រ",2)&&k.KA(2,k.KC(4,1,t),this.s_v_combo_N)&&k.KCM(3,t,"ំ្",2)&&k.KA(5,k.KC(1,1,t),this.s_subcons)) {   // Line 403
      m=1;
      k.KO(6,t,"្");
      k.KIO(-1,this.s_subcons,6,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"រ");
      k.KIO(-1,this.s_v_combo_N,3,t);
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"្រ",2)&&k.KA(2,k.KC(4,1,t),this.s_v_combo_R)&&k.KCM(3,t,"ះ្",2)&&k.KA(5,k.KC(1,1,t),this.s_subcons)) {   // Line 404
      m=1;
      k.KO(6,t,"្");
      k.KIO(-1,this.s_subcons,6,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"រ");
      k.KIO(-1,this.s_v_combo_R,3,t);
      k.KO(-1,t,"ះ");
    }
    else if(k.KA(0,k.KC(6,1,t),this.s_c_combo_QA)&&k.KCM(5,t,"្អុាំ",5)) {   // Line 420
      m=1;
      k.KIO(6,this.s_c_combo_QA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"អ៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"ល្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_1st_combo_LO)&&k.KCM(3,t,"ុាំ",3)) {   // Line 425
      m=1;
      k.KO(6,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_LO,3,t);
      k.KO(-1,t,"៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"ម្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_1st_combo_MO)&&k.KCM(3,t,"ុាំ",3)) {   // Line 430
      m=1;
      k.KO(6,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_MO,3,t);
      k.KO(-1,t,"៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"ស្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_combo_SA)&&k.KCM(3,t,"ុាំ",3)) {   // Line 435
      m=1;
      k.KO(6,t,"ស");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_combo_SA,3,t);
      k.KO(-1,t,"៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KA(0,k.KC(6,1,t),this.s_c_combo_HA)&&k.KCM(5,t,"្ហុាំ",5)) {   // Line 440
      m=1;
      k.KIO(6,this.s_c_combo_HA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"ហ៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"អ្ងុាំ",6)) {   // Line 445
      m=1;
      k.KO(6,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"ង៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"អ្វុាំ",6)) {   // Line 450
      m=1;
      k.KO(6,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"វ៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"ល្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_2nd_combo_LO)&&k.KCM(3,t,"ុាំ",3)) {   // Line 457
      m=1;
      k.KO(6,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_LO,3,t);
      k.KO(-1,t,"៉");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"ម្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_2nd_combo_MO)&&k.KCM(3,t,"ុាំ",3)) {   // Line 462
      m=1;
      k.KO(6,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_MO,3,t);
      k.KO(-1,t,"៉");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"ល្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_2nd_combo_LO)&&k.KCM(3,t,"៊",1)&&k.KA(4,k.KC(2,1,t),this.s_v_gen)&&k.KA(5,k.KC(1,1,t),this.s_v_pseudo)) {   // Line 484
      m=1;
      k.KO(6,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_LO,3,t);
      k.KO(-1,t,"៉");
      k.KIO(-1,this.s_v_gen,5,t);
      k.KIO(-1,this.s_v_pseudo,6,t);
    }
    else if(k.KCM(6,t,"ម្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_2nd_combo_MO)&&k.KCM(3,t,"៊",1)&&k.KA(4,k.KC(2,1,t),this.s_v_gen)&&k.KA(5,k.KC(1,1,t),this.s_v_pseudo)) {   // Line 485
      m=1;
      k.KO(6,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_MO,3,t);
      k.KO(-1,t,"៉");
      k.KIO(-1,this.s_v_gen,5,t);
      k.KIO(-1,this.s_v_pseudo,6,t);
    }
    else if(k.KA(0,k.KC(6,1,t),this.s_c_combo_QA)&&k.KCM(5,t,"្អ៉ាំ",5)) {   // Line 518
      m=1;
      k.KIO(6,this.s_c_combo_QA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"អ៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"ល្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_1st_combo_LO)&&k.KCM(3,t,"៉ាំ",3)) {   // Line 519
      m=1;
      k.KO(6,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_LO,3,t);
      k.KO(-1,t,"៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"ម្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_1st_combo_MO)&&k.KCM(3,t,"៉ាំ",3)) {   // Line 520
      m=1;
      k.KO(6,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_MO,3,t);
      k.KO(-1,t,"៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"ស្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_combo_SA)&&k.KCM(3,t,"៉ាំ",3)) {   // Line 521
      m=1;
      k.KO(6,t,"ស");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_combo_SA,3,t);
      k.KO(-1,t,"៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KA(0,k.KC(6,1,t),this.s_c_combo_HA)&&k.KCM(5,t,"្ហ៉ាំ",5)) {   // Line 522
      m=1;
      k.KIO(6,this.s_c_combo_HA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"ហ៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"អ្ង៉ាំ",6)) {   // Line 523
      m=1;
      k.KO(6,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"ង៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"អ្វ៉ាំ",6)) {   // Line 524
      m=1;
      k.KO(6,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"វ៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KA(0,k.KC(6,1,t),this.s_c_combo_QA)&&k.KCM(5,t,"្អាុំ",5)) {   // Line 531
      m=1;
      k.KIO(6,this.s_c_combo_QA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"អ៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KA(0,k.KC(6,1,t),this.s_c_combo_QA)&&k.KCM(5,t,"្អុំា",5)) {   // Line 532
      m=1;
      k.KIO(6,this.s_c_combo_QA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"អ៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"ល្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_1st_combo_LO)&&k.KCM(3,t,"ាុំ",3)) {   // Line 534
      m=1;
      k.KO(6,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_LO,3,t);
      k.KO(-1,t,"៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"ល្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_1st_combo_LO)&&k.KCM(3,t,"ុំា",3)) {   // Line 535
      m=1;
      k.KO(6,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_LO,3,t);
      k.KO(-1,t,"៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"ម្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_1st_combo_MO)&&k.KCM(3,t,"ាុំ",3)) {   // Line 537
      m=1;
      k.KO(6,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_MO,3,t);
      k.KO(-1,t,"៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"ម្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_1st_combo_MO)&&k.KCM(3,t,"ុំា",3)) {   // Line 538
      m=1;
      k.KO(6,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_MO,3,t);
      k.KO(-1,t,"៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"ស្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_combo_SA)&&k.KCM(3,t,"ាុំ",3)) {   // Line 540
      m=1;
      k.KO(6,t,"ស");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_combo_SA,3,t);
      k.KO(-1,t,"៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"ស្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_combo_SA)&&k.KCM(3,t,"ុំា",3)) {   // Line 541
      m=1;
      k.KO(6,t,"ស");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_combo_SA,3,t);
      k.KO(-1,t,"៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KA(0,k.KC(6,1,t),this.s_c_combo_HA)&&k.KCM(5,t,"្ហាុំ",5)) {   // Line 543
      m=1;
      k.KIO(6,this.s_c_combo_HA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"ហ៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KA(0,k.KC(6,1,t),this.s_c_combo_HA)&&k.KCM(5,t,"្ហុំា",5)) {   // Line 544
      m=1;
      k.KIO(6,this.s_c_combo_HA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"ហ៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"អ្ងាុំ",6)) {   // Line 546
      m=1;
      k.KO(6,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"ង៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"អ្ងុំា",6)) {   // Line 547
      m=1;
      k.KO(6,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"ង៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"អ្វាុំ",6)) {   // Line 549
      m=1;
      k.KO(6,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"វ៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"អ្វុំា",6)) {   // Line 550
      m=1;
      k.KO(6,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"វ៊");
      k.KO(-1,t,"ុ");
      k.KO(-1,t,"ា");
    }
    else if(k.KCM(6,t,"ល្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_2nd_combo_LO)&&k.KCM(3,t,"ាុំ",3)) {   // Line 557
      m=1;
      k.KO(6,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_LO,3,t);
      k.KO(-1,t,"៉");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"ល្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_2nd_combo_LO)&&k.KCM(3,t,"ុំា",3)) {   // Line 558
      m=1;
      k.KO(6,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_LO,3,t);
      k.KO(-1,t,"៉");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"ម្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_2nd_combo_MO)&&k.KCM(3,t,"ាុំ",3)) {   // Line 560
      m=1;
      k.KO(6,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_MO,3,t);
      k.KO(-1,t,"៉");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(6,t,"ម្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_2nd_combo_MO)&&k.KCM(3,t,"ុំា",3)) {   // Line 561
      m=1;
      k.KO(6,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_MO,3,t);
      k.KO(-1,t,"៉");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KA(0,k.KC(6,1,t),this.s_c_combo_QA)&&k.KCM(5,t,"្អេុី",5)) {   // Line 569
      m=1;
      k.KIO(6,this.s_c_combo_QA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"អ៊ើ");
    }
    else if(k.KA(0,k.KC(6,1,t),this.s_c_combo_QA)&&k.KCM(5,t,"្អុេី",5)) {   // Line 570
      m=1;
      k.KIO(6,this.s_c_combo_QA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"អ៊ើ");
    }
    else if(k.KA(0,k.KC(6,1,t),this.s_c_combo_QA)&&k.KCM(5,t,"្អ៉េី",5)) {   // Line 571
      m=1;
      k.KIO(6,this.s_c_combo_QA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"អ៊ើ");
    }
    else if(k.KCM(6,t,"ល្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_1st_combo_LO)&&k.KCM(3,t,"េុី",3)) {   // Line 573
      m=1;
      k.KO(6,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_LO,3,t);
      k.KO(-1,t,"៊ើ");
    }
    else if(k.KCM(6,t,"ល្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_1st_combo_LO)&&k.KCM(3,t,"ុេី",3)) {   // Line 574
      m=1;
      k.KO(6,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_LO,3,t);
      k.KO(-1,t,"៊ើ");
    }
    else if(k.KCM(6,t,"ល្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_1st_combo_LO)&&k.KCM(3,t,"៉េី",3)) {   // Line 575
      m=1;
      k.KO(6,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_LO,3,t);
      k.KO(-1,t,"៊ើ");
    }
    else if(k.KCM(6,t,"ម្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_1st_combo_MO)&&k.KCM(3,t,"េុី",3)) {   // Line 577
      m=1;
      k.KO(6,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_MO,3,t);
      k.KO(-1,t,"៊ើ");
    }
    else if(k.KCM(6,t,"ម្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_1st_combo_MO)&&k.KCM(3,t,"ុេី",3)) {   // Line 578
      m=1;
      k.KO(6,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_MO,3,t);
      k.KO(-1,t,"៊ើ");
    }
    else if(k.KCM(6,t,"ម្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_1st_combo_MO)&&k.KCM(3,t,"៉េី",3)) {   // Line 579
      m=1;
      k.KO(6,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_MO,3,t);
      k.KO(-1,t,"៊ើ");
    }
    else if(k.KCM(6,t,"ស្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_combo_SA)&&k.KCM(3,t,"េុី",3)) {   // Line 581
      m=1;
      k.KO(6,t,"ស");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_combo_SA,3,t);
      k.KO(-1,t,"៊ើ");
    }
    else if(k.KCM(6,t,"ស្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_combo_SA)&&k.KCM(3,t,"ុេី",3)) {   // Line 582
      m=1;
      k.KO(6,t,"ស");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_combo_SA,3,t);
      k.KO(-1,t,"៊ើ");
    }
    else if(k.KCM(6,t,"ស្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_combo_SA)&&k.KCM(3,t,"៉េី",3)) {   // Line 583
      m=1;
      k.KO(6,t,"ស");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_combo_SA,3,t);
      k.KO(-1,t,"៊ើ");
    }
    else if(k.KA(0,k.KC(6,1,t),this.s_c_combo_HA)&&k.KCM(5,t,"្ហេុី",5)) {   // Line 585
      m=1;
      k.KIO(6,this.s_c_combo_HA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"ហ៊ើ");
    }
    else if(k.KA(0,k.KC(6,1,t),this.s_c_combo_HA)&&k.KCM(5,t,"្ហុេី",5)) {   // Line 586
      m=1;
      k.KIO(6,this.s_c_combo_HA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"ហ៊ើ");
    }
    else if(k.KA(0,k.KC(6,1,t),this.s_c_combo_HA)&&k.KCM(5,t,"្ហ៉េី",5)) {   // Line 587
      m=1;
      k.KIO(6,this.s_c_combo_HA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"ហ៊ើ");
    }
    else if(k.KCM(6,t,"អ្ងេុី",6)) {   // Line 589
      m=1;
      k.KO(6,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"ង៊ើ");
    }
    else if(k.KCM(6,t,"អ្ងុេី",6)) {   // Line 590
      m=1;
      k.KO(6,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"ង៊ើ");
    }
    else if(k.KCM(6,t,"អ្ង៉េី",6)) {   // Line 591
      m=1;
      k.KO(6,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"ង៊ើ");
    }
    else if(k.KCM(6,t,"អ្វេុី",6)) {   // Line 593
      m=1;
      k.KO(6,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"វ៊ើ");
    }
    else if(k.KCM(6,t,"អ្វុេី",6)) {   // Line 594
      m=1;
      k.KO(6,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"វ៊ើ");
    }
    else if(k.KCM(6,t,"អ្វ៉េី",6)) {   // Line 595
      m=1;
      k.KO(6,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"វ៊ើ");
    }
    else if(k.KCM(6,t,"ល្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_2nd_combo_LO)&&k.KCM(3,t,"េុី",3)) {   // Line 603
      m=1;
      k.KO(6,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_LO,3,t);
      k.KO(-1,t,"៉ើ");
    }
    else if(k.KCM(6,t,"ល្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_2nd_combo_LO)&&k.KCM(3,t,"ុេី",3)) {   // Line 604
      m=1;
      k.KO(6,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_LO,3,t);
      k.KO(-1,t,"៉ើ");
    }
    else if(k.KCM(6,t,"ល្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_2nd_combo_LO)&&k.KCM(3,t,"៊េី",3)) {   // Line 605
      m=1;
      k.KO(6,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_LO,3,t);
      k.KO(-1,t,"៉ើ");
    }
    else if(k.KCM(6,t,"ម្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_2nd_combo_MO)&&k.KCM(3,t,"េុី",3)) {   // Line 607
      m=1;
      k.KO(6,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_MO,3,t);
      k.KO(-1,t,"៉ើ");
    }
    else if(k.KCM(6,t,"ម្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_2nd_combo_MO)&&k.KCM(3,t,"ុេី",3)) {   // Line 608
      m=1;
      k.KO(6,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_MO,3,t);
      k.KO(-1,t,"៉ើ");
    }
    else if(k.KCM(6,t,"ម្",2)&&k.KA(2,k.KC(4,1,t),this.s_c_2nd_combo_MO)&&k.KCM(3,t,"៊េី",3)) {   // Line 609
      m=1;
      k.KO(6,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_MO,3,t);
      k.KO(-1,t,"៉ើ");
    }
    else if(k.KA(0,k.KC(5,1,t),this.s_c_shifter)&&k.KA(1,k.KC(4,1,t),this.s_v_combo_N)&&k.KCM(3,t,"ំ្",2)&&k.KA(4,k.KC(1,1,t),this.s_subcons)) {   // Line 384
      m=1;
      k.KO(5,t,"្");
      k.KIO(-1,this.s_subcons,5,t);
      k.KIO(-1,this.s_c_shifter,1,t);
      k.KIO(-1,this.s_v_combo_N,2,t);
      k.KO(-1,t,"ំ");
    }
    else if(k.KA(0,k.KC(5,1,t),this.s_c_shifter)&&k.KA(1,k.KC(4,1,t),this.s_v_combo_R)&&k.KCM(3,t,"ះ្",2)&&k.KA(4,k.KC(1,1,t),this.s_subcons)) {   // Line 385
      m=1;
      k.KO(5,t,"្");
      k.KIO(-1,this.s_subcons,5,t);
      k.KIO(-1,this.s_c_shifter,1,t);
      k.KIO(-1,this.s_v_combo_R,2,t);
      k.KO(-1,t,"ះ");
    }
    else if(k.KCM(5,t,"្ដ",2)&&k.KA(2,k.KC(3,1,t),this.s_v_any)&&k.KCM(2,t,"្រ",2)) {   // Line 392
      m=1;
      k.KO(5,t,"្ត");
      k.KO(-1,t,"្");
      k.KO(-1,t,"រ");
      k.KIO(-1,this.s_v_any,3,t);
    }
    else if(k.KCM(5,t,"្រ",2)&&k.KA(2,k.KC(3,1,t),this.s_v_any)&&k.KCM(2,t,"្ដ",2)) {   // Line 396
      m=1;
      k.KO(5,t,"្ត");
      k.KO(-1,t,"្");
      k.KO(-1,t,"រ");
      k.KIO(-1,this.s_v_any,3,t);
    }
    else if(k.KCM(5,t,"្រ",2)&&k.KA(2,k.KC(3,1,t),this.s_c_shifter)&&k.KCM(2,t,"្",1)&&k.KA(4,k.KC(1,1,t),this.s_subcons)) {   // Line 400
      m=1;
      k.KO(5,t,"្");
      k.KIO(-1,this.s_subcons,5,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"រ");
      k.KIO(-1,this.s_c_shifter,3,t);
    }
    else if(k.KCM(5,t,"្រ",2)&&k.KA(2,k.KC(3,1,t),this.s_v_any)&&k.KCM(2,t,"្",1)&&k.KA(4,k.KC(1,1,t),this.s_subcons)) {   // Line 402
      m=1;
      k.KO(5,t,"្");
      k.KIO(-1,this.s_subcons,5,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"រ");
      k.KIO(-1,this.s_v_any,3,t);
    }
    else if(k.KA(0,k.KC(5,1,t),this.s_c_combo_QA)&&k.KCM(4,t,"្អុ",3)&&k.KA(4,k.KC(1,1,t),this.s_v_above)) {   // Line 419
      m=1;
      k.KIO(5,this.s_c_combo_QA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"អ៊");
      k.KIO(-1,this.s_v_above,5,t);
    }
    else if(k.KA(0,k.KC(5,1,t),this.s_c_combo_QA)&&k.KCM(4,t,"្អ",2)&&k.KA(3,k.KC(2,1,t),this.s_v_above)&&k.KCM(1,t,"ុ",1)) {   // Line 421
      m=1;
      k.KIO(5,this.s_c_combo_QA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"អ៊");
      k.KIO(-1,this.s_v_above,4,t);
    }
    else if(k.KCM(5,t,"ល្",2)&&k.KA(2,k.KC(3,1,t),this.s_c_1st_combo_LO)&&k.KCM(2,t,"ុ",1)&&k.KA(4,k.KC(1,1,t),this.s_v_above)) {   // Line 424
      m=1;
      k.KO(5,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_LO,3,t);
      k.KO(-1,t,"៊");
      k.KIO(-1,this.s_v_above,5,t);
    }
    else if(k.KCM(5,t,"ល្",2)&&k.KA(2,k.KC(3,1,t),this.s_c_1st_combo_LO)&&k.KA(3,k.KC(2,1,t),this.s_v_above)&&k.KCM(1,t,"ុ",1)) {   // Line 426
      m=1;
      k.KO(5,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_LO,3,t);
      k.KO(-1,t,"៊");
      k.KIO(-1,this.s_v_above,4,t);
    }
    else if(k.KCM(5,t,"ម្",2)&&k.KA(2,k.KC(3,1,t),this.s_c_1st_combo_MO)&&k.KCM(2,t,"ុ",1)&&k.KA(4,k.KC(1,1,t),this.s_v_above)) {   // Line 429
      m=1;
      k.KO(5,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_MO,3,t);
      k.KO(-1,t,"៊");
      k.KIO(-1,this.s_v_above,5,t);
    }
    else if(k.KCM(5,t,"ម្",2)&&k.KA(2,k.KC(3,1,t),this.s_c_1st_combo_MO)&&k.KA(3,k.KC(2,1,t),this.s_v_above)&&k.KCM(1,t,"ុ",1)) {   // Line 431
      m=1;
      k.KO(5,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_MO,3,t);
      k.KO(-1,t,"៊");
      k.KIO(-1,this.s_v_above,4,t);
    }
    else if(k.KCM(5,t,"ស្",2)&&k.KA(2,k.KC(3,1,t),this.s_c_combo_SA)&&k.KCM(2,t,"ុ",1)&&k.KA(4,k.KC(1,1,t),this.s_v_above)) {   // Line 434
      m=1;
      k.KO(5,t,"ស");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_combo_SA,3,t);
      k.KO(-1,t,"៊");
      k.KIO(-1,this.s_v_above,5,t);
    }
    else if(k.KCM(5,t,"ស្",2)&&k.KA(2,k.KC(3,1,t),this.s_c_combo_SA)&&k.KA(3,k.KC(2,1,t),this.s_v_above)&&k.KCM(1,t,"ុ",1)) {   // Line 436
      m=1;
      k.KO(5,t,"ស");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_combo_SA,3,t);
      k.KO(-1,t,"៊");
      k.KIO(-1,this.s_v_above,4,t);
    }
    else if(k.KA(0,k.KC(5,1,t),this.s_c_combo_HA)&&k.KCM(4,t,"្ហុ",3)&&k.KA(4,k.KC(1,1,t),this.s_v_above)) {   // Line 439
      m=1;
      k.KIO(5,this.s_c_combo_HA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"ហ៊");
      k.KIO(-1,this.s_v_above,5,t);
    }
    else if(k.KA(0,k.KC(5,1,t),this.s_c_combo_HA)&&k.KCM(4,t,"្ហ",2)&&k.KA(3,k.KC(2,1,t),this.s_v_above)&&k.KCM(1,t,"ុ",1)) {   // Line 441
      m=1;
      k.KIO(5,this.s_c_combo_HA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"ហ៊");
      k.KIO(-1,this.s_v_above,4,t);
    }
    else if(k.KCM(5,t,"អ្ងុ",4)&&k.KA(4,k.KC(1,1,t),this.s_v_above)) {   // Line 444
      m=1;
      k.KO(5,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"ង៊");
      k.KIO(-1,this.s_v_above,5,t);
    }
    else if(k.KCM(5,t,"អ្ង",3)&&k.KA(3,k.KC(2,1,t),this.s_v_above)&&k.KCM(1,t,"ុ",1)) {   // Line 446
      m=1;
      k.KO(5,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"ង៊");
      k.KIO(-1,this.s_v_above,4,t);
    }
    else if(k.KCM(5,t,"អ្វុ",4)&&k.KA(4,k.KC(1,1,t),this.s_v_above)) {   // Line 449
      m=1;
      k.KO(5,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"វ៊");
      k.KIO(-1,this.s_v_above,5,t);
    }
    else if(k.KCM(5,t,"អ្វ",3)&&k.KA(3,k.KC(2,1,t),this.s_v_above)&&k.KCM(1,t,"ុ",1)) {   // Line 451
      m=1;
      k.KO(5,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"វ៊");
      k.KIO(-1,this.s_v_above,4,t);
    }
    else if(k.KCM(5,t,"ល្",2)&&k.KA(2,k.KC(3,1,t),this.s_c_2nd_combo_LO)&&k.KCM(2,t,"ុ",1)&&k.KA(4,k.KC(1,1,t),this.s_v_above)) {   // Line 456
      m=1;
      k.KO(5,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_LO,3,t);
      k.KO(-1,t,"៉");
      k.KIO(-1,this.s_v_above,5,t);
    }
    else if(k.KCM(5,t,"ល្",2)&&k.KA(2,k.KC(3,1,t),this.s_c_2nd_combo_LO)&&k.KA(3,k.KC(2,1,t),this.s_v_above)&&k.KCM(1,t,"ុ",1)) {   // Line 458
      m=1;
      k.KO(5,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_LO,3,t);
      k.KO(-1,t,"៉");
      k.KIO(-1,this.s_v_above,4,t);
    }
    else if(k.KCM(5,t,"ម្",2)&&k.KA(2,k.KC(3,1,t),this.s_c_2nd_combo_MO)&&k.KCM(2,t,"ុ",1)&&k.KA(4,k.KC(1,1,t),this.s_v_above)) {   // Line 461
      m=1;
      k.KO(5,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_MO,3,t);
      k.KO(-1,t,"៉");
      k.KIO(-1,this.s_v_above,5,t);
    }
    else if(k.KCM(5,t,"ម្",2)&&k.KA(2,k.KC(3,1,t),this.s_c_2nd_combo_MO)&&k.KA(3,k.KC(2,1,t),this.s_v_above)&&k.KCM(1,t,"ុ",1)) {   // Line 463
      m=1;
      k.KO(5,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_MO,3,t);
      k.KO(-1,t,"៉");
      k.KIO(-1,this.s_v_above,4,t);
    }
    else if(k.KA(0,k.KC(5,1,t),this.s_shiftable_c_1st)&&k.KCM(4,t,"ុំាំ",4)) {   // Line 471
      m=1;
      k.KIO(5,this.s_shiftable_c_1st,1,t);
      k.KO(-1,t,"៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KA(0,k.KC(5,1,t),this.s_shiftable_c_2nd)&&k.KCM(4,t,"ុំាំ",4)) {   // Line 478
      m=1;
      k.KIO(5,this.s_shiftable_c_2nd,1,t);
      k.KO(-1,t,"៉");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KCM(5,t,"ល្",2)&&k.KA(2,k.KC(3,1,t),this.s_c_2nd_combo_LO)&&k.KCM(2,t,"៊",1)&&k.KA(4,k.KC(1,1,t),this.s_v_above)) {   // Line 482
      m=1;
      k.KO(5,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_LO,3,t);
      k.KO(-1,t,"៉");
      k.KIO(-1,this.s_v_above,5,t);
    }
    else if(k.KCM(5,t,"ម្",2)&&k.KA(2,k.KC(3,1,t),this.s_c_2nd_combo_MO)&&k.KCM(2,t,"៊",1)&&k.KA(4,k.KC(1,1,t),this.s_v_above)) {   // Line 483
      m=1;
      k.KO(5,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_2nd_combo_MO,3,t);
      k.KO(-1,t,"៉");
      k.KIO(-1,this.s_v_above,5,t);
    }
    else if(k.KCM(5,t,"្",1)&&k.KA(1,k.KC(4,1,t),this.s_shiftable_c_2nd)&&k.KCM(3,t,"៊",1)&&k.KA(3,k.KC(2,1,t),this.s_v_gen)&&k.KA(4,k.KC(1,1,t),this.s_v_pseudo)) {   // Line 493
      m=1;
    }
    else if(k.KA(0,k.KC(5,1,t),this.s_c_combo_QA)&&k.KCM(4,t,"្អ៉",3)&&k.KA(4,k.KC(1,1,t),this.s_v_above)) {   // Line 511
      m=1;
      k.KIO(5,this.s_c_combo_QA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"អ៊");
      k.KIO(-1,this.s_v_above,5,t);
    }
    else if(k.KCM(5,t,"ល្",2)&&k.KA(2,k.KC(3,1,t),this.s_c_1st_combo_LO)&&k.KCM(2,t,"៉",1)&&k.KA(4,k.KC(1,1,t),this.s_v_above)) {   // Line 512
      m=1;
      k.KO(5,t,"ល");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_LO,3,t);
      k.KO(-1,t,"៊");
      k.KIO(-1,this.s_v_above,5,t);
    }
    else if(k.KCM(5,t,"ម្",2)&&k.KA(2,k.KC(3,1,t),this.s_c_1st_combo_MO)&&k.KCM(2,t,"៉",1)&&k.KA(4,k.KC(1,1,t),this.s_v_above)) {   // Line 513
      m=1;
      k.KO(5,t,"ម");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_1st_combo_MO,3,t);
      k.KO(-1,t,"៊");
      k.KIO(-1,this.s_v_above,5,t);
    }
    else if(k.KCM(5,t,"ស្",2)&&k.KA(2,k.KC(3,1,t),this.s_c_combo_SA)&&k.KCM(2,t,"៉",1)&&k.KA(4,k.KC(1,1,t),this.s_v_above)) {   // Line 514
      m=1;
      k.KO(5,t,"ស");
      k.KO(-1,t,"្");
      k.KIO(-1,this.s_c_combo_SA,3,t);
      k.KO(-1,t,"៊");
      k.KIO(-1,this.s_v_above,5,t);
    }
    else if(k.KA(0,k.KC(5,1,t),this.s_c_combo_HA)&&k.KCM(4,t,"្ហ៉",3)&&k.KA(4,k.KC(1,1,t),this.s_v_above)) {   // Line 515
      m=1;
      k.KIO(5,this.s_c_combo_HA,1,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"ហ៊");
      k.KIO(-1,this.s_v_above,5,t);
    }
    else if(k.KCM(5,t,"អ្ង៉",4)&&k.KA(4,k.KC(1,1,t),this.s_v_above)) {   // Line 516
      m=1;
      k.KO(5,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"ង៊");
      k.KIO(-1,this.s_v_above,5,t);
    }
    else if(k.KCM(5,t,"អ្វ៉",4)&&k.KA(4,k.KC(1,1,t),this.s_v_above)) {   // Line 517
      m=1;
      k.KO(5,t,"អ");
      k.KO(-1,t,"្");
      k.KO(-1,t,"វ៊");
      k.KIO(-1,this.s_v_above,5,t);
    }
    else if(k.KA(0,k.KC(4,1,t),this.s_c_shifter)&&k.KA(1,k.KC(3,1,t),this.s_v_gen)&&k.KA(2,k.KC(2,1,t),this.s_v_pseudo)&&k.KA(3,k.KC(1,1,t),this.s_c_shifter)) {   // Line 353
      m=1;
      k.KIO(4,this.s_v_gen,2,t);
      k.KIO(-1,this.s_v_pseudo,3,t);
      k.KIO(-1,this.s_c_shifter,4,t);
    }
    else if(k.KA(0,k.KC(4,1,t),this.s_v_gen)&&k.KA(1,k.KC(3,1,t),this.s_v_pseudo)&&k.KA(2,k.KC(2,1,t),this.s_v_gen)&&k.KA(3,k.KC(1,1,t),this.s_v_pseudo)) {   // Line 365
      m=1;
      k.KIO(4,this.s_v_gen,3,t);
      k.KIO(-1,this.s_v_pseudo,4,t);
    }
    else if(k.KA(0,k.KC(4,1,t),this.s_v_combo_N)&&k.KCM(3,t,"ំ្",2)&&k.KA(3,k.KC(1,1,t),this.s_subcons)) {   // Line 378
      m=1;
      k.KO(4,t,"្");
      k.KIO(-1,this.s_subcons,4,t);
      k.KIO(-1,this.s_v_combo_N,1,t);
      k.KO(-1,t,"ំ");
    }
    else if(k.KA(0,k.KC(4,1,t),this.s_v_combo_R)&&k.KCM(3,t,"ះ្",2)&&k.KA(3,k.KC(1,1,t),this.s_subcons)) {   // Line 379
      m=1;
      k.KO(4,t,"្");
      k.KIO(-1,this.s_subcons,4,t);
      k.KIO(-1,this.s_v_combo_R,1,t);
      k.KO(-1,t,"ះ");
    }
    else if(k.KA(0,k.KC(4,1,t),this.s_c_shifter)&&k.KA(1,k.KC(3,1,t),this.s_v_any)&&k.KCM(2,t,"្",1)&&k.KA(3,k.KC(1,1,t),this.s_subcons)) {   // Line 383
      m=1;
      k.KO(4,t,"្");
      k.KIO(-1,this.s_subcons,4,t);
      k.KIO(-1,this.s_c_shifter,1,t);
      k.KIO(-1,this.s_v_any,2,t);
    }
    else if(k.KCM(4,t,"្ដ្រ",4)) {   // Line 389
      m=1;
      k.KO(4,t,"្ត");
      k.KO(-1,t,"្");
      k.KO(-1,t,"រ");
    }
    else if(k.KCM(4,t,"្រ្ដ",4)) {   // Line 390
      m=1;
      k.KO(4,t,"្ត");
      k.KO(-1,t,"្");
      k.KO(-1,t,"រ");
    }
    else if(k.KCM(4,t,"្រ្",3)&&k.KA(3,k.KC(1,1,t),this.s_subcons)) {   // Line 401
      m=1;
      k.KO(4,t,"្");
      k.KIO(-1,this.s_subcons,4,t);
      k.KO(-1,t,"្");
      k.KO(-1,t,"រ");
    }
    else if(k.KA(0,k.KC(4,1,t),this.s_shiftable_c_1st)&&k.KCM(3,t,"ុាំ",3)) {   // Line 469
      m=1;
      k.KIO(4,this.s_shiftable_c_1st,1,t);
      k.KO(-1,t,"៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KA(0,k.KC(4,1,t),this.s_shiftable_c_2nd)&&k.KCM(3,t,"ុាំ",3)) {   // Line 476
      m=1;
      k.KIO(4,this.s_shiftable_c_2nd,1,t);
      k.KO(-1,t,"៉");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KA(0,k.KC(4,1,t),this.s_shiftable_c_2nd)&&k.KCM(3,t,"៊",1)&&k.KA(2,k.KC(2,1,t),this.s_v_gen)&&k.KA(3,k.KC(1,1,t),this.s_v_pseudo)) {   // Line 490
      m=1;
      k.KIO(4,this.s_shiftable_c_2nd,1,t);
      k.KO(-1,t,"៉");
      k.KIO(-1,this.s_v_gen,3,t);
      k.KIO(-1,this.s_v_pseudo,4,t);
    }
    else if(k.KCM(4,t,"្",1)&&k.KA(1,k.KC(3,1,t),this.s_shiftable_c_2nd)&&k.KCM(2,t,"៊",1)&&k.KA(3,k.KC(1,1,t),this.s_v_above)) {   // Line 492
      m=1;
    }
    else if(k.KCM(4,t,"ប្យ",3)&&k.KA(3,k.KC(1,1,t),this.s_c_shifter)) {   // Line 497
      m=1;
    }
    else if(k.KCM(4,t,"ស្ប",3)&&k.KA(3,k.KC(1,1,t),this.s_c_shifter)) {   // Line 498
      m=1;
    }
    else if(k.KCM(4,t,"ឆ្ប",3)&&k.KA(3,k.KC(1,1,t),this.s_c_shifter)) {   // Line 499
      m=1;
    }
    else if(k.KCM(4,t,"ប្យ",3)&&k.KA(3,k.KC(1,1,t),this.s_c_shifter)) {   // Line 500
      m=1;
    }
    else if(k.KCM(4,t,"ស្ប",3)&&k.KA(3,k.KC(1,1,t),this.s_c_shifter)) {   // Line 501
      m=1;
    }
    else if(k.KCM(4,t,"ឆ្ប",3)&&k.KA(3,k.KC(1,1,t),this.s_c_shifter)) {   // Line 502
      m=1;
    }
    else if(k.KA(0,k.KC(4,1,t),this.s_shiftable_c_1st)&&k.KCM(3,t,"៉",1)&&k.KA(2,k.KC(2,1,t),this.s_v_gen)&&k.KA(3,k.KC(1,1,t),this.s_v_pseudo)) {   // Line 507
      m=1;
      k.KIO(4,this.s_shiftable_c_1st,1,t);
      k.KO(-1,t,"៊");
      k.KIO(-1,this.s_v_gen,3,t);
      k.KIO(-1,this.s_v_pseudo,4,t);
    }
    else if(k.KA(0,k.KC(4,1,t),this.s_shiftable_c_1st)&&k.KCM(3,t,"ាុំ",3)) {   // Line 528
      m=1;
      k.KIO(4,this.s_shiftable_c_1st,1,t);
      k.KO(-1,t,"៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KA(0,k.KC(4,1,t),this.s_shiftable_c_1st)&&k.KCM(3,t,"ុំា",3)) {   // Line 529
      m=1;
      k.KIO(4,this.s_shiftable_c_1st,1,t);
      k.KO(-1,t,"៊");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KA(0,k.KC(4,1,t),this.s_shiftable_c_2nd)&&k.KCM(3,t,"ាុំ",3)) {   // Line 554
      m=1;
      k.KIO(4,this.s_shiftable_c_2nd,1,t);
      k.KO(-1,t,"៉");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KA(0,k.KC(4,1,t),this.s_shiftable_c_2nd)&&k.KCM(3,t,"ុំា",3)) {   // Line 555
      m=1;
      k.KIO(4,this.s_shiftable_c_2nd,1,t);
      k.KO(-1,t,"៉");
      k.KO(-1,t,"ា");
      k.KO(-1,t,"ំ");
    }
    else if(k.KA(0,k.KC(4,1,t),this.s_shiftable_c_1st)&&k.KCM(3,t,"េុី",3)) {   // Line 565
      m=1;
      k.KIO(4,this.s_shiftable_c_1st,1,t);
      k.KO(-1,t,"៊ើ");
    }
    else if(k.KA(0,k.KC(4,1,t),this.s_shiftable_c_1st)&&k.KCM(3,t,"ុេី",3)) {   // Line 566
      m=1;
      k.KIO(4,this.s_shiftable_c_1st,1,t);
      k.KO(-1,t,"៊ើ");
    }
    else if(k.KA(0,k.KC(4,1,t),this.s_shiftable_c_1st)&&k.KCM(3,t,"៉េី",3)) {   // Line 567
      m=1;
      k.KIO(4,this.s_shiftable_c_1st,1,t);
      k.KO(-1,t,"៊ើ");
    }
    else if(k.KA(0,k.KC(4,1,t),this.s_shiftable_c_2nd)&&k.KCM(3,t,"េុី",3)) {   // Line 599
      m=1;
      k.KIO(4,this.s_shiftable_c_2nd,1,t);
      k.KO(-1,t,"៉ើ");
    }
    else if(k.KA(0,k.KC(4,1,t),this.s_shiftable_c_2nd)&&k.KCM(3,t,"ុេី",3)) {   // Line 600
      m=1;
      k.KIO(4,this.s_shiftable_c_2nd,1,t);
      k.KO(-1,t,"៉ើ");
    }
    else if(k.KA(0,k.KC(4,1,t),this.s_shiftable_c_2nd)&&k.KCM(3,t,"៊េី",3)) {   // Line 601
      m=1;
      k.KIO(4,this.s_shiftable_c_2nd,1,t);
      k.KO(-1,t,"៉ើ");
    }
    else if(k.KA(0,k.KC(3,1,t),this.s_v_gen)&&k.KA(1,k.KC(2,1,t),this.s_v_pseudo)&&k.KA(2,k.KC(1,1,t),this.s_c_shifter)) {   // Line 351
      m=1;
      k.KIO(3,this.s_c_shifter,3,t);
      k.KIO(-1,this.s_v_gen,1,t);
      k.KIO(-1,this.s_v_pseudo,2,t);
    }
    else if(k.KA(0,k.KC(3,1,t),this.s_c_shifter)&&k.KA(1,k.KC(2,1,t),this.s_v_any)&&k.KA(2,k.KC(1,1,t),this.s_c_shifter)) {   // Line 352
      m=1;
      k.KIO(3,this.s_c_shifter,3,t);
      k.KIO(-1,this.s_v_any,2,t);
    }
    else if(k.KA(0,k.KC(3,1,t),this.s_v_gen)&&k.KA(1,k.KC(2,1,t),this.s_v_pseudo)&&k.KA(2,k.KC(1,1,t),this.s_v_gen)) {   // Line 361
      m=1;
      k.KIO(3,this.s_v_gen,3,t);
    }
    else if(k.KA(0,k.KC(3,1,t),this.s_v_gen)&&k.KA(1,k.KC(2,1,t),this.s_v_pseudo)&&k.KA(2,k.KC(1,1,t),this.s_v_pseudo)) {   // Line 362
      m=1;
      k.KIO(3,this.s_v_pseudo,3,t);
    }
    else if(k.KA(0,k.KC(3,1,t),this.s_v_gen)&&k.KA(1,k.KC(2,1,t),this.s_v_gen)&&k.KA(2,k.KC(1,1,t),this.s_v_pseudo)) {   // Line 363
      m=1;
      k.KIO(3,this.s_v_gen,2,t);
      k.KIO(-1,this.s_v_pseudo,3,t);
    }
    else if(k.KA(0,k.KC(3,1,t),this.s_v_pseudo)&&k.KA(1,k.KC(2,1,t),this.s_v_gen)&&k.KA(2,k.KC(1,1,t),this.s_v_pseudo)) {   // Line 364
      m=1;
      k.KIO(3,this.s_v_gen,2,t);
      k.KIO(-1,this.s_v_pseudo,3,t);
    }
    else if(k.KCM(3,t,"្",1)&&k.KA(1,k.KC(2,1,t),this.s_v_gen)&&k.KA(2,k.KC(1,1,t),this.s_v_pseudo)) {   // Line 373
      m=1;
      k.KIO(3,this.s_v_gen,2,t);
      k.KIO(-1,this.s_v_pseudo,3,t);
    }
    else if(k.KA(0,k.KC(3,1,t),this.s_v_any)&&k.KCM(2,t,"្",1)&&k.KA(2,k.KC(1,1,t),this.s_subcons)) {   // Line 377
      m=1;
      k.KO(3,t,"្");
      k.KIO(-1,this.s_subcons,3,t);
      k.KIO(-1,this.s_v_any,1,t);
    }
    else if(k.KA(0,k.KC(3,1,t),this.s_c_shifter)&&k.KCM(2,t,"្",1)&&k.KA(2,k.KC(1,1,t),this.s_subcons)) {   // Line 408
      m=1;
      k.KO(3,t,"្");
      k.KIO(-1,this.s_subcons,3,t);
      k.KIO(-1,this.s_c_shifter,1,t);
    }
    else if(k.KA(0,k.KC(3,1,t),this.s_shiftable_c_1st)&&k.KCM(2,t,"ុ",1)&&k.KA(2,k.KC(1,1,t),this.s_v_above)) {   // Line 468
      m=1;
      k.KIO(3,this.s_shiftable_c_1st,1,t);
      k.KO(-1,t,"៊");
      k.KIO(-1,this.s_v_above,3,t);
    }
    else if(k.KA(0,k.KC(3,1,t),this.s_shiftable_c_1st)&&k.KA(1,k.KC(2,1,t),this.s_v_above)&&k.KCM(1,t,"ុ",1)) {   // Line 470
      m=1;
      k.KIO(3,this.s_shiftable_c_1st,1,t);
      k.KO(-1,t,"៊");
      k.KIO(-1,this.s_v_above,2,t);
    }
    else if(k.KA(0,k.KC(3,1,t),this.s_shiftable_c_2nd)&&k.KCM(2,t,"ុ",1)&&k.KA(2,k.KC(1,1,t),this.s_v_above)) {   // Line 475
      m=1;
      k.KIO(3,this.s_shiftable_c_2nd,1,t);
      k.KO(-1,t,"៉");
      k.KIO(-1,this.s_v_above,3,t);
    }
    else if(k.KA(0,k.KC(3,1,t),this.s_shiftable_c_2nd)&&k.KA(1,k.KC(2,1,t),this.s_v_above)&&k.KCM(1,t,"ុ",1)) {   // Line 477
      m=1;
      k.KIO(3,this.s_shiftable_c_2nd,1,t);
      k.KO(-1,t,"៉");
      k.KIO(-1,this.s_v_above,2,t);
    }
    else if(k.KA(0,k.KC(3,1,t),this.s_shiftable_c_2nd)&&k.KCM(2,t,"៊",1)&&k.KA(2,k.KC(1,1,t),this.s_v_above)) {   // Line 489
      m=1;
      k.KIO(3,this.s_shiftable_c_2nd,1,t);
      k.KO(-1,t,"៉");
      k.KIO(-1,this.s_v_above,3,t);
    }
    else if(k.KA(0,k.KC(3,1,t),this.s_shiftable_c_1st)&&k.KCM(2,t,"៉",1)&&k.KA(2,k.KC(1,1,t),this.s_v_above)) {   // Line 506
      m=1;
      k.KIO(3,this.s_shiftable_c_1st,1,t);
      k.KO(-1,t,"៊");
      k.KIO(-1,this.s_v_above,3,t);
    }
    else if(k.KCM(3,t,"ណ្ត",3)) {   // Line 613
      m=1;
      k.KO(3,t,"ណ");
      k.KO(-1,t,"្ដ");
    }
    else if(k.KCM(3,t,"ន្ដ",3)) {   // Line 614
      m=1;
      k.KO(3,t,"ន");
      k.KO(-1,t,"្ត");
    }
    else if(k.KCM(3,t,"ទ្ប",3)) {   // Line 618
      m=1;
      k.KO(3,t,"ឡ");
    }
    else if(k.KCM(3,t,"ប្ញ",3)) {   // Line 620
      m=1;
      k.KO(3,t,"ឫ");
    }
    else if(k.KCM(3,t,"ព្ញ",3)) {   // Line 626
      m=1;
      k.KO(3,t,"ឭ");
    }
    else if(k.KCM(3,t,"ព្ឋ",3)) {   // Line 629
      m=1;
      k.KO(3,t,"ឰ");
    }
    else if(k.KCM(3,t,"ដ្ធ",3)) {   // Line 637
      m=1;
      k.KO(3,t,"ដ្ឋ");
    }
    else if(k.KCM(3,t,"ទ្ឋ",3)) {   // Line 638
      m=1;
      k.KO(3,t,"ទ្ធ");
    }
    else if(k.KCM(2,t,"េា",2)) {   // Line 338
      m=1;
      k.KO(2,t,"ោ");
    }
    else if(k.KCM(2,t,"ាេ",2)) {   // Line 339
      m=1;
      k.KO(2,t,"ោ");
    }
    else if(k.KCM(2,t,"េី",2)) {   // Line 340
      m=1;
      k.KO(2,t,"ើ");
    }
    else if(k.KCM(2,t,"ីេ",2)) {   // Line 341
      m=1;
      k.KO(2,t,"ើ");
    }
    else if(k.KCM(2,t,"ំុ",2)) {   // Line 345
      m=1;
      k.KO(2,t,"ុំ");
    }
    else if(k.KCM(2,t,"ំា",2)) {   // Line 346
      m=1;
      k.KO(2,t,"ាំ");
    }
    else if(k.KA(0,k.KC(2,1,t),this.s_v_any)&&k.KA(1,k.KC(1,1,t),this.s_c_shifter)) {   // Line 350
      m=1;
      k.KIO(2,this.s_c_shifter,2,t);
      k.KIO(-1,this.s_v_any,1,t);
    }
    else if(k.KA(0,k.KC(2,1,t),this.s_v_gen)&&k.KA(1,k.KC(1,1,t),this.s_v_gen)) {   // Line 357
      m=1;
      k.KIO(2,this.s_v_gen,2,t);
    }
    else if(k.KA(0,k.KC(2,1,t),this.s_v_pseudo)&&k.KA(1,k.KC(1,1,t),this.s_v_pseudo)) {   // Line 366
      m=1;
      k.KIO(2,this.s_v_pseudo,2,t);
    }
    else if(k.KCM(2,t,"្្",2)) {   // Line 371
      m=1;
      k.KO(2,t,"្");
    }
    else if(k.KCM(2,t,"្",1)&&k.KA(1,k.KC(1,1,t),this.s_v_any)) {   // Line 372
      m=1;
      k.KIO(2,this.s_v_any,2,t);
    }
    else if(k.KCM(2,t,"ឫុ",2)) {   // Line 621
      m=1;
      k.KO(2,t,"ឬ");
    }
    else if(k.KCM(2,t,"ឭា",2)) {   // Line 623
      m=1;
      k.KO(2,t,"ញ");
    }
    else if(k.KCM(2,t,"ឮា",2)) {   // Line 624
      m=1;
      k.KO(2,t,"ញ");
    }
    else if(k.KCM(2,t,"ឭុ",2)) {   // Line 627
      m=1;
      k.KO(2,t,"ឮ");
    }
    else if(k.KCM(2,t,"ឧិ",2)) {   // Line 631
      m=1;
      k.KO(2,t,"ឱ");
    }
    else if(k.KCM(2,t,"ឧ៌",2)) {   // Line 632
      m=1;
      k.KO(2,t,"ឱ");
    }
    else if(k.KCM(2,t,"ឧ៍",2)) {   // Line 633
      m=1;
      k.KO(2,t,"ឱ");
    }
    return r;
  };
}
