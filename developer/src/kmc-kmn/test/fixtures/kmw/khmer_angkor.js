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

  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_khmer_angkor";
  this.KN="Khmer Angkor";
  this.KMINVER="10.0";
  this.KV={F:' 1em "Khmer Busra Kbd"',K102:0};
  this.KV.KLS={
    "rightalt": ["‍","‌","@","","$","€","៙","៚","*","{","}","≈","","","","","ៜ","","ឯ","ឫ","ឨ","[","]","ឦ","ឱ","ឰ","ឩ","ឳ","\\","","","","+","-","×","÷",":","‘","’","ឝ","៘","៖","ៈ","","","","","","","<",">","#","&","ឞ",";","",",",".","/","","","","",""," "],
    "rightalt-shift": ["","៱","៲","៳","៴","៵","៶","៷","៸","៹","៰","","","","","","᧠","᧡","᧢","᧣","᧤","᧥","᧦","᧧","᧨","᧩","᧪","᧫","","","","","᧬","᧭","᧮","᧯","᧰","᧱","᧲","᧳","᧴","᧵","᧶","","","","","","","᧷","᧸","᧹","᧺","᧻","᧼","᧽","᧾","᧿","","","","","","",""],
    "default": ["«","១","២","៣","៤","៥","៦","៧","៨","៩","០","ឥ","ឲ","","","","ឆ","","","រ","ត","យ","","","","ផ","","ឪ","ឮ","","","","","ស","ដ","ថ","ង","ហ","","ក","ល","","","","","","","","","ឋ","ខ","ច","វ","ប","ន","ម","","។","","","","","","","​"],
    "shift": ["»","!","ៗ","\"","៛","%","","","","(",")","","=","","","","ឈ","","","ឬ","ទ","","","","","ភ","","ឧ","ឭ","","","","","","ឌ","ធ","អ","ះ","ញ","គ","ឡ","","","","","","","","","ឍ","ឃ","ជ","","ព","ណ","","","៕","?","","","","","",""]
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
  this.KDU=0;
  this.KH='';
  this.KM=0;
  this.KBVER="1.3";
  this.KMBM=modCodes.RALT | modCodes.SHIFT /* 0x0018 */;
  this.KVKD="T_17D2_1780 T_17D2_1781 T_17D2_1782 T_17D2_1783 T_17D2_1784 T_17D2_1785 T_17D2_1786 T_17D2_1787 T_17D2_1788 T_17D2_1789 T_17D2_178A T_17D2_178B T_17D2_178C T_17D2_178D T_17D2_178E T_17D2_178F T_17D2_1790 T_17D2_1791 T_17D2_1792 T_17D2_1793 T_17D2_1794 T_17D2_1795 T_17D2_1796 T_17D2_1797 T_17D2_1798 T_17D2_1799 T_17D2_179A T_17D2_179B T_17D2_179C T_17D2_179D T_17D2_179E T_17D2_179F T_17D2_17A0 T_17D2_17A1 T_17D2_17A2 U_0030 U_0031 U_0032 U_0033 U_0034 U_0035 U_0036 U_0037 U_0038 U_0039";
  this.KVKL={
  "phone": {
    "font": "Khmer Busra Kbd",
    "fontsize": "0.8em",
    "displayUnderlying": false,
    "layer": [
      {
        "id": "default",
        "row": [
          {
            "id": "1",
            "key": [
              {
                "id": "K_Q",
                "text": "\u1786",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_Q",
                    "text": "\u1788"
                  },
                  {
                    "id": "T_17D2_1786",
                    "text": "\uEF86"
                  },
                  {
                    "id": "T_17D2_1788",
                    "text": "\uEF88"
                  }
                ]
              },
              {
                "id": "K_W",
                "text": "\uEFB9",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_W",
                    "text": "\uEFBA"
                  }
                ]
              },
              {
                "id": "K_E",
                "text": "\uEFC1",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_E",
                    "text": "\uEFC2"
                  },
                  {
                    "layer": "shift",
                    "id": "K_S",
                    "text": "\uEFC3"
                  },
                  {
                    "layer": "shift",
                    "id": "K_V",
                    "text": "\uEF12"
                  },
                  {
                    "id": "U_17AF",
                    "text": "\u17AF"
                  },
                  {
                    "id": "U_17B0",
                    "text": "\u17B0"
                  }
                ]
              },
              {
                "id": "K_R",
                "text": "\u179A",
                "sk": [
                  {
                    "id": "T_17D2_179A",
                    "text": "\uEF9A"
                  },
                  {
                    "id": "U_17AB",
                    "text": "\u17AB"
                  },
                  {
                    "id": "U_17AC",
                    "text": "\u17AC"
                  }
                ]
              },
              {
                "id": "K_T",
                "text": "\u178F",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_T",
                    "text": "\u1791"
                  },
                  {
                    "id": "T_17D2_178F",
                    "text": "\uEF8F"
                  },
                  {
                    "layer": "default",
                    "id": "T_17D2_1791",
                    "text": "\uEF91"
                  }
                ]
              },
              {
                "id": "K_Y",
                "text": "\u1799",
                "sk": [
                  {
                    "id": "T_17D2_1799",
                    "text": "\uEF99"
                  }
                ]
              },
              {
                "id": "K_U",
                "text": "\uEFBB",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_U",
                    "text": "\uEFBC"
                  },
                  {
                    "layer": "shift",
                    "id": "K_Y",
                    "text": "\uEFBD"
                  },
                  {
                    "id": "U_17A7",
                    "text": "\u17A7"
                  },
                  {
                    "layer": "shift",
                    "id": "U_17AA",
                    "text": "\u17AA"
                  },
                  {
                    "layer": "shift",
                    "id": "U_17A9",
                    "text": "\u17A9"
                  },
                  {
                    "id": "U_17A8",
                    "text": "\u17A8"
                  }
                ]
              },
              {
                "id": "K_I",
                "text": "\uEFB7",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_I",
                    "text": "\uEFB8"
                  },
                  {
                    "id": "U_17A5",
                    "text": "\u17A5"
                  },
                  {
                    "layer": "shift",
                    "id": "U_17A6",
                    "text": "\u17A6"
                  }
                ]
              },
              {
                "id": "K_O",
                "text": "\uEFC4",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_O",
                    "text": "\uEFC5"
                  },
                  {
                    "id": "K_LBRKT",
                    "text": "\uEFC0"
                  },
                  {
                    "layer": "shift",
                    "id": "K_LBRKT",
                    "text": "\uEFBF"
                  },
                  {
                    "layer": "shift",
                    "id": "K_COLON",
                    "text": "\uEF14"
                  },
                  {
                    "id": "U_17B1",
                    "text": "\u17B1"
                  },
                  {
                    "id": "U_17B2",
                    "text": "\u17B2"
                  },
                  {
                    "layer": "shift",
                    "id": "U_17B3",
                    "text": "\u17B3"
                  }
                ]
              },
              {
                "id": "K_P",
                "text": "\u1795",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_P",
                    "text": "\u1797"
                  },
                  {
                    "id": "T_17D2_1795",
                    "text": "\uEF95"
                  },
                  {
                    "layer": "default",
                    "id": "T_17D2_1797",
                    "text": "\uEF97"
                  }
                ]
              }
            ]
          },
          {
            "id": "2",
            "key": [
              {
                "width": "100",
                "id": "K_A",
                "text": "\uEFB6",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_A",
                    "text": "\uEF11"
                  }
                ]
              },
              {
                "id": "K_S",
                "text": "\u179F",
                "sk": [
                  {
                    "id": "T_17D2_179F",
                    "text": "\uEF9F"
                  },
                  {
                    "id": "U_179D",
                    "text": "\u179D"
                  },
                  {
                    "id": "U_179E",
                    "text": "\u179E"
                  }
                ]
              },
              {
                "id": "K_D",
                "text": "\u178A",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_D",
                    "text": "\u178C"
                  },
                  {
                    "id": "T_17D2_178A",
                    "text": "\uEF8A"
                  },
                  {
                    "layer": "default",
                    "id": "T_17D2_178C",
                    "text": "\uEF8C"
                  }
                ]
              },
              {
                "id": "K_F",
                "text": "\u1790",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_F",
                    "text": "\u1792"
                  },
                  {
                    "id": "T_17D2_1790",
                    "text": "\uEF90"
                  },
                  {
                    "layer": "default",
                    "id": "T_17D2_1792",
                    "text": "\uEF92"
                  }
                ]
              },
              {
                "id": "K_G",
                "text": "\u1784",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_G",
                    "text": "\u17A2"
                  },
                  {
                    "id": "T_17D2_1784",
                    "text": "\uEF84"
                  },
                  {
                    "layer": "default",
                    "id": "T_17D2_17A2",
                    "text": "\uEFA2"
                  }
                ]
              },
              {
                "id": "K_H",
                "text": "\u17A0",
                "sk": [
                  {
                    "id": "T_17D2_17A0",
                    "text": "\uEFA0"
                  },
                  {
                    "layer": "shift",
                    "id": "K_H",
                    "text": "\u17C7"
                  },
                  {
                    "id": "U_17C8",
                    "text": "\u17C8"
                  }
                ]
              },
              {
                "layer": "shift",
                "id": "K_J",
                "text": "\u1789",
                "sk": [
                  {
                    "id": "T_17D2_1789",
                    "text": "\uEF89"
                  }
                ]
              },
              {
                "id": "K_K",
                "text": "\u1780",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_K",
                    "text": "\u1782"
                  },
                  {
                    "id": "T_17D2_1780",
                    "text": "\uEF80"
                  },
                  {
                    "id": "T_17D2_1782",
                    "text": "\uEF82"
                  }
                ]
              },
              {
                "id": "K_L",
                "text": "\u179B",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_L",
                    "text": "\u17A1"
                  },
                  {
                    "id": "T_17D2_179B",
                    "text": "\uEF9B"
                  },
                  {
                    "id": "U_17AD",
                    "text": "\u17AD"
                  },
                  {
                    "id": "U_17AE",
                    "text": "\u17AE"
                  }
                ]
              },
              {
                "id": "K_COLON",
                "text": "\uEFBE"
              }
            ]
          },
          {
            "id": "3",
            "key": [
              {
                "id": "K_Z",
                "text": "\u178B",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_Z",
                    "text": "\u178D"
                  },
                  {
                    "id": "T_17D2_178B",
                    "text": "\uEF8B"
                  },
                  {
                    "layer": "default",
                    "id": "T_17D2_178D",
                    "text": "\uEF8D"
                  }
                ]
              },
              {
                "id": "K_X",
                "text": "\u1781",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_X",
                    "text": "\u1783"
                  },
                  {
                    "id": "T_17D2_1781",
                    "text": "\uEF81"
                  },
                  {
                    "layer": "default",
                    "id": "T_17D2_1783",
                    "text": "\uEF83"
                  }
                ]
              },
              {
                "id": "K_C",
                "text": "\u1785",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_C",
                    "text": "\u1787"
                  },
                  {
                    "id": "T_17D2_1785",
                    "text": "\uEF85"
                  },
                  {
                    "layer": "default",
                    "id": "T_17D2_1787",
                    "text": "\uEF87"
                  }
                ]
              },
              {
                "id": "K_V",
                "text": "\u179C",
                "sk": [
                  {
                    "id": "T_17D2_179C",
                    "text": "\uEF9C"
                  }
                ]
              },
              {
                "id": "K_B",
                "text": "\u1794",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_B",
                    "text": "\u1796"
                  },
                  {
                    "id": "T_17D2_1794",
                    "text": "\uEF94"
                  },
                  {
                    "layer": "default",
                    "id": "T_17D2_1796",
                    "text": "\uEF96"
                  }
                ]
              },
              {
                "id": "K_N",
                "text": "\u1793",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_N",
                    "text": "\u178E"
                  },
                  {
                    "id": "T_17D2_1793",
                    "text": "\uEF93"
                  },
                  {
                    "layer": "default",
                    "id": "T_17D2_178E",
                    "text": "\uEF8E"
                  }
                ]
              },
              {
                "id": "K_M",
                "text": "\u1798",
                "sk": [
                  {
                    "id": "T_17D2_1798",
                    "text": "\uEF98"
                  },
                  {
                    "layer": "shift",
                    "id": "K_M",
                    "text": "\uEFC6"
                  }
                ]
              },
              {
                "id": "K_COMMA",
                "text": "\uEF10",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_COMMA",
                    "text": "\uEF13"
                  },
                  {
                    "layer": "shift",
                    "id": "K_6",
                    "text": "\uEFCD"
                  },
                  {
                    "layer": "shift",
                    "id": "K_7",
                    "text": "\uEFD0"
                  },
                  {
                    "layer": "shift",
                    "id": "K_8",
                    "text": "\uEFCF"
                  },
                  {
                    "layer": "shift",
                    "id": "K_HYPHEN",
                    "text": "\uEFCC"
                  },
                  {
                    "layer": "shift",
                    "id": "U_17D1",
                    "text": "\uEFD1"
                  },
                  {
                    "layer": "shift",
                    "id": "U_17DD",
                    "text": "\uEFDD"
                  },
                  {
                    "layer": "shift",
                    "id": "U_17CE",
                    "text": "\uEFCE"
                  }
                ]
              },
              {
                "width": "100",
                "id": "K_QUOTE",
                "text": "\uEFCB",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_QUOTE",
                    "text": "\uEFC9"
                  },
                  {
                    "id": "K_SLASH",
                    "text": "\uEFCA"
                  }
                ]
              },
              {
                "width": "100",
                "id": "K_BKSP",
                "sp": "1",
                "text": "*BkSp*"
              }
            ]
          },
          {
            "id": "4",
            "key": [
              {
                "nextlayer": "numeric",
                "width": "140",
                "id": "K_NUMLOCK",
                "sp": "1",
                "text": "\u17E1\u17E2\u17E3"
              },
              {
                "width": "120",
                "id": "K_LOPT",
                "sp": "1",
                "text": "*Menu*"
              },
              {
                "width": "555",
                "id": "K_SPACE",
                "text": "\u200B",
                "sk": [
                  {
                    "layer": "default",
                    "id": "U_0020",
                    "text": " "
                  }
                ]
              },
              {
                "width": "120",
                "id": "K_PERIOD",
                "text": "\u17D4",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_PERIOD",
                    "text": "\u17D5"
                  },
                  {
                    "id": "U_0021",
                    "text": "!"
                  },
                  {
                    "id": "U_003F",
                    "text": "?"
                  }
                ]
              },
              {
                "width": "140",
                "id": "K_ENTER",
                "sp": "1",
                "text": "*Enter*"
              }
            ]
          }
        ]
      },
      {
        "id": "numeric",
        "row": [
          {
            "id": "1",
            "key": [
              {
                "id": "K_1",
                "text": "\u17E1",
                "sk": [
                  {
                    "id": "U_0031",
                    "text": "1"
                  }
                ]
              },
              {
                "id": "K_2",
                "text": "\u17E2",
                "sk": [
                  {
                    "id": "U_0032",
                    "text": "2"
                  }
                ]
              },
              {
                "id": "K_3",
                "text": "\u17E3",
                "sk": [
                  {
                    "id": "U_0033",
                    "text": "3"
                  }
                ]
              },
              {
                "id": "K_4",
                "text": "\u17E4",
                "sk": [
                  {
                    "id": "U_0034",
                    "text": "4"
                  }
                ]
              },
              {
                "id": "K_5",
                "text": "\u17E5",
                "sk": [
                  {
                    "id": "U_0035",
                    "text": "5"
                  }
                ]
              },
              {
                "id": "K_6",
                "text": "\u17E6",
                "sk": [
                  {
                    "id": "U_0036",
                    "text": "6"
                  }
                ]
              },
              {
                "id": "K_7",
                "text": "\u17E7",
                "sk": [
                  {
                    "id": "U_0037",
                    "text": "7"
                  }
                ]
              },
              {
                "id": "K_8",
                "text": "\u17E8",
                "sk": [
                  {
                    "id": "U_0038",
                    "text": "8"
                  }
                ]
              },
              {
                "id": "K_9",
                "text": "\u17E9",
                "sk": [
                  {
                    "id": "U_0039",
                    "text": "9"
                  }
                ]
              },
              {
                "id": "K_0",
                "text": "\u17E0",
                "sk": [
                  {
                    "id": "U_0030",
                    "text": "0"
                  },
                  {
                    "id": "U_17D3",
                    "text": "\uEFD3"
                  }
                ]
              }
            ]
          },
          {
            "id": "2",
            "key": [
              {
                "id": "U_0040",
                "text": "@",
                "sk": [
                  {
                    "id": "U_00A9",
                    "text": "\u00A9"
                  },
                  {
                    "id": "U_00AE",
                    "text": "\u00AE"
                  }
                ]
              },
              {
                "id": "U_0023",
                "text": "#",
                "sk": [
                  {
                    "id": "U_2116",
                    "text": "\u2116"
                  },
                  {
                    "id": "U_007E",
                    "text": "~"
                  }
                ]
              },
              {
                "id": "U_17DB",
                "text": "\u17DB",
                "sk": [
                  {
                    "id": "U_0024",
                    "text": "$"
                  },
                  {
                    "id": "U_0E3F",
                    "text": "\u0E3F"
                  },
                  {
                    "id": "U_00A2",
                    "text": "\u00A2"
                  },
                  {
                    "id": "U_00A3",
                    "text": "\u00A3"
                  },
                  {
                    "id": "U_00A5",
                    "text": "\u00A5"
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
                    "id": "U_2030",
                    "text": "\u2030"
                  },
                  {
                    "id": "U_2031",
                    "text": "\u2031"
                  }
                ]
              },
              {
                "id": "U_002B",
                "text": "+",
                "sk": [
                  {
                    "id": "U_002D",
                    "text": "-"
                  },
                  {
                    "id": "U_00D7",
                    "text": "\u00D7"
                  },
                  {
                    "id": "U_00F7",
                    "text": "\u00F7"
                  },
                  {
                    "id": "U_00B1",
                    "text": "\u00B1"
                  }
                ]
              },
              {
                "id": "U_003D",
                "text": "=",
                "sk": [
                  {
                    "id": "U_005F",
                    "text": "_"
                  },
                  {
                    "id": "U_2260",
                    "text": "\u2260"
                  }
                ]
              },
              {
                "id": "U_002A",
                "text": "*",
                "sk": [
                  {
                    "id": "U_005E",
                    "text": "^"
                  }
                ]
              },
              {
                "id": "U_003F",
                "text": "?",
                "sk": [
                  {
                    "id": "U_00BF",
                    "text": "\u00BF"
                  }
                ]
              },
              {
                "id": "U_0021",
                "text": "!",
                "sk": [
                  {
                    "id": "U_00A1",
                    "text": "\u00A1"
                  }
                ]
              }
            ]
          },
          {
            "id": "3",
            "key": [
              {
                "id": "U_2018",
                "text": "\u2018",
                "sk": [
                  {
                    "id": "U_2019",
                    "text": "\u2019"
                  }
                ]
              },
              {
                "id": "U_201C",
                "text": "\u201C",
                "sk": [
                  {
                    "id": "U_201D",
                    "text": "\u201D"
                  }
                ]
              },
              {
                "id": "U_00AB",
                "text": "\u00AB",
                "sk": [
                  {
                    "id": "U_00BB",
                    "text": "\u00BB"
                  }
                ]
              },
              {
                "id": "U_002F",
                "text": "/",
                "sk": [
                  {
                    "id": "U_005C",
                    "text": "\\"
                  },
                  {
                    "id": "U_007C",
                    "text": "|"
                  },
                  {
                    "id": "U_00A6",
                    "text": "\u00A6"
                  }
                ]
              },
              {
                "id": "U_0028",
                "text": "(",
                "sk": [
                  {
                    "id": "U_0029",
                    "text": ")"
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
                "id": "U_17D9",
                "text": "\u17D9",
                "sk": [
                  {
                    "id": "U_17DA",
                    "text": "\u17DA"
                  },
                  {
                    "id": "U_17DC",
                    "text": "\u17DC"
                  },
                  {
                    "id": "U_00A7",
                    "text": "\u00A7"
                  },
                  {
                    "id": "U_00D8",
                    "text": "\u00D8"
                  }
                ]
              },
              {
                "id": "U_17D7",
                "text": "\u17D7"
              },
              {
                "id": "U_003C",
                "text": "<",
                "sk": [
                  {
                    "id": "U_2264",
                    "text": "\u2264"
                  },
                  {
                    "id": "U_003E",
                    "text": ">"
                  },
                  {
                    "id": "U_2265",
                    "text": "\u2265"
                  }
                ]
              },
              {
                "id": "U_17D6",
                "text": "\u17D6",
                "sk": [
                  {
                    "id": "U_003A",
                    "text": ":"
                  },
                  {
                    "id": "U_003B",
                    "text": ";"
                  },
                  {
                    "id": "U_2026",
                    "text": "\u2026"
                  }
                ]
              },
              {
                "id": "K_BKSP",
                "sp": "1",
                "text": "*BkSp*"
              }
            ]
          },
          {
            "id": "4",
            "key": [
              {
                "nextlayer": "default",
                "width": "140",
                "id": "K_LCONTROL",
                "sp": "2",
                "text": "\u17E1\u17E2\u17E3"
              },
              {
                "width": "120",
                "id": "K_LOPT",
                "sp": "1",
                "text": "*Menu*"
              },
              {
                "layer": "shift",
                "width": "555",
                "id": "K_SPACE",
                "text": "\u200B"
              },
              {
                "width": "120",
                "id": "K_PERIOD",
                "text": "\u17D4",
                "sk": [
                  {
                    "layer": "shift",
                    "id": "K_PERIOD",
                    "text": "\u17D5"
                  },
                  {
                    "id": "U_0021",
                    "text": "!"
                  },
                  {
                    "id": "U_003F",
                    "text": "?"
                  }
                ]
              },
              {
                "width": "140",
                "id": "K_ENTER",
                "sp": "1",
                "text": "*Enter*"
              }
            ]
          }
        ]
      }
    ]
  },
  "tablet": {
    "font": "Khmer Busra Kbd",
    "fontsize": "0.8em",
    "displayUnderlying": false,
    "layer": [
      {
        "id": "default",
        "row": [
          {
            "id": "1",
            "key": [
              {
                "id": "K_1",
                "text": "\u17E1"
              },
              {
                "id": "K_2",
                "text": "\u17E2"
              },
              {
                "id": "K_3",
                "text": "\u17E3"
              },
              {
                "id": "K_4",
                "text": "\u17E4"
              },
              {
                "id": "K_5",
                "text": "\u17E5"
              },
              {
                "id": "K_6",
                "text": "\u17E6"
              },
              {
                "id": "K_7",
                "text": "\u17E7"
              },
              {
                "id": "K_8",
                "text": "\u17E8"
              },
              {
                "id": "K_9",
                "text": "\u17E9"
              },
              {
                "id": "K_0",
                "text": "\u17E0"
              },
              {
                "id": "K_HYPHEN",
                "text": "\u17A5"
              },
              {
                "id": "K_EQUAL",
                "text": "\u17B2"
              },
              {
                "width": "100",
                "id": "K_BKSP",
                "sp": "1",
                "text": "*BkSp*"
              }
            ]
          },
          {
            "id": "2",
            "key": [
              {
                "id": "K_Q",
                "pad": "75",
                "text": "\u1786"
              },
              {
                "id": "K_W",
                "text": "\uEFB9"
              },
              {
                "id": "K_E",
                "text": "\uEFC1"
              },
              {
                "id": "K_R",
                "text": "\u179A"
              },
              {
                "id": "K_T",
                "text": "\u178F"
              },
              {
                "id": "K_Y",
                "text": "\u1799"
              },
              {
                "id": "K_U",
                "text": "\uEFBB"
              },
              {
                "id": "K_I",
                "text": "\uEFB7"
              },
              {
                "id": "K_O",
                "text": "\uEFC4"
              },
              {
                "id": "K_P",
                "text": "\u1795"
              },
              {
                "id": "K_LBRKT",
                "text": "\uEFC0"
              },
              {
                "id": "K_RBRKT",
                "text": "\u17AA"
              },
              {
                "width": "10",
                "id": "T_new_138",
                "sp": "10"
              }
            ]
          },
          {
            "id": "3",
            "key": [
              {
                "id": "K_BKQUOTE",
                "text": "\u00AB"
              },
              {
                "id": "K_A",
                "text": "\uEFB6"
              },
              {
                "id": "K_S",
                "text": "\u179F"
              },
              {
                "id": "K_D",
                "text": "\u178A"
              },
              {
                "id": "K_F",
                "text": "\u1790"
              },
              {
                "id": "K_G",
                "text": "\u1784"
              },
              {
                "id": "K_H",
                "text": "\u17A0"
              },
              {
                "id": "K_J",
                "text": "\uEFD2"
              },
              {
                "id": "K_K",
                "text": "\u1780"
              },
              {
                "id": "K_L",
                "text": "\u179B"
              },
              {
                "id": "K_COLON",
                "text": "\uEFBE"
              },
              {
                "id": "K_QUOTE",
                "text": "\uEFCB"
              },
              {
                "id": "K_BKSLASH",
                "text": "\u17AE"
              }
            ]
          },
          {
            "id": "4",
            "key": [
              {
                "nextlayer": "shift",
                "width": "160",
                "id": "K_SHIFT",
                "sp": "1",
                "text": "*Shift*"
              },
              {
                "id": "K_oE2"
              },
              {
                "id": "K_Z",
                "text": "\u178B"
              },
              {
                "id": "K_X",
                "text": "\u1781"
              },
              {
                "id": "K_C",
                "text": "\u1785"
              },
              {
                "id": "K_V",
                "text": "\u179C"
              },
              {
                "id": "K_B",
                "text": "\u1794"
              },
              {
                "id": "K_N",
                "text": "\u1793"
              },
              {
                "id": "K_M",
                "text": "\u1798"
              },
              {
                "id": "K_COMMA",
                "text": "\uEF10"
              },
              {
                "id": "K_PERIOD",
                "text": "\u17D4"
              },
              {
                "id": "K_SLASH",
                "text": "\uEFCA"
              },
              {
                "width": "10",
                "id": "T_new_164",
                "sp": "10"
              }
            ]
          },
          {
            "id": "5",
            "key": [
              {
                "nextlayer": "rightalt",
                "width": "160",
                "id": "K_LCONTROL",
                "sp": "1",
                "text": "*AltGr*"
              },
              {
                "width": "160",
                "id": "K_LOPT",
                "sp": "1",
                "text": "*Menu*"
              },
              {
                "width": "930",
                "id": "K_SPACE",
                "text": "\u200B"
              },
              {
                "width": "160",
                "id": "K_ENTER",
                "sp": "1",
                "text": "*Enter*"
              }
            ]
          }
        ]
      },
      {
        "id": "rightalt",
        "row": [
          {
            "id": "1",
            "key": [
              {
                "id": "K_1",
                "text": "\u200C"
              },
              {
                "id": "K_2",
                "text": "@"
              },
              {
                "id": "K_3",
                "text": "\uEFD1"
              },
              {
                "id": "K_4",
                "text": "$"
              },
              {
                "id": "K_5",
                "text": "\u20AC"
              },
              {
                "id": "K_6",
                "text": "\u17D9"
              },
              {
                "id": "K_7",
                "text": "\u17DA"
              },
              {
                "id": "K_8",
                "text": "*"
              },
              {
                "id": "K_9",
                "text": "{"
              },
              {
                "id": "K_0",
                "text": "}"
              },
              {
                "id": "K_HYPHEN",
                "text": "\u2248"
              },
              {
                "id": "K_EQUAL",
                "text": "\uEFCE"
              },
              {
                "width": "100",
                "id": "K_BKSP",
                "sp": "1",
                "text": "*BkSp*"
              }
            ]
          },
          {
            "id": "2",
            "key": [
              {
                "id": "K_Q",
                "pad": "75",
                "text": "\u17DC"
              },
              {
                "id": "K_W",
                "text": "\uEFDD"
              },
              {
                "id": "K_E",
                "text": "\u17AF"
              },
              {
                "id": "K_R",
                "text": "\u17AB"
              },
              {
                "id": "K_T",
                "text": "\u17A8"
              },
              {
                "id": "K_Y",
                "text": "["
              },
              {
                "id": "K_U",
                "text": "]"
              },
              {
                "id": "K_I",
                "text": "\u17A6"
              },
              {
                "id": "K_O",
                "text": "\u17B1"
              },
              {
                "id": "K_P",
                "text": "\u17B0"
              },
              {
                "id": "K_LBRKT",
                "text": "\u17A9"
              },
              {
                "id": "K_RBRKT",
                "text": "\u17B3"
              },
              {
                "width": "10",
                "id": "T_new_307",
                "sp": "10"
              }
            ]
          },
          {
            "id": "3",
            "key": [
              {
                "id": "K_BKQUOTE",
                "text": "\u200D"
              },
              {
                "id": "K_A",
                "text": "+"
              },
              {
                "id": "K_S",
                "text": "-"
              },
              {
                "id": "K_D",
                "text": "\u00D7"
              },
              {
                "id": "K_F",
                "text": "\u00F7"
              },
              {
                "id": "K_G",
                "text": ":"
              },
              {
                "id": "K_H",
                "text": "\u2018"
              },
              {
                "id": "K_J",
                "text": "\u2019"
              },
              {
                "id": "K_K",
                "text": "\u179D"
              },
              {
                "id": "K_L",
                "text": "\u17D8"
              },
              {
                "id": "K_COLON",
                "text": "\u17D6"
              },
              {
                "id": "K_QUOTE",
                "text": "\u17C8"
              },
              {
                "id": "K_BKSLASH",
                "text": "\\"
              }
            ]
          },
          {
            "id": "4",
            "key": [
              {
                "nextlayer": "shift",
                "width": "160",
                "id": "K_SHIFT",
                "sp": "1",
                "text": "*Shift*"
              },
              {
                "id": "K_oE2"
              },
              {
                "id": "K_Z",
                "text": "<"
              },
              {
                "id": "K_X",
                "text": ">"
              },
              {
                "id": "K_C",
                "text": "#"
              },
              {
                "id": "K_V",
                "text": "&"
              },
              {
                "id": "K_B",
                "text": "\u179E"
              },
              {
                "id": "K_N",
                "text": ";"
              },
              {
                "id": "K_M",
                "text": "\uEFD3"
              },
              {
                "id": "K_COMMA",
                "text": ","
              },
              {
                "id": "K_PERIOD",
                "text": "."
              },
              {
                "id": "K_SLASH",
                "text": "/"
              },
              {
                "width": "10",
                "id": "T_new_333",
                "sp": "10"
              }
            ]
          },
          {
            "id": "5",
            "key": [
              {
                "nextlayer": "default",
                "width": "160",
                "id": "K_LCONTROL",
                "sp": "2",
                "text": "*AltGr*"
              },
              {
                "width": "160",
                "id": "K_LOPT",
                "sp": "1",
                "text": "*Menu*"
              },
              {
                "width": "930",
                "id": "K_SPACE",
                "text": "\u00A0"
              },
              {
                "width": "160",
                "id": "K_ENTER",
                "sp": "1",
                "text": "*Enter*"
              }
            ]
          }
        ]
      },
      {
        "id": "shift",
        "row": [
          {
            "id": "1",
            "key": [
              {
                "id": "K_1",
                "text": "!"
              },
              {
                "id": "K_2",
                "text": "\u17D7"
              },
              {
                "id": "K_3",
                "text": "\""
              },
              {
                "id": "K_4",
                "text": "\u17DB"
              },
              {
                "id": "K_5",
                "text": "%"
              },
              {
                "id": "K_6",
                "text": "\uEFCD"
              },
              {
                "id": "K_7",
                "text": "\uEFD0"
              },
              {
                "id": "K_8",
                "text": "\uEFCF"
              },
              {
                "id": "K_9",
                "text": "("
              },
              {
                "id": "K_0",
                "text": ")"
              },
              {
                "id": "K_HYPHEN",
                "text": "\uEFCC"
              },
              {
                "id": "K_EQUAL",
                "text": "="
              },
              {
                "width": "100",
                "id": "K_BKSP",
                "sp": "1",
                "text": "*BkSp*"
              }
            ]
          },
          {
            "id": "2",
            "key": [
              {
                "id": "K_Q",
                "pad": "75",
                "text": "\u1788"
              },
              {
                "id": "K_W",
                "text": "\uEFBA"
              },
              {
                "id": "K_E",
                "text": "\uEFC2"
              },
              {
                "id": "K_R",
                "text": "\u17AC"
              },
              {
                "id": "K_T",
                "text": "\u1791"
              },
              {
                "id": "K_Y",
                "text": "\uEFBD"
              },
              {
                "id": "K_U",
                "text": "\uEFBC"
              },
              {
                "id": "K_I",
                "text": "\uEFB8"
              },
              {
                "id": "K_O",
                "text": "\uEFC5"
              },
              {
                "id": "K_P",
                "text": "\u1797"
              },
              {
                "id": "K_LBRKT",
                "text": "\uEFBF"
              },
              {
                "id": "K_RBRKT",
                "text": "\u17A7"
              },
              {
                "width": "10",
                "id": "T_new_364",
                "sp": "10"
              }
            ]
          },
          {
            "id": "3",
            "key": [
              {
                "id": "K_BKQUOTE",
                "text": "\u00BB"
              },
              {
                "id": "K_A",
                "text": "\uEF11"
              },
              {
                "id": "K_S",
                "text": "\uEFC3"
              },
              {
                "id": "K_D",
                "text": "\u178C"
              },
              {
                "id": "K_F",
                "text": "\u1792"
              },
              {
                "id": "K_G",
                "text": "\u17A2"
              },
              {
                "id": "K_H",
                "text": "\u17C7"
              },
              {
                "id": "K_J",
                "text": "\u1789"
              },
              {
                "id": "K_K",
                "text": "\u1782"
              },
              {
                "id": "K_L",
                "text": "\u17A1"
              },
              {
                "id": "K_COLON",
                "text": "\uEF14"
              },
              {
                "id": "K_QUOTE",
                "text": "\uEFC9"
              },
              {
                "id": "K_BKSLASH",
                "text": "\u17AD"
              }
            ]
          },
          {
            "id": "4",
            "key": [
              {
                "nextlayer": "default",
                "width": "160",
                "id": "K_SHIFT",
                "sp": "2",
                "text": "*Shift*"
              },
              {
                "id": "K_oE2"
              },
              {
                "id": "K_Z",
                "text": "\u178D"
              },
              {
                "id": "K_X",
                "text": "\u1783"
              },
              {
                "id": "K_C",
                "text": "\u1787"
              },
              {
                "id": "K_V",
                "text": "\uEF12"
              },
              {
                "id": "K_B",
                "text": "\u1796"
              },
              {
                "id": "K_N",
                "text": "\u178E"
              },
              {
                "id": "K_M",
                "text": "\uEFC6"
              },
              {
                "id": "K_COMMA",
                "text": "\uEF13"
              },
              {
                "id": "K_PERIOD",
                "text": "\u17D5"
              },
              {
                "id": "K_SLASH",
                "text": "?"
              },
              {
                "width": "10",
                "id": "T_new_390",
                "sp": "10"
              }
            ]
          },
          {
            "id": "5",
            "key": [
              {
                "nextlayer": "rightalt",
                "width": "160",
                "id": "K_LCONTROL",
                "sp": "1",
                "text": "*AltGr*"
              },
              {
                "width": "160",
                "id": "K_LOPT",
                "sp": "1",
                "text": "*Menu*"
              },
              {
                "width": "930",
                "id": "K_SPACE"
              },
              {
                "width": "160",
                "id": "K_ENTER",
                "sp": "1",
                "text": "*Enter*"
              }
            ]
          }
        ]
      }
    ]
  }
}
;
  this.s_c_key_11=['','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',''];
  this.s_c_out_12="កខគឃងចឆជឈញដឋឌឍណតថទធនបផពភមយរលវសហឡអឝឞ";
  this.s_v_gen_key_13=['','','','','','','','','','','','','','','',''];
  this.s_v_gen_14="ាិីឹឺុូួើឿៀេែៃោៅ";
  this.s_v_pseudo_key_15=['','',''];
  this.s_v_pseudo_16="ំះៈ";
  this.s_v_key_17=['','','','','','','','','','','','','','','','','','',''];
  this.s_v_out_18="ាិីឹឺុូួើឿៀេែៃោៅំះៈ";
  this.s_v_any_19="ាិីឹឺុូួើឿៀេែៃោៅំះៈ";
  this.s_v_combo_R_20="េោុិីឹែ";
  this.s_v_combo_N_21="ាុ";
  this.s_v_combo_22="េោុិីឹែាុ";
  this.s_ind_v_key_23=['','','','','','','','','','','','','','',''];
  this.s_ind_v_out_24="ឥឦឧឨឩឪឫឬឭឮឯឰឱឲឳ";
  this.s_diacritic_key_25=['','','','','','','','','','',''];
  this.s_diacritic_out_26="់័៌៏៍ៈ៎៑៝ៜ្";
  this.s_c_shifter_key_27=['',''];
  this.s_c_shifter_28="៉៊";
  this.s_punct_key_29=['','','','','','','',''];
  this.s_punct_out_30="។៕៖ៗ៘៙៚៓";
  this.s_latin_punct_key_31=['','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',''];
  this.s_latin_punct_out_32="«»()!\"%=?{}\\@*,×./[]‍‌+-÷:≈‘’;<>#&";
  this.s_spaces_key_33=['','',''];
  this.s_spaces_out_34="​  ";
  this.s_currency_key_35=['','',''];
  this.s_currency_out_36="៛$€";
  this.s_digit_key_37=['','','','','','','','','',''];
  this.s_digit_out_38="០១២៣៤៥៦៧៨៩";
  this.s_lek_attak_key_39=['','','','','','','','','',''];
  this.s_lek_attak_out_40="៰៱៲៳៴៵៶៷៸៹";
  this.s_lunar_date_key_41=['','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',''];
  this.s_lunar_date_out_42="᧬᧻᧹᧮᧢᧯᧰᧱᧧᧲᧳᧴᧽᧼᧨᧩᧠᧣᧭᧤᧦᧺᧡᧸᧥᧷᧵᧾᧿᧪᧫᧶";
  this.s_input_subcons_43=['','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',''];
  this.s_subcons_44="កខគឃងចឆជឈញដឋឌឍណតថទធនបផពភមយរលវឝឞសហឡអ";
  this.s_arabic_digit_key_45=['','','','','','','','','',''];
  this.s_arabic_digit_out_46="0123456789";
  this.s_v_above_47="ិីឹឺើ័";
  this.s_shiftable_c_1st_48="សហអ";
  this.s_shiftable_BA_49="ប";
  this.s_shiftable_c_2nd_50="ងញមយរវនល";
  this.s_shiftable_c_2nd_with_BA_51="ងញមយរវនលប";
  this.s_c_2nd_combo_LO_52="យមងបវ";
  this.s_c_2nd_combo_MO_53="យលងរ";
  this.s_c_1st_combo_LO_54="បហអ";
  this.s_c_1st_combo_MO_55="ហសអ";
  this.s_c_combo_SA_56="បយលមនញងរវអ";
  this.s_c_combo_QA_57="ឆឈបផតទ";
  this.s_c_combo_HA_58="វឣ";
  this.s62="touch";
  this.KVS=[];
  this.gs=function(t,e) {
    return this.g_main_0(t,e);
  };
  this.gs=function(t,e) {
    return this.g_main_0(t,e);
  };
  this.g_main_0=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_BKSP /* 0x08 */)) {
      if(k.KFCM(2,t,['្',{t:'a',a:this.s_c_out_12}])&&k.KIFS(31,this.s62,t)){
        r=m=1;   // Line 266
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,[{t:'a',a:this.s_v_combo_N_21},'ំ'])){
        r=m=1;   // Line 229
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,[{t:'a',a:this.s_v_combo_R_20},'ះ'])){
        r=m=1;   // Line 230
        k.KDC(2,t);
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_B /* 0x42 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ឞ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_K /* 0x4B */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ឝ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_QUOTE /* 0xDE */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ៈ");
      }
      else if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"ៈ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_E /* 0x45 */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ឯ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_I /* 0x49 */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ឦ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_O /* 0x4F */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ឱ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_P /* 0x50 */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ឰ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_R /* 0x52 */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ឫ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_T /* 0x54 */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ឨ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_LBRKT /* 0xDB */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ឩ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_RBRKT /* 0xDD */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ឳ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_3 /* 0x33 */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"៑");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_Q /* 0x51 */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"ៜ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_W /* 0x57 */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"៝");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_EQUAL /* 0xBB */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"៎");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_6 /* 0x36 */)) {
      if(1){
        r=m=1;   // Line 193
        k.KDC(0,t);
        k.KO(-1,t,"៙");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_7 /* 0x37 */)) {
      if(1){
        r=m=1;   // Line 193
        k.KDC(0,t);
        k.KO(-1,t,"៚");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_L /* 0x4C */)) {
      if(1){
        r=m=1;   // Line 193
        k.KDC(0,t);
        k.KO(-1,t,"៘");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_M /* 0x4D */)) {
      if(1){
        r=m=1;   // Line 193
        k.KDC(0,t);
        k.KO(-1,t,"៓");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_COLON /* 0xBA */)) {
      if(1){
        r=m=1;   // Line 193
        k.KDC(0,t);
        k.KO(-1,t,"៖");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_0 /* 0x30 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"}");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_1 /* 0x31 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"‌");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_2 /* 0x32 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"@");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_8 /* 0x38 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"*");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_9 /* 0x39 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"{");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_A /* 0x41 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"+");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_C /* 0x43 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"#");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_D /* 0x44 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"×");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_F /* 0x46 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"÷");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_G /* 0x47 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,":");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_H /* 0x48 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"‘");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_J /* 0x4A */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"’");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_N /* 0x4E */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,";");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_S /* 0x53 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"-");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_U /* 0x55 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"]");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_V /* 0x56 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"&");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_X /* 0x58 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,">");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_Y /* 0x59 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"[");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_Z /* 0x5A */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"<");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_COMMA /* 0xBC */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,",");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_HYPHEN /* 0xBD */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"≈");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_PERIOD /* 0xBE */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,".");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_SLASH /* 0xBF */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"/");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_BKQUOTE /* 0xC0 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"‍");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_BKSLASH /* 0xDC */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"\\");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_4 /* 0x34 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"$");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_5 /* 0x35 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"€");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_0 /* 0x30 */)) {
      if(1){
        r=m=1;   // Line 197
        k.KDC(0,t);
        k.KO(-1,t,"៰");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_1 /* 0x31 */)) {
      if(1){
        r=m=1;   // Line 197
        k.KDC(0,t);
        k.KO(-1,t,"៱");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_2 /* 0x32 */)) {
      if(1){
        r=m=1;   // Line 197
        k.KDC(0,t);
        k.KO(-1,t,"៲");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_3 /* 0x33 */)) {
      if(1){
        r=m=1;   // Line 197
        k.KDC(0,t);
        k.KO(-1,t,"៳");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_4 /* 0x34 */)) {
      if(1){
        r=m=1;   // Line 197
        k.KDC(0,t);
        k.KO(-1,t,"៴");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_5 /* 0x35 */)) {
      if(1){
        r=m=1;   // Line 197
        k.KDC(0,t);
        k.KO(-1,t,"៵");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_6 /* 0x36 */)) {
      if(1){
        r=m=1;   // Line 197
        k.KDC(0,t);
        k.KO(-1,t,"៶");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_7 /* 0x37 */)) {
      if(1){
        r=m=1;   // Line 197
        k.KDC(0,t);
        k.KO(-1,t,"៷");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_8 /* 0x38 */)) {
      if(1){
        r=m=1;   // Line 197
        k.KDC(0,t);
        k.KO(-1,t,"៸");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_9 /* 0x39 */)) {
      if(1){
        r=m=1;   // Line 197
        k.KDC(0,t);
        k.KO(-1,t,"៹");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_A /* 0x41 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧬");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_B /* 0x42 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧻");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_C /* 0x43 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧹");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_D /* 0x44 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧮");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_E /* 0x45 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧢");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_F /* 0x46 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧯");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_G /* 0x47 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧰");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_H /* 0x48 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧱");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_I /* 0x49 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧧");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_J /* 0x4A */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧲");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_K /* 0x4B */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧳");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_L /* 0x4C */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧴");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_M /* 0x4D */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧽");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_N /* 0x4E */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧼");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_O /* 0x4F */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧨");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_P /* 0x50 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧩");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_Q /* 0x51 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧠");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_R /* 0x52 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧣");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_S /* 0x53 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧭");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_T /* 0x54 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧤");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_U /* 0x55 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧦");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_V /* 0x56 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧺");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_W /* 0x57 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧡");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_X /* 0x58 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧸");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_Y /* 0x59 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧥");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_Z /* 0x5A */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧷");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_COLON /* 0xBA */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧵");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_COMMA /* 0xBC */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧾");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_PERIOD /* 0xBE */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧿");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_LBRKT /* 0xDB */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧪");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_RBRKT /* 0xDD */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧫");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_QUOTE /* 0xDE */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"᧶");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_SPACE /* 0x20 */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t," ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x100)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ក");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x101)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ខ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x102)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្គ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x103)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ឃ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x104)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ង");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x105)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ច");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x106)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ឆ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x107)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ជ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x108)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ឈ");
      }
    }
      if(m) {}
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x109)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ញ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x10A)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ដ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x10B)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ឋ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x10C)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ឌ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x10D)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ឍ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x10E)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ណ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x10F)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ត");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x110)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ថ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x111)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ទ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x112)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ធ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x113)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ន");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x114)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ប");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x115)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ផ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x116)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ព");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x117)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ភ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x118)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ម");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x119)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្យ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x11A)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្រ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x11B)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ល");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x11C)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្វ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x11D)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ឝ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x11E)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ឞ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x11F)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ស");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x120)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ហ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x121)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្ឡ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x122)) {
      if(1){
        r=m=1;   // Line 262
        k.KDC(0,t);
        k.KO(-1,t,"្អ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_NPSTAR /* 0x6A */)) {
      if(1){
        r=m=1;   // Line 268
        k.KDC(0,t);
        k.KO(-1,t,"*");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_NPSTAR /* 0x6A */)) {
      if(1){
        r=m=1;   // Line 269
        k.KDC(0,t);
        k.KO(-1,t,"*");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_NPPLUS /* 0x6B */)) {
      if(1){
        r=m=1;   // Line 270
        k.KDC(0,t);
        k.KO(-1,t,"+");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_NPPLUS /* 0x6B */)) {
      if(1){
        r=m=1;   // Line 271
        k.KDC(0,t);
        k.KO(-1,t,"+");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_NPMINUS /* 0x6D */)) {
      if(1){
        r=m=1;   // Line 272
        k.KDC(0,t);
        k.KO(-1,t,"-");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_NPMINUS /* 0x6D */)) {
      if(1){
        r=m=1;   // Line 273
        k.KDC(0,t);
        k.KO(-1,t,"-");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_NPDOT /* 0x6E */)) {
      if(1){
        r=m=1;   // Line 274
        k.KDC(0,t);
        k.KO(-1,t,".");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_NPDOT /* 0x6E */)) {
      if(1){
        r=m=1;   // Line 275
        k.KDC(0,t);
        k.KO(-1,t,".");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_NPSLASH /* 0x6F */)) {
      if(1){
        r=m=1;   // Line 276
        k.KDC(0,t);
        k.KO(-1,t,"/");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_NPSLASH /* 0x6F */)) {
      if(1){
        r=m=1;   // Line 277
        k.KDC(0,t);
        k.KO(-1,t,"/");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_SPACE /* 0x20 */)) {
      if(k.KFCM(1,t,['​'])){
        r=m=1;   // Line 225
        k.KDC(1,t);
        k.KO(-1,t," ");
      }
      else if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"​");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_SPACE /* 0x20 */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t," ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_1 /* 0x31 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"!");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_QUOTE /* 0xDE */)) {
      if(k.KFCM(3,t,[{t:'a',a:this.s_c_combo_QA_57},'្','អ'])){
        r=m=1;   // Line 244
        k.KDC(3,t);
        k.KIO(-1,this.s_c_combo_QA_57,1,t);
        k.KO(-1,t,"្អ៉");
        k.KB(t);
      }
      else if(k.KFCM(3,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_54}])){
        r=m=1;   // Line 245
        k.KDC(3,t);
        k.KO(-1,t,"ល្");
        k.KIO(-1,this.s_c_1st_combo_LO_54,3,t);
        k.KO(-1,t,"៉");
        k.KB(t);
      }
      else if(k.KFCM(3,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_55}])){
        r=m=1;   // Line 246
        k.KDC(3,t);
        k.KO(-1,t,"ម្");
        k.KIO(-1,this.s_c_1st_combo_MO_55,3,t);
        k.KO(-1,t,"៉");
        k.KB(t);
      }
      else if(k.KFCM(3,t,['ស','្',{t:'a',a:this.s_c_combo_SA_56}])){
        r=m=1;   // Line 247
        k.KDC(3,t);
        k.KO(-1,t,"ស្");
        k.KIO(-1,this.s_c_combo_SA_56,3,t);
        k.KO(-1,t,"៉");
        k.KB(t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_c_combo_HA_58},'្','ហ'])){
        r=m=1;   // Line 248
        k.KDC(3,t);
        k.KIO(-1,this.s_c_combo_HA_58,1,t);
        k.KO(-1,t,"្ហ៉");
        k.KB(t);
      }
      else if(k.KFCM(3,t,['អ','្','ង'])){
        r=m=1;   // Line 249
        k.KDC(3,t);
        k.KO(-1,t,"អ្ង៉");
        k.KB(t);
      }
      else if(k.KFCM(3,t,['អ','្','វ'])){
        r=m=1;   // Line 250
        k.KDC(3,t);
        k.KO(-1,t,"អ្វ៉");
        k.KB(t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_c_shifter_28}])){
        r=m=1;   // Line 215
        k.KDC(1,t);
        k.KIO(-1,this.s_c_shifter_28,1,t);
        k.KB(t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_shiftable_c_1st_48}])){
        r=m=1;   // Line 239
        k.KDC(1,t);
        k.KIO(-1,this.s_shiftable_c_1st_48,1,t);
        k.KO(-1,t,"៉");
        k.KB(t);
      }
      else if(1){
        r=m=1;   // Line 192
        k.KDC(0,t);
        k.KO(-1,t,"៉");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_3 /* 0x33 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"\"");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_4 /* 0x34 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"៛");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_5 /* 0x35 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"%");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_7 /* 0x37 */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"័");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_QUOTE /* 0xDE */)) {
      if(k.KFCM(2,t,['្',{t:'a',a:this.s_c_out_12}])){
        r=m=1;   // Line 214
        k.KDC(2,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_out_12,2,t);
        k.KB(t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_v_gen_14}])){
        r=m=1;   // Line 211
        k.KDC(1,t);
        k.KIO(-1,this.s_v_gen_14,1,t);
        k.KB(t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_v_pseudo_16}])){
        r=m=1;   // Line 212
        k.KDC(1,t);
        k.KIO(-1,this.s_v_pseudo_16,1,t);
        k.KB(t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_c_shifter_28}])){
        r=m=1;   // Line 213
        k.KDC(1,t);
        k.KIO(-1,this.s_c_shifter_28,1,t);
        k.KB(t);
      }
      else if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"់");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_9 /* 0x39 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"(");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_0 /* 0x30 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,")");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_8 /* 0x38 */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"៏");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_EQUAL /* 0xBB */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"=");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_COMMA /* 0xBC */)) {
      if(1){
        r=m=1;   // Line 206
        k.KDC(0,t);
        k.KO(-1,t,"ុំ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_HYPHEN /* 0xBD */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ឥ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_PERIOD /* 0xBE */)) {
      if(1){
        r=m=1;   // Line 193
        k.KDC(0,t);
        k.KO(-1,t,"។");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_SLASH /* 0xBF */)) {
      if(k.KFCM(3,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_52}])){
        r=m=1;   // Line 254
        k.KDC(3,t);
        k.KO(-1,t,"ល្");
        k.KIO(-1,this.s_c_2nd_combo_LO_52,3,t);
        k.KO(-1,t,"៊");
        k.KB(t);
      }
      else if(k.KFCM(3,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_53}])){
        r=m=1;   // Line 255
        k.KDC(3,t);
        k.KO(-1,t,"ម្");
        k.KIO(-1,this.s_c_2nd_combo_MO_53,3,t);
        k.KO(-1,t,"៊");
        k.KB(t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_c_shifter_28}])){
        r=m=1;   // Line 215
        k.KDC(1,t);
        k.KIO(-1,this.s_c_shifter_28,1,t);
        k.KB(t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_shiftable_c_2nd_with_BA_51}])){
        r=m=1;   // Line 240
        k.KDC(1,t);
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_51,1,t);
        k.KO(-1,t,"៊");
        k.KB(t);
      }
      else if(1){
        r=m=1;   // Line 192
        k.KDC(0,t);
        k.KO(-1,t,"៊");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_0 /* 0x30 */)) {
      if(1){
        r=m=1;   // Line 196
        k.KDC(0,t);
        k.KO(-1,t,"០");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_1 /* 0x31 */)) {
      if(1){
        r=m=1;   // Line 196
        k.KDC(0,t);
        k.KO(-1,t,"១");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_2 /* 0x32 */)) {
      if(1){
        r=m=1;   // Line 196
        k.KDC(0,t);
        k.KO(-1,t,"២");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_3 /* 0x33 */)) {
      if(1){
        r=m=1;   // Line 196
        k.KDC(0,t);
        k.KO(-1,t,"៣");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_4 /* 0x34 */)) {
      if(1){
        r=m=1;   // Line 196
        k.KDC(0,t);
        k.KO(-1,t,"៤");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_5 /* 0x35 */)) {
      if(1){
        r=m=1;   // Line 196
        k.KDC(0,t);
        k.KO(-1,t,"៥");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_6 /* 0x36 */)) {
      if(1){
        r=m=1;   // Line 196
        k.KDC(0,t);
        k.KO(-1,t,"៦");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_7 /* 0x37 */)) {
      if(1){
        r=m=1;   // Line 196
        k.KDC(0,t);
        k.KO(-1,t,"៧");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_8 /* 0x38 */)) {
      if(1){
        r=m=1;   // Line 196
        k.KDC(0,t);
        k.KO(-1,t,"៨");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_9 /* 0x39 */)) {
      if(1){
        r=m=1;   // Line 196
        k.KDC(0,t);
        k.KO(-1,t,"៩");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_COLON /* 0xBA */)) {
      if(1){
        r=m=1;   // Line 205
        k.KDC(0,t);
        k.KO(-1,t,"ោះ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_COLON /* 0xBA */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ើ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_COMMA /* 0xBC */)) {
      if(1){
        r=m=1;   // Line 207
        k.KDC(0,t);
        k.KO(-1,t,"ុះ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_EQUAL /* 0xBB */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ឲ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_PERIOD /* 0xBE */)) {
      if(1){
        r=m=1;   // Line 193
        k.KDC(0,t);
        k.KO(-1,t,"៕");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_SLASH /* 0xBF */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"?");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_2 /* 0x32 */)) {
      if(1){
        r=m=1;   // Line 193
        k.KDC(0,t);
        k.KO(-1,t,"ៗ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_A /* 0x41 */)) {
      if(1){
        r=m=1;   // Line 203
        k.KDC(0,t);
        k.KO(-1,t,"ាំ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_B /* 0x42 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ព");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_C /* 0x43 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ជ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_D /* 0x44 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ឌ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_E /* 0x45 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ែ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_F /* 0x46 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ធ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_G /* 0x47 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"អ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_H /* 0x48 */)) {
      if(k.KFCM(1,t,['ះ'])){
        r=m=1;   // Line 219
        k.KDC(1,t);
        k.KO(-1,t,"ៈ");
      }
      else if(k.KFCM(1,t,['ៈ'])){
        r=m=1;   // Line 220
        k.KDC(1,t);
        k.KO(-1,t,"ះ");
      }
      else if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ះ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_I /* 0x49 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ី");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_J /* 0x4A */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ញ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_K /* 0x4B */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"គ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_L /* 0x4C */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ឡ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_M /* 0x4D */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ំ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_N /* 0x4E */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ណ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_O /* 0x4F */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ៅ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_P /* 0x50 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ភ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_Q /* 0x51 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ឈ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_R /* 0x52 */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ឬ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_S /* 0x53 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ៃ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_T /* 0x54 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ទ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_U /* 0x55 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ូ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_V /* 0x56 */)) {
      if(1){
        r=m=1;   // Line 204
        k.KDC(0,t);
        k.KO(-1,t,"េះ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_W /* 0x57 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ឺ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_X /* 0x58 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ឃ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_Y /* 0x59 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ួ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_Z /* 0x5A */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ឍ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_LBRKT /* 0xDB */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ៀ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_BKSLASH /* 0xDC */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ឮ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_RBRKT /* 0xDD */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ឪ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_6 /* 0x36 */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"៍");
      }
    }
      if(m) {}
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_HYPHEN /* 0xBD */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"៌");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_BKQUOTE /* 0xC0 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"«");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_A /* 0x41 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ា");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_B /* 0x42 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ប");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_C /* 0x43 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ច");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_D /* 0x44 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ដ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_E /* 0x45 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"េ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_F /* 0x46 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ថ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_G /* 0x47 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ង");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_H /* 0x48 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ហ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_I /* 0x49 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ិ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_J /* 0x4A */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"្");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_K /* 0x4B */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ក");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_L /* 0x4C */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ល");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_M /* 0x4D */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ម");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_N /* 0x4E */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ន");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_O /* 0x4F */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ោ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_P /* 0x50 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ផ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Q /* 0x51 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ឆ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_R /* 0x52 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"រ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_S /* 0x53 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ស");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_T /* 0x54 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ត");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_U /* 0x55 */)) {
      if(k.KFCM(3,t,[{t:'a',a:this.s_shiftable_c_1st_48},'ា','ំ'])){
        r=m=1;   // Line 234
        k.KDC(3,t);
        k.KIO(-1,this.s_shiftable_c_1st_48,1,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_shiftable_c_2nd_with_BA_51},'ា','ំ'])){
        r=m=1;   // Line 235
        k.KDC(3,t);
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_51,1,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ុ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_V /* 0x56 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"វ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_W /* 0x57 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ឹ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_X /* 0x58 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ខ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Y /* 0x59 */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"យ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Z /* 0x5A */)) {
      if(1){
        r=m=1;   // Line 188
        k.KDC(0,t);
        k.KO(-1,t,"ឋ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_LBRKT /* 0xDB */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ឿ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_BKSLASH /* 0xDC */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ឭ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_RBRKT /* 0xDD */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ឧ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_BKQUOTE /* 0xC0 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"»");
      }
    }
    if(m==1) {
    
      k.KDC(-1,t);
      r=this.g_normalise_1(t,e);
      m=2;
    }
    return r;
  };
  this.g_normalise_1=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
      if(k.KFCM(7,t,[{t:'a',a:this.s_c_combo_QA_57},'្','អ','ុ','ំ','ា','ំ'])){
        m=1;   // Line 376
        k.KDC(7,t);
        k.KIO(-1,this.s_c_combo_QA_57,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_54},'ុ','ំ','ា','ំ'])){
        m=1;   // Line 381
        k.KDC(7,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_54,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_55},'ុ','ំ','ា','ំ'])){
        m=1;   // Line 386
        k.KDC(7,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_55,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,['ស','្','ប','ុ','ំ','ា','ំ'])){
        m=1;   // Line 391
        k.KDC(7,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ប៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,['ស','្',{t:'a',a:this.s_c_combo_SA_56},'ុ','ំ','ា','ំ'])){
        m=1;   // Line 396
        k.KDC(7,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_56,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,[{t:'a',a:this.s_c_combo_HA_58},'្','ហ','ុ','ំ','ា','ំ'])){
        m=1;   // Line 401
        k.KDC(7,t);
        k.KIO(-1,this.s_c_combo_HA_58,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,['អ','្','ង','ុ','ំ','ា','ំ'])){
        m=1;   // Line 406
        k.KDC(7,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,['អ','្','វ','ុ','ំ','ា','ំ'])){
        m=1;   // Line 411
        k.KDC(7,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,['ហ','្','ប','ុ','ំ','ា','ំ'])){
        m=1;   // Line 416
        k.KDC(7,t);
        k.KO(-1,t,"ហ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ប៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,['ហ','្',{t:'a',a:this.s_shiftable_c_2nd_with_BA_51},'ុ','ំ','ា','ំ'])){
        m=1;   // Line 422
        k.KDC(7,t);
        k.KO(-1,t,"ហ");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_51,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_52},'ុ','ំ','ា','ំ'])){
        m=1;   // Line 429
        k.KDC(7,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_52,3,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_53},'ុ','ំ','ា','ំ'])){
        m=1;   // Line 434
        k.KDC(7,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_53,3,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['្','ដ',{t:'a',a:this.s_v_combo_N_21},'ំ','្','រ'])){
        m=1;   // Line 340
        k.KDC(6,t);
        k.KO(-1,t,"្ត");
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
        k.KIO(-1,this.s_v_combo_N_21,3,t);
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['្','ដ',{t:'a',a:this.s_v_combo_R_20},'ះ','្','រ'])){
        m=1;   // Line 341
        k.KDC(6,t);
        k.KO(-1,t,"្ត");
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
        k.KIO(-1,this.s_v_combo_R_20,3,t);
        k.KO(-1,t,"ះ");
      }
      else if(k.KFCM(6,t,['្','រ',{t:'a',a:this.s_v_combo_N_21},'ំ','្','ដ'])){
        m=1;   // Line 344
        k.KDC(6,t);
        k.KO(-1,t,"្ត");
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
        k.KIO(-1,this.s_v_combo_N_21,3,t);
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['្','រ',{t:'a',a:this.s_v_combo_R_20},'ះ','្','ដ'])){
        m=1;   // Line 345
        k.KDC(6,t);
        k.KO(-1,t,"្ត");
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
        k.KIO(-1,this.s_v_combo_R_20,3,t);
        k.KO(-1,t,"ះ");
      }
      else if(k.KFCM(6,t,['្','រ',{t:'a',a:this.s_v_combo_N_21},'ំ','្',{t:'a',a:this.s_subcons_44}])){
        m=1;   // Line 350
        k.KDC(6,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_44,6,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
        k.KIO(-1,this.s_v_combo_N_21,3,t);
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['្','រ',{t:'a',a:this.s_v_combo_R_20},'ះ','្',{t:'a',a:this.s_subcons_44}])){
        m=1;   // Line 351
        k.KDC(6,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_44,6,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
        k.KIO(-1,this.s_v_combo_R_20,3,t);
        k.KO(-1,t,"ះ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_QA_57},'្','អ','ុ','ា','ំ'])){
        m=1;   // Line 374
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_QA_57,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_54},'ុ','ា','ំ'])){
        m=1;   // Line 379
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_54,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_55},'ុ','ា','ំ'])){
        m=1;   // Line 384
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_55,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ស','្','ប','ុ','ា','ំ'])){
        m=1;   // Line 389
        k.KDC(6,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ប៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ស','្',{t:'a',a:this.s_c_combo_SA_56},'ុ','ា','ំ'])){
        m=1;   // Line 394
        k.KDC(6,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_56,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_HA_58},'្','ហ','ុ','ា','ំ'])){
        m=1;   // Line 399
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_HA_58,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['អ','្','ង','ុ','ា','ំ'])){
        m=1;   // Line 404
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['អ','្','វ','ុ','ា','ំ'])){
        m=1;   // Line 409
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ហ','្','ប','ុ','ា','ំ'])){
        m=1;   // Line 414
        k.KDC(6,t);
        k.KO(-1,t,"ហ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ប៉");
        k.KO(-1,t,"ប");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ហ','្',{t:'a',a:this.s_shiftable_c_2nd_with_BA_51},'ុ','ា','ំ'])){
        m=1;   // Line 420
        k.KDC(6,t);
        k.KO(-1,t,"ហ");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_51,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_51,3,t);
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_52},'ុ','ា','ំ'])){
        m=1;   // Line 427
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_52,3,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_53},'ុ','ា','ំ'])){
        m=1;   // Line 432
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_53,3,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_52},'៊',{t:'a',a:this.s_v_gen_14},{t:'a',a:this.s_v_pseudo_16}])){
        m=1;   // Line 454
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_52,3,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_gen_14,5,t);
        k.KIO(-1,this.s_v_pseudo_16,6,t);
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_53},'៊',{t:'a',a:this.s_v_gen_14},{t:'a',a:this.s_v_pseudo_16}])){
        m=1;   // Line 455
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_53,3,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_gen_14,5,t);
        k.KIO(-1,this.s_v_pseudo_16,6,t);
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_QA_57},'្','អ','៉','ា','ំ'])){
        m=1;   // Line 489
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_QA_57,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_54},'៉','ា','ំ'])){
        m=1;   // Line 490
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_54,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_55},'៉','ា','ំ'])){
        m=1;   // Line 491
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_55,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ស','្','ប','៉','ា','ំ'])){
        m=1;   // Line 492
        k.KDC(6,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ប៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ស','្',{t:'a',a:this.s_c_combo_SA_56},'៉','ា','ំ'])){
        m=1;   // Line 493
        k.KDC(6,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_56,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_HA_58},'្','ហ','៉','ា','ំ'])){
        m=1;   // Line 494
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_HA_58,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['អ','្','ង','៉','ា','ំ'])){
        m=1;   // Line 495
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['អ','្','វ','៉','ា','ំ'])){
        m=1;   // Line 496
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_QA_57},'្','អ','ា','ុ','ំ'])){
        m=1;   // Line 503
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_QA_57,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_QA_57},'្','អ','ុ','ំ','ា'])){
        m=1;   // Line 504
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_QA_57,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_54},'ា','ុ','ំ'])){
        m=1;   // Line 506
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_54,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_54},'ុ','ំ','ា'])){
        m=1;   // Line 507
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_54,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_55},'ា','ុ','ំ'])){
        m=1;   // Line 509
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_55,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_55},'ុ','ំ','ា'])){
        m=1;   // Line 510
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_55,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ស','្',{t:'a',a:this.s_c_combo_SA_56},'ា','ុ','ំ'])){
        m=1;   // Line 512
        k.KDC(6,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_56,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ស','្',{t:'a',a:this.s_c_combo_SA_56},'ុ','ំ','ា'])){
        m=1;   // Line 513
        k.KDC(6,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_56,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_HA_58},'្','ហ','ា','ុ','ំ'])){
        m=1;   // Line 515
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_HA_58,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_HA_58},'្','ហ','ុ','ំ','ា'])){
        m=1;   // Line 516
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_HA_58,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['អ','្','ង','ា','ុ','ំ'])){
        m=1;   // Line 518
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['អ','្','ង','ុ','ំ','ា'])){
        m=1;   // Line 519
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['អ','្','វ','ា','ុ','ំ'])){
        m=1;   // Line 521
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['អ','្','វ','ុ','ំ','ា'])){
        m=1;   // Line 522
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊");
        k.KO(-1,t,"ុ");
        k.KO(-1,t,"ា");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_52},'ា','ុ','ំ'])){
        m=1;   // Line 529
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_52,3,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_52},'ុ','ំ','ា'])){
        m=1;   // Line 530
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_52,3,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_53},'ា','ុ','ំ'])){
        m=1;   // Line 532
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_53,3,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_53},'ុ','ំ','ា'])){
        m=1;   // Line 533
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_53,3,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_QA_57},'្','អ','េ','ុ','ី'])){
        m=1;   // Line 541
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_QA_57,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊ើ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_QA_57},'្','អ','ុ','េ','ី'])){
        m=1;   // Line 542
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_QA_57,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊ើ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_QA_57},'្','អ','៉','េ','ី'])){
        m=1;   // Line 543
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_QA_57,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊ើ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_54},'េ','ុ','ី'])){
        m=1;   // Line 545
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_54,3,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_54},'ុ','េ','ី'])){
        m=1;   // Line 546
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_54,3,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_54},'៉','េ','ី'])){
        m=1;   // Line 547
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_54,3,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_55},'េ','ុ','ី'])){
        m=1;   // Line 549
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_55,3,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_55},'ុ','េ','ី'])){
        m=1;   // Line 550
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_55,3,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_55},'៉','េ','ី'])){
        m=1;   // Line 551
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_55,3,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(6,t,['ស','្',{t:'a',a:this.s_c_combo_SA_56},'េ','ុ','ី'])){
        m=1;   // Line 553
        k.KDC(6,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_56,3,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(6,t,['ស','្',{t:'a',a:this.s_c_combo_SA_56},'ុ','េ','ី'])){
        m=1;   // Line 554
        k.KDC(6,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_56,3,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(6,t,['ស','្',{t:'a',a:this.s_c_combo_SA_56},'៉','េ','ី'])){
        m=1;   // Line 555
        k.KDC(6,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_56,3,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_HA_58},'្','ហ','េ','ុ','ី'])){
        m=1;   // Line 557
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_HA_58,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊ើ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_HA_58},'្','ហ','ុ','េ','ី'])){
        m=1;   // Line 558
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_HA_58,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊ើ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_HA_58},'្','ហ','៉','េ','ី'])){
        m=1;   // Line 559
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_HA_58,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊ើ");
      }
      else if(k.KFCM(6,t,['អ','្','ង','េ','ុ','ី'])){
        m=1;   // Line 561
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊ើ");
      }
      else if(k.KFCM(6,t,['អ','្','ង','ុ','េ','ី'])){
        m=1;   // Line 562
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊ើ");
      }
      else if(k.KFCM(6,t,['អ','្','ង','៉','េ','ី'])){
        m=1;   // Line 563
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊ើ");
      }
      else if(k.KFCM(6,t,['អ','្','វ','េ','ុ','ី'])){
        m=1;   // Line 565
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊ើ");
      }
      else if(k.KFCM(6,t,['អ','្','វ','ុ','េ','ី'])){
        m=1;   // Line 566
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊ើ");
      }
      else if(k.KFCM(6,t,['អ','្','វ','៉','េ','ី'])){
        m=1;   // Line 567
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊ើ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_52},'េ','ុ','ី'])){
        m=1;   // Line 575
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_52,3,t);
        k.KO(-1,t,"៉ើ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_52},'ុ','េ','ី'])){
        m=1;   // Line 576
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_52,3,t);
        k.KO(-1,t,"៉ើ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_52},'៊','េ','ី'])){
        m=1;   // Line 577
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_52,3,t);
        k.KO(-1,t,"៉ើ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_53},'េ','ុ','ី'])){
        m=1;   // Line 579
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_53,3,t);
        k.KO(-1,t,"៉ើ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_53},'ុ','េ','ី'])){
        m=1;   // Line 580
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_53,3,t);
        k.KO(-1,t,"៉ើ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_53},'៊','េ','ី'])){
        m=1;   // Line 581
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_53,3,t);
        k.KO(-1,t,"៉ើ");
      }
      else if(k.KFCM(6,t,['្','យ','្',{t:'a',a:this.s_c_out_12},'េ','ឺ'])){
        m=1;   // Line 631
        k.KDC(6,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_out_12,4,t);
        k.KO(-1,t,"ឿ");
      }
      else if(k.KFCM(6,t,['្','យ','្',{t:'a',a:this.s_c_out_12},'េ','ឹ'])){
        m=1;   // Line 632
        k.KDC(6,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_out_12,4,t);
        k.KO(-1,t,"ឿ");
      }
      else if(k.KFCM(6,t,['្','យ','្',{t:'a',a:this.s_c_out_12},'េ','ី'])){
        m=1;   // Line 633
        k.KDC(6,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_out_12,4,t);
        k.KO(-1,t,"ឿ");
      }
      else if(k.KFCM(5,t,[{t:'a',a:this.s_c_shifter_28},{t:'a',a:this.s_v_combo_N_21},'ំ','្',{t:'a',a:this.s_subcons_44}])){
        m=1;   // Line 331
        k.KDC(5,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_44,5,t);
        k.KIO(-1,this.s_c_shifter_28,1,t);
        k.KIO(-1,this.s_v_combo_N_21,2,t);
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(5,t,[{t:'a',a:this.s_c_shifter_28},{t:'a',a:this.s_v_combo_R_20},'ះ','្',{t:'a',a:this.s_subcons_44}])){
        m=1;   // Line 332
        k.KDC(5,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_44,5,t);
        k.KIO(-1,this.s_c_shifter_28,1,t);
        k.KIO(-1,this.s_v_combo_R_20,2,t);
        k.KO(-1,t,"ះ");
      }
      else if(k.KFCM(5,t,['្','ដ',{t:'a',a:this.s_v_any_19},'្','រ'])){
        m=1;   // Line 339
        k.KDC(5,t);
        k.KO(-1,t,"្ត");
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
        k.KIO(-1,this.s_v_any_19,3,t);
      }
      else if(k.KFCM(5,t,['្','រ',{t:'a',a:this.s_v_any_19},'្','ដ'])){
        m=1;   // Line 343
        k.KDC(5,t);
        k.KO(-1,t,"្ត");
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
        k.KIO(-1,this.s_v_any_19,3,t);
      }
      else if(k.KFCM(5,t,['្','រ',{t:'a',a:this.s_c_shifter_28},'្',{t:'a',a:this.s_subcons_44}])){
        m=1;   // Line 347
        k.KDC(5,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_44,5,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
        k.KIO(-1,this.s_c_shifter_28,3,t);
      }
      else if(k.KFCM(5,t,['្','រ',{t:'a',a:this.s_v_any_19},'្',{t:'a',a:this.s_subcons_44}])){
        m=1;   // Line 349
        k.KDC(5,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_44,5,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
        k.KIO(-1,this.s_v_any_19,3,t);
      }
      else if(k.KFCM(5,t,[{t:'a',a:this.s_c_combo_QA_57},'្','អ','ុ',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 373
        k.KDC(5,t);
        k.KIO(-1,this.s_c_combo_QA_57,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,[{t:'a',a:this.s_c_combo_QA_57},'្','អ',{t:'a',a:this.s_v_above_47},'ុ'])){
        m=1;   // Line 375
        k.KDC(5,t);
        k.KIO(-1,this.s_c_combo_QA_57,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊");
        k.KIO(-1,this.s_v_above_47,4,t);
      }
      else if(k.KFCM(5,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_54},'ុ',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 378
        k.KDC(5,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_54,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_54},{t:'a',a:this.s_v_above_47},'ុ'])){
        m=1;   // Line 380
        k.KDC(5,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_54,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_47,4,t);
      }
      else if(k.KFCM(5,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_55},'ុ',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 383
        k.KDC(5,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_55,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_55},{t:'a',a:this.s_v_above_47},'ុ'])){
        m=1;   // Line 385
        k.KDC(5,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_55,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_47,4,t);
      }
      if(m) {}
      else if(k.KFCM(5,t,['ស','្','ប','ុ',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 388
        k.KDC(5,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ប៉");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,['ស','្','ប',{t:'a',a:this.s_v_above_47},'ុ'])){
        m=1;   // Line 390
        k.KDC(5,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ប៉");
        k.KIO(-1,this.s_v_above_47,4,t);
      }
      else if(k.KFCM(5,t,['ស','្',{t:'a',a:this.s_c_combo_SA_56},'ុ',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 393
        k.KDC(5,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_56,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,['ស','្',{t:'a',a:this.s_c_combo_SA_56},{t:'a',a:this.s_v_above_47},'ុ'])){
        m=1;   // Line 395
        k.KDC(5,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_56,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_47,4,t);
      }
      else if(k.KFCM(5,t,[{t:'a',a:this.s_c_combo_HA_58},'្','ហ','ុ',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 398
        k.KDC(5,t);
        k.KIO(-1,this.s_c_combo_HA_58,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,[{t:'a',a:this.s_c_combo_HA_58},'្','ហ',{t:'a',a:this.s_v_above_47},'ុ'])){
        m=1;   // Line 400
        k.KDC(5,t);
        k.KIO(-1,this.s_c_combo_HA_58,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊");
        k.KIO(-1,this.s_v_above_47,4,t);
      }
      else if(k.KFCM(5,t,['អ','្','ង','ុ',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 403
        k.KDC(5,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,['អ','្','ង',{t:'a',a:this.s_v_above_47},'ុ'])){
        m=1;   // Line 405
        k.KDC(5,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊");
        k.KIO(-1,this.s_v_above_47,4,t);
      }
      else if(k.KFCM(5,t,['អ','្','វ','ុ',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 408
        k.KDC(5,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,['អ','្','វ',{t:'a',a:this.s_v_above_47},'ុ'])){
        m=1;   // Line 410
        k.KDC(5,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊");
        k.KIO(-1,this.s_v_above_47,4,t);
      }
      else if(k.KFCM(5,t,['ហ','្','ប','ុ',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 413
        k.KDC(5,t);
        k.KO(-1,t,"ហ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ប៉");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,['ហ','្','ប',{t:'a',a:this.s_v_above_47},'ុ'])){
        m=1;   // Line 415
        k.KDC(5,t);
        k.KO(-1,t,"ហ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ប៉");
        k.KIO(-1,this.s_v_above_47,4,t);
      }
      else if(k.KFCM(5,t,['ហ','្',{t:'a',a:this.s_shiftable_c_2nd_with_BA_51},'ុ',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 419
        k.KDC(5,t);
        k.KO(-1,t,"ហ");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_51,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,['ហ','្',{t:'a',a:this.s_shiftable_c_2nd_with_BA_51},{t:'a',a:this.s_v_above_47},'ុ'])){
        m=1;   // Line 421
        k.KDC(5,t);
        k.KO(-1,t,"ហ");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_51,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_47,4,t);
      }
      else if(k.KFCM(5,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_52},'ុ',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 426
        k.KDC(5,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_52,3,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_52},{t:'a',a:this.s_v_above_47},'ុ'])){
        m=1;   // Line 428
        k.KDC(5,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_52,3,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_above_47,4,t);
      }
      else if(k.KFCM(5,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_53},'ុ',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 431
        k.KDC(5,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_53,3,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_53},{t:'a',a:this.s_v_above_47},'ុ'])){
        m=1;   // Line 433
        k.KDC(5,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_53,3,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_above_47,4,t);
      }
      else if(k.KFCM(5,t,[{t:'a',a:this.s_shiftable_c_1st_48},'ុ','ំ','ា','ំ'])){
        m=1;   // Line 441
        k.KDC(5,t);
        k.KIO(-1,this.s_shiftable_c_1st_48,1,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(5,t,[{t:'a',a:this.s_shiftable_c_2nd_with_BA_51},'ុ','ំ','ា','ំ'])){
        m=1;   // Line 448
        k.KDC(5,t);
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_51,1,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(5,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_52},'៊',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 452
        k.KDC(5,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_52,3,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_53},'៊',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 453
        k.KDC(5,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_53,3,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,['្',{t:'a',a:this.s_shiftable_c_2nd_50},'៊',{t:'a',a:this.s_v_gen_14},{t:'a',a:this.s_v_pseudo_16}])){
        m=1;   // Line 463
        k.KDC(5,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_shiftable_c_2nd_50,2,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_gen_14,4,t);
        k.KIO(-1,this.s_v_pseudo_16,5,t);
      }
      else if(k.KFCM(5,t,[{t:'a',a:this.s_c_combo_QA_57},'្','អ','៉',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 481
        k.KDC(5,t);
        k.KIO(-1,this.s_c_combo_QA_57,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_54},'៉',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 482
        k.KDC(5,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_54,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_55},'៉',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 483
        k.KDC(5,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_55,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,['ស','្','ប','៉',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 484
        k.KDC(5,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ប៉");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,['ស','្',{t:'a',a:this.s_c_combo_SA_56},'៉',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 485
        k.KDC(5,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_56,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,[{t:'a',a:this.s_c_combo_HA_58},'្','ហ','៉',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 486
        k.KDC(5,t);
        k.KIO(-1,this.s_c_combo_HA_58,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,['អ','្','ង','៉',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 487
        k.KDC(5,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,['អ','្','វ','៉',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 488
        k.KDC(5,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊");
        k.KIO(-1,this.s_v_above_47,5,t);
      }
      else if(k.KFCM(5,t,['ព','័','ន','្','ឋ'])){
        m=1;   // Line 619
        k.KDC(5,t);
        k.KO(-1,t,"ព");
        k.KO(-1,t,"័");
        k.KO(-1,t,"ន");
        k.KO(-1,t,"្ធ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_v_gen_14},{t:'a',a:this.s_v_pseudo_16},{t:'a',a:this.s_v_gen_14},{t:'a',a:this.s_v_pseudo_16}])){
        m=1;   // Line 312
        k.KDC(4,t);
        k.KIO(-1,this.s_v_gen_14,3,t);
        k.KIO(-1,this.s_v_pseudo_16,4,t);
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_v_combo_N_21},'ំ','្',{t:'a',a:this.s_subcons_44}])){
        m=1;   // Line 325
        k.KDC(4,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_44,4,t);
        k.KIO(-1,this.s_v_combo_N_21,1,t);
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_v_combo_R_20},'ះ','្',{t:'a',a:this.s_subcons_44}])){
        m=1;   // Line 326
        k.KDC(4,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_44,4,t);
        k.KIO(-1,this.s_v_combo_R_20,1,t);
        k.KO(-1,t,"ះ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_c_shifter_28},{t:'a',a:this.s_v_any_19},'្',{t:'a',a:this.s_subcons_44}])){
        m=1;   // Line 330
        k.KDC(4,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_44,4,t);
        k.KIO(-1,this.s_c_shifter_28,1,t);
        k.KIO(-1,this.s_v_any_19,2,t);
      }
      else if(k.KFCM(4,t,['្','ដ','្','រ'])){
        m=1;   // Line 336
        k.KDC(4,t);
        k.KO(-1,t,"្ត");
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
      }
      else if(k.KFCM(4,t,['្','រ','្','ដ'])){
        m=1;   // Line 337
        k.KDC(4,t);
        k.KO(-1,t,"្ត");
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
      }
      else if(k.KFCM(4,t,['្','រ','្',{t:'a',a:this.s_subcons_44}])){
        m=1;   // Line 348
        k.KDC(4,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_44,4,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_c_shifter_28},{t:'a',a:this.s_v_gen_14},{t:'a',a:this.s_v_pseudo_16},{t:'a',a:this.s_c_shifter_28}])){
        m=1;   // Line 362
        k.KDC(4,t);
        k.KIO(-1,this.s_v_gen_14,2,t);
        k.KIO(-1,this.s_v_pseudo_16,3,t);
        k.KIO(-1,this.s_c_shifter_28,4,t);
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_1st_48},'ុ','ា','ំ'])){
        m=1;   // Line 439
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_1st_48,1,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_2nd_with_BA_51},'ុ','ា','ំ'])){
        m=1;   // Line 446
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_51,1,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_2nd_50},'៊',{t:'a',a:this.s_v_gen_14},{t:'a',a:this.s_v_pseudo_16}])){
        m=1;   // Line 460
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_2nd_50,1,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_gen_14,3,t);
        k.KIO(-1,this.s_v_pseudo_16,4,t);
      }
      else if(k.KFCM(4,t,['្',{t:'a',a:this.s_shiftable_c_2nd_50},'៊',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 462
        k.KDC(4,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_shiftable_c_2nd_50,2,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_47,4,t);
      }
      else if(k.KFCM(4,t,['ប','្','យ',{t:'a',a:this.s_c_shifter_28}])){
        m=1;   // Line 467
        k.KDC(4,t);
        k.KO(-1,t,"ប្យ");
        k.KIO(-1,this.s_c_shifter_28,4,t);
      }
      else if(k.KFCM(4,t,['ស','្','ប',{t:'a',a:this.s_c_shifter_28}])){
        m=1;   // Line 468
        k.KDC(4,t);
        k.KO(-1,t,"ស្ប");
        k.KIO(-1,this.s_c_shifter_28,4,t);
      }
      else if(k.KFCM(4,t,['ឆ','្','ប',{t:'a',a:this.s_c_shifter_28}])){
        m=1;   // Line 469
        k.KDC(4,t);
        k.KO(-1,t,"ឆ្ប");
        k.KIO(-1,this.s_c_shifter_28,4,t);
      }
      else if(k.KFCM(4,t,['ប','្','យ',{t:'a',a:this.s_c_shifter_28}])){
        m=1;   // Line 470
        k.KDC(4,t);
        k.KO(-1,t,"ប្យ");
        k.KIO(-1,this.s_c_shifter_28,4,t);
      }
      else if(k.KFCM(4,t,['ស','្','ប',{t:'a',a:this.s_c_shifter_28}])){
        m=1;   // Line 471
        k.KDC(4,t);
        k.KO(-1,t,"ស្ប");
        k.KIO(-1,this.s_c_shifter_28,4,t);
      }
      else if(k.KFCM(4,t,['ឆ','្','ប',{t:'a',a:this.s_c_shifter_28}])){
        m=1;   // Line 472
        k.KDC(4,t);
        k.KO(-1,t,"ឆ្ប");
        k.KIO(-1,this.s_c_shifter_28,4,t);
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_1st_48},'៉',{t:'a',a:this.s_v_gen_14},{t:'a',a:this.s_v_pseudo_16}])){
        m=1;   // Line 477
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_1st_48,1,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_gen_14,3,t);
        k.KIO(-1,this.s_v_pseudo_16,4,t);
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_1st_48},'ា','ុ','ំ'])){
        m=1;   // Line 500
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_1st_48,1,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_1st_48},'ុ','ំ','ា'])){
        m=1;   // Line 501
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_1st_48,1,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_2nd_with_BA_51},'ា','ុ','ំ'])){
        m=1;   // Line 526
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_51,1,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_2nd_with_BA_51},'ុ','ំ','ា'])){
        m=1;   // Line 527
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_51,1,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_1st_48},'េ','ុ','ី'])){
        m=1;   // Line 537
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_1st_48,1,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_1st_48},'ុ','េ','ី'])){
        m=1;   // Line 538
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_1st_48,1,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_1st_48},'៉','េ','ី'])){
        m=1;   // Line 539
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_1st_48,1,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_2nd_50},'េ','ុ','ី'])){
        m=1;   // Line 571
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_2nd_50,1,t);
        k.KO(-1,t,"៉ើ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_2nd_50},'ុ','េ','ី'])){
        m=1;   // Line 572
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_2nd_50,1,t);
        k.KO(-1,t,"៉ើ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_2nd_50},'៊','េ','ី'])){
        m=1;   // Line 573
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_2nd_50,1,t);
        k.KO(-1,t,"៉ើ");
      }
      else if(k.KFCM(4,t,['ព','ន','្','ឋ'])){
        m=1;   // Line 618
        k.KDC(4,t);
        k.KO(-1,t,"ព");
        k.KO(-1,t,"ន");
        k.KO(-1,t,"្ធ");
      }
      else if(k.KFCM(4,t,['្','យ','េ','ឺ'])){
        m=1;   // Line 627
        k.KDC(4,t);
        k.KO(-1,t,"ឿ");
      }
      else if(k.KFCM(4,t,['្','យ','េ','ឹ'])){
        m=1;   // Line 628
        k.KDC(4,t);
        k.KO(-1,t,"ឿ");
      }
      else if(k.KFCM(4,t,['្','យ','េ','ី'])){
        m=1;   // Line 629
        k.KDC(4,t);
        k.KO(-1,t,"ឿ");
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_v_gen_14},{t:'a',a:this.s_v_pseudo_16},{t:'a',a:this.s_v_gen_14}])){
        m=1;   // Line 308
        k.KDC(3,t);
        k.KIO(-1,this.s_v_gen_14,3,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_v_gen_14},{t:'a',a:this.s_v_pseudo_16},{t:'a',a:this.s_v_pseudo_16}])){
        m=1;   // Line 309
        k.KDC(3,t);
        k.KIO(-1,this.s_v_pseudo_16,3,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_v_gen_14},{t:'a',a:this.s_v_gen_14},{t:'a',a:this.s_v_pseudo_16}])){
        m=1;   // Line 310
        k.KDC(3,t);
        k.KIO(-1,this.s_v_gen_14,2,t);
        k.KIO(-1,this.s_v_pseudo_16,3,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_v_pseudo_16},{t:'a',a:this.s_v_gen_14},{t:'a',a:this.s_v_pseudo_16}])){
        m=1;   // Line 311
        k.KDC(3,t);
        k.KIO(-1,this.s_v_gen_14,2,t);
        k.KIO(-1,this.s_v_pseudo_16,3,t);
      }
      else if(k.KFCM(3,t,['្',{t:'a',a:this.s_v_gen_14},{t:'a',a:this.s_v_pseudo_16}])){
        m=1;   // Line 320
        k.KDC(3,t);
        k.KIO(-1,this.s_v_gen_14,2,t);
        k.KIO(-1,this.s_v_pseudo_16,3,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_v_any_19},'្',{t:'a',a:this.s_subcons_44}])){
        m=1;   // Line 324
        k.KDC(3,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_44,3,t);
        k.KIO(-1,this.s_v_any_19,1,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_c_shifter_28},'្',{t:'a',a:this.s_subcons_44}])){
        m=1;   // Line 355
        k.KDC(3,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_44,3,t);
        k.KIO(-1,this.s_c_shifter_28,1,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_v_gen_14},{t:'a',a:this.s_v_pseudo_16},{t:'a',a:this.s_c_shifter_28}])){
        m=1;   // Line 360
        k.KDC(3,t);
        k.KIO(-1,this.s_c_shifter_28,3,t);
        k.KIO(-1,this.s_v_gen_14,1,t);
        k.KIO(-1,this.s_v_pseudo_16,2,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_c_shifter_28},{t:'a',a:this.s_v_any_19},{t:'a',a:this.s_c_shifter_28}])){
        m=1;   // Line 361
        k.KDC(3,t);
        k.KIO(-1,this.s_c_shifter_28,3,t);
        k.KIO(-1,this.s_v_any_19,2,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_shiftable_c_1st_48},'ុ',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 438
        k.KDC(3,t);
        k.KIO(-1,this.s_shiftable_c_1st_48,1,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_47,3,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_shiftable_c_1st_48},{t:'a',a:this.s_v_above_47},'ុ'])){
        m=1;   // Line 440
        k.KDC(3,t);
        k.KIO(-1,this.s_shiftable_c_1st_48,1,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_47,2,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_shiftable_c_2nd_with_BA_51},'ុ',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 445
        k.KDC(3,t);
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_51,1,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_above_47,3,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_shiftable_c_2nd_with_BA_51},{t:'a',a:this.s_v_above_47},'ុ'])){
        m=1;   // Line 447
        k.KDC(3,t);
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_51,1,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_above_47,2,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_shiftable_c_2nd_50},'៊',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 459
        k.KDC(3,t);
        k.KIO(-1,this.s_shiftable_c_2nd_50,1,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_above_47,3,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_shiftable_c_1st_48},'៉',{t:'a',a:this.s_v_above_47}])){
        m=1;   // Line 476
        k.KDC(3,t);
        k.KIO(-1,this.s_shiftable_c_1st_48,1,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_47,3,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_c_out_12},{t:'a',a:this.s_v_gen_14},'៌'])){
        m=1;   // Line 585
        k.KDC(3,t);
        k.KIO(-1,this.s_c_out_12,1,t);
        k.KO(-1,t,"៌");
        k.KIO(-1,this.s_v_gen_14,2,t);
      }
      else if(k.KFCM(3,t,['ណ','្','ត'])){
        m=1;   // Line 589
        k.KDC(3,t);
        k.KO(-1,t,"ណ");
        k.KO(-1,t,"្ដ");
      }
      else if(k.KFCM(3,t,['ន','្','ដ'])){
        m=1;   // Line 590
        k.KDC(3,t);
        k.KO(-1,t,"ន");
        k.KO(-1,t,"្ត");
      }
      else if(k.KFCM(3,t,['ទ','្','ប'])){
        m=1;   // Line 594
        k.KDC(3,t);
        k.KO(-1,t,"ឡ");
      }
      else if(k.KFCM(3,t,['ប','្','ញ'])){
        m=1;   // Line 596
        k.KDC(3,t);
        k.KO(-1,t,"ឫ");
      }
      else if(k.KFCM(3,t,['ព','្','ញ'])){
        m=1;   // Line 602
        k.KDC(3,t);
        k.KO(-1,t,"ឭ");
      }
      else if(k.KFCM(3,t,['ព','្','ឋ'])){
        m=1;   // Line 605
        k.KDC(3,t);
        k.KO(-1,t,"ឰ");
      }
      else if(k.KFCM(3,t,['ដ','្','ធ'])){
        m=1;   // Line 613
        k.KDC(3,t);
        k.KO(-1,t,"ដ្ឋ");
      }
      else if(k.KFCM(3,t,['ទ','្','ឋ'])){
        m=1;   // Line 614
        k.KDC(3,t);
        k.KO(-1,t,"ទ្ធ");
      }
      else if(k.KFCM(3,t,['ឪ','្','យ'])){
        m=1;   // Line 622
        k.KDC(3,t);
        k.KO(-1,t,"ឱ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"យ");
      }
      else if(k.KFCM(3,t,['ឳ','្','យ'])){
        m=1;   // Line 623
        k.KDC(3,t);
        k.KO(-1,t,"ឱ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"យ");
      }
      else if(k.KFCM(3,t,['ញ','្','វ'])){
        m=1;   // Line 625
        k.KDC(3,t);
        k.KO(-1,t,"ព");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វា");
      }
      else if(k.KFCM(2,t,['េ','ា'])){
        m=1;   // Line 291
        k.KDC(2,t);
        k.KO(-1,t,"ោ");
      }
      else if(k.KFCM(2,t,['ា','េ'])){
        m=1;   // Line 292
        k.KDC(2,t);
        k.KO(-1,t,"ោ");
      }
      else if(k.KFCM(2,t,['េ','ី'])){
        m=1;   // Line 293
        k.KDC(2,t);
        k.KO(-1,t,"ើ");
      }
      else if(k.KFCM(2,t,['ី','េ'])){
        m=1;   // Line 294
        k.KDC(2,t);
        k.KO(-1,t,"ើ");
      }
      else if(k.KFCM(2,t,['ំ','ុ'])){
        m=1;   // Line 298
        k.KDC(2,t);
        k.KO(-1,t,"ុំ");
      }
      else if(k.KFCM(2,t,['ំ','ា'])){
        m=1;   // Line 299
        k.KDC(2,t);
        k.KO(-1,t,"ាំ");
      }
      else if(k.KFCM(2,t,[{t:'a',a:this.s_v_gen_14},{t:'a',a:this.s_v_gen_14}])){
        m=1;   // Line 304
        k.KDC(2,t);
        k.KIO(-1,this.s_v_gen_14,2,t);
      }
      else if(k.KFCM(2,t,[{t:'a',a:this.s_v_pseudo_16},{t:'a',a:this.s_v_pseudo_16}])){
        m=1;   // Line 313
        k.KDC(2,t);
        k.KIO(-1,this.s_v_pseudo_16,2,t);
      }
      if(m) {}
      else if(k.KFCM(2,t,['្','្'])){
        m=1;   // Line 318
        k.KDC(2,t);
        k.KO(-1,t,"្");
      }
      else if(k.KFCM(2,t,['្',{t:'a',a:this.s_v_any_19}])){
        m=1;   // Line 319
        k.KDC(2,t);
        k.KIO(-1,this.s_v_any_19,2,t);
      }
      else if(k.KFCM(2,t,[{t:'a',a:this.s_v_any_19},{t:'a',a:this.s_c_shifter_28}])){
        m=1;   // Line 359
        k.KDC(2,t);
        k.KIO(-1,this.s_c_shifter_28,2,t);
        k.KIO(-1,this.s_v_any_19,1,t);
      }
      else if(k.KFCM(2,t,['ឫ','ុ'])){
        m=1;   // Line 597
        k.KDC(2,t);
        k.KO(-1,t,"ឬ");
      }
      else if(k.KFCM(2,t,['ឭ','ា'])){
        m=1;   // Line 599
        k.KDC(2,t);
        k.KO(-1,t,"ញ");
      }
      else if(k.KFCM(2,t,['ឮ','ា'])){
        m=1;   // Line 600
        k.KDC(2,t);
        k.KO(-1,t,"ញ");
      }
      else if(k.KFCM(2,t,['ឭ','ុ'])){
        m=1;   // Line 603
        k.KDC(2,t);
        k.KO(-1,t,"ឮ");
      }
      else if(k.KFCM(2,t,['ឧ','ិ'])){
        m=1;   // Line 607
        k.KDC(2,t);
        k.KO(-1,t,"ឱ");
      }
      else if(k.KFCM(2,t,['ឧ','៌'])){
        m=1;   // Line 608
        k.KDC(2,t);
        k.KO(-1,t,"ឱ");
      }
      else if(k.KFCM(2,t,['ឧ','៍'])){
        m=1;   // Line 609
        k.KDC(2,t);
        k.KO(-1,t,"ឱ");
      }
    return r;
  };
}
