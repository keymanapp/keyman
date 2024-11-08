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
  this.KV={F:' 1em "KbdKhmr"',K102:0};
  this.KV.KLS={
    "rightalt": ["","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""],
    "rightalt-shift": ["","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""],
    "default": ["","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""],
    "shift": ["","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""]
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
  this.KBVER="1.5";
  this.KMBM=modCodes.RALT | modCodes.SHIFT /* 0x0018 */;
  this.KVKD="T_17D2_1780 T_17D2_1781 T_17D2_1782 T_17D2_1783 T_17D2_1784 T_17D2_1785 T_17D2_1786 T_17D2_1787 T_17D2_1788 T_17D2_1789 T_17D2_178A T_17D2_178B T_17D2_178C T_17D2_178D T_17D2_178E T_17D2_178F T_17D2_1790 T_17D2_1791 T_17D2_1792 T_17D2_1793 T_17D2_1794 T_17D2_1795 T_17D2_1796 T_17D2_1797 T_17D2_1798 T_17D2_1799 T_17D2_179A T_17D2_179B T_17D2_179C T_17D2_179D T_17D2_179E T_17D2_179F T_17D2_17A0 T_17D2_17A1 T_17D2_17A2 U_0030 U_0031 U_0032 U_0033 U_0034 U_0035 U_0036 U_0037 U_0038 U_0039";
  this.KVKL={
  "tablet": {
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
                "text": ""
              },
              {
                "id": "K_2",
                "text": ""
              },
              {
                "id": "K_3",
                "text": ""
              },
              {
                "id": "K_4",
                "text": ""
              },
              {
                "id": "K_5",
                "text": ""
              },
              {
                "id": "K_6",
                "text": ""
              },
              {
                "id": "K_7",
                "text": ""
              },
              {
                "id": "K_8",
                "text": ""
              },
              {
                "id": "K_9",
                "text": ""
              },
              {
                "id": "K_0",
                "text": ""
              },
              {
                "id": "K_HYPHEN",
                "text": ""
              },
              {
                "id": "K_EQUAL",
                "text": ""
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
            "id": "2",
            "key": [
              {
                "id": "K_Q",
                "text": ""
              },
              {
                "id": "K_W",
                "text": ""
              },
              {
                "id": "K_E",
                "text": ""
              },
              {
                "id": "K_R",
                "text": ""
              },
              {
                "id": "K_T",
                "text": ""
              },
              {
                "id": "K_Y",
                "text": ""
              },
              {
                "id": "K_U",
                "text": ""
              },
              {
                "id": "K_I",
                "text": ""
              },
              {
                "id": "K_O",
                "text": ""
              },
              {
                "id": "K_P",
                "text": ""
              },
              {
                "id": "K_LBRKT",
                "text": ""
              },
              {
                "id": "K_RBRKT",
                "text": ""
              },
              {
                "id": "T_new_138",
                "width": "10",
                "sp": "10"
              }
            ]
          },
          {
            "id": "3",
            "key": [
              {
                "id": "K_BKQUOTE",
                "text": ""
              },
              {
                "id": "K_A",
                "text": ""
              },
              {
                "id": "K_S",
                "text": ""
              },
              {
                "id": "K_D",
                "text": ""
              },
              {
                "id": "K_F",
                "text": ""
              },
              {
                "id": "K_G",
                "text": ""
              },
              {
                "id": "K_H",
                "text": ""
              },
              {
                "id": "K_J",
                "text": ""
              },
              {
                "id": "K_K",
                "text": ""
              },
              {
                "id": "K_L",
                "text": ""
              },
              {
                "id": "K_COLON",
                "text": ""
              },
              {
                "id": "K_QUOTE",
                "text": ""
              },
              {
                "id": "K_BKSLASH",
                "text": ""
              }
            ]
          },
          {
            "id": "4",
            "key": [
              {
                "id": "K_SHIFT",
                "text": "*Shift*",
                "width": "160",
                "sp": "1",
                "nextlayer": "shift"
              },
              {
                "id": "K_oE2"
              },
              {
                "id": "K_Z",
                "text": ""
              },
              {
                "id": "K_X",
                "text": ""
              },
              {
                "id": "K_C",
                "text": ""
              },
              {
                "id": "K_V",
                "text": ""
              },
              {
                "id": "K_B",
                "text": ""
              },
              {
                "id": "K_N",
                "text": ""
              },
              {
                "id": "K_M",
                "text": ""
              },
              {
                "id": "K_COMMA",
                "text": ""
              },
              {
                "id": "K_PERIOD",
                "text": ""
              },
              {
                "id": "K_SLASH",
                "text": ""
              },
              {
                "id": "T_new_164",
                "width": "10",
                "sp": "10"
              }
            ]
          },
          {
            "id": "5",
            "key": [
              {
                "id": "K_LCONTROL",
                "text": "*AltGr*",
                "width": "160",
                "sp": "1",
                "nextlayer": "rightalt"
              },
              {
                "id": "K_LOPT",
                "text": "*Menu*",
                "width": "160",
                "sp": "1"
              },
              {
                "id": "K_SPACE",
                "text": "",
                "width": "930"
              },
              {
                "id": "K_ENTER",
                "text": "*Enter*",
                "width": "160",
                "sp": "1"
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
                "text": ""
              },
              {
                "id": "K_2",
                "text": ""
              },
              {
                "id": "K_3",
                "text": ""
              },
              {
                "id": "K_4",
                "text": ""
              },
              {
                "id": "K_5",
                "text": ""
              },
              {
                "id": "K_6",
                "text": ""
              },
              {
                "id": "K_7",
                "text": ""
              },
              {
                "id": "K_8",
                "text": ""
              },
              {
                "id": "K_9",
                "text": ""
              },
              {
                "id": "K_0",
                "text": ""
              },
              {
                "id": "K_HYPHEN",
                "text": ""
              },
              {
                "id": "K_EQUAL",
                "text": ""
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
            "id": "2",
            "key": [
              {
                "id": "K_Q",
                "text": ""
              },
              {
                "id": "K_W",
                "text": ""
              },
              {
                "id": "K_E",
                "text": ""
              },
              {
                "id": "K_R",
                "text": ""
              },
              {
                "id": "K_T",
                "text": ""
              },
              {
                "id": "K_Y",
                "text": ""
              },
              {
                "id": "K_U",
                "text": ""
              },
              {
                "id": "K_I",
                "text": ""
              },
              {
                "id": "K_O",
                "text": ""
              },
              {
                "id": "K_P",
                "text": ""
              },
              {
                "id": "K_LBRKT",
                "text": ""
              },
              {
                "id": "K_RBRKT",
                "text": ""
              },
              {
                "id": "T_new_307",
                "width": "10",
                "sp": "10"
              }
            ]
          },
          {
            "id": "3",
            "key": [
              {
                "id": "K_BKQUOTE",
                "text": ""
              },
              {
                "id": "K_A",
                "text": ""
              },
              {
                "id": "K_S",
                "text": ""
              },
              {
                "id": "K_D",
                "text": ""
              },
              {
                "id": "K_F",
                "text": ""
              },
              {
                "id": "K_G",
                "text": ""
              },
              {
                "id": "K_H",
                "text": ""
              },
              {
                "id": "K_J",
                "text": ""
              },
              {
                "id": "K_K",
                "text": ""
              },
              {
                "id": "K_L",
                "text": ""
              },
              {
                "id": "K_COLON",
                "text": ""
              },
              {
                "id": "K_QUOTE",
                "text": ""
              },
              {
                "id": "K_BKSLASH",
                "text": ""
              }
            ]
          },
          {
            "id": "4",
            "key": [
              {
                "id": "K_SHIFT",
                "text": "*Shift*",
                "width": "160",
                "sp": "1",
                "nextlayer": "shift"
              },
              {
                "id": "K_oE2"
              },
              {
                "id": "K_Z",
                "text": ""
              },
              {
                "id": "K_X",
                "text": ""
              },
              {
                "id": "K_C",
                "text": ""
              },
              {
                "id": "K_V",
                "text": ""
              },
              {
                "id": "K_B",
                "text": ""
              },
              {
                "id": "K_N",
                "text": ""
              },
              {
                "id": "K_M",
                "text": ""
              },
              {
                "id": "K_COMMA",
                "text": ""
              },
              {
                "id": "K_PERIOD",
                "text": ""
              },
              {
                "id": "K_SLASH",
                "text": ""
              },
              {
                "id": "T_new_333",
                "width": "10",
                "sp": "10"
              }
            ]
          },
          {
            "id": "5",
            "key": [
              {
                "id": "K_LCONTROL",
                "text": "*AltGr*",
                "width": "160",
                "sp": "2",
                "nextlayer": "default"
              },
              {
                "id": "K_LOPT",
                "text": "*Menu*",
                "width": "160",
                "sp": "1"
              },
              {
                "id": "K_SPACE",
                "text": "",
                "width": "930"
              },
              {
                "id": "K_ENTER",
                "text": "*Enter*",
                "width": "160",
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
            "id": "1",
            "key": [
              {
                "id": "K_1",
                "text": ""
              },
              {
                "id": "K_2",
                "text": ""
              },
              {
                "id": "K_3",
                "text": ""
              },
              {
                "id": "K_4",
                "text": ""
              },
              {
                "id": "K_5",
                "text": ""
              },
              {
                "id": "K_6",
                "text": ""
              },
              {
                "id": "K_7",
                "text": ""
              },
              {
                "id": "K_8",
                "text": ""
              },
              {
                "id": "K_9",
                "text": ""
              },
              {
                "id": "K_0",
                "text": ""
              },
              {
                "id": "K_HYPHEN",
                "text": ""
              },
              {
                "id": "K_EQUAL",
                "text": ""
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
            "id": "2",
            "key": [
              {
                "id": "K_Q",
                "text": ""
              },
              {
                "id": "K_W",
                "text": ""
              },
              {
                "id": "K_E",
                "text": ""
              },
              {
                "id": "K_R",
                "text": ""
              },
              {
                "id": "K_T",
                "text": ""
              },
              {
                "id": "K_Y",
                "text": ""
              },
              {
                "id": "K_U",
                "text": ""
              },
              {
                "id": "K_I",
                "text": ""
              },
              {
                "id": "K_O",
                "text": ""
              },
              {
                "id": "K_P",
                "text": ""
              },
              {
                "id": "K_LBRKT",
                "text": ""
              },
              {
                "id": "K_RBRKT",
                "text": ""
              },
              {
                "id": "T_new_364",
                "width": "10",
                "sp": "10"
              }
            ]
          },
          {
            "id": "3",
            "key": [
              {
                "id": "K_BKQUOTE",
                "text": ""
              },
              {
                "id": "K_A",
                "text": ""
              },
              {
                "id": "K_S",
                "text": ""
              },
              {
                "id": "K_D",
                "text": ""
              },
              {
                "id": "K_F",
                "text": ""
              },
              {
                "id": "K_G",
                "text": ""
              },
              {
                "id": "K_H",
                "text": ""
              },
              {
                "id": "K_J",
                "text": ""
              },
              {
                "id": "K_K",
                "text": ""
              },
              {
                "id": "K_L",
                "text": ""
              },
              {
                "id": "K_COLON",
                "text": ""
              },
              {
                "id": "K_QUOTE",
                "text": ""
              },
              {
                "id": "K_BKSLASH",
                "text": ""
              }
            ]
          },
          {
            "id": "4",
            "key": [
              {
                "id": "K_SHIFT",
                "text": "*Shift*",
                "width": "160",
                "sp": "2",
                "nextlayer": "default"
              },
              {
                "id": "K_oE2"
              },
              {
                "id": "K_Z",
                "text": ""
              },
              {
                "id": "K_X",
                "text": ""
              },
              {
                "id": "K_C",
                "text": ""
              },
              {
                "id": "K_V",
                "text": ""
              },
              {
                "id": "K_B",
                "text": ""
              },
              {
                "id": "K_N",
                "text": ""
              },
              {
                "id": "K_M",
                "text": ""
              },
              {
                "id": "K_COMMA",
                "text": ""
              },
              {
                "id": "K_PERIOD",
                "text": ""
              },
              {
                "id": "K_SLASH",
                "text": ""
              },
              {
                "id": "T_new_390",
                "width": "10",
                "sp": "10"
              }
            ]
          },
          {
            "id": "5",
            "key": [
              {
                "id": "K_LCONTROL",
                "text": "*AltGr*",
                "width": "160",
                "sp": "1",
                "nextlayer": "rightalt"
              },
              {
                "id": "K_LOPT",
                "text": "*Menu*",
                "width": "160",
                "sp": "1"
              },
              {
                "id": "K_SPACE",
                "width": "930"
              },
              {
                "id": "K_ENTER",
                "text": "*Enter*",
                "width": "160",
                "sp": "1"
              }
            ]
          }
        ]
      }
    ],
    "font": "Khmer Mondulkiri"
  },
  "phone": {
    "layer": [
      {
        "id": "default",
        "row": [
          {
            "id": "1",
            "key": [
              {
                "id": "K_Q",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "K_Q",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_1786"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_1788"
                  }
                ]
              },
              {
                "id": "K_W",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "K_W",
                    "layer": "shift"
                  }
                ]
              },
              {
                "id": "K_E",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "K_E",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "K_S",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "K_V",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "U_17AF"
                  },
                  {
                    "text": "",
                    "id": "U_17B0"
                  }
                ]
              },
              {
                "id": "K_R",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "T_17D2_179A"
                  },
                  {
                    "text": "",
                    "id": "U_17AB"
                  },
                  {
                    "text": "",
                    "id": "U_17AC"
                  }
                ]
              },
              {
                "id": "K_T",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "K_T",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_178F"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_1791",
                    "layer": "default"
                  }
                ]
              },
              {
                "id": "K_Y",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "T_17D2_1799"
                  }
                ]
              },
              {
                "id": "K_U",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "K_U",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "K_Y",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "U_17A7"
                  },
                  {
                    "text": "",
                    "id": "U_17AA",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "U_17A9",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "U_17A8"
                  }
                ]
              },
              {
                "id": "K_I",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "K_I",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "U_17A5"
                  },
                  {
                    "text": "",
                    "id": "U_17A6",
                    "layer": "shift"
                  }
                ]
              },
              {
                "id": "K_O",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "K_O",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "K_LBRKT"
                  },
                  {
                    "text": "",
                    "id": "K_LBRKT",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "K_COLON",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "U_17B1"
                  },
                  {
                    "text": "",
                    "id": "U_17B2"
                  },
                  {
                    "text": "",
                    "id": "U_17B3",
                    "layer": "shift"
                  }
                ]
              },
              {
                "id": "K_P",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "K_P",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_1795"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_1797",
                    "layer": "default"
                  }
                ]
              }
            ]
          },
          {
            "id": "2",
            "key": [
              {
                "id": "K_A",
                "text": "",
                "width": "100",
                "sk": [
                  {
                    "text": "",
                    "id": "K_A",
                    "layer": "shift"
                  }
                ]
              },
              {
                "id": "K_S",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "T_17D2_179F"
                  },
                  {
                    "text": "",
                    "id": "U_179D"
                  },
                  {
                    "text": "",
                    "id": "U_179E"
                  }
                ]
              },
              {
                "id": "K_D",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "K_D",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_178A"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_178C",
                    "layer": "default"
                  }
                ]
              },
              {
                "id": "K_F",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "K_F",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_1790"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_1792",
                    "layer": "default"
                  }
                ]
              },
              {
                "id": "K_G",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "K_G",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_1784"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_17A2",
                    "layer": "default"
                  }
                ]
              },
              {
                "id": "K_H",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "T_17D2_17A0"
                  },
                  {
                    "text": "",
                    "id": "K_H",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "U_17C8"
                  }
                ]
              },
              {
                "id": "K_J",
                "text": "",
                "layer": "shift",
                "sk": [
                  {
                    "text": "",
                    "id": "T_17D2_1789"
                  }
                ]
              },
              {
                "id": "K_K",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "K_K",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_1780"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_1782"
                  }
                ]
              },
              {
                "id": "K_L",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "K_L",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_179B"
                  },
                  {
                    "text": "",
                    "id": "U_17AD"
                  },
                  {
                    "text": "",
                    "id": "U_17AE"
                  }
                ]
              },
              {
                "id": "K_COLON",
                "text": ""
              }
            ]
          },
          {
            "id": "3",
            "key": [
              {
                "id": "K_Z",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "K_Z",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_178B"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_178D",
                    "layer": "default"
                  }
                ]
              },
              {
                "id": "K_X",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "K_X",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_1781"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_1783",
                    "layer": "default"
                  }
                ]
              },
              {
                "id": "K_C",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "K_C",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_1785"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_1787",
                    "layer": "default"
                  }
                ]
              },
              {
                "id": "K_V",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "T_17D2_179C"
                  }
                ]
              },
              {
                "id": "K_B",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "K_B",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_1794"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_1796",
                    "layer": "default"
                  }
                ]
              },
              {
                "id": "K_N",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "K_N",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_1793"
                  },
                  {
                    "text": "",
                    "id": "T_17D2_178E",
                    "layer": "default"
                  }
                ]
              },
              {
                "id": "K_M",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "T_17D2_1798"
                  },
                  {
                    "id": "K_M",
                    "text": "",
                    "layer": "shift"
                  }
                ]
              },
              {
                "id": "K_COMMA",
                "text": "",
                "sk": [
                  {
                    "id": "K_COMMA",
                    "text": "",
                    "layer": "shift"
                  },
                  {
                    "id": "K_6",
                    "text": "",
                    "layer": "shift"
                  },
                  {
                    "id": "K_7",
                    "text": "",
                    "layer": "shift"
                  },
                  {
                    "id": "K_8",
                    "text": "",
                    "layer": "shift"
                  },
                  {
                    "id": "K_HYPHEN",
                    "text": "",
                    "layer": "shift"
                  },
                  {
                    "id": "U_17D1",
                    "text": "",
                    "layer": "shift"
                  },
                  {
                    "id": "U_17DD",
                    "text": "",
                    "layer": "shift"
                  },
                  {
                    "id": "U_17CE",
                    "text": "",
                    "layer": "shift"
                  }
                ]
              },
              {
                "id": "K_QUOTE",
                "text": "",
                "width": "100",
                "sk": [
                  {
                    "text": "",
                    "id": "K_QUOTE",
                    "layer": "shift"
                  },
                  {
                    "text": "",
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
            "id": "4",
            "key": [
              {
                "id": "K_NUMLOCK",
                "text": "",
                "width": "140",
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
                "text": "*ZWSp*​",
                "width": "555",
                "sk": [
                  {
                    "text": "*Sp*",
                    "id": "U_0020",
                    "layer": "default"
                  }
                ]
              },
              {
                "id": "K_PERIOD",
                "text": "",
                "width": "120",
                "sk": [
                  {
                    "text": "",
                    "id": "K_PERIOD",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "U_0021"
                  },
                  {
                    "text": "",
                    "id": "U_003F"
                  }
                ]
              },
              {
                "id": "K_ENTER",
                "text": "*Enter*",
                "width": "140",
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
            "id": "1",
            "key": [
              {
                "id": "K_1",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_0031"
                  }
                ]
              },
              {
                "id": "K_2",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_0032"
                  }
                ]
              },
              {
                "id": "K_3",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_0033"
                  }
                ]
              },
              {
                "id": "K_4",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_0034"
                  }
                ]
              },
              {
                "id": "K_5",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_0035"
                  }
                ]
              },
              {
                "id": "K_6",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_0036"
                  }
                ]
              },
              {
                "id": "K_7",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_0037"
                  }
                ]
              },
              {
                "id": "K_8",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_0038"
                  }
                ]
              },
              {
                "id": "K_9",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_0039"
                  }
                ]
              },
              {
                "id": "K_0",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_0030"
                  },
                  {
                    "text": "",
                    "id": "U_17D3"
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
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_00A9"
                  },
                  {
                    "text": "",
                    "id": "U_00AE"
                  }
                ]
              },
              {
                "id": "U_0023",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_2116"
                  },
                  {
                    "text": "",
                    "id": "U_007E"
                  }
                ]
              },
              {
                "id": "U_17DB",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_0024"
                  },
                  {
                    "text": "",
                    "id": "U_0E3F"
                  },
                  {
                    "text": "",
                    "id": "U_00A2"
                  },
                  {
                    "text": "",
                    "id": "U_00A3"
                  },
                  {
                    "text": "",
                    "id": "U_00A5"
                  }
                ]
              },
              {
                "id": "U_0026",
                "text": ""
              },
              {
                "id": "U_0025",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_2030"
                  },
                  {
                    "text": "",
                    "id": "U_2031"
                  }
                ]
              },
              {
                "id": "U_002B",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_002D"
                  },
                  {
                    "text": "",
                    "id": "U_00D7"
                  },
                  {
                    "text": "",
                    "id": "U_00F7"
                  },
                  {
                    "text": "",
                    "id": "U_00B1"
                  }
                ]
              },
              {
                "id": "U_003D",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_005F"
                  },
                  {
                    "text": "",
                    "id": "U_2260"
                  }
                ]
              },
              {
                "id": "U_002A",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_005E"
                  }
                ]
              },
              {
                "id": "U_003F",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_00BF"
                  }
                ]
              },
              {
                "id": "U_0021",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_00A1"
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
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_2019"
                  }
                ]
              },
              {
                "id": "U_201C",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_201D"
                  }
                ]
              },
              {
                "id": "U_00AB",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_00BB"
                  }
                ]
              },
              {
                "id": "U_002F",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_005C"
                  },
                  {
                    "text": "",
                    "id": "U_007C"
                  },
                  {
                    "text": "",
                    "id": "U_00A6"
                  }
                ]
              },
              {
                "id": "U_0028",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_0029"
                  },
                  {
                    "text": "",
                    "id": "U_005B"
                  },
                  {
                    "text": "",
                    "id": "U_005D"
                  },
                  {
                    "text": "",
                    "id": "U_007B"
                  },
                  {
                    "text": "",
                    "id": "U_007D"
                  }
                ]
              },
              {
                "id": "U_17D9",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_17DA"
                  },
                  {
                    "text": "",
                    "id": "U_17DC"
                  },
                  {
                    "text": "",
                    "id": "U_00A7"
                  },
                  {
                    "text": "",
                    "id": "U_00D8"
                  }
                ]
              },
              {
                "id": "U_17D7",
                "text": ""
              },
              {
                "id": "U_003C",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_2264"
                  },
                  {
                    "text": "",
                    "id": "U_003E"
                  },
                  {
                    "text": "",
                    "id": "U_2265"
                  }
                ]
              },
              {
                "id": "U_17D6",
                "text": "",
                "sk": [
                  {
                    "text": "",
                    "id": "U_003A"
                  },
                  {
                    "text": "",
                    "id": "U_003B"
                  },
                  {
                    "text": "",
                    "id": "U_2026"
                  }
                ]
              },
              {
                "id": "K_BKSP",
                "text": "*BkSp*",
                "sp": "1"
              }
            ]
          },
          {
            "id": "4",
            "key": [
              {
                "id": "K_LCONTROL",
                "text": "",
                "width": "140",
                "sp": "2",
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
                "text": "*Sp*​",
                "width": "555",
                "layer": "shift"
              },
              {
                "id": "K_PERIOD",
                "text": "",
                "width": "120",
                "sk": [
                  {
                    "text": "",
                    "id": "K_PERIOD",
                    "layer": "shift"
                  },
                  {
                    "text": "",
                    "id": "U_0021"
                  },
                  {
                    "text": "",
                    "id": "U_003F"
                  }
                ]
              },
              {
                "id": "K_ENTER",
                "text": "*Enter*",
                "width": "140",
                "sp": "1"
              }
            ]
          }
        ]
      }
    ],
    "displayUnderlying": false,
    "font": "Khmer Mondulkiri"
  }
};
  this.s_c_key_13=['','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',''];
  this.s_c_out_14="កខគឃងចឆជឈញដឋឌឍណតថទធនបផពភមយរលវសហឡអឝឞ";
  this.s_v_gen_key_15=['','','','','','','','','','','','','','','',''];
  this.s_v_gen_16="ាិីឹឺុូួើឿៀេែៃោៅ";
  this.s_v_pseudo_key_17=['','',''];
  this.s_v_pseudo_18="ំះៈ";
  this.s_v_key_19=['','','','','','','','','','','','','','','','','','',''];
  this.s_v_out_20="ាិីឹឺុូួើឿៀេែៃោៅំះៈ";
  this.s_v_any_21="ាិីឹឺុូួើឿៀេែៃោៅំះៈ";
  this.s_v_combo_R_22="េោុិីឹែ";
  this.s_v_combo_N_23="ាុ";
  this.s_v_combo_24="េោុិីឹែាុ";
  this.s_ind_v_key_25=['','','','','','','','','','','','','','',''];
  this.s_ind_v_out_26="ឥឦឧឨឩឪឫឬឭឮឯឰឱឲឳ";
  this.s_diacritic_key_27=['','','','','','','','','','',''];
  this.s_diacritic_out_28="់័៌៏៍ៈ៎៑៝ៜ្";
  this.s_c_shifter_key_29=['',''];
  this.s_c_shifter_30="៉៊";
  this.s_punct_key_31=['','','','','','','',''];
  this.s_punct_out_32="។៕៖ៗ៘៙៚៓";
  this.s_latin_punct_key_33=['','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',''];
  this.s_latin_punct_out_34="«»()!\"%=?{}\\@*,×./[]‍‌+-÷:≈‘’;<>#&";
  this.s_spaces_key_35=['','',''];
  this.s_spaces_out_36="​  ";
  this.s_currency_key_37=['','',''];
  this.s_currency_out_38="៛$€";
  this.s_digit_key_39=['','','','','','','','','',''];
  this.s_digit_out_40="០១២៣៤៥៦៧៨៩";
  this.s_lek_attak_key_41=['','','','','','','','','',''];
  this.s_lek_attak_out_42="៰៱៲៳៴៵៶៷៸៹";
  this.s_lunar_date_key_43=['','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',''];
  this.s_lunar_date_out_44="᧬᧻᧹᧮᧢᧯᧰᧱᧧᧲᧳᧴᧽᧼᧨᧩᧠᧣᧭᧤᧦᧺᧡᧸᧥᧷᧵᧾᧿᧪᧫᧶";
  this.s_input_subcons_45=['','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',''];
  this.s_subcons_46="កខគឃងចឆជឈញតឋឌឍណតថទធនបផពភមយរលវឝឞសហឡអ";
  this.s_arabic_digit_key_47=['','','','','','','','','',''];
  this.s_arabic_digit_out_48="0123456789";
  this.s_v_above_49="ិីឹឺើ័";
  this.s_shiftable_c_1st_50="សហអ";
  this.s_shiftable_BA_51="ប";
  this.s_shiftable_c_2nd_52="ងញមយរវនល";
  this.s_shiftable_c_2nd_with_BA_53="ងញមយរវនលប";
  this.s_c_2nd_combo_LO_54="យមងបវ";
  this.s_c_2nd_combo_MO_55="យលងរ";
  this.s_c_1st_combo_LO_56="បហអ";
  this.s_c_1st_combo_MO_57="ហសអ";
  this.s_c_combo_SA_58="បយលមនញងរវអ";
  this.s_c_combo_QA_59="ឆឈបផតទ";
  this.s_c_combo_HA_60="វឣ";
  this.s64="touch";
  this.KVER="18.0.117.0";
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
      if(k.KFCM(2,t,['្',{t:'a',a:this.s_c_out_14}])&&k.KIFS(31,this.s64,t)){
        r=m=1;   // Line 271
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,[{t:'a',a:this.s_v_combo_N_23},'ំ'])){
        r=m=1;   // Line 234
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,[{t:'a',a:this.s_v_combo_R_22},'ះ'])){
        r=m=1;   // Line 235
        k.KDC(2,t);
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_B /* 0x42 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ឞ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_K /* 0x4B */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ឝ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_QUOTE /* 0xDE */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ៈ");
      }
      else if(1){
        r=m=1;   // Line 192
        k.KDC(0,t);
        k.KO(-1,t,"ៈ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_E /* 0x45 */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"ឯ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_I /* 0x49 */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"ឦ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_O /* 0x4F */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"ឱ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_P /* 0x50 */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"ឰ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_R /* 0x52 */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"ឫ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_T /* 0x54 */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"ឨ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_LBRKT /* 0xDB */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"ឩ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_RBRKT /* 0xDD */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"ឳ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_3 /* 0x33 */)) {
      if(1){
        r=m=1;   // Line 192
        k.KDC(0,t);
        k.KO(-1,t,"៑");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_Q /* 0x51 */)) {
      if(1){
        r=m=1;   // Line 192
        k.KDC(0,t);
        k.KO(-1,t,"ៜ");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_W /* 0x57 */)) {
      if(1){
        r=m=1;   // Line 192
        k.KDC(0,t);
        k.KO(-1,t,"៝");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_EQUAL /* 0xBB */)) {
      if(1){
        r=m=1;   // Line 192
        k.KDC(0,t);
        k.KO(-1,t,"៎");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_6 /* 0x36 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"៙");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_7 /* 0x37 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"៚");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_L /* 0x4C */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"៘");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_M /* 0x4D */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"៓");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_COLON /* 0xBA */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"៖");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_0 /* 0x30 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"}");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_1 /* 0x31 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"‌");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_2 /* 0x32 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"@");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_8 /* 0x38 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"*");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_9 /* 0x39 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"{");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_A /* 0x41 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"+");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_C /* 0x43 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"#");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_D /* 0x44 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"×");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_F /* 0x46 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"÷");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_G /* 0x47 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,":");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_H /* 0x48 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"‘");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_J /* 0x4A */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"’");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_N /* 0x4E */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,";");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_S /* 0x53 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"-");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_U /* 0x55 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"]");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_V /* 0x56 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"&");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_X /* 0x58 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,">");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_Y /* 0x59 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"[");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_Z /* 0x5A */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"<");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_COMMA /* 0xBC */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,",");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_HYPHEN /* 0xBD */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"≈");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_PERIOD /* 0xBE */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,".");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_SLASH /* 0xBF */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"/");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_BKQUOTE /* 0xC0 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"‍");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_BKSLASH /* 0xDC */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"\\");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_4 /* 0x34 */)) {
      if(1){
        r=m=1;   // Line 196
        k.KDC(0,t);
        k.KO(-1,t,"$");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_5 /* 0x35 */)) {
      if(1){
        r=m=1;   // Line 196
        k.KDC(0,t);
        k.KO(-1,t,"€");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_0 /* 0x30 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"៰");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_1 /* 0x31 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"៱");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_2 /* 0x32 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"៲");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_3 /* 0x33 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"៳");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_4 /* 0x34 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"៴");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_5 /* 0x35 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"៵");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_6 /* 0x36 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"៶");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_7 /* 0x37 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"៷");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_8 /* 0x38 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"៸");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_9 /* 0x39 */)) {
      if(1){
        r=m=1;   // Line 198
        k.KDC(0,t);
        k.KO(-1,t,"៹");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_A /* 0x41 */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧬");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_B /* 0x42 */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧻");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_C /* 0x43 */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧹");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_D /* 0x44 */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧮");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_E /* 0x45 */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧢");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_F /* 0x46 */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧯");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_G /* 0x47 */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧰");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_H /* 0x48 */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧱");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_I /* 0x49 */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧧");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_J /* 0x4A */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧲");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_K /* 0x4B */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧳");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_L /* 0x4C */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧴");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_M /* 0x4D */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧽");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_N /* 0x4E */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧼");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_O /* 0x4F */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧨");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_P /* 0x50 */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧩");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_Q /* 0x51 */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧠");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_R /* 0x52 */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧣");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_S /* 0x53 */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧭");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_T /* 0x54 */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧤");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_U /* 0x55 */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧦");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_V /* 0x56 */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧺");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_W /* 0x57 */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧡");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_X /* 0x58 */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧸");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_Y /* 0x59 */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧥");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_Z /* 0x5A */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧷");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_COLON /* 0xBA */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧵");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_COMMA /* 0xBC */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧾");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_PERIOD /* 0xBE */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧿");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_LBRKT /* 0xDB */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧪");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_RBRKT /* 0xDD */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧫");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4018 */, keyCodes.K_QUOTE /* 0xDE */)) {
      if(1){
        r=m=1;   // Line 199
        k.KDC(0,t);
        k.KO(-1,t,"᧶");
      }
    }
    else if(k.KKM(e, modCodes.RALT | modCodes.VIRTUAL_KEY /* 0x4008 */, keyCodes.K_SPACE /* 0x20 */)) {
      if(1){
        r=m=1;   // Line 200
        k.KDC(0,t);
        k.KO(-1,t," ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x100)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ក");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x101)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ខ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x102)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្គ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x103)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ឃ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x104)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ង");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x105)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ច");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x106)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ឆ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x107)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ជ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x108)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ឈ");
      }
    }
      if(m) {}
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x109)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ញ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x10A)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ត");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x10B)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ឋ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x10C)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ឌ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x10D)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ឍ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x10E)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ណ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x10F)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ត");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x110)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ថ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x111)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ទ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x112)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ធ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x113)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ន");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x114)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ប");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x115)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ផ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x116)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ព");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x117)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ភ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x118)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ម");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x119)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្យ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x11A)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្រ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x11B)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ល");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x11C)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្វ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x11D)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ឝ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x11E)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ឞ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x11F)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ស");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x120)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ហ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x121)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្ឡ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x122)) {
      if(1){
        r=m=1;   // Line 267
        k.KDC(0,t);
        k.KO(-1,t,"្អ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_NPSTAR /* 0x6A */)) {
      if(1){
        r=m=1;   // Line 273
        k.KDC(0,t);
        k.KO(-1,t,"*");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_NPSTAR /* 0x6A */)) {
      if(1){
        r=m=1;   // Line 274
        k.KDC(0,t);
        k.KO(-1,t,"*");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_NPPLUS /* 0x6B */)) {
      if(1){
        r=m=1;   // Line 275
        k.KDC(0,t);
        k.KO(-1,t,"+");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_NPPLUS /* 0x6B */)) {
      if(1){
        r=m=1;   // Line 276
        k.KDC(0,t);
        k.KO(-1,t,"+");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_NPMINUS /* 0x6D */)) {
      if(1){
        r=m=1;   // Line 277
        k.KDC(0,t);
        k.KO(-1,t,"-");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_NPMINUS /* 0x6D */)) {
      if(1){
        r=m=1;   // Line 278
        k.KDC(0,t);
        k.KO(-1,t,"-");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_NPDOT /* 0x6E */)) {
      if(1){
        r=m=1;   // Line 279
        k.KDC(0,t);
        k.KO(-1,t,".");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_NPDOT /* 0x6E */)) {
      if(1){
        r=m=1;   // Line 280
        k.KDC(0,t);
        k.KO(-1,t,".");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_NPSLASH /* 0x6F */)) {
      if(1){
        r=m=1;   // Line 281
        k.KDC(0,t);
        k.KO(-1,t,"/");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_NPSLASH /* 0x6F */)) {
      if(1){
        r=m=1;   // Line 282
        k.KDC(0,t);
        k.KO(-1,t,"/");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_SPACE /* 0x20 */)) {
      if(k.KFCM(1,t,['​'])){
        r=m=1;   // Line 230
        k.KDC(1,t);
        k.KO(-1,t," ");
      }
      else if(1){
        r=m=1;   // Line 200
        k.KDC(0,t);
        k.KO(-1,t,"​");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_SPACE /* 0x20 */)) {
      if(1){
        r=m=1;   // Line 200
        k.KDC(0,t);
        k.KO(-1,t," ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_1 /* 0x31 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"!");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_QUOTE /* 0xDE */)) {
      if(k.KFCM(3,t,[{t:'a',a:this.s_c_combo_QA_59},'្','អ'])){
        r=m=1;   // Line 249
        k.KDC(3,t);
        k.KIO(-1,this.s_c_combo_QA_59,1,t);
        k.KO(-1,t,"្អ៉");
        k.KB(t);
      }
      else if(k.KFCM(3,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_56}])){
        r=m=1;   // Line 250
        k.KDC(3,t);
        k.KO(-1,t,"ល្");
        k.KIO(-1,this.s_c_1st_combo_LO_56,3,t);
        k.KO(-1,t,"៉");
        k.KB(t);
      }
      else if(k.KFCM(3,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_57}])){
        r=m=1;   // Line 251
        k.KDC(3,t);
        k.KO(-1,t,"ម្");
        k.KIO(-1,this.s_c_1st_combo_MO_57,3,t);
        k.KO(-1,t,"៉");
        k.KB(t);
      }
      else if(k.KFCM(3,t,['ស','្',{t:'a',a:this.s_c_combo_SA_58}])){
        r=m=1;   // Line 252
        k.KDC(3,t);
        k.KO(-1,t,"ស្");
        k.KIO(-1,this.s_c_combo_SA_58,3,t);
        k.KO(-1,t,"៉");
        k.KB(t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_c_combo_HA_60},'្','ហ'])){
        r=m=1;   // Line 253
        k.KDC(3,t);
        k.KIO(-1,this.s_c_combo_HA_60,1,t);
        k.KO(-1,t,"្ហ៉");
        k.KB(t);
      }
      else if(k.KFCM(3,t,['អ','្','ង'])){
        r=m=1;   // Line 254
        k.KDC(3,t);
        k.KO(-1,t,"អ្ង៉");
        k.KB(t);
      }
      else if(k.KFCM(3,t,['អ','្','វ'])){
        r=m=1;   // Line 255
        k.KDC(3,t);
        k.KO(-1,t,"អ្វ៉");
        k.KB(t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_c_shifter_30}])){
        r=m=1;   // Line 216
        k.KDC(1,t);
        k.KIO(-1,this.s_c_shifter_30,1,t);
        k.KB(t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_shiftable_c_1st_50}])){
        r=m=1;   // Line 244
        k.KDC(1,t);
        k.KIO(-1,this.s_shiftable_c_1st_50,1,t);
        k.KO(-1,t,"៉");
        k.KB(t);
      }
      else if(1){
        r=m=1;   // Line 193
        k.KDC(0,t);
        k.KO(-1,t,"៉");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_3 /* 0x33 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"\"");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_4 /* 0x34 */)) {
      if(1){
        r=m=1;   // Line 196
        k.KDC(0,t);
        k.KO(-1,t,"៛");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_5 /* 0x35 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"%");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_7 /* 0x37 */)) {
      if(1){
        r=m=1;   // Line 192
        k.KDC(0,t);
        k.KO(-1,t,"័");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_QUOTE /* 0xDE */)) {
      if(k.KFCM(2,t,['្',{t:'a',a:this.s_c_out_14}])){
        r=m=1;   // Line 215
        k.KDC(2,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_out_14,2,t);
        k.KB(t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_v_gen_16}])){
        r=m=1;   // Line 212
        k.KDC(1,t);
        k.KIO(-1,this.s_v_gen_16,1,t);
        k.KB(t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_v_pseudo_18}])){
        r=m=1;   // Line 213
        k.KDC(1,t);
        k.KIO(-1,this.s_v_pseudo_18,1,t);
        k.KB(t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_c_shifter_30}])){
        r=m=1;   // Line 214
        k.KDC(1,t);
        k.KIO(-1,this.s_c_shifter_30,1,t);
        k.KB(t);
      }
      else if(1){
        r=m=1;   // Line 192
        k.KDC(0,t);
        k.KO(-1,t,"់");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_9 /* 0x39 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"(");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_0 /* 0x30 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,")");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_8 /* 0x38 */)) {
      if(1){
        r=m=1;   // Line 192
        k.KDC(0,t);
        k.KO(-1,t,"៏");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_EQUAL /* 0xBB */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"=");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_COMMA /* 0xBC */)) {
      if(1){
        r=m=1;   // Line 207
        k.KDC(0,t);
        k.KO(-1,t,"ុំ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_HYPHEN /* 0xBD */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"ឥ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_PERIOD /* 0xBE */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"។");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_SLASH /* 0xBF */)) {
      if(k.KFCM(3,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_54}])){
        r=m=1;   // Line 259
        k.KDC(3,t);
        k.KO(-1,t,"ល្");
        k.KIO(-1,this.s_c_2nd_combo_LO_54,3,t);
        k.KO(-1,t,"៊");
        k.KB(t);
      }
      else if(k.KFCM(3,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_55}])){
        r=m=1;   // Line 260
        k.KDC(3,t);
        k.KO(-1,t,"ម្");
        k.KIO(-1,this.s_c_2nd_combo_MO_55,3,t);
        k.KO(-1,t,"៊");
        k.KB(t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_c_shifter_30}])){
        r=m=1;   // Line 216
        k.KDC(1,t);
        k.KIO(-1,this.s_c_shifter_30,1,t);
        k.KB(t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_shiftable_c_2nd_with_BA_53}])){
        r=m=1;   // Line 245
        k.KDC(1,t);
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_53,1,t);
        k.KO(-1,t,"៊");
        k.KB(t);
      }
      else if(1){
        r=m=1;   // Line 193
        k.KDC(0,t);
        k.KO(-1,t,"៊");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_0 /* 0x30 */)) {
      if(1){
        r=m=1;   // Line 197
        k.KDC(0,t);
        k.KO(-1,t,"០");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_1 /* 0x31 */)) {
      if(1){
        r=m=1;   // Line 197
        k.KDC(0,t);
        k.KO(-1,t,"១");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_2 /* 0x32 */)) {
      if(1){
        r=m=1;   // Line 197
        k.KDC(0,t);
        k.KO(-1,t,"២");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_3 /* 0x33 */)) {
      if(1){
        r=m=1;   // Line 197
        k.KDC(0,t);
        k.KO(-1,t,"៣");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_4 /* 0x34 */)) {
      if(1){
        r=m=1;   // Line 197
        k.KDC(0,t);
        k.KO(-1,t,"៤");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_5 /* 0x35 */)) {
      if(1){
        r=m=1;   // Line 197
        k.KDC(0,t);
        k.KO(-1,t,"៥");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_6 /* 0x36 */)) {
      if(1){
        r=m=1;   // Line 197
        k.KDC(0,t);
        k.KO(-1,t,"៦");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_7 /* 0x37 */)) {
      if(1){
        r=m=1;   // Line 197
        k.KDC(0,t);
        k.KO(-1,t,"៧");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_8 /* 0x38 */)) {
      if(1){
        r=m=1;   // Line 197
        k.KDC(0,t);
        k.KO(-1,t,"៨");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_9 /* 0x39 */)) {
      if(1){
        r=m=1;   // Line 197
        k.KDC(0,t);
        k.KO(-1,t,"៩");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_COLON /* 0xBA */)) {
      if(1){
        r=m=1;   // Line 206
        k.KDC(0,t);
        k.KO(-1,t,"ោះ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_COLON /* 0xBA */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ើ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_COMMA /* 0xBC */)) {
      if(1){
        r=m=1;   // Line 208
        k.KDC(0,t);
        k.KO(-1,t,"ុះ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_EQUAL /* 0xBB */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"ឲ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_PERIOD /* 0xBE */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"៕");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_SLASH /* 0xBF */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"?");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_2 /* 0x32 */)) {
      if(1){
        r=m=1;   // Line 194
        k.KDC(0,t);
        k.KO(-1,t,"ៗ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_A /* 0x41 */)) {
      if(1){
        r=m=1;   // Line 204
        k.KDC(0,t);
        k.KO(-1,t,"ាំ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_B /* 0x42 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ព");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_C /* 0x43 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ជ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_D /* 0x44 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ឌ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_E /* 0x45 */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ែ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_F /* 0x46 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ធ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_G /* 0x47 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"អ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_H /* 0x48 */)) {
      if(k.KFCM(1,t,['ះ'])){
        r=m=1;   // Line 224
        k.KDC(1,t);
        k.KO(-1,t,"ៈ");
      }
      else if(k.KFCM(1,t,['ៈ'])){
        r=m=1;   // Line 225
        k.KDC(1,t);
        k.KO(-1,t,"ះ");
      }
      else if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ះ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_I /* 0x49 */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ី");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_J /* 0x4A */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ញ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_K /* 0x4B */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"គ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_L /* 0x4C */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ឡ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_M /* 0x4D */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ំ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_N /* 0x4E */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ណ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_O /* 0x4F */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ៅ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_P /* 0x50 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ភ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_Q /* 0x51 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ឈ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_R /* 0x52 */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"ឬ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_S /* 0x53 */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ៃ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_T /* 0x54 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ទ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_U /* 0x55 */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ូ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_V /* 0x56 */)) {
      if(1){
        r=m=1;   // Line 205
        k.KDC(0,t);
        k.KO(-1,t,"េះ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_W /* 0x57 */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ឺ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_X /* 0x58 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ឃ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_Y /* 0x59 */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ួ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_Z /* 0x5A */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ឍ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_LBRKT /* 0xDB */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ៀ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_BKSLASH /* 0xDC */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"ឮ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_RBRKT /* 0xDD */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"ឪ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_6 /* 0x36 */)) {
      if(1){
        r=m=1;   // Line 192
        k.KDC(0,t);
        k.KO(-1,t,"៍");
      }
    }
      if(m) {}
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_HYPHEN /* 0xBD */)) {
      if(1){
        r=m=1;   // Line 192
        k.KDC(0,t);
        k.KO(-1,t,"៌");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_BKQUOTE /* 0xC0 */)) {
      if(1){
        r=m=1;   // Line 195
        k.KDC(0,t);
        k.KO(-1,t,"«");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_A /* 0x41 */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ា");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_B /* 0x42 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ប");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_C /* 0x43 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ច");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_D /* 0x44 */)) {
      if(k.KFCM(1,t,['្'])){
        r=m=1;   // Line 220
        k.KDC(1,t);
        k.KO(-1,t,"្ត");
      }
      else if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ដ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_E /* 0x45 */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"េ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_F /* 0x46 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ថ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_G /* 0x47 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ង");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_H /* 0x48 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ហ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_I /* 0x49 */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ិ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_J /* 0x4A */)) {
      if(1){
        r=m=1;   // Line 192
        k.KDC(0,t);
        k.KO(-1,t,"្");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_K /* 0x4B */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ក");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_L /* 0x4C */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ល");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_M /* 0x4D */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ម");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_N /* 0x4E */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ន");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_O /* 0x4F */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ោ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_P /* 0x50 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ផ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Q /* 0x51 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ឆ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_R /* 0x52 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"រ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_S /* 0x53 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ស");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_T /* 0x54 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ត");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_U /* 0x55 */)) {
      if(k.KFCM(3,t,[{t:'a',a:this.s_shiftable_c_1st_50},'ា','ំ'])){
        r=m=1;   // Line 239
        k.KDC(3,t);
        k.KIO(-1,this.s_shiftable_c_1st_50,1,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_shiftable_c_2nd_with_BA_53},'ា','ំ'])){
        r=m=1;   // Line 240
        k.KDC(3,t);
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_53,1,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ុ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_V /* 0x56 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"វ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_W /* 0x57 */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ឹ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_X /* 0x58 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ខ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Y /* 0x59 */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"យ");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Z /* 0x5A */)) {
      if(1){
        r=m=1;   // Line 189
        k.KDC(0,t);
        k.KO(-1,t,"ឋ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_LBRKT /* 0xDB */)) {
      if(1){
        r=m=1;   // Line 190
        k.KDC(0,t);
        k.KO(-1,t,"ឿ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_BKSLASH /* 0xDC */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"ឭ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_RBRKT /* 0xDD */)) {
      if(1){
        r=m=1;   // Line 191
        k.KDC(0,t);
        k.KO(-1,t,"ឧ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_BKQUOTE /* 0xC0 */)) {
      if(1){
        r=m=1;   // Line 195
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
      if(k.KFCM(7,t,[{t:'a',a:this.s_c_combo_QA_59},'្','អ','ុ','ំ','ា','ំ'])){
        m=1;   // Line 381
        k.KDC(7,t);
        k.KIO(-1,this.s_c_combo_QA_59,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_56},'ុ','ំ','ា','ំ'])){
        m=1;   // Line 386
        k.KDC(7,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_56,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_57},'ុ','ំ','ា','ំ'])){
        m=1;   // Line 391
        k.KDC(7,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_57,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,['ស','្','ប','ុ','ំ','ា','ំ'])){
        m=1;   // Line 396
        k.KDC(7,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ប៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,['ស','្',{t:'a',a:this.s_c_combo_SA_58},'ុ','ំ','ា','ំ'])){
        m=1;   // Line 401
        k.KDC(7,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_58,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,[{t:'a',a:this.s_c_combo_HA_60},'្','ហ','ុ','ំ','ា','ំ'])){
        m=1;   // Line 406
        k.KDC(7,t);
        k.KIO(-1,this.s_c_combo_HA_60,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,['អ','្','ង','ុ','ំ','ា','ំ'])){
        m=1;   // Line 411
        k.KDC(7,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,['អ','្','វ','ុ','ំ','ា','ំ'])){
        m=1;   // Line 416
        k.KDC(7,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,['ហ','្','ប','ុ','ំ','ា','ំ'])){
        m=1;   // Line 421
        k.KDC(7,t);
        k.KO(-1,t,"ហ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ប៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,['ហ','្',{t:'a',a:this.s_shiftable_c_2nd_with_BA_53},'ុ','ំ','ា','ំ'])){
        m=1;   // Line 427
        k.KDC(7,t);
        k.KO(-1,t,"ហ");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_53,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_54},'ុ','ំ','ា','ំ'])){
        m=1;   // Line 434
        k.KDC(7,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_54,3,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(7,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_55},'ុ','ំ','ា','ំ'])){
        m=1;   // Line 439
        k.KDC(7,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_55,3,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['្','ដ',{t:'a',a:this.s_v_combo_N_23},'ំ','្','រ'])){
        m=1;   // Line 345
        k.KDC(6,t);
        k.KO(-1,t,"្ត");
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
        k.KIO(-1,this.s_v_combo_N_23,3,t);
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['្','ដ',{t:'a',a:this.s_v_combo_R_22},'ះ','្','រ'])){
        m=1;   // Line 346
        k.KDC(6,t);
        k.KO(-1,t,"្ត");
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
        k.KIO(-1,this.s_v_combo_R_22,3,t);
        k.KO(-1,t,"ះ");
      }
      else if(k.KFCM(6,t,['្','រ',{t:'a',a:this.s_v_combo_N_23},'ំ','្','ដ'])){
        m=1;   // Line 349
        k.KDC(6,t);
        k.KO(-1,t,"្ត");
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
        k.KIO(-1,this.s_v_combo_N_23,3,t);
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['្','រ',{t:'a',a:this.s_v_combo_R_22},'ះ','្','ដ'])){
        m=1;   // Line 350
        k.KDC(6,t);
        k.KO(-1,t,"្ត");
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
        k.KIO(-1,this.s_v_combo_R_22,3,t);
        k.KO(-1,t,"ះ");
      }
      else if(k.KFCM(6,t,['្','រ',{t:'a',a:this.s_v_combo_N_23},'ំ','្',{t:'a',a:this.s_subcons_46}])){
        m=1;   // Line 355
        k.KDC(6,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_46,6,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
        k.KIO(-1,this.s_v_combo_N_23,3,t);
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['្','រ',{t:'a',a:this.s_v_combo_R_22},'ះ','្',{t:'a',a:this.s_subcons_46}])){
        m=1;   // Line 356
        k.KDC(6,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_46,6,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
        k.KIO(-1,this.s_v_combo_R_22,3,t);
        k.KO(-1,t,"ះ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_QA_59},'្','អ','ុ','ា','ំ'])){
        m=1;   // Line 379
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_QA_59,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_56},'ុ','ា','ំ'])){
        m=1;   // Line 384
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_56,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_57},'ុ','ា','ំ'])){
        m=1;   // Line 389
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_57,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ស','្','ប','ុ','ា','ំ'])){
        m=1;   // Line 394
        k.KDC(6,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ប៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ស','្',{t:'a',a:this.s_c_combo_SA_58},'ុ','ា','ំ'])){
        m=1;   // Line 399
        k.KDC(6,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_58,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_HA_60},'្','ហ','ុ','ា','ំ'])){
        m=1;   // Line 404
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_HA_60,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['អ','្','ង','ុ','ា','ំ'])){
        m=1;   // Line 409
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['អ','្','វ','ុ','ា','ំ'])){
        m=1;   // Line 414
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ហ','្','ប','ុ','ា','ំ'])){
        m=1;   // Line 419
        k.KDC(6,t);
        k.KO(-1,t,"ហ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ប៉");
        k.KO(-1,t,"ប");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ហ','្',{t:'a',a:this.s_shiftable_c_2nd_with_BA_53},'ុ','ា','ំ'])){
        m=1;   // Line 425
        k.KDC(6,t);
        k.KO(-1,t,"ហ");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_53,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_53,3,t);
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_54},'ុ','ា','ំ'])){
        m=1;   // Line 432
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_54,3,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_55},'ុ','ា','ំ'])){
        m=1;   // Line 437
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_55,3,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_54},'៊',{t:'a',a:this.s_v_gen_16},{t:'a',a:this.s_v_pseudo_18}])){
        m=1;   // Line 459
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_54,3,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_gen_16,5,t);
        k.KIO(-1,this.s_v_pseudo_18,6,t);
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_55},'៊',{t:'a',a:this.s_v_gen_16},{t:'a',a:this.s_v_pseudo_18}])){
        m=1;   // Line 460
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_55,3,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_gen_16,5,t);
        k.KIO(-1,this.s_v_pseudo_18,6,t);
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_QA_59},'្','អ','៉','ា','ំ'])){
        m=1;   // Line 494
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_QA_59,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_56},'៉','ា','ំ'])){
        m=1;   // Line 495
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_56,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_57},'៉','ា','ំ'])){
        m=1;   // Line 496
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_57,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ស','្','ប','៉','ា','ំ'])){
        m=1;   // Line 497
        k.KDC(6,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ប៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ស','្',{t:'a',a:this.s_c_combo_SA_58},'៉','ា','ំ'])){
        m=1;   // Line 498
        k.KDC(6,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_58,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_HA_60},'្','ហ','៉','ា','ំ'])){
        m=1;   // Line 499
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_HA_60,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['អ','្','ង','៉','ា','ំ'])){
        m=1;   // Line 500
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['អ','្','វ','៉','ា','ំ'])){
        m=1;   // Line 501
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_QA_59},'្','អ','ា','ុ','ំ'])){
        m=1;   // Line 508
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_QA_59,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_QA_59},'្','អ','ុ','ំ','ា'])){
        m=1;   // Line 509
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_QA_59,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_56},'ា','ុ','ំ'])){
        m=1;   // Line 511
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_56,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_56},'ុ','ំ','ា'])){
        m=1;   // Line 512
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_56,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_57},'ា','ុ','ំ'])){
        m=1;   // Line 514
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_57,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_57},'ុ','ំ','ា'])){
        m=1;   // Line 515
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_57,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ស','្',{t:'a',a:this.s_c_combo_SA_58},'ា','ុ','ំ'])){
        m=1;   // Line 517
        k.KDC(6,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_58,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ស','្',{t:'a',a:this.s_c_combo_SA_58},'ុ','ំ','ា'])){
        m=1;   // Line 518
        k.KDC(6,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_58,3,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_HA_60},'្','ហ','ា','ុ','ំ'])){
        m=1;   // Line 520
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_HA_60,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_HA_60},'្','ហ','ុ','ំ','ា'])){
        m=1;   // Line 521
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_HA_60,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['អ','្','ង','ា','ុ','ំ'])){
        m=1;   // Line 523
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['អ','្','ង','ុ','ំ','ា'])){
        m=1;   // Line 524
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['អ','្','វ','ា','ុ','ំ'])){
        m=1;   // Line 526
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['អ','្','វ','ុ','ំ','ា'])){
        m=1;   // Line 527
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊");
        k.KO(-1,t,"ុ");
        k.KO(-1,t,"ា");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_54},'ា','ុ','ំ'])){
        m=1;   // Line 534
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_54,3,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_54},'ុ','ំ','ា'])){
        m=1;   // Line 535
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_54,3,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_55},'ា','ុ','ំ'])){
        m=1;   // Line 537
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_55,3,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_55},'ុ','ំ','ា'])){
        m=1;   // Line 538
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_55,3,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_QA_59},'្','អ','េ','ុ','ី'])){
        m=1;   // Line 546
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_QA_59,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊ើ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_QA_59},'្','អ','ុ','េ','ី'])){
        m=1;   // Line 547
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_QA_59,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊ើ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_QA_59},'្','អ','៉','េ','ី'])){
        m=1;   // Line 548
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_QA_59,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊ើ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_56},'េ','ុ','ី'])){
        m=1;   // Line 550
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_56,3,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_56},'ុ','េ','ី'])){
        m=1;   // Line 551
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_56,3,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_56},'៉','េ','ី'])){
        m=1;   // Line 552
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_56,3,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_57},'េ','ុ','ី'])){
        m=1;   // Line 554
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_57,3,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_57},'ុ','េ','ី'])){
        m=1;   // Line 555
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_57,3,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_57},'៉','េ','ី'])){
        m=1;   // Line 556
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_57,3,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(6,t,['ស','្',{t:'a',a:this.s_c_combo_SA_58},'េ','ុ','ី'])){
        m=1;   // Line 558
        k.KDC(6,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_58,3,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(6,t,['ស','្',{t:'a',a:this.s_c_combo_SA_58},'ុ','េ','ី'])){
        m=1;   // Line 559
        k.KDC(6,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_58,3,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(6,t,['ស','្',{t:'a',a:this.s_c_combo_SA_58},'៉','េ','ី'])){
        m=1;   // Line 560
        k.KDC(6,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_58,3,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_HA_60},'្','ហ','េ','ុ','ី'])){
        m=1;   // Line 562
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_HA_60,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊ើ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_HA_60},'្','ហ','ុ','េ','ី'])){
        m=1;   // Line 563
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_HA_60,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊ើ");
      }
      else if(k.KFCM(6,t,[{t:'a',a:this.s_c_combo_HA_60},'្','ហ','៉','េ','ី'])){
        m=1;   // Line 564
        k.KDC(6,t);
        k.KIO(-1,this.s_c_combo_HA_60,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊ើ");
      }
      else if(k.KFCM(6,t,['អ','្','ង','េ','ុ','ី'])){
        m=1;   // Line 566
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊ើ");
      }
      else if(k.KFCM(6,t,['អ','្','ង','ុ','េ','ី'])){
        m=1;   // Line 567
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊ើ");
      }
      else if(k.KFCM(6,t,['អ','្','ង','៉','េ','ី'])){
        m=1;   // Line 568
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊ើ");
      }
      else if(k.KFCM(6,t,['អ','្','វ','េ','ុ','ី'])){
        m=1;   // Line 570
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊ើ");
      }
      else if(k.KFCM(6,t,['អ','្','វ','ុ','េ','ី'])){
        m=1;   // Line 571
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊ើ");
      }
      else if(k.KFCM(6,t,['អ','្','វ','៉','េ','ី'])){
        m=1;   // Line 572
        k.KDC(6,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊ើ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_54},'េ','ុ','ី'])){
        m=1;   // Line 580
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_54,3,t);
        k.KO(-1,t,"៉ើ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_54},'ុ','េ','ី'])){
        m=1;   // Line 581
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_54,3,t);
        k.KO(-1,t,"៉ើ");
      }
      else if(k.KFCM(6,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_54},'៊','េ','ី'])){
        m=1;   // Line 582
        k.KDC(6,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_54,3,t);
        k.KO(-1,t,"៉ើ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_55},'េ','ុ','ី'])){
        m=1;   // Line 584
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_55,3,t);
        k.KO(-1,t,"៉ើ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_55},'ុ','េ','ី'])){
        m=1;   // Line 585
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_55,3,t);
        k.KO(-1,t,"៉ើ");
      }
      else if(k.KFCM(6,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_55},'៊','េ','ី'])){
        m=1;   // Line 586
        k.KDC(6,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_55,3,t);
        k.KO(-1,t,"៉ើ");
      }
      else if(k.KFCM(6,t,['្','យ','្',{t:'a',a:this.s_c_out_14},'េ','ឺ'])){
        m=1;   // Line 631
        k.KDC(6,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_out_14,4,t);
        k.KO(-1,t,"ឿ");
      }
      else if(k.KFCM(6,t,['្','យ','្',{t:'a',a:this.s_c_out_14},'េ','ឹ'])){
        m=1;   // Line 632
        k.KDC(6,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_out_14,4,t);
        k.KO(-1,t,"ឿ");
      }
      else if(k.KFCM(6,t,['្','យ','្',{t:'a',a:this.s_c_out_14},'េ','ី'])){
        m=1;   // Line 633
        k.KDC(6,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_out_14,4,t);
        k.KO(-1,t,"ឿ");
      }
      else if(k.KFCM(5,t,[{t:'a',a:this.s_c_shifter_30},{t:'a',a:this.s_v_combo_N_23},'ំ','្',{t:'a',a:this.s_subcons_46}])){
        m=1;   // Line 336
        k.KDC(5,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_46,5,t);
        k.KIO(-1,this.s_c_shifter_30,1,t);
        k.KIO(-1,this.s_v_combo_N_23,2,t);
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(5,t,[{t:'a',a:this.s_c_shifter_30},{t:'a',a:this.s_v_combo_R_22},'ះ','្',{t:'a',a:this.s_subcons_46}])){
        m=1;   // Line 337
        k.KDC(5,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_46,5,t);
        k.KIO(-1,this.s_c_shifter_30,1,t);
        k.KIO(-1,this.s_v_combo_R_22,2,t);
        k.KO(-1,t,"ះ");
      }
      else if(k.KFCM(5,t,['្','ដ',{t:'a',a:this.s_v_any_21},'្','រ'])){
        m=1;   // Line 344
        k.KDC(5,t);
        k.KO(-1,t,"្ត");
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
        k.KIO(-1,this.s_v_any_21,3,t);
      }
      else if(k.KFCM(5,t,['្','រ',{t:'a',a:this.s_v_any_21},'្','ដ'])){
        m=1;   // Line 348
        k.KDC(5,t);
        k.KO(-1,t,"្ត");
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
        k.KIO(-1,this.s_v_any_21,3,t);
      }
      else if(k.KFCM(5,t,['្','រ',{t:'a',a:this.s_c_shifter_30},'្',{t:'a',a:this.s_subcons_46}])){
        m=1;   // Line 352
        k.KDC(5,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_46,5,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
        k.KIO(-1,this.s_c_shifter_30,3,t);
      }
      else if(k.KFCM(5,t,['្','រ',{t:'a',a:this.s_v_any_21},'្',{t:'a',a:this.s_subcons_46}])){
        m=1;   // Line 354
        k.KDC(5,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_46,5,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
        k.KIO(-1,this.s_v_any_21,3,t);
      }
      else if(k.KFCM(5,t,[{t:'a',a:this.s_c_combo_QA_59},'្','អ','ុ',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 378
        k.KDC(5,t);
        k.KIO(-1,this.s_c_combo_QA_59,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,[{t:'a',a:this.s_c_combo_QA_59},'្','អ',{t:'a',a:this.s_v_above_49},'ុ'])){
        m=1;   // Line 380
        k.KDC(5,t);
        k.KIO(-1,this.s_c_combo_QA_59,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊");
        k.KIO(-1,this.s_v_above_49,4,t);
      }
      else if(k.KFCM(5,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_56},'ុ',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 383
        k.KDC(5,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_56,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_56},{t:'a',a:this.s_v_above_49},'ុ'])){
        m=1;   // Line 385
        k.KDC(5,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_56,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_49,4,t);
      }
      else if(k.KFCM(5,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_57},'ុ',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 388
        k.KDC(5,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_57,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_57},{t:'a',a:this.s_v_above_49},'ុ'])){
        m=1;   // Line 390
        k.KDC(5,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_57,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_49,4,t);
      }
      if(m) {}
      else if(k.KFCM(5,t,['ស','្','ប','ុ',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 393
        k.KDC(5,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ប៉");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,['ស','្','ប',{t:'a',a:this.s_v_above_49},'ុ'])){
        m=1;   // Line 395
        k.KDC(5,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ប៉");
        k.KIO(-1,this.s_v_above_49,4,t);
      }
      else if(k.KFCM(5,t,['ស','្',{t:'a',a:this.s_c_combo_SA_58},'ុ',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 398
        k.KDC(5,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_58,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,['ស','្',{t:'a',a:this.s_c_combo_SA_58},{t:'a',a:this.s_v_above_49},'ុ'])){
        m=1;   // Line 400
        k.KDC(5,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_58,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_49,4,t);
      }
      else if(k.KFCM(5,t,[{t:'a',a:this.s_c_combo_HA_60},'្','ហ','ុ',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 403
        k.KDC(5,t);
        k.KIO(-1,this.s_c_combo_HA_60,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,[{t:'a',a:this.s_c_combo_HA_60},'្','ហ',{t:'a',a:this.s_v_above_49},'ុ'])){
        m=1;   // Line 405
        k.KDC(5,t);
        k.KIO(-1,this.s_c_combo_HA_60,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊");
        k.KIO(-1,this.s_v_above_49,4,t);
      }
      else if(k.KFCM(5,t,['អ','្','ង','ុ',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 408
        k.KDC(5,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,['អ','្','ង',{t:'a',a:this.s_v_above_49},'ុ'])){
        m=1;   // Line 410
        k.KDC(5,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊");
        k.KIO(-1,this.s_v_above_49,4,t);
      }
      else if(k.KFCM(5,t,['អ','្','វ','ុ',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 413
        k.KDC(5,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,['អ','្','វ',{t:'a',a:this.s_v_above_49},'ុ'])){
        m=1;   // Line 415
        k.KDC(5,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊");
        k.KIO(-1,this.s_v_above_49,4,t);
      }
      else if(k.KFCM(5,t,['ហ','្','ប','ុ',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 418
        k.KDC(5,t);
        k.KO(-1,t,"ហ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ប៉");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,['ហ','្','ប',{t:'a',a:this.s_v_above_49},'ុ'])){
        m=1;   // Line 420
        k.KDC(5,t);
        k.KO(-1,t,"ហ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ប៉");
        k.KIO(-1,this.s_v_above_49,4,t);
      }
      else if(k.KFCM(5,t,['ហ','្',{t:'a',a:this.s_shiftable_c_2nd_with_BA_53},'ុ',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 424
        k.KDC(5,t);
        k.KO(-1,t,"ហ");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_53,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,['ហ','្',{t:'a',a:this.s_shiftable_c_2nd_with_BA_53},{t:'a',a:this.s_v_above_49},'ុ'])){
        m=1;   // Line 426
        k.KDC(5,t);
        k.KO(-1,t,"ហ");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_53,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_49,4,t);
      }
      else if(k.KFCM(5,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_54},'ុ',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 431
        k.KDC(5,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_54,3,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_54},{t:'a',a:this.s_v_above_49},'ុ'])){
        m=1;   // Line 433
        k.KDC(5,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_54,3,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_above_49,4,t);
      }
      else if(k.KFCM(5,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_55},'ុ',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 436
        k.KDC(5,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_55,3,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_55},{t:'a',a:this.s_v_above_49},'ុ'])){
        m=1;   // Line 438
        k.KDC(5,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_55,3,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_above_49,4,t);
      }
      else if(k.KFCM(5,t,[{t:'a',a:this.s_shiftable_c_1st_50},'ុ','ំ','ា','ំ'])){
        m=1;   // Line 446
        k.KDC(5,t);
        k.KIO(-1,this.s_shiftable_c_1st_50,1,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(5,t,[{t:'a',a:this.s_shiftable_c_2nd_with_BA_53},'ុ','ំ','ា','ំ'])){
        m=1;   // Line 453
        k.KDC(5,t);
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_53,1,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(5,t,['ល','្',{t:'a',a:this.s_c_2nd_combo_LO_54},'៊',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 457
        k.KDC(5,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_LO_54,3,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,['ម','្',{t:'a',a:this.s_c_2nd_combo_MO_55},'៊',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 458
        k.KDC(5,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_2nd_combo_MO_55,3,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,['្',{t:'a',a:this.s_shiftable_c_2nd_52},'៊',{t:'a',a:this.s_v_gen_16},{t:'a',a:this.s_v_pseudo_18}])){
        m=1;   // Line 468
        k.KDC(5,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_shiftable_c_2nd_52,2,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_gen_16,4,t);
        k.KIO(-1,this.s_v_pseudo_18,5,t);
      }
      else if(k.KFCM(5,t,[{t:'a',a:this.s_c_combo_QA_59},'្','អ','៉',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 486
        k.KDC(5,t);
        k.KIO(-1,this.s_c_combo_QA_59,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"អ៊");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,['ល','្',{t:'a',a:this.s_c_1st_combo_LO_56},'៉',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 487
        k.KDC(5,t);
        k.KO(-1,t,"ល");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_LO_56,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,['ម','្',{t:'a',a:this.s_c_1st_combo_MO_57},'៉',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 488
        k.KDC(5,t);
        k.KO(-1,t,"ម");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_1st_combo_MO_57,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,['ស','្','ប','៉',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 489
        k.KDC(5,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ប៉");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,['ស','្',{t:'a',a:this.s_c_combo_SA_58},'៉',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 490
        k.KDC(5,t);
        k.KO(-1,t,"ស");
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_c_combo_SA_58,3,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,[{t:'a',a:this.s_c_combo_HA_60},'្','ហ','៉',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 491
        k.KDC(5,t);
        k.KIO(-1,this.s_c_combo_HA_60,1,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"ហ៊");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,['អ','្','ង','៉',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 492
        k.KDC(5,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"ង៊");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,['អ','្','វ','៉',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 493
        k.KDC(5,t);
        k.KO(-1,t,"អ");
        k.KO(-1,t,"្");
        k.KO(-1,t,"វ៊");
        k.KIO(-1,this.s_v_above_49,5,t);
      }
      else if(k.KFCM(5,t,['ព','័','ន','្','ឋ'])){
        m=1;   // Line 619
        k.KDC(5,t);
        k.KO(-1,t,"ព");
        k.KO(-1,t,"័");
        k.KO(-1,t,"ន");
        k.KO(-1,t,"្ធ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_v_gen_16},{t:'a',a:this.s_v_pseudo_18},{t:'a',a:this.s_v_gen_16},{t:'a',a:this.s_v_pseudo_18}])){
        m=1;   // Line 317
        k.KDC(4,t);
        k.KIO(-1,this.s_v_gen_16,3,t);
        k.KIO(-1,this.s_v_pseudo_18,4,t);
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_v_combo_N_23},'ំ','្',{t:'a',a:this.s_subcons_46}])){
        m=1;   // Line 330
        k.KDC(4,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_46,4,t);
        k.KIO(-1,this.s_v_combo_N_23,1,t);
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_v_combo_R_22},'ះ','្',{t:'a',a:this.s_subcons_46}])){
        m=1;   // Line 331
        k.KDC(4,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_46,4,t);
        k.KIO(-1,this.s_v_combo_R_22,1,t);
        k.KO(-1,t,"ះ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_c_shifter_30},{t:'a',a:this.s_v_any_21},'្',{t:'a',a:this.s_subcons_46}])){
        m=1;   // Line 335
        k.KDC(4,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_46,4,t);
        k.KIO(-1,this.s_c_shifter_30,1,t);
        k.KIO(-1,this.s_v_any_21,2,t);
      }
      else if(k.KFCM(4,t,['្','ដ','្','រ'])){
        m=1;   // Line 341
        k.KDC(4,t);
        k.KO(-1,t,"្ត");
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
      }
      else if(k.KFCM(4,t,['្','រ','្','ដ'])){
        m=1;   // Line 342
        k.KDC(4,t);
        k.KO(-1,t,"្ត");
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
      }
      else if(k.KFCM(4,t,['្','រ','្',{t:'a',a:this.s_subcons_46}])){
        m=1;   // Line 353
        k.KDC(4,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_46,4,t);
        k.KO(-1,t,"្");
        k.KO(-1,t,"រ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_c_shifter_30},{t:'a',a:this.s_v_gen_16},{t:'a',a:this.s_v_pseudo_18},{t:'a',a:this.s_c_shifter_30}])){
        m=1;   // Line 367
        k.KDC(4,t);
        k.KIO(-1,this.s_v_gen_16,2,t);
        k.KIO(-1,this.s_v_pseudo_18,3,t);
        k.KIO(-1,this.s_c_shifter_30,4,t);
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_1st_50},'ុ','ា','ំ'])){
        m=1;   // Line 444
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_1st_50,1,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_2nd_with_BA_53},'ុ','ា','ំ'])){
        m=1;   // Line 451
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_53,1,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_2nd_52},'៊',{t:'a',a:this.s_v_gen_16},{t:'a',a:this.s_v_pseudo_18}])){
        m=1;   // Line 465
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_2nd_52,1,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_gen_16,3,t);
        k.KIO(-1,this.s_v_pseudo_18,4,t);
      }
      else if(k.KFCM(4,t,['្',{t:'a',a:this.s_shiftable_c_2nd_52},'៊',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 467
        k.KDC(4,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_shiftable_c_2nd_52,2,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_49,4,t);
      }
      else if(k.KFCM(4,t,['ប','្','យ',{t:'a',a:this.s_c_shifter_30}])){
        m=1;   // Line 472
        k.KDC(4,t);
        k.KO(-1,t,"ប្យ");
        k.KIO(-1,this.s_c_shifter_30,4,t);
      }
      else if(k.KFCM(4,t,['ស','្','ប',{t:'a',a:this.s_c_shifter_30}])){
        m=1;   // Line 473
        k.KDC(4,t);
        k.KO(-1,t,"ស្ប");
        k.KIO(-1,this.s_c_shifter_30,4,t);
      }
      else if(k.KFCM(4,t,['ឆ','្','ប',{t:'a',a:this.s_c_shifter_30}])){
        m=1;   // Line 474
        k.KDC(4,t);
        k.KO(-1,t,"ឆ្ប");
        k.KIO(-1,this.s_c_shifter_30,4,t);
      }
      else if(k.KFCM(4,t,['ប','្','យ',{t:'a',a:this.s_c_shifter_30}])){
        m=1;   // Line 475
        k.KDC(4,t);
        k.KO(-1,t,"ប្យ");
        k.KIO(-1,this.s_c_shifter_30,4,t);
      }
      else if(k.KFCM(4,t,['ស','្','ប',{t:'a',a:this.s_c_shifter_30}])){
        m=1;   // Line 476
        k.KDC(4,t);
        k.KO(-1,t,"ស្ប");
        k.KIO(-1,this.s_c_shifter_30,4,t);
      }
      else if(k.KFCM(4,t,['ឆ','្','ប',{t:'a',a:this.s_c_shifter_30}])){
        m=1;   // Line 477
        k.KDC(4,t);
        k.KO(-1,t,"ឆ្ប");
        k.KIO(-1,this.s_c_shifter_30,4,t);
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_1st_50},'៉',{t:'a',a:this.s_v_gen_16},{t:'a',a:this.s_v_pseudo_18}])){
        m=1;   // Line 482
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_1st_50,1,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_gen_16,3,t);
        k.KIO(-1,this.s_v_pseudo_18,4,t);
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_1st_50},'ា','ុ','ំ'])){
        m=1;   // Line 505
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_1st_50,1,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_1st_50},'ុ','ំ','ា'])){
        m=1;   // Line 506
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_1st_50,1,t);
        k.KO(-1,t,"៊");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_2nd_with_BA_53},'ា','ុ','ំ'])){
        m=1;   // Line 531
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_53,1,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_2nd_with_BA_53},'ុ','ំ','ា'])){
        m=1;   // Line 532
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_53,1,t);
        k.KO(-1,t,"៉");
        k.KO(-1,t,"ា");
        k.KO(-1,t,"ំ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_1st_50},'េ','ុ','ី'])){
        m=1;   // Line 542
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_1st_50,1,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_1st_50},'ុ','េ','ី'])){
        m=1;   // Line 543
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_1st_50,1,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_1st_50},'៉','េ','ី'])){
        m=1;   // Line 544
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_1st_50,1,t);
        k.KO(-1,t,"៊ើ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_2nd_52},'េ','ុ','ី'])){
        m=1;   // Line 576
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_2nd_52,1,t);
        k.KO(-1,t,"៉ើ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_2nd_52},'ុ','េ','ី'])){
        m=1;   // Line 577
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_2nd_52,1,t);
        k.KO(-1,t,"៉ើ");
      }
      else if(k.KFCM(4,t,[{t:'a',a:this.s_shiftable_c_2nd_52},'៊','េ','ី'])){
        m=1;   // Line 578
        k.KDC(4,t);
        k.KIO(-1,this.s_shiftable_c_2nd_52,1,t);
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
      else if(k.KFCM(3,t,[{t:'a',a:this.s_v_gen_16},{t:'a',a:this.s_v_pseudo_18},{t:'a',a:this.s_v_gen_16}])){
        m=1;   // Line 313
        k.KDC(3,t);
        k.KIO(-1,this.s_v_gen_16,3,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_v_gen_16},{t:'a',a:this.s_v_pseudo_18},{t:'a',a:this.s_v_pseudo_18}])){
        m=1;   // Line 314
        k.KDC(3,t);
        k.KIO(-1,this.s_v_pseudo_18,3,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_v_gen_16},{t:'a',a:this.s_v_gen_16},{t:'a',a:this.s_v_pseudo_18}])){
        m=1;   // Line 315
        k.KDC(3,t);
        k.KIO(-1,this.s_v_gen_16,2,t);
        k.KIO(-1,this.s_v_pseudo_18,3,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_v_pseudo_18},{t:'a',a:this.s_v_gen_16},{t:'a',a:this.s_v_pseudo_18}])){
        m=1;   // Line 316
        k.KDC(3,t);
        k.KIO(-1,this.s_v_gen_16,2,t);
        k.KIO(-1,this.s_v_pseudo_18,3,t);
      }
      else if(k.KFCM(3,t,['្',{t:'a',a:this.s_v_gen_16},{t:'a',a:this.s_v_pseudo_18}])){
        m=1;   // Line 325
        k.KDC(3,t);
        k.KIO(-1,this.s_v_gen_16,2,t);
        k.KIO(-1,this.s_v_pseudo_18,3,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_v_any_21},'្',{t:'a',a:this.s_subcons_46}])){
        m=1;   // Line 329
        k.KDC(3,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_46,3,t);
        k.KIO(-1,this.s_v_any_21,1,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_c_shifter_30},'្',{t:'a',a:this.s_subcons_46}])){
        m=1;   // Line 360
        k.KDC(3,t);
        k.KO(-1,t,"្");
        k.KIO(-1,this.s_subcons_46,3,t);
        k.KIO(-1,this.s_c_shifter_30,1,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_v_gen_16},{t:'a',a:this.s_v_pseudo_18},{t:'a',a:this.s_c_shifter_30}])){
        m=1;   // Line 365
        k.KDC(3,t);
        k.KIO(-1,this.s_c_shifter_30,3,t);
        k.KIO(-1,this.s_v_gen_16,1,t);
        k.KIO(-1,this.s_v_pseudo_18,2,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_c_shifter_30},{t:'a',a:this.s_v_any_21},{t:'a',a:this.s_c_shifter_30}])){
        m=1;   // Line 366
        k.KDC(3,t);
        k.KIO(-1,this.s_c_shifter_30,3,t);
        k.KIO(-1,this.s_v_any_21,2,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_shiftable_c_1st_50},'ុ',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 443
        k.KDC(3,t);
        k.KIO(-1,this.s_shiftable_c_1st_50,1,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_49,3,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_shiftable_c_1st_50},{t:'a',a:this.s_v_above_49},'ុ'])){
        m=1;   // Line 445
        k.KDC(3,t);
        k.KIO(-1,this.s_shiftable_c_1st_50,1,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_49,2,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_shiftable_c_2nd_with_BA_53},'ុ',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 450
        k.KDC(3,t);
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_53,1,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_above_49,3,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_shiftable_c_2nd_with_BA_53},{t:'a',a:this.s_v_above_49},'ុ'])){
        m=1;   // Line 452
        k.KDC(3,t);
        k.KIO(-1,this.s_shiftable_c_2nd_with_BA_53,1,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_above_49,2,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_shiftable_c_2nd_52},'៊',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 464
        k.KDC(3,t);
        k.KIO(-1,this.s_shiftable_c_2nd_52,1,t);
        k.KO(-1,t,"៉");
        k.KIO(-1,this.s_v_above_49,3,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_shiftable_c_1st_50},'៉',{t:'a',a:this.s_v_above_49}])){
        m=1;   // Line 481
        k.KDC(3,t);
        k.KIO(-1,this.s_shiftable_c_1st_50,1,t);
        k.KO(-1,t,"៊");
        k.KIO(-1,this.s_v_above_49,3,t);
      }
      else if(k.KFCM(3,t,[{t:'a',a:this.s_c_out_14},{t:'a',a:this.s_v_gen_16},'៌'])){
        m=1;   // Line 590
        k.KDC(3,t);
        k.KIO(-1,this.s_c_out_14,1,t);
        k.KO(-1,t,"៌");
        k.KIO(-1,this.s_v_gen_16,2,t);
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
        m=1;   // Line 296
        k.KDC(2,t);
        k.KO(-1,t,"ោ");
      }
      else if(k.KFCM(2,t,['ា','េ'])){
        m=1;   // Line 297
        k.KDC(2,t);
        k.KO(-1,t,"ោ");
      }
      else if(k.KFCM(2,t,['េ','ី'])){
        m=1;   // Line 298
        k.KDC(2,t);
        k.KO(-1,t,"ើ");
      }
      else if(k.KFCM(2,t,['ី','េ'])){
        m=1;   // Line 299
        k.KDC(2,t);
        k.KO(-1,t,"ើ");
      }
      else if(k.KFCM(2,t,['ំ','ុ'])){
        m=1;   // Line 303
        k.KDC(2,t);
        k.KO(-1,t,"ុំ");
      }
      else if(k.KFCM(2,t,['ំ','ា'])){
        m=1;   // Line 304
        k.KDC(2,t);
        k.KO(-1,t,"ាំ");
      }
      else if(k.KFCM(2,t,[{t:'a',a:this.s_v_gen_16},{t:'a',a:this.s_v_gen_16}])){
        m=1;   // Line 309
        k.KDC(2,t);
        k.KIO(-1,this.s_v_gen_16,2,t);
      }
      else if(k.KFCM(2,t,[{t:'a',a:this.s_v_pseudo_18},{t:'a',a:this.s_v_pseudo_18}])){
        m=1;   // Line 318
        k.KDC(2,t);
        k.KIO(-1,this.s_v_pseudo_18,2,t);
      }
      else if(k.KFCM(2,t,['្','្'])){
        m=1;   // Line 323
        k.KDC(2,t);
        k.KO(-1,t,"្");
      }
      else if(k.KFCM(2,t,['្',{t:'a',a:this.s_v_any_21}])){
        m=1;   // Line 324
        k.KDC(2,t);
        k.KIO(-1,this.s_v_any_21,2,t);
      }
      if(m) {}
      else if(k.KFCM(2,t,[{t:'a',a:this.s_v_any_21},{t:'a',a:this.s_c_shifter_30}])){
        m=1;   // Line 364
        k.KDC(2,t);
        k.KIO(-1,this.s_c_shifter_30,2,t);
        k.KIO(-1,this.s_v_any_21,1,t);
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
