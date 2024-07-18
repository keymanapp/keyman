if(typeof keyman === 'undefined') {
  console.log('Keyboard requires KeymanWeb 10.0 or later');
  if(typeof tavultesoft !== 'undefined') tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");
} else {
KeymanWeb.KR(new Keyboard_ahom_star());
}
function Keyboard_ahom_star()
{
  var modCodes = keyman.osk.modifierCodes;
  var keyCodes = keyman.osk.keyCodes;

  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_ahom_star";
  this.KN="Ahom Star";
  this.KMINVER="10.0";
  this.KV={F:' 1em "Noto Serif Ahom"',K102:0};
  this.KV.KLS={
    "shift": ["~","!","@","#","$","%","^","&","*","(",")","_","+","","","","𑜩","𑜻","𑜟","𑜞","𑜌","","𑜥","𑜣","𑜧","𑜇","{","}","|","","","","𑜒","","𑜔","","𑜗","𑜠","𑜙","𑜁","𑜝",":","\"","","","","","","","𑜽","𑜺","","𑜿","𑜘","𑜐","𑜪","<",">","?","","","","","",""],
    "default": ["𑜾","𑜱","𑜲","𑜳","𑜴","𑜵","𑜶","𑜷","𑜸","𑜹","𑜰","-","=","","","","𑜫","𑜚","𑜦","𑜍","𑜄","𑜊","𑜤","𑜢","𑜨","𑜆","[","]","\\","","","","𑜡","𑜏","𑜓","𑜂","𑜕","𑜑","𑜖","𑜀","𑜎",";","'","","","","","","","𑜼","𑜅","𑜋","𑜚","𑜈","𑜃","𑜉",",",".","/","","","","","",""]
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
  this.KDU=1;
  this.KH="Ahom Unicode";
  this.KM=0;
  this.KBVER="1.0.1";
  this.KMBM=modCodes.SHIFT /* 0x0010 */;
  this.KS=1;
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
                "text": "𑜱",
                "sk": [
                  {
                    "text": "1",
                    "id": "U_0031"
                  }
                ]
              },
              {
                "id": "K_2",
                "text": "𑜲",
                "sk": [
                  {
                    "text": "2",
                    "id": "U_0032"
                  }
                ]
              },
              {
                "id": "K_3",
                "text": "𑜳",
                "sk": [
                  {
                    "text": "3",
                    "id": "U_0033"
                  }
                ]
              },
              {
                "id": "K_4",
                "text": "𑜴",
                "sk": [
                  {
                    "text": "4",
                    "id": "U_0034"
                  }
                ]
              },
              {
                "id": "K_5",
                "text": "𑜵",
                "sk": [
                  {
                    "text": "5",
                    "id": "U_0035"
                  }
                ]
              },
              {
                "id": "K_6",
                "text": "𑜶",
                "sk": [
                  {
                    "text": "6",
                    "id": "U_0036"
                  }
                ]
              },
              {
                "id": "K_7",
                "text": "𑜷",
                "sk": [
                  {
                    "text": "7",
                    "id": "U_0037"
                  }
                ]
              },
              {
                "id": "K_8",
                "text": "𑜸",
                "sk": [
                  {
                    "text": "8",
                    "id": "U_0038"
                  }
                ]
              },
              {
                "id": "K_9",
                "text": "𑜹",
                "sk": [
                  {
                    "text": "9",
                    "id": "U_0039"
                  }
                ]
              },
              {
                "id": "K_0",
                "text": "𑜰",
                "sk": [
                  {
                    "text": "0",
                    "id": "U_0030"
                  }
                ]
              },
              {
                "id": "K_HYPHEN",
                "text": "-"
              },
              {
                "id": "K_EQUAL",
                "text": "="
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
                "text": "𑜫",
                "pad": "75"
              },
              {
                "id": "K_W",
                "text": "𑜚"
              },
              {
                "id": "K_E",
                "text": "𑜦"
              },
              {
                "id": "K_R",
                "text": "𑜍"
              },
              {
                "id": "K_T",
                "text": "𑜄"
              },
              {
                "id": "K_Y",
                "text": "𑜊"
              },
              {
                "id": "K_U",
                "text": "𑜤"
              },
              {
                "id": "K_I",
                "text": "𑜢"
              },
              {
                "id": "K_O",
                "text": "𑜨"
              },
              {
                "id": "K_P",
                "text": "𑜆"
              },
              {
                "id": "K_LBRKT",
                "text": "["
              },
              {
                "id": "K_RBRKT",
                "text": "]"
              },
              {
                "id": "T_new_136",
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
                "text": "𑜾"
              },
              {
                "id": "K_A",
                "text": "𑜡"
              },
              {
                "id": "K_S",
                "text": "𑜏"
              },
              {
                "id": "K_D",
                "text": "𑜓"
              },
              {
                "id": "K_F",
                "text": "𑜂"
              },
              {
                "id": "K_G",
                "text": "𑜕"
              },
              {
                "id": "K_H",
                "text": "𑜑"
              },
              {
                "id": "K_J",
                "text": "𑜖"
              },
              {
                "id": "K_K",
                "text": "𑜀"
              },
              {
                "id": "K_L",
                "text": "𑜎"
              },
              {
                "id": "K_COLON",
                "text": ";"
              },
              {
                "id": "K_QUOTE",
                "text": "'"
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
                "id": "K_SHIFT",
                "text": "*Shift*",
                "width": "160",
                "sp": "1",
                "nextlayer": "shift"
              },
              {
                "id": "K_Z",
                "text": "𑜼"
              },
              {
                "id": "K_X",
                "text": "𑜅"
              },
              {
                "id": "K_C",
                "text": "𑜋"
              },
              {
                "id": "K_V",
                "text": "𑜚"
              },
              {
                "id": "K_B",
                "text": "𑜈"
              },
              {
                "id": "K_N",
                "text": "𑜃"
              },
              {
                "id": "K_M",
                "text": "𑜉"
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
                "id": "T_new_162",
                "width": "10",
                "sp": "10"
              }
            ]
          },
          {
            "id": "5",
            "key": [
              {
                "id": "K_LOPT",
                "text": "*Menu*",
                "width": "140",
                "sp": "1"
              },
              {
                "id": "K_SPACE",
                "width": "930"
              },
              {
                "id": "K_ENTER",
                "text": "*Enter*",
                "width": "145",
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
                "text": "!"
              },
              {
                "id": "K_2",
                "text": "@"
              },
              {
                "id": "K_3",
                "text": "#"
              },
              {
                "id": "K_4",
                "text": "$"
              },
              {
                "id": "K_5",
                "text": "%"
              },
              {
                "id": "K_6",
                "text": "^"
              },
              {
                "id": "K_7",
                "text": "&"
              },
              {
                "id": "K_8",
                "text": "*"
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
                "text": "_"
              },
              {
                "id": "K_EQUAL",
                "text": "+"
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
                "text": "𑜩",
                "pad": "75"
              },
              {
                "id": "K_W",
                "text": "𑜻"
              },
              {
                "id": "K_E",
                "text": "𑜟"
              },
              {
                "id": "K_R",
                "text": "𑜞"
              },
              {
                "id": "K_T",
                "text": "𑜌"
              },
              {
                "id": "K_Y"
              },
              {
                "id": "K_U",
                "text": "𑜥"
              },
              {
                "id": "K_I",
                "text": "𑜣"
              },
              {
                "id": "K_O",
                "text": "𑜧"
              },
              {
                "id": "K_P",
                "text": "𑜇"
              },
              {
                "id": "K_LBRKT",
                "text": "{"
              },
              {
                "id": "K_RBRKT",
                "text": "}"
              },
              {
                "id": "T_new_431",
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
                "text": "~"
              },
              {
                "id": "K_A",
                "text": "𑜒"
              },
              {
                "id": "K_S"
              },
              {
                "id": "K_D",
                "text": "𑜔"
              },
              {
                "id": "K_F"
              },
              {
                "id": "K_G",
                "text": "𑜗"
              },
              {
                "id": "K_H",
                "text": "𑜠"
              },
              {
                "id": "K_J",
                "text": "𑜙"
              },
              {
                "id": "K_K",
                "text": "𑜁"
              },
              {
                "id": "K_L",
                "text": "𑜝"
              },
              {
                "id": "K_COLON",
                "text": ":"
              },
              {
                "id": "K_QUOTE",
                "text": "\""
              },
              {
                "id": "K_BKSLASH",
                "text": "|"
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
                "nextlayer": "default"
              },
              {
                "id": "K_oE2"
              },
              {
                "id": "K_Z",
                "text": "𑜽"
              },
              {
                "id": "K_X",
                "text": "𑜺"
              },
              {
                "id": "K_C"
              },
              {
                "id": "K_V",
                "text": "𑜿"
              },
              {
                "id": "K_B",
                "text": "𑜘"
              },
              {
                "id": "K_N",
                "text": "𑜐"
              },
              {
                "id": "K_M",
                "text": "𑜪"
              },
              {
                "id": "K_COMMA",
                "text": "<"
              },
              {
                "id": "K_PERIOD",
                "text": ">"
              },
              {
                "id": "K_SLASH",
                "text": "?"
              },
              {
                "id": "T_new_457",
                "width": "10",
                "sp": "10"
              }
            ]
          },
          {
            "id": "5",
            "key": [
              {
                "id": "K_LOPT",
                "text": "*Menu*",
                "width": "140",
                "sp": "1"
              },
              {
                "id": "K_SPACE",
                "width": "930"
              },
              {
                "id": "K_ENTER",
                "text": "*Enter*",
                "width": "145",
                "sp": "1"
              }
            ]
          }
        ]
      }
    ],
    "font": "Noto Serif Ahom"
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
                "text": "𑜫"
              },
              {
                "id": "K_W",
                "text": "𑜚"
              },
              {
                "id": "K_E",
                "text": "𑜦"
              },
              {
                "id": "K_R",
                "text": "𑜍"
              },
              {
                "id": "K_T",
                "text": "𑜄"
              },
              {
                "id": "K_Y",
                "text": "𑜊"
              },
              {
                "id": "K_U",
                "text": "𑜤"
              },
              {
                "id": "K_I",
                "text": "𑜢"
              },
              {
                "id": "K_O",
                "text": "𑜨"
              },
              {
                "id": "K_P",
                "text": "𑜆"
              }
            ]
          },
          {
            "id": "2",
            "key": [
              {
                "id": "K_A",
                "text": "𑜡",
                "pad": "50"
              },
              {
                "id": "K_S",
                "text": "𑜏"
              },
              {
                "id": "K_D",
                "text": "𑜓"
              },
              {
                "id": "K_F",
                "text": "𑜂"
              },
              {
                "id": "K_G",
                "text": "𑜕"
              },
              {
                "id": "K_H",
                "text": "𑜑"
              },
              {
                "id": "K_J",
                "text": "𑜖"
              },
              {
                "id": "K_K",
                "text": "𑜀"
              },
              {
                "id": "K_L",
                "text": "𑜎"
              },
              {
                "id": "T_new_88",
                "width": "10",
                "sp": "10"
              }
            ]
          },
          {
            "id": "3",
            "key": [
              {
                "id": "K_SHIFT",
                "text": "*Shift*",
                "sp": "1",
                "nextlayer": "shift"
              },
              {
                "id": "K_Z",
                "text": "𑜼"
              },
              {
                "id": "K_X",
                "text": "𑜅"
              },
              {
                "id": "K_C",
                "text": "𑜋"
              },
              {
                "id": "K_V",
                "text": "𑜚"
              },
              {
                "id": "K_B",
                "text": "𑜈"
              },
              {
                "id": "K_N",
                "text": "𑜃"
              },
              {
                "id": "K_M",
                "text": "𑜉"
              },
              {
                "id": "K_PERIOD",
                "text": ".",
                "sk": [
                  {
                    "text": ",",
                    "id": "K_COMMA"
                  },
                  {
                    "text": "!",
                    "id": "K_1",
                    "layer": "shift"
                  },
                  {
                    "text": "?",
                    "id": "K_SLASH",
                    "layer": "shift"
                  },
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
                    "text": "\\",
                    "id": "K_BKSLASH"
                  },
                  {
                    "text": ":",
                    "id": "K_COLON",
                    "layer": "shift"
                  },
                  {
                    "text": ";",
                    "id": "K_COLON"
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
                "text": "𑜱𑜲𑜳",
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
                "width": "610"
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
            "id": "1",
            "key": [
              {
                "id": "K_Q",
                "text": "𑜩"
              },
              {
                "id": "K_W",
                "text": "𑜻"
              },
              {
                "id": "K_E",
                "text": "𑜟"
              },
              {
                "id": "K_R",
                "text": "𑜞"
              },
              {
                "id": "K_T",
                "text": "𑜌"
              },
              {
                "id": "K_Y"
              },
              {
                "id": "K_U",
                "text": "𑜥"
              },
              {
                "id": "K_I",
                "text": "𑜣"
              },
              {
                "id": "K_O",
                "text": "𑜧"
              },
              {
                "id": "K_P",
                "text": "𑜇"
              }
            ]
          },
          {
            "id": "2",
            "key": [
              {
                "id": "K_A",
                "text": "𑜒",
                "pad": "50"
              },
              {
                "id": "K_S"
              },
              {
                "id": "K_D",
                "text": "𑜔"
              },
              {
                "id": "K_F"
              },
              {
                "id": "K_G",
                "text": "𑜗"
              },
              {
                "id": "K_H",
                "text": "𑜠"
              },
              {
                "id": "K_J",
                "text": "𑜙"
              },
              {
                "id": "K_K",
                "text": "𑜁"
              },
              {
                "id": "K_L",
                "text": "𑜝"
              },
              {
                "id": "T_new_122",
                "width": "10",
                "sp": "10"
              }
            ]
          },
          {
            "id": "3",
            "key": [
              {
                "id": "K_SHIFT",
                "text": "*Shift*",
                "sp": "2",
                "nextlayer": "default"
              },
              {
                "id": "K_Z",
                "text": "𑜽"
              },
              {
                "id": "K_X",
                "text": "𑜺"
              },
              {
                "id": "K_C"
              },
              {
                "id": "K_V",
                "text": "𑜿"
              },
              {
                "id": "K_B",
                "text": "𑜘"
              },
              {
                "id": "K_N",
                "text": "𑜐"
              },
              {
                "id": "K_M",
                "text": "𑜪"
              },
              {
                "id": "K_PERIOD",
                "text": ".",
                "layer": "default",
                "sk": [
                  {
                    "text": ",",
                    "id": "K_COMMA",
                    "layer": "default"
                  },
                  {
                    "text": "!",
                    "id": "K_1",
                    "layer": "shift"
                  },
                  {
                    "text": "?",
                    "id": "K_SLASH",
                    "layer": "shift"
                  },
                  {
                    "text": "'",
                    "id": "K_QUOTE",
                    "layer": "default"
                  },
                  {
                    "text": "\"",
                    "id": "K_QUOTE",
                    "layer": "shift"
                  },
                  {
                    "text": "\\",
                    "id": "K_BKSLASH",
                    "layer": "default"
                  },
                  {
                    "text": ":",
                    "id": "K_COLON",
                    "layer": "shift"
                  },
                  {
                    "text": ";",
                    "id": "K_COLON",
                    "layer": "default"
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
                "id": "K_NUMLOCK",
                "text": "𑜱𑜲𑜳",
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
                "width": "610"
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
            "id": "1",
            "key": [
              {
                "id": "K_1",
                "text": "𑜱",
                "sk": [
                  {
                    "text": "1",
                    "id": "U_0031"
                  }
                ]
              },
              {
                "id": "K_2",
                "text": "𑜲",
                "sk": [
                  {
                    "text": "2",
                    "id": "U_0032"
                  }
                ]
              },
              {
                "id": "K_3",
                "text": "𑜳",
                "sk": [
                  {
                    "text": "3",
                    "id": "U_0033"
                  }
                ]
              },
              {
                "id": "K_4",
                "text": "𑜴",
                "sk": [
                  {
                    "text": "4",
                    "id": "U_0034"
                  }
                ]
              },
              {
                "id": "K_5",
                "text": "𑜵",
                "sk": [
                  {
                    "text": "5",
                    "id": "U_0035"
                  }
                ]
              },
              {
                "id": "K_6",
                "text": "𑜶",
                "sk": [
                  {
                    "text": "6",
                    "id": "U_0036"
                  }
                ]
              },
              {
                "id": "K_7",
                "text": "𑜷",
                "sk": [
                  {
                    "text": "7",
                    "id": "U_0037"
                  }
                ]
              },
              {
                "id": "K_8",
                "text": "𑜸",
                "sk": [
                  {
                    "text": "8",
                    "id": "U_0038"
                  }
                ]
              },
              {
                "id": "K_9",
                "text": "𑜹",
                "sk": [
                  {
                    "text": "9",
                    "id": "U_0039"
                  }
                ]
              },
              {
                "id": "K_0",
                "text": "𑜰",
                "sk": [
                  {
                    "text": "0",
                    "id": "U_0030"
                  }
                ]
              }
            ]
          },
          {
            "id": "2",
            "key": [
              {
                "id": "K_4",
                "text": "$",
                "pad": "50",
                "layer": "shift"
              },
              {
                "id": "K_2",
                "text": "@",
                "layer": "shift",
                "sk": [
                  {
                    "text": "𑜾",
                    "id": "U_1173E"
                  }
                ]
              },
              {
                "id": "K_3",
                "text": "#",
                "layer": "shift"
              },
              {
                "id": "K_5",
                "text": "%",
                "layer": "shift"
              },
              {
                "id": "K_6",
                "text": "^",
                "layer": "shift"
              },
              {
                "id": "K_HYPHEN",
                "text": "_",
                "layer": "shift"
              },
              {
                "id": "K_EQUAL",
                "text": "=",
                "layer": "default"
              },
              {
                "id": "K_BKSLASH",
                "text": "|",
                "layer": "shift"
              },
              {
                "id": "K_BKSLASH",
                "text": "\\",
                "layer": "default"
              },
              {
                "id": "T_new_156",
                "width": "10",
                "sp": "10"
              }
            ]
          },
          {
            "id": "3",
            "key": [
              {
                "id": "K_LBRKT",
                "text": "[",
                "pad": "130",
                "sk": [
                  {
                    "id": "U_00AB",
                    "text": "«"
                  },
                  {
                    "id": "K_COMMA",
                    "text": "<",
                    "layer": "shift"
                  },
                  {
                    "id": "K_LBRKT",
                    "text": "{",
                    "layer": "shift"
                  }
                ]
              },
              {
                "id": "K_9",
                "text": "(",
                "layer": "shift"
              },
              {
                "id": "K_0",
                "text": ")",
                "layer": "shift"
              },
              {
                "id": "K_RBRKT",
                "text": "]",
                "sk": [
                  {
                    "id": "U_00BB",
                    "text": "»"
                  },
                  {
                    "id": "K_PERIOD",
                    "text": ">",
                    "layer": "shift"
                  },
                  {
                    "id": "K_RBRKT",
                    "text": "}",
                    "layer": "shift"
                  }
                ]
              },
              {
                "id": "K_EQUAL",
                "text": "+",
                "layer": "shift"
              },
              {
                "id": "K_HYPHEN",
                "text": "-"
              },
              {
                "id": "K_8",
                "text": "*",
                "layer": "shift"
              },
              {
                "id": "K_SLASH",
                "text": "/"
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
                "id": "K_LOWER",
                "text": "𑜀𑜁𑜂",
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
                "width": "610"
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
    ],
    "displayUnderlying": false,
    "font": "Noto Serif Ahom"
  }
};
  this.KVS=[];
  this.gs=function(t,e) {
    return this.g_main_0(t,e);
  };
  this.gs=function(t,e) {
    return this.g_main_0(t,e);
  };
  this.g_main_0=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_1 /* 0x31 */)) {
      if(1){
        r=m=1;   // Line 47
        k.KDC(0,t);
        k.KO(-1,t,"!");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_QUOTE /* 0xDE */)) {
      if(1){
        r=m=1;   // Line 31
        k.KDC(0,t);
        k.KO(-1,t,"\"");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_3 /* 0x33 */)) {
      if(1){
        r=m=1;   // Line 45
        k.KDC(0,t);
        k.KO(-1,t,"#");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_4 /* 0x34 */)) {
      if(1){
        r=m=1;   // Line 44
        k.KDC(0,t);
        k.KO(-1,t,"$");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_5 /* 0x35 */)) {
      if(1){
        r=m=1;   // Line 43
        k.KDC(0,t);
        k.KO(-1,t,"%");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_7 /* 0x37 */)) {
      if(1){
        r=m=1;   // Line 41
        k.KDC(0,t);
        k.KO(-1,t,"&");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_QUOTE /* 0xDE */)) {
      if(1){
        r=m=1;   // Line 23
        k.KDC(0,t);
        k.KO(-1,t,"'");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_9 /* 0x39 */)) {
      if(1){
        r=m=1;   // Line 39
        k.KDC(0,t);
        k.KO(-1,t,"(");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_0 /* 0x30 */)) {
      if(1){
        r=m=1;   // Line 38
        k.KDC(0,t);
        k.KO(-1,t,")");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_8 /* 0x38 */)) {
      if(1){
        r=m=1;   // Line 40
        k.KDC(0,t);
        k.KO(-1,t,"*");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_EQUAL /* 0xBB */)) {
      if(1){
        r=m=1;   // Line 36
        k.KDC(0,t);
        k.KO(-1,t,"+");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_COMMA /* 0xBC */)) {
      if(1){
        r=m=1;   // Line 21
        k.KDC(0,t);
        k.KO(-1,t,",");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_HYPHEN /* 0xBD */)) {
      if(1){
        r=m=1;   // Line 19
        k.KDC(0,t);
        k.KO(-1,t,"-");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_PERIOD /* 0xBE */)) {
      if(1){
        r=m=1;   // Line 18
        k.KDC(0,t);
        k.KO(-1,t,".");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_SLASH /* 0xBF */)) {
      if(1){
        r=m=1;   // Line 22
        k.KDC(0,t);
        k.KO(-1,t,"/");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_0 /* 0x30 */)) {
      if(k.KFCM(1,t,['#'])){
        r=m=1;   // Line 116
        k.KDC(1,t);
        k.KO(-1,t,"0");
      }
      else if(1){
        r=m=1;   // Line 98
        k.KDC(0,t);
        k.KO(-1,t,"𑜰");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_1 /* 0x31 */)) {
      if(k.KFCM(1,t,['#'])){
        r=m=1;   // Line 125
        k.KDC(1,t);
        k.KO(-1,t,"1");
      }
      else if(1){
        r=m=1;   // Line 107
        k.KDC(0,t);
        k.KO(-1,t,"𑜱");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_2 /* 0x32 */)) {
      if(k.KFCM(1,t,['#'])){
        r=m=1;   // Line 124
        k.KDC(1,t);
        k.KO(-1,t,"2");
      }
      else if(1){
        r=m=1;   // Line 106
        k.KDC(0,t);
        k.KO(-1,t,"𑜲");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_3 /* 0x33 */)) {
      if(k.KFCM(1,t,['#'])){
        r=m=1;   // Line 123
        k.KDC(1,t);
        k.KO(-1,t,"3");
      }
      else if(1){
        r=m=1;   // Line 105
        k.KDC(0,t);
        k.KO(-1,t,"𑜳");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_4 /* 0x34 */)) {
      if(k.KFCM(1,t,['#'])){
        r=m=1;   // Line 122
        k.KDC(1,t);
        k.KO(-1,t,"4");
      }
      else if(1){
        r=m=1;   // Line 104
        k.KDC(0,t);
        k.KO(-1,t,"𑜴");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_5 /* 0x35 */)) {
      if(k.KFCM(1,t,['#'])){
        r=m=1;   // Line 121
        k.KDC(1,t);
        k.KO(-1,t,"5");
      }
      else if(1){
        r=m=1;   // Line 103
        k.KDC(0,t);
        k.KO(-1,t,"𑜵");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_6 /* 0x36 */)) {
      if(k.KFCM(1,t,['#'])){
        r=m=1;   // Line 120
        k.KDC(1,t);
        k.KO(-1,t,"6");
      }
      else if(1){
        r=m=1;   // Line 102
        k.KDC(0,t);
        k.KO(-1,t,"𑜶");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_7 /* 0x37 */)) {
      if(k.KFCM(1,t,['#'])){
        r=m=1;   // Line 119
        k.KDC(1,t);
        k.KO(-1,t,"7");
      }
      else if(1){
        r=m=1;   // Line 101
        k.KDC(0,t);
        k.KO(-1,t,"𑜷");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_8 /* 0x38 */)) {
      if(k.KFCM(1,t,['#'])){
        r=m=1;   // Line 118
        k.KDC(1,t);
        k.KO(-1,t,"8");
      }
      else if(1){
        r=m=1;   // Line 100
        k.KDC(0,t);
        k.KO(-1,t,"𑜸");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_9 /* 0x39 */)) {
      if(k.KFCM(1,t,['#'])){
        r=m=1;   // Line 117
        k.KDC(1,t);
        k.KO(-1,t,"9");
      }
      else if(1){
        r=m=1;   // Line 99
        k.KDC(0,t);
        k.KO(-1,t,"𑜹");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_COLON /* 0xBA */)) {
      if(1){
        r=m=1;   // Line 32
        k.KDC(0,t);
        k.KO(-1,t,":");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_COLON /* 0xBA */)) {
      if(1){
        r=m=1;   // Line 24
        k.KDC(0,t);
        k.KO(-1,t,";");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_COMMA /* 0xBC */)) {
      if(1){
        r=m=1;   // Line 30
        k.KDC(0,t);
        k.KO(-1,t,"<");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_EQUAL /* 0xBB */)) {
      if(1){
        r=m=1;   // Line 20
        k.KDC(0,t);
        k.KO(-1,t,"=");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_PERIOD /* 0xBE */)) {
      if(1){
        r=m=1;   // Line 29
        k.KDC(0,t);
        k.KO(-1,t,">");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_SLASH /* 0xBF */)) {
      if(1){
        r=m=1;   // Line 28
        k.KDC(0,t);
        k.KO(-1,t,"?");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_2 /* 0x32 */)) {
      if(1){
        r=m=1;   // Line 46
        k.KDC(0,t);
        k.KO(-1,t,"@");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_A /* 0x41 */)) {
      if(1){
        r=m=1;   // Line 61
        k.KDC(0,t);
        k.KO(-1,t,"𑜒");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_B /* 0x42 */)) {
      if(1){
        r=m=1;   // Line 51
        k.KDC(0,t);
        k.KO(-1,t,"𑜘");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_C /* 0x43 */)) {
      if(1){
        r=m=1;   // Line 113
        k.KDC(0,t);
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_D /* 0x44 */)) {
      if(1){
        r=m=1;   // Line 60
        k.KDC(0,t);
        k.KO(-1,t,"𑜔");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_E /* 0x45 */)) {
      if(1){
        r=m=1;   // Line 68
        k.KDC(0,t);
        k.KO(-1,t,"𑜟");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_F /* 0x46 */)) {
      if(1){
        r=m=1;   // Line 112
        k.KDC(0,t);
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_G /* 0x47 */)) {
      if(1){
        r=m=1;   // Line 59
        k.KDC(0,t);
        k.KO(-1,t,"𑜗");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_H /* 0x48 */)) {
      if(1){
        r=m=1;   // Line 58
        k.KDC(0,t);
        k.KO(-1,t,"𑜠");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_I /* 0x49 */)) {
      if(1){
        r=m=1;   // Line 64
        k.KDC(0,t);
        k.KO(-1,t,"𑜣");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_J /* 0x4A */)) {
      if(1){
        r=m=1;   // Line 57
        k.KDC(0,t);
        k.KO(-1,t,"𑜙");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_K /* 0x4B */)) {
      if(1){
        r=m=1;   // Line 56
        k.KDC(0,t);
        k.KO(-1,t,"𑜁");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_L /* 0x4C */)) {
      if(1){
        r=m=1;   // Line 55
        k.KDC(0,t);
        k.KO(-1,t,"𑜝");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_M /* 0x4D */)) {
      if(1){
        r=m=1;   // Line 49
        k.KDC(0,t);
        k.KO(-1,t,"𑜪");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_N /* 0x4E */)) {
      if(1){
        r=m=1;   // Line 50
        k.KDC(0,t);
        k.KO(-1,t,"𑜐");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_O /* 0x4F */)) {
      if(1){
        r=m=1;   // Line 63
        k.KDC(0,t);
        k.KO(-1,t,"𑜧");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_P /* 0x50 */)) {
      if(1){
        r=m=1;   // Line 62
        k.KDC(0,t);
        k.KO(-1,t,"𑜇");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_Q /* 0x51 */)) {
      if(1){
        r=m=1;   // Line 70
        k.KDC(0,t);
        k.KO(-1,t,"𑜩");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_R /* 0x52 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"𑜞");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_S /* 0x53 */)) {
      if(1){
        r=m=1;   // Line 111
        k.KDC(0,t);
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_T /* 0x54 */)) {
      if(1){
        r=m=1;   // Line 66
        k.KDC(0,t);
        k.KO(-1,t,"𑜌");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_U /* 0x55 */)) {
      if(1){
        r=m=1;   // Line 65
        k.KDC(0,t);
        k.KO(-1,t,"𑜥");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_V /* 0x56 */)) {
      if(1){
        r=m=1;   // Line 52
        k.KDC(0,t);
        k.KO(-1,t,"𑜿");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_W /* 0x57 */)) {
      if(1){
        r=m=1;   // Line 69
        k.KDC(0,t);
        k.KO(-1,t,"𑜻");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_X /* 0x58 */)) {
      if(1){
        r=m=1;   // Line 53
        k.KDC(0,t);
        k.KO(-1,t,"𑜺");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_Y /* 0x59 */)) {
      if(1){
        r=m=1;   // Line 110
        k.KDC(0,t);
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_Z /* 0x5A */)) {
      if(1){
        r=m=1;   // Line 54
        k.KDC(0,t);
        k.KO(-1,t,"𑜽");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_LBRKT /* 0xDB */)) {
      if(1){
        r=m=1;   // Line 27
        k.KDC(0,t);
        k.KO(-1,t,"[");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_BKSLASH /* 0xDC */)) {
      if(1){
        r=m=1;   // Line 25
        k.KDC(0,t);
        k.KO(-1,t,"\\");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_RBRKT /* 0xDD */)) {
      if(1){
        r=m=1;   // Line 26
        k.KDC(0,t);
        k.KO(-1,t,"]");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_6 /* 0x36 */)) {
      if(1){
        r=m=1;   // Line 42
        k.KDC(0,t);
        k.KO(-1,t,"^");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_HYPHEN /* 0xBD */)) {
      if(1){
        r=m=1;   // Line 37
        k.KDC(0,t);
        k.KO(-1,t,"_");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_BKQUOTE /* 0xC0 */)) {
      if(1){
        r=m=1;   // Line 97
        k.KDC(0,t);
        k.KO(-1,t,"𑜾");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_A /* 0x41 */)) {
      if(1){
        r=m=1;   // Line 86
        k.KDC(0,t);
        k.KO(-1,t,"𑜡");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_B /* 0x42 */)) {
      if(1){
        r=m=1;   // Line 73
        k.KDC(0,t);
        k.KO(-1,t,"𑜈");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_C /* 0x43 */)) {
      if(1){
        r=m=1;   // Line 75
        k.KDC(0,t);
        k.KO(-1,t,"𑜋");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_D /* 0x44 */)) {
      if(1){
        r=m=1;   // Line 84
        k.KDC(0,t);
        k.KO(-1,t,"𑜓");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_E /* 0x45 */)) {
      if(1){
        r=m=1;   // Line 94
        k.KDC(0,t);
        k.KO(-1,t,"𑜦");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_F /* 0x46 */)) {
      if(1){
        r=m=1;   // Line 83
        k.KDC(0,t);
        k.KO(-1,t,"𑜂");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_G /* 0x47 */)) {
      if(1){
        r=m=1;   // Line 82
        k.KDC(0,t);
        k.KO(-1,t,"𑜕");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_H /* 0x48 */)) {
      if(1){
        r=m=1;   // Line 81
        k.KDC(0,t);
        k.KO(-1,t,"𑜑");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_I /* 0x49 */)) {
      if(1){
        r=m=1;   // Line 89
        k.KDC(0,t);
        k.KO(-1,t,"𑜢");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_J /* 0x4A */)) {
      if(1){
        r=m=1;   // Line 80
        k.KDC(0,t);
        k.KO(-1,t,"𑜖");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_K /* 0x4B */)) {
      if(1){
        r=m=1;   // Line 79
        k.KDC(0,t);
        k.KO(-1,t,"𑜀");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_L /* 0x4C */)) {
      if(1){
        r=m=1;   // Line 78
        k.KDC(0,t);
        k.KO(-1,t,"𑜎");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_M /* 0x4D */)) {
      if(1){
        r=m=1;   // Line 71
        k.KDC(0,t);
        k.KO(-1,t,"𑜉");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_N /* 0x4E */)) {
      if(1){
        r=m=1;   // Line 72
        k.KDC(0,t);
        k.KO(-1,t,"𑜃");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_O /* 0x4F */)) {
      if(1){
        r=m=1;   // Line 88
        k.KDC(0,t);
        k.KO(-1,t,"𑜨");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_P /* 0x50 */)) {
      if(1){
        r=m=1;   // Line 87
        k.KDC(0,t);
        k.KO(-1,t,"𑜆");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Q /* 0x51 */)) {
      if(1){
        r=m=1;   // Line 96
        k.KDC(0,t);
        k.KO(-1,t,"𑜫");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_R /* 0x52 */)) {
      if(1){
        r=m=1;   // Line 93
        k.KDC(0,t);
        k.KO(-1,t,"𑜍");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_S /* 0x53 */)) {
      if(1){
        r=m=1;   // Line 85
        k.KDC(0,t);
        k.KO(-1,t,"𑜏");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_T /* 0x54 */)) {
      if(1){
        r=m=1;   // Line 92
        k.KDC(0,t);
        k.KO(-1,t,"𑜄");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_U /* 0x55 */)) {
      if(1){
        r=m=1;   // Line 90
        k.KDC(0,t);
        k.KO(-1,t,"𑜤");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_V /* 0x56 */)) {
      if(1){
        r=m=1;   // Line 74
        k.KDC(0,t);
        k.KO(-1,t,"𑜚");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_W /* 0x57 */)) {
      if(1){
        r=m=1;   // Line 95
        k.KDC(0,t);
        k.KO(-1,t,"𑜚");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_X /* 0x58 */)) {
      if(1){
        r=m=1;   // Line 76
        k.KDC(0,t);
        k.KO(-1,t,"𑜅");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Y /* 0x59 */)) {
      if(1){
        r=m=1;   // Line 91
        k.KDC(0,t);
        k.KO(-1,t,"𑜊");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Z /* 0x5A */)) {
      if(1){
        r=m=1;   // Line 77
        k.KDC(0,t);
        k.KO(-1,t,"𑜼");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_LBRKT /* 0xDB */)) {
      if(1){
        r=m=1;   // Line 35
        k.KDC(0,t);
        k.KO(-1,t,"{");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_BKSLASH /* 0xDC */)) {
      if(1){
        r=m=1;   // Line 33
        k.KDC(0,t);
        k.KO(-1,t,"|");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_RBRKT /* 0xDD */)) {
      if(1){
        r=m=1;   // Line 34
        k.KDC(0,t);
        k.KO(-1,t,"}");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_BKQUOTE /* 0xC0 */)) {
      if(1){
        r=m=1;   // Line 48
        k.KDC(0,t);
        k.KO(-1,t,"~");
      }
    }
    return r;
  };
}
