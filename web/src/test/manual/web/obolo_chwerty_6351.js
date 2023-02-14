if(typeof keyman === 'undefined') {
  console.log('Keyboard requires KeymanWeb 10.0 or later');
  if(typeof tavultesoft !== 'undefined') tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");
} else {
KeymanWeb.KR(new Keyboard_obolo_chwerty_6351());
}
function Keyboard_obolo_chwerty_6351()
{
  var modCodes = keyman.osk.modifierCodes;
  var keyCodes = keyman.osk.keyCodes;

  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_obolo_chwerty_6351";
  this.KN="Obolo Chwerty #6351";
  this.KMINVER="15.0";
  this.KV={F:' 1em "Tahoma"',K102:1};
  this.KV.KLS={
    "default": ["`","1","2","3","4","5","6","7","8","9","0","-","=","","","","ch","w","e","r","t","y","u","i","o","p","[","]","\\ˆ","","","","a","s","d","f","g","sh","j","k","l",";","'","","","","","","","z","ọ","n̄","v","b","n","m",",",".","/","","","","","",""],
    "shift": ["~´","!","@","#","$","%","₦","&","*","(",")","_","+","","","","CH","W","E","R","T","Y","U","I","O","P","{","}","|ˇ","","","","A","S","D","F","G","SH","J","K","L",":","\"","","","","","","","Z","Ọ","N̄","V","B","N","M","<",">","?","","","","","",""]
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
  this.KH="A simple keyboard layout (chwerty) to help you type very easily in Obolo language.";
  this.KM=0;
  this.KBVER="1.2.1";
  this.KMBM=modCodes.SHIFT | modCodes.ALT | modCodes.CAPS | modCodes.NO_CAPS /* 0x0350 */;
  this.KVKD="T_GRV T_ACU T_CCM T_CRN T_ENG T_Q T_H T_X T_C";
  this.KVKL={
  "tablet": {
    "font": "Tahoma",
    "displayUnderlying": false,
    "layer": [
      {
        "id": "default",
        "row": [
          {
            "id": "1",
            "key": [
              {
                "width": "111",
                "id": "K_1",
                "text": "1",
                "sk": [
                  {
                    "id": "U_00B9",
                    "text": "\u00B9"
                  },
                  {
                    "id": "U_2081",
                    "text": "\u2081"
                  }
                ]
              },
              {
                "width": "111",
                "id": "K_2",
                "text": "2",
                "sk": [
                  {
                    "id": "U_00B2",
                    "text": "\u00B2"
                  },
                  {
                    "id": "U_2082"
                  }
                ]
              },
              {
                "width": "111",
                "id": "K_3",
                "text": "3",
                "sk": [
                  {
                    "id": "U_00B3",
                    "text": "\u00B3"
                  },
                  {
                    "id": "U_2083"
                  }
                ]
              },
              {
                "width": "111",
                "id": "K_4",
                "text": "4",
                "sk": [
                  {
                    "id": "U_2074",
                    "text": "\u2074"
                  },
                  {
                    "id": "U_2084",
                    "text": "\u2084"
                  }
                ]
              },
              {
                "width": "111",
                "id": "K_5",
                "text": "5",
                "sk": [
                  {
                    "id": "U_2075",
                    "text": "\u2075"
                  },
                  {
                    "id": "U_2085"
                  }
                ]
              },
              {
                "width": "111",
                "id": "K_6",
                "text": "6",
                "sk": [
                  {
                    "id": "U_2076",
                    "text": "\u2076"
                  },
                  {
                    "id": "U_2086"
                  }
                ]
              },
              {
                "width": "111",
                "id": "K_7",
                "text": "7",
                "sk": [
                  {
                    "id": "U_2077",
                    "text": "\u2077"
                  },
                  {
                    "id": "U_2087"
                  }
                ]
              },
              {
                "width": "111",
                "id": "K_8",
                "text": "8",
                "sk": [
                  {
                    "id": "U_2078",
                    "text": "\u2078"
                  },
                  {
                    "id": "U_2088",
                    "text": "\u2088"
                  }
                ]
              },
              {
                "width": "111",
                "id": "K_9",
                "text": "9",
                "sk": [
                  {
                    "id": "U_2079",
                    "text": "\u2079"
                  },
                  {
                    "id": "U_2089"
                  }
                ]
              },
              {
                "width": "111",
                "id": "K_0",
                "text": "0",
                "sk": [
                  {
                    "id": "U_2070",
                    "text": "\u2070"
                  },
                  {
                    "id": "U_2080"
                  }
                ]
              }
            ]
          },
          {
            "id": "2",
            "key": [
              {
                "id": "T_Q",
                "text": "ch",
                "sk": [
                  {
                    "id": "U_0071",
                    "text": "q"
                  }
                ]
              },
              {
                "id": "K_W",
                "text": "w"
              },
              {
                "id": "K_E",
                "text": "e",
                "sk": [
                  {
                    "id": "U_00E8",
                    "text": "\u00E8"
                  },
                  {
                    "id": "U_00EA",
                    "text": "\u00EA"
                  }
                ]
              },
              {
                "id": "K_R",
                "text": "r"
              },
              {
                "id": "K_T",
                "text": "t"
              },
              {
                "id": "K_Y",
                "text": "y"
              },
              {
                "id": "K_U",
                "text": "u",
                "sk": [
                  {
                    "id": "U_00F9",
                    "text": "\u00F9"
                  },
                  {
                    "id": "U_00FB",
                    "text": "\u00FB"
                  }
                ]
              },
              {
                "id": "K_I",
                "text": "i",
                "sk": [
                  {
                    "id": "U_00EC",
                    "text": "\u00EC"
                  },
                  {
                    "id": "U_00EE",
                    "text": "\u00EE"
                  }
                ]
              },
              {
                "id": "K_O",
                "text": "o",
                "sk": [
                  {
                    "id": "U_00F2",
                    "text": "\u00F2"
                  },
                  {
                    "id": "U_00F4",
                    "text": "\u00F4"
                  }
                ]
              },
              {
                "id": "K_P",
                "text": "p"
              },
              {
                "id": "T_GRV",
                "text": "\u02CB",
                "sk": [
                  {
                    "id": "T_ACU",
                    "text": "\u00B4"
                  }
                ]
              }
            ]
          },
          {
            "id": "3",
            "key": [
              {
                "id": "K_A",
                "pad": "70",
                "text": "a",
                "sk": [
                  {
                    "id": "U_00E0",
                    "text": "\u00E0"
                  },
                  {
                    "id": "U_00E2",
                    "text": "\u00E2"
                  }
                ]
              },
              {
                "id": "K_S",
                "text": "s"
              },
              {
                "id": "K_D",
                "text": "d"
              },
              {
                "id": "K_F",
                "text": "f"
              },
              {
                "id": "K_G",
                "text": "g"
              },
              {
                "id": "T_H",
                "text": "sh",
                "sk": [
                  {
                    "id": "U_0068",
                    "text": "h"
                  },
                  {
                    "id": "U_1E63",
                    "text": "\u1E63"
                  }
                ]
              },
              {
                "id": "K_J",
                "text": "j"
              },
              {
                "id": "K_K",
                "text": "k"
              },
              {
                "id": "K_L",
                "text": "l"
              },
              {
                "id": "T_CCM",
                "text": "\u02C6",
                "sk": [
                  {
                    "id": "T_CRN",
                    "text": "\u02C7"
                  }
                ]
              },
              {
                "width": "40",
                "id": "T_SPA",
                "sp": "10"
              }
            ]
          },
          {
            "id": "4",
            "key": [
              {
                "nextlayer": "shift",
                "width": "120",
                "id": "K_SHIFT",
                "pad": "50",
                "sp": "1",
                "text": "*Shift*"
              },
              {
                "id": "K_Z",
                "text": "z"
              },
              {
                "id": "T_X",
                "text": "\u1ECD",
                "sk": [
                  {
                    "id": "U_1ECD_0300",
                    "text": "\u1ECD\u0300"
                  },
                  {
                    "id": "U_1ED9",
                    "text": "\u1ED9"
                  },
                  {
                    "id": "U_0078",
                    "text": "x"
                  }
                ]
              },
              {
                "id": "T_C",
                "text": "n\u0304",
                "sk": [
                  {
                    "id": "U_00F1",
                    "text": "\u00F1"
                  },
                  {
                    "id": "U_0063",
                    "text": "c"
                  }
                ]
              },
              {
                "id": "K_V",
                "text": "v"
              },
              {
                "id": "K_B",
                "text": "b"
              },
              {
                "id": "K_N",
                "text": "n",
                "sk": [
                  {
                    "id": "U_01F9",
                    "text": "\u01F9"
                  }
                ]
              },
              {
                "id": "K_M",
                "text": "m",
                "sk": [
                  {
                    "id": "U_006D_0300",
                    "text": "m\u0300"
                  }
                ]
              },
              {
                "id": "U_002D",
                "text": "-",
                "sk": [
                  {
                    "id": "U_0021",
                    "text": "!"
                  },
                  {
                    "id": "U_002F",
                    "text": "/"
                  },
                  {
                    "id": "U_0027",
                    "text": "'"
                  },
                  {
                    "id": "U_0022",
                    "text": "\""
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
                    "id": "U_003F",
                    "text": "?"
                  }
                ]
              },
              {
                "width": "120",
                "id": "K_BKSP",
                "sp": "1",
                "text": "*BkSp*"
              },
              {
                "width": "20",
                "id": "T_SPB",
                "sp": "10"
              }
            ]
          },
          {
            "id": "5",
            "key": [
              {
                "nextlayer": "alt",
                "width": "120",
                "id": "K_ALT",
                "sp": "1",
                "text": "*Symbol*"
              },
              {
                "id": "K_LOPT",
                "sp": "1",
                "text": "*Menu*"
              },
              {
                "id": "K_COMMA",
                "text": ",",
                "sk": [
                  {
                    "id": "U_003B",
                    "text": ";"
                  }
                ]
              },
              {
                "width": "520",
                "id": "K_SPACE"
              },
              {
                "id": "K_PERIOD",
                "text": ".",
                "sk": [
                  {
                    "id": "U_003A",
                    "text": ":"
                  }
                ]
              },
              {
                "id": "T_ENG",
                "sp": "1",
                "text": "\u2A37",
                "sk": [
                  {
                    "id": "U_2013",
                    "text": "\u2013"
                  },
                  {
                    "id": "U_2014",
                    "text": "\u2014"
                  },
                  {
                    "id": "U_005F",
                    "text": "_"
                  }
                ]
              },
              {
                "width": "120",
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
                "width": "111",
                "id": "K_1",
                "text": "!"
              },
              {
                "width": "111",
                "id": "K_2",
                "text": "@"
              },
              {
                "width": "111",
                "id": "K_3",
                "text": "#"
              },
              {
                "width": "111",
                "id": "K_4",
                "text": "$"
              },
              {
                "width": "111",
                "id": "K_5",
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
                "width": "111",
                "id": "U_20A6",
                "text": "\u20A6",
                "sk": [
                  {
                    "id": "U_005E",
                    "text": "^"
                  }
                ]
              },
              {
                "width": "111",
                "id": "K_7",
                "text": "&"
              },
              {
                "width": "111",
                "id": "K_8",
                "text": "*"
              },
              {
                "width": "111",
                "id": "K_9",
                "text": "("
              },
              {
                "width": "111",
                "id": "K_0",
                "text": ")"
              }
            ]
          },
          {
            "id": "2",
            "key": [
              {
                "id": "T_Q",
                "text": "Ch",
                "sk": [
                  {
                    "id": "U_0051",
                    "text": "Q"
                  }
                ]
              },
              {
                "id": "K_W",
                "text": "W"
              },
              {
                "id": "K_E",
                "text": "E",
                "sk": [
                  {
                    "id": "U_00C8",
                    "text": "\u00C8"
                  },
                  {
                    "id": "U_00CA",
                    "text": "\u00CA"
                  }
                ]
              },
              {
                "id": "K_R",
                "text": "R"
              },
              {
                "id": "K_T",
                "text": "T"
              },
              {
                "id": "K_Y",
                "text": "Y"
              },
              {
                "id": "K_U",
                "text": "U",
                "sk": [
                  {
                    "id": "U_00D9",
                    "text": "\u00D9"
                  },
                  {
                    "id": "U_00DB",
                    "text": "\u00DB"
                  }
                ]
              },
              {
                "id": "K_I",
                "text": "I",
                "sk": [
                  {
                    "id": "U_00CC",
                    "text": "\u00CC"
                  },
                  {
                    "id": "U_00CE",
                    "text": "\u00CE"
                  }
                ]
              },
              {
                "id": "K_O",
                "text": "O",
                "sk": [
                  {
                    "id": "U_00D2",
                    "text": "\u00D2"
                  },
                  {
                    "id": "U_00D4",
                    "text": "\u00D4"
                  }
                ]
              },
              {
                "id": "K_P",
                "text": "P"
              },
              {
                "id": "T_GRV",
                "text": "`",
                "sk": [
                  {
                    "id": "T_ACU",
                    "text": "\u00B4"
                  }
                ]
              }
            ]
          },
          {
            "id": "3",
            "key": [
              {
                "id": "K_A",
                "pad": "70",
                "text": "A",
                "sk": [
                  {
                    "id": "U_00C0",
                    "text": "\u00C0"
                  },
                  {
                    "id": "U_00C2",
                    "text": "\u00C2"
                  }
                ]
              },
              {
                "id": "K_S",
                "text": "S"
              },
              {
                "id": "K_D",
                "text": "D"
              },
              {
                "id": "K_F",
                "text": "F"
              },
              {
                "id": "K_G",
                "text": "G"
              },
              {
                "id": "T_H",
                "text": "Sh",
                "sk": [
                  {
                    "id": "U_0048",
                    "text": "H"
                  },
                  {
                    "id": "U_1E62",
                    "text": "\u1E62"
                  }
                ]
              },
              {
                "id": "K_J",
                "text": "J"
              },
              {
                "id": "K_K",
                "text": "K"
              },
              {
                "id": "K_L",
                "text": "L"
              },
              {
                "id": "T_CCM",
                "text": "\u02C6",
                "sk": [
                  {
                    "id": "T_CRN",
                    "text": "\u02C7"
                  }
                ]
              },
              {
                "width": "40",
                "id": "T_SPC",
                "sp": "10"
              }
            ]
          },
          {
            "id": "4",
            "key": [
              {
                "nextlayer": "default",
                "width": "120",
                "id": "K_SHIFT",
                "pad": "50",
                "sp": "2",
                "text": "*Shift*"
              },
              {
                "id": "K_Z",
                "text": "Z"
              },
              {
                "id": "T_X",
                "text": "\u1ECC",
                "sk": [
                  {
                    "id": "U_1ECC_0300",
                    "text": "\u1ECC\u0300"
                  },
                  {
                    "id": "U_1ED8",
                    "text": "\u1ED8"
                  },
                  {
                    "id": "U_0058",
                    "text": "X"
                  }
                ]
              },
              {
                "id": "T_C",
                "text": "N\u0304",
                "sk": [
                  {
                    "id": "U_00D1",
                    "text": "\u00D1"
                  },
                  {
                    "id": "U_0043",
                    "text": "C"
                  }
                ]
              },
              {
                "id": "K_V",
                "text": "V"
              },
              {
                "id": "K_B",
                "text": "B"
              },
              {
                "id": "K_N",
                "text": "N",
                "sk": [
                  {
                    "id": "U_01F8",
                    "text": "\u01F8"
                  }
                ]
              },
              {
                "id": "K_M",
                "text": "M",
                "sk": [
                  {
                    "id": "U_004D_0300",
                    "text": "M\u0300"
                  }
                ]
              },
              {
                "id": "U_002D",
                "text": "-",
                "sk": [
                  {
                    "id": "U_0021",
                    "text": "!"
                  },
                  {
                    "id": "U_002F",
                    "text": "/"
                  },
                  {
                    "id": "U_0027",
                    "text": "'"
                  },
                  {
                    "id": "U_0022",
                    "text": "\""
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
                    "id": "U_003F",
                    "text": "?"
                  }
                ]
              },
              {
                "width": "120",
                "id": "K_BKSP",
                "sp": "1",
                "text": "*BkSp*"
              },
              {
                "width": "20",
                "id": "T_SPD",
                "sp": "10"
              }
            ]
          },
          {
            "id": "5",
            "key": [
              {
                "nextlayer": "alt",
                "width": "120",
                "id": "K_ALT",
                "sp": "1",
                "text": "*Symbol*"
              },
              {
                "id": "K_LOPT",
                "sp": "1",
                "text": "*Menu*"
              },
              {
                "id": "U_002C",
                "text": ",",
                "sk": [
                  {
                    "id": "U_003B",
                    "text": ";"
                  }
                ]
              },
              {
                "width": "520",
                "id": "K_SPACE"
              },
              {
                "id": "U_002E",
                "text": ".",
                "sk": [
                  {
                    "id": "U_003A",
                    "text": ":"
                  }
                ]
              },
              {
                "id": "T_ENG",
                "sp": "1",
                "text": "\u2A37",
                "sk": [
                  {
                    "id": "U_2013",
                    "text": "\u2013"
                  },
                  {
                    "id": "U_2014",
                    "text": "\u2014"
                  },
                  {
                    "id": "U_005F",
                    "text": "_"
                  }
                ]
              },
              {
                "width": "120",
                "id": "K_ENTER",
                "sp": "1",
                "text": "*Enter*"
              }
            ]
          }
        ]
      },
      {
        "id": "alt",
        "row": [
          {
            "id": "1",
            "key": [
              {
                "layer": "default",
                "width": "111",
                "id": "K_1",
                "text": "1",
                "sk": [
                  {
                    "id": "U_00B9",
                    "text": "\u00B9"
                  },
                  {
                    "id": "U_2081",
                    "text": "\u2081"
                  }
                ]
              },
              {
                "layer": "default",
                "width": "111",
                "id": "K_2",
                "text": "2",
                "sk": [
                  {
                    "id": "U_00B2",
                    "text": "\u00B2"
                  },
                  {
                    "id": "U_2082",
                    "text": "\u2082"
                  }
                ]
              },
              {
                "layer": "default",
                "width": "111",
                "id": "K_3",
                "text": "3",
                "sk": [
                  {
                    "id": "U_00B3",
                    "text": "\u00B3"
                  },
                  {
                    "id": "U_2083",
                    "text": "\u2083"
                  }
                ]
              },
              {
                "layer": "default",
                "width": "111",
                "id": "K_4",
                "text": "4",
                "sk": [
                  {
                    "id": "U_2074",
                    "text": "\u2074"
                  },
                  {
                    "id": "U_2084",
                    "text": "\u2084"
                  }
                ]
              },
              {
                "layer": "default",
                "width": "111",
                "id": "K_5",
                "text": "5",
                "sk": [
                  {
                    "id": "U_2075",
                    "text": "\u2075"
                  },
                  {
                    "id": "U_2085",
                    "text": "\u2085"
                  }
                ]
              },
              {
                "layer": "default",
                "width": "111",
                "id": "K_6",
                "text": "6",
                "sk": [
                  {
                    "id": "U_2076",
                    "text": "\u2076"
                  },
                  {
                    "id": "U_2086",
                    "text": "\u2086"
                  }
                ]
              },
              {
                "layer": "default",
                "width": "111",
                "id": "K_7",
                "text": "7",
                "sk": [
                  {
                    "id": "U_2077",
                    "text": "\u2077"
                  },
                  {
                    "id": "U_2087",
                    "text": "\u2087"
                  }
                ]
              },
              {
                "layer": "default",
                "width": "111",
                "id": "K_8",
                "text": "8",
                "sk": [
                  {
                    "id": "U_2078",
                    "text": "\u2078"
                  },
                  {
                    "id": "U_2088",
                    "text": "\u2088"
                  }
                ]
              },
              {
                "layer": "default",
                "width": "111",
                "id": "K_9",
                "text": "9",
                "sk": [
                  {
                    "id": "U_2079",
                    "text": "\u2079"
                  },
                  {
                    "id": "U_2089",
                    "text": "\u2089"
                  }
                ]
              },
              {
                "layer": "default",
                "width": "111",
                "id": "K_0",
                "text": "0",
                "sk": [
                  {
                    "id": "U_2070",
                    "text": "\u2070"
                  },
                  {
                    "id": "U_2080",
                    "text": "\u2080"
                  }
                ]
              }
            ]
          },
          {
            "id": "2",
            "key": [
              {
                "id": "U_002B",
                "text": "+",
                "sk": [
                  {
                    "id": "U_00B1",
                    "text": "\u00B1"
                  }
                ]
              },
              {
                "id": "U_002D",
                "text": "-",
                "sk": [
                  {
                    "id": "U_2213",
                    "text": "\u2213"
                  }
                ]
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
                "id": "U_2236",
                "text": "\u2236",
                "sk": [
                  {
                    "id": "U_00B7",
                    "text": "\u00B7"
                  },
                  {
                    "id": "U_2234",
                    "text": "\u2234"
                  },
                  {
                    "id": "U_2235",
                    "text": "\u2235"
                  }
                ]
              },
              {
                "id": "U_221A",
                "text": "\u221A",
                "sk": [
                  {
                    "id": "U_221B",
                    "text": "\u221B"
                  },
                  {
                    "id": "U_221C",
                    "text": "\u221C"
                  }
                ]
              },
              {
                "id": "U_005E",
                "text": "^"
              },
              {
                "id": "U_003D",
                "text": "=",
                "sk": [
                  {
                    "id": "U_2260",
                    "text": "\u2260"
                  },
                  {
                    "id": "U_2248",
                    "text": "\u2248"
                  },
                  {
                    "id": "U_2261",
                    "text": "\u2261"
                  }
                ]
              },
              {
                "id": "U_007E",
                "text": "~"
              },
              {
                "id": "U_203D",
                "text": "\u203D",
                "sk": [
                  {
                    "id": "U_00A1",
                    "text": "\u00A1"
                  }
                ]
              },
              {
                "id": "U_2713",
                "text": "\u2713",
                "sk": [
                  {
                    "id": "U_0394",
                    "text": "\u0394"
                  },
                  {
                    "id": "U_2717",
                    "text": "\u2717"
                  },
                  {
                    "id": "U_26EF",
                    "text": "\u26EF"
                  },
                  {
                    "id": "U_26ED",
                    "text": "\u26ED"
                  }
                ]
              }
            ]
          },
          {
            "id": "3",
            "key": [
              {
                "id": "U_007B",
                "text": "{"
              },
              {
                "id": "U_00AB",
                "text": "\u00AB"
              },
              {
                "id": "U_003C",
                "text": "<",
                "sk": [
                  {
                    "id": "U_226A",
                    "text": "\u226A"
                  },
                  {
                    "id": "U_2264",
                    "text": "\u2264"
                  }
                ]
              },
              {
                "id": "U_005B",
                "text": "["
              },
              {
                "id": "U_0028",
                "text": "("
              },
              {
                "id": "U_007C",
                "text": "|"
              },
              {
                "id": "U_0029",
                "text": ")"
              },
              {
                "id": "U_005D",
                "text": "]"
              },
              {
                "id": "U_003E",
                "text": ">",
                "sk": [
                  {
                    "id": "U_226B",
                    "text": "\u226B"
                  },
                  {
                    "id": "U_2265",
                    "text": "\u2265"
                  }
                ]
              },
              {
                "id": "U_00BB",
                "text": "\u00BB"
              },
              {
                "id": "U_007D",
                "text": "}"
              }
            ]
          },
          {
            "id": "4",
            "key": [
              {
                "nextlayer": "shift",
                "width": "120",
                "id": "K_SHIFT",
                "pad": "50",
                "sp": "1",
                "text": "*Shift*"
              },
              {
                "id": "U_221E",
                "text": "\u221E",
                "sk": [
                  {
                    "id": "U_2202",
                    "text": "\u2202"
                  }
                ]
              },
              {
                "id": "U_222B",
                "text": "\u222B",
                "sk": [
                  {
                    "id": "U_1E8B",
                    "text": "\u1E8B"
                  },
                  {
                    "id": "U_1E8D",
                    "text": "\u1E8D"
                  },
                  {
                    "id": "U_1E8F",
                    "text": "\u1E8F"
                  },
                  {
                    "id": "U_04F1",
                    "text": "\u04F1"
                  }
                ]
              },
              {
                "id": "U_00B0",
                "text": "\u00B0",
                "sk": [
                  {
                    "id": "U_2103",
                    "text": "\u2103"
                  },
                  {
                    "id": "U_2109",
                    "text": "\u2109"
                  }
                ]
              },
              {
                "id": "U_03C0",
                "text": "\u03C0"
              },
              {
                "id": "U_03B8",
                "text": "\u03B8"
              },
              {
                "id": "U_33D1",
                "text": "\u33D1",
                "sk": [
                  {
                    "id": "U_33D2",
                    "text": "\u33D2"
                  }
                ]
              },
              {
                "id": "U_00A9",
                "text": "\u00A9",
                "sk": [
                  {
                    "id": "U_00AE",
                    "text": "\u00AE"
                  },
                  {
                    "id": "U_2117",
                    "text": "\u2117"
                  },
                  {
                    "id": "U_1F6C8",
                    "text": "\uD83D\uDEC8"
                  },
                  {
                    "id": "U_2122",
                    "text": "\u2122"
                  },
                  {
                    "id": "U_2105",
                    "text": "\u2105"
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
                  }
                ]
              },
              {
                "width": "120",
                "id": "K_BKSP",
                "sp": "1",
                "text": "*BkSp*"
              },
              {
                "width": "20",
                "id": "T_SPF",
                "sp": "10"
              }
            ]
          },
          {
            "id": "5",
            "key": [
              {
                "nextlayer": "default",
                "width": "120",
                "id": "K_ALT",
                "sp": "1",
                "text": "*abc*"
              },
              {
                "id": "K_LOPT",
                "sp": "1",
                "text": "*Menu*"
              },
              {
                "id": "U_00A7",
                "text": "\u00A7",
                "sk": [
                  {
                    "id": "U_208D",
                    "text": "\u208D"
                  },
                  {
                    "id": "U_2093",
                    "text": "\u2093"
                  },
                  {
                    "id": "U_208A",
                    "text": "\u208A"
                  },
                  {
                    "id": "U_208B",
                    "text": "\u208B"
                  },
                  {
                    "id": "U_208E",
                    "text": "\u208E"
                  },
                  {
                    "id": "U_208C",
                    "text": "\u208C"
                  }
                ]
              },
              {
                "layer": "default",
                "width": "520",
                "id": "K_SPACE"
              },
              {
                "id": "U_00B6",
                "text": "\u00B6",
                "sk": [
                  {
                    "id": "U_207D",
                    "text": "\u207D"
                  },
                  {
                    "id": "U_207F",
                    "text": "\u207F"
                  },
                  {
                    "id": "U_207A",
                    "text": "\u207A"
                  },
                  {
                    "id": "U_207B",
                    "text": "\u207B"
                  },
                  {
                    "id": "U_207E",
                    "text": "\u207E"
                  },
                  {
                    "id": "U_207C",
                    "text": "\u207C"
                  }
                ]
              },
              {
                "id": "U_03A9",
                "sp": "1",
                "text": "\u03A9",
                "sk": [
                  {
                    "id": "U_265A",
                    "text": "\u265A"
                  },
                  {
                    "id": "U_2654",
                    "text": "\u2654"
                  },
                  {
                    "id": "U_265B",
                    "text": "\u265B"
                  },
                  {
                    "id": "U_2655",
                    "text": "\u2655"
                  },
                  {
                    "id": "U_265D",
                    "text": "\u265D"
                  },
                  {
                    "id": "U_2657",
                    "text": "\u2657"
                  },
                  {
                    "id": "U_265C",
                    "text": "\u265C"
                  },
                  {
                    "id": "U_2656",
                    "text": "\u2656"
                  },
                  {
                    "id": "U_265E",
                    "text": "\u265E"
                  },
                  {
                    "id": "U_2658",
                    "text": "\u2658"
                  },
                  {
                    "id": "U_265F",
                    "text": "\u265F"
                  },
                  {
                    "id": "U_2659",
                    "text": "\u2659"
                  }
                ]
              },
              {
                "width": "120",
                "id": "K_ENTER",
                "sp": "1",
                "text": "*Enter*"
              }
            ]
          }
        ]
      },
      {
        "id": "caps",
        "row": [
          {
            "id": "1",
            "key": [
              {
                "width": "111",
                "id": "K_1",
                "text": "1"
              },
              {
                "width": "111",
                "id": "K_2",
                "text": "2"
              },
              {
                "width": "111",
                "id": "K_3",
                "text": "3"
              },
              {
                "width": "111",
                "id": "K_4",
                "text": "4"
              },
              {
                "width": "111",
                "id": "K_5",
                "text": "5"
              },
              {
                "width": "111",
                "id": "K_6",
                "text": "6"
              },
              {
                "width": "111",
                "id": "K_7",
                "text": "7"
              },
              {
                "width": "111",
                "id": "K_8",
                "text": "8"
              },
              {
                "width": "111",
                "id": "K_9",
                "text": "9"
              },
              {
                "width": "111",
                "id": "K_0",
                "text": "0"
              }
            ]
          },
          {
            "id": "2",
            "key": [
              {
                "id": "T_Q",
                "text": "CH",
                "sk": [
                  {
                    "id": "U_0051",
                    "text": "Q"
                  }
                ]
              },
              {
                "id": "K_W",
                "text": "W"
              },
              {
                "id": "K_E",
                "text": "E",
                "sk": [
                  {
                    "id": "U_00C8",
                    "text": "\u00C8"
                  },
                  {
                    "id": "U_00CA",
                    "text": "\u00CA"
                  }
                ]
              },
              {
                "id": "K_R",
                "text": "R"
              },
              {
                "id": "K_T",
                "text": "T"
              },
              {
                "id": "K_Y",
                "text": "Y"
              },
              {
                "id": "K_U",
                "text": "U",
                "sk": [
                  {
                    "id": "U_00D9",
                    "text": "\u00D9"
                  },
                  {
                    "id": "U_00DB",
                    "text": "\u00DB"
                  }
                ]
              },
              {
                "id": "K_I",
                "text": "I",
                "sk": [
                  {
                    "id": "U_00CC",
                    "text": "\u00CC"
                  },
                  {
                    "id": "U_00CE",
                    "text": "\u00CE"
                  }
                ]
              },
              {
                "id": "K_O",
                "text": "O",
                "sk": [
                  {
                    "id": "U_00D2",
                    "text": "\u00D2"
                  },
                  {
                    "id": "U_00D4",
                    "text": "\u00D4"
                  }
                ]
              },
              {
                "id": "K_P",
                "text": "P"
              },
              {
                "id": "T_GRV",
                "text": "`",
                "sk": [
                  {
                    "id": "T_ACU",
                    "text": "\u00B4"
                  }
                ]
              }
            ]
          },
          {
            "id": "3",
            "key": [
              {
                "id": "K_A",
                "pad": "70",
                "text": "A",
                "sk": [
                  {
                    "id": "U_00C0",
                    "text": "\u00C0"
                  },
                  {
                    "id": "U_00C2",
                    "text": "\u00C2"
                  }
                ]
              },
              {
                "id": "K_S",
                "text": "S"
              },
              {
                "id": "K_D",
                "text": "D"
              },
              {
                "id": "K_F",
                "text": "F"
              },
              {
                "id": "K_G",
                "text": "G"
              },
              {
                "id": "T_H",
                "text": "SH",
                "sk": [
                  {
                    "id": "U_0048",
                    "text": "H"
                  },
                  {
                    "id": "U_1E62",
                    "text": "\u1E62"
                  }
                ]
              },
              {
                "id": "K_J",
                "text": "J"
              },
              {
                "id": "K_K",
                "text": "K"
              },
              {
                "id": "K_L",
                "text": "L"
              },
              {
                "id": "T_CCM",
                "text": "\u02C6",
                "sk": [
                  {
                    "id": "T_CRN",
                    "text": "\u02C7"
                  }
                ]
              },
              {
                "width": "40",
                "id": "T_SPC",
                "sp": "10"
              }
            ]
          },
          {
            "id": "4",
            "key": [
              {
                "nextlayer": "default",
                "width": "120",
                "id": "K_SHIFT",
                "pad": "50",
                "sp": "2",
                "text": "*Shift*"
              },
              {
                "id": "K_Z",
                "text": "Z"
              },
              {
                "id": "T_X",
                "text": "\u1ECC",
                "sk": [
                  {
                    "id": "U_1ECC_0300",
                    "text": "\u1ECC\u0300"
                  },
                  {
                    "id": "U_1ED8",
                    "text": "\u1ED8"
                  },
                  {
                    "id": "U_0058",
                    "text": "X"
                  }
                ]
              },
              {
                "id": "T_C",
                "text": "N\u0304",
                "sk": [
                  {
                    "id": "U_00D1",
                    "text": "\u00D1"
                  },
                  {
                    "id": "U_0043",
                    "text": "C"
                  }
                ]
              },
              {
                "id": "K_V",
                "text": "V"
              },
              {
                "id": "K_B",
                "text": "B"
              },
              {
                "id": "K_N",
                "text": "N",
                "sk": [
                  {
                    "id": "U_01F8",
                    "text": "\u01F8"
                  }
                ]
              },
              {
                "id": "K_M",
                "text": "M",
                "sk": [
                  {
                    "id": "U_004D_0300",
                    "text": "M\u0300"
                  }
                ]
              },
              {
                "id": "U_002D",
                "text": "-",
                "sk": [
                  {
                    "id": "U_0021",
                    "text": "!"
                  },
                  {
                    "id": "U_002F",
                    "text": "/"
                  },
                  {
                    "id": "U_0027",
                    "text": "'"
                  },
                  {
                    "id": "U_0022",
                    "text": "\""
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
                    "id": "U_003F",
                    "text": "?"
                  }
                ]
              },
              {
                "width": "120",
                "id": "K_BKSP",
                "sp": "1",
                "text": "*BkSp*"
              },
              {
                "width": "20",
                "id": "T_SPD",
                "sp": "10"
              }
            ]
          },
          {
            "id": "5",
            "key": [
              {
                "nextlayer": "alt",
                "width": "120",
                "id": "K_ALT",
                "sp": "1",
                "text": "*Symbol*"
              },
              {
                "id": "K_LOPT",
                "sp": "1",
                "text": "*Menu*"
              },
              {
                "id": "U_002C",
                "text": ",",
                "sk": [
                  {
                    "id": "U_003B",
                    "text": ";"
                  }
                ]
              },
              {
                "width": "520",
                "id": "K_SPACE"
              },
              {
                "id": "U_002E",
                "text": ".",
                "sk": [
                  {
                    "id": "U_003A",
                    "text": ":"
                  }
                ]
              },
              {
                "id": "T_ENG",
                "sp": "1",
                "text": "\u2A37",
                "sk": [
                  {
                    "id": "U_2013",
                    "text": "\u2013"
                  },
                  {
                    "id": "U_2014",
                    "text": "\u2014"
                  },
                  {
                    "id": "U_005F",
                    "text": "_"
                  }
                ]
              },
              {
                "width": "120",
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
  this.KCSS="/* tone keys colour */\n .android .kmw-keyboard-obolo_chwerty_6351 #default-T_GRV {color: #02ff02;}\n .android .kmw-keyboard-obolo_chwerty_6351 #default-T_CCM {color: #02ff02;}\n .android .kmw-keyboard-obolo_chwerty_6351 #shift-T_GRV {color: #02ff02;}\n .android .kmw-keyboard-obolo_chwerty_6351 #shift-T_CCM {color: #02ff02;}\n .android .kmw-keyboard-obolo_chwerty_6351 #caps-T_GRV {color: #02ff02;}\n .android .kmw-keyboard-obolo_chwerty_6351 #caps-T_CCM {color: #02ff02;}\n ";
  this.s_digit_14="0123456789";
  this.s_sentencePunctuation_18=".?!‽";
  this.s_accentibles_19="aeiọoumnAEIỌOUMNn̄";
  this.s_accents_20=['','','','','','','','','','','',''];
  this.s_nasals_21="mnMN";
  this.s_nnas_22=['','','','','',''];
  this.s_vow_23="aeiouAEIOU";
  this.s_grave_24="àèìòùÀÈÌÒÙ";
  this.s_acute_25="áéíóúÁÉÍÓÚ";
  this.s_circum_26="âêîôûÂÊÎÔÛ";
  this.s_caron_27="ǎěǐǒǔǍĚǏǑǓ";
  this.s_acnasals_28="ḿńḾŃ";
  this.s_spguy_29="mnọMNỌ";
  this.s_spguyop_30="ḿǹộḾǸỘ";
  this.s_erase_31=['','','',''];
  this.s_gravekeys_32=['','','',''];
  this.s_acutekeys_33=['','','',''];
  this.s_circumflexkeys_34=['','','',''];
  this.s_caronkeys_35=['','','',''];
  this.s41="";
  this.s42="numeric";
  this.s43="caps";
  this.s44="";
  this.s45="shift";
  this.s46="shift";
  this.s47="shift";
  this.s48="default";
  this.KVER="15.0.213.0";
  this.KVS=[];
  this.gs=function(t,e) {
    return this.g_main_3(t,e);
  };
  this.gn=function(t,e) {
    return this.g_NewContext_0(t,e);
  };
  this.gpk=function(t,e) {
    return this.g_PostKeystroke_1(t,e);
  };
  this.gs=function(t,e) {
    return this.g_main_3(t,e);
  };
  this.g_NewContext_0=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
    if(!m) {
    
      k.KDC(-1,t);
      r=this.g_detectStartOfSentence_2(t,e);
      m=2;
    }
    return r;
  };
  this.g_PostKeystroke_1=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
      if(k.KFCM(1,t,[{t:'a',a:this.s_digit_14}])&&k.KIFS(42,this.s41,t)&&k.KIFS(33,this.s42,t)){
        m=1;   // Line 24
        k.KDC(1,t);
        k.KIO(-1,this.s_digit_14,1,t);
      }
      else if(k.KIFS(33,this.s43,t)){
        m=1;   // Line 25
        k.KDC(0,t);
      }
      else if(k.KIFS(42,this.s44,t)){
        m=1;   // Line 26
        k.KDC(0,t);
        r=this.g_detectStartOfSentence_2(t,e);
        m=2;
      }
    return r;
  };
  this.g_detectStartOfSentence_2=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
      if(k.KFCM(3,t,[{t:'a',a:this.s_sentencePunctuation_18},' ',' '])){
        m=1;   // Line 31
        k.KDC(3,t);
        k.KIO(-1,this.s_sentencePunctuation_18,1,t);
        k.KO(-1,t,"  ");
        k.KSETS(33,this.s47,t);
      }
      else if(k.KFCM(2,t,[{t:'a',a:this.s_sentencePunctuation_18},' '])){
        m=1;   // Line 30
        k.KDC(2,t);
        k.KIO(-1,this.s_sentencePunctuation_18,1,t);
        k.KO(-1,t," ");
        k.KSETS(33,this.s46,t);
      }
      else if(k.KFCM(1,t,[{t:'n'}])){
        m=1;   // Line 29
        k.KDC(0,t);
        k.KSETS(33,this.s45,t);
      }
    if(!m) {
    
      k.KDC(-1,t);
      k.KSETS(33,this.s48,t);
    }
    return r;
  };
  this.g_main_3=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_BKSP /* 0x08 */)) {
      if(k.KFCM(2,t,['c','h'])){
        r=m=1;   // Line 237
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['C','h'])){
        r=m=1;   // Line 238
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['C','H'])){
        r=m=1;   // Line 239
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['s','h'])){
        r=m=1;   // Line 240
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['S','h'])){
        r=m=1;   // Line 241
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['S','H'])){
        r=m=1;   // Line 242
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['n','̄'])){
        r=m=1;   // Line 243
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['N','̄'])){
        r=m=1;   // Line 244
        k.KDC(2,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_grave_24}])){
        r=m=1;   // Line 247
        k.KDC(1,t);
        k.KIO(-1,this.s_vow_23,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_acute_25}])){
        r=m=1;   // Line 248
        k.KDC(1,t);
        k.KIO(-1,this.s_vow_23,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_circum_26}])){
        r=m=1;   // Line 249
        k.KDC(1,t);
        k.KIO(-1,this.s_vow_23,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_caron_27}])){
        r=m=1;   // Line 250
        k.KDC(1,t);
        k.KIO(-1,this.s_vow_23,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_spguyop_30}])){
        r=m=1;   // Line 251
        k.KDC(1,t);
        k.KIO(-1,this.s_spguy_29,1,t);
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_BKSP /* 0x08 */)) {
      if(k.KFCM(2,t,['c','h'])){
        r=m=1;   // Line 237
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['C','h'])){
        r=m=1;   // Line 238
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['C','H'])){
        r=m=1;   // Line 239
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['s','h'])){
        r=m=1;   // Line 240
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['S','h'])){
        r=m=1;   // Line 241
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['S','H'])){
        r=m=1;   // Line 242
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['n','̄'])){
        r=m=1;   // Line 243
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['N','̄'])){
        r=m=1;   // Line 244
        k.KDC(2,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_grave_24}])){
        r=m=1;   // Line 247
        k.KDC(1,t);
        k.KIO(-1,this.s_vow_23,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_acute_25}])){
        r=m=1;   // Line 248
        k.KDC(1,t);
        k.KIO(-1,this.s_vow_23,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_circum_26}])){
        r=m=1;   // Line 249
        k.KDC(1,t);
        k.KIO(-1,this.s_vow_23,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_caron_27}])){
        r=m=1;   // Line 250
        k.KDC(1,t);
        k.KIO(-1,this.s_vow_23,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_spguyop_30}])){
        r=m=1;   // Line 251
        k.KDC(1,t);
        k.KIO(-1,this.s_spguy_29,1,t);
      }
    }
    else if(k.KKM(e, modCodes.ALT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4240 */, keyCodes.K_BKSP /* 0x08 */)) {
      if(k.KFCM(2,t,['c','h'])){
        r=m=1;   // Line 237
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['C','h'])){
        r=m=1;   // Line 238
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['C','H'])){
        r=m=1;   // Line 239
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['s','h'])){
        r=m=1;   // Line 240
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['S','h'])){
        r=m=1;   // Line 241
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['S','H'])){
        r=m=1;   // Line 242
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['n','̄'])){
        r=m=1;   // Line 243
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['N','̄'])){
        r=m=1;   // Line 244
        k.KDC(2,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_grave_24}])){
        r=m=1;   // Line 247
        k.KDC(1,t);
        k.KIO(-1,this.s_vow_23,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_acute_25}])){
        r=m=1;   // Line 248
        k.KDC(1,t);
        k.KIO(-1,this.s_vow_23,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_circum_26}])){
        r=m=1;   // Line 249
        k.KDC(1,t);
        k.KIO(-1,this.s_vow_23,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_caron_27}])){
        r=m=1;   // Line 250
        k.KDC(1,t);
        k.KIO(-1,this.s_vow_23,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_spguyop_30}])){
        r=m=1;   // Line 251
        k.KDC(1,t);
        k.KIO(-1,this.s_spguy_29,1,t);
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_BKSP /* 0x08 */)) {
      if(k.KFCM(2,t,['c','h'])){
        r=m=1;   // Line 237
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['C','h'])){
        r=m=1;   // Line 238
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['C','H'])){
        r=m=1;   // Line 239
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['s','h'])){
        r=m=1;   // Line 240
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['S','h'])){
        r=m=1;   // Line 241
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['S','H'])){
        r=m=1;   // Line 242
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['n','̄'])){
        r=m=1;   // Line 243
        k.KDC(2,t);
      }
      else if(k.KFCM(2,t,['N','̄'])){
        r=m=1;   // Line 244
        k.KDC(2,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_grave_24}])){
        r=m=1;   // Line 247
        k.KDC(1,t);
        k.KIO(-1,this.s_vow_23,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_acute_25}])){
        r=m=1;   // Line 248
        k.KDC(1,t);
        k.KIO(-1,this.s_vow_23,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_circum_26}])){
        r=m=1;   // Line 249
        k.KDC(1,t);
        k.KIO(-1,this.s_vow_23,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_caron_27}])){
        r=m=1;   // Line 250
        k.KDC(1,t);
        k.KIO(-1,this.s_vow_23,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_spguyop_30}])){
        r=m=1;   // Line 251
        k.KDC(1,t);
        k.KIO(-1,this.s_spguy_29,1,t);
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, 0x105)) {
      if(k.KFCM(1,t,[{t:'d',d:0}])){
        r=m=1;   // Line 163
        k.KDC(1,t);
        k.KO(-1,t,"q");
      }
      else if(1){
        r=m=1;   // Line 109
        k.KDC(0,t);
        k.KO(-1,t,"ch");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, 0x105)) {
      if(k.KFCM(1,t,[{t:'d',d:1}])){
        r=m=1;   // Line 164
        k.KDC(1,t);
        k.KO(-1,t,"Q");
      }
      else if(1){
        r=m=1;   // Line 110
        k.KDC(0,t);
        k.KO(-1,t,"Ch");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, 0x105)) {
      if(k.KFCM(1,t,[{t:'d',d:2}])){
        r=m=1;   // Line 165
        k.KDC(1,t);
        k.KO(-1,t,"Q");
      }
      else if(1){
        r=m=1;   // Line 111
        k.KDC(0,t);
        k.KO(-1,t,"CH");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, 0x106)) {
      if(k.KFCM(1,t,[{t:'d',d:0}])){
        r=m=1;   // Line 167
        k.KDC(1,t);
        k.KO(-1,t,"h");
      }
      else if(1){
        r=m=1;   // Line 113
        k.KDC(0,t);
        k.KO(-1,t,"sh");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, 0x106)) {
      if(k.KFCM(1,t,[{t:'d',d:1}])){
        r=m=1;   // Line 168
        k.KDC(1,t);
        k.KO(-1,t,"H");
      }
      else if(1){
        r=m=1;   // Line 114
        k.KDC(0,t);
        k.KO(-1,t,"Sh");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, 0x106)) {
      if(k.KFCM(1,t,[{t:'d',d:2}])){
        r=m=1;   // Line 169
        k.KDC(1,t);
        k.KO(-1,t,"H");
      }
      else if(1){
        r=m=1;   // Line 115
        k.KDC(0,t);
        k.KO(-1,t,"SH");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, 0x107)) {
      if(k.KFCM(1,t,[{t:'d',d:0}])){
        r=m=1;   // Line 171
        k.KDC(1,t);
        k.KO(-1,t,"x");
      }
      else if(1){
        r=m=1;   // Line 117
        k.KDC(0,t);
        k.KO(-1,t,"ọ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, 0x107)) {
      if(k.KFCM(1,t,[{t:'d',d:1}])){
        r=m=1;   // Line 172
        k.KDC(1,t);
        k.KO(-1,t,"X");
      }
      else if(1){
        r=m=1;   // Line 118
        k.KDC(0,t);
        k.KO(-1,t,"Ọ");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, 0x107)) {
      if(k.KFCM(1,t,[{t:'d',d:2}])){
        r=m=1;   // Line 173
        k.KDC(1,t);
        k.KO(-1,t,"X");
      }
      else if(1){
        r=m=1;   // Line 119
        k.KDC(0,t);
        k.KO(-1,t,"Ọ");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, 0x108)) {
      if(k.KFCM(1,t,[{t:'d',d:0}])){
        r=m=1;   // Line 175
        k.KDC(1,t);
        k.KO(-1,t,"c");
      }
      else if(1){
        r=m=1;   // Line 121
        k.KDC(0,t);
        k.KO(-1,t,"n̄");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, 0x108)) {
      if(k.KFCM(1,t,[{t:'d',d:1}])){
        r=m=1;   // Line 176
        k.KDC(1,t);
        k.KO(-1,t,"C");
      }
      else if(1){
        r=m=1;   // Line 122
        k.KDC(0,t);
        k.KO(-1,t,"N̄");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, 0x108)) {
      if(k.KFCM(1,t,[{t:'d',d:2}])){
        r=m=1;   // Line 177
        k.KDC(1,t);
        k.KO(-1,t,"C");
      }
      else if(1){
        r=m=1;   // Line 123
        k.KDC(0,t);
        k.KO(-1,t,"N̄");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, 0x100)) {
      if(k.KFCM(1,t,[{t:'d',d:0}])){
        r=m=1;   // Line 184
        k.KDC(1,t);
        k.KO(-1,t,"`");
      }
      else if(k.KFCM(1,t,['m'])){
        r=m=1;   // Line 205
        k.KDC(1,t);
        k.KO(-1,t,"m̀");
      }
      else if(k.KFCM(1,t,['M'])){
        r=m=1;   // Line 206
        k.KDC(1,t);
        k.KO(-1,t,"M̀");
      }
      else if(k.KFCM(1,t,['n'])){
        r=m=1;   // Line 208
        k.KDC(1,t);
        k.KO(-1,t,"ǹ");
      }
      else if(k.KFCM(1,t,['N'])){
        r=m=1;   // Line 209
        k.KDC(1,t);
        k.KO(-1,t,"Ǹ");
      }
      else if(k.KFCM(1,t,['ọ'])){
        r=m=1;   // Line 211
        k.KDC(1,t);
        k.KO(-1,t,"ọ̀");
      }
      else if(k.KFCM(1,t,['Ọ'])){
        r=m=1;   // Line 212
        k.KDC(1,t);
        k.KO(-1,t,"Ọ̀");
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_vow_23}])){
        r=m=1;   // Line 230
        k.KDC(1,t);
        k.KIO(-1,this.s_grave_24,1,t);
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, 0x100)) {
      if(k.KFCM(1,t,[{t:'d',d:1}])){
        r=m=1;   // Line 185
        k.KDC(1,t);
        k.KO(-1,t,"`");
      }
      else if(k.KFCM(1,t,['m'])){
        r=m=1;   // Line 205
        k.KDC(1,t);
        k.KO(-1,t,"m̀");
      }
      else if(k.KFCM(1,t,['M'])){
        r=m=1;   // Line 206
        k.KDC(1,t);
        k.KO(-1,t,"M̀");
      }
      else if(k.KFCM(1,t,['n'])){
        r=m=1;   // Line 208
        k.KDC(1,t);
        k.KO(-1,t,"ǹ");
      }
      else if(k.KFCM(1,t,['N'])){
        r=m=1;   // Line 209
        k.KDC(1,t);
        k.KO(-1,t,"Ǹ");
      }
      else if(k.KFCM(1,t,['ọ'])){
        r=m=1;   // Line 211
        k.KDC(1,t);
        k.KO(-1,t,"ọ̀");
      }
      else if(k.KFCM(1,t,['Ọ'])){
        r=m=1;   // Line 212
        k.KDC(1,t);
        k.KO(-1,t,"Ọ̀");
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_vow_23}])){
        r=m=1;   // Line 230
        k.KDC(1,t);
        k.KIO(-1,this.s_grave_24,1,t);
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, 0x100)) {
      if(k.KFCM(1,t,[{t:'d',d:2}])){
        r=m=1;   // Line 186
        k.KDC(1,t);
        k.KO(-1,t,"`");
      }
      else if(k.KFCM(1,t,['m'])){
        r=m=1;   // Line 205
        k.KDC(1,t);
        k.KO(-1,t,"m̀");
      }
      else if(k.KFCM(1,t,['M'])){
        r=m=1;   // Line 206
        k.KDC(1,t);
        k.KO(-1,t,"M̀");
      }
      else if(k.KFCM(1,t,['n'])){
        r=m=1;   // Line 208
        k.KDC(1,t);
        k.KO(-1,t,"ǹ");
      }
      else if(k.KFCM(1,t,['N'])){
        r=m=1;   // Line 209
        k.KDC(1,t);
        k.KO(-1,t,"Ǹ");
      }
      else if(k.KFCM(1,t,['ọ'])){
        r=m=1;   // Line 211
        k.KDC(1,t);
        k.KO(-1,t,"ọ̀");
      }
      else if(k.KFCM(1,t,['Ọ'])){
        r=m=1;   // Line 212
        k.KDC(1,t);
        k.KO(-1,t,"Ọ̀");
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_vow_23}])){
        r=m=1;   // Line 230
        k.KDC(1,t);
        k.KIO(-1,this.s_grave_24,1,t);
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, 0x101)) {
      if(k.KFCM(1,t,[{t:'d',d:0}])){
        r=m=1;   // Line 188
        k.KDC(1,t);
        k.KO(-1,t,"´");
      }
      else if(k.KFCM(1,t,['ọ'])){
        r=m=1;   // Line 214
        k.KDC(1,t);
        k.KO(-1,t,"ọ́");
      }
      else if(k.KFCM(1,t,['Ọ'])){
        r=m=1;   // Line 215
        k.KDC(1,t);
        k.KO(-1,t,"Ọ́");
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_vow_23}])){
        r=m=1;   // Line 231
        k.KDC(1,t);
        k.KIO(-1,this.s_acute_25,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_nasals_21}])){
        r=m=1;   // Line 234
        k.KDC(1,t);
        k.KIO(-1,this.s_acnasals_28,1,t);
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, 0x101)) {
      if(k.KFCM(1,t,[{t:'d',d:1}])){
        r=m=1;   // Line 189
        k.KDC(1,t);
        k.KO(-1,t,"´");
      }
      else if(k.KFCM(1,t,['ọ'])){
        r=m=1;   // Line 214
        k.KDC(1,t);
        k.KO(-1,t,"ọ́");
      }
      else if(k.KFCM(1,t,['Ọ'])){
        r=m=1;   // Line 215
        k.KDC(1,t);
        k.KO(-1,t,"Ọ́");
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_vow_23}])){
        r=m=1;   // Line 231
        k.KDC(1,t);
        k.KIO(-1,this.s_acute_25,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_nasals_21}])){
        r=m=1;   // Line 234
        k.KDC(1,t);
        k.KIO(-1,this.s_acnasals_28,1,t);
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, 0x101)) {
      if(k.KFCM(1,t,[{t:'d',d:2}])){
        r=m=1;   // Line 190
        k.KDC(1,t);
        k.KO(-1,t,"´");
      }
      else if(k.KFCM(1,t,['ọ'])){
        r=m=1;   // Line 214
        k.KDC(1,t);
        k.KO(-1,t,"ọ́");
      }
      else if(k.KFCM(1,t,['Ọ'])){
        r=m=1;   // Line 215
        k.KDC(1,t);
        k.KO(-1,t,"Ọ́");
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_vow_23}])){
        r=m=1;   // Line 231
        k.KDC(1,t);
        k.KIO(-1,this.s_acute_25,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_nasals_21}])){
        r=m=1;   // Line 234
        k.KDC(1,t);
        k.KIO(-1,this.s_acnasals_28,1,t);
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, 0x102)) {
      if(k.KFCM(1,t,[{t:'d',d:0}])){
        r=m=1;   // Line 192
        k.KDC(1,t);
        k.KO(-1,t,"ˆ");
      }
      else if(k.KFCM(1,t,['ọ'])){
        r=m=1;   // Line 217
        k.KDC(1,t);
        k.KO(-1,t,"ộ");
      }
      else if(k.KFCM(1,t,['Ọ'])){
        r=m=1;   // Line 218
        k.KDC(1,t);
        k.KO(-1,t,"Ộ");
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_vow_23}])){
        r=m=1;   // Line 232
        k.KDC(1,t);
        k.KIO(-1,this.s_circum_26,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_nasals_21}])){
        r=m=1;   // Line 253
        k.KDC(1,t);
        k.KIO(-1,this.s_nasals_21,1,t);
        k.KB(t);
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, 0x102)) {
      if(k.KFCM(1,t,[{t:'d',d:1}])){
        r=m=1;   // Line 193
        k.KDC(1,t);
        k.KO(-1,t,"ˆ");
      }
      else if(k.KFCM(1,t,['ọ'])){
        r=m=1;   // Line 217
        k.KDC(1,t);
        k.KO(-1,t,"ộ");
      }
      else if(k.KFCM(1,t,['Ọ'])){
        r=m=1;   // Line 218
        k.KDC(1,t);
        k.KO(-1,t,"Ộ");
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_vow_23}])){
        r=m=1;   // Line 232
        k.KDC(1,t);
        k.KIO(-1,this.s_circum_26,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_nasals_21}])){
        r=m=1;   // Line 253
        k.KDC(1,t);
        k.KIO(-1,this.s_nasals_21,1,t);
        k.KB(t);
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, 0x102)) {
      if(k.KFCM(1,t,[{t:'d',d:2}])){
        r=m=1;   // Line 194
        k.KDC(1,t);
        k.KO(-1,t,"ˆ");
      }
      else if(k.KFCM(1,t,['ọ'])){
        r=m=1;   // Line 217
        k.KDC(1,t);
        k.KO(-1,t,"ộ");
      }
      else if(k.KFCM(1,t,['Ọ'])){
        r=m=1;   // Line 218
        k.KDC(1,t);
        k.KO(-1,t,"Ộ");
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_vow_23}])){
        r=m=1;   // Line 232
        k.KDC(1,t);
        k.KIO(-1,this.s_circum_26,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_nasals_21}])){
        r=m=1;   // Line 253
        k.KDC(1,t);
        k.KIO(-1,this.s_nasals_21,1,t);
        k.KB(t);
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, 0x103)) {
      if(k.KFCM(1,t,[{t:'d',d:0}])){
        r=m=1;   // Line 196
        k.KDC(1,t);
        k.KO(-1,t,"ˇ");
      }
      else if(k.KFCM(1,t,['ọ'])){
        r=m=1;   // Line 220
        k.KDC(1,t);
        k.KO(-1,t,"ọ̌");
      }
      else if(k.KFCM(1,t,['Ọ'])){
        r=m=1;   // Line 221
        k.KDC(1,t);
        k.KO(-1,t,"Ọ̌");
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_vow_23}])){
        r=m=1;   // Line 233
        k.KDC(1,t);
        k.KIO(-1,this.s_caron_27,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_nasals_21}])){
        r=m=1;   // Line 253
        k.KDC(1,t);
        k.KIO(-1,this.s_nasals_21,1,t);
        k.KB(t);
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, 0x103)) {
      if(k.KFCM(1,t,[{t:'d',d:1}])){
        r=m=1;   // Line 197
        k.KDC(1,t);
        k.KO(-1,t,"ˇ");
      }
      else if(k.KFCM(1,t,['ọ'])){
        r=m=1;   // Line 220
        k.KDC(1,t);
        k.KO(-1,t,"ọ̌");
      }
      else if(k.KFCM(1,t,['Ọ'])){
        r=m=1;   // Line 221
        k.KDC(1,t);
        k.KO(-1,t,"Ọ̌");
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_vow_23}])){
        r=m=1;   // Line 233
        k.KDC(1,t);
        k.KIO(-1,this.s_caron_27,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_nasals_21}])){
        r=m=1;   // Line 253
        k.KDC(1,t);
        k.KIO(-1,this.s_nasals_21,1,t);
        k.KB(t);
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, 0x103)) {
      if(k.KFCM(1,t,[{t:'d',d:2}])){
        r=m=1;   // Line 198
        k.KDC(1,t);
        k.KO(-1,t,"ˇ");
      }
      else if(k.KFCM(1,t,['ọ'])){
        r=m=1;   // Line 220
        k.KDC(1,t);
        k.KO(-1,t,"ọ̌");
      }
      else if(k.KFCM(1,t,['Ọ'])){
        r=m=1;   // Line 221
        k.KDC(1,t);
        k.KO(-1,t,"Ọ̌");
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_vow_23}])){
        r=m=1;   // Line 233
        k.KDC(1,t);
        k.KIO(-1,this.s_caron_27,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_nasals_21}])){
        r=m=1;   // Line 253
        k.KDC(1,t);
        k.KIO(-1,this.s_nasals_21,1,t);
        k.KB(t);
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, 0x104)) {
      if(1){
        r=m=1;   // Line 58
        k.KDC(0,t);
        k.KDO(-1,t,0);
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, 0x104)) {
      if(1){
        r=m=1;   // Line 59
        k.KDC(0,t);
        k.KDO(-1,t,1);
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, 0x104)) {
      if(1){
        r=m=1;   // Line 60
        k.KDC(0,t);
        k.KDO(-1,t,2);
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_HYPHEN /* 0xBD */)) {
      if(k.KFCM(1,t,[{t:'d',d:0}])){
        r=m=1;   // Line 200
        k.KDC(1,t);
        k.KO(-1,t,"¯");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_HYPHEN /* 0xBD */)) {
      if(k.KFCM(1,t,[{t:'d',d:2}])){
        r=m=1;   // Line 202
        k.KDC(1,t);
        k.KO(-1,t,"¯");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_A /* 0x41 */)) {
      if(1){
        r=m=1;   // Line 63
        k.KDC(0,t);
        k.KO(-1,t,"A");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_B /* 0x42 */)) {
      if(1){
        r=m=1;   // Line 64
        k.KDC(0,t);
        k.KO(-1,t,"B");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_C /* 0x43 */)) {
      if(k.KFCM(1,t,[']'])){
        r=m=1;   // Line 133
        k.KDC(1,t);
        k.KO(-1,t,"C");
      }
      else if(1){
        r=m=1;   // Line 89
        k.KDC(0,t);
        k.KO(-1,t,"N̄");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_C /* 0x43 */)) {
      if(k.KFCM(1,t,[']'])){
        r=m=1;   // Line 135
        k.KDC(1,t);
        k.KO(-1,t,"C");
      }
      else if(1){
        r=m=1;   // Line 104
        k.KDC(0,t);
        k.KO(-1,t,"N̄");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_D /* 0x44 */)) {
      if(1){
        r=m=1;   // Line 65
        k.KDC(0,t);
        k.KO(-1,t,"D");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_E /* 0x45 */)) {
      if(1){
        r=m=1;   // Line 66
        k.KDC(0,t);
        k.KO(-1,t,"E");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_F /* 0x46 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"F");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_G /* 0x47 */)) {
      if(1){
        r=m=1;   // Line 68
        k.KDC(0,t);
        k.KO(-1,t,"G");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_H /* 0x48 */)) {
      if(k.KFCM(1,t,[']'])){
        r=m=1;   // Line 143
        k.KDC(1,t);
        k.KO(-1,t,"H");
      }
      else if(1){
        r=m=1;   // Line 87
        k.KDC(0,t);
        k.KO(-1,t,"SH");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_H /* 0x48 */)) {
      if(k.KFCM(1,t,[']'])){
        r=m=1;   // Line 145
        k.KDC(1,t);
        k.KO(-1,t,"H");
      }
      else if(1){
        r=m=1;   // Line 102
        k.KDC(0,t);
        k.KO(-1,t,"Sh");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_I /* 0x49 */)) {
      if(1){
        r=m=1;   // Line 69
        k.KDC(0,t);
        k.KO(-1,t,"I");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_J /* 0x4A */)) {
      if(1){
        r=m=1;   // Line 70
        k.KDC(0,t);
        k.KO(-1,t,"J");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_K /* 0x4B */)) {
      if(1){
        r=m=1;   // Line 71
        k.KDC(0,t);
        k.KO(-1,t,"K");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_L /* 0x4C */)) {
      if(1){
        r=m=1;   // Line 72
        k.KDC(0,t);
        k.KO(-1,t,"L");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_M /* 0x4D */)) {
      if(1){
        r=m=1;   // Line 73
        k.KDC(0,t);
        k.KO(-1,t,"M");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_N /* 0x4E */)) {
      if(1){
        r=m=1;   // Line 74
        k.KDC(0,t);
        k.KO(-1,t,"N");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_O /* 0x4F */)) {
      if(1){
        r=m=1;   // Line 75
        k.KDC(0,t);
        k.KO(-1,t,"O");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_P /* 0x50 */)) {
      if(1){
        r=m=1;   // Line 76
        k.KDC(0,t);
        k.KO(-1,t,"P");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_Q /* 0x51 */)) {
      if(k.KFCM(1,t,[']'])){
        r=m=1;   // Line 148
        k.KDC(1,t);
        k.KO(-1,t,"Q");
      }
      else if(1){
        r=m=1;   // Line 86
        k.KDC(0,t);
        k.KO(-1,t,"CH");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_Q /* 0x51 */)) {
      if(k.KFCM(1,t,[']'])){
        r=m=1;   // Line 150
        k.KDC(1,t);
        k.KO(-1,t,"Q");
      }
      else if(1){
        r=m=1;   // Line 101
        k.KDC(0,t);
        k.KO(-1,t,"Ch");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_R /* 0x52 */)) {
      if(1){
        r=m=1;   // Line 77
        k.KDC(0,t);
        k.KO(-1,t,"R");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_S /* 0x53 */)) {
      if(k.KFCM(1,t,[{t:'d',d:1}])){
        r=m=1;   // Line 180
        k.KDC(1,t);
        k.KO(-1,t,"Ṣ");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_S /* 0x53 */)) {
      if(k.KFCM(1,t,[{t:'d',d:2}])){
        r=m=1;   // Line 181
        k.KDC(1,t);
        k.KO(-1,t,"Ṣ");
      }
      else if(1){
        r=m=1;   // Line 78
        k.KDC(0,t);
        k.KO(-1,t,"S");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_T /* 0x54 */)) {
      if(1){
        r=m=1;   // Line 79
        k.KDC(0,t);
        k.KO(-1,t,"T");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_U /* 0x55 */)) {
      if(1){
        r=m=1;   // Line 80
        k.KDC(0,t);
        k.KO(-1,t,"U");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_V /* 0x56 */)) {
      if(1){
        r=m=1;   // Line 81
        k.KDC(0,t);
        k.KO(-1,t,"V");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_W /* 0x57 */)) {
      if(1){
        r=m=1;   // Line 82
        k.KDC(0,t);
        k.KO(-1,t,"W");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_X /* 0x58 */)) {
      if(k.KFCM(1,t,[']'])){
        r=m=1;   // Line 138
        k.KDC(1,t);
        k.KO(-1,t,"X");
      }
      else if(1){
        r=m=1;   // Line 88
        k.KDC(0,t);
        k.KO(-1,t,"Ọ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_X /* 0x58 */)) {
      if(k.KFCM(1,t,[']'])){
        r=m=1;   // Line 140
        k.KDC(1,t);
        k.KO(-1,t,"X");
      }
      else if(1){
        r=m=1;   // Line 103
        k.KDC(0,t);
        k.KO(-1,t,"Ọ");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_Y /* 0x59 */)) {
      if(1){
        r=m=1;   // Line 83
        k.KDC(0,t);
        k.KO(-1,t,"Y");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_Z /* 0x5A */)) {
      if(1){
        r=m=1;   // Line 84
        k.KDC(0,t);
        k.KO(-1,t,"Z");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_BKSLASH /* 0xDC */)) {
      if(k.KFCM(2,t,['n','̄'])){
        r=m=1;   // Line 223
        k.KDC(2,t);
        k.KO(-1,t,"ñ");
      }
      else if(k.KFCM(2,t,['N','̄'])){
        r=m=1;   // Line 224
        k.KDC(2,t);
        k.KO(-1,t,"Ñ");
      }
      else if(k.KFCM(1,t,[']'])){
        r=m=1;   // Line 159
        k.KDC(1,t);
        k.KO(-1,t,"ˆ");
      }
      else if(k.KFCM(1,t,['ọ'])){
        r=m=1;   // Line 217
        k.KDC(1,t);
        k.KO(-1,t,"ộ");
      }
      else if(k.KFCM(1,t,['Ọ'])){
        r=m=1;   // Line 218
        k.KDC(1,t);
        k.KO(-1,t,"Ộ");
      }
      else if(k.KFCM(1,t,['s'])){
        r=m=1;   // Line 226
        k.KDC(1,t);
        k.KO(-1,t,"ṣ");
      }
      else if(k.KFCM(1,t,['S'])){
        r=m=1;   // Line 227
        k.KDC(1,t);
        k.KO(-1,t,"Ṣ");
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_vow_23}])){
        r=m=1;   // Line 232
        k.KDC(1,t);
        k.KIO(-1,this.s_circum_26,1,t);
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_6 /* 0x36 */)) {
      if(k.KFCM(1,t,[']'])){
        r=m=1;   // Line 153
        k.KDC(1,t);
        k.KO(-1,t,"^");
      }
      else if(1){
        r=m=1;   // Line 106
        k.KDC(0,t);
        k.KO(-1,t,"₦");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_HYPHEN /* 0xBD */)) {
      if(k.KFCM(1,t,[{t:'d',d:1}])){
        r=m=1;   // Line 201
        k.KDC(1,t);
        k.KO(-1,t,"¯");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_BKQUOTE /* 0xC0 */)) {
      if(k.KFCM(1,t,[']'])){
        r=m=1;   // Line 156
        k.KDC(1,t);
        k.KO(-1,t,"`");
      }
      else if(k.KFCM(1,t,['m'])){
        r=m=1;   // Line 205
        k.KDC(1,t);
        k.KO(-1,t,"m̀");
      }
      else if(k.KFCM(1,t,['M'])){
        r=m=1;   // Line 206
        k.KDC(1,t);
        k.KO(-1,t,"M̀");
      }
      else if(k.KFCM(1,t,['n'])){
        r=m=1;   // Line 208
        k.KDC(1,t);
        k.KO(-1,t,"ǹ");
      }
      else if(k.KFCM(1,t,['N'])){
        r=m=1;   // Line 209
        k.KDC(1,t);
        k.KO(-1,t,"Ǹ");
      }
      else if(k.KFCM(1,t,['ọ'])){
        r=m=1;   // Line 211
        k.KDC(1,t);
        k.KO(-1,t,"ọ̀");
      }
      else if(k.KFCM(1,t,['Ọ'])){
        r=m=1;   // Line 212
        k.KDC(1,t);
        k.KO(-1,t,"Ọ̀");
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_vow_23}])){
        r=m=1;   // Line 230
        k.KDC(1,t);
        k.KIO(-1,this.s_grave_24,1,t);
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_C /* 0x43 */)) {
      if(k.KFCM(1,t,[']'])){
        r=m=1;   // Line 134
        k.KDC(1,t);
        k.KO(-1,t,"c");
      }
      else if(1){
        r=m=1;   // Line 94
        k.KDC(0,t);
        k.KO(-1,t,"n̄");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_C /* 0x43 */)) {
      if(k.KFCM(1,t,[']'])){
        r=m=1;   // Line 136
        k.KDC(1,t);
        k.KO(-1,t,"c");
      }
      else if(1){
        r=m=1;   // Line 99
        k.KDC(0,t);
        k.KO(-1,t,"n̄");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_H /* 0x48 */)) {
      if(k.KFCM(1,t,[']'])){
        r=m=1;   // Line 144
        k.KDC(1,t);
        k.KO(-1,t,"h");
      }
      else if(1){
        r=m=1;   // Line 92
        k.KDC(0,t);
        k.KO(-1,t,"sh");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_H /* 0x48 */)) {
      if(k.KFCM(1,t,[']'])){
        r=m=1;   // Line 146
        k.KDC(1,t);
        k.KO(-1,t,"h");
      }
      else if(1){
        r=m=1;   // Line 97
        k.KDC(0,t);
        k.KO(-1,t,"Sh");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_Q /* 0x51 */)) {
      if(k.KFCM(1,t,[']'])){
        r=m=1;   // Line 149
        k.KDC(1,t);
        k.KO(-1,t,"q");
      }
      else if(1){
        r=m=1;   // Line 91
        k.KDC(0,t);
        k.KO(-1,t,"ch");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_Q /* 0x51 */)) {
      if(k.KFCM(1,t,[']'])){
        r=m=1;   // Line 151
        k.KDC(1,t);
        k.KO(-1,t,"q");
      }
      else if(1){
        r=m=1;   // Line 96
        k.KDC(0,t);
        k.KO(-1,t,"Ch");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_S /* 0x53 */)) {
      if(k.KFCM(1,t,[{t:'d',d:0}])){
        r=m=1;   // Line 179
        k.KDC(1,t);
        k.KO(-1,t,"ṣ");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_X /* 0x58 */)) {
      if(k.KFCM(1,t,[']'])){
        r=m=1;   // Line 139
        k.KDC(1,t);
        k.KO(-1,t,"x");
      }
      else if(1){
        r=m=1;   // Line 93
        k.KDC(0,t);
        k.KO(-1,t,"ọ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_X /* 0x58 */)) {
      if(k.KFCM(1,t,[']'])){
        r=m=1;   // Line 141
        k.KDC(1,t);
        k.KO(-1,t,"x");
      }
      else if(1){
        r=m=1;   // Line 98
        k.KDC(0,t);
        k.KO(-1,t,"ọ");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_BKSLASH /* 0xDC */)) {
      if(k.KFCM(1,t,[']'])){
        r=m=1;   // Line 160
        k.KDC(1,t);
        k.KO(-1,t,"ˇ");
      }
      else if(k.KFCM(1,t,['ọ'])){
        r=m=1;   // Line 220
        k.KDC(1,t);
        k.KO(-1,t,"ọ̌");
      }
      else if(k.KFCM(1,t,['Ọ'])){
        r=m=1;   // Line 221
        k.KDC(1,t);
        k.KO(-1,t,"Ọ̌");
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_vow_23}])){
        r=m=1;   // Line 233
        k.KDC(1,t);
        k.KIO(-1,this.s_caron_27,1,t);
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_BKQUOTE /* 0xC0 */)) {
      if(k.KFCM(1,t,[']'])){
        r=m=1;   // Line 157
        k.KDC(1,t);
        k.KO(-1,t,"´");
      }
      else if(k.KFCM(1,t,['ọ'])){
        r=m=1;   // Line 214
        k.KDC(1,t);
        k.KO(-1,t,"ọ́");
      }
      else if(k.KFCM(1,t,['Ọ'])){
        r=m=1;   // Line 215
        k.KDC(1,t);
        k.KO(-1,t,"Ọ́");
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_vow_23}])){
        r=m=1;   // Line 231
        k.KDC(1,t);
        k.KIO(-1,this.s_acute_25,1,t);
      }
      else if(k.KFCM(1,t,[{t:'a',a:this.s_nasals_21}])){
        r=m=1;   // Line 234
        k.KDC(1,t);
        k.KIO(-1,this.s_acnasals_28,1,t);
      }
    }
    return r;
  };
}
