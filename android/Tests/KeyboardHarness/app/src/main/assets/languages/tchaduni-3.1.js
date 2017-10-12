KeymanWeb.KR(new Keyboard_tchaduni());
function Keyboard_tchaduni()
{
  var modCodes = tavultesoft.keymanweb.osk.modifierCodes;
  var keyCodes = tavultesoft.keymanweb.osk.keyCodes;

  this.KI="Keyboard_tchaduni";
  this.KN="Tchad Unicode v3";
  this.KV=null;
  this.KH='';
  this.KM=0;
  this.KBVER="3.1";
  this.KMBM=modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */;
  this.KVKD="T_A_U_0330 T_A_U_0327 T_E_U_0330 T_E_U_0327 T_I_U_0330 T_I_U_0327 T_O_U_0330 T_O_U_0327 T_U_U_0330 T_U_U_0327";
  this.KVKL={
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
                "text": "q",
                "sk": [
                  {
                    "text": "1",
                    "id": "K_1"
                  }
                ]
              },
              {
                "id": "K_W",
                "text": "w",
                "sk": [
                  {
                    "text": "2",
                    "id": "K_2"
                  }
                ]
              },
              {
                "id": "K_E",
                "text": "e",
                "sk": [
                  {
                    "text": "3",
                    "id": "K_3"
                  },
                  {
                    "text": "ɛ",
                    "id": "U_025B"
                  },
                  {
                    "text": "ə",
                    "id": "U_0259"
                  },
                  {
                    "text": "é",
                    "id": "U_00E9"
                  },
                  {
                    "text": "è",
                    "id": "U_00E8"
                  },
                  {
                    "text": "ē",
                    "id": "U_0113"
                  },
                  {
                    "text": "ê",
                    "id": "U_00EA"
                  },
                  {
                    "text": "ě",
                    "id": "U_011B"
                  },
                  {
                    "text": "ë",
                    "id": "U_00EB"
                  },
                  {
                    "text": "ẽ",
                    "id": "U_1EBD"
                  },
                  {
                    "text": "ḛ",
                    "id": "T_E_U_0330"
                  },
                  {
                    "text": "ȩ",
                    "id": "T_E_U_0327"
                  }
                ]
              },
              {
                "id": "K_R",
                "text": "r",
                "sk": [
                  {
                    "text": "4",
                    "id": "K_4"
                  },
                  {
                    "text": "ɾ",
                    "id": "U_027E"
                  },
                  {
                    "text": "ɽ",
                    "id": "U_027D"
                  },
                  {
                    "text": "ʁ",
                    "id": "U_0281"
                  }
                ]
              },
              {
                "id": "K_T",
                "text": "t",
                "sk": [
                  {
                    "text": "5",
                    "id": "K_5"
                  }
                ]
              },
              {
                "id": "K_Y",
                "text": "y",
                "sk": [
                  {
                    "text": "6",
                    "id": "K_6"
                  },
                  {
                    "text": "ƴ",
                    "id": "U_01B4"
                  }
                ]
              },
              {
                "id": "K_U",
                "text": "u",
                "sk": [
                  {
                    "text": "7",
                    "id": "K_7"
                  },
                  {
                    "text": "ú",
                    "id": "U_00FA"
                  },
                  {
                    "text": "ù",
                    "id": "U_00F9"
                  },
                  {
                    "text": "ū",
                    "id": "U_016B"
                  },
                  {
                    "text": "û",
                    "id": "U_00FB"
                  },
                  {
                    "text": "ǔ",
                    "id": "U_01D4"
                  },
                  {
                    "text": "ü",
                    "id": "U_00FC"
                  },
                  {
                    "text": "ũ",
                    "id": "U_0169"
                  },
                  {
                    "text": "ṵ",
                    "id": "T_U_U_0330"
                  },
                  {
                    "text": "u̧",
                    "id": "T_U_U_0327"
                  },
                  {
                    "text": "ʊ",
                    "id": "U_028A"
                  }
                ]
              },
              {
                "id": "K_I",
                "text": "i",
                "sk": [
                  {
                    "text": "8",
                    "id": "K_8"
                  },
                  {
                    "text": "ɨ",
                    "id": "U_0268"
                  },
                  {
                    "text": "ɪ",
                    "id": "U_026A"
                  },
                  {
                    "text": "í",
                    "id": "U_00ED"
                  },
                  {
                    "text": "ì",
                    "id": "U_00EC"
                  },
                  {
                    "text": "ī",
                    "id": "U_012B"
                  },
                  {
                    "text": "î",
                    "id": "U_00EE"
                  },
                  {
                    "text": "ǐ",
                    "id": "U_01D0"
                  },
                  {
                    "text": "ï",
                    "id": "U_00EF"
                  },
                  {
                    "text": "ĩ",
                    "id": "U_0129"
                  },
                  {
                    "text": "ḭ",
                    "id": "T_I_U_0330"
                  },
                  {
                    "text": "i̧",
                    "id": "T_I_U_0327"
                  }
                ]
              },
              {
                "id": "K_O",
                "text": "o",
                "sk": [
                  {
                    "text": "9",
                    "id": "K_9"
                  },
                  {
                    "text": "ɔ",
                    "id": "U_0254"
                  },
                  {
                    "text": "ó",
                    "id": "U_00F3"
                  },
                  {
                    "text": "ò",
                    "id": "U_00F2"
                  },
                  {
                    "text": "ō",
                    "id": "U_014D"
                  },
                  {
                    "text": "ô",
                    "id": "U_00F4"
                  },
                  {
                    "text": "ǒ",
                    "id": "U_01D2"
                  },
                  {
                    "text": "ö",
                    "id": "U_00F6"
                  },
                  {
                    "text": "õ",
                    "id": "U_00F5"
                  },
                  {
                    "text": "o̰",
                    "id": "T_O_U_0330"
                  },
                  {
                    "text": "o̧",
                    "id": "T_O_U_0327"
                  },
                  {
                    "text": "œ",
                    "id": "U_0153"
                  },
                  {
                    "text": "ø",
                    "id": "U_00F8"
                  }
                ]
              },
              {
                "id": "K_P",
                "text": "p",
                "sk": [
                  {
                    "text": "0",
                    "id": "K_0"
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
                "text": "a",
                "pad": "",
                "sk": [
                  {
                    "text": "á",
                    "id": "U_00E1"
                  },
                  {
                    "text": "à",
                    "id": "U_00E0"
                  },
                  {
                    "text": "ā",
                    "id": "U_0101"
                  },
                  {
                    "text": "â",
                    "id": "U_00E2"
                  },
                  {
                    "text": "ǎ",
                    "id": "U_01CE"
                  },
                  {
                    "text": "ä",
                    "id": "U_00E4"
                  },
                  {
                    "text": "ã",
                    "id": "U_00E3"
                  },
                  {
                    "text": "a̰",
                    "id": "T_A_U_0330"
                  },
                  {
                    "text": "a̧",
                    "id": "T_A_U_0327"
                  },
                  {
                    "text": "æ",
                    "id": "U_00E6"
                  }
                ]
              },
              {
                "id": "K_S",
                "text": "s",
                "sk": [
                  {
                    "text": "ʃ",
                    "id": "U_0283"
                  },
                  {
                    "text": "ß",
                    "id": "U_00DF"
                  }
                ]
              },
              {
                "id": "K_D",
                "text": "d",
                "sk": [
                  {
                    "text": "ɗ",
                    "id": "U_0257"
                  }
                ]
              },
              {
                "id": "K_F",
                "text": "f"
              },
              {
                "id": "K_G",
                "text": "g",
                "sk": [
                  {
                    "text": "ɣ",
                    "id": "U_0263"
                  }
                ]
              },
              {
                "id": "K_H",
                "text": "h",
                "sk": [
                  {
                    "text": "ɦ",
                    "id": "U_0266"
                  }
                ]
              },
              {
                "id": "K_J",
                "text": "j",
                "sk": [
                  {
                    "text": "ɟ",
                    "id": "U_025F"
                  },
                  {
                    "text": "ʄ",
                    "id": "U_0284"
                  }
                ]
              },
              {
                "id": "K_K",
                "text": "k"
              },
              {
                "id": "K_L",
                "text": "l",
                "sk": [
                  {
                    "text": "ɬ",
                    "id": "U_026C"
                  },
                  {
                    "text": "ɮ",
                    "id": "U_026E"
                  }
                ]
              },
              {
                "id": "U_02BC",
                "text": "ʼ",
                "width": ""
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
                "text": "z",
                "sk": [
                  {
                    "text": "ʒ",
                    "id": "U_0292"
                  },
                  {
                    "text": "ɮ",
                    "id": "U_026E"
                  }
                ]
              },
              {
                "id": "K_X",
                "text": "x",
                "sk": [
                  {
                    "text": "χ",
                    "id": "U_03C7"
                  }
                ]
              },
              {
                "id": "K_C",
                "text": "c",
                "sk": [
                  {
                    "text": "ç",
                    "id": "U_00E7"
                  }
                ]
              },
              {
                "id": "K_V",
                "text": "v",
                "sk": [
                  {
                    "text": "ʋ",
                    "id": "U_028B"
                  }
                ]
              },
              {
                "id": "K_B",
                "text": "b",
                "sk": [
                  {
                    "text": "ɓ",
                    "id": "U_0253"
                  }
                ]
              },
              {
                "id": "K_N",
                "text": "n",
                "sk": [
                  {
                    "text": "ŋ",
                    "id": "U_014B"
                  },
                  {
                    "text": "ɲ",
                    "id": "U_0272"
                  }
                ]
              },
              {
                "id": "K_M",
                "text": "m"
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
            "id": 4,
            "key": [
              {
                "id": "K_NUMLOCK",
                "text": "*123*",
                "width": "",
                "sp": "1",
                "nextlayer": "numeric"
              },
              {
                "id": "T_ACCENTS",
                "text": "◌̰̀",
                "nextlayer": "accents"
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
                "width": "555",
                "sp": "0",
                "sk": [
                  {
                    "text": "[nbsp]",
                    "id": "U_00A0"
                  }
                ]
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
                "text": "Q"
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
                    "text": "Ɛ",
                    "id": "U_0190"
                  },
                  {
                    "text": "Ə",
                    "id": "U_018F"
                  },
                  {
                    "text": "Ǝ",
                    "id": "U_018E"
                  },
                  {
                    "text": "É",
                    "id": "U_00C9"
                  },
                  {
                    "text": "È",
                    "id": "U_00C8"
                  },
                  {
                    "text": "Ē",
                    "id": "U_0112"
                  },
                  {
                    "text": "Ê",
                    "id": "U_00CA"
                  },
                  {
                    "text": "Ě",
                    "id": "U_011A"
                  },
                  {
                    "text": "Ë",
                    "id": "U_00CB"
                  },
                  {
                    "text": "Ẽ",
                    "id": "U_1EBC"
                  },
                  {
                    "text": "Ḛ",
                    "id": "T_E_U_0330"
                  },
                  {
                    "text": "Ȩ",
                    "id": "T_E_U_0327"
                  }
                ]
              },
              {
                "id": "K_R",
                "text": "R",
                "sk": [
                  {
                    "text": "ʁ",
                    "id": "U_0281"
                  }
                ]
              },
              {
                "id": "K_T",
                "text": "T"
              },
              {
                "id": "K_Y",
                "text": "Y",
                "sk": [
                  {
                    "text": "Ƴ",
                    "id": "U_01B3"
                  }
                ]
              },
              {
                "id": "K_U",
                "text": "U",
                "sk": [
                  {
                    "text": "Ú",
                    "id": "U_00DA"
                  },
                  {
                    "text": "Ù",
                    "id": "U_00D9"
                  },
                  {
                    "text": "Ū",
                    "id": "U_016A"
                  },
                  {
                    "text": "Û",
                    "id": "U_00DB"
                  },
                  {
                    "text": "Ǔ",
                    "id": "U_01D3"
                  },
                  {
                    "text": "Ü",
                    "id": "U_00DC"
                  },
                  {
                    "text": "Ũ",
                    "id": "U_0168"
                  },
                  {
                    "text": "Ṵ",
                    "id": "T_U_U_0330"
                  },
                  {
                    "text": "U̧",
                    "id": "T_U_U_0327"
                  }
                ]
              },
              {
                "id": "K_I",
                "text": "I",
                "sk": [
                  {
                    "text": "Ɨ",
                    "id": "U_0197"
                  },
                  {
                    "text": "Í",
                    "id": "U_00CD"
                  },
                  {
                    "text": "Ì",
                    "id": "U_00CC"
                  },
                  {
                    "text": "Ī",
                    "id": "U_012A"
                  },
                  {
                    "text": "Î",
                    "id": "U_00CE"
                  },
                  {
                    "text": "Ǐ",
                    "id": "U_01CF"
                  },
                  {
                    "text": "Ï",
                    "id": "U_00CF"
                  },
                  {
                    "text": "Ĩ",
                    "id": "U_0128"
                  },
                  {
                    "text": "Ḭ",
                    "id": "T_I_U_0330"
                  },
                  {
                    "text": "I̧",
                    "id": "T_I_U_0327"
                  }
                ]
              },
              {
                "id": "K_O",
                "text": "O",
                "sk": [
                  {
                    "text": "Ɔ",
                    "id": "U_0186"
                  },
                  {
                    "text": "Ó",
                    "id": "U_00D3"
                  },
                  {
                    "text": "Ò",
                    "id": "U_00D2"
                  },
                  {
                    "text": "Ō",
                    "id": "U_014C"
                  },
                  {
                    "text": "Ô",
                    "id": "U_00D4"
                  },
                  {
                    "text": "Ǒ",
                    "id": "U_01D1"
                  },
                  {
                    "text": "Ö",
                    "id": "U_00D6"
                  },
                  {
                    "text": "Õ",
                    "id": "U_00D5"
                  },
                  {
                    "text": "O̰",
                    "id": "T_O_U_0330"
                  },
                  {
                    "text": "O̧",
                    "id": "T_O_U_0327"
                  },
                  {
                    "text": "Œ",
                    "id": "U_0152"
                  }
                ]
              },
              {
                "id": "K_P",
                "text": "P"
              }
            ]
          },
          {
            "id": 2,
            "key": [
              {
                "id": "K_A",
                "text": "A",
                "pad": "",
                "sk": [
                  {
                    "text": "Á",
                    "id": "U_00C1"
                  },
                  {
                    "text": "À",
                    "id": "U_00C0"
                  },
                  {
                    "text": "Ā",
                    "id": "U_0100"
                  },
                  {
                    "text": "Â",
                    "id": "U_00C2"
                  },
                  {
                    "text": "Ǎ",
                    "id": "U_01CD"
                  },
                  {
                    "text": "Ä",
                    "id": "U_00C4"
                  },
                  {
                    "text": "Ã",
                    "id": "U_00C3"
                  },
                  {
                    "text": "A̰",
                    "id": "T_A_U_0330"
                  },
                  {
                    "text": "A̧",
                    "id": "T_A_U_0327"
                  },
                  {
                    "text": "Æ",
                    "id": "U_00C6"
                  }
                ]
              },
              {
                "id": "K_S",
                "text": "S"
              },
              {
                "id": "K_D",
                "text": "D",
                "sk": [
                  {
                    "text": "Ɗ",
                    "id": "U_018A"
                  }
                ]
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
                "id": "K_H",
                "text": "H",
                "sk": [
                  {
                    "text": "Ɦ",
                    "id": "U_A7AA"
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
                "id": "U_A78C",
                "text": "ꞌ",
                "width": ""
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
                "text": "Z"
              },
              {
                "id": "K_X",
                "text": "X"
              },
              {
                "id": "K_C",
                "text": "C",
                "sk": [
                  {
                    "text": "Ç",
                    "id": "U_00C7"
                  }
                ]
              },
              {
                "id": "K_V",
                "text": "V"
              },
              {
                "id": "K_B",
                "text": "B",
                "sk": [
                  {
                    "text": "Ɓ",
                    "id": "U_0181"
                  }
                ]
              },
              {
                "id": "K_N",
                "text": "N",
                "sk": [
                  {
                    "text": "Ŋ",
                    "id": "U_014A"
                  },
                  {
                    "text": "Ɲ",
                    "id": "U_019D"
                  }
                ]
              },
              {
                "id": "K_M",
                "text": "M"
              },
              {
                "id": "K_PERIOD",
                "text": ".",
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
            "id": 4,
            "key": [
              {
                "id": "K_NUMLOCK",
                "text": "*123*",
                "width": "",
                "sp": "1",
                "nextlayer": "numeric"
              },
              {
                "id": "T_ACCENTS",
                "text": "◌̰̀",
                "nextlayer": "accents"
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
                "width": "555",
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
                "id": "U_0040",
                "text": "@",
                "pad": "",
                "sk": [
                  {
                    "text": "©",
                    "id": "U_00A9"
                  },
                  {
                    "text": "℗",
                    "id": "U_2117"
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
                    "text": "̚",
                    "id": "U_031A"
                  }
                ]
              },
              {
                "id": "U_0024",
                "text": "$",
                "pad": "",
                "sk": [
                  {
                    "text": "€",
                    "id": "U_20AC"
                  },
                  {
                    "text": "£",
                    "id": "U_00A3"
                  }
                ]
              },
              {
                "id": "U_005F",
                "text": "_"
              },
              {
                "id": "U_0026",
                "text": "&"
              },
              {
                "id": "K_HYPHEN",
                "text": "-",
                "sk": [
                  {
                    "text": "–",
                    "id": "U_2013"
                  },
                  {
                    "text": "—",
                    "id": "U_2014"
                  }
                ]
              },
              {
                "id": "K_EQUAL",
                "text": "+",
                "layer": "shift",
                "sk": [
                  {
                    "text": "×",
                    "id": "U_00D7"
                  },
                  {
                    "text": "÷",
                    "id": "U_00F7"
                  }
                ]
              },
              {
                "id": "U_0028",
                "text": "(",
                "pad": "",
                "sk": [
                  {
                    "text": "[",
                    "id": "U_005B"
                  },
                  {
                    "text": "«",
                    "id": "U_00AB",
                    "nextlayer": "numeric"
                  },
                  {
                    "text": "‹",
                    "id": "U_2039"
                  },
                  {
                    "text": "<",
                    "id": "U_003C",
                    "nextlayer": "numeric"
                  },
                  {
                    "text": "{",
                    "id": "U_007B"
                  }
                ]
              },
              {
                "id": "U_0029",
                "text": ")",
                "sk": [
                  {
                    "text": "]",
                    "id": "U_005D"
                  },
                  {
                    "text": "»",
                    "id": "U_00BB"
                  },
                  {
                    "text": "›",
                    "id": "U_203A"
                  },
                  {
                    "text": ">",
                    "id": "U_003E"
                  },
                  {
                    "text": "}",
                    "id": "U_007D"
                  }
                ]
              },
              {
                "id": "K_BKSLASH",
                "text": "/",
                "sk": [
                  {
                    "text": "\\",
                    "id": "U_005C"
                  },
                  {
                    "text": "|",
                    "id": "U_007C"
                  }
                ]
              }
            ]
          },
          {
            "id": 3,
            "key": [
              {
                "id": "T_accents",
                "text": "◌̰̀",
                "sp": "1",
                "nextlayer": "accents"
              },
              {
                "id": "U_002A",
                "text": "*",
                "pad": "40",
                "sk": [
                  {
                    "text": "º",
                    "id": "U_00BA"
                  }
                ]
              },
              {
                "id": "U_0022",
                "text": "\"",
                "pad": "",
                "sk": [
                  {
                    "text": "“",
                    "id": "U_201C"
                  },
                  {
                    "text": "”",
                    "id": "U_201D"
                  },
                  {
                    "text": "«",
                    "id": "U_00AB"
                  },
                  {
                    "text": "»",
                    "id": "U_00BB"
                  }
                ]
              },
              {
                "id": "U_0027",
                "text": "'",
                "pad": "",
                "sk": [
                  {
                    "text": "‘",
                    "id": "U_2018"
                  },
                  {
                    "text": "’",
                    "id": "U_2019"
                  },
                  {
                    "text": "‹",
                    "id": "U_2039"
                  },
                  {
                    "text": "›",
                    "id": "U_203A"
                  },
                  {
                    "text": "`",
                    "id": "U_0060"
                  }
                ]
              },
              {
                "id": "U_003A",
                "text": ":",
                "sk": [
                  {
                    "text": "ː",
                    "id": "U_02D0"
                  },
                  {
                    "text": "ˑ",
                    "id": "U_02D1"
                  }
                ]
              },
              {
                "id": "U_003B",
                "text": ";"
              },
              {
                "id": "U_0021",
                "text": "!"
              },
              {
                "id": "U_003F",
                "text": "?",
                "sk": [
                  {
                    "text": "ʔ",
                    "id": "U_0294"
                  },
                  {
                    "text": "ʕ",
                    "id": "U_0295"
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
      },
      {
        "id": "accents",
        "row": [
          {
            "id": 1,
            "key": [
              {
                "id": "U_0300",
                "text": "◌̀",
                "pad": "60"
              },
              {
                "id": "U_0301",
                "text": "◌́"
              },
              {
                "id": "U_0304",
                "text": "◌̄"
              },
              {
                "id": "U_0302",
                "text": "◌̂"
              },
              {
                "id": "U_030C",
                "text": "◌̌"
              },
              {
                "id": "U_0308",
                "text": "◌̈"
              },
              {
                "id": "U_0303",
                "text": "◌̃"
              },
              {
                "id": "U_0330",
                "text": "◌̰"
              },
              {
                "id": "U_0327",
                "text": "◌̧"
              },
              {
                "id": "T_new_3726",
                "text": "",
                "width": "10",
                "sp": "10"
              }
            ]
          },
          {
            "id": 2,
            "key": [
              {
                "id": "U_1DC7",
                "text": "◌᷇",
                "pad": "60"
              },
              {
                "id": "U_1DC5",
                "text": "◌᷅"
              },
              {
                "id": "U_1DC4",
                "text": "◌᷄"
              },
              {
                "id": "U_1DC6",
                "text": "◌᷆"
              },
              {
                "id": "U_030B",
                "text": "◌̋"
              },
              {
                "id": "U_030F",
                "text": "◌̏"
              },
              {
                "id": "U_0325",
                "text": "◌̥"
              },
              {
                "id": "U_0323",
                "text": "◌̣"
              },
              {
                "id": "U_0329",
                "text": "◌̩"
              },
              {
                "id": "T_new_119",
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
                "id": "T_numeric",
                "text": "123",
                "sp": "1",
                "nextlayer": "numeric"
              },
              {
                "id": "U_007E",
                "text": "~",
                "pad": "40"
              },
              {
                "id": "U_0060",
                "text": "`"
              },
              {
                "id": "U_007C",
                "text": "|"
              },
              {
                "id": "U_005E",
                "text": "^"
              },
              {
                "id": "U_003D",
                "text": "="
              },
              {
                "id": "U_0025",
                "text": "%",
                "pad": ""
              },
              {
                "id": "U_263A",
                "text": "☺"
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
  this.s_aeiou="aeiouAEIOU";
  this.s_aeiou_gra="àèìòùÀÈÌÒÙ";
  this.s_aeiou_acu="áéíóúÁÉÍÓÚ";
  this.s_aeiou_mcr="āēīōūĀĒĪŌŪ";
  this.s_aeiou_cir="âêîôûÂÊÎÔÛ";
  this.s_aeiou_wdg="ǎěǐǒǔǍĚǏǑǓ";
  this.s_aeiou_uml="äëïöüÄËÏÖÜ";
  this.s_aeiou_tld="ãẽĩõũÃẼĨÕŨ";
  this.s_speckey="aAbBcCdDeEfFghHiIjJkKlLmMnNoOpPqrRsSuUvxXyYzZ?*][=^#|~_`@%$<>:{}()'\" ";
  this.s_spec="æÆɓƁçÇɗƊɛƐəƏɣɦꞪɪƗʄɟɨƗɬɮɲƝŋŊɔƆœŒʁɾɽʃßʊøʋχƎƴƳʒɮʔº][=^#|~_`@%$‹›ː“”‘’ꞌ' ";
  this.s_diakey="][=^|#~_`@%$";
  this.s_dia="̧̰̥̣̩̀́̄̂̌̈̃";
  this.KVER="10.0.955.0";
  this.gs=function(t,e) {
    return this.g_Main(t,e);
  };
  this.g_Main=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x100)) {   // Line 122
      r=m=1;
      k.KO(0,t,"a̰");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x101)) {   // Line 123
      r=m=1;
      k.KO(0,t,"a̧");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, 0x100)) {   // Line 124
      r=m=1;
      k.KO(0,t,"A̰");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, 0x101)) {   // Line 125
      r=m=1;
      k.KO(0,t,"A̧");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x102)) {   // Line 126
      r=m=1;
      k.KO(0,t,"ḛ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x103)) {   // Line 127
      r=m=1;
      k.KO(0,t,"ȩ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, 0x102)) {   // Line 128
      r=m=1;
      k.KO(0,t,"Ḛ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, 0x103)) {   // Line 129
      r=m=1;
      k.KO(0,t,"Ȩ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x104)) {   // Line 130
      r=m=1;
      k.KO(0,t,"ḭ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x105)) {   // Line 131
      r=m=1;
      k.KO(0,t,"i̧");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, 0x104)) {   // Line 132
      r=m=1;
      k.KO(0,t,"Ḭ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, 0x105)) {   // Line 133
      r=m=1;
      k.KO(0,t,"I̧");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x106)) {   // Line 134
      r=m=1;
      k.KO(0,t,"o̰");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x107)) {   // Line 135
      r=m=1;
      k.KO(0,t,"o̧");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, 0x106)) {   // Line 136
      r=m=1;
      k.KO(0,t,"O̰");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, 0x107)) {   // Line 137
      r=m=1;
      k.KO(0,t,"O̧");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x108)) {   // Line 138
      r=m=1;
      k.KO(0,t,"ṵ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, 0x109)) {   // Line 139
      r=m=1;
      k.KO(0,t,"u̧");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, 0x108)) {   // Line 140
      r=m=1;
      k.KO(0,t,"Ṵ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, 0x109)) {   // Line 141
      r=m=1;
      k.KO(0,t,"U̧");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_SPACE /* 0x20 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t," ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_SPACE /* 0x20 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t," ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_QUOTE /* 0xDE */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"'");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_QUOTE /* 0xDE */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"'");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_3 /* 0x33 */)&&k.KA(0,k.KC(1,1,t),this.s_aeiou)) {   // Line 45
      r=m=1;
      k.KIO(1,this.s_aeiou_uml,1,t);
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_3 /* 0x33 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"#");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_3 /* 0x33 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"#");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_3 /* 0x33 */)&&k.KCM(1,t,"#",1)) {   // Line 112
      r=m=1;
      k.KO(1,t,"̚");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_3 /* 0x33 */)) {   // Line 93
      r=m=1;
      k.KO(0,t,"̈");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_4 /* 0x34 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"$");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_4 /* 0x34 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"$");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_4 /* 0x34 */)&&k.KCM(1,t,"$",1)) {   // Line 116
      r=m=1;
      k.KO(1,t,"€");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_4 /* 0x34 */)&&k.KCM(1,t,"€",1)) {   // Line 117
      r=m=1;
      k.KO(1,t,"£");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_4 /* 0x34 */)) {   // Line 93
      r=m=1;
      k.KO(0,t,"̩");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_5 /* 0x35 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"%");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_5 /* 0x35 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"%");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_5 /* 0x35 */)) {   // Line 93
      r=m=1;
      k.KO(0,t,"̣");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_7 /* 0x37 */)&&k.KCM(1,t,";",1)) {   // Line 101
      r=m=1;
      k.KO(1,t,"&");
      k.KDO(-1,t,0);
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_7 /* 0x37 */)&&k.KCM(1,t,"&",1)) {   // Line 103
      r=m=1;
      k.KO(1,t,"&");
      k.KDO(-1,t,0);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_QUOTE /* 0xDE */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ꞌ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_QUOTE /* 0xDE */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ꞌ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_QUOTE /* 0xDE */)&&k.KCM(1,t,"’",1)) {   // Line 107
      r=m=1;
      k.KO(1,t,"ʼ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_QUOTE /* 0xDE */)) {   // Line 106
      r=m=1;
      k.KO(0,t,"’");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_9 /* 0x39 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"‘");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_9 /* 0x39 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"‘");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_0 /* 0x30 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"’");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_0 /* 0x30 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"’");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_8 /* 0x38 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"º");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_8 /* 0x38 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"º");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_HYPHEN /* 0xBD */)&&k.KCM(1,t,"-",1)) {   // Line 110
      r=m=1;
      k.KO(1,t,"–");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_HYPHEN /* 0xBD */)&&k.KCM(1,t,"–",1)) {   // Line 111
      r=m=1;
      k.KO(1,t,"—");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_COLON /* 0xBA */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ː");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_COLON /* 0xBA */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ː");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_COLON /* 0xBA */)&&k.KCM(1,t,"ː",1)) {   // Line 118
      r=m=1;
      k.KO(1,t,"ˑ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_COLON /* 0xBA */)&&k.KCM(1,t,";",1)) {   // Line 100
      r=m=1;
      k.KO(1,t,";");
      k.KDO(-1,t,0);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_COLON /* 0xBA */)&&k.KCM(1,t,"&",1)) {   // Line 102
      r=m=1;
      k.KO(1,t,";");
      k.KDO(-1,t,0);
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_COMMA /* 0xBC */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"‹");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_COMMA /* 0xBC */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"‹");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_COMMA /* 0xBC */)&&k.KCM(1,t,"<",1)) {   // Line 108
      r=m=1;
      k.KO(1,t,"«");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_EQUAL /* 0xBB */)&&k.KA(0,k.KC(1,1,t),this.s_aeiou)) {   // Line 42
      r=m=1;
      k.KIO(1,this.s_aeiou_mcr,1,t);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_EQUAL /* 0xBB */)&&k.KA(0,k.KC(1,1,t),this.s_aeiou_gra)) {   // Line 78
      r=m=1;
      k.KIO(1,this.s_aeiou,1,t);
      k.KO(-1,t,"᷅");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_EQUAL /* 0xBB */)&&k.KCM(1,t,"̀",1)) {   // Line 79
      r=m=1;
      k.KO(1,t,"᷅");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_EQUAL /* 0xBB */)&&k.KA(0,k.KC(1,1,t),this.s_aeiou_acu)) {   // Line 80
      r=m=1;
      k.KIO(1,this.s_aeiou,1,t);
      k.KO(-1,t,"᷇");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_EQUAL /* 0xBB */)&&k.KCM(1,t,"́",1)) {   // Line 81
      r=m=1;
      k.KO(1,t,"᷇");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_EQUAL /* 0xBB */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"=");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_EQUAL /* 0xBB */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"=");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_EQUAL /* 0xBB */)) {   // Line 93
      r=m=1;
      k.KO(0,t,"̄");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_PERIOD /* 0xBE */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"›");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_PERIOD /* 0xBE */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"›");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_PERIOD /* 0xBE */)&&k.KCM(1,t,">",1)) {   // Line 109
      r=m=1;
      k.KO(1,t,"»");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_SLASH /* 0xBF */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ʔ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_SLASH /* 0xBF */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ʔ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_SLASH /* 0xBF */)&&k.KCM(1,t,"ʔ",1)) {   // Line 119
      r=m=1;
      k.KO(1,t,"ʕ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_2 /* 0x32 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"@");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_2 /* 0x32 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"@");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_2 /* 0x32 */)&&k.KCM(1,t,"@",1)) {   // Line 113
      r=m=1;
      k.KO(1,t,"©");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_2 /* 0x32 */)&&k.KCM(1,t,"©",1)) {   // Line 114
      r=m=1;
      k.KO(1,t,"℗");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_2 /* 0x32 */)&&k.KCM(1,t,"℗",1)) {   // Line 115
      r=m=1;
      k.KO(1,t,"®");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_2 /* 0x32 */)) {   // Line 93
      r=m=1;
      k.KO(0,t,"̥");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_A /* 0x41 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"Æ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_A /* 0x41 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"Æ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_B /* 0x42 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"Ɓ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_B /* 0x42 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"Ɓ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_C /* 0x43 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"Ç");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_C /* 0x43 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"Ç");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_D /* 0x44 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"Ɗ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_D /* 0x44 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"Ɗ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_E /* 0x45 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"Ɛ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_E /* 0x45 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"Ɛ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_F /* 0x46 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"Ə");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_F /* 0x46 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"Ə");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_H /* 0x48 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"Ɦ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_H /* 0x48 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"Ɦ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_I /* 0x49 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"Ɨ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_I /* 0x49 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"Ɨ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_J /* 0x4A */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ɟ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_J /* 0x4A */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ɟ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_K /* 0x4B */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"Ɨ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_K /* 0x4B */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"Ɨ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_L /* 0x4C */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ɮ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_L /* 0x4C */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ɮ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_M /* 0x4D */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"Ɲ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_M /* 0x4D */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"Ɲ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_N /* 0x4E */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"Ŋ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_N /* 0x4E */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"Ŋ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_O /* 0x4F */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"Ɔ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_O /* 0x4F */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"Ɔ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_P /* 0x50 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"Œ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_P /* 0x50 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"Œ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_R /* 0x52 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ɽ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_R /* 0x52 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ɽ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_S /* 0x53 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ß");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_S /* 0x53 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ß");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_U /* 0x55 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ø");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_U /* 0x55 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ø");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_X /* 0x58 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"Ǝ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_X /* 0x58 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"Ǝ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_Y /* 0x59 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"Ƴ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_Y /* 0x59 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"Ƴ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_Z /* 0x5A */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ɮ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_Z /* 0x5A */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ɮ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_LBRKT /* 0xDB */)&&k.KA(0,k.KC(1,1,t),this.s_aeiou)) {   // Line 41
      r=m=1;
      k.KIO(1,this.s_aeiou_acu,1,t);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_LBRKT /* 0xDB */)&&k.KA(0,k.KC(1,1,t),this.s_aeiou_acu)) {   // Line 76
      r=m=1;
      k.KIO(1,this.s_aeiou,1,t);
      k.KO(-1,t,"̋");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_LBRKT /* 0xDB */)&&k.KCM(1,t,"́",1)) {   // Line 77
      r=m=1;
      k.KO(1,t,"̋");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_LBRKT /* 0xDB */)&&k.KA(0,k.KC(1,1,t),this.s_aeiou_mcr)) {   // Line 84
      r=m=1;
      k.KIO(1,this.s_aeiou,1,t);
      k.KO(-1,t,"᷄");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_LBRKT /* 0xDB */)&&k.KCM(1,t,"̄",1)) {   // Line 85
      r=m=1;
      k.KO(1,t,"᷄");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_LBRKT /* 0xDB */)&&k.KA(0,k.KC(1,1,t),this.s_aeiou_gra)) {   // Line 86
      r=m=1;
      k.KIO(1,this.s_aeiou_wdg,1,t);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_LBRKT /* 0xDB */)&&k.KCM(1,t,"̀",1)) {   // Line 87
      r=m=1;
      k.KO(1,t,"̌");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_LBRKT /* 0xDB */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"[");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_LBRKT /* 0xDB */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"[");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_LBRKT /* 0xDB */)) {   // Line 93
      r=m=1;
      k.KO(0,t,"́");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_RBRKT /* 0xDD */)&&k.KA(0,k.KC(1,1,t),this.s_aeiou)) {   // Line 40
      r=m=1;
      k.KIO(1,this.s_aeiou_gra,1,t);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_RBRKT /* 0xDD */)&&k.KA(0,k.KC(1,1,t),this.s_aeiou_gra)) {   // Line 74
      r=m=1;
      k.KIO(1,this.s_aeiou,1,t);
      k.KO(-1,t,"̏");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_RBRKT /* 0xDD */)&&k.KCM(1,t,"̀",1)) {   // Line 75
      r=m=1;
      k.KO(1,t,"̏");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_RBRKT /* 0xDD */)&&k.KA(0,k.KC(1,1,t),this.s_aeiou_mcr)) {   // Line 82
      r=m=1;
      k.KIO(1,this.s_aeiou,1,t);
      k.KO(-1,t,"᷆");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_RBRKT /* 0xDD */)&&k.KCM(1,t,"̄",1)) {   // Line 83
      r=m=1;
      k.KO(1,t,"᷆");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_RBRKT /* 0xDD */)&&k.KA(0,k.KC(1,1,t),this.s_aeiou_acu)) {   // Line 88
      r=m=1;
      k.KIO(1,this.s_aeiou_cir,1,t);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_RBRKT /* 0xDD */)&&k.KCM(1,t,"́",1)) {   // Line 89
      r=m=1;
      k.KO(1,t,"̂");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_RBRKT /* 0xDD */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"]");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_RBRKT /* 0xDD */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"]");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_RBRKT /* 0xDD */)) {   // Line 93
      r=m=1;
      k.KO(0,t,"̀");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_6 /* 0x36 */)&&k.KA(0,k.KC(1,1,t),this.s_aeiou)) {   // Line 43
      r=m=1;
      k.KIO(1,this.s_aeiou_cir,1,t);
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_6 /* 0x36 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"^");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_6 /* 0x36 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"^");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_6 /* 0x36 */)) {   // Line 93
      r=m=1;
      k.KO(0,t,"̂");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_HYPHEN /* 0xBD */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"_");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_HYPHEN /* 0xBD */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"_");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_HYPHEN /* 0xBD */)) {   // Line 93
      r=m=1;
      k.KO(0,t,"̰");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_BKQUOTE /* 0xC0 */)&&k.KCM(1,t,"c",1)) {   // Line 63
      r=m=1;
      k.KO(1,t,"ç");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_BKQUOTE /* 0xC0 */)&&k.KCM(1,t,"C",1)) {   // Line 64
      r=m=1;
      k.KO(1,t,"Ç");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_BKQUOTE /* 0xC0 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"`");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_BKQUOTE /* 0xC0 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"`");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_BKQUOTE /* 0xC0 */)) {   // Line 93
      r=m=1;
      k.KO(0,t,"̧");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_A /* 0x41 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"æ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_A /* 0x41 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"æ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_B /* 0x42 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ɓ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_B /* 0x42 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ɓ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_C /* 0x43 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ç");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_C /* 0x43 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ç");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_D /* 0x44 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ɗ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_D /* 0x44 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ɗ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_E /* 0x45 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ɛ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_E /* 0x45 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ɛ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_F /* 0x46 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ə");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_F /* 0x46 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ə");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_G /* 0x47 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ɣ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_G /* 0x47 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ɣ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_H /* 0x48 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ɦ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_H /* 0x48 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ɦ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_I /* 0x49 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ɪ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_I /* 0x49 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ɪ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_J /* 0x4A */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ʄ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_J /* 0x4A */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ʄ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_K /* 0x4B */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ɨ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_K /* 0x4B */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ɨ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_L /* 0x4C */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ɬ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_L /* 0x4C */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ɬ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_M /* 0x4D */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ɲ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_M /* 0x4D */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ɲ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_N /* 0x4E */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ŋ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_N /* 0x4E */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ŋ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_O /* 0x4F */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ɔ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_O /* 0x4F */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ɔ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_P /* 0x50 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"œ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_P /* 0x50 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"œ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Q /* 0x51 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ʁ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Q /* 0x51 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ʁ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_R /* 0x52 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ɾ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_R /* 0x52 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ɾ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_S /* 0x53 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ʃ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_S /* 0x53 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ʃ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_U /* 0x55 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ʊ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_U /* 0x55 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ʊ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_V /* 0x56 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ʋ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_V /* 0x56 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ʋ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_X /* 0x58 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"χ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_X /* 0x58 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"χ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Y /* 0x59 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ƴ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Y /* 0x59 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ƴ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Z /* 0x5A */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"ʒ");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Z /* 0x5A */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"ʒ");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_LBRKT /* 0xDB */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"“");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_LBRKT /* 0xDB */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"“");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_BKSLASH /* 0xDC */)&&k.KA(0,k.KC(1,1,t),this.s_aeiou)) {   // Line 44
      r=m=1;
      k.KIO(1,this.s_aeiou_wdg,1,t);
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_BKSLASH /* 0xDC */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"|");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_BKSLASH /* 0xDC */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"|");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_BKSLASH /* 0xDC */)) {   // Line 93
      r=m=1;
      k.KO(0,t,"̌");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_RBRKT /* 0xDD */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"”");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_RBRKT /* 0xDD */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"”");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_BKQUOTE /* 0xC0 */)&&k.KA(0,k.KC(1,1,t),this.s_aeiou)) {   // Line 46
      r=m=1;
      k.KIO(1,this.s_aeiou_tld,1,t);
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_BKQUOTE /* 0xC0 */)&&k.KCM(1,t,";",1)) {   // Line 96
      r=m=1;
      k.KO(1,t,"~");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_BKQUOTE /* 0xC0 */)&&k.KCM(1,t,"&",1)) {   // Line 97
      r=m=1;
      k.KO(1,t,"~");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_BKQUOTE /* 0xC0 */)) {   // Line 93
      r=m=1;
      k.KO(0,t,"̃");
    }
    return r;
  };
}
