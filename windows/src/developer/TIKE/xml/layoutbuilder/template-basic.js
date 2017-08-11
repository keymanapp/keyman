{
  "tablet": {
    "font": "Tahoma",
    "layer": [
      {
        "id": "default",
        "row": [
          {
            "id": 1,
            "key": [
              {"id": "K_Q","text":"q"},
              {"id": "K_W","text":"w"},
              {"id": "K_E","text":"e"},
              {"id": "K_R","text":"r"},
              {"id": "K_T","text":"t"},
              {"id": "K_Y","text":"y"},
              {"id": "K_U","text":"u"},
              {"id": "K_I","text":"i"},
              {"id": "K_O","text":"o"},
              {"id": "K_P","text":"p"}
            ]
          },
          {
            "id": 2,
            "key": [
              {"id": "K_A","text":"a","pad":70},
              {"id": "K_S","text":"s"},
              {"id": "K_D","text":"d"},
              {"id": "K_F","text":"f"},
              {"id": "K_G","text":"g"},
              {"id": "K_H","text":"h"},
              {"id": "K_J","text":"j"},
              {"id": "K_K","text":"k"},
              {"id": "K_L","text":"l"},
              {"sp": "10","width":"10"}
            ]
          },
          {
            "id": 3,
            "key": [
              {"id": "K_SHIFT","text": "*Shift*","width": "110","sp": "1","nextlayer": "shift"},
              {"id": "K_Z","text":"z"},
              {"id": "K_X","text":"x"},
              {"id": "K_C","text":"c"},
              {"id": "K_V","text":"v"},
              {"id": "K_B","text":"b"},
              {"id": "K_N","text":"n"},
              {"id": "K_M","text":"m"},
              {"id": "K_PERIOD","text": ".","sk": [
                  {"text": ",","id": "K_COMMA"},
                  {"text": "!","id": "K_1", "layer": "shift"},
                  {"text": "?","id": "K_SLASH", "layer": "shift"},
                  {"text": "'","id": "K_QUOTE"},
                  {"text": "\"","id": "K_QUOTE", "layer": "shift"},
                  {"text": "\\","id": "K_BKSLASH"},
                  {"text": ":","id": "K_COLON", "layer": "shift"},
                  {"text": ";","id": "K_COLON"}
                ]
              },
              {"id": "K_BKSP","text": "*BkSp*","width":"90","sp": "1"}
            ]
          },
          {
            "id": 4,
            "key": [
              {"id": "K_NUMLOCK","text": "*123*","width":"140","sp": "1","nextlayer": "numeric"},
              {"id": "K_LOPT","text": "*Menu*","width": "120","sp": "1"},
              {"id": "K_SPACE","text": "","width": "630","sp": "0"},
              {"id": "K_ENTER","text": "*Enter*","width":"140","sp": "1"}
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
              {"id": "K_Q","text":"Q"},
              {"id": "K_W","text":"W"},
              {"id": "K_E","text":"E"},
              {"id": "K_R","text":"R"},
              {"id": "K_T","text":"T"},
              {"id": "K_Y","text":"Y"},
              {"id": "K_U","text":"U"},
              {"id": "K_I","text":"I"},
              {"id": "K_O","text":"O"},
              {"id": "K_P","text":"P"}
            ]
          },
          {
            "id": 2,
            "key": [
              {"id": "K_A","text":"A","pad":70},
              {"id": "K_S","text":"S"},
              {"id": "K_D","text":"D"},
              {"id": "K_F","text":"F"},
              {"id": "K_G","text":"G"},
              {"id": "K_H","text":"H"},
              {"id": "K_J","text":"J"},
              {"id": "K_K","text":"K"},
              {"id": "K_L","text":"L"},
              {"sp": "10","width":"10"}
            ]
          },
          {
            "id": 3,
            "key": [
              {"id": "K_SHIFT","text": "*Shift*","width": "110","sp": "2","nextlayer": "default"},
              {"id": "K_Z","text":"Z"},
              {"id": "K_X","text":"X"},
              {"id": "K_C","text":"C"},
              {"id": "K_V","text":"V"},
              {"id": "K_B","text":"B"},
              {"id": "K_N","text":"N"},
              {"id": "K_M","text":"M"},
              {"id": "K_PERIOD","text": ".","sk": [
                  {"text": ",","id": "K_COMMA", "layer": "default"},
                  {"text": "!","id": "K_1", "layer": "shift"},
                  {"text": "?","id": "K_SLASH", "layer": "shift"},
                  {"text": "'","id": "K_QUOTE", "layer": "default"},
                  {"text": "\"","id": "K_QUOTE", "layer": "shift"},
                  {"text": "\\","id": "K_BKSLASH", "layer": "default"},
                  {"text": ":","id": "K_COLON", "layer": "shift"},
                  {"text": ";","id": "K_COLON", "layer": "default"}
                ]
              },
              {"id": "K_BKSP","text": "*BkSp*","width": "90","sp": "1"}
            ]
          },
          {
            "id": 4,
            "key": [            
              {"id": "K_NUMLOCK","text": "*123*","width":"140","sp": "1","nextlayer": "numeric"},
              {"id": "K_LOPT","text": "*Menu*","width": "120","sp": "1"},
              {"id": "K_SPACE","text": "","width": "630","sp": "0"},
              {"id": "K_ENTER","text": "*Enter*","width":"140","sp": "1"}
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
              {"id": "K_1","text": "1"},
              {"id": "K_2","text": "2"},
              {"id": "K_3","text": "3"},
              {"id": "K_4","text": "4"},
              {"id": "K_5","text": "5"},
              {"id": "K_6","text": "6"},
              {"id": "K_7","text": "7"},
              {"id": "K_8","text": "8"},
              {"id": "K_9","text": "9"},
              {"id": "K_0","text": "0"}
            ]
          },
          {
            "id": 2,
            "key": [
              {"id": "K_4","text": "$","layer":"shift","pad":70},
              {"id": "K_2","text": "@","layer":"shift"},
              {"id": "K_3","text": "#","layer":"shift"},
              {"id": "K_5","text": "%","layer":"shift"},
              {"id": "K_7","text": "&","layer":"shift"},
              {"id": "K_HYPHEN","text": "_","layer":"shift"},
              {"id": "K_EQUAL","text": "=","layer":"default"},
              {"id": "K_BKSLASH","text": "|","layer":"shift"},
              {"id": "K_BKSLASH","text": "\\","layer":"default"},
              {"text": "","width": "10","sp": "10"}
            ]
          },
          {
            "id": 3,
            "key": [
              {"id": "K_SHIFT","text": "*Shift*","width": "110","sp": "1"},
              {"id": "K_LBRKT","text": "[","sk": [
                  {"id": "U_00AB","text":"\u00ab"},
                  {"id": "K_COMMA","text":"<","layer":"shift"},
                  {"id": "K_LBRKT","text":"{","layer":"shift"}
                ]
              },
              {"id": "K_9","text": "(","layer":"shift"},
              {"id": "K_0","text": ")","layer":"shift"},
              {"id": "K_RBRKT","text": "]","sk": [
                  {"id": "U_00BB","text":"\u00bb"},
                  {"id": "K_PERIOD","text":">","layer":"shift"},
                  {"id": "K_RBRKT","text":"}","layer":"shift"}
                ]
              },
              {"id": "K_EQUAL","text": "+","layer":"shift"},
              {"id": "K_HYPHEN","text": "-","layer":"default"},
              {"id": "K_8","text": "*","layer":"shift"},
              {"id": "K_SLASH","text": "/","layer":"default"},
              {"id": "K_BKSP","text": "*BkSp*","width": "90","sp": "1"}
            ]
          },
          {
            "id": 4,
            "key": [
              {"id": "K_LOWER","text": "*abc*","width": "140","sp": "1","nextlayer": "default"},
              {"id": "K_LOPT","text": "*Menu*","width": "120","sp": "1"},
              {"id": "K_SPACE","text": "","width": "630","sp": "0"},
              {"id": "K_ENTER","text": "*Enter*","width": "140","sp": "1"}
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
              {"id": "K_Q","text":"q"},
              {"id": "K_W","text":"w"},
              {"id": "K_E","text":"e"},
              {"id": "K_R","text":"r"},
              {"id": "K_T","text":"t"},
              {"id": "K_Y","text":"y"},
              {"id": "K_U","text":"u"},
              {"id": "K_I","text":"i"},
              {"id": "K_O","text":"o"},
              {"id": "K_P","text":"p"}
            ]
          },
          {
            "id": 2,
            "key": [
              {"id": "K_A","text":"a","pad":"50"},
              {"id": "K_S","text":"s"},
              {"id": "K_D","text":"d"},
              {"id": "K_F","text":"f"},
              {"id": "K_G","text":"g"},
              {"id": "K_H","text":"h"},
              {"id": "K_J","text":"j"},
              {"id": "K_K","text":"k"},
              {"id": "K_L","text":"l"},
              {"text": "","width": "10","sp": "10"}                            
            ]
          },
          {
            "id": 3,
            "key": [
              {"id": "K_SHIFT","text": "*Shift*","sp": "1","nextlayer": "shift"},
              {"id": "K_Z","text":"z"},
              {"id": "K_X","text":"x"},
              {"id": "K_C","text":"c"},
              {"id": "K_V","text":"v"},
              {"id": "K_B","text":"b"},
              {"id": "K_N","text":"n"},
              {"id": "K_M","text":"m"},
              {"id": "K_PERIOD","text": ".","sk": [
                  {"text": ",","id": "K_COMMA"},
                  {"text": "!","id": "K_1", "layer": "shift"},
                  {"text": "?","id": "K_SLASH", "layer": "shift"},
                  {"text": "'","id": "K_QUOTE"},
                  {"text": "\"","id": "K_QUOTE", "layer": "shift"},
                  {"text": "\\","id": "K_BKSLASH"},
                  {"text": ":","id": "K_COLON", "layer": "shift"},
                  {"text": ";","id": "K_COLON"}
                ]
              },
              {"id": "K_BKSP","text": "*BkSp*","width": "100","sp": "1"}
            ]
          },
          {
            "id": 4,
            "key": [
              {"id": "K_NUMLOCK","text": "*123*","width":"150","sp": "1","nextlayer": "numeric"},
              {"id": "K_LOPT","text": "*Menu*","width": "120","sp": "1"},
              {"id": "K_SPACE","text": "","width": "610","sp": "0"},
              {"id": "K_ENTER","text": "*Enter*","width":"150","sp": "1"}
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
              {"id": "K_Q","text":"Q"},
              {"id": "K_W","text":"W"},
              {"id": "K_E","text":"E"},
              {"id": "K_R","text":"R"},
              {"id": "K_T","text":"T"},
              {"id": "K_Y","text":"Y"},
              {"id": "K_U","text":"U"},
              {"id": "K_I","text":"I"},
              {"id": "K_O","text":"O"},
              {"id": "K_P","text":"P"}
            ]
          },
          {
            "id": 2,
            "key": [
              {"id": "K_A","text":"A","pad":"50"},
              {"id": "K_S","text":"S"},
              {"id": "K_D","text":"D"},
              {"id": "K_F","text":"F"},
              {"id": "K_G","text":"G"},
              {"id": "K_H","text":"H"},
              {"id": "K_J","text":"J"},
              {"id": "K_K","text":"K"},
              {"id": "K_L","text":"L"},
              {"text": "","width": "10","sp": "10"}                            
            ]
          },
          {
            "id": 3,
            "key": [
              {"id": "K_SHIFT","text": "*Shift*","sp": "2","nextlayer": "default"},
              {"id": "K_Z","text":"Z"},
              {"id": "K_X","text":"X"},
              {"id": "K_C","text":"C"},
              {"id": "K_V","text":"V"},
              {"id": "K_B","text":"B"},
              {"id": "K_N","text":"N"},
              {"id": "K_M","text":"M"},
              {"id": "K_PERIOD","text": ".","sk": [
                  {"text": ",","id": "K_COMMA", "layer": "default"},
                  {"text": "!","id": "K_1", "layer": "shift"},
                  {"text": "?","id": "K_SLASH", "layer": "shift"},
                  {"text": "'","id": "K_QUOTE", "layer": "default"},
                  {"text": "\"","id": "K_QUOTE", "layer": "shift"},
                  {"text": "\\","id": "K_BKSLASH", "layer": "default"},
                  {"text": ":","id": "K_COLON", "layer": "shift"},
                  {"text": ";","id": "K_COLON", "layer": "default"}
                ]
              },
              {"id": "K_BKSP","text": "*BkSp*","sp": "1"}
            ]
          },
          {
            "id": 4,
            "key": [
              {"id": "K_NUMLOCK","text": "*123*","width":"150","sp": "1","nextlayer": "numeric"},
              {"id": "K_LOPT","text": "*Menu*","width": "120","sp": "1"},
              {"id": "K_SPACE","text": "","width": "610","sp": "0"},
              {"id": "K_ENTER","text": "*Enter*","width":"150","sp": "1"}
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
              {"id": "K_1","text": "1"},
              {"id": "K_2","text": "2"},
              {"id": "K_3","text": "3"},
              {"id": "K_4","text": "4"},
              {"id": "K_5","text": "5"},
              {"id": "K_6","text": "6"},
              {"id": "K_7","text": "7"},
              {"id": "K_8","text": "8"},
              {"id": "K_9","text": "9"},
              {"id": "K_0","text": "0"}
            ]
          },
          {
            "id": 2,
            "key": [
              {"id": "K_4","layer":"shift","text": "$","pad": "50"},
              {"id": "K_2","layer":"shift","text": "@"},
              {"id": "K_3","layer":"shift","text": "#"},
              {"id": "K_5","layer":"shift","text": "%"},
              {"id": "K_6","layer":"shift","text": "&"},
              {"id": "K_HYPHEN","layer":"shift","text": "_"},
              {"id": "K_EQUAL","text": "=","layer":"default"},
              {"id": "K_BKSLASH","layer":"shift","text": "|"},
              {"id": "K_BKSLASH","text": "\\","layer":"default"},
              {"text": "","width": "10","sp": "10"}
            ]
          },
          {
            "id": 3,
            "key": [
              {"id": "K_LBRKT","text": "[","pad":"110","sk": [
                  {"id": "U_00AB","text":"\u00ab"},
                  {"id": "K_COMMA","text":"<","layer":"shift"},
                  {"id": "K_LBRKT","text":"{","layer":"shift"}
                ]
              },
              {"id": "K_9","layer":"shift","text": "("},
              {"id": "K_0","layer":"shift","text": ")"},
              {"id": "K_RBRKT","text": "]","sk": [
                  {"id": "U_00BB","text":"\u00bb"},
                  {"id": "K_PERIOD","text":">","layer":"shift"},
                  {"id": "K_RBRKT","text":"}","layer":"shift"}
                ]
              },
              {"id": "K_EQUAL","layer":"shift","text": "+"},
              {"id": "K_HYPHEN","text": "-"},
              {"id": "K_8","layer":"shift","text": "*"},
              {"id": "K_SLASH","text": "/"},
              {"id": "K_BKSP","text": "*BkSp*","width": "100","sp": "1"}
            ]
          },
          {
            "id": 4,
            "key": [
              {"id": "K_LOWER","text": "*abc*","width": "150","sp": "1","nextlayer": "default"},
              {"id": "K_LOPT","text": "*Menu*","width": "120","sp": "1"},
              {"id": "K_SPACE","text": "","width": "610","sp": "0"},
              {"id": "K_ENTER","text": "*Enter*","width": "150","sp": "1"}
            ]
          }
        ]
      }
    ]
  }
}