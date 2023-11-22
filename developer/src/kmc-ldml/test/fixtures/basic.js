if(typeof keyman === 'undefined') {
  console.error('Keyboard requires KeymanWeb 16.0 or later');
} else {
  KeymanWeb.KR(new Keyboard_basic());
}
function Keyboard_basic() {
  this.KI="Keyboard_basic";
  this.KN="TestKbd";
  this.KMINVER="16.0";
  this.KV={F: ' 1em "Arial"', K102: 0};
  this.KV.KLS={
    TODO_LDML: 2
  };
  this.KDU=1;
  this.KH="";
  this.KM=0;
  this.KBVER="1.0.0";
  this.KMBM=0;
  this.KVKL={
  "desktop": {
    "defaultHint": "none",
    "layer": [
      {
        "id": "base",
        "row": [
          {
            "id": "0",
            "key": [
              {
                "id": "T_hmaqtugha",
                "text": "ħ"
              },
              {
                "id": "T_that",
                "text": "ថា"
              }
            ]
          }
        ]
      }
    ],
    "displayUnderlying": false
  }
};
  this.gs=function(t,e){
    return 0;
  };
}
