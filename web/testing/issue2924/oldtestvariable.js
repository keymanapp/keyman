if(typeof keyman === 'undefined') {
  console.log('Keyboard requires KeymanWeb 10.0 or later');
  if(typeof tavultesoft !== 'undefined') tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");
} else {
KeymanWeb.KR(new Keyboard_oldtestvariable());
}
function Keyboard_oldtestvariable()
{
  var modCodes = keyman.osk.modifierCodes;
  var keyCodes = keyman.osk.keyCodes;

  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_oldtestvariable";
  this.KN="oldtestvariable";
  this.KMINVER="10.0";
  this.KV={F:' 1em "Arial"',K102:0};
  this.KV.KLS={
    "default": ["","","","","","","","","","","","","","","","","","","","Reveal","","","","","","","","","","","","","","Set","","","","","","","","","","","","","","","","","","Clear","","","","","","","","","","","","",""]
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
  this.KBVER="1.0";
  this.KMBM=0 /* 0x0000 */;
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
                "id": "K_S",
                "text": "Set"
              },
              {
                "id": "K_C",
                "text": "Clear"
              },
              {
                "id": "K_R",
                "text": "Reveal"
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
                "width": "140",
                "id": "K_LOPT",
                "sp": "1",
                "text": "*Menu*"
              },
              {
                "id": "K_SPACE"
              },
              {
                "width": "145",
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
  this.s_myVar_9=KeymanWeb.KLOAD(this.KI,"myVar","1");
  this.s13="1";
  this.s14="0";
  this.s15="1";
  this.s16="1";
  this.KVER="15.0.105.0";
  //this.KVS=['s_myVar_9']; <-- manually stubbed out to demonstrate old behaviour
  this.gs=function(t,e) {
    return this.g_main_0(t,e);
  };
  this.g_main_0=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_C /* 0x43 */)) {
      if(1){
        r=m=1;   // Line 16
        k.KDC(0,t);
        k.KO(-1,t,"(-)");
        this.s_myVar_9=this.s14;
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_R /* 0x52 */)) {
      if(this.s_myVar_9===this.s15){
        r=m=1;   // Line 17
        k.KDC(0,t);
        k.KO(-1,t,"+");
      }
      else if(this.s_myVar_9!==this.s16){
        r=m=1;   // Line 18
        k.KDC(0,t);
        k.KO(-1,t,"-");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_S /* 0x53 */)) {
      if(1){
        r=m=1;   // Line 15
        k.KDC(0,t);
        k.KO(-1,t,"(+)");
        this.s_myVar_9=this.s13;
      }
    }
    return r;
  };
}
