if(typeof keyman === 'undefined') {
  console.log('Keyboard requires KeymanWeb 10.0 or later');
  if(typeof tavultesoft !== 'undefined') tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");
} else {
KeymanWeb.KR(new Keyboard_k_0803___if_and_context());
}
function Keyboard_k_0803___if_and_context()
{
  var modCodes = keyman.osk.modifierCodes;
  var keyCodes = keyman.osk.keyCodes;

  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0803___if_and_context";
  this.KN="0803 - if and context";
  this.KMINVER="10.0";
  this.KV=null;
  this.KDU=0;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=0 /* 0x0000 */;
  this.s_nfc_5=KeymanWeb.KLOAD(this.KI,"nfc","0");
  this.s_diaeresisBase_6="ae";
  this.s8="0";
  this.KVS=['s_nfc_5'];
  this.gs=function(t,e) {
    return this.g_Main_0(t,e);
  };
  this.gs=function(t,e) {
    return this.g_Main_0(t,e);
  };
  this.g_Main_0=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_A /* 0x41 */)) {
      if(1){
        r=m=1;   // Line 16
        k.KDC(0,t);
        k.KO(-1,t,"exay");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_B /* 0x42 */)) {
      if(k.KFCM(4,t,[{t:'a',a:this.s_diaeresisBase_6},'x',{t:'a',a:this.s_diaeresisBase_6},'y'])&&this.s_nfc_5===this.s8){
        r=m=1;   // Line 20
        k.KDC(4,t);
        k.KIO(-1,this.s_diaeresisBase_6,1,t);
        k.KO(-1,t,"x");
      }
    }
    return r;
  };
}
