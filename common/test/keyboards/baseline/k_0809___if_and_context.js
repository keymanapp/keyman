if(typeof keyman === 'undefined') {
  console.log('Keyboard requires KeymanWeb 10.0 or later');
  if(typeof tavultesoft !== 'undefined') tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");
} else {
KeymanWeb.KR(new Keyboard_k_0809___if_and_context());
}
function Keyboard_k_0809___if_and_context()
{
  var modCodes = keyman.osk.modifierCodes;
  var keyCodes = keyman.osk.keyCodes;

  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0809___if_and_context";
  this.KN="0809 - if_and_context";
  this.KMINVER="10.0";
  this.KV=null;
  this.KDU=0;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=0 /* 0x0000 */;
  this.s_cons_4="mnpqrstv";
  this.s_ifx_5=KeymanWeb.KLOAD(this.KI,"ifx","1");
  this.s8="1";
  this.KVS=['s_ifx_5'];
  this.gs=function(t,e) {
    return this.g_Main_0(t,e);
  };
  this.gs=function(t,e) {
    return this.g_Main_0(t,e);
  };
  this.g_Main_0=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_D /* 0x44 */)) {
      if(k.KFCM(1,t,[{t:'a',a:this.s_cons_4}])&&this.s_ifx_5===this.s8){
        r=m=1;   // Line 16
        k.KDC(1,t);
        k.KO(-1,t,"4");
        k.KIO(-1,this.s_cons_4,1,t);
        k.KO(-1,t,"5");
      }
    }
    return r;
  };
}
