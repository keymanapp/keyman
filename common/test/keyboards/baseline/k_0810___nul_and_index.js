if(typeof keyman === 'undefined') {
  console.log('Keyboard requires KeymanWeb 10.0 or later');
  if(typeof tavultesoft !== 'undefined') tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");
} else {
KeymanWeb.KR(new Keyboard_k_0810___nul_and_index());
}
function Keyboard_k_0810___nul_and_index()
{
  var modCodes = keyman.osk.modifierCodes;
  var keyCodes = keyman.osk.keyCodes;

  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0810___nul_and_index";
  this.KN="0810 - nul_and_index";
  this.KMINVER="10.0";
  this.KV=null;
  this.KDU=0;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=0 /* 0x0000 */;
  this.s_cons_4="mnpqrstv";
  this.s_outs_5="MNPQRSTV";
  this.KVS=[];
  this.gs=function(t,e) {
    return this.g_Main_0(t,e);
  };
  this.gs=function(t,e) {
    return this.g_Main_0(t,e);
  };
  this.g_Main_0=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_B /* 0x42 */)) {
      if(k.KFCM(2,t,[{t:'n'},{t:'a',a:this.s_cons_4}])){
        r=m=1;   // Line 16
        k.KDC(1,t);
        k.KO(-1,t,"2");
        k.KIO(-1,this.s_outs_5,1,t);
        k.KO(-1,t,"3");
      }
    }
    return r;
  };
}
