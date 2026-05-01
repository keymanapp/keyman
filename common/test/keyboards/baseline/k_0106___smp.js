if(typeof keyman === 'undefined') {
  console.log('Keyboard requires KeymanWeb 10.0 or later');
  if(typeof tavultesoft !== 'undefined') tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");
} else {
KeymanWeb.KR(new Keyboard_k_0106___smp());
}
function Keyboard_k_0106___smp()
{
  var modCodes = keyman.osk.modifierCodes;
  var keyCodes = keyman.osk.keyCodes;

  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0106___smp";
  this.KN="0106 - smp";
  this.KMINVER="10.0";
  this.KV=null;
  this.KDU=0;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=0 /* 0x0000 */;
  this.KS=1;
  this.KVS=[];
  this.gs=function(t,e) {
    return this.g_Main_0(t,e);
  };
  this.gs=function(t,e) {
    return this.g_Main_0(t,e);
  };
  this.g_Main_0=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_1 /* 0x31 */)) {
      if(1){
        r=m=1;   // Line 15
        k.KDC(0,t);
        k.KO(-1,t,"🙂");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_2 /* 0x32 */)) {
      if(k.KFCM(1,t,['🙂'])){
        r=m=1;   // Line 16
        k.KDC(1,t);
        k.KO(-1,t,"🙂hi😀");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_3 /* 0x33 */)) {
      if(k.KFCM(1,t,['😀'])){
        r=m=1;   // Line 17
        k.KDC(1,t);
        k.KO(-1,t,"x");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_4 /* 0x34 */)) {
      if(k.KFCM(4,t,['🙂','h','i','x'])){
        r=m=1;   // Line 18
        k.KDC(4,t);
        k.KO(-1,t,"😁y");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_5 /* 0x35 */)) {
      if(1){
        r=m=1;   // Line 22
        k.KDC(0,t);
        k.KO(-1,t,"𐌳");
        r=this.g_second_1(t,e);
        m=2;
      }
    }
    return r;
  };
  this.g_second_1=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
      if(k.KFCM(1,t,['𐌳'])){
        m=1;   // Line 26
        k.KDC(1,t);
        k.KO(-1,t,"x");
      }
    return r;
  };
}
