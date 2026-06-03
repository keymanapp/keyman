if(typeof keyman === 'undefined') {
  console.log('Keyboard requires KeymanWeb 10.0 or later');
  if(typeof tavultesoft !== 'undefined') tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");
} else {
KeymanWeb.KR(new Keyboard_k_0401___multiple_groups());
}
function Keyboard_k_0401___multiple_groups()
{
  var modCodes = keyman.osk.modifierCodes;
  var keyCodes = keyman.osk.keyCodes;

  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0401___multiple_groups";
  this.KN="401 - multiple groups";
  this.KMINVER="10.0";
  this.KV=null;
  this.KDU=0;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=0 /* 0x0000 */;
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
        r=m=1;   // Line 13
        k.KDC(0,t);
        k.KO(-1,t,"a");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_2 /* 0x32 */)) {
      if(k.KFCM(1,t,['b'])){
        r=m=1;   // Line 14
        k.KDC(1,t);
        k.KO(-1,t,"b");
        r=this.g_b_2(t,e);
        m=2;
      }
    }
    if(m==1) {
    
      k.KDC(-1,t);
      r=this.g_a_1(t,e);
      m=2;
    }
    return r;
  };
  this.g_a_1=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
      if(k.KFCM(1,t,['a'])){
        m=1;   // Line 20
        k.KDC(1,t);
        k.KO(-1,t,"b");
      }
    return r;
  };
  this.g_b_2=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
      if(k.KFCM(1,t,['b'])){
        m=1;   // Line 24
        k.KDC(1,t);
        k.KO(-1,t,"abc");
      }
    return r;
  };
}
