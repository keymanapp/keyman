if(typeof keyman === 'undefined') {
  console.log('Keyboard requires KeymanWeb 10.0 or later');
  if(typeof tavultesoft !== 'undefined') tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");
} else {
KeymanWeb.KR(new Keyboard_test_options());
}
function Keyboard_test_options()
{
  var modCodes = keyman.osk.modifierCodes;
  var keyCodes = keyman.osk.keyCodes;

  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_test_options";
  this.KN="test_options";
  this.KMINVER="10.0";
  this.KV=null;
  this.KDU=0;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=0 /* 0x0000 */;
  this.s_foo_5=KeymanWeb.KLOAD(this.KI,"foo","0");
  this.s8="1";
  this.s9="0";
  this.s10="1";
  this.s11="0";
  this.KVS=['s_foo_5'];
  this.gs=function(t,e) {
    return this.g_Main_0(t,e);
  };
  this.gs=function(t,e) {
    return this.g_Main_0(t,e);
  };
  this.g_Main_0=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_0 /* 0x30 */)) {
      if(1){
        r=m=1;   // Line 16
        k.KDC(0,t);
        this.s_foo_5=this.s11;
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_1 /* 0x31 */)) {
      if(1){
        r=m=1;   // Line 15
        k.KDC(0,t);
        this.s_foo_5=this.s10;
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_A /* 0x41 */)) {
      if(this.s_foo_5===this.s8){
        r=m=1;   // Line 13
        k.KDC(0,t);
        k.KO(-1,t,"foo.");
      }
      else if(this.s_foo_5===this.s9){
        r=m=1;   // Line 14
        k.KDC(0,t);
        k.KO(-1,t,"no foo.");
      }
    }
    return r;
  };
}
