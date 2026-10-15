if(typeof keyman === 'undefined') {
  console.log('Keyboard requires KeymanWeb 10.0 or later');
  if(typeof tavultesoft !== 'undefined') tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");
} else {
KeymanWeb.KR(new Keyboard_k_0505___options_double_set_reset());
}
function Keyboard_k_0505___options_double_set_reset()
{
  var modCodes = keyman.osk.modifierCodes;
  var keyCodes = keyman.osk.keyCodes;

  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0505___options_double_set_reset";
  this.KN="0505 - options double set reset";
  this.KMINVER="10.0";
  this.KV=null;
  this.KDU=0;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=0 /* 0x0000 */;
  this.s_foo_4=KeymanWeb.KLOAD(this.KI,"foo","0");
  this.s7="1";
  this.s8="2";
  this.s9="3";
  this.s10="2";
  this.s11="3";
  this.s12="0";
  this.KVS=['s_foo_4'];
  this.gs=function(t,e) {
    return this.g_Main_0(t,e);
  };
  this.gs=function(t,e) {
    return this.g_Main_0(t,e);
  };
  this.g_Main_0=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_A /* 0x41 */)) {
      if(this.s_foo_4===this.s7){
        r=m=1;   // Line 17
        k.KDC(0,t);
        k.KO(-1,t,"foo.");
        this.s_foo_4=this.s8;
        this.s_foo_4=k.KLOAD(this.KI,"foo","0");
        this.s_foo_4=this.s9;
        this.s_foo_4=k.KLOAD(this.KI,"foo","0");
      }
      else if(this.s_foo_4===this.s10){
        r=m=1;   // Line 18
        k.KDC(0,t);
        k.KO(-1,t,"bar.");
      }
      else if(this.s_foo_4===this.s11){
        r=m=1;   // Line 19
        k.KDC(0,t);
        k.KO(-1,t,"baz.");
      }
      else if(this.s_foo_4===this.s12){
        r=m=1;   // Line 20
        k.KDC(0,t);
        k.KO(-1,t,"no foo.");
      }
    }
    return r;
  };
}
