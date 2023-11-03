if(typeof keyman === 'undefined') {
  console.log('Keyboard requires KeymanWeb 10.0 or later');
  if(typeof tavultesoft !== 'undefined') tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");
} else {
KeymanWeb.KR(new Keyboard_test_8568_deadkeys());
}
function Keyboard_test_8568_deadkeys()
{
  var modCodes = keyman.osk.modifierCodes;
  var keyCodes = keyman.osk.keyCodes;

  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_test_8568_deadkeys";
  this.KN="Testcase for deadkeys bug (#8568)";
  this.KMINVER="10.0";
  this.KV=null;
  this.KDU=0;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=0 /* 0x0000 */;
  this.KVER="17.0.203.0";
  this.KVS=[];
  this.gs=function(t,e) {
    return this.g_main_0(t,e);
  };
  this.gs=function(t,e) {
    return this.g_main_0(t,e);
  };
  this.g_main_0=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_BKSP /* 0x08 */)) {
      if(k.KFCM(3,t,['c',{t:'d',d:0},'e'])){
        r=m=1;   // Line 12
        k.KDC(3,t);
        k.KO(-1,t,"pass");
      }
      else if(k.KFCM(2,t,['c','e'])){
        r=m=1;   // Line 11
        k.KDC(2,t);
        k.KO(-1,t,"fail");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_X /* 0x58 */)) {
      if(1){
        r=m=1;   // Line 10
        k.KDC(0,t);
        k.KO(-1,t,"c");
        k.KDO(-1,t,0);
        k.KO(-1,t,"e");
      }
    }
    return r;
  };
}
