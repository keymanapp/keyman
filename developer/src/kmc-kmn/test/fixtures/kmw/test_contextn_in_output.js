if(typeof keyman === 'undefined') {
  console.log('Keyboard requires KeymanWeb 10.0 or later');
  if(typeof tavultesoft !== 'undefined') tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");
} else {
KeymanWeb.KR(new Keyboard_test_contextn_in_output());
}
function Keyboard_test_contextn_in_output()
{
  var modCodes = keyman.osk.modifierCodes;
  var keyCodes = keyman.osk.keyCodes;

  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_test_contextn_in_output";
  this.KN="test context(n) in output, #9930, v10.0";
  this.KMINVER="10.0";
  this.KV=null;
  this.KDU=0;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=modCodes.SHIFT /* 0x0010 */;
  this.s_liveQwerty_6="qwerty";
  this.s_deadQwerty_7=[{t:'d',d:0},{t:'d',d:1},{t:'d',d:2},{t:'d',d:3},{t:'d',d:4},{t:'d',d:5}];
  this.KVS=[];
  this.gs=function(t,e) {
    return this.g_main_0(t,e);
  };
  this.gs=function(t,e) {
    return this.g_main_0(t,e);
  };
  this.g_main_0=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_1 /* 0x31 */)) {
      if(k.KFCM(2,t,[{t:'a',a:this.s_liveQwerty_6},'1'])){
        r=m=1;   // Line 13
        k.KDC(2,t);
        k.KO(-1,t,"?");
        k.KIO(-1,this.s_deadQwerty_7,1,t);
        k.KO(-1,t,"1");
      }
    }
    return r;
  };
}
