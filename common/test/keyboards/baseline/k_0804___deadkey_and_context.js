if(typeof keyman === 'undefined') {
  console.log('Keyboard requires KeymanWeb 10.0 or later');
  if(typeof tavultesoft !== 'undefined') tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");
} else {
KeymanWeb.KR(new Keyboard_k_0804___deadkey_and_context());
}
function Keyboard_k_0804___deadkey_and_context()
{
  var modCodes = keyman.osk.modifierCodes;
  var keyCodes = keyman.osk.keyCodes;

  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0804___deadkey_and_context";
  this.KN="0804 - deadkey and context";
  this.KMINVER="10.0";
  this.KV=null;
  this.KDU=0;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=modCodes.SHIFT /* 0x0010 */;
  this.KVS=[];
  this.gs=function(t,e) {
    return this.g_Main_0(t,e);
  };
  this.gs=function(t,e) {
    return this.g_Main_0(t,e);
  };
  this.g_Main_0=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_SLASH /* 0xBF */)) {
      if(k.KFCM(7,t,['<',{t:'d',d:0},'a',{t:'d',d:1},'b',{t:'d',d:2},'>'])){
        r=m=1;   // Line 18
        k.KDC(7,t);
        k.KO(-1,t,"correct");
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Y /* 0x59 */)) {
      if(1){
        r=m=1;   // Line 13
        k.KDC(0,t);
        k.KDO(-1,t,0);
        k.KO(-1,t,"a");
        k.KDO(-1,t,1);
        k.KO(-1,t,"b");
        k.KDO(-1,t,2);
      }
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Z /* 0x5A */)) {
      if(k.KFCM(5,t,[{t:'d',d:0},'a',{t:'d',d:1},'b',{t:'d',d:2}])){
        r=m=1;   // Line 17
        k.KDC(5,t);
        k.KO(-1,t,"<");
        k.KDO(-1,t,0);
        k.KO(-1,t,"a");
        k.KDO(-1,t,1);
        k.KO(-1,t,"b");
        k.KDO(-1,t,2);
        k.KO(-1,t,">");
      }
    }
    return r;
  };
}
