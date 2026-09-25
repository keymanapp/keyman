
KeymanWeb.KR(new Keyboard_k_0806___modifier_keys_keep_context());

function Keyboard_k_0806___modifier_keys_keep_context()
{
  
  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0806___modifier_keys_keep_context";
  this.KN="0806 - modifier keys keep context";
  this.KMINVER="9.0";
  this.KV=null;
  this.KDU=0;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=0x0000;
  this.KVS=[];
  this.gs=function(t,e) {
    return this.g_main_0(t,e);
  };
  this.gs=function(t,e) {
    return this.g_main_0(t,e);
  };
  this.g_main_0=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, 0x4000, 0x41)) {
      if(1){
        r=m=1;   // Line 13
        k.KO(0,t,"a");
      }
    }
    else if(k.KKM(e, 0x4000, 0x42)) {
      if(k.KCM(1,t,"a",1)){
        r=m=1;   // Line 15
        k.KO(1,t,"pass.");
      }
    }
    return r;
  };
}
