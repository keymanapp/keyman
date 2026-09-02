
KeymanWeb.KR(new Keyboard_k_0203___generic_ctrlalt());

function Keyboard_k_0203___generic_ctrlalt()
{
  
  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0203___generic_ctrlalt";
  this.KN="0203 - generic ctrlalt";
  this.KMINVER="6.0";
  this.KV=null;
  this.KDU=0;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=0x0060;
  this.KVS=[];
  this.gs=function(t,e) {
    return this.g_Main_0(t,e);
  };
  this.gs=function(t,e) {
    return this.g_Main_0(t,e);
  };
  this.g_Main_0=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, 0x4020, 0x42)) {
      if(k.KCM(1,t,"a",1)){
        r=m=1;   // Line 13
        k.KO(1,t,"b");
      }
    }
    else if(k.KKM(e, 0x4040, 0x43)) {
      if(k.KCM(1,t,"a",1)){
        r=m=1;   // Line 14
        k.KO(1,t,"c");
      }
    }
    return r;
  };
}
