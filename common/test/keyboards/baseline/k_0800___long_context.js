
KeymanWeb.KR(new Keyboard_k_0800___long_context());

function Keyboard_k_0800___long_context()
{
  
  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0800___long_context";
  this.KN="0800 - long context";
  this.KMINVER="6.0";
  this.KV=null;
  this.KDU=0;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=0x0000;
  this.KVS=[];
  this.gs=function(t,e) {
    return this.g_Main_0(t,e);
  };
  this.gs=function(t,e) {
    return this.g_Main_0(t,e);
  };
  this.g_Main_0=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, 0x4000, 0x20)) {
      if(1){
        r=m=1;   // Line 13
        k.KO(0,t," ");
      }
    }
    else if(k.KKM(e, 0x4000, 0x46)) {
      if(k.KCM(1,t," ",1)){
        r=m=1;   // Line 16
        k.KO(1,t," ");
      }
    }
    return r;
  };
}
