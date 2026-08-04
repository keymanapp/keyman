
KeymanWeb.KR(new Keyboard_k_0200___ralt());

function Keyboard_k_0200___ralt()
{
  
  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0200___ralt";
  this.KN="0200 - ralt";
  this.KMINVER="9.0";
  this.KV=null;
  this.KDU=0;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=0x0060;
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
        r=m=1;   // Line 14
        k.KO(0,t,"ז");
      }
    }
    else if(k.KKM(e, 0x4000, 0x4F)) {
      if(1){
        r=m=1;   // Line 15
        k.KO(0,t,"ר");
      }
    }
    else if(k.KKM(e, 0x4060, 0x41)) {
      if(1){
        r=m=1;   // Line 16
        k.KO(0,t,"b");
      }
    }
    else if(k.KKM(e, 0x4060, 0x4F)) {
      if(1){
        r=m=1;   // Line 17
        k.KO(0,t,"c");
      }
    }
    return r;
  };
}
