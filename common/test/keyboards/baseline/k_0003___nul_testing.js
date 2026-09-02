
KeymanWeb.KR(new Keyboard_k_0003___nul_testing());

function Keyboard_k_0003___nul_testing()
{
  
  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0003___nul_testing";
  this.KN="0003 - nul testing";
  this.KMINVER="9.0";
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
    if(k.KKM(e, 0x4000, 0x41)) {
      if(k.KN(0,t)){
        r=m=1;   // Line 12
        k.KO(0,t,"OK");
      }
    }
    return r;
  };
}
