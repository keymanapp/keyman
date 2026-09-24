
KeymanWeb.KR(new Keyboard_k_0202___alt());

function Keyboard_k_0202___alt()
{
  
  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0202___alt";
  this.KN="0202 - alt";
  this.KMINVER="9.0";
  this.KV=null;
  this.KDU=0;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=0x0040;
  this.KVS=[];
  this.gs=function(t,e) {
    return this.g_main_0(t,e);
  };
  this.gs=function(t,e) {
    return this.g_main_0(t,e);
  };
  this.g_main_0=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, 0x4040, 0x41)) {
      if(k.KCM(1,t,"c",1)){
        r=m=1;   // Line 13
        k.KO(1,t,"cd");
      }
    }
    return r;
  };
}
