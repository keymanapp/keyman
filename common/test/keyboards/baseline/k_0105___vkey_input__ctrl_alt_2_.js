
KeymanWeb.KR(new Keyboard_k_0105___vkey_input__ctrl_alt_2_());

function Keyboard_k_0105___vkey_input__ctrl_alt_2_()
{
  
  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0105___vkey_input__ctrl_alt_2_";
  this.KN="0105 - vkey input (ctrl alt 2)";
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
    if(k.KKM(e, 0x4060, 0x41)) {
      if(1){
        r=m=1;   // Line 14
        k.KO(0,t,"ก");
      }
    }
    else if(k.KKM(e, 0x4060, 0x43)) {
      if(1){
        r=m=1;   // Line 15
        k.KO(0,t,"ฃ");
      }
    }
    return r;
  };
}
