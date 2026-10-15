
KeymanWeb.KR(new Keyboard_k_0103___vkey_input__shift_ctrl_());

function Keyboard_k_0103___vkey_input__shift_ctrl_()
{
  
  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0103___vkey_input__shift_ctrl_";
  this.KN="0103 - vkey input (shift ctrl)";
  this.KMINVER="6.0";
  this.KV=null;
  this.KDU=0;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=0x0030;
  this.KVS=[];
  this.gs=function(t,e) {
    return this.g_Main_0(t,e);
  };
  this.gs=function(t,e) {
    return this.g_Main_0(t,e);
  };
  this.g_Main_0=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, 0x4030, 0x41)) {
      if(1){
        r=m=1;   // Line 13
        k.KO(0,t,"ก");
      }
    }
    else if(k.KKM(e, 0x4010, 0x42)) {
      if(1){
        r=m=1;   // Line 14
        k.KO(0,t,"ข");
      }
    }
    else if(k.KKM(e, 0x4030, 0x43)) {
      if(1){
        r=m=1;   // Line 15
        k.KO(0,t,"ฃ");
      }
    }
    else if(k.KKM(e, 0x4010, 0x46)) {
      if(k.KCM(2,t,"DE",2)){
        r=m=1;   // Line 17
        k.KO(2,t,"คฅฆ");
      }
    }
    return r;
  };
}
