
KeymanWeb.KR(new Keyboard_k_0004___space_mnemonic_kbd());

function Keyboard_k_0004___space_mnemonic_kbd()
{
  
  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0004___space_mnemonic_kbd";
  this.KN="0004 - space mnemonic kbd";
  this.KMINVER="9.0";
  this.KV=null;
  this.KDU=0;
  this.KH='';
  this.KM=1;
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
    if(k.KKM(e, 0x4000, 0x20)) {
      if(k.KCM(2,t,"ab",2)){
        r=m=1;   // Line 14
        k.KO(2,t,"X");
      }
    }
    else if(k.KKM(e, 0x4000, 0x64)) {
      if(k.KCM(2,t,"c ",2)){
        r=m=1;   // Line 15
        k.KO(2,t,"Y");
      }
    }
    else if(k.KKM(e, 0x4000, 0x65)) {
      if(k.KCM(2,t," d",2)){
        r=m=1;   // Line 16
        k.KO(2,t,"Z");
      }
    }
    return r;
  };
}
