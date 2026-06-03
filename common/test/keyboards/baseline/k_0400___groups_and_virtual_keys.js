
KeymanWeb.KR(new Keyboard_k_0400___groups_and_virtual_keys());

function Keyboard_k_0400___groups_and_virtual_keys()
{
  
  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0400___groups_and_virtual_keys";
  this.KN="0400 - groups and virtual keys";
  this.KMINVER="9.0";
  this.KV=null;
  this.KDU=0;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=0x0070;
  this.KVS=[];
  this.gs=function(t,e) {
    return this.g_UMain_0(t,e);
  };
  this.gs=function(t,e) {
    return this.g_UMain_0(t,e);
  };
  this.g_UMain_0=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, 0x4020, 0x32)) {
      if(1){
        r=m=1;   // Line 16
        k.KDO(0,t,0);
      }
    }
    else if(k.KKM(e, 0x4040, 0x32)) {
      if(1){
        r=m=1;   // Line 17
        k.KDO(0,t,0);
      }
    }
    else if(k.KKM(e, 0x4010, 0x32)) {
      if(1){
        r=m=1;   // Line 18
        k.KDO(0,t,1);
      }
    }
    else if(k.KKM(e, 0x4000, 0x41)) {
      if(1){
        r=m=1;   // Line 15
        k.KO(0,t,"α");
      }
    }
    if(m==1) {
    
      r=this.g_DK1_1(t,e);
      m=2;
    }
    return r;
  };
  this.g_DK1_1=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
      if(k.KDM(1,t,0)&&k.KCM(1,t,"α",1)){
        m=1;   // Line 25
        k.KO(1,t,"ᾱ");
      }
      else if(k.KDM(1,t,1)&&k.KCM(1,t,"α",1)){
        m=1;   // Line 26
        k.KO(1,t,"ᾰ");
      }
    return r;
  };
}
