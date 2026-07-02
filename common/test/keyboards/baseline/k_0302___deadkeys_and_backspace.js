
KeymanWeb.KR(new Keyboard_k_0302___deadkeys_and_backspace());

function Keyboard_k_0302___deadkeys_and_backspace()
{
  
  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0302___deadkeys_and_backspace";
  this.KN="0302- deadkeys and backspace";
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
    if(k.KKM(e, 0x4000, 0x08)) {
      if(k.KCM(2,t,"c",1)&&k.KDM(1,t,11)&&k.KCM(1,t,"d",1)){
        r=m=1;   // Line 32
        k.KO(2,t," ok");
      }
      else if(k.KCM(2,t,"cd",2)){
        r=m=1;   // Line 31
        k.KO(2,t," fail");
      }
    }
    else if(k.KKM(e, 0x4000, 0x31)) {
      if(1){
        r=m=1;   // Line 24
        k.KDO(0,t,0);
      }
    }
    else if(k.KKM(e, 0x4000, 0x32)) {
      if(1){
        r=m=1;   // Line 25
        k.KO(0,t,"a");
        k.KDO(-1,t,1);
      }
    }
    else if(k.KKM(e, 0x4000, 0x33)) {
      if(1){
        r=m=1;   // Line 26
        k.KDO(0,t,2);
        k.KO(-1,t,"a");
      }
    }
    else if(k.KKM(e, 0x4000, 0x34)) {
      if(1){
        r=m=1;   // Line 27
        k.KDO(0,t,3);
        k.KDO(-1,t,4);
      }
    }
    else if(k.KKM(e, 0x4000, 0x35)) {
      if(1){
        r=m=1;   // Line 28
        k.KO(0,t,"a");
        k.KDO(-1,t,5);
        k.KDO(-1,t,6);
        k.KO(-1,t,"b");
      }
    }
    else if(k.KKM(e, 0x4000, 0x36)) {
      if(1){
        r=m=1;   // Line 29
        k.KO(0,t,"a");
        k.KDO(-1,t,7);
        k.KDO(-1,t,8);
        k.KO(-1,t,"b");
        k.KDO(-1,t,9);
        k.KDO(-1,t,10);
      }
    }
    else if(k.KKM(e, 0x4000, 0x37)) {
      if(1){
        r=m=1;   // Line 30
        k.KO(0,t,"c");
        k.KDO(-1,t,11);
        k.KO(-1,t,"de");
      }
    }
    return r;
  };
}
