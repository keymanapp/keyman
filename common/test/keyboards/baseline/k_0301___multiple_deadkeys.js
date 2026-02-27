
KeymanWeb.KR(new Keyboard_k_0301___multiple_deadkeys());

function Keyboard_k_0301___multiple_deadkeys()
{
  
  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0301___multiple_deadkeys";
  this.KN="0301 - multiple deadkeys";
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
    if(k.KKM(e, 0x4000, 0x31)) {
      if(1){
        r=m=1;   // Line 22
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
        r=m=1;   // Line 28
        k.KDO(0,t,2);
        k.KO(-1,t,"a");
      }
    }
    else if(k.KKM(e, 0x4000, 0x34)) {
      if(1){
        r=m=1;   // Line 31
        k.KDO(0,t,3);
        k.KDO(-1,t,4);
      }
    }
    else if(k.KKM(e, 0x4000, 0x35)) {
      if(1){
        r=m=1;   // Line 34
        k.KO(0,t,"a");
        k.KDO(-1,t,5);
        k.KDO(-1,t,6);
      }
    }
    else if(k.KKM(e, 0x4000, 0x36)) {
      if(1){
        r=m=1;   // Line 37
        k.KO(0,t,"a");
        k.KDO(-1,t,7);
        k.KDO(-1,t,8);
        k.KO(-1,t,"b");
      }
    }
    else if(k.KKM(e, 0x4000, 0x37)) {
      if(1){
        r=m=1;   // Line 40
        k.KDO(0,t,9);
        k.KDO(-1,t,10);
        k.KO(-1,t,"a");
      }
    }
    else if(k.KKM(e, 0x4000, 0x38)) {
      if(1){
        r=m=1;   // Line 45
        k.KDO(0,t,11);
        k.KDO(-1,t,12);
        k.KDO(-1,t,13);
      }
    }
    else if(k.KKM(e, 0x4000, 0x39)) {
      if(1){
        r=m=1;   // Line 56
        k.KO(0,t,"{");
        k.KDO(-1,t,14);
        k.KDO(-1,t,15);
        k.KO(-1,t,"}");
        k.KDO(-1,t,16);
        k.KDO(-1,t,17);
      }
    }
    else if(k.KKM(e, 0x4000, 0x58)) {
      if(k.KCM(2,t,"{",1)&&k.KDM(1,t,14)&&k.KDM(1,t,18)&&k.KCM(1,t,"}",1)&&k.KDM(0,t,19)&&k.KDM(0,t,20)){
        r=m=1;   // Line 59
        k.KO(2,t,"9=OK");
      }
      else if(k.KCM(2,t,"a",1)&&k.KDM(1,t,7)&&k.KDM(1,t,8)&&k.KCM(1,t,"b",1)){
        r=m=1;   // Line 38
        k.KO(2,t,"6=OK ");
      }
      else if(k.KDM(1,t,15)&&k.KCM(1,t,"}",1)&&k.KDM(0,t,16)&&k.KDM(0,t,17)){
        r=m=1;   // Line 58
        k.KDO(1,t,18);
        k.KO(-1,t,"}");
        k.KDO(-1,t,19);
        k.KDO(-1,t,20);
      }
      else if(k.KCM(1,t,"a",1)&&k.KDM(0,t,5)&&k.KDM(0,t,6)){
        r=m=1;   // Line 35
        k.KO(1,t,"5=OK ");
      }
      else if(k.KDM(1,t,9)&&k.KDM(1,t,10)&&k.KCM(1,t,"a",1)){
        r=m=1;   // Line 41
        k.KO(1,t,"7=OK ");
      }
      else if(k.KDM(0,t,11)&&k.KDM(0,t,12)&&k.KDM(0,t,13)){
        r=m=1;   // Line 46
        k.KO(0,t,"8=OK ");
      }
      else if(k.KCM(1,t,"a",1)&&k.KDM(0,t,1)){
        r=m=1;   // Line 26
        k.KO(1,t,"2=OK ");
      }
      else if(k.KDM(1,t,2)&&k.KCM(1,t,"a",1)){
        r=m=1;   // Line 29
        k.KO(1,t,"3=OK ");
      }
      else if(k.KDM(0,t,3)&&k.KDM(0,t,4)){
        r=m=1;   // Line 32
        k.KO(0,t,"4=OK ");
      }
      else if(k.KDM(1,t,9)&&k.KCM(1,t,"a",1)){
        r=m=1;   // Line 42
        k.KO(1,t,"7=Fail1 ");
      }
      else if(k.KDM(1,t,10)&&k.KCM(1,t,"a",1)){
        r=m=1;   // Line 43
        k.KO(1,t,"7=Fail2 ");
      }
      else if(k.KDM(0,t,11)&&k.KDM(0,t,12)){
        r=m=1;   // Line 50
        k.KO(0,t,"8=Fail4 ");
      }
      else if(k.KDM(0,t,11)&&k.KDM(0,t,13)){
        r=m=1;   // Line 51
        k.KO(0,t,"8=Fail5 ");
      }
      else if(k.KDM(0,t,12)&&k.KDM(0,t,13)){
        r=m=1;   // Line 52
        k.KO(0,t,"8=Fail6 ");
      }
      else if(k.KDM(0,t,0)){
        r=m=1;   // Line 23
        k.KO(0,t,"1=OK ");
      }
      else if(k.KDM(0,t,11)){
        r=m=1;   // Line 47
        k.KO(0,t,"8=Fail1 ");
      }
      else if(k.KDM(0,t,12)){
        r=m=1;   // Line 48
        k.KO(0,t,"8=Fail2 ");
      }
      else if(k.KDM(0,t,13)){
        r=m=1;   // Line 49
        k.KO(0,t,"8=Fail3 ");
      }
    }
    return r;
  };
}
