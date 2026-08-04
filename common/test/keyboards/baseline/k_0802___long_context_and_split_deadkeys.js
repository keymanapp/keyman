
KeymanWeb.KR(new Keyboard_k_0802___long_context_and_split_deadkeys());

function Keyboard_k_0802___long_context_and_split_deadkeys()
{
  
  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0802___long_context_and_split_deadkeys";
  this.KN="0802 - long context and split deadkeys";
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
      if(k.KCM(64,t,"\x01abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijk",64)){
        r=m=1;   // Line 18
        k.KO(64,t,"FAIL");
      }
      else if(k.KDM(63,t,0)&&k.KCM(63,t,"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijk",63)){
        r=m=1;   // Line 19
        k.KO(63,t,"UNEXPECTED");
      }
    }
    else if(k.KKM(e, 0x4000, 0x31)) {
      if(k.KCM(61,t,"defghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijk ",61)){
        r=m=1;   // Line 22
        k.KO(61,t,"def");
      }
    }
    else if(k.KKM(e, 0x4000, 0x32)) {
      if(k.KDM(6,t,0)&&k.KCM(6,t,"abcdef",6)){
        r=m=1;   // Line 23
        k.KO(6,t,"PASS");
      }
    }
    else if(k.KKM(e, 0x4000, 0x5A)) {
      if(1){
        r=m=1;   // Line 14
        k.KDO(0,t,0);
        k.KO(-1,t,"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijk");
      }
    }
    return r;
  };
}
