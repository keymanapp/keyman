
KeymanWeb.KR(new Keyboard_k_0801___long_context_and_deadkeys());

function Keyboard_k_0801___long_context_and_deadkeys()
{
  
  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0801___long_context_and_deadkeys";
  this.KN="0801 - long context and deadkeys";
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
    if(k.KKM(e, 0x4000, 0x31)) {
      if(k.KDM(60,t,0)&&k.KCM(60,t,"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh",60)){
        r=m=1;   // Line 18
        k.KO(-1,t,"1");
      }
    }
    else if(k.KKM(e, 0x4000, 0x32)) {
      if(k.KCM(63,t,"\x08\x01abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh1",63)){
        r=m=1;   // Line 22
        k.KO(63,t,"FAIL TEST 2");
      }
      else if(k.KCM(62,t,"\x01abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh1",62)){
        r=m=1;   // Line 23
        k.KO(62,t,"FAIL TEST 2");
      }
      else if(k.KCM(61,t,"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh1",61)){
        r=m=1;   // Line 24
        k.KO(-1,t,"2");
      }
    }
    else if(k.KKM(e, 0x4000, 0x33)) {
      if(k.KCM(64,t,"\x08\x01abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh12",64)){
        r=m=1;   // Line 28
        k.KO(64,t,"FAIL TEST 3");
      }
      else if(k.KCM(63,t,"\x01abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh12",63)){
        r=m=1;   // Line 29
        k.KO(63,t,"FAIL TEST 3");
      }
      else if(k.KCM(62,t,"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh12",62)){
        r=m=1;   // Line 30
        k.KO(62,t,"abcdefghijklmnopqrstuvwxyz123");
      }
    }
    else if(k.KKM(e, 0x4000, 0x34)) {
      if(k.KDM(29,t,0)&&k.KCM(29,t,"abcdefghijklmnopqrstuvwxyz123",29)){
        r=m=1;   // Line 33
        k.KO(29,t,"PASS");
      }
    }
    else if(k.KKM(e, 0x4000, 0x5A)) {
      if(1){
        r=m=1;   // Line 15
        k.KDO(0,t,0);
        k.KO(-1,t,"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh");
      }
    }
    return r;
  };
}
