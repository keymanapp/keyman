
KeymanWeb.KR(new Keyboard_test_context_in_context_9());

function Keyboard_test_context_in_context_9()
{
  
  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_test_context_in_context_9";
  this.KN="test context(n) in context, #9930, v9.0";
  this.KMINVER="9.0";
  this.KV=null;
  this.KDU=0;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=0x0010;
  this.s_liveQwerty_6="qwerty";
  this.s_deadQwerty_7="123456";
  this.KVS=[];
  this.gs=function(t,e) {
    return this.g_main_0(t,e);
  };
  this.gs=function(t,e) {
    return this.g_main_0(t,e);
  };
  this.g_main_0=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, 0x4010, 0x31)) {
      if(k.KA(0,k.KC(1,1,t),this.s_liveQwerty_6)){
        r=m=1;   // Line 13
        k.KO(1,t,"?");
        k.KIO(-1,this.s_deadQwerty_7,1,t);
        k.KIO(-1,this.s_deadQwerty_7,1,t);
      }
    }
    else if(k.KKM(e, 0x4000, 0xBE)) {
      if(k.KCM(3,t,"?",1)&&k.KA(1,k.KC(2,1,t),this.s_deadQwerty_7)&&k.KCCM(1,2,t)){
        r=m=1;   // Line 17
        k.KO(3,t,"(");
        k.KIO(-1,this.s_liveQwerty_6,2,t);
        k.KIO(-1,this.s_liveQwerty_6,2,t);
        k.KO(-1,t,")");
      }
    }
    return r;
  };
}
