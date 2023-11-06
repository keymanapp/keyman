
KeymanWeb.KR(new Keyboard_test_contextn_in_output_9());

function Keyboard_test_contextn_in_output_9()
{
  
  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_test_contextn_in_output_9";
  this.KN="test context(n) in output, #9930, v9.0";
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
      if(k.KA(0,k.KC(2,1,t),this.s_liveQwerty_6)&&k.KCM(1,t,"1",1)){
        r=m=1;   // Line 13
        k.KO(2,t,"?");
        k.KIO(-1,this.s_deadQwerty_7,1,t);
        k.KO(-1,t,"1");
      }
    }
    return r;
  };
}
