
KeymanWeb.KR(new Keyboard_k_0107___punctkeys());

function Keyboard_k_0107___punctkeys()
{
  
  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_k_0107___punctkeys";
  this.KN="0107 - punctkeys";
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
    if(k.KKM(e, 0x4000, 0xE2)) {
      if(k.KCM(1,t,"a",1)){
        r=m=1;   // Line 24
        k.KO(1,t,"p");
      }
    }
    else if(k.KKM(e, 0x4000, 0xDE)) {
      if(k.KCM(1,t,"a",1)){
        r=m=1;   // Line 23
        k.KO(1,t,"o");
      }
    }
    else if(k.KKM(e, 0x4000, 0xBC)) {
      if(k.KCM(1,t,"a",1)){
        r=m=1;   // Line 15
        k.KO(1,t,"g");
      }
    }
    else if(k.KKM(e, 0x4000, 0xBD)) {
      if(k.KCM(1,t,"a",1)){
        r=m=1;   // Line 16
        k.KO(1,t,"h");
      }
    }
    else if(k.KKM(e, 0x4000, 0xBE)) {
      if(k.KCM(1,t,"a",1)){
        r=m=1;   // Line 17
        k.KO(1,t,"i");
      }
    }
    else if(k.KKM(e, 0x4000, 0xBF)) {
      if(k.KCM(1,t,"a",1)){
        r=m=1;   // Line 18
        k.KO(1,t,"j");
      }
    }
    else if(k.KKM(e, 0x4000, 0xBA)) {
      if(k.KCM(1,t,"a",1)){
        r=m=1;   // Line 13
        k.KO(1,t,"e");
      }
    }
    else if(k.KKM(e, 0x4000, 0xBB)) {
      if(k.KCM(1,t,"a",1)){
        r=m=1;   // Line 14
        k.KO(1,t,"f");
      }
    }
    else if(k.KKM(e, 0x4000, 0xDB)) {
      if(k.KCM(1,t,"a",1)){
        r=m=1;   // Line 20
        k.KO(1,t,"l");
      }
    }
    else if(k.KKM(e, 0x4000, 0xDC)) {
      if(k.KCM(1,t,"a",1)){
        r=m=1;   // Line 21
        k.KO(1,t,"m");
      }
    }
    else if(k.KKM(e, 0x4000, 0xDD)) {
      if(k.KCM(1,t,"a",1)){
        r=m=1;   // Line 22
        k.KO(1,t,"n");
      }
    }
    else if(k.KKM(e, 0x4000, 0xC0)) {
      if(k.KCM(1,t,"a",1)){
        r=m=1;   // Line 19
        k.KO(1,t,"k");
      }
    }
    return r;
  };
}
