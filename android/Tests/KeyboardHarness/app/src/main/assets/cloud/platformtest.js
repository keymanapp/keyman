
KeymanWeb.KR(new Keyboard_platformtest());

function Keyboard_platformtest()
{
  
  this.KI="Keyboard_platformtest";
  this.KN="PlatformTest";
  this.KMINVER="9.0";
  this.KV=null;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=0x0010;
  this.s7="touch";
  this.s8="hardware";
  this.s9="windows";
  this.s10="android";
  this.s11="iOS";
  this.s12="macosx";
  this.s13="linux";
  this.s14="desktop";
  this.s15="tablet";
  this.s16="phone";
  this.s17="native";
  this.s18="web";
  this.s19="ie";
  this.s20="chrome";
  this.s21="firefox";
  this.s22="safari";
  this.s23="opera";
  this.s24="touch";
  this.s25="hardware";
  this.KVER="10.0.1103.0";
  this.gs=function(t,e) {
    return this.g_main(t,e);
  };
  this.g_main=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, 0x4010, 0x50)&&!k.KIFS(31,this.s24,t)) {   // Line 34
      r=m=1;
      k.KO(0,t," !Touch");
    }
    else if(k.KKM(e, 0x4010, 0x50)&&!k.KIFS(31,this.s25,t)) {   // Line 35
      r=m=1;
      k.KO(0,t," !Hardware");
    }
    else if(k.KKM(e, 0x4000, 0x49)&&k.KIFS(31,this.s14,t)) {   // Line 18
      r=m=1;
      k.KO(0,t," Desktop");
    }
    else if(k.KKM(e, 0x4000, 0x49)&&k.KIFS(31,this.s15,t)) {   // Line 19
      r=m=1;
      k.KO(0,t," Tablet");
    }
    else if(k.KKM(e, 0x4000, 0x49)&&k.KIFS(31,this.s16,t)) {   // Line 20
      r=m=1;
      k.KO(0,t," Phone");
    }
    else if(k.KKM(e, 0x4000, 0x49)) {   // Line 21
      r=m=1;
      k.KO(0,t," [FF Undefined]");
    }
    else if(k.KKM(e, 0x4000, 0x4F)&&k.KIFS(31,this.s9,t)) {   // Line 11
      r=m=1;
      k.KO(0,t," Windows");
    }
    else if(k.KKM(e, 0x4000, 0x4F)&&k.KIFS(31,this.s10,t)) {   // Line 12
      r=m=1;
      k.KO(0,t," Android");
    }
    else if(k.KKM(e, 0x4000, 0x4F)&&k.KIFS(31,this.s11,t)) {   // Line 13
      r=m=1;
      k.KO(0,t," iOS");
    }
    else if(k.KKM(e, 0x4000, 0x4F)&&k.KIFS(31,this.s12,t)) {   // Line 14
      r=m=1;
      k.KO(0,t," OSX");
    }
    else if(k.KKM(e, 0x4000, 0x4F)&&k.KIFS(31,this.s13,t)) {   // Line 15
      r=m=1;
      k.KO(0,t," Linux");
    }
    else if(k.KKM(e, 0x4000, 0x4F)) {   // Line 16
      r=m=1;
      k.KO(0,t," [OS Undefined]");
    }
    else if(k.KKM(e, 0x4000, 0x50)&&k.KIFS(31,this.s7,t)) {   // Line 7
      r=m=1;
      k.KO(0,t,"Touch");
    }
    else if(k.KKM(e, 0x4000, 0x50)&&k.KIFS(31,this.s8,t)) {   // Line 8
      r=m=1;
      k.KO(0,t,"Hardware");
    }
    else if(k.KKM(e, 0x4000, 0x50)) {   // Line 9
      r=m=1;
      k.KO(0,t," [UI Undefined]");
    }
    else if(k.KKM(e, 0x4000, 0x55)&&k.KIFS(31,this.s17,t)) {   // Line 23
      r=m=1;
      k.KO(0,t," Native");
    }
    else if(k.KKM(e, 0x4000, 0x55)&&k.KIFS(31,this.s18,t)) {   // Line 24
      r=m=1;
      k.KO(0,t," Web");
    }
    else if(k.KKM(e, 0x4000, 0x55)) {   // Line 25
      r=m=1;
      k.KO(0,t," [Nativeness Undefined]");
    }
    else if(k.KKM(e, 0x4000, 0x59)&&k.KIFS(31,this.s19,t)) {   // Line 27
      r=m=1;
      k.KO(0,t," IE");
    }
    else if(k.KKM(e, 0x4000, 0x59)&&k.KIFS(31,this.s20,t)) {   // Line 28
      r=m=1;
      k.KO(0,t," Chrome");
    }
    else if(k.KKM(e, 0x4000, 0x59)&&k.KIFS(31,this.s21,t)) {   // Line 29
      r=m=1;
      k.KO(0,t," Firefox");
    }
    else if(k.KKM(e, 0x4000, 0x59)&&k.KIFS(31,this.s22,t)) {   // Line 30
      r=m=1;
      k.KO(0,t," Safari");
    }
    else if(k.KKM(e, 0x4000, 0x59)&&k.KIFS(31,this.s23,t)) {   // Line 31
      r=m=1;
      k.KO(0,t," Opera");
    }
    else if(k.KKM(e, 0x4000, 0x59)) {   // Line 32
      r=m=1;
      k.KO(0,t," [Browser Undefined]");
    }
    return r;
  };
}
