
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
  this.KMBM=0x0000;
  this.s17="touch";
  this.s18="hardware";
  this.s21="windows";
  this.s22="android";
  this.s23="ios";
  this.s24="macosx";
  this.s25="linux";
  this.s28="desktop";
  this.s29="tablet";
  this.s30="phone";
  this.s33="native";
  this.s34="web";
  this.s37="ie";
  this.s38="chrome";
  this.s39="firefox";
  this.s40="safari";
  this.s41="opera";
  this.s44="touch";
  this.s45="hardware";
  this.s48="platform-x";
  this.s51="WinDOWS";
  this.s52="ANDroid";
  this.s53="iOS";
  this.s54="macOSX";
  this.s55="LINUX";
  this.s57="touch";
  this.s58="hardware";
  this.s59="windows";
  this.s60="android";
  this.s61="ios";
  this.s62="macosx";
  this.s63="linux";
  this.s64="desktop";
  this.s65="tablet";
  this.s66="phone";
  this.s67="native";
  this.s68="web";
  this.s69="ie";
  this.s70="chrome";
  this.s71="firefox";
  this.s72="safari";
  this.s73="opera";
  this.s74="touch";
  this.s75="hardware";
  this.s76="platform-x";
  this.s77="WinDOWS";
  this.s78="ANDroid";
  this.s79="iOS";
  this.s80="macOSX";
  this.s81="LINUX";
  this.KVER="10.0.1104.0";
  this.gs=function(t,e) {
    return this.g_main(t,e);
  };
  this.g_main=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, 0x4000, 0x41)) {   // Line 7
      r=m=1;
      r=this.g_ui(t,e);
    }
    if(!m&&k.KIK(e)) {
      r=1;
      r=this.g_old_main(t,e);
    }
    return r;
  };
  this.g_ui=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
    if(k.KIFS(31,this.s17,t)) {   // Line 12
      m=1;
      k.KO(0,t,"touch");
    }
    else if(k.KIFS(31,this.s18,t)) {   // Line 13
      m=1;
      k.KO(0,t,"hardware");
    }
    if(m) {
    
      r=this.g_os(t,e);
    }
    if(!m) {
    
      k.KO(-1,t,"undefined");
      r=this.g_os(t,e);
    }
    return r;
  };
  this.g_os=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
    if(k.KIFS(31,this.s21,t)) {   // Line 19
      m=1;
      k.KO(0,t," windows");
    }
    else if(k.KIFS(31,this.s22,t)) {   // Line 20
      m=1;
      k.KO(0,t," android");
    }
    else if(k.KIFS(31,this.s23,t)) {   // Line 21
      m=1;
      k.KO(0,t," ios");
    }
    else if(k.KIFS(31,this.s24,t)) {   // Line 22
      m=1;
      k.KO(0,t," macosx");
    }
    else if(k.KIFS(31,this.s25,t)) {   // Line 23
      m=1;
      k.KO(0,t," linux");
    }
    if(m) {
    
      r=this.g_ff(t,e);
    }
    if(!m) {
    
      k.KO(-1,t," undefined");
      r=this.g_ff(t,e);
    }
    return r;
  };
  this.g_ff=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
    if(k.KIFS(31,this.s28,t)) {   // Line 29
      m=1;
      k.KO(0,t," desktop");
    }
    else if(k.KIFS(31,this.s29,t)) {   // Line 30
      m=1;
      k.KO(0,t," tablet");
    }
    else if(k.KIFS(31,this.s30,t)) {   // Line 31
      m=1;
      k.KO(0,t," phone");
    }
    if(m) {
    
      r=this.g_app(t,e);
    }
    if(!m) {
    
      k.KO(-1,t," undefined");
      r=this.g_app(t,e);
    }
    return r;
  };
  this.g_app=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
    if(k.KIFS(31,this.s33,t)) {   // Line 37
      m=1;
      k.KO(0,t," native");
    }
    else if(k.KIFS(31,this.s34,t)) {   // Line 38
      m=1;
      k.KO(0,t," web");
    }
    if(m) {
    
      r=this.g_browser(t,e);
    }
    if(!m) {
    
      k.KO(-1,t," undefined");
      r=this.g_browser(t,e);
    }
    return r;
  };
  this.g_browser=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
    if(k.KIFS(31,this.s37,t)) {   // Line 44
      m=1;
      k.KO(0,t," ie");
    }
    else if(k.KIFS(31,this.s38,t)) {   // Line 45
      m=1;
      k.KO(0,t," chrome");
    }
    else if(k.KIFS(31,this.s39,t)) {   // Line 46
      m=1;
      k.KO(0,t," firefox");
    }
    else if(k.KIFS(31,this.s40,t)) {   // Line 47
      m=1;
      k.KO(0,t," safari");
    }
    else if(k.KIFS(31,this.s41,t)) {   // Line 48
      m=1;
      k.KO(0,t," opera");
    }
    if(m) {
    
      r=this.g_inverted(t,e);
    }
    if(!m) {
    
      k.KO(-1,t," undefined");
      r=this.g_inverted(t,e);
    }
    return r;
  };
  this.g_inverted=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
    if(!k.KIFS(31,this.s44,t)) {   // Line 54
      m=1;
      k.KO(0,t," !touch");
    }
    else if(!k.KIFS(31,this.s45,t)) {   // Line 55
      m=1;
      k.KO(0,t," !hardware");
    }
    if(m) {
    
      r=this.g_unknown(t,e);
    }
    if(!m) {
    
      k.KO(-1,t," undefined");
      r=this.g_unknown(t,e);
    }
    return r;
  };
  this.g_unknown=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
    if(k.KIFS(31,this.s48,t)) {   // Line 61
      m=1;
      k.KO(0,t," platform-x");
    }
    if(m) {
    
      r=this.g_case_sensitive(t,e);
    }
    if(!m) {
    
      k.KO(-1,t," undefined");
      r=this.g_case_sensitive(t,e);
    }
    return r;
  };
  this.g_case_sensitive=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
    if(k.KIFS(31,this.s51,t)) {   // Line 67
      m=1;
      k.KO(0,t," windows");
    }
    else if(k.KIFS(31,this.s52,t)) {   // Line 68
      m=1;
      k.KO(0,t," android");
    }
    else if(k.KIFS(31,this.s53,t)) {   // Line 69
      m=1;
      k.KO(0,t," ios");
    }
    else if(k.KIFS(31,this.s54,t)) {   // Line 70
      m=1;
      k.KO(0,t," macosx");
    }
    else if(k.KIFS(31,this.s55,t)) {   // Line 71
      m=1;
      k.KO(0,t," linux");
    }
    if(!m) {
    
      k.KO(-1,t," undefined");
    }
    return r;
  };
  this.g_old_main=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, 0x4000, 0x45)&&k.KIFS(31,this.s77,t)) {   // Line 130
      r=m=1;
      k.KO(0,t," Windows");
    }
    else if(k.KKM(e, 0x4000, 0x45)&&k.KIFS(31,this.s78,t)) {   // Line 131
      r=m=1;
      k.KO(0,t," Android");
    }
    else if(k.KKM(e, 0x4000, 0x45)&&k.KIFS(31,this.s79,t)) {   // Line 132
      r=m=1;
      k.KO(0,t," iOS");
    }
    else if(k.KKM(e, 0x4000, 0x45)&&k.KIFS(31,this.s80,t)) {   // Line 133
      r=m=1;
      k.KO(0,t," macOS");
    }
    else if(k.KKM(e, 0x4000, 0x45)&&k.KIFS(31,this.s81,t)) {   // Line 134
      r=m=1;
      k.KO(0,t," Linux");
    }
    else if(k.KKM(e, 0x4000, 0x45)) {   // Line 135
      r=m=1;
      k.KO(0,t," [OS Undefined (case insensitive test)]");
    }
    else if(k.KKM(e, 0x4000, 0x49)&&k.KIFS(31,this.s64,t)) {   // Line 97
      r=m=1;
      k.KO(0,t," Desktop");
    }
    else if(k.KKM(e, 0x4000, 0x49)&&k.KIFS(31,this.s65,t)) {   // Line 98
      r=m=1;
      k.KO(0,t," Tablet");
    }
    else if(k.KKM(e, 0x4000, 0x49)&&k.KIFS(31,this.s66,t)) {   // Line 99
      r=m=1;
      k.KO(0,t," Phone");
    }
    else if(k.KKM(e, 0x4000, 0x49)) {   // Line 100
      r=m=1;
      k.KO(0,t," [FF Undefined]");
    }
    else if(k.KKM(e, 0x4000, 0x4F)&&k.KIFS(31,this.s59,t)) {   // Line 88
      r=m=1;
      k.KO(0,t," Windows");
    }
    else if(k.KKM(e, 0x4000, 0x4F)&&k.KIFS(31,this.s60,t)) {   // Line 89
      r=m=1;
      k.KO(0,t," Android");
    }
    else if(k.KKM(e, 0x4000, 0x4F)&&k.KIFS(31,this.s61,t)) {   // Line 90
      r=m=1;
      k.KO(0,t," iOS");
    }
    else if(k.KKM(e, 0x4000, 0x4F)&&k.KIFS(31,this.s62,t)) {   // Line 91
      r=m=1;
      k.KO(0,t," OSX");
    }
    else if(k.KKM(e, 0x4000, 0x4F)&&k.KIFS(31,this.s63,t)) {   // Line 92
      r=m=1;
      k.KO(0,t," Linux");
    }
    else if(k.KKM(e, 0x4000, 0x4F)) {   // Line 93
      r=m=1;
      k.KO(0,t," [OS Undefined]");
    }
    else if(k.KKM(e, 0x4000, 0x50)&&k.KIFS(31,this.s57,t)) {   // Line 82
      r=m=1;
      k.KO(0,t,"Touch");
    }
    else if(k.KKM(e, 0x4000, 0x50)&&k.KIFS(31,this.s58,t)) {   // Line 83
      r=m=1;
      k.KO(0,t,"Hardware");
    }
    else if(k.KKM(e, 0x4000, 0x50)) {   // Line 84
      r=m=1;
      k.KO(0,t,"[UI Undefined]");
    }
    else if(k.KKM(e, 0x4000, 0x52)&&k.KIFS(31,this.s76,t)) {   // Line 125
      r=m=1;
      k.KO(0,t," Platform-X");
    }
    else if(k.KKM(e, 0x4000, 0x52)) {   // Line 126
      r=m=1;
      k.KO(0,t," [Platform-X Undefined]");
    }
    else if(k.KKM(e, 0x4000, 0x54)&&!k.KIFS(31,this.s74,t)) {   // Line 119
      r=m=1;
      k.KO(0,t," !Touch");
    }
    else if(k.KKM(e, 0x4000, 0x54)&&!k.KIFS(31,this.s75,t)) {   // Line 120
      r=m=1;
      k.KO(0,t," !Hardware");
    }
    else if(k.KKM(e, 0x4000, 0x54)) {   // Line 121
      r=m=1;
      k.KO(0,t," [Inverted OS Undefined]");
    }
    else if(k.KKM(e, 0x4000, 0x55)&&k.KIFS(31,this.s67,t)) {   // Line 104
      r=m=1;
      k.KO(0,t," Native");
    }
    else if(k.KKM(e, 0x4000, 0x55)&&k.KIFS(31,this.s68,t)) {   // Line 105
      r=m=1;
      k.KO(0,t," Web");
    }
    else if(k.KKM(e, 0x4000, 0x55)) {   // Line 106
      r=m=1;
      k.KO(0,t," [Nativeness Undefined]");
    }
    else if(k.KKM(e, 0x4000, 0x59)&&k.KIFS(31,this.s69,t)) {   // Line 110
      r=m=1;
      k.KO(0,t," IE");
    }
    else if(k.KKM(e, 0x4000, 0x59)&&k.KIFS(31,this.s70,t)) {   // Line 111
      r=m=1;
      k.KO(0,t," Chrome");
    }
    else if(k.KKM(e, 0x4000, 0x59)&&k.KIFS(31,this.s71,t)) {   // Line 112
      r=m=1;
      k.KO(0,t," Firefox");
    }
    else if(k.KKM(e, 0x4000, 0x59)&&k.KIFS(31,this.s72,t)) {   // Line 113
      r=m=1;
      k.KO(0,t," Safari");
    }
    else if(k.KKM(e, 0x4000, 0x59)&&k.KIFS(31,this.s73,t)) {   // Line 114
      r=m=1;
      k.KO(0,t," Opera");
    }
    else if(k.KKM(e, 0x4000, 0x59)) {   // Line 115
      r=m=1;
      k.KO(0,t," [Browser Undefined]");
    }
    return r;
  };
}
