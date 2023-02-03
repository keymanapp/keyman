if(typeof keyman === 'undefined') {
  console.log('Keyboard requires KeymanWeb 10.0 or later');
  if(typeof tavultesoft !== 'undefined') tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");
} else {
KeymanWeb.KR(new Keyboard_test_deadkeys());
}
function Keyboard_test_deadkeys()
{
  var modCodes = keyman.osk.modifierCodes;
  var keyCodes = keyman.osk.keyCodes;

  this.KI="Keyboard_test_deadkeys";
  this.KN="Keyman Deadkey Stress-Tester";
  this.KMINVER="10.0";
  this.KV={F:' 1em "Arial"',K102:0};
  this.KDU=0;
  this.KLS={

  };
  this.KV.BK=(function(x){
    var
      empty=Array.apply(null, Array(65)).map(String.prototype.valueOf,""),
      result=[], v, i,
      modifiers=['default','shift','ctrl','shift-ctrl','alt','shift-alt','ctrl-alt','shift-ctrl-alt'];
    for(i=modifiers.length-1;i>=0;i--) {
      v = x[modifiers[i]];
      if(v || result.length > 0) {
        result=(v ? v : empty).slice().concat(result);
      }
    }
    return result;
  })(this.KLS);
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=modCodes.SHIFT /* 0x0010 */;
  this.s_deadnums=[{d:0},{d:1},{d:2},{d:3},{d:4},{d:5},{d:6},{d:7},{d:8},{d:9}];
  this.s_livenums="0123456789";
  this.s_liveQwerty="qwerty";
  this.s_deadQwerty=[{d:10},{d:11},{d:12},{d:13},{d:14},{d:15}];
  this.KVER="10.0.700.0";
  this.gs=function(t,e) {
    return this.g_main(t,e);
  };
  this.g_main=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_1 /* 0x31 */)&&k.KFCM(1,t,[{t:'a',a:this.s_liveQwerty}])) {   // Line 77
      r=m=1;
      k.KDC(1,t);
      k.KO(-1,t,"?");
      k.KIO(-1,this.s_deadQwerty,1,t);
      k.KIO(-1,this.s_deadQwerty,1,t);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_PERIOD /* 0xBE */)&&k.KFCM(7,t,['(','o',')',{t:'d',d:18},'(','o',')'])) {   // Line 51
      r=m=1;
      k.KDC(7,t);
      k.KO(-1,t,"dk(o)");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_PERIOD /* 0xBE */)&&k.KFCM(4,t,[{t:'d',d:18},'(','m',')'])) {   // Line 45
      r=m=1;
      k.KDC(4,t);
      k.KO(-1,t,"dk(m)");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_PERIOD /* 0xBE */)&&k.KFCM(4,t,['(','n',')',{t:'d',d:18}])) {   // Line 48
      r=m=1;
      k.KDC(4,t);
      k.KO(-1,t,"dk(n)");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_PERIOD /* 0xBE */)&&k.KFCM(3,t,['?',{t:'a',a:this.s_deadQwerty},{t:'c',c:2}])) {   // Line 78
      r=m=1;
      k.KDC(3,t);
      k.KO(-1,t,"(");
      k.KIO(-1,this.s_liveQwerty,2,t);
      k.KIO(-1,this.s_liveQwerty,2,t);
      k.KO(-1,t,")");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_PERIOD /* 0xBE */)&&k.KFCM(2,t,[{t:'d',d:16},{t:'d',d:17}])) {   // Line 40
      r=m=1;
      k.KDC(2,t);
      k.KO(-1,t,"(a)-(s)");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_PERIOD /* 0xBE */)&&k.KFCM(2,t,[{t:'d',d:17},{t:'d',d:16}])) {   // Line 41
      r=m=1;
      k.KDC(2,t);
      k.KO(-1,t,"(s)+(a)");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_PERIOD /* 0xBE */)&&k.KFCM(1,t,[{t:'a',a:this.s_deadnums}])) {   // Line 76
      r=m=1;
      k.KDC(1,t);
      k.KO(-1,t,"#");
      k.KIO(-1,this.s_livenums,1,t);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_0 /* 0x30 */)) {   // Line 64
      r=m=1;
      k.KDC(0,t);
      k.KDO(-1,t,0);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_1 /* 0x31 */)) {   // Line 65
      r=m=1;
      k.KDC(0,t);
      k.KDO(-1,t,1);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_2 /* 0x32 */)) {   // Line 66
      r=m=1;
      k.KDC(0,t);
      k.KDO(-1,t,2);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_3 /* 0x33 */)) {   // Line 67
      r=m=1;
      k.KDC(0,t);
      k.KDO(-1,t,3);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_4 /* 0x34 */)) {   // Line 68
      r=m=1;
      k.KDC(0,t);
      k.KDO(-1,t,4);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_5 /* 0x35 */)) {   // Line 69
      r=m=1;
      k.KDC(0,t);
      k.KDO(-1,t,5);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_6 /* 0x36 */)) {   // Line 70
      r=m=1;
      k.KDC(0,t);
      k.KDO(-1,t,6);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_7 /* 0x37 */)) {   // Line 71
      r=m=1;
      k.KDC(0,t);
      k.KDO(-1,t,7);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_8 /* 0x38 */)) {   // Line 72
      r=m=1;
      k.KDC(0,t);
      k.KDO(-1,t,8);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_9 /* 0x39 */)) {   // Line 73
      r=m=1;
      k.KDC(0,t);
      k.KDO(-1,t,9);
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_SLASH /* 0xBF */)) {   // Line 126
      r=m=1;
      k.KDC(0,t);
      k.KO(-1,t,"ab    > (a)(b)          tests basic deadkey\rb     > (b)             only matches if \"a\" not pressed first\rbb    > (b)+(b)         Missing deadkey does not trigger match.\rbab   > (b)+(a)+(b)     deadkey at end of context\rac    > x               tests basic deadkey\rade   > (a)(d)(e)       deadkey at start of context\rfade  > (f)-(a)-(d)-(e) deadkey in middle of context\ras.   > (a)-(s)         defined deadkey order (1/2)\rsa.   > (s)+(a)         defined deadkey order (2/2)\rm.    > dk(m)           deadkey output then letters\rn.    > dk(n)           letters output then deadkey\ro.    > dk(o)           letters, deadkey, letters\rpqr   > (p)(q)(r)       deadkey, unmatched, letters\rPQR   > (P)(Q)(R)       deadkey, deadkey, match, match\r12bb  > success         deadkey reordering (with the following)\r21bb  > success         \r21b   > dead2           \r34bb  > success         \r43bb  > success         \rABCDE > success         deadkey output before match(group) rule\r1.    > #1              deadkey matched by any\rq!    > ?               same as above, with extra deadkey output.\rq!.   > (qq)            deadkey matched by context() in context\r11    > (1)(1)          deadkey matched by index() within context.\r\r");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_A /* 0x41 */)) {   // Line 93
      r=m=1;
      k.KDC(0,t);
      k.KDO(-1,t,16);
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_B /* 0x42 */)&&k.KFCM(1,t,[{t:'d',d:16}])) {   // Line 94
      r=m=1;
      k.KDC(1,t);
      k.KO(-1,t,"ab");
      k.KDO(-1,t,21);
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_C /* 0x43 */)&&k.KFCM(1,t,[{t:'d',d:22}])) {   // Line 95
      r=m=1;
      k.KDC(1,t);
      k.KO(-1,t,"abc");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_D /* 0x44 */)&&k.KFCM(3,t,['a','b','c'])) {   // Line 96
      r=m=1;
      k.KDC(3,t);
      k.KO(-1,t,"$abc$d");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_E /* 0x45 */)&&k.KFCM(4,t,['a','b',{t:'d',d:23},'d'])) {   // Line 97
      r=m=1;
      k.KDC(4,t);
      k.KO(-1,t,"success");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_P /* 0x50 */)) {   // Line 58
      r=m=1;
      k.KDC(0,t);
      k.KDO(-1,t,20);
      k.KDO(-1,t,20);
      k.KO(-1,t,"(P)");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_Q /* 0x51 */)&&k.KFCM(4,t,[{t:'d',d:20},'(','P',')'])) {   // Line 59
      r=m=1;
      k.KDC(4,t);
      k.KO(-1,t,"(P)(Q)");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_R /* 0x52 */)&&k.KFCM(8,t,[{t:'d',d:20},{t:'d',d:20},'(','P',')','(','Q',')'])) {   // Line 61
      r=m=1;
      k.KDC(8,t);
      k.KO(-1,t,"ERROR");
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.VIRTUAL_KEY /* 0x4010 */, keyCodes.K_R /* 0x52 */)&&k.KFCM(7,t,[{t:'d',d:20},'(','P',')','(','Q',')'])) {   // Line 60
      r=m=1;
      k.KDC(7,t);
      k.KO(-1,t,"(P)(Q)(R)");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_A /* 0x41 */)) {   // Line 18
      r=m=1;
      k.KDC(0,t);
      k.KDO(-1,t,16);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_B /* 0x42 */)&&k.KFCM(6,t,[{t:'d',d:2},'d','e','a','d','1'])) {   // Line 84
      r=m=1;
      k.KDC(6,t);
      k.KO(-1,t,"ERROR");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_B /* 0x42 */)&&k.KFCM(6,t,[{t:'d',d:1},'d','e','a','d','2'])) {   // Line 85
      r=m=1;
      k.KDC(6,t);
      k.KO(-1,t,"success");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_B /* 0x42 */)&&k.KFCM(6,t,[{t:'d',d:4},'d','e','a','d','3'])) {   // Line 89
      r=m=1;
      k.KDC(6,t);
      k.KO(-1,t,"success");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_B /* 0x42 */)&&k.KFCM(6,t,[{t:'d',d:3},'d','e','a','d','4'])) {   // Line 90
      r=m=1;
      k.KDC(6,t);
      k.KO(-1,t,"ERROR");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_B /* 0x42 */)&&k.KFCM(4,t,['(','b',')',{t:'d',d:16}])) {   // Line 24
      r=m=1;
      k.KDC(4,t);
      k.KO(-1,t,"(b)+(a)+(b)");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_B /* 0x42 */)&&k.KFCM(3,t,['(','b',')'])) {   // Line 23
      r=m=1;
      k.KDC(3,t);
      k.KO(-1,t,"(b)+(b)");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_B /* 0x42 */)&&k.KFCM(1,t,[{t:'d',d:16}])) {   // Line 19
      r=m=1;
      k.KDC(1,t);
      k.KO(-1,t,"(a)(b)");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_B /* 0x42 */)&&k.KFCM(1,t,[{t:'d',d:2}])) {   // Line 82
      r=m=1;
      k.KDC(1,t);
      k.KO(-1,t,"dead2");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_B /* 0x42 */)&&k.KFCM(1,t,[{t:'d',d:1}])) {   // Line 83
      r=m=1;
      k.KDC(1,t);
      k.KO(-1,t,"dead1");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_B /* 0x42 */)&&k.KFCM(1,t,[{t:'d',d:4}])) {   // Line 87
      r=m=1;
      k.KDC(1,t);
      k.KO(-1,t,"dead4");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_B /* 0x42 */)&&k.KFCM(1,t,[{t:'d',d:3}])) {   // Line 88
      r=m=1;
      k.KDC(1,t);
      k.KO(-1,t,"dead3");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_B /* 0x42 */)) {   // Line 20
      r=m=1;
      k.KDC(0,t);
      k.KO(-1,t,"(b)");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_C /* 0x43 */)&&k.KFCM(1,t,[{t:'d',d:16}])) {   // Line 27
      r=m=1;
      k.KDC(1,t);
      k.KO(-1,t,"x");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_D /* 0x44 */)) {   // Line 30
      r=m=1;
      k.KDC(0,t);
      k.KO(-1,t,"d");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_E /* 0x45 */)&&k.KFCM(3,t,['f',{t:'d',d:16},'d'])) {   // Line 35
      r=m=1;
      k.KDC(3,t);
      k.KO(-1,t,"(f)-(a)-(d)-(e)");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_E /* 0x45 */)&&k.KFCM(2,t,[{t:'d',d:16},'d'])) {   // Line 34
      r=m=1;
      k.KDC(2,t);
      k.KO(-1,t,"(a)(d)(e)");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_F /* 0x46 */)) {   // Line 31
      r=m=1;
      k.KDC(0,t);
      k.KO(-1,t,"f");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_M /* 0x4D */)) {   // Line 44
      r=m=1;
      k.KDC(0,t);
      k.KDO(-1,t,18);
      k.KO(-1,t,"(m)");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_N /* 0x4E */)) {   // Line 47
      r=m=1;
      k.KDC(0,t);
      k.KO(-1,t,"(n)");
      k.KDO(-1,t,18);
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_O /* 0x4F */)) {   // Line 50
      r=m=1;
      k.KDC(0,t);
      k.KO(-1,t,"(o)");
      k.KDO(-1,t,18);
      k.KO(-1,t,"(o)");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_P /* 0x50 */)) {   // Line 54
      r=m=1;
      k.KDC(0,t);
      k.KDO(-1,t,19);
      k.KDO(-1,t,10);
      k.KO(-1,t,"(p)");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Q /* 0x51 */)&&k.KFCM(4,t,[{t:'d',d:10},'(','p',')'])) {   // Line 55
      r=m=1;
      k.KDC(4,t);
      k.KO(-1,t,"(p)(q)");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_R /* 0x52 */)&&k.KFCM(7,t,[{t:'d',d:19},'(','p',')','(','q',')'])) {   // Line 56
      r=m=1;
      k.KDC(7,t);
      k.KO(-1,t,"(p)(q)(r)");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_S /* 0x53 */)) {   // Line 38
      r=m=1;
      k.KDC(0,t);
      k.KDO(-1,t,17);
    }
    if(m) {

      k.KDC(-1,t);
      r=this.g_dead_reorder(t,e);
    }
    if(!m&&k.KIK(e)) {
      r=1;
      k.KDC(-1,t);
      r=this.g_qwerty_out(t,e);
    }
    return r;
  };
  this.g_dead_reorder=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
    if(k.KFCM(6,t,['$','a','b','c','$','d'])) {   // Line 134
      m=1;
      k.KDC(6,t);
      k.KO(-1,t,"ab");
      k.KDO(-1,t,23);
      k.KO(-1,t,"d");
    }
    else if(k.KFCM(3,t,['a','b',{t:'d',d:21}])) {   // Line 133
      m=1;
      k.KDC(3,t);
      k.KDO(-1,t,22);
    }
    else if(k.KFCM(2,t,[{t:'d',d:2},{t:'d',d:1}])) {   // Line 130
      m=1;
      k.KDC(2,t);
      k.KDO(-1,t,1);
      k.KDO(-1,t,2);
    }
    else if(k.KFCM(2,t,[{t:'d',d:3},{t:'d',d:4}])) {   // Line 131
      m=1;
      k.KDC(2,t);
      k.KDO(-1,t,4);
      k.KDO(-1,t,3);
    }
    else if(k.KFCM(2,t,[{t:'a',a:this.s_deadnums},{t:'i',i:this.s_deadnums,o:1}])) {   // Line 138
      m=1;
      k.KDC(2,t);
      k.KO(-1,t,"(");
      k.KIO(-1,this.s_livenums,1,t);
      k.KO(-1,t,")(");
      k.KIO(-1,this.s_livenums,1,t);
      k.KO(-1,t,")");
    }
    return r;
  };
  this.g_qwerty_out=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_E /* 0x45 */)) {   // Line 145
      r=m=1;
      k.KDC(0,t);
      k.KO(-1,t,"e");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Q /* 0x51 */)) {   // Line 143
      r=m=1;
      k.KDC(0,t);
      k.KO(-1,t,"q");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_R /* 0x52 */)) {   // Line 146
      r=m=1;
      k.KDC(0,t);
      k.KO(-1,t,"r");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_T /* 0x54 */)) {   // Line 147
      r=m=1;
      k.KDC(0,t);
      k.KO(-1,t,"t");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_W /* 0x57 */)) {   // Line 144
      r=m=1;
      k.KDC(0,t);
      k.KO(-1,t,"w");
    }
    else if(k.KKM(e, modCodes.VIRTUAL_KEY /* 0x4000 */, keyCodes.K_Y /* 0x59 */)) {   // Line 148
      r=m=1;
      k.KDC(0,t);
      k.KO(-1,t,"y");
    }
    return r;
  };
}
