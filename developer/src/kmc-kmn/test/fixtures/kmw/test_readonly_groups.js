if(typeof keyman === 'undefined') {
  console.log('Keyboard requires KeymanWeb 10.0 or later');
  if(typeof tavultesoft !== 'undefined') tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");
} else {
KeymanWeb.KR(new Keyboard_test_readonly_groups());
}
function Keyboard_test_readonly_groups()
{
  var modCodes = keyman.osk.modifierCodes;
  var keyCodes = keyman.osk.keyCodes;

  this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;
  this.KI="Keyboard_test_readonly_groups";
  this.KN="test_readonly_groups";
  this.KMINVER="10.0";
  this.KV=null;
  this.KDU=0;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=modCodes.SHIFT | modCodes.CAPS | modCodes.NO_CAPS /* 0x0310 */;
  this.s_key_6=['','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',''];
  this.s_out_7="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  this.s_caps_8="ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  this.s_digit_9="0123456789";
  this.s_sentencePunctuation_13=".?!";
  this.s19="";
  this.s20="numeric";
  this.s21="caps";
  this.s22="";
  this.s23="shift";
  this.s24="shift";
  this.s25="shift";
  this.s26="default";
  this.KVS=[];
  this.gs=function(t,e) {
    return this.g_main_3(t,e);
  };
  this.gn=function(t,e) {
    return this.g_NewContext_0(t,e);
  };
  this.gpk=function(t,e) {
    return this.g_PostKeystroke_1(t,e);
  };
  this.gs=function(t,e) {
    return this.g_main_3(t,e);
  };
  this.g_NewContext_0=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
    if(!m) {
    
      r=this.g_detectStartOfSentence_2(t,e);
      m=2;
    }
    return r;
  };
  this.g_PostKeystroke_1=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
      if(k.KFCM(1,t,[{t:'a',a:this.s_digit_9}])&&k.KIFS(42,this.s19,t)&&k.KIFS(33,this.s20,t)){
        m=1;   // Line 35
      }
      else if(k.KIFS(33,this.s21,t)){
        m=1;   // Line 38
      }
      else if(k.KIFS(42,this.s22,t)){
        m=1;   // Line 42
        r=this.g_detectStartOfSentence_2(t,e);
        m=2;
      }
    return r;
  };
  this.g_detectStartOfSentence_2=function(t,e) {
    var k=KeymanWeb,r=1,m=0;
      if(k.KFCM(3,t,[{t:'a',a:this.s_sentencePunctuation_13},' ',' '])){
        m=1;   // Line 58
        k.KSETS(33,this.s25,t);
      }
      else if(k.KFCM(2,t,[{t:'a',a:this.s_sentencePunctuation_13},' '])){
        m=1;   // Line 57
        k.KSETS(33,this.s24,t);
      }
      else if(k.KFCM(1,t,[{t:'n'}])){
        m=1;   // Line 53
        k.KSETS(33,this.s23,t);
      }
    if(!m) {
    
      k.KSETS(33,this.s26,t);
    }
    return r;
  };
  this.g_main_3=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_A /* 0x41 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"A");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_A /* 0x41 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"A");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_B /* 0x42 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"B");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_B /* 0x42 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"B");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_C /* 0x43 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"C");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_C /* 0x43 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"C");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_D /* 0x44 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"D");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_D /* 0x44 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"D");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_E /* 0x45 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"E");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_E /* 0x45 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"E");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_F /* 0x46 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"F");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_F /* 0x46 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"F");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_G /* 0x47 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"G");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_G /* 0x47 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"G");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_H /* 0x48 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"H");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_H /* 0x48 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"H");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_I /* 0x49 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"I");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_I /* 0x49 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"I");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_J /* 0x4A */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"J");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_J /* 0x4A */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"J");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_K /* 0x4B */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"K");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_K /* 0x4B */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"K");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_L /* 0x4C */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"L");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_L /* 0x4C */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"L");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_M /* 0x4D */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"M");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_M /* 0x4D */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"M");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_N /* 0x4E */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"N");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_N /* 0x4E */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"N");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_O /* 0x4F */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"O");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_O /* 0x4F */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"O");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_P /* 0x50 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"P");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_P /* 0x50 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"P");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_Q /* 0x51 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"Q");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_Q /* 0x51 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"Q");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_R /* 0x52 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"R");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_R /* 0x52 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"R");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_S /* 0x53 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"S");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_S /* 0x53 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"S");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_T /* 0x54 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"T");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_T /* 0x54 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"T");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_U /* 0x55 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"U");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_U /* 0x55 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"U");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_V /* 0x56 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"V");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_V /* 0x56 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"V");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_W /* 0x57 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"W");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_W /* 0x57 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"W");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_X /* 0x58 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"X");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_X /* 0x58 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"X");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_Y /* 0x59 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"Y");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_Y /* 0x59 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"Y");
      }
    }
    else if(k.KKM(e, modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4100 */, keyCodes.K_Z /* 0x5A */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"Z");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4210 */, keyCodes.K_Z /* 0x5A */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"Z");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_A /* 0x41 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"a");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_A /* 0x41 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"a");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_B /* 0x42 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"b");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_B /* 0x42 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"b");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_C /* 0x43 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"c");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_C /* 0x43 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"c");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_D /* 0x44 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"d");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_D /* 0x44 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"d");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_E /* 0x45 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"e");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_E /* 0x45 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"e");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_F /* 0x46 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"f");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_F /* 0x46 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"f");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_G /* 0x47 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"g");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_G /* 0x47 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"g");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_H /* 0x48 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"h");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_H /* 0x48 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"h");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_I /* 0x49 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"i");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_I /* 0x49 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"i");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_J /* 0x4A */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"j");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_J /* 0x4A */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"j");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_K /* 0x4B */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"k");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_K /* 0x4B */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"k");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_L /* 0x4C */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"l");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_L /* 0x4C */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"l");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_M /* 0x4D */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"m");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_M /* 0x4D */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"m");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_N /* 0x4E */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"n");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_N /* 0x4E */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"n");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_O /* 0x4F */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"o");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_O /* 0x4F */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"o");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_P /* 0x50 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"p");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_P /* 0x50 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"p");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_Q /* 0x51 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"q");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_Q /* 0x51 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"q");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_R /* 0x52 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"r");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_R /* 0x52 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"r");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_S /* 0x53 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"s");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_S /* 0x53 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"s");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_T /* 0x54 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"t");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_T /* 0x54 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"t");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_U /* 0x55 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"u");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_U /* 0x55 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"u");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_V /* 0x56 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"v");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_V /* 0x56 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"v");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_W /* 0x57 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"w");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_W /* 0x57 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"w");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_X /* 0x58 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"x");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_X /* 0x58 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"x");
      }
    }
      if(m) {}
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_Y /* 0x59 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"y");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_Y /* 0x59 */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"y");
      }
    }
    else if(k.KKM(e, modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */, keyCodes.K_Z /* 0x5A */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"z");
      }
    }
    else if(k.KKM(e, modCodes.NO_CAPS | modCodes.VIRTUAL_KEY /* 0x4200 */, keyCodes.K_Z /* 0x5A */)) {
      if(1){
        r=m=1;   // Line 67
        k.KDC(0,t);
        k.KO(-1,t,"z");
      }
    }
    return r;
  };
}
