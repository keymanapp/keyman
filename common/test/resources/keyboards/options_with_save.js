// Original source may be found within the repo at /common/tests/keyboards/baseline/k_023___options_with_save.kmn
if(typeof keyman === 'undefined') {
  console.log('Keyboard requires KeymanWeb 10.0 or later');

  if(typeof tavultesoft !== 'undefined')
    tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");
} else {
  KeymanWeb.KR(new Keyboard_options_with_save());
}

function Keyboard_options_with_save() {
  this.KI="Keyboard_options_with_save";
  this.KN="";
  this.KMINVER="10.0";
  this.KV=null;
  this.KDU=0;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KMBM=0x0000;
  this.s4=KeymanWeb.KLOAD(this.KI,"foo","0");
  this.s8="1";
  this.s9="0";
  this.s10="1";
  this.s11="0";
  this.KVER="13.0.9999.0";

  this.gs=function(t,e) {
    return this.g0(t,e);
  };

  this.g0=function(t,e) {
    var k=KeymanWeb,r=0,m=0;

    if(k.KKM(e,16384,48)) {
      if(1){
        r=m=1;
        k.KDC(0,t);
        this.s4=this.s11;
      }
    } else if(k.KKM(e,16384,49)) {
      if(1){
        r=m=1;
        k.KDC(0,t);
        this.s4=this.s10;
      }
    } else if(k.KKM(e,16384,50)) {
      if(1){
        r=m=1;
        k.KDC(0,t);
        k.KSAVE("foo",this.s4);
      }
    } else if(k.KKM(e,16384,65)) {
      if(this.s4===this.s8){
        r=m=1;
        k.KDC(0,t);
        k.KO(-1,t,"foo.");
      } else if(this.s4===this.s9){
        r=m=1;k.KDC(0,t);k.KO(-1,t,"no foo.");
      }
    }
    return r;
  };
}