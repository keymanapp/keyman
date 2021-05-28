KeymanWeb.KR(new Keyboard__031___delete_diacritics());
function Keyboard__031___delete_diacritics()
{
  this.KI="Keyboard__031___delete_diacritics";
  this.KN="031 - delete diacritics";
  this.KV=null;
  this.KH='';
  this.KM=0;
  this.KBVER="1.0";
  this.KVER="9.0.518.0";
  this.gs=function(t,e) {
    return this.g_main(t,e);
  };
  this.g_main=function(t,e) {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e,16384,222)) {   // Line 17
      r=m=1;
      k.KO(0,t,"́");
    }
    else if(k.KKM(e,16384,88)&&k.KCM(1,t,"́",1)) {   // Line 18
      r=m=1;
      k.KO(1,t,"x̊");
    }
    return r;
  };
}
