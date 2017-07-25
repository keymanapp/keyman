KeymanWeb.KR(new Keyboard_i3438());
function Keyboard_i3438()
{
  this.KI="Keyboard_i3438";
  this.KN="Test I3438";
  this.KV=null;
  this.KH='';
  this.KM=0;
  this.KVKD="K_MYKJEY K_BOO";
  this.KVER="9.0.419.0";
  this.s8="K_MYKJEY K_BOO";
  this.gs=function(t,e)
  {
    return this.g_main(t,e);
  };
  this.g_main=function(t,e)
  {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e,16384,256))
    {
      r=m=1;
      k.KO(0,t,"ah");
    }
    else if(k.KKM(e,16384,257))
    {
      r=m=1;
      k.KO(0,t,"blah");
    }
    return r;
  };
}
