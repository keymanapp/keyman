KeymanWeb.KR(new Keyboard_test_deadkey_contextex_2_i3910());
function Keyboard_test_deadkey_contextex_2_i3910()
{
  this.KI="Keyboard_test_deadkey_contextex_2_i3910";
  this.KN="Test deadkey index I3910";
  this.KV=null;
  this.KH='';
  this.KM=0;
  this.s_alpha=" α";
  this.KVER="9.0.430.0";
  this.gs=function(t,e)
  {
    return this.g_main(t,e);
  };
  this.g_main=function(t,e)
  {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e,16384,32))
    {
      r=m=1;
      k.KO(0,t," ");
    }
    else if(k.KKM(e,16384,65))
    {
      r=m=1;
      k.KO(0,t,"α");
    }
    if(m)
    {
      r=this.g_dk(t,e);
    }
    return r;
  };
  this.g_dk=function(t,e)
  {
    var k=KeymanWeb,r=1,m=0;
    if(k.KCM(2,t,".",1)&&k.KA(1,k.KC(1,1,t),this.s_alpha))
    {
      m=1;
      k.KO(2,t,"<");k.KIO(-1,this.s_alpha,2,t);
      k.KO(-1,t,">");
    }
    return r;
  };
}
