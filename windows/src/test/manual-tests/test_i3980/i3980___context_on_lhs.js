KeymanWeb.KR(new Keyboard_i3980___context_on_lhs());
function Keyboard_i3980___context_on_lhs()
{
  this.KI="Keyboard_i3980___context_on_lhs";
  this.KN="I3980 - Context() on LHS";
  this.KV=null;
  this.KH='';
  this.KM=0;
  this.s_b="bB";
  this.s_c="cC";
  this.s_d="dD";
  this.KVER="9.0.430.0";
  this.gs=function(t,e)
  {
    return this.g_main(t,e);
  };
  this.g_main=function(t,e)
  {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e,16384,65)&&k.KCM(2,t,"a",1)&&k.KCCM(1,2,t))
    {
      r=m=1;
      k.KO(2,t,"match(3)");
    }
    else if(k.KKM(e,16384,66)&&k.KA(0,k.KC(2,1,t),this.s_b)&&k.KCCM(1,2,t))
    {
      r=m=1;
      k.KO(2,t,"match(3)");
    }
    else if(k.KKM(e,16384,67)&&k.KCM(3,t,"c",1)&&k.KA(1,k.KC(2,1,t),this.s_c)&&k.KCCM(1,2,t))
    {
      r=m=1;
      k.KO(3,t,"match(3)");
    }
    else if(k.KKM(e,16384,68)&&k.KCM(5,t,"d",1)&&k.KA(1,k.KC(4,1,t),this.s_d)&&k.KCCM(3,5,t)&&k.KCCM(2,4,t)&&k.KCCM(1,3,t))
    {
      r=m=1;
      k.KO(5,t,"match(5)");
    }
    return r;
  };
}
