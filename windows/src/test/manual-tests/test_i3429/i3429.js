KeymanWeb.KR(new Keyboard_i3429());
function Keyboard_i3429()
{
  this.KI="Keyboard_i3429";
  this.KN="I3429 -  Add support for if, set, reset, save to KeymanWeb compiler";
  this.KV=null;
  this.KH='';
  this.KM=0;
  this.s_col=KeymanWeb.KLOAD(this.KI,"col","a");
  this.s9="a";
  this.s10="b";
  this.s11="b";
  this.s12="a";
  this.s13="z";
  this.KVER="9.0.419.0";
  this.gs=function(t,e)
  {
    return this.g_main(t,e);
  };
  this.g_main=function(t,e)
  {
    var k=KeymanWeb,r=0,m=0;
    if(k.KKM(e,16400,90)&&this.s_col!==this.s13)
    {
      r=m=1;
      k.KO(0,t,"boo!");
    }
    else if(k.KKM(e,16384,65))
    {
      r=m=1;
      this.s_col=this.s12;
      k.KO(-1,t," set to <a> ");
    }
    else if(k.KKM(e,16384,66))
    {
      r=m=1;
      this.s_col=this.s11;
      k.KO(-1,t," set to <b> ");
    }
    else if(k.KKM(e,16384,88))
    {
      r=m=1;
      k.KSAVE("_col",this.s_col);
      k.KO(-1,t," saved ");
    }
    else if(k.KKM(e,16384,89))
    {
      r=m=1;
      this.s_col=k.KLOAD(this.KI,"col","a");
      k.KO(-1,t," reset ");
    }
    else if(k.KKM(e,16384,90)&&this.s_col===this.s9)
    {
      r=m=1;
      k.KO(0,t," col is <a> ");
    }
    else if(k.KKM(e,16384,90)&&this.s_col===this.s10)
    {
      r=m=1;
      k.KO(0,t," col is <b> ");
    }
    else if(k.KKM(e,16384,90))
    {
      r=m=1;
      r=this.g_my(t,e);
    }
    if(m)
    {
      r=this.g_my(t,e);
    }
    if(!m&&k.KIK(e))
    {
      r=1;
      r=this.g_my(t,e);
    }
    return r;
  };
  this.g_my=function(t,e)
  {
    var k=KeymanWeb,r=1,m=0;
    if(k.KCM(1,t,"a",1))
    {
      m=1;
      k.KO(1,t,"b");
    }
    return r;
  };
}
