unit BCP47SuppressScriptRegistry;

interface

// File-Date: 2016-02-10
// Extracted from http://www.iana.org/assignments/language-subtag-registry/language-subtag-registry

const
  SuppressScriptSubtagRegistry: string =
    'ab=Cyrl'#13#10+
    'af=Latn'#13#10+
    'am=Ethi'#13#10+
    'ar=Arab'#13#10+
    'as=Beng'#13#10+
    'ay=Latn'#13#10+
    'be=Cyrl'#13#10+
    'bg=Cyrl'#13#10+
    'bn=Beng'#13#10+
    'bs=Latn'#13#10+
    'ca=Latn'#13#10+
    'ch=Latn'#13#10+
    'cs=Latn'#13#10+
    'cy=Latn'#13#10+
    'da=Latn'#13#10+
    'de=Latn'#13#10+
    'dv=Thaa'#13#10+
    'dz=Tibt'#13#10+
    'el=Grek'#13#10+
    'en=Latn'#13#10+
    'eo=Latn'#13#10+
    'es=Latn'#13#10+
    'et=Latn'#13#10+
    'eu=Latn'#13#10+
    'fa=Arab'#13#10+
    'fi=Latn'#13#10+
    'fj=Latn'#13#10+
    'fo=Latn'#13#10+
    'fr=Latn'#13#10+
    'fy=Latn'#13#10+
    'ga=Latn'#13#10+
    'gl=Latn'#13#10+
    'gn=Latn'#13#10+
    'gu=Gujr'#13#10+
    'gv=Latn'#13#10+
    'he=Hebr'#13#10+
    'hi=Deva'#13#10+
    'hr=Latn'#13#10+
    'ht=Latn'#13#10+
    'hu=Latn'#13#10+
    'hy=Armn'#13#10+
    'id=Latn'#13#10+
    'in=Latn'#13#10+
    'is=Latn'#13#10+
    'it=Latn'#13#10+
    'iw=Hebr'#13#10+
    'ja=Jpan'#13#10+
    'ka=Geor'#13#10+
    'kk=Cyrl'#13#10+
    'kl=Latn'#13#10+
    'km=Khmr'#13#10+
    'kn=Knda'#13#10+
    'ko=Kore'#13#10+
    'la=Latn'#13#10+
    'lb=Latn'#13#10+
    'ln=Latn'#13#10+
    'lo=Laoo'#13#10+
    'lt=Latn'#13#10+
    'lv=Latn'#13#10+
    'mg=Latn'#13#10+
    'mh=Latn'#13#10+
    'mk=Cyrl'#13#10+
    'ml=Mlym'#13#10+
    'mo=Latn'#13#10+
    'mr=Deva'#13#10+
    'ms=Latn'#13#10+
    'mt=Latn'#13#10+
    'my=Mymr'#13#10+
    'na=Latn'#13#10+
    'nb=Latn'#13#10+
    'nd=Latn'#13#10+
    'ne=Deva'#13#10+
    'nl=Latn'#13#10+
    'nn=Latn'#13#10+
    'no=Latn'#13#10+
    'nr=Latn'#13#10+
    'ny=Latn'#13#10+
    'om=Latn'#13#10+
    'or=Orya'#13#10+
    'pa=Guru'#13#10+
    'pl=Latn'#13#10+
    'ps=Arab'#13#10+
    'pt=Latn'#13#10+
    'qu=Latn'#13#10+
    'rm=Latn'#13#10+
    'rn=Latn'#13#10+
    'ro=Latn'#13#10+
    'ru=Cyrl'#13#10+
    'rw=Latn'#13#10+
    'sg=Latn'#13#10+
    'si=Sinh'#13#10+
    'sk=Latn'#13#10+
    'sl=Latn'#13#10+
    'sm=Latn'#13#10+
    'so=Latn'#13#10+
    'sq=Latn'#13#10+
    'ss=Latn'#13#10+
    'st=Latn'#13#10+
    'sv=Latn'#13#10+
    'sw=Latn'#13#10+
    'ta=Taml'#13#10+
    'te=Telu'#13#10+
    'th=Thai'#13#10+
    'ti=Ethi'#13#10+
    'tl=Latn'#13#10+
    'tn=Latn'#13#10+
    'to=Latn'#13#10+
    'tr=Latn'#13#10+
    'ts=Latn'#13#10+
    'uk=Cyrl'#13#10+
    'ur=Arab'#13#10+
    've=Latn'#13#10+
    'vi=Latn'#13#10+
    'xh=Latn'#13#10+
    'yi=Hebr'#13#10+
    'zu=Latn'#13#10+
    'dsb=Latn'#13#10+
    'frr=Latn'#13#10+
    'frs=Latn'#13#10+
    'gsw=Latn'#13#10+
    'hsb=Latn'#13#10+
    'kok=Deva'#13#10+
    'mai=Deva'#13#10+
    'men=Latn'#13#10+
    'nds=Latn'#13#10+
    'niu=Latn'#13#10+
    'nqo=Nkoo'#13#10+
    'nso=Latn'#13#10+
    'tem=Latn'#13#10+
    'tkl=Latn'#13#10+
    'tmh=Latn'#13#10+
    'tpi=Latn'#13#10+
    'tvl=Latn'#13#10+
    'zbl=Blis';

implementation

(* code to build this from web
var
  n: Integer;
  s: string;
  FLangCode: string;
begin
  memo1.Lines.LoadFromFile('c:\temp\subtag-registry');
  n := 0;
  FLangCode := '';
  while n < memo1.Lines.Count-1 do
  begin
    s := Trim(memo1.Lines[n]);
    if s = '%%' then
      FLangCode := '';
    if Copy(s,1,Length('Subtag:')) = 'Subtag:' then
      FlangCode := Trim(Copy(s,Length('Subtag:')+1,MaxInt));
    if Copy(s,1,Length('Suppress-Script:')) = 'Suppress-Script:' then
      memo2.Lines.Add(FLangCode+'='+ Trim(Copy(s,Length('Suppress-Script:')+1,MaxInt)));
    Inc(n);
  end;
end;
*)

end.
