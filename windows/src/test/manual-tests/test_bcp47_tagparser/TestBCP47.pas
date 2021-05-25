// Adapted from https://github.com/gagle/node-bcp47 [MIT]
unit TestBCP47;

interface

uses
  BCP47Tag,
  BCP47SuppressScriptRegistry;

procedure Run;

implementation

procedure Grandfathered; forward;

procedure Run;
begin
  // Empty

  with TBCP47Tag.Create('') do
  try
    Assert(Language = '');
    Assert(Tag = '');
  finally
    Free;
  end;

  // Invalid

  with TBCP47Tag.Create('blahblahblah') do
  try
    Assert(Language = '');
    Assert(Tag = '');
  finally
    Free;
  end;

  // Ordinary lang + region

  with TBCP47Tag.Create('en-US') do
  try
    Assert(Language = 'en');
    Assert(ExtLang = '');
    Assert(Region = 'US');
    Assert(Tag = 'en-US');
  finally
    Free;
  end;

  // Language

  with TBCP47Tag.Create('aa') do
  try
    Assert(Language = 'aa');
    Assert(ExtLang = '');
    Assert(Tag = 'aa');
  finally
    Free;
  end;

  with TBCP47Tag.Create('aaa') do
  try
    Assert(Language = 'aaa');
    Assert(ExtLang = '');
    Assert(Tag = 'aaa');
  finally
    Free;
  end;

  with TBCP47Tag.Create('aaaa') do
  try
    Assert(Language = 'aaaa');
    Assert(ExtLang = '');
    Assert(Tag = 'aaaa');
  finally
    Free;
  end;

  with TBCP47Tag.Create('aaaaa') do
  try
    Assert(Language = 'aaaaa');
    Assert(ExtLang = '');
    Assert(Tag = 'aaaaa');
  finally
    Free;
  end;

  with TBCP47Tag.Create('aaaaaa') do
  try
    Assert(Language = 'aaaaaa');
    Assert(ExtLang = '');
    Assert(Tag = 'aaaaaa');
  finally
    Free;
  end;

  with TBCP47Tag.Create('aaaaaaa') do
  try
    Assert(Language = 'aaaaaaa');
    Assert(ExtLang = '');
    Assert(Tag = 'aaaaaaa');
  finally
    Free;
  end;

  with TBCP47Tag.Create('aaaaaaaa') do
  try
    Assert(Language = 'aaaaaaaa');
    Assert(ExtLang = '');
    Assert(Tag = 'aaaaaaaa');
  finally
    Free;
  end;

  with TBCP47Tag.Create('aa-bbb') do
  try
    Assert(Language = 'aa');
    Assert(ExtLang = 'bbb');
    Assert(Tag = 'aa-bbb');
  finally
    Free;
  end;

  with TBCP47Tag.Create('aa-bbb-ccc') do
  try
    Assert(Language = 'aa');
    Assert(ExtLang = 'bbb-ccc');
    Assert(Tag = 'aa-bbb-ccc');
  finally
    Free;
  end;

  with TBCP47Tag.Create('aa-bbb-ccc-ddd') do
  try
    Assert(Language = 'aa');
    Assert(ExtLang = 'bbb-ccc-ddd');
    Assert(Tag = 'aa-bbb-ccc-ddd');
  finally
    Free;
  end;

  // Script

  with TBCP47Tag.Create('aa-bbbb') do
  try
    Assert(Language = 'aa');
    Assert(Script = 'bbbb');
    Assert(Tag = 'aa-bbbb');
  finally
    Free;
  end;

  // Region

  with TBCP47Tag.Create('aa-bb') do
  try
    Assert(Language = 'aa');
    Assert(Region = 'bb');
    Assert(Tag = 'aa-bb');
  finally
    Free;
  end;

  with TBCP47Tag.Create('aa-111') do
  try
    Assert(Language = 'aa');
    Assert(Region = '111');
    Assert(Tag = 'aa-111');
  finally
    Free;
  end;

  with TBCP47Tag.Create('aa-bbbb-cc') do
  try
    Assert(Language = 'aa');
    Assert(Script = 'bbbb');
    Assert(Region = 'cc');
    Assert(Tag = 'aa-bbbb-cc');
  finally
    Free;
  end;

  // Variant

  with TBCP47Tag.Create('aa-b1b1b') do
  try
    Assert(Language = 'aa');
    Assert(Variant = 'b1b1b');
    Assert(Tag = 'aa-b1b1b');
  finally
    Free;
  end;

  with TBCP47Tag.Create('aa-b1b1b-6a8b-cccccc') do
  try
    Assert(Language = 'aa');
    Assert(Variant = 'b1b1b-6a8b-cccccc');
    Assert(Tag = 'aa-b1b1b-6a8b-cccccc');
  finally
    Free;
  end;

  with TBCP47Tag.Create('aa-bbb-ccc-1111-ccccc-b1b1b') do
  try
    Assert(Language = 'aa');
    Assert(ExtLang = 'bbb-ccc');
    Assert(Variant = '1111-ccccc-b1b1b');
    Assert(Tag = 'aa-bbb-ccc-1111-ccccc-b1b1b');
  finally
    Free;
  end;

  // Extension

  with TBCP47Tag.Create('aa-7-123abc-abc-a-12') do
  try
    Assert(Language = 'aa');
    Assert(Extension = '7-123abc-abc-a-12');
    Assert(Tag = 'aa-7-123abc-abc-a-12');
  finally
    Free;
  end;

  // Lang Tag Private use

  with TBCP47Tag.Create('aa-x-1234ab-d') do
  try
    Assert(Language = 'aa');
    Assert(LangTag_PrivateUse = '1234ab-d');
    Assert(Tag = 'aa-x-1234ab-d');
  finally
    Free;
  end;

  // Everything

  with TBCP47Tag.Create('aaa-bbb-ccc-ddd-abcd-123-abc123-0abc-b-01-' +
          'abc123-x-01ab-abc12') do
  try
    Assert(Language = 'aaa');
    Assert(ExtLang = 'bbb-ccc-ddd');
    Assert(Script = 'abcd');
    Assert(Region = '123');
    Assert(Variant = 'abc123-0abc');
    Assert(Extension = 'b-01-abc123');
    Assert(LangTag_PrivateUse = '01ab-abc12');
    Assert(Tag = 'aaa-bbb-ccc-ddd-abcd-123-abc123-0abc-b-01-' +
          'abc123-x-01ab-abc12');
  finally
    Free;
  end;

  // Private use

  with TBCP47Tag.Create('x-111-aaaaa-BBB') do
  try
    Assert(Language = '');
    Assert(PrivateUse = '111-aaaaa-BBB');
    Assert(Tag = 'x-111-aaaaa-BBB');
  finally
    Free;
  end;

  with TBCP47Tag.Create('x-a') do
  try
    Assert(Language = '');
    Assert(PrivateUse = 'a');
    Assert(Tag = 'x-a');
  finally
    Free;
  end;

  with TBCP47Tag.Create('x-1-2-a-b') do
  try
    Assert(Language = '');
    Assert(PrivateUse = '1-2-a-b');
    Assert(Tag = 'x-1-2-a-b');
  finally
    Free;
  end;

  // Grandfathered
  Grandfathered;
end;

procedure Grandfathered;
const
  irregular: array[0..16] of string = ('en-GB-oed', 'i-ami', 'i-bnn', 'i-default', 'i-enochian',
          'i-hak', 'i-klingon', 'i-lux', 'i-mingo', 'i-navajo', 'i-pwn',
          'i-tao', 'i-tay', 'i-tsu', 'sgn-BE-FR', 'sgn-BE-NL', 'sgn-CH-DE');
  regular: array[0..8] of string = ('art-lojban', 'cel-gaulish', 'no-bok', 'no-nyn', 'zh-guoyu',
          'zh-hakka', 'zh-min', 'zh-min-nan', 'zh-xiang');
var
  I: Integer;
begin
  for I := Low(irregular) to High(irregular) do
    with TBCP47Tag.Create(irregular[i]) do
    try
      Assert(Language = '');
      Assert(Grandfathered = irregular[i]);
      Assert(Tag = irregular[i]);
    finally
      Free;
    end;

  for I := Low(regular) to High(regular) do
    with TBCP47Tag.Create(regular[i]) do
    try
      Assert(Language = '');
      Assert(Grandfathered = regular[i]);
      Assert(Tag = regular[i]);
    finally
      Free;
    end;
end;

end.
