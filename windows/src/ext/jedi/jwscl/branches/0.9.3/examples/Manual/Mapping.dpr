program Mapping;

{$APPTYPE CONSOLE}

uses
  JwaWindows,
  JwsclMapping;

var GM : TGenericMapping;
    Text : String;
    Rights : Cardinal;
begin
  //returns a TGenericMapping structure
  GM := TJwSecurityFileMapping.GetMapping;

  //returns specific access rights from a generic right
  Rights := TJwSecurityFileMapping.Map(GENERIC_READ);
  //convert rights to a string
  Text := TJwSecurityFileMapping.MapAccessMaskToString(Rights);
  //Text = 'STANDARD_RIGHTS_READ or Dateilesezugriff or
  //  Dateiattributlesezugriff or FILE_READ_EA or SYNCHRONIZE'

  //get access mask bit explanation
  Text := TJwSecurityFileMapping.GetBitMappingString(24);
  //Text = 'Bit 24 [SACL]'
end.
