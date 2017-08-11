program Mapping2;

{$APPTYPE CONSOLE}

uses
  JwaWindows,
  jwsclConstants,
  jwsclTypes,
  JwsclMapping;

const
  MYREAD_RIGHT = $1;    //specific bit 1
  MYWRITE_RIGHT = $2;   //specific bit 2
  MYEXECUTE_RIGHT = $4; //specific bit 3

var
  MyMapping: array[1..3] of TJwRightsMapping =
    (
    (Right: MYREAD_RIGHT;
      Name: 'MYREAD_RIGHT'; Flags: 0),
    (Right: MYWRITE_RIGHT;
      Name: 'MYWRITE_RIGHT'; Flags: 0),
    (Right: MYEXECUTE_RIGHT;
      Name: 'MYEXECUTE_RIGHT'; Flags: 0)
    );

  MyGenericMapping: TGenericMapping =
    (GenericRead: MYREAD_RIGHT;
    GenericWrite: MYWRITE_RIGHT;
    GenericExecute: MYEXECUTE_RIGHT;
    GenericAll: MYREAD_RIGHT or MYWRITE_RIGHT or MYEXECUTE_RIGHT;
    );

var GM : TGenericMapping;
    Text : String;
    Rights : Cardinal;
begin
  //returns a TGenericMapping structure
  GM := TJwSecurityUserMapping.GetMapping(MyGenericMapping);

  //returns specific access rights from a generic right
  Rights := TJwSecurityUserMapping.GenericMap(MyGenericMapping, GENERIC_ALL);
  //convert rights to a string
  Text := TJwSecurityUserMapping.MapAccessMaskToString(Rights, MyMapping);
  //Text = 'MYREAD_RIGHT or MYWRITE_RIGHT or MYEXECUTE_RIGHT'

end.
