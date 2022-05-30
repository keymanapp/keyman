unit UserProfileImage;

interface
uses
  Classes,
  SysUtils,
  ActiveX,
  JwaWindows,
  Wabiab,
  WabDefs,
  wabtags,
  comobj,
  graphics,
  JwsclToken,
  JwsclUtils,
  WabApi;

type
  TOnGetImage = procedure () of object;

type
  TImageRetrievingThread = class(TJwThread)
  protected
    OnGetImage : TOnGetImage;
  public
    procedure Execute; override;
  end;

procedure RetrieveProfileImage(const OnGetImage : TOnGetImage);

var ImageStream : TMemoryStream;
    ImageType : String;

implementation


procedure RetrieveProfileImage(const OnGetImage : TOnGetImage);
var F : TImageRetrievingThread;
begin
  F := TImageRetrievingThread.Create(true, 'ImageRetrievingThread');
  F.OnGetImage := OnGetImage;
  F.FreeOnTerminate := true;
  F.Resume;
end;

{ TImageRetrievingThread }

procedure FreeSRowSet(WabObject: IWabObject; var P: PSRowSet);
var
  I: Integer;
begin
  for I := 0 to P^.cRows - 1 do
    OleCheck(WabObject.FreeBuffer(P^.aRow[I].lpProps));
  OleCheck(WabObject.FreeBuffer(P));
  P := nil;
end;

procedure TImageRetrievingThread.Execute;
var
  WP: TWabParam;

  EntryID: PEntryID;
  EntryIDSize : ULONG;
  ObjType: ULONG;

  Container: IABContainer;
  AddrBook: IAddrBook;
  WabObject: IWabObject;

  Table: IMAPITable;
  TableRow: PSRowSet;

  MailUser: IMailUser;

  DetailsCount : DWORD;
  Details: PSPropsArray;

  PropType,
  PropID : DWORD;

  i : Integer;

  MessageSent : Boolean;
  Token : TJwSecurityToken;

  H : HRESULT;

  imp : TWabImportParam;
begin
  CoInitialize(nil);
  inherited;
  if not WabApiLoaded then
    exit;

 { Token := TJwSecurityToken.CreateLogonUser('TestBenutzer','','',LOGON32_LOGON_INTERACTIVE,LOGON32_PROVIDER_DEFAULT);
  Token.ImpersonateLoggedOnUser;}

  MessageSent := false;
  FreeAndNil(ImageStream);
  ImageType := '';

  ZeroMemory(@WP, Sizeof(WP));
  WP.cbSize := Sizeof(WP);
  //WP.szFileName := 'TestBenutzer.contact';
  //'C:\Users\UserAsAdmin\Contacts\TestBenutzer.contact';
  WP.ulFlags := 0;
  WP.hwnd := 0;

  OleCheck(WabOpen(AddrBook, WabObject, @WP, 0));

  MailUser := nil;
  

//      'TestBenutzer', MailUser));

 { OleCheck(WabObject.LDAPUrl(AddrBook, GetActiveWindow,
        0,//WABOBJECT_LDAPURL_RETURN_MAILUSER,
        PCHAR('ldap://CN=Chris'),
        MailUser));    }

  OleCheck(AddrBook.GetPAB(EntryIDSize, EntryID));
 {
  ZeroMemory(@imp, sizeof(imp));
  imp.cbSize := sizeof(imp);
  imp.lpAdrBook := AddrBook;
  imp.lpszFileName := PCHAR('C:\Users\Christian\Contacts\TestBenutzer.contact');
  H := WabObject.Import(@imp);
  }

  {H := (WabObject.VCardRetrieve(AddrBook, WAB_VCARD_FILE,
      PCHAR('C:\Users\Christian\Contacts\TestBenutzer.contact'),
      //PCHAR('C:\Users\UserAsAdmin\Contacts\TestBenutzer.contact'),
      //PCHAR('Contacts'),
      MailUser));}
  //OleCheck(H);
                                                        istream
  OleCheck(AddrBook.OpenEntry(EntryIDSize, EntryID, nil, 0,
    ObjType, IUnknown(Container)));

  OleCheck(WabObject.FreeBuffer(EntryID));

  OleCheck(Container.GetContentsTable(0, Table));
  OleCheck(Table.SeekRow(BOOKMARK_BEGINNING, 0, nil));

  repeat
    OleCheck(Table.QueryRows(1,0,TableRow));

    if TableRow.cRows > 0 then
    begin
      //if (ULONG(TableRow.aRow[0].lpProps[4].Value.l) in [MAPI_MAILUSER, MAPI_DISTLIST]) then
      begin
        EntryID := TableRow.aRow[0].lpProps[3].Value.bin.lpb;
        EntryIDSize := TableRow.aRow[0].lpProps[3].Value.bin.cb;

        ObjType := 0;
        OleCheck(AddrBook.OpenEntry(EntryIDSize, EntryID, nil, 0,
            ObjType, IUnknown(MailUser)));
        OleCheck(MailUser.GetProps(nil, 0, @DetailsCount, PSPropValue(Details)));

        for i := 0 to DetailsCount -1 do
        begin
          PropType := PROP_TYPE(Details[i].ulPropTag);
          PropID := PROP_ID(Details[i].ulPropTag);

          if (PropId = PROP_ID(PR_ICON)) and
             not Assigned(ImageStream) and
             (Details[0].Value.bin.cb > 0) then
          begin
            ImageStream := TMemoryStream.Create;
            ImageStream.Write(Details[i].Value.bin.lpb^, Details[i].Value.bin.cb);
            ImageStream.Seek(0,soFromBeginning);
          end
          else
          if PropId = $660d then
          begin
            if Length(ImageType) = 0 then
              ImageType := Details[i].Value.lpszA;
          end;
        end;

        FreeSRowSet(WabObject,TableRow);

        if Assigned(ImageStream) and Assigned(OnGetImage) then
          Synchronize(OnGetImage);

        break;
      end;
      FreeSRowSet(WabObject,TableRow);
    end
    else
      break;

    if Terminated then
      break;
  until false;



end;

end.
