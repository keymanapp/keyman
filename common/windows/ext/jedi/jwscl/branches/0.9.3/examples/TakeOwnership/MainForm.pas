unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvExMask, JvToolEdit, JvBaseDlg,
  JvObjectPickerDialog,

  jwaWindows,
     JwsclTypes, JwsclExceptions, JwsclSid, JwsclAcl, JwsclToken,
     JwsclMapping, JwsclKnownSid, JwsclSecureObjects, JwsclSecurityDialogs,
     JwsclVersion, JwsclConstants, JwsclProcess, JwsclDescriptor,
     JwsclStrings, JvExControls, JvPoweredBy;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    JvFilenameEdit1: TJvFilenameEdit;
    JvObjectPickerDialog_2: TJvObjectPickerDialog;
    Button_ShowSecurityDialog: TButton;
    Label_Owner: TLabel;
    Button_ChangeOwner: TButton;
    Memo: TMemo;
    JvObjectPickerDialog: TJvObjectPickerDialog;
    JvObjectPickerDialog_OK: TJvObjectPickerDialog;
    JvPoweredByJVCL1: TJvPoweredByJVCL;
    procedure FormCreate(Sender: TObject);
    procedure JvFilenameEdit1Change(Sender: TObject);
    procedure Button_ShowSecurityDialogClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button_ChangeOwnerClick(Sender: TObject);
  private
    { Private-Deklarationen }
    secureFile : TJwSecureFileObject;

    procedure OnSetSecurity(Sender : TJwSecurityDescriptorDialog;
              SecurityType : TJwSecurityInformationFlagSet;
              SecurityDialogFlags : TJwSecurityDialogFlags;
              SecurityResetType : TJwSecurityResetType;
              Settings : TJwSecurityDescriptorControlSet;
              NewSecurityDescriptor,
              MergedSecurityDescriptor
               : TJwSecurityDescriptor;
               var bSuccess: boolean);
  public
    { Public-Deklarationen }
    procedure UpdateFileEdit;

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  secureFile := nil;
  JvFilenameEdit1.FileName := PAramStr(0);


  Memo.Clear;
  if not JwIsPrivilegeSet(SE_TAKE_OWNERSHIP_NAME) then
    Memo.Lines.Add('Warning: You cannot take ownership for objects that do not include your principal in DACL.');


  if not JwIsPrivilegeSet(SE_RESTORE_NAME) then
    Memo.Lines.Add('Warning: You cannot change owner to arbitrary principals.');

end;

procedure TForm1.JvFilenameEdit1Change(Sender: TObject);
var Owner : TJwSecurityId;
begin
  //
  Button_ShowSecurityDialog.Enabled := FileExists(JvFilenameEdit1.FileName);
  Button_ChangeOwner.Enabled := FileExists(JvFilenameEdit1.FileName);


  if Assigned(secureFile) then
    FreeAndNil(secureFile);

  if FileExists(JvFilenameEdit1.FileName) then
  begin
    secureFile := TJwSecureFileObject.Create(JvFilenameEdit1.FileName);

    try
      Owner := secureFile.GetOwner;
      try
        Label_Owner.Caption := 'Current Owner: '+Owner.AccountName[''];
      except
        on E : EJwsclSecurityException do
          Label_Owner.Caption := 'Current Owner: '+Owner.StringSID;
      end;
      FreeAndNil(Owner);
    except
      on E : EJwsclSecurityException do
        Label_Owner.Caption := 'Current Owner: (unknown)';
    end;
  end;




end;

procedure TForm1.Button_ShowSecurityDialogClick(Sender: TObject);
var Dlg : TJwSecurityDescriptorDialog;

    SD : TJwSecurityDescriptor;
begin

  try
    SD := secureFile.GetSecurityDescriptor([siOwnerSecurityInformation,siDaclSecurityInformation]);

    Dlg := TJwSecurityDescriptorDialog.Create(Handle);
    Dlg.PageTitle := 'Security';
    Dlg.ObjectName := ExtractFileName(JvFilenameEdit1.FileName);
    Dlg.Mapping := TJwSecurityFileFolderMapping;
    Dlg.Flags := [sdfEditDacl, sdfEditOwner, sdfNoAclProtect, sdfNoTreeApply, sdfNoAdditionalPermission,
                  {sdfReadOnly,}{ sdfOwnerReadOnly,} sdfAdvanced];
    Dlg.OnSetSecurity := OnSetSecurity;
    Dlg.SecurityDescriptor := SD;
    FreeAndNil(SD);

    Dlg.ShowModal;

  except
    on E : EJwsclSecurityException do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(secureFile);
end;

procedure TForm1.Button_ChangeOwnerClick(Sender: TObject);
  procedure ExtractDS(Srv : TJwString; out Domain, User : TJwString);
  var i,i1 : Cardinal;
      pName : TJwPChar;
  begin
    pName := nil;

    //WinNT://XXX/YYY/ASPNET
    i := pos('://',Srv);
    if i > 0 then
      System.Delete(Srv,1,i+2);  //WINNT://

    i := pos('/',Srv); //domain/
    if i > 0 then
    begin
      Domain := Copy(Srv,1,i-1);
      System.Delete(Srv,1,i);
    end;

    i := pos('/',Srv); //computername
    if i > 0 then
    begin
      System.Delete(Srv,1,i);
    end;

    User := Copy(Srv,1,Length(Srv));

  end;

var obj : TObjectPickerSelection;
    newOwner : TJwSecurityId;
    s : String;
    user, domain : TJwString;
begin
 //
  if not JvObjectPickerDialog.Execute then
   exit;

  if JwIsPrivilegeSet(SE_TAKE_OWNERSHIP_NAME) then
    JwEnablePrivilege(SE_TAKE_OWNERSHIP_NAME,pst_Enable);

  if JwIsPrivilegeSet(SE_RESTORE_NAME) then
    JwEnablePrivilege(SE_RESTORE_NAME,pst_Enable);

  obj := JvObjectPickerDialog.Selection.Items[0];
  if obj.UPN = obj.AdsPath then;
  if obj.ObjectClass = obj.Name then;
  if s = '' then;


  if pos('S-',obj.Name) = 1 then
    newOwner := TJwSecurityId.Create(obj.Name)
  else
  begin
    if Length(obj.AdsPath) = 0 then
    begin
      Domain := '';
      User := obj.Name;
    end
    else
      ExtractDS(obj.AdsPath, domain, user);

    try
      newOwner := TJwSecurityId.Create(domain,user);
    except
      on E : EJwsclSecurityException do
      begin
        if E.LastError = 1722 then
        begin
          newOwner := TJwSecurityId.Create('',user);
        end;
      end;
    end;
  end;
  try
    secureFile.SetOwner(newOwner);
   except
    on E : EJwsclSecurityException do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;

  if JwIsPrivilegeSet(SE_RESTORE_NAME) then
    JwEnablePrivilege(SE_RESTORE_NAME,pst_Disable);
  if JwIsPrivilegeSet(SE_TAKE_OWNERSHIP_NAME) then
    JwEnablePrivilege(SE_TAKE_OWNERSHIP_NAME,pst_Disable);



  UpdateFileEdit;
end;

procedure TForm1.UpdateFileEdit;
begin
  JvFilenameEdit1Change(JvFilenameEdit1);
end;

procedure TForm1.OnSetSecurity(Sender: TJwSecurityDescriptorDialog;
  SecurityType: TJwSecurityInformationFlagSet;
  SecurityDialogFlags: TJwSecurityDialogFlags;
  SecurityResetType: TJwSecurityResetType;
  Settings: TJwSecurityDescriptorControlSet; NewSecurityDescriptor,
  MergedSecurityDescriptor: TJwSecurityDescriptor;
  var bSuccess: boolean);
var bOk : Boolean;
begin
  //
  bOK := false;
  if siOwnerSecurityInformation in SecurityType then
  begin
    if JwIsPrivilegeSet(SE_TAKE_OWNERSHIP_NAME) then
      JwEnablePrivilege(SE_TAKE_OWNERSHIP_NAME,pst_Enable);

    if JwIsPrivilegeSet(SE_RESTORE_NAME) then
      JwEnablePrivilege(SE_RESTORE_NAME,pst_Enable);

     bOK := true;
     try
       secureFile.SetOwner(NewSecurityDescriptor.Owner);
     except
       on E : EJwsclSecurityException do
       begin
         bOK := FALSE;
         MessageDlg(E.Message, mtError, [mbOK], 0);
       end;
     end;
     if bOK then
       MessageDlg('Owner changed successfully', mtInformation, [mbOK], 0);

    if JwIsPrivilegeSet(SE_RESTORE_NAME) then
      JwEnablePrivilege(SE_RESTORE_NAME,pst_Disable);
    if JwIsPrivilegeSet(SE_TAKE_OWNERSHIP_NAME) then
      JwEnablePrivilege(SE_TAKE_OWNERSHIP_NAME,pst_Disable);
    UpdateFileEdit;
  end
  else
  begin
    MessageDlg('The DACL cannot be changed in this version.', mtError, [mbOK], 0);

  end;

  bSuccess := bOK;
end;

end.
