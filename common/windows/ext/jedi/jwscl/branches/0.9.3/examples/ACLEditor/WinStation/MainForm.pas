{
This examples shows how to get, create Winstation and Desktops
and show/change their security information in the window ACL editor.
}
unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, JwsclWinStations, JwsclDesktops,
{$IFDEF FPC}
  LResources,
  Buttons,
{$ENDIF FPC}
     ActiveX,
     
     jwaWindows,
     JwsclTypes, JwsclExceptions, JwsclSid, JwsclAcl, JwsclToken,
     JwsclMapping, JwsclKnownSid, JwsclSecureObjects, JwsclSecurityDialogs,
     JwsclVersion, JwsclConstants, JwsclProcess, JwsclDescriptor, JwsclLsa,

     JwsclStrings; //JwsclStrings, must be at the end of uses list!!!;

type
  TForm1 = class(TForm)
    TreeView1: TTreeView;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Memo1: TMemo;
    ComboBoxSession: TComboBox;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure TreeView1Expanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
    fSecurityWindowStations : TJwSecurityWindowStations;
    fMyWinStation : TJwSecurityWindowStation;


    function OnLookupSIDs(Sender : TJwSecurityDescriptorDialog; const SIDList : TJwSecurityIdList; var SIDInfoList : TJwSidInfoRecordArray) : Cardinal;

    procedure OnSetSecurity(Sender : TJwSecurityDescriptorDialog;
              SecurityType : TJwSecurityInformationFlagSet;
              SecurityDialogFlags : TJwSecurityDialogFlags;
              SecurityResetType : TJwSecurityResetTypes;
              Settings : TJwSecurityDescriptorControlSet;
              NewSecurityDescriptor,
              MergedSecurityDescriptor
               : TJwSecurityDescriptor;
               var bSuccess : Boolean);
    function OnGetInheriteSource(Sender : TJwSecurityDescriptorDialog;
                                const Info : TJwSecurityInformationFlagSet;
                                const ACL : TJwSecurityAccessControlList;
                                var InheritanceArray : TJwInheritedFromArray) : Cardinal;

    procedure SetSelectedPermissions(mapping : TJwSecurityGenericMappingClass; SD : TJwSecurityDescriptor; SInfo : TJwSecurityInformationFlagSet);
    function GetSelectedPermissions(out mapping : TJwSecurityGenericMappingClass; out Name :TJwString; out ReadOnlyDACL, ReadOnlyOwner, bAuditAccess : Boolean) : TJwSecurityDescriptor;
    procedure CreateWinsta;
    procedure CheckAccessToSD(SD:  TJwSecurityDescriptor;
          cExtraDesiredAccess: TJwAccessMask; mapping : TJwSecurityGenericMappingClass;
          out ReadOnlyDACL, ReadOnlyOwner : Boolean);
    procedure InitSessionComboBox;
  public
    { Public-Deklarationen }
    procedure UpdateContent;

    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ENDIF FPC}


{ TForm1 }

procedure TForm1.UpdateContent;
var i : Integer;
    Names : TJwTJwStringArray;
    tempNode : TTreeNode;
begin
  Names := TJwSecurityWindowStations.GetWindowStationNames;

  TreeView1.Items.Clear;
  for i := 0 to Length(Names)-1 do
  begin
    tempNode := TreeView1.Items.AddChild(nil,Names[i]);
    TreeView1.Items.AddChildFirst(tempNode,'-');
  end;
  Names := nil;
end;

procedure TForm1.FormCreate(Sender: TObject);
  procedure EnableAdminGroup;
  var Tok : TJwSecurityToken;
  begin
    Tok := TJwSecurityToken.CreateTokenEffective(TOKEN_ALL_ACCESS);
    try
      Tok.TokenGroupsAttributesBySid[JwAdministratorsSID] :=
        Tok.TokenGroupsAttributesBySid[JwAdministratorsSID] + [sidaGroupEnabled];
      ShowMessage('Admin group was enabled');
    except
      //its normal for non admin users to fail here
    end;
    FreeAndNil(Tok);
  end;
var s : String;
begin
  //Enable well known Sids
  JwInitWellKnownSIDs;

  //to get real admin status we need to activate the administrator token
  EnableAdminGroup;
  try
    S := '';
    if not JwEnablePrivilege(SE_TCB_NAME,pst_EnableIfAvail) then
      S := SE_TCB_NAME;
    if not JwEnablePrivilege(SE_TAKE_OWNERSHIP_NAME,pst_EnableIfAvail) then
      S := S + ','+SE_TAKE_OWNERSHIP_NAME;
    if not JwEnablePrivilege(SE_RESTORE_NAME,pst_EnableIfAvail) then
      S := S + ','+SE_RESTORE_NAME;
    if not JwEnablePrivilege(SE_BACKUP_NAME,pst_EnableIfAvail) then
      S := S + ','+SE_BACKUP_NAME;
    if not JwEnablePrivilege(SE_SECURITY_NAME,pst_EnableIfAvail) then
      S := S + ','+SE_SECURITY_NAME;
    if not JwEnablePrivilege(SE_CHANGE_NOTIFY_NAME,pst_EnableIfAvail) then
      S := S + ','+SE_SERVICE_LOGON_NAME;

   { if Length(S) > 0 then
      MessageDlg('Privs not enabled: '+S,mtError, [mbok],0);
    }
  except
    on E : EJwsclSecurityException do
      MessageDlg(E.Message,mtError, [mbok],0);

  end;


  fSecurityWindowStations := TJwSecurityWindowStations.Create;


  InitSessionComboBox;

  UpdateContent;
  CreateWinsta;
end;

procedure TForm1.TreeView1Expanding(Sender: TObject; Node: TTreeNode;  var AllowExpansion: Boolean);

var WinName : TJwString;
    WinSta : TJwSecurityWindowStation;

    i : Integer;
    Names : TJwTJwStringArray;
    tempNode : TTreeNode;
begin
  Node.DeleteChildren;
  //
  WinName := Node.Text;


  try
    WinSta := TJwSecurityWindowStation.Open(WinName,False, WINSTA_ENUMDESKTOPS or WINSTA_ENUMERATE);
  except
    on E : EJwsclOpenWindowStationException do
    begin
      TreeView1.Items.AddChild(Node, 'GetLastError: '+IntToStr(E.LastError));
      exit;
    end;
  end;

  Names := WinSta.DesktopNames;

  for i := 0 to Length(Names) -1 do
  begin
    TreeView1.Items.AddChild(Node, Names[i]);
  end;

  FreeAndNil(WinSta);
end;

procedure TForm1.CheckAccessToSD(SD:  TJwSecurityDescriptor; cExtraDesiredAccess: TJwAccessMask; mapping : TJwSecurityGenericMappingClass;
          out ReadOnlyDACL, ReadOnlyOwner : Boolean);
var PrivilegeSet : TJwPrivilegeSet;
    GrantedAccess : TJwAccessMask;
begin
   if not Assigned(SD.PrimaryGroup) then
     SD.PrimaryGroup := JwNullSID; //AC needs a group

   TJwSecureGeneralObject.AccessCheck(
                                SD,//const SecurityDescriptor : TJwSecurityDescriptor;
                                nil,//const ClientToken : TJwSecurityToken;
                                WRITE_DAC or cExtraDesiredAccess,//const DesiredAccess : TJwAccessMask;
                                mapping,//const GenericMapping : TJwSecurityGenericMappingClass;
                                PrivilegeSet,//out PrivilegeSet : TJwPrivilegeSet;
                                GrantedAccess,//out GrantedAccess : TJwAccessMask;
                                ReadOnlyDACL//out AccessStatus : Boolean);
                                );
    ReadOnlyDACL := not ReadOnlyDACL;
    FreeAndNil(PrivilegeSet);


    TJwSecureGeneralObject.AccessCheck(
                                SD,//const SecurityDescriptor : TJwSecurityDescriptor;
                                nil,//const ClientToken : TJwSecurityToken;
                                WRITE_OWNER or cExtraDesiredAccess,//const DesiredAccess : TJwAccessMask;
                                mapping,//const GenericMapping : TJwSecurityGenericMappingClass;
                                PrivilegeSet,//out PrivilegeSet : TJwPrivilegeSet;
                                GrantedAccess,//out GrantedAccess : TJwAccessMask;
                                ReadOnlyOwner//out AccessStatus : Boolean);
                                );
    ReadOnlyOwner := not ReadOnlyOwner;
    FreeAndNil(PrivilegeSet);
end;


function TForm1.GetSelectedPermissions(out mapping : TJwSecurityGenericMappingClass; out Name :TJwString; out ReadOnlyDACL, ReadOnlyOwner, bAuditAccess : Boolean) : TJwSecurityDescriptor;
var Level : Integer;
    WinSta : TJwSecurityWindowStation;
    Desk : TJwSecurityDesktop;

    hWin : HWINSTA;

    SACL : TJwSAccessControlList;
    Flags : TJwSecurityInformationFlagSet;

    cExDesiredAccessMask : TJwAccessMask;
begin
  result := nil;
  if not Assigned(TreeView1.Selected) then
    exit;

  bAuditAccess := JwIsPrivilegeSet(SE_SECURITY_NAME,pqt_Available);

  cExDesiredAccessMask := 0;
  Flags := [siDaclSecurityInformation, siOwnerSecurityInformation];
  if bAuditAccess then
  begin
    Include(Flags, siSaclSecurityInformation);
    JwEnablePrivilege(SE_SECURITY_NAME,pst_Enable); //enable access to SACL generally

    cExDesiredAccessMask := ACCESS_SYSTEM_SECURITY; //add SACL access to handle
  end;

  try
    Level := TreeView1.Selected.Level;
    if Level = 0 then
    begin
      mapping := TJwSecurityWinStationMapping;
      try
        WinSta := TJwSecurityWindowStation.Open(TreeView1.Selected.Text,False, MAXIMUM_ALLOWED or cExDesiredAccessMask);
      except
        on E : EJwsclOpenWindowStationException do
        begin
          MessageDlg(E.Message,mtError,[mbok],0);
          exit;
        end;
      end;


      try
        Name := Winsta.Name; 
        result := TJwSecureGeneralObject.GetSecurityInfo(WinSta.Handle, SE_WINDOW_OBJECT, Flags);
      finally
        FreeAndNil(WinSta);
      end;

      {
        Test if we can write DAC and/or OWNER
      }
      CheckAccessToSD(
          result,//SD:  TJwSecurityDescriptor;
          WINSTA_WRITEATTRIBUTES,//cExtraDesiredAccess: TJwAccessMask;
          TJwSecurityWinStationMapping,//mapping : TJwSecurityGenericMappingClass;
          ReadOnlyDACL,ReadOnlyOwner//out ReadOnlyDACL, ReadOnlyOwner : Boolean
          );


    end
    else
    if Level = 1 then
    begin
      mapping := TJwSecurityDesktopMapping;

      hWin := GetProcessWindowStation;
      try
        WinSta := TJwSecurityWindowStation.Open(TreeView1.Selected.Parent.Text,False, MAXIMUM_ALLOWED);//WINSTA_ENUMDESKTOPS or WINSTA_ENUMERATE);
      except
        on E : EJwsclOpenWindowStationException do
        begin
          MessageDlg(E.Message,mtError,[mbok],0);
          exit;
        end;
      end;
      WinSta.SetWindowStation;

      try
        Name := TreeView1.Selected.Text;
        Desk := TJwSecurityDesktop.Create(
                            nil,//const aParent : TJwSecurityDesktops;
                            dcfOpen,//const aFlag : TJwDesktopCreationFlag;
                            false,//const aCloseOnDestroy : Boolean;
                            Name,//const aName : TJwString;
                            [dfAllowOtherAccountHook],//const DesktopFlags : TJwSecurityDesktopFlags;
                            false,//const doInherit : Boolean;
                            MAXIMUM_ALLOWED or cExDesiredAccessMask,
                            //READ_CONTROL or
                            //DESKTOP_READOBJECTS and DESKTOP_WRITEOBJECTS
                            //const aDesiredAccess : ACCESS_MASK;
                            nil//const aSecurityDescriptor : TJwSecurityDescriptor
                            );
        try
          Name := Desk.Name;
          result := TJwSecureGeneralObject.GetSecurityInfo(Desk.Handle, SE_WINDOW_OBJECT, Flags);
        finally
          FreeAndNil(Desk);
        end;

        {
        Test if we can write DAC and/or OWNER
        }
        CheckAccessToSD(
          result,//SD:  TJwSecurityDescriptor;
          DESKTOP_WRITEOBJECTS,//cExtraDesiredAccess: TJwAccessMask;
          TJwSecurityDesktopMapping,//mapping : TJwSecurityGenericMappingClass;
          ReadOnlyDACL,ReadOnlyOwner//out ReadOnlyDACL, ReadOnlyOwner : Boolean
          );
      finally
       WinSta.RevertWindowStation;
       FreeAndNil(WinSta);
      end;
    end;

  finally
    if bAuditAccess then
      JwEnablePrivilege(SE_SECURITY_NAME,pst_Disable);
  end;


end;

procedure TForm1.Button2Click(Sender: TObject);
var ACLEditor : TJwSecurityDescriptorDialog;
    SD : TJwSecurityDescriptor;
    WinSta : TJwSecurityWindowStation;
    Desk : TJwSecurityDesktop;
    mapping : TJwSecurityGenericMappingClass;
    bAuditAccess,
    ReadOnlyDACL,
    ReadOnlyOwner : Boolean;

    bIsDesktop : Boolean;
    Name,
    Title : TJwString;
begin
  bIsDesktop := TreeView1.Selected.Level = 1;

  Cursor := crHourGlass;

  ACLEditor := TJwSecurityDescriptorDialog.Create(GetActiveWindow);

  ACLEditor.Flags :=  [sdfAdvanced, sdfEditDacl, sdfEditOwner, sdfEditEffective
                      ,sdfContainer
                      ,sdfNoAdditionalPermission
                      ];



  ACLEditor.OnSetSecurity := OnSetSecurity;
  ACLEditor.OnGetInheriteSource := OnGetInheriteSource;
  ACLEditor.OnLookupSIDs := OnLookupSIDs;

  try
    //get security descriptor and write access status
    SD := GetSelectedPermissions(mapping, Name, ReadOnlyDACL,ReadOnlyOwner, bAuditAccess);

    Title := mapping.ClassName;
    System.Delete(Title,1,1); 
    ACLEditor.PageTitle := Title;

    ACLEditor.ObjectName := Name;

    //disable pages where necessary
    if ReadOnlyDACL then
      ACLEditor.Flags := ACLEditor.Flags + [sdfReadOnly];
    if ReadOnlyOwner then
      ACLEditor.Flags := ACLEditor.Flags + [sdfOwnerReadOnly];

    if ReadOnlyDACL and ReadOnlyOwner then
      ACLEditor.Flags := ACLEditor.Flags + [sdfReadOnly];

    if bAuditAccess then
      ACLEditor.Flags := ACLEditor.Flags + [sdfEditSacl];

    if not bIsDesktop then
      ACLEditor.Flags := ACLEditor.Flags + [sdfNoAclProtect];


    if SD <> nil then
    begin
      //we must define a correct mapping
      //otherwise ACL editor fails horribly
      //use TJwSecurityGenericMapping for all types
      ACLEditor.Mapping := mapping;


      ACLEditor.SecurityDescriptor := SD;
      SD.Free;

      ACLEditor.ShowModal;
    end;
  finally
    ACLEditor.Free;

    Cursor := crDefault;
  end;

end;


procedure TForm1.SetSelectedPermissions(mapping : TJwSecurityGenericMappingClass; SD : TJwSecurityDescriptor; SInfo : TJwSecurityInformationFlagSet);
var Level : Integer;
    WinSta : TJwSecurityWindowStation;
    Desk : TJwSecurityDesktop;
    Name : TJwString;

    anOwner, aGroup : TJwSecurityId;
    desktopDACL : TJwDAccessControlList;

    SACL : TJwSAccessControlList;
    Flags : TJwSecurityInformationFlagSet;
    cExDesiredAccessMask : TJwAccessMask;

    hWin : HWINSTA;
begin
  if not Assigned(TreeView1.Selected) then
    exit;

  cExDesiredAccessMask := 0;

  if (siSaclSecurityInformation in SInfo) then
  begin
    Include(Flags, siSaclSecurityInformation);
    //the caller is responsible for handle an exception
    JwEnablePrivilege(SE_SECURITY_NAME,pst_Enable); //enable access to SACL generally

    cExDesiredAccessMask := ACCESS_SYSTEM_SECURITY; //add SACL access to handle
  end;

  Level := TreeView1.Selected.Level;
  if Level = 0 then
  begin
    mapping := TJwSecurityWinStationMapping;
    try
      WinSta := TJwSecurityWindowStation.Open(TreeView1.Selected.Text,False, MAXIMUM_ALLOWED or cExDesiredAccessMask);
    except
      on E : EJwsclOpenWindowStationException do
      begin
        MessageDlg(E.Message,mtError,[mbok],0);
        exit;
      end;
    end;

    try
      TJwSecureGeneralObject.SetSecurityInfo(WinSta.Handle, SE_WINDOW_OBJECT,
            SInfo,SD);
    finally
      FreeAndNil(WinSta);
    end;
  end
  else
  if Level = 1 then
  begin
    mapping := TJwSecurityDesktopMapping;

    hWin := GetProcessWindowStation;
    try
      WinSta := TJwSecurityWindowStation.Open(TreeView1.Selected.Parent.Text,False, MAXIMUM_ALLOWED);
    except
      on E : EJwsclOpenWindowStationException do
      begin
        MessageDlg(E.Message,mtError,[mbok],0);
        exit;
      end;
    end;
    WinSta.SetWindowStation;

    try
      Name := TreeView1.Selected.Text;
      Desk := TJwSecurityDesktop.Create(
                          nil,//const aParent : TJwSecurityDesktops;
                          dcfOpen,//const aFlag : TJwDesktopCreationFlag;
                          false,//const aCloseOnDestroy : Boolean;
                          Name,//const aName : TJwString;
                          [dfAllowOtherAccountHook],//const DesktopFlags : TJwSecurityDesktopFlags;
                          true,//const doInherit : Boolean;
                          //READ_CONTROL or WRITE_DAC or WRITE_OWNER or DESKTOP_READOBJECTS and DESKTOP_WRITEOBJECTS
                          MAXIMUM_ALLOWED or cExDesiredAccessMask//GENERIC_ALL
                          ,//const aDesiredAccess : ACCESS_MASK;
                          nil//const aSecurityDescriptor : TJwSecurityDescriptor
                          );
//      Exclude(SInfo,siUnprotectedDaclSecurityInformation);
      try
        TJwSecureGeneralObject.SetSecurityInfo(Desk.Handle, SE_WINDOW_OBJECT,
                SInfo,SD);
       finally
        FreeAndNil(Desk);
      end;
    finally
     WinSta.RevertWindowStation;
     FreeAndNil(WinSta);
    end;
  end;
end;

function TForm1.OnGetInheriteSource(Sender : TJwSecurityDescriptorDialog;
                                const Info : TJwSecurityInformationFlagSet;
                                const ACL : TJwSecurityAccessControlList;
                                var InheritanceArray : TJwInheritedFromArray) : Cardinal;
var i : Integer;
begin
  result := S_FALSE;

  if TreeView1.Selected.Level < 1 then
    exit;
  //
  for i := 0 to ACL.Count -1 do
  begin
    if afInheritedAce in ACL.Items[i].Flags then
    begin
      InheritanceArray[i].AncestorName := TreeView1.Selected.Parent.Text;
    end;
  end;

  result := S_OK;
end;


procedure TForm1.OnSetSecurity(Sender: TJwSecurityDescriptorDialog;
  SecurityType: TJwSecurityInformationFlagSet;
  SecurityDialogFlags: TJwSecurityDialogFlags;
  SecurityResetType: TJwSecurityResetTypes;
  Settings: TJwSecurityDescriptorControlSet;
  NewSecurityDescriptor,
  MergedSecurityDescriptor: TJwSecurityDescriptor;
  var bSuccess : Boolean);



begin
  //bSuccess = false
  try
    SetSelectedPermissions(nil,
          NewSecurityDescriptor,
          SecurityType);
    bSuccess := true;
  except
    On E : EJwsclSecurityException do
      MessageDlg(E.Message,mtError,[mbok],0);
  end;
end;

procedure TForm1.CreateWinsta;
var i : Integer;
    desk : TJwSecurityDesktop;
    SD : TJwSecurityDescriptor;
    tempNode : TTreeNode;
begin
  fMyWinStation := nil;
  try
    fMyWinStation := TJwSecurityWindowStation.Create(
                      '',//const sName: TJwString;
                      true,//bInherit: Boolean;
                      GENERIC_ALL,//cDesiredAccess: TJwAccessMask;
                      true,//bCreateOnly: Boolean;
                      nil//SecurityDescriptor : TJwSecurityDescriptor
              );
    tempNode := TreeView1.Items.AddFirst(nil,fMyWinStation.Name);
    TreeView1.Items.AddChild(tempNode,'-');

    fMyWinStation.SetWindowStation;

    desk := TJwSecurityDesktop.Create(
                    nil,//const aParent: TJwSecurityDesktops;
                    dcfCreate,//const aFlag: TJwDesktopCreationFlag;
                    false,//const aCloseOnDestroy: Boolean;
                    'default',//const aName: TJwString;
                    [],//const DesktopFlags: TJwSecurityDesktopFlags;
                    false,//const doInherit: Boolean;
                    GENERIC_ALL,//const aDesiredAccess: ACCESS_MASK;
                    nil//const aSecurityDescriptor: TJwSecurityDescriptor
                   );
    FreeAndNil(desk);

    //
    SD := TJwSecurityDescriptor.Create;
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwSecurityProcessUserSID,false));
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,TJwSecurityId.Create('S-1-1-12345'),true));
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwAdministratorsSID,false));

    try
      desk := TJwSecurityDesktop.Create(
                    nil,//const aParent: TJwSecurityDesktops;
                    dcfCreate,//const aFlag: TJwDesktopCreationFlag;
                    false,//const aCloseOnDestroy: Boolean;
                    'secure',//const aName: TJwString;
                    [],//const DesktopFlags: TJwSecurityDesktopFlags;
                    false,//const doInherit: Boolean;
                    GENERIC_ALL,//const aDesiredAccess: ACCESS_MASK;
                    SD//const aSecurityDescriptor: TJwSecurityDescriptor
                   );      
    finally
      FreeAndNil(SD);
      FreeAndNil(desk);
      fMyWinStation.RevertWindowStation;
    end;
  except
    on E : EJwsclSecurityException do
      MessageDlg(E.Message,mtError,[mbok],0);
  end;


end;

destructor TForm1.Destroy;
begin
  FreeAndNil(fMyWinStation);
  inherited;                
end;

procedure TForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
var WinSt : TJwSecurityWindowStation;
    Desk  : TJwSecurityDesktop;
    obj : TObject;
begin
  Memo1.Clear;
  StatusBar1.SimpleText := '';

  if Assigned(fMyWinStation) then
  if (CompareText(fMyWinStation.Name,Node.Text) = 0) then
    StatusBar1.SimpleText := 'This winstation has been created by this demo!'
  else
  if Assigned(Node.Parent) and (CompareText(fMyWinStation.Name,Node.Parent.Text) = 0) then
   StatusBar1.SimpleText := 'This desktop has been created by this demo!';

  if (Node.Level = 0) then
  begin
    try
      WinSt := TJwSecurityWindowStation.Open(Node.Text,False,WINSTA_READATTRIBUTES);
    except
      on E : EJwsclSecurityException do
      begin
        Memo1.Lines.Add(E.Message);
        exit;
      end;
    end;

    try
      Memo1.Lines.Add('Name : '+Winst.Name);
      Memo1.Lines.Add('Flags.dwFlags : '+IntToStr(Winst.Flags.dwFlags));
      Memo1.Lines.Add('Flags.fInherit : '+BoolToStr(Winst.Flags.fInherit));
      Memo1.Lines.Add('Flags.fReserved : '+IntToStr(Integer(Winst.Flags.fReserved)));
      Memo1.Lines.Add('ObjectType : '+Winst.ObjectType);

      if Assigned(Winst.UserSID) then
      try
        Memo1.Lines.Add('UserSID : '+Winst.UserSID.AccountName['']);
      except
        Memo1.Lines.Add('UserSID : '+Winst.UserSID.StringSID);
      end
      else
        Memo1.Lines.Add('UserSID : (nil)');
    finally
      FreeAndNil(obj);
      FreeAndNil(WinSt);
    end;
  end
  else
  begin

    try
      WinSt := TJwSecurityWindowStation.Open(TreeView1.Selected.Parent.Text,False, MAXIMUM_ALLOWED);
    except
      on E : EJwsclOpenWindowStationException do
      begin
        MessageDlg(E.Message,mtError,[mbok],0);
        exit;
      end;
    end;
    WinSt.SetWindowStation;

    try
      Desk := TJwSecurityDesktop.Create(
                  nil,//const aParent: TJwSecurityDesktops;
                  dcfOpen,//const aFlag: TJwDesktopCreationFlag;
                  false,//const aCloseOnDestroy: Boolean;
                  Node.Text,//const aName: TJwString;
                  [],//const DesktopFlags: TJwSecurityDesktopFlags;
                  false,//const doInherit: Boolean;
                  DESKTOP_READOBJECTS,//const aDesiredAccess: ACCESS_MASK;
                  nil//const aSecurityDescriptor: TJwSecurityDescriptor
                  );

    except
      on E : EJwsclSecurityException do
      begin
        Winst.RevertWindowStation;
        FreeAndNil(Winst);
        Memo1.Lines.Add(E.Message);
        exit;
      end;
    end;

    Winst.RevertWindowStation;
    FreeAndNil(Winst);

    try
      Memo1.Lines.Add('Name : '+Desk.Name);

      Memo1.Lines.Add('DesktopFlags : '+IntToStr(Desk.DesktopFlagsToInt(Desk.DesktopFlags)));
      Memo1.Lines.Add('Opened : '+BoolToStr(Desk.Opened));
      Memo1.Lines.Add('IsInputDesktop : '+BoolToStr(Desk.IsInputDesktop));
      Memo1.Lines.Add('ObjectType : '+Desk.ObjectType);


      if Assigned(Desk.UserSID) then
      begin
        try
          Memo1.Lines.Add('UserSID : '+Desk.UserSID.AccountName['']);
        except
          Memo1.Lines.Add('UserSID : '+Desk.UserSID.StringSID);
        end;
      end
      else
        Memo1.Lines.Add('UserSID : (nil)');
    finally
      FreeAndNil(Desk);
    end;
  end;




end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Button3Click(Sender: TObject);
var mapping : TJwSecurityGenericMappingClass;
    Name : TJwString;
    ReadOnlyDACL,
    ReadOnlyOwner,
    bAuditAccess : Boolean;
    SD : TJwSecurityDescriptor;

    sOwner,
    sDACL,
    sText : TJwString;
begin
  try
    SD := GetSelectedPermissions(mapping, Name, ReadOnlyDACL,ReadOnlyOwner, bAuditAccess);
  except
    on E : Exception do
    begin
      MessageDlg(E.Message,mtError,[mbok],0);
      exit;
    end;
  end;

  if Assigned(SD) then
  try
    if Assigned(SD.Owner) then
      sOwner := SD.Owner.GetText(true);
    if Assigned(SD.DACL) then
      sDACL := SD.DACL.GetTextMap(mapping);

    sText := 'Owner: '+ sOwner + #13#10#13#10;
    sText := sText + sDACL;

    MessageDlg(sText,mtInformation,[mbok],0);
  finally
    SD.Free;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fSecurityWindowStations);
end;

function TForm1.OnLookupSIDs(Sender: TJwSecurityDescriptorDialog;
  const SIDList: TJwSecurityIdList;
  var SIDInfoList: TJwSidInfoRecordArray): Cardinal;
var i : Integer;
begin
 //
   result := S_OK;
   for i := 0 to Length(SIDInfoList) -1 do
   begin
     if Assigned(SIDInfoList[i].Exception) and Assigned(SIDInfoList[i].pSid) then
     begin
       SIDInfoList[i].sUPN := 'unknown\???';
       SIDInfoList[i].sCommonName := 'Who is this? ('+TJwSecurityId(SIDInfoList[i].pSid).StringSID+')';
       SIDInfoList[i].sClass := scnUnknown;
     end;
   end;
end;

procedure TForm1.InitSessionComboBox;
var Sessions : TJwLogonSessionArray;
    i : Integer;
    S : String;
    Data : TJwLsaLogonSessionData;
begin
  ComboBoxSession.Items.Clear;
  Sessions := TJwLsaLogonSession.GetSessions;
  for i := low(Sessions) to high(Sessions) do
  begin
    S := TJwPrivilege.LUIDtoText(Sessions[i]);
    try
      Data := TJwLsaLogonSession.GetSessionData(Sessions[i]);
      S := S + ': '+Data.LogonDomain+'\'+Data.LogonServer+'\'+Data.UserName;
      FreeAndNil(Data);
    except
      on E : EJwsclWinCallFailedException do
        S := S + ' Error: '+E.GetLastErrorMessage(E.LastError);
    end;
    ComboBoxSession.Items.Add(S);
  end;

end;

initialization
{$IFDEF FPC}
  {$I MainForm.lrs}
{$ENDIF FPC}
end.


