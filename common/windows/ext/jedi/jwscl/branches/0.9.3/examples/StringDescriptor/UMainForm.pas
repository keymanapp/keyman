unit UMainForm;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  jwaWindows, JwsclUtils,
  JwsclTypes, JwsclExceptions, JwsclSid, JwsclAcl, JwsclToken,
  JwsclMapping, JwsclKnownSid, JwsclSecureObjects, JwsclSecurityDialogs,
  JwsclVersion, JwsclConstants, JwsclProcess, JwsclDescriptor,

  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!;


  

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo_SD: TMemo;
    ComboBoxMapping: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure OnSetSecurity(Sender: TJwSecurityDescriptorDialog;
      SecurityType: TJwSecurityInformationFlagSet;
      SecurityDialogFlags: TJwSecurityDialogFlags;
      SecurityResetType: TJwSecurityResetTypes;
      Settings: TJwSecurityDescriptorControlSet;
      NewSecurityDescriptor, MergedSecurityDescriptor
      : TJwSecurityDescriptor;
      var bSuccess: boolean);

  public
    { Public-Deklarationen }

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var ACLEditor : TJwSecurityDescriptorDialog;
    SecurityDescriptor : TJwSecurityDescriptor;
    Mapping : TJwSecurityGenericMappingClass;
    s : TJwString;
begin
  ACLEditor := TJwSecurityDescriptorDialog.Create(GetActiveWindow);

  ACLEditor.Flags :=  [sdfAdvanced, sdfEditDacl, sdfEditOwner, sdfEditEffective,
                       sdfNoAdditionalPermission, sdfEditSacl];
  ACLEditor.ObjectName := 'ACL Editor Demo';
  ACLEditor.ServerName := '';


  SecurityDescriptor := nil;
  try
    if Memo_SD.Text <> '' then
      SecurityDescriptor := TJwSecurityDescriptor.Create(Memo_SD.Text)
    else
    begin
      SecurityDescriptor := TJwSecurityDescriptor.Create;
      SecurityDescriptor.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwWorldSID,false));
    end;

    ACLEditor.OnSetSecurity := OnSetSecurity;
    Mapping := TJwSecurityGenericMappingClass(ComboBoxMapping.Items.Objects[ComboBoxMapping.ItemIndex]);
    ACLEditor.Mapping := Mapping;


    ACLEditor.SecurityDescriptor := SecurityDescriptor;


    if ACLEditor.ShowModal then
    begin
      s := ACLEditor.SecurityDescriptor.GetSecurityDescriptorString(JwAllSiFlags);
      Memo_SD.Text := s;
    end;

  finally
    ACLEditor.Free;
    FreeAndNil(SecurityDescriptor);
    Cursor := crDefault;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  JwInitWellKnownSIDs;
  ComboBoxMapping.ItemIndex := ComboBoxMapping.Items.AddObject('Generic',Pointer(TJwSecurityGenericMapping));
  ComboBoxMapping.Items.AddObject('Process',Pointer(TJwSecurityProcessMapping));
  ComboBoxMapping.Items.AddObject('Threads',Pointer(TJwSecurityThreadMapping));
  ComboBoxMapping.Items.AddObject('Service',Pointer(TJwSecurityServiceMapping));
  ComboBoxMapping.Items.AddObject('ServiceManager',Pointer(TJwSecurityServiceManagerMapping));
  ComboBoxMapping.Items.AddObject('Desktop',Pointer(TJwSecurityDesktopMapping));
  ComboBoxMapping.Items.AddObject('WindowStation',Pointer(TJwSecurityWinStationMapping));
  ComboBoxMapping.Items.AddObject('Registry',Pointer(TJwSecurityRegistryMapping));
  ComboBoxMapping.Items.AddObject('TJwSecurityFileMapping',Pointer(TJwSecurityFileMapping));
  ComboBoxMapping.Items.AddObject('TJwSecurityFileMapMapping',Pointer(TJwSecurityFileMapMapping));
  ComboBoxMapping.Items.AddObject('FileFolder',Pointer(TJwSecurityFileFolderMapping));
  ComboBoxMapping.Items.AddObject('Token',Pointer(TJwSecurityTokenMapping));
  ComboBoxMapping.Items.AddObject('Semaphore',Pointer(TJwSecuritySemaphoreMapping));
  ComboBoxMapping.Items.AddObject('Event',Pointer(TJwSecurityEventMapping));
  ComboBoxMapping.Items.AddObject('Mutex',Pointer(TJwSecurityMutexMapping));
  ComboBoxMapping.Items.AddObject('Job',Pointer(TJwSecurityJobMapping));
  ComboBoxMapping.Items.AddObject('Pipe',Pointer(TJwSecurityPipeMapping));
  ComboBoxMapping.Items.AddObject('Service Manager',Pointer(TJwSecurityServiceMapping));
  ComboBoxMapping.Items.AddObject('Service',Pointer(TJwSecurityServiceMapping));
  ComboBoxMapping.Items.AddObject('Printer',Pointer(TJwSecurityPrinterMapping));
  ComboBoxMapping.Items.AddObject('Timer',Pointer(TJwSecurityTimerMapping));
end;

procedure TForm1.OnSetSecurity(Sender: TJwSecurityDescriptorDialog;
  SecurityType: TJwSecurityInformationFlagSet;
  SecurityDialogFlags: TJwSecurityDialogFlags;
  SecurityResetType: TJwSecurityResetTypes;
  Settings: TJwSecurityDescriptorControlSet; NewSecurityDescriptor,
  MergedSecurityDescriptor: TJwSecurityDescriptor; var bSuccess: boolean);
begin
  bSuccess := true;
end;

end.
