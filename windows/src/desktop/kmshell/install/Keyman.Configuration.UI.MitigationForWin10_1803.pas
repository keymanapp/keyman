// See #1285 for detail
unit Keyman.Configuration.UI.MitigationForWin10_1803;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.StdCtrls;

procedure CheckForMitigationWarningFor_Win10_1803(ASilent: Boolean; const ALogFile: string);
procedure ShowMitigationWarningFormFor_Win10_1803(const message: string);

implementation

uses
  kmcomapi_errors,
  kmint,
  Upload_Settings,
  utilexecute;

const
  SLearnMoreURLPath = '/go/desktop/10.0/issue-1285';


procedure CheckForMitigationWarningFor_Win10_1803(ASilent: Boolean; const ALogFile: string);
var
  i: Integer;
  str: TStringList;
begin
  for i := 0 to kmcom.Errors.Count - 1 do
    if kmcom.Errors[i].ErrorCode = KMN_W_ProfileInstall_Win10_1803_MitigationApplied then
    begin
      if ASilent then
      begin
        if ALogFile <> '' then
        begin
          str := TStringList.Create;
          try
            str.Add(kmcom.Errors[i].Description);
            str.SaveToFile(ALogFile);
          finally
            str.Free;
          end;
        end;
      end
      else
        ShowMitigationWarningFormFor_Win10_1803(kmcom.Errors[i].Description);
      Exit;
    end;
end;

procedure ShowMitigationWarningFormFor_Win10_1803(const message: string);
var
  i: Integer;
  frm: TForm;
begin
  frm := CreateMessageDialog(message, mtInformation, [mbYes, mbOK]); // mtWarning looks too scary
  try
    for i := 0 to frm.ControlCount - 1 do
      if (frm.Controls[i] is TButton) and ((frm.Controls[i] as TButton).ModalResult = mrYes) then
        (frm.Controls[i] as TButton).Caption := 'Learn more';

    if frm.ShowModal = mrYes then
      TUtilExecute.URL(MakeKeymanURL(SLearnMoreURLPath));
  finally
    frm.Free;
  end;
end;

end.
