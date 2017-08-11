unit PersistForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, PersistSettings;

type
  // base form to inherit from if you want your forms size and position to be saved automatically
  TfrmPersistable = class(TForm, IUnknown, IPersistSettings)
  private
    { Private declarations }
  protected
    procedure Load(Storage: TPersistStorage); virtual;
    procedure Save(Storage: TPersistStorage); virtual;
  public
    { Public declarations }
  end;

var
  frmPersistable: TfrmPersistable;

implementation

{$R *.DFM}

{ TfrmPersistable }

procedure TfrmPersistable.Load(Storage: TPersistStorage);
begin
  if Position in [poDesigned, poDefault, poDefaultSizeOnly, poDefaultPosOnly] then
  begin
    Top := Storage.ReadInteger(ClassName, 'Top', (Screen.Height - ClientHeight) div 2);
    Left := Storage.ReadInteger(ClassName, 'Left', (Screen.Width - ClientWidth) div 2);
  end;
  if BorderStyle in [bsSizeable, bsSizeToolWin] then
  begin
    Width := Storage.ReadInteger(ClassName, 'Width', Width);
    Height := Storage.ReadInteger(ClassName, 'Height', Height);
  end;
  WindowState := TWindowState(Storage.ReadInteger(ClassName, 'WindowState',Ord(WindowState)));
end;

procedure TfrmPersistable.Save(Storage: TPersistStorage);
begin
  if not IsZoomed(Handle) and not IsIconic(Application.Handle) then
  begin
    if Position in [poDesigned, poDefault, poDefaultSizeOnly, poDefaultPosOnly] then
    begin
      Storage.WriteInteger(ClassName, 'Top', Top);
      Storage.WriteInteger(ClassName, 'Left', Left);
    end;
    if (BorderStyle in [bsSizeable, bsSizeToolWin]) then
    begin
      Storage.WriteInteger(ClassName, 'Width', Width);
      Storage.WriteInteger(ClassName, 'Height', Height);
    end;
  end;
  Storage.WriteInteger(ClassName, 'WindowState',Ord(WindowState));
end;

end.

