unit Keyman.Developer.UI.ImportWindowsKeyboardDialogManager;

interface

uses
  System.Classes,
  Winapi.Windows;

function ShowImportWindowsKeyboard(Owner: TComponent): Boolean;

implementation

uses
  Vcl.Controls,
  Vcl.Dialogs,

  Keyman.Developer.System.ImportWindowsKeyboard,
  Keyman.Developer.UI.Project.UfrmNewProjectParameters,
  UfrmMain,
  UfrmSelectSystemKeyboard;

function ShowImportWindowsKeyboard(Owner: TComponent): Boolean;
var
  f: TfrmNewProjectParameters;
  iwk: TImportWindowsKeyboard;
  KLID: string;
  ProjectFilename: string;
  BCP47Tag: string;
  KeyboardName, KeyboardID: string;
begin
  // First, show picker for Windows languages

  if not SelectSystemKeyboard(Owner, KLID) then
    Exit(False);

  //
  // Collect data about the KLID: keyboard name, language tag
  //

  BCP47Tag := TImportWindowsKeyboard.FindBCP47TagForKLID(KLID);
  if not TImportWindowsKeyboard.GetKLIDDetails(KLID, KeyboardName, KeyboardID) then
    Exit(False);

  //
  // Present the New Project dialog, pre-populated
  //

  f := TfrmNewProjectParameters.Create(Owner);
  try
    f.KeyboardName := KeyboardName;
    f.KeyboardID := KeyboardID;
    f.BCP47Tags := BCP47Tag;

    Result := f.ShowModal = mrOk;
    if not Result then
      Exit;

    //
    // Run the import
    //

    iwk := TImportWindowsKeyboard.Create;
    try
      iwk.SourceKLID := KLID;
      iwk.DestinationPath := f.BasePath;
      iwk.KeyboardIDTemplate := f.KeyboardID;
      iwk.NameTemplate := f.KeyboardName;
      iwk.Copyright := f.Copyright;
      iwk.FullCopyright := f.FullCopyright;
      iwk.Version := f.Version;
      iwk.BCP47Tags := f.BCP47Tags;
      iwk.Author := f.Author;
      iwk.Targets := f.Targets;

      if not iwk.Execute then
        Exit(False);

      ProjectFilename := iwk.ProjectFilename;
    finally
      iwk.Free;
    end;
  finally
    f.Free;
  end;

  frmKeymanDeveloper.OpenProject(ProjectFilename);
  Result := True;
end;

end.
