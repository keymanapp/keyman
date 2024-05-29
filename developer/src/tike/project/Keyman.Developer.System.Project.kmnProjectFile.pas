(*
  Name:             Keyman.Developer.System.Project.kmnProjectFile
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      1 Aug 2006

  Modified Date:    24 Aug 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Aug 2006 - mcdurdin - Add loading and saving from XML
                    23 Aug 2006 - mcdurdin - Add CompileKeyboardToWeb function
                    28 Sep 2006 - mcdurdin - Editions
                    06 Oct 2006 - mcdurdin - Add TestKeymanWeb
                    04 Dec 2006 - mcdurdin - WideString all properties; Add TestKeymanWeb function
                    04 Jan 2007 - mcdurdin - Clear messages before compiling
                    16 May 2007 - mcdurdin - I759 - Compiler did not say 'complete' when finished
                    16 May 2007 - mcdurdin - I791 - Unicode characters not rendering in Project
                    30 May 2007 - mcdurdin - I791 - Non-ascii characters not rendering correctly in Project (keyboard name only)
                    19 Nov 2007 - mcdurdin - I1153 - Crash when closing file
                    27 Mar 2008 - mcdurdin - I1370 - Rename files to avoid invalid characters during compile of KeymanWeb keyboards
                    16 Jan 2009 - mcdurdin - Add font helper call
                    26 Jul 2010 - mcdurdin - I2468 - Eliminate KeymanWeb Pack
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    13 Dec 2012 - mcdurdin - I3681 - V9.0 - KeymanWeb compiler should output formatted js when debug=1
                    10 Jan 2014 - mcdurdin - I4021 - V9.0 - Redesign Keyboard Wizard to integrate V9 features
                    21 Feb 2014 - mcdurdin - I4057 - V9.0 - Keyman Developer Keyboard Font dialog helpful to reduce font confusion
                    21 Feb 2014 - mcdurdin - I4063 - V9.0 - Web Debugger needs to embed fonts for OSK and text area
                    27 Feb 2014 - mcdurdin - I4087 - V9.0 - KeymanWeb compiler does not clear messages before starting
                    19 Mar 2014 - mcdurdin - I4140 - V9.0 - Add keyboard version information to keyboards
                    25 Sep 2014 - mcdurdin - I4409 - V9.0 - Wrong font selected in keyboard debugger touch layout
                    04 Nov 2014 - mcdurdin - I4504 - V9.0 - Consolidate the compile action into single command
                    04 May 2015 - mcdurdin - I4687 - V9.0 - Split project UI actions into separate classes
                    04 May 2015 - mcdurdin - I4688 - V9.0 - Add build path to project settings
                    04 May 2015 - mcdurdin - I4692 - V9.0 - Add Clean as project action
                    04 May 2015 - mcdurdin - I4694 - V9.0 - Split UI actions from non-UI actions in projects
                    05 May 2015 - mcdurdin - I4698 - V9.0 - Split project and user preferences files
                    06 May 2015 - mcdurdin - I4701 - V9.0 - If version is not specified in keyboard file, it gets blank instead of 1.0 in kmcomp .kpj
                    11 May 2015 - mcdurdin - I4706 - V9.0 - Update compile logging for silent and warning-as-error cleanness
                    27 May 2015 - mcdurdin - I4564 - V9.0 - If only web/touch targets are selected, compiler does not run
                    27 May 2015 - mcdurdin - I4720 - Compiling a standalone keyboard crashes Developer [CrashID:tike.exe_9.0.503.0_00A4316C_EAccessViolation]
                    03 Aug 2015 - mcdurdin - I4823 - Note in compile log if symbols are included in build
                    24 Aug 2015 - mcdurdin - I4865 - Add treat hints and warnings as errors into project
                    24 Aug 2015 - mcdurdin - I4866 - Add warn on deprecated features to project and compile

*)
unit Keyman.Developer.System.Project.kmnProjectFile;  // I3306   // I4687   // I4688   // I4692

interface

uses
  System.SysUtils,
  Xml.XMLIntf,

  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectFiles,
  Keyman.Developer.System.Project.ProjectFileType,
  UKeymanTargets;

type
  TkmnProjectFile = class;

  TkmnProjectFile = class(TOpenableProjectFile)
  private
    FDebug: Boolean;
    FHeader_Name: WideString;
    FHeader_Message: WideString;
    FHeader_Copyright: WideString;
    FTargets: TKeymanTargets;
    FKVKFileName: string;

    function GetTargetFilename: string;
    function GetKmxTargetFilename: string;
    function GetJSTargetFilename: string;
  protected
    function GetRelativeOrder: Integer; override;
    procedure GetFileParameters; override;

    property KVKFileName: string read FKVKFileName;
    property IsDebug: Boolean read FDebug;
  public
    function IsCompilable: Boolean; override;
    procedure Load(node: IXMLNode); override;   // I4698
    procedure Save(node: IXMLNode); override;   // I4698
    procedure LoadState(node: IXMLNode); override;   // I4698
    procedure SaveState(node: IXMLNode); override;   // I4698

    property Debug: Boolean read FDebug write FDebug;

    property TargetFilename: string read GetTargetFilename;
    property KmxTargetFilename: string read GetKmxTargetFilename;
    property JSTargetFilename: string read GetJSTargetFilename;
    property Header_Name: WideString read FHeader_Name;
    property Header_Copyright: WideString read FHeader_Copyright;
    property Header_Message: WideString read FHeader_Message;
    property Targets: TKeymanTargets read FTargets;
  end;

implementation

uses
  System.Classes,
  System.Variants,
  Winapi.Windows,

  KeyboardParser,
  kmxfileconsts,
  KeyboardFonts,
  Keyman.System.KeyboardUtils,
  utilfiletypes,
  utilsystem;

{-------------------------------------------------------------------------------
 - TkmnProjectFile                                                             -
 -------------------------------------------------------------------------------}

procedure TkmnProjectFile.Save(node: IXMLNode);   // I4698
begin
  inherited Save(node);   // I4698
  node := node.AddChild('Details');
  if FHeader_Name <> '' then node.AddChild('Name').NodeValue := FHeader_Name;
  if FHeader_Copyright <> '' then node.AddChild('Copyright').NodeValue := FHeader_Copyright;
  if FHeader_Message <> '' then node.AddChild('Message').NodeValue := FHeader_Message;
end;

procedure TkmnProjectFile.SaveState(node: IXMLNode);   // I4698
begin
  inherited SaveState(node);
  node.AddChild('Debug').NodeValue := FDebug;
end;

procedure TkmnProjectFile.Load(node: IXMLNode);   // I4698
begin
  inherited Load(node);

  if node.ChildNodes.IndexOf('Details') < 0 then Exit;
  node := node.ChildNodes['Details'];
  if node.ChildNodes.IndexOf('Name') >= 0 then FHeader_Name := VarToWideStr(node.ChildValues['Name']);
  if node.ChildNodes.IndexOf('Copyright') >= 0 then FHeader_Copyright := VarToWideStr(node.ChildValues['Copyright']);
  if node.ChildNodes.IndexOf('Message') >= 0 then FHeader_Message := VarToWideStr(node.ChildValues['Message']);
end;

procedure TkmnProjectFile.LoadState(node: IXMLNode);   // I4698
begin
  inherited LoadState(node);
  try
    if node.ChildNodes.IndexOf('Debug') >= 0 then FDebug := node.ChildValues['Debug'];
  except
    FDebug := False;
  end;
end;

function TkmnProjectFile.GetRelativeOrder: Integer;
begin
  Result := 20;
end;

function TkmnProjectFile.GetTargetFilename: string;
begin
  // Always returns the .kmx, even if the keyboard
  // only compiles to a .js
  Result := GetKmxTargetFilename;
end;

function TkmnProjectFile.GetKmxTargetFilename: string;
var
  FTempFileVersion: string;
begin
  if FTargets = [] then
    GetFileParameters;

  // https://github.com/keymanapp/keyman/issues/631
  // This appears to be a Delphi compiler bug (RSP-20457)
  // Workaround is to make a copy of the parameter locally
  // which fixes the reference counting.
  FTempFileVersion := FileVersion;
  Result := OwnerProject.GetTargetFilename(ChangeFileExt(FileName, Ext_KeymanFile), FileName, FTempFileVersion);
end;

function TkmnProjectFile.GetJSTargetFilename: string;
var
  FTempFileVersion: string;
begin
  if FTargets = [] then
    GetFileParameters;

  // https://github.com/keymanapp/keyman/issues/631
  // This appears to be a Delphi compiler bug (RSP-20457)
  // Workaround is to make a copy of the parameter locally
  // which fixes the reference counting.
  FTempFileVersion := FileVersion;
  Result := OwnerProject.GetTargetFilename(ChangeFileExt(FileName, Ext_Javascript), FileName, FTempFileVersion);
end;

function TkmnProjectFile.IsCompilable: Boolean;
begin
  Result := True;
end;

procedure TkmnProjectFile.GetFileParameters;
var
  j: Integer;
  value: WideString;
  FVersion: string;   // I4701
begin
  FHeader_Message := '';
  FHeader_Copyright := '';
  FHeader_Name := '';
  FKVKFileName := '';
  SetFileVersion('1.0');   // I4701

  if not FileExists(FileName) then Exit;

  with TKeyboardParser.Create do   // I4720
  try
    LoadFromFile(Self.FileName);

    FHeader_Name := GetSystemStoreValue(ssName);
    FHeader_Copyright := GetSystemStoreValue(ssCopyright);
    FHeader_Message := GetSystemStoreValue(ssMessage);

    FVersion := GetSystemStoreValue(ssKeyboardVersion);   // I4701
    if FVersion = '' then
      FVersion := '1.0';
    SetFileVersion(FVersion);

    FKVKFileName := GetSystemStoreValue(ssVisualKeyboard);

    // Compile targets
    FTargets := StringToKeymanTargets(GetSystemStoreValue(ssTargets));
    if ktAny in FTargets then FTargets := AllKeymanTargets;
    if FTargets = [] then FTargets := [ktWindows];

    if Assigned(Project) then   // I4720
    begin
      // Get sub files
      value := GetSystemStoreValue(ssBitmap);
      if (value <> '') then
      begin
        if ExtractFileExt(value) = '' then value := value + '.bmp';
        value := ExpandFileNameClean(Self.FileName, value);
        if Project.Files.IndexOfFileName(value) < 0 then
        begin
          for j := Project.Files.Count - 1 downto 0 do
            if Project.Files[j].Parent = Self then
              Project.Files.Delete(j);

          CreateProjectFile(Project, value, Self);
        end
      end
      else
        for j := Project.Files.Count - 1 downto 0 do
          if Project.Files[j].Parent = Self then
            Project.Files.Delete(j);
    end;
  finally
    Free;
  end;
end;

initialization
  RegisterProjectFileType('.kmn', TkmnProjectFile);
end.

