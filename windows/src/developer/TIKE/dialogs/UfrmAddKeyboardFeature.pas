(*
  Name:             UfrmAddKeyboardFeature
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Nov 2014

  Modified Date:    4 Nov 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Nov 2014 - mcdurdin - I4504 - V9.0 - Consolidate the compile action into single command
                    
*)
unit UfrmAddKeyboardFeature;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  KeyboardParser;

type
  TfrmAddKeyboardFeature = class(TForm)
    lbFeature: TListBox;
    cmdOK: TButton;
    cmdCancel: TButton;
    procedure lbFeatureClick(Sender: TObject);
    procedure lbFeatureDblClick(Sender: TObject);
  private
    FKeyboardParser: TKeyboardParser;
    procedure SetKeyboardParser(const Value: TKeyboardParser);
    procedure EnableControls;
    function GetNewFeatureID: TKeyboardParser_FeatureID;
  public
    { Public declarations }
    property KeyboardParser: TKeyboardParser read FKeyboardParser write SetKeyboardParser;
    property NewFeatureID: TKeyboardParser_FeatureID read GetNewFeatureID;
  end;

implementation

uses
  kmxfile,
  kmxfileconsts,
  UKeymanTargets;

{$R *.dfm}

{ TfrmAddKeyboardFeature }

procedure TfrmAddKeyboardFeature.EnableControls;
begin
  cmdOK.Enabled := lbFeature.ItemIndex >= 0;
end;

function TfrmAddKeyboardFeature.GetNewFeatureID: TKeyboardParser_FeatureID;
begin
  Result := TKeyboardParser_FeatureID(Integer(lbFeature.Items.Objects[lbFeature.ItemIndex]));
end;

procedure TfrmAddKeyboardFeature.lbFeatureClick(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmAddKeyboardFeature.lbFeatureDblClick(Sender: TObject);
begin
  if cmdOK.Enabled then
    cmdOK.Click;
end;

procedure TfrmAddKeyboardFeature.SetKeyboardParser(const Value: TKeyboardParser);
var
  s: string;
  kf: TKeyboardParser_FeatureID;
  FTargets: TKeymanTargets;
begin
  FKeyboardParser := Value;
  FTargets := StringToKeymanTargets(FKeyboardParser.GetSystemStoreValue(ssTargets));   // I4504
  if FTargets = [] then
    FTargets := [ktWindows];
  if ktAny in FTargets then
    FTargets := AllKeymanTargets;

  for kf := Low(kf) to High(kf) do
  begin
    if not FKeyboardParser.Features.ContainsKey(kf) then
    begin
      s := KeyboardFeatureName[kf];
      if FTargets * KeyboardFeatureTargets[kf] = [] then
        s := s + ' (not used on target platforms)';
      lbFeature.Items.AddObject(s, Pointer(Integer(kf)));
    end;
  end;

  EnableControls;
end;

end.
