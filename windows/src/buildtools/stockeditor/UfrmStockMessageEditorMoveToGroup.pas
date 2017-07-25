(*
  Name:             UfrmStockMessageEditorMoveToGroup
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Dec 2006

  Modified Date:    17 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Dec 2006 - mcdurdin - Initial version
                    17 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit UfrmStockMessageEditorMoveToGroup;  // I3306

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, StockMessages;

type
  TfrmStockMessageEditorMoveToGroup = class(TForm)
    lbSections: TListBox;
    cmdOK: TButton;
    cmdCancel: TButton;
  private
    function GetActiveSection: TStockMessageSection;
    procedure SetActiveSection(const Value: TStockMessageSection);
    { Private declarations }
  public
    { Public declarations }
    property ActiveSection: TStockMessageSection read GetActiveSection write SetActiveSection;
  end;

implementation

{$R *.dfm}

{ TForm2 }

function TfrmStockMessageEditorMoveToGroup.GetActiveSection: TStockMessageSection;
begin
  Result := lbSections.Items.Objects[lbSections.ItemIndex] as TStockMessageSection;
end;

procedure TfrmStockMessageEditorMoveToGroup.SetActiveSection(const Value: TStockMessageSection);
begin
  lbSections.ItemIndex := lbSections.Items.IndexOfObject(Value);
end;

end.

