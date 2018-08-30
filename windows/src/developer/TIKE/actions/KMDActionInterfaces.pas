(*
  Name:             KMDActionInterfaces
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    6 Feb 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Initial version
                    14 Sep 2006 - mcdurdin - Recreate IKMDEditActions interface
                    06 Feb 2014 - mcdurdin - I4032 - V9.0 - Add support for Redo to Keyman Developer actions
*)
unit KMDActionInterfaces;

interface

uses
  System.Types,

  TextFileFormat;

type
  IKMDEditActions = interface
    ['{271C7917-7BA5-4F19-B78B-C16A81DFE53B}']
    procedure CutToClipboard;
    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    procedure Undo;
    procedure Redo;   // I4032
    procedure SelectAll;
    procedure ClearSelection;
    function CanCut: Boolean;
    function CanCopy: Boolean;
    function CanPaste: Boolean;
    function CanUndo: Boolean;
    function CanRedo: Boolean;   // I4032
    function CanSelectAll: Boolean;
    function CanClearSelection: Boolean;
  end;

  IKMDPrintActions = interface
    ['{C6F05AD6-42A3-404F-944C-904393A51837}']
    //function PageSetup: Boolean;
    function PrintFile: Boolean;
  end;

  IKMDPrintPreviewActions = interface(IKMDPrintActions)
    ['{B5879BC4-7A0C-4E00-AAE3-E2D48489152A}']
    function PrintPreview: Boolean;
  end;

  IKMDViewExpandEditorActions = interface
    ['{680A172E-A221-4CDA-B68C-1E2AF8487510}']
    procedure ExpandContractEditor;
    function IsEditorExpanded: Boolean;
  end;

  IKMDSearchActions = interface(IKMDEditActions)
    ['{5268F7B2-A28E-4471-A708-41BFC968696F}']
    procedure EditFind;
    procedure EditFindNext;
    procedure EditReplace;
    function CanEditFind: Boolean;
    function CanEditFindNext: Boolean;
  end;

  IKMDTextEditorActions = interface(IKMDSearchActions)
    ['{118B5CF5-7FED-4669-9A51-7DC58BF571D4}']
    function GetText: string;
    procedure SetText(const Value: string);
    function GetSelectedRow: Integer;
    function GetSelectedCol: Integer;
    function GetSelectedRange: TRect;
    function GetEditorFormat: TEditorFormat;
    procedure ReplaceSelection(ARange: TRect; const ANewText: string);

    property EditorFormat: TEditorFormat read GetEditorFormat;
    property SelectedRow: Integer read GetSelectedRow;
    property SelectedCol: Integer read GetSelectedCol;
    property SelectedRange: TRect read GetSelectedRange;
    property Text: string read GetText write SetText;
  end;

implementation

end.
