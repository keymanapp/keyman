object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PlusMemo1: TPlusMemo
    Left = 0
    Top = 0
    Width = 635
    Height = 299
    Cursor = crIBeam
    Alignment = taLeftJustify
    BorderStyle = bsNone
    HideSelection = False
    Lines.Strings = (
      'unit Unit3;'
      ''
      'interface'
      ''
      'uses'
      
        '  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Varia' +
        'nts, System.Classes, Vcl.Graphics,'
      
        '  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, PlusMemo, PMSupport, Ext' +
        'Hilit, OOPHilit;'
      ''
      'type'
      '  TForm3 = class(TForm)'
      '    PlusMemo1: TPlusMemo;'
      '    procedure FormCreate(Sender: TObject);'
      '    procedure PlusMemo1Change(Sender: TObject);'
      '  private'
      '  public'
      '  end;'
      ''
      'var'
      '  Form3: TForm3;'
      ''
      'implementation'
      ''
      '{$R *.dfm}'
      ''
      'procedure TForm3.FormCreate(Sender: TObject);'
      'begin'
      '  plusmemo1.Lines.LoadFromFile('#39'..\..\unit3.pas'#39');'
      'end;'
      ''
      'procedure TForm3.PlusMemo1Change(Sender: TObject);'
      'begin'
      '  Caption := Caption + '#39'-'#39';'
      'end;'
      ''
      'end.'
      '')
    ScrollBars = ssBoth
    WordWrap = False
    OnChange = PlusMemo1Change
    AltFont.Charset = ANSI_CHARSET
    AltFont.Color = clWindowText
    AltFont.Height = -11
    AltFont.Name = 'Lucida Console'
    AltFont.Style = []
    BackgroundBmp.Data = {07544269746D617000000000}
    CaretWidth = 1
    ColumnWrap = 0
    DisplayOnly = False
    EnableHotKeys = False
    EndOfTextMark.Color = clRed
    Justified = False
    LineHeight = 0
    Options = [pmoKeepColumnPos, pmoPutExtraSpaces, pmoInsertKeyActive, pmoWideOverwriteCaret, pmoAutoScrollBars, pmoAutoIndent, pmoWindowsSelColors, pmoFullLineSelect, pmoBlockSelection, pmoKeepParBackgnd]
    Overwrite = False
    RightLinePen.Color = clGray
    RightLinePen.Style = psDot
    SelBackColor = clHighlight
    SelTextColor = clHighlightText
    Separators = #9#10#13' $%&(),./:;<=>'
    ShowEndParSelected = False
    SpecUnderline.Color = clRed
    TabStops = 4
    UndoMaxSpace = 65536
    UpdateMode = umOnNeed
    Version = 'v7.1  Professional ed.'
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    TabStop = True
  end
end
