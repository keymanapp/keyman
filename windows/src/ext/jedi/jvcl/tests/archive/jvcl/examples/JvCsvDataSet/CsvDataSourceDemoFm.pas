unit CsvDataSourceDemoFm;

{ Demo by Warren Postma, warrenpstma@hotmail.com }

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, Grids, DBGrids, ExtCtrls, DBCtrls, JvCsvData, StdCtrls,
  JvCsvParse, ComCtrls;


type
  TCsvDataSourceForm = class(TForm)
    DataSource1: TDataSource;
    DBNavigator1: TDBNavigator;
    DBGrid1: TDBGrid;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Button3: TButton;
    JvCsvDataSet1: TJvCsvDataSet;
    JvCsvDataSet1NAME: TStringField;
    JvCsvDataSet1ADDRESS: TStringField;
    JvCsvDataSet1ADDRESS2: TStringField;
    JvCsvDataSet1TELEPHONE: TStringField;
    JvCsvDataSet1AGE: TIntegerField;
    JvCsvDataSet1LASTPHONECALL: TDateTimeField;
    JvCsvDataSet1PRIVATENUMBER: TBooleanField;
    Label2: TLabel;
    ComboBox1: TComboBox;
    RichEdit1: TRichEdit;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CsvDataSourceForm: TCsvDataSourceForm;

implementation

{$R *.dfm}

procedure TCsvDataSourceForm.Button2Click(Sender: TObject);
begin
 // OutputDebugString(PChar(JvCsvDataSet1.GetCsvHeader)); // neat trick. reads first line of file only.

          JvCsvDataSet1.Flush; // flush current contents to disk. okay, maybe it should be called save,
                             // then you wouldn't see jokes in the code about how this component has
                             // been tested, and shown to last for over two thousand flushes. <grin>
end;

procedure TCsvDataSourceForm.Button1Click(Sender: TObject);
begin
   Memo1.Clear;
   JvCsvDataSet1.AssignToStrings(Memo1.Lines);
end;

procedure TCsvDataSourceForm.Button3Click(Sender: TObject);
begin
   JvCsvDataSet1.Active := false;
   JvCsvDataSet1.AssignFromStrings(Memo1.Lines);
   JvCsvDataSet1.Active := true;

end;

procedure TCsvDataSourceForm.FormCreate(Sender: TObject);
begin
   JvCsvDataSet1.Active := true; // ensure it's opened when the app is started.

   // second component tests what happens if you don't set up the CsvDef field

//  JvCsvDataSet2.Active := true; // ensure it's opened when the app is started.
end;

procedure TCsvDataSourceForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
    if DataSource1.State = dsEdit then
         JvCsvDataSet1.Post;
end;

procedure TCsvDataSourceForm.ComboBox1Change(Sender: TObject);
begin
   JvCsvDataSet1LASTPHONECALL.DisplayFormat :=  StrStrip(ComboBox1.Text);
end;

end.
