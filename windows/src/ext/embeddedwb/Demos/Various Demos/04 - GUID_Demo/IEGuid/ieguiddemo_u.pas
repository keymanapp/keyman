{*******************************************************}
{              EmbeddedWB - IEGuid Demo                 }
{                    by  Per larsen                     }
{                       Enjoy!                          }
{   UPDATES:                                            }
{               http://www.bsalsa.com                   }
{*******************************************************}

unit ieguiddemo_u;

interface

uses
  Activex, IEGuid, Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, clipbrd, Mshtml_Ewb, StdCtrls, ComCtrls, OleCtrls, SHDocVw_EWB, EmbeddedWB,
  ExtCtrls, EwbCore;

type
  TForm1 = class(TForm)
    TreeView1: TTreeView;
    ListBox1: TListBox;
    ListBox2: TListBox;
    ListBox3: TListBox;
    GroupBox1: TGroupBox;
    IUnknownBox: TCheckBox;
    IdispatchBox: TCheckBox;
    IdispatchExBox: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ServiceGroup: TRadioGroup;
    DispInterfacesBox: TCheckBox;
    Label4: TLabel;
    Label6: TLabel;
    EmbeddedWB1: TEmbeddedWB;
    Label7: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox2DblClick(Sender: TObject);
    procedure UpdateLists;
    procedure FormShow(Sender: TObject);
    procedure ServiceGroupClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MyGuids: TIEGuid;
  Form1: TForm1;
  IList: TInterfaceList;
  EmbeddedWBNode, DocumentNode: TTreeNode;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  if not FileExists('IEGuidList.txt') then
    TreeView1.SaveToFile('IEGuidList.txt'); // dummy
  MyGuids := TIEGuid.Create('IEGuidList.txt');
end;

procedure TForm1.FormShow(Sender: TObject);
var
  IDisp: IDispatch;
  x: Integer;
  sl: TStringlist;
  HTMLDoc4: IHTMLDocument4;
begin
  EmbeddedWB1.AssignEmptyDocument;
  IList := TInterfaceList.Create;
  sl := TStringlist.Create;
  MyGuids.GetPropertylist(Embeddedwb1.Document, sl);
  TreeView1.ReadOnly := True;
  with TreeView1.Items do
  begin
    EmbeddedwbNode := Add(nil, 'EmbeddedWB');
    IList.Add(EmbeddedWB1);
    AddChild(EmbeddedWBNode, 'Application');
    IList.Add(EmbeddedWB1.Application);
    AddChild(EmbeddedWBNode, 'Document');
    IList.Add(EmbeddedWB1.Document);
    DocumentNode := TreeView1.Items[2];
    HTMLDoc4 := EmbeddedWB1.Doc4;
    if HTMLDoc4 <> nil then
      for x := 0 to sl.Count - 1 do
      begin
        try
          if (sl[x] <> 'parentDocument') and (sl[x] <> 'constructor') then
          begin
            IDisp := MyGuids.GetDispatchFromName(HTMLDoc4, sl[x]);
            AddChild(DocumentNode, sl[x]);
            IList.Add(IDisp);
          end;
        except
          caption := sl[x];
          Break;
        end;
      end;
  end;
  TreeView1.Items[0].Expand(True);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  IList.Free;
  MyGuids.Free;
end;

procedure TForm1.UpdateLists;
var
  SID: string;
begin
  if ServiceGroup.ItemIndex = 1 then SID := 'SID_STopLevelBrowser' else sid := '';
  Listbox1.Clear;
  Listbox2.Clear;
  Listbox3.Clear;
  MyGuids.GetInterfacesEx(IList[Treeview1.Selected.AbsoluteIndex], listbox1.Items,
    IUnknownBox.Checked, IdispatchBox.Checked, IdispatchExBox.Checked, DispInterfacesBox.Checked);
  MyGuids.GetServices(IList[Treeview1.Selected.AbsoluteIndex], SID, listbox2.Items);
  MyGuids.GetConnectionpoints(IList[Treeview1.Selected.AbsoluteIndex], listbox3.Items, IDispatchBox.Checked);
end;

procedure TForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  UpdateLists;
end;

procedure TForm1.ListBox1DblClick(Sender: TObject);
var
  S: string;
begin
  S := 'var' + #13 + 'ID : ' + Listbox1.Items[Listbox1.ItemIndex] + ';' + #13#13;

  if treeview1.Selected.Parent = DocumentNode then
    S := S + 'If SUCCEEDED((EmbeddedWB1.Document as IHTMLDocument4).' + Treeview1.Selected.Text +
      '.QueryInterface(' + Listbox1.Items[Listbox1.ItemIndex] + ', ID)) then' else

    if Treeview1.Selected.Text = 'Document' then
      S := S + 'If SUCCEEDED((EmbeddedWB1.Document as IHTMLDocument4).QueryInterface(' + Listbox1.Items[Listbox1.ItemIndex] +
        ', ID))' else

      if Treeview1.Selected.Text = 'Application' then
        S := S + 'If SUCCEEDED(EmbeddedWB1.Application.QueryInterface(' + Listbox1.Items[Listbox1.ItemIndex] + ', ID)) then' else

        if Treeview1.Selected.Text = 'EmbeddedWB' then
          S := S + 'If SUCCEEDED(EmbeddedWB1.QueryInterface(' + Listbox1.Items[Listbox1.ItemIndex] + ', ID)) then';

  ClipBoard.SetTextBuf(Pchar(s));
  Showmessage('Copied to Clipboard:' + #13#13#13 + S + #13);
end;

procedure TForm1.ListBox2DblClick(Sender: TObject);
var
  S, SID: string;
begin
  if ServiceGroup.ItemIndex = 1 then SID := 'SID_STopLevelBrowser' else sid := Listbox2.Items[Listbox2.ItemIndex];
  S := 'var' + #13 + 'ID : ' + Listbox2.Items[Listbox2.ItemIndex] + ';' + #13#13;
  if treeview1.Selected.Parent = DocumentNode then
    S := s + 'If SUCCEEDED(((IHTMLDocument4(EmbeddedWB1.Document).' + Treeview1.Selected.Text +
      ' as IServiceProvider).QueryService(' + SID + ', ' + Listbox2.Items[Listbox2.ItemIndex] + ',ID))) then' else
    if Treeview1.Selected.Text = 'Document' then
      S := s + 'If SUCCEEDED((EmbeddedWB1.Document as IServiceProvider).QueryService(' + SID + ', ' + Listbox2.Items[Listbox2.ItemIndex] + ',ID)) then' else
      if Treeview1.Selected.Text = 'Application' then
        S := s + 'If SUCCEEDED((EmbeddedWB1.Application as IServiceProvider).QueryService(' + SID + ', ' + Listbox2.Items[Listbox2.ItemIndex] + ',ID)) then' else
        if Treeview1.Selected.Text = 'EmbeddedWB' then
          S := s + 'If SUCCEEDED((EmbeddedWB1 as IServiceProvider).QueryService(' + SID + ', ' +
            Listbox2.Items[Listbox2.ItemIndex] + ',ID)) then';
  ClipBoard.SetTextBuf(Pchar(s));
  Showmessage('Copied to Clipboard:' + #13#13#13 + S + #13);
end;

procedure TForm1.ServiceGroupClick(Sender: TObject);
begin
  UpdateLists;
end;

end.

