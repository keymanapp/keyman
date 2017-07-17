unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, ToolWin, Menus,

  JwaVista, JwsclUtils, JwsclSecureUserObjects, JwsclSecurePrivateObjects,
  JwsclTypes, JwsclExceptions, JwsclAcl, JwsclMapping, JwsclSid, JwsclKnownSid,
  JwsclSecureObjects, JwsclResource, JwsclEnumerations,
  JwsclVersion, JwsclConstants, JwsclProcess, JwsclDescriptor, JwsclToken,
  JwsclStrings;


type
  TSecurityData = TJwInterfacedPrivateSecurityInformation;
  TChildData = class;
  TTreeItemData = class;

  TFormMain = class(TForm)
    MainMenu: TMainMenu;
    MainTreeView: TTreeView;
    file1: TMenuItem;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    Splitter1: TSplitter;
    Panel1: TPanel;
    MainListView: TListView;
    Splitter2: TSplitter;
    Panel2: TPanel;
    ACLListView: TListView;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ACLEditButton: TBitBtn;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure MainTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure ToolButton1Click(Sender: TObject);
    procedure MainTreeViewDeletion(Sender: TObject; Node: TTreeNode);
  private
    { Private-Deklarationen }
    function GetSecurityData(const Node : TTreeNode) : TSecurityData; overload;
    function GetSecurityData(const Item : TListItem) : TSecurityData; overload;

    function GetData(const Node : TTreeNode) : TTreeItemData; overload;
    function GetData(const Item : TListItem) : TChildData; overload;

    function AddTreeItem(const Parent : TTreeNode; const Name : String) : TTreeNode;
  public
    { Public-Deklarationen }
  end;


  TChildData = class
  public
    Parent : TTreeItemData;
    Security : IJwPrivateSecurityInformation;
    Name : String;

    constructor Create(aParent : TTreeItemData);
    destructor Destroy; override;
  end;

  TTreeItemData = class
  public
    Parent : TTreeItemData;
    Security : IJwPrivateSecurityInformation;
    Childs   : TList;

    constructor Create(aParent : TTreeItemData);
    destructor Destroy; override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

var TreeItem : TTreeNode;
    ListItem : TListItem;
    I : Integer;

{ TFormMain }

function GetDefaultSecurity : TSecurityData;
begin
  result := TSecurityData.Create();
end;


procedure TFormMain.FormCreate(Sender: TObject);
begin
  JwInitWellKnownSIDs;

  TreeItem := MainTreeView.Items.AddChild(nil,'root');

  TreeItem.Data := TTreeItemData.Create(nil);
end;

procedure TFormMain.MainTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  //
  MainListView.Clear;
  if Assigned(GetData(Node)) and Assigned(GetData(Node).Childs) then
  begin
    for I := 0 to GetData(Node).Childs.Count -1 do
    begin
      ListItem := MainListView.Items.Add;
      ListItem.Data := TChildData(GetData(Node).Childs[i]);
      ListItem.Caption := GetData(ListItem).Name;
    end; 
  end;
end;

function TFormMain.GetSecurityData(const Node: TTreeNode): TSecurityData;
begin
  result := nil;
  if Assigned(Node) and Assigned(Node.Data) then
    result := TSecurityData(TTreeItemData(Node.Data).Security);
end;

function TFormMain.GetSecurityData(const Item: TListItem): TSecurityData;
begin
  result := nil;
  if Assigned(Item) and Assigned(Item.Data) then
    result := TSecurityData(TChildData(Item.Data).Security);
end;

function TFormMain.GetData(const Node: TTreeNode): TTreeItemData;
begin
  result := nil;
  if Assigned(Node) then
    result := TTreeItemData(Node.Data);
end;

function TFormMain.GetData(const Item: TListItem): TChildData;
begin
  result := nil;
  if Assigned(Item) then
    result := TChildData(Item.Data);
end;

procedure TFormMain.ToolButton1Click(Sender: TObject);
var Name : String;
begin
  //
  Name := '2';
 // if not InputQuery('New folder','Foldername',Name) then
 //   exit;
  MainTreeView.Selected := AddTreeItem(MainTreeView.Selected,Name);
end;

procedure TFormMain.MainTreeViewDeletion(Sender: TObject; Node: TTreeNode);
begin
  TTreeItemData(Node.Data).Free;
end;

function TFormMain.AddTreeItem(const Parent: TTreeNode;
  const Name: String): TTreeNode;
begin
  result := MainTreeView.Items.AddChild(Parent, Name);
  result.Data := TTreeItemData.Create(GetData(Parent));
end;


{ TTreeItemData }

constructor TTreeItemData.Create(aParent : TTreeItemData);
var SD : TJwSecurityDescriptor;
begin
  inherited Create;
  Childs := TList.Create;
  Parent := aParent;
  if Assigned(aParent) then
  begin
    aParent.Security.GetSecurity(JwSecurityInformationAllFlags,SD);
    try
      Security := TSecurityData.Create(SD);
    finally
      SD.Free;
    end;
  end
  else
    Security := GetDefaultSecurity;
end;

destructor TTreeItemData.Destroy;
begin
  FreeAndNil(Childs);
  Security._Release;
  Security := nil;
  inherited;
end;

{ TChildData }

constructor TChildData.Create(aParent : TTreeItemData);
begin
  inherited Create;

  Security := GetDefaultSecurity;
end;

destructor TChildData.Destroy;
begin
  Security._Release;
  Security := nil;
  inherited;
end;

end.
