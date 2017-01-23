unit form.editor;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  cxGraphics,
  cxControls,
  cxLookAndFeels,
  cxLookAndFeelPainters,
  cxStyles,
  cxEdit,
  cxInplaceContainer,
  cxVGrid,
  cxCustomData,
  cxTL,
  cxTextEdit,
  cxTLdxBarBuiltInMenu,
  cxClasses,
  RzButton,
  Vcl.ExtCtrls,
  RzPanel,
  suporte.superobj, Vcl.ImgList;

type
  TFormEditor = class(TForm)
    tv: TcxTreeList;
    colkey: TcxTreeListColumn;
    sty1: TcxStyleRepository;
    sty2: TcxStyle;
    sty3: TcxStyle;
    RzToolbar1: TRzToolbar;
    btnSave: TRzToolButton;
    il: TImageList;
    btnReload: TRzToolButton;
    procedure FormDestroy(Sender: TObject);
    procedure btnReloadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    _arquivos: TStringList;
    procedure LoadValues; overload;
    procedure LoadValues(node: TcxTreeListNode; coluna: Integer; json: ISuperObject); overload;
    procedure SaveValues(node: TcxTreeListNode; coluna: Integer; json: ISuperObject);
    function getName(parentNode: TcxTreeListNode; name: string): string;
    procedure SaveJson;
  public
    const pastaLocales: string = 'D:\Projetos\Colibri-Master\src\frontend\locales\';
    const TRANSLATION = 'translation.json';
    procedure Load;
  end;

var
  FormEditor: TFormEditor;

implementation
uses
  suporte.arquivos;

{$R *.dfm}

procedure TFormEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(_arquivos);
end;

procedure TFormEditor.btnReloadClick(Sender: TObject);
begin
  Load();
end;

procedure TFormEditor.FormCreate(Sender: TObject);
begin
  _arquivos := TStringList.Create;
  arquivos.ListarPastas(pastaLocales, _arquivos);
  Load();
end;


function TFormEditor.getName(parentNode: TcxTreeListNode; name: string): string;
begin
  result := name;
  while Assigned(parentNode) and (parentNode.Texts[0] <> '') do
  begin
    result := parentNode.Texts[0] + '.' + Result;
    parentNode := parentNode.Parent;
  end;
end;

procedure TFormEditor.Load;
var f: TStringList;
  json, i: ISuperObject;

  function addNode(parentNode: TcxTreeListNode; obj: ISuperObject): TcxTreeListNode;
  var i: ISuperObject;
    name: string;
  begin
    result := tv.AddChild(parentnode);
    Result.Texts[0] := obj.AsString;
    name := getName(parentNode, obj.AsString);
    if json[name].IsType(stObject) then
      for i in json[name].AsObject.GetNames do
        addNode(Result, i);
  end;

begin
  f := TStringList.Create;
  try
    f.LoadFromFile(_arquivos[0]+ '\' +TRANSLATION);
    json := SO(f.Text);
  finally
    FreeAndNil(f);
  end;

  tv.BeginUpdate;
  try
    tv.Clear;
    tv.DeleteAllColumns;
    tv.CreateColumn();
    for i in json.asObject.getnames do
      addNode(nil, i).Expand(True);

    LoadValues;
  finally
    tv.EndUpdate;
  end;
end;

procedure TFormEditor.LoadValues(node: TcxTreeListNode; coluna: Integer; json: ISuperObject);
var
  i: Integer;
  name: string;
begin
  name := getName(node.Parent, node.Texts[0]);
  if (name <> '') and json[name].IsType(stString) then
    node.Texts[coluna] := json.S[name];
  for i := 0 to node.Count - 1 do
    LoadValues(node.Items[i], coluna, json);
end;

procedure TFormEditor.btnSaveClick(Sender: TObject);
begin
  SaveJson();
end;

procedure TFormEditor.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = ord('S')) and (ssCtrl in Shift) then
    SaveJson();
end;

procedure TFormEditor.LoadValues;
var
  f: TStringList;
  json: ISuperObject;
  i: Integer;
begin
  f := TStringList.Create;
  try
    for i := 1 to _arquivos.Count do
    begin
      tv.CreateColumn();
      tv.Columns[i].Caption.Text := ExtractFileName(_arquivos[i-1]);
      f.LoadFromFile(_arquivos[i-1]+'\'+TRANSLATION);
      json := SO(f.Text);
      LoadValues(tv.Root, i, json);
    end;
  finally
    FreeAndNil(f);
  end;
end;

procedure TFormEditor.SaveJson;
var
  json: ISuperObject;
  f: TStringList;
  i: Integer;
begin
  f := TStringList.Create;
  try
    for i := 1 to _arquivos.Count do
    begin
      json := SO();
      SaveValues(tv.Root, i, json);
      f.Text := json.AsJSon(True);
      f.WriteBOM := True;
      f.SaveToFile(_arquivos[i-1] + '\'+TRANSLATION, TEncoding.ANSI);
    end;
  finally
    FreeAndNil(f);
  end;
end;


procedure TFormEditor.SaveValues(node: TcxTreeListNode; coluna: Integer; json: ISuperObject);
var i:Integer;
begin
  if not node.HasChildren then
    json.S[getName(node.Parent, node.Texts[0])] := node.Texts[coluna]
  else
    for i := 0 to node.Count - 1 do
      SaveValues(node.Items[i], coluna, json);
end;

end.
