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
    RzSpacer1: TRzSpacer;
    btnAdd: TRzToolButton;
    btnRemove: TRzToolButton;
    procedure btnAddClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnReloadClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    _arquivos: TStringList;
    procedure LoadFiles;
    procedure LoadValues; overload;
    procedure LoadValues(node: TcxTreeListNode; coluna: Integer; json: ISuperObject); overload;
    procedure SaveValues(node: TcxTreeListNode; coluna: Integer; json: ISuperObject);
    function getName(parentNode: TcxTreeListNode; name: string): string;
    procedure SaveJson;
  public
    procedure Load;
  end;

var
  FormEditor: TFormEditor;

implementation
uses
  suporte.arquivos, System.IniFiles;

{$R *.dfm}

procedure TFormEditor.btnAddClick(Sender: TObject);
var
  text: string;
  node: TcxTreeListNode;
begin
  text := InputBox('Chave', 'Digite o valor da chave a ser adicionada' , '');
  node := TcxTreeListNode.Create(tv);
  node.Texts[0] := text;
  tv.InsertEx(node, tv.FocusedNode);
  tv.SetFocusedNode(node, []);
end;

procedure TFormEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(_arquivos);
end;

procedure TFormEditor.btnReloadClick(Sender: TObject);
begin
  Load();
end;

procedure TFormEditor.btnRemoveClick(Sender: TObject);
begin
  tv.DeleteSelection;
end;

procedure TFormEditor.FormCreate(Sender: TObject);
begin
  _arquivos := TStringList.Create;
  LoadFiles;
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
    f.LoadFromFile(_arquivos[0]);
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

procedure TFormEditor.LoadFiles;
var
  pastaLocales: string;
  nomeArquivo: string;
  ini: TIniFile;
  i: integer;
begin
  ini := TIniFile.Create('config.ini');
  try
    pastaLocales := ini.ReadString('config', 'pasta-locale', 'D:\Projetos\Colibri-Master\src\frontend\locales\');
    nomeArquivo := ini.ReadString('config', 'arquivo', 'translation.json');
    arquivos.ListarPastas(pastaLocales, _arquivos);
    for I := 0 to _arquivos.Count - 1 do
      _arquivos[i] := _arquivos[i] + '\' + nomeArquivo;
  finally
    FreeAndNil(ini);
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
      tv.Columns[i].Caption.Text := ExtractFileName(ExtractFileDir(_arquivos[i-1]));
      f.LoadFromFile(_arquivos[i-1]);
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
      f.SaveToFile(_arquivos[i-1], TEncoding.ANSI);
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
