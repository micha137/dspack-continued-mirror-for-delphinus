unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, OleServer, dspack, StdCtrls, DirectShow9,
  DSUtil, ComCtrls;

type
  TForm1 = class(TForm)
    CaptureGraph: TFilterGraph;
    Button1: TButton;
    OpenDialog: TOpenDialog;
    debug: TMemo;
    Label1: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    Button2: TButton;
    SaveDialog: TSaveDialog;
    Label2: TLabel;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    ListBox3: TListBox;
    Button6: TButton;
    Button7: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Button8: TButton;
    Label9: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure CaptureGraphDSEvent(sender: TComponent; Event, Param1,
      Param2: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  SourceFile  : IBaseFilter;
  multiplexer : IBaseFilter;
  DestFile    : IFileSinkFilter;
  CapFilters  : TSysDevEnum;
  AudFilters  : TSysDevEnum;
  CompFilter  : TFilterList;

implementation

{$R *.dfm}

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CompFilter.Free;
  CaptureGraph.ClearGraph;
  CaptureGraph.Active := false;
  CapFilters.Free;
  AudFilters.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  EnumMT: TEnumMediaType;
  i: integer;
  GraphBuilder: IGraphBuilder;
begin
  if OpenDialog.Execute then
  begin
    CaptureGraph.ClearGraph;
    CompFilter.Clear;
    ListBox2.Clear;
    label2.Caption := '';
    Label1.Caption := OpenDialog.FileName;
    debug.Clear;
    EnumMT := TEnumMediaType.Create(OpenDialog.FileName);
    if EnumMT.Count > 0 then
      for i := 0 to EnumMT.Count - 1 do
        debug.lines.Add(EnumMT.MediaDescription[i]);
    EnumMT.Free;
    CaptureGraph.QueryInterface(IGraphBuilder, GraphBuilder);
    GraphBuilder.AddSourceFilter(StringToOleStr(OpenDialog.FileName),
      StringToOleStr(OpenDialog.FileName), SourceFile);
    debug.Lines.Add('Source filter added')
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  GraphBuilder: ICaptureGraphBuilder2;
begin
  if SaveDialog.Execute then
  begin
    CaptureGraph.QueryInterface(ICaptureGraphBuilder2, GraphBuilder);
    GraphBuilder.SetOutputFileName(MEDIASUBTYPE_Avi,
      StringToOleStr(SaveDialog.FileName), multiplexer, DestFile);
    Label2.Caption := SaveDialog.FileName;
    debug.Lines.Add('Destination filter added');
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var i : integer;
begin
  CompFilter  := TFilterList.Create;
  AudFilters  := TSysDevEnum.Create(CLSID_AudioCompressorCategory);
  CapFilters := TSysDevEnum.create(CLSID_VideoCompressorCategory);
  For i := 0 to CapFilters.CountFilters - 1 do
    ListBox1.Items.Add(CapFilters.Filters[i].FriendlyName);
  For i := 0 to AudFilters.CountFilters - 1 do
    ListBox3.Items.Add(AudFilters.Filters[i].FriendlyName);
end;

procedure TForm1.Button3Click(Sender: TObject);
var GraphBuilder: IGraphBuilder;
begin
  if ListBox1.ItemIndex <> -1 then
  begin
    CompFilter.Add(CapFilters.GetBaseFilter(ListBox1.ItemIndex));
    CaptureGraph.QueryInterface(IGraphBuilder, GraphBuilder);
    GraphBuilder.AddFilter(CompFilter.Last, StringToOleStr(ListBox1.Items.Strings[ListBox1.ItemIndex]));
    ListBox2.Items.Add(ListBox1.Items.Strings[ListBox1.ItemIndex])
  end;

end;

procedure TForm1.Button4Click(Sender: TObject);
var GraphBuilder: IGraphBuilder;
begin
  if ListBox2.ItemIndex <> -1 then
  begin
    CaptureGraph.QueryInterface(IGraphBuilder, GraphBuilder);
    GraphBuilder.RemoveFilter(CompFilter.Items[ListBox2.ItemIndex]);
    CompFilter.Delete(ListBox2.ItemIndex);
    ListBox2.Items.Delete(ListBox2.ItemIndex);
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  i: integer;
  GraphBuilder: ICaptureGraphBuilder2;
begin
  if CompFilter.Count > 0 then
  begin
    CaptureGraph.QueryInterface(ICaptureGraphBuilder2, GraphBuilder);
    for i := 0 to CompFilter.Count - 1 do
      GraphBuilder.RenderStream(nil,nil,SourceFile,CompFilter.Items[i],multiplexer);
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
var GraphBuilder: IGraphBuilder;
begin
  if ListBox3.ItemIndex <> -1 then
  begin
    CompFilter.Add(AudFilters.GetBaseFilter(ListBox3.ItemIndex));
    CaptureGraph.QueryInterface(IGraphBuilder, GraphBuilder);
    GraphBuilder.AddFilter(CompFilter.Last, StringToOleStr(ListBox3.Items.Strings[ListBox3.ItemIndex]));
    ListBox2.Items.Add(ListBox3.Items.Strings[ListBox3.ItemIndex])
  end;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
//  with CaptureGraph as ICaptureGraphBuilder2 do
  CaptureGraph.Play;
end;

procedure TForm1.CaptureGraphDSEvent(sender: TComponent; Event, Param1,
  Param2: Integer);
begin
  debug.Lines.Add(GetEventCodeDef(Event));
end;

end.
