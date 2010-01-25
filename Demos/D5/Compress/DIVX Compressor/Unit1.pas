unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DSPack, DSUtil, StdCtrls, DirectShow9;

type
  TForm1 = class(TForm)
    FilterGraph: TFilterGraph;
    DivXEncoder: TFilter;
    OpenDialog: TOpenDialog;
    GO: TButton;
    Output: TEdit;
    FileSource: TFilter;
    Label1: TLabel;
    Memo1: TMemo;
    MP3Enc: TFilter;
    procedure GOClick(Sender: TObject);
    procedure FilterGraphDSEvent(sender: TComponent; Event, Param1,
      Param2: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.GOClick(Sender: TObject);
var Filter: IBaseFilter;
    FileSink: IFileSinkFilter;
    CaptureGraph: ICaptureGraphBuilder2;
    DivXFilter: IBaseFilter;
    FileSourceFilter: IFileSourceFilter;
    SourceFilter: IBaseFilter;
begin
  FilterGraph.Active := true;
  FilterGraph.QueryInterface(ICaptureGraphBuilder2, CaptureGraph);
  DivXEncoder.QueryInterface(IBaseFilter, DivXFilter);
  CaptureGraph.SetOutputFileName(MEDIASUBTYPE_Avi, StringToOleStr(Output.Text), Filter, FileSink);
  ShowFilterPropertyPage(Self.Handle, DivXFilter, ppVFWCompConfig);
  OpenDialog.Title := 'Select Video File';
  if OpenDialog.Execute then
  begin
    FileSource.QueryInterface(IFileSourceFilter, FileSourceFilter);
    FileSourceFilter.Load(StringToOleStr(OpenDialog.FileName), nil);
    FileSource.QueryInterface(IBaseFilter, SourceFilter);
    CaptureGraph.RenderStream(nil, nil, SourceFilter, DivXFilter, Filter);
    CaptureGraph.RenderStream(nil, nil, SourceFilter, DivXFilter, Filter);
    FilterGraph.Play;
  end;
end;

procedure TForm1.FilterGraphDSEvent(sender: TComponent; Event, Param1,
  Param2: Integer);
begin
  memo1.Lines.Add(GetEventCodeDef(Event))
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FilterGraph.ClearGraph;
  FilterGraph.Active := false;
end;

end.
