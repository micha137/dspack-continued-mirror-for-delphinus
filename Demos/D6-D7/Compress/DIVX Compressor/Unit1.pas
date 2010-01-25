unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
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
begin
  FilterGraph.Active := true;
  with FilterGraph as ICaptureGraphBuilder2 do
    begin
      SetOutputFileName(MEDIASUBTYPE_Avi, StringToOleStr(Output.Text), Filter, FileSink);
      ShowFilterPropertyPage(Self.Handle, DivXEncoder as IBaseFilter, ppVFWCompConfig);
      OpenDialog.Title := 'Select Video File';
      if OpenDialog.Execute then
      begin
        with FileSource as IFileSourceFilter do Load(StringToOleStr(OpenDialog.FileName), nil);
        RenderStream(nil, nil, FileSource as IBaseFilter, DivXEncoder as IBaseFilter, Filter);
        RenderStream(nil, nil, FileSource as IBaseFilter, DivXEncoder as IBaseFilter, Filter);
        FilterGraph.Play;
      end;
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
