unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DSUtil, StdCtrls, DSPack, DirectShow9, Menus, ExtCtrls;

type


  TVideoForm = class(TForm)
    FilterGraph: TFilterGraph;
    VideoWindow: TVideoWindow;
    MainMenu1: TMainMenu;
    Devices: TMenuItem;
    Filter: TFilter;
    Blend1: TMenuItem;
    ext1: TMenuItem;
    Bitmap1: TMenuItem;
    OpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure ext1Click(Sender: TObject);
    procedure Bitmap1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    VMRBitmap: TVMRBitmap;
    procedure OnSelectDevice(sender: TObject);

  end;

var
  VideoForm: TVideoForm;
  SysDev: TSysDevEnum;
implementation

uses Math;

{$R *.dfm}

procedure TVideoForm.FormCreate(Sender: TObject);
var
  i: integer;
  Device: TMenuItem;
begin
  SysDev:= TSysDevEnum.Create(CLSID_VideoInputDeviceCategory);
  if SysDev.CountFilters > 0 then
    for i := 0 to SysDev.CountFilters - 1 do
    begin
      Device := TMenuItem.Create(Devices);
      Device.Caption := SysDev.Filters[i].FriendlyName;
      Device.Tag := i;
      Device.OnClick := OnSelectDevice;
      Devices.Add(Device);
    end;
  VMRBitmap:= TVMRBitmap.Create(VideoWindow);
end;

procedure TVideoForm.OnSelectDevice(sender: TObject);
begin
  FilterGraph.ClearGraph;
  FilterGraph.Active := false;
  Filter.BaseFilter.Moniker := SysDev.GetMoniker(TMenuItem(Sender).tag);
  FilterGraph.Active := true;
  with FilterGraph as ICaptureGraphBuilder2 do
    CheckDSError(RenderStream(@PIN_CATEGORY_PREVIEW , nil, Filter as IBaseFilter, nil, VideoWindow as IbaseFilter));
  FilterGraph.Play;
end;

procedure TVideoForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SysDev.Free;
  FilterGraph.ClearGraph;
  FilterGraph.Active := false;
end;

procedure TVideoForm.FormDestroy(Sender: TObject);
begin
  VMRBitmap.Free;
end;

procedure TVideoForm.ext1Click(Sender: TObject);
begin
  with VMRBitmap, Canvas do
  begin
    LoadEmptyBitmap(300,200,pf24bit, clSilver);
    Source := VMRBitmap.Canvas.ClipRect;
    Options := VMRBitmap.Options + [vmrbSrcColorKey];
    ColorKey := clSilver;
    Brush.Color := clSilver;
    Font.Color := clWhite;
    Font.Style := [fsBold];
    Font.Size := 30;
    Font.Name := 'Arial';
    TextOut(0,0,'Hello Word :)');
    DrawTo(0,0,1,1,0.5);
  end;
end;

procedure TVideoForm.Bitmap1Click(Sender: TObject);
var Bitmap: TBitmap;
begin
  if OpenDialog.Execute then
  Begin
    Bitmap:= TBitmap.Create;
    try
      Bitmap.LoadFromFile(OpenDialog.FileName);
      VMRBitmap.LoadBitmap(Bitmap);
      VMRBitmap.Source := VMRBitmap.Canvas.ClipRect;
      VMRBitmap.DrawTo(0,0,0.5,0.5, 0.5);
    finally
      Bitmap.Free;
    end;
  end;

end;








end.
