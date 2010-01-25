unit ColorControl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, ComCtrls, ExtCtrls;

type
  TColorControlForm = class(TForm)
    Shape1: TShape;
    Label1: TLabel;
    TrackBar1: TTrackBar;
    Label2: TLabel;
    TrackBar2: TTrackBar;
    Label3: TLabel;
    TrackBar3: TTrackBar;
    Label4: TLabel;
    TrackBar4: TTrackBar;
    CheckBox1: TCheckBox;
    SpeedButton1: TSpeedButton;
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure GetValues;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ColorControlForm: TColorControlForm;

implementation

uses Unit1;

{$R *.dfm}

procedure TColorControlForm.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TColorControlForm.FormShow(Sender: TObject);
begin
  Color := Form1.DSVideoWindowEx1.ColorKey;
  Shape1.Brush.Color := Form1.DSVideoWindowEx1.ColorKey;
  Left := Form1.DSVideoWindowEx1.ClientOrigin.X + (Form1.DSVideoWindowEx1.Width div 2) - (Width div 2);
  Top := Form1.DSVideoWindowEx1.ClientOrigin.Y + (Form1.DSVideoWindowEx1.Height div 2) - (Height div 2);
  Trackbar1.Enabled := Form1.DSVideoWindowEx1.Capabilities.CanControlBrigtness;
  Trackbar1.SliderVisible := Form1.DSVideoWindowEx1.Capabilities.CanControlBrigtness;
  Trackbar2.Enabled := Form1.DSVideoWindowEx1.Capabilities.CanControlContrast;
  Trackbar2.SliderVisible := Form1.DSVideoWindowEx1.Capabilities.CanControlContrast;
  Trackbar3.Enabled := Form1.DSVideoWindowEx1.Capabilities.CanControlHue;
  Trackbar3.SliderVisible := Form1.DSVideoWindowEx1.Capabilities.CanControlHue;
  Trackbar4.Enabled := Form1.DSVideoWindowEx1.Capabilities.CanControlSaturation;
  Trackbar4.SliderVisible := Form1.DSVideoWindowEx1.Capabilities.CanControlSaturation;
  CheckBox1.Enabled := Form1.DSVideoWindowEx1.Capabilities.CanControlColorEnabled;
  GetValues;
end;

procedure TColorControlForm.Button1Click(Sender: TObject);
begin
  Form1.DSVideoWindowEx1.ColorControl.RestoreDefault;
end;

procedure TColorControlForm.TrackBar1Change(Sender: TObject);
begin
  Form1.DSVideoWindowEx1.ColorControl.Brightness := TrackBar1.Position;
end;

procedure TColorControlForm.TrackBar2Change(Sender: TObject);
begin
  Form1.DSVideoWindowEx1.ColorControl.Contrast := TrackBar2.Position;
end;

procedure TColorControlForm.TrackBar3Change(Sender: TObject);
begin
  Form1.DSVideoWindowEx1.ColorControl.Hue := TrackBar3.Position;
end;

procedure TColorControlForm.TrackBar4Change(Sender: TObject);
begin
  Form1.DSVideoWindowEx1.ColorControl.Saturation := TrackBar4.Position;
end;

procedure TColorControlForm.CheckBox1Click(Sender: TObject);
begin
  Form1.DSVideoWindowEx1.ColorControl.ColorEnable := CheckBox1.Checked;
end;

procedure TColorControlForm.SpeedButton1Click(Sender: TObject);
begin
  Form1.DSVideoWindowEx1.ColorControl.RestoreDefault;
  GetValues;
end;

procedure TColorControlForm.GetValues;
Begin
  CheckBox1.Checked := Form1.DSVideoWindowEx1.ColorControl.ColorEnable;
  TrackBar4.Position := Form1.DSVideoWindowEx1.ColorControl.Saturation;
  TrackBar3.Position := Form1.DSVideoWindowEx1.ColorControl.Hue;
  TrackBar2.Position := Form1.DSVideoWindowEx1.ColorControl.Contrast;
  TrackBar1.Position := Form1.DSVideoWindowEx1.ColorControl.Brightness;
End;

end.
