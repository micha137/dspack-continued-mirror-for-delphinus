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
  Color := FormDVDPlayer.DSVideoWindowEx1.ColorKey;
  Shape1.Brush.Color := FormDVDPlayer.DSVideoWindowEx1.ColorKey;
  Left := FormDVDPlayer.DSVideoWindowEx1.ClientOrigin.X + (FormDVDPlayer.DSVideoWindowEx1.Width div 2) - (Width div 2);
  Top := FormDVDPlayer.DSVideoWindowEx1.ClientOrigin.Y + (FormDVDPlayer.DSVideoWindowEx1.Height div 2) - (Height div 2);
  GetValues;
end;

procedure TColorControlForm.Button1Click(Sender: TObject);
begin
  FormDVDPlayer.DSVideoWindowEx1.ColorControl.RestoreDefault;
end;

procedure TColorControlForm.TrackBar1Change(Sender: TObject);
begin
  FormDVDPlayer.DSVideoWindowEx1.ColorControl.Brightness := TrackBar1.Position;
end;

procedure TColorControlForm.TrackBar2Change(Sender: TObject);
begin
  FormDVDPlayer.DSVideoWindowEx1.ColorControl.Contrast := TrackBar2.Position;
end;

procedure TColorControlForm.TrackBar3Change(Sender: TObject);
begin
  FormDVDPlayer.DSVideoWindowEx1.ColorControl.Hue := TrackBar3.Position;
end;

procedure TColorControlForm.TrackBar4Change(Sender: TObject);
begin
  FormDVDPlayer.DSVideoWindowEx1.ColorControl.Saturation := TrackBar4.Position;
end;

procedure TColorControlForm.CheckBox1Click(Sender: TObject);
begin
  FormDVDPlayer.DSVideoWindowEx1.ColorControl.ColorEnable := CheckBox1.Checked;
end;

procedure TColorControlForm.SpeedButton1Click(Sender: TObject);
begin
  FormDVDPlayer.DSVideoWindowEx1.ColorControl.RestoreDefault;
  GetValues;
end;

procedure TColorControlForm.GetValues;
Begin
  CheckBox1.Checked := FormDVDPlayer.DSVideoWindowEx1.ColorControl.ColorEnable;
  TrackBar4.Position := FormDVDPlayer.DSVideoWindowEx1.ColorControl.Saturation;
  TrackBar3.Position := FormDVDPlayer.DSVideoWindowEx1.ColorControl.Hue;
  TrackBar2.Position := FormDVDPlayer.DSVideoWindowEx1.ColorControl.Contrast;
  TrackBar1.Position := FormDVDPlayer.DSVideoWindowEx1.ColorControl.Brightness;
End;

end.
