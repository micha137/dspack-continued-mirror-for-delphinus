unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DSPack, StdCtrls, Menus;

type
  TMainForm = class(TForm)
    VideoWindow: TVideoWindow;
    FilterGraph: TFilterGraph;
    OpenDialog: TOpenDialog;
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuOpen: TMenuItem;
    procedure MenuOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  MainForm: TMainForm;

implementation
uses Allocator;

{$R *.dfm}

procedure TMainForm.MenuOpenClick(Sender: TObject);
begin
  // Open and play a video file
  if OpenDialog.Execute then
  with FilterGraph do
  begin
    Active := False;
    Active := True;
    RenderFile(OpenDialog.FileName);
    Play;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Set the Allocator
  VideoWindow.SetAllocator(TAllocator, $ACDCACDC);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Cleanup
  FilterGraph.Active := False;
end;

end.
