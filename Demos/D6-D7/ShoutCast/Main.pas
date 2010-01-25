unit Main;

   (*********************************************************************
    * The contents of this file are used with permission, subject to    *
    * the Mozilla Public License Version 1.1 (the "License"); you may   *
    * not use this file except in compliance with the License. You may  *
    * obtain a copy of the License at                                   *
    * http://www.mozilla.org/MPL/MPL-1.1.html                           *
    *                                                                   *
    * Software distributed under the License is distributed on an       *
    * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or    *
    * implied. See the License for the specific language governing      *
    * rights and limitations under the License.                         *
    *                                                                   *
    * (C) 2004 Martin Offenwanger: coder@dsplayer.de                    *
    *********************************************************************)
{
@author(Martin Offenwanger: coder@dsplayer.de)
@created(Apr 22, 2004)
@lastmod(May 13, 2005)
}

interface

uses
  Windows, Messages, SysUtils, Graphics, Controls, Forms, Dialogs, DirectShow9,
  ActiveX, StdCtrls, DSUtil, ExtCtrls, ComCtrls, Buttons, TabNotBk, Classes,
  XPMan, Definitions, Filter;

type
  TForm1 = class(TForm, IAsyncExCallBack)
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    CheckBox1: TCheckBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    GroupBox9: TGroupBox;
    Button6: TButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    Label7: TLabel;
    Label2: TLabel;
    Label8: TLabel;
    Label3: TLabel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label4: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Edit4: TEdit;
    ListBox1: TListBox;
    TmrCloseApp: TTimer;
    TmrNilAll: TTimer;
    TmrOpenUrl: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TmrNilAllTimer(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure TmrOpenUrlTimer(Sender: TObject);
    procedure TmrCloseAppTimer(Sender: TObject);
  private
    m_ripdir: string;
    m_BTAsycEx: IBaseFilter;
    m_GraphBuilder: IGraphBuilder;
    m_MediaControl: IMediaControl;
    m_Mp3Dec: IBaseFilter;
    m_BTAsyncExControl: IAsyncExControl;
    m_Mpeg1Splitter: IBaseFilter;
    m_Pin: IPin;
    procedure NilAll();
    procedure OpenURL();
    procedure refreshripstream();
    function AsyncExFilterState(Buffering: LongBool; PreBuffering: LongBool;
      Connecting: LongBool; Playing: LongBool;
      BufferState: integer): HRESULT; stdcall;
    function AsyncExICYNotice(IcyItemName: PChar;
      ICYItem: PChar): HRESULT; stdcall;
    function AsyncExMetaData(Title: PChar; URL: PChar): HRESULT; stdcall;
    function AsyncExSockError(ErrString: PChar): HRESULT; stdcall;
  public
  end;

var
  Form1: TForm1;

const
  CLSID_Mpeg1Split: TGUID = '{336475D0-942A-11CE-A870-00AA002FEAB5}';
  CLSID_Mp3Dec: TGUID = '{38BE3000-DBF4-11D0-860E-00A024CFEF6D}';

implementation

{$R *.dfm}

procedure TForm1.NilAll();
begin
  if Assigned(m_MediaControl) then
    m_MediaControl.Stop;
  if Assigned(m_BTAsyncExControl) then begin
    m_BTAsyncExControl.FreeCallback;
    m_BTAsyncExControl := nil;
  end;
  ListBox1.Enabled := false;
  Button6.Enabled := false;
  if Assigned(m_BTAsycEx) then
    m_BTAsycEx := nil;
  ListBox1.Enabled := true;
  Button6.Enabled := true;
  if Assigned(m_Pin) then
    m_Pin := nil;
  if Assigned(m_MediaControl) then
    m_MediaControl := nil;
  if Assigned(m_GraphBuilder) then
    m_GraphBuilder := nil;
  button6.Caption := 'connect';
end;

procedure TForm1.OpenURL();
begin
  button6.Caption := 'disconnect';
  CheckDSError(CoCreateInstance(TGUID(CLSID_FilterGraph), nil, CLSCTX_INPROC,
    TGUID(IID_IGraphBuilder), m_GraphBuilder));
  CheckDSError(m_GraphBuilder.QueryInterface(IID_IMediaControl, m_MediaControl));
  m_BTAsycEx := TAsyncEx.Create;
  CheckDSError(CoCreateInstance(CLSID_Mp3Dec, nil, CLSCTX_INPROC,
    IID_IBaseFilter, m_Mp3Dec));
  CheckDSError(CoCreateInstance(CLSID_Mpeg1Split, nil, CLSCTX_INPROC,
    IID_IBaseFilter, m_Mpeg1Splitter));
  CheckDSError(m_GraphBuilder.AddFilter(m_Mpeg1Splitter, 'MPEG1 Splitter'));
  CheckDSError(m_BTAsycEx.QueryInterface(IID_IAsyncExControl,
    m_BTAsyncExControl));
  if assigned(m_BTAsyncExControl) then
    if failed(m_BTAsyncExControl.SetCallBack(self)) then begin
      exit;
    end;
  refreshripstream();
  if assigned(m_BTAsyncExControl) then begin
    if RadioButton3.Checked then
      if failed(m_BTAsyncExControl.SetConnectToURL(PChar(ListBox1.Items[ListBox1.ItemIndex]), TrackBar1.Position * 1000, true)) then begin
        exit;
      end;
    if RadioButton4.Checked then
      if failed(m_BTAsyncExControl.SetConnectToURL(PChar(ListBox1.Items[ListBox1.ItemIndex]), TrackBar1.Position * 1000, false)) then begin
        exit;
      end;
  end;
  if assigned(m_BTAsyncExControl) then
    if failed(m_BTAsyncExControl.SetBuffersize(TrackBar2.Position * 1000)) then
      exit;
  if assigned(m_BTAsycEx) then
    if failed(m_BTAsycEx.FindPin(PinID, m_Pin)) then
      exit;
  if assigned(m_GraphBuilder) then
    if failed(m_GraphBuilder.AddFilter(m_BTAsycEx,
      StringToOleStr(FilterID))) then
      exit;
  if assigned(m_Mp3Dec) then
    if failed(m_GraphBuilder.AddFilter(m_Mp3Dec,
      StringToOleStr('MP3 Dec'))) then
      exit;
  if assigned(m_GraphBuilder) then
    if failed(m_GraphBuilder.Render(m_Pin)) then
      exit;
  if assigned(m_MediaControl) then
    if failed(m_MediaControl.Run) then
      exit;
end;


procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  NilAll;
  TmrCloseApp.Enabled := true;
  { CoUninitialize causes a crash when closing the
    Application while preBuffering }
  // CoUninitialize;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ListBox1.ItemIndex := 0;
  PageControl1.DoubleBuffered := true;
  { CoUninitialize causes a crash when closing the
    Application while preBuffering, however works also without CoInitialize }
  // CoInitialize(nil);
  m_ripdir := GetCurrentDir;
  if length(m_ripdir) > 50 then
    label11.Caption := copy(m_ripdir, 1, 50) + '...'
  else
    label11.Caption := m_ripdir;
  label11.Hint := m_ripdir;
  label11.ShowHint := true;
  label18.Hint := label18.Caption;
  label18.ShowHint := true;
  label19.Hint := label19.Caption;
  label19.ShowHint := true;
  label19.Font.Color := clBlue;
  label19.Font.Style := [fsUnderline];
  label24.Hint := label24.Caption;
  label24.ShowHint := true;
  label25.Hint := label25.Caption;
  label25.ShowHint := true;
  label26.Hint := label26.Caption;
  label26.ShowHint := true;
  label26.Font.Color := clBlue;
  label26.Font.Style := [fsUnderline];
  Label8.Caption := inttostr(TrackBar1.Position) + ' kb';
  Label3.Caption := inttostr(TrackBar2.Position) + ' kb';
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  Label8.Caption := inttostr(TrackBar1.Position) + ' kb';
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  if Button6.Caption = 'connect' then
    OpenURL
  else begin
    Button6.Enabled := false;
    nilall;
    Button6.Enabled := true;
  end;
end;

function TForm1.AsyncExFilterState(Buffering: LongBool; PreBuffering: LongBool;
  Connecting: LongBool; Playing: LongBool;
  BufferState: integer): HRESULT; stdcall;
begin
  if PreBuffering then
    Label6.Caption := '( ' + inttostr(BufferState) + '% )' + ' prebuffering....';
  if Buffering then begin
    Label6.Caption := '( ' + inttostr(BufferState) + '% )' + ' buffering....';
  end;
  if Connecting then
    Label6.Caption := 'connecting....';
  if Playing then begin
    Label6.Caption := 'playing....';
  end;
  if not Buffering and not PreBuffering and not Connecting and not Playing then begin
    Label6.Caption := 'N/A';
    Label18.Caption := 'N/A';
    Label19.Caption := 'N/A';
  end;
  Result := S_OK;
end;

function TForm1.AsyncExICYNotice(IcyItemName: PChar;
  ICYItem: PChar): HRESULT; stdcall;
const // ICY Item Names
  c_ICYMetaInt = 'icy-metaint:';
  c_ICYName = 'icy-name:';
  c_ICYGenre = 'icy-genre:';
  c_ICYURL = 'icy-url:';
  c_ICYBitrate = 'icy-br:';
  c_ICYError = 'icy-error:';
begin
  if IcyItemName = c_ICYError then begin
    ListBox1.Enabled := false;
    Button6.Enabled := false;
    showmessage(copy(ICYItem, 1, length(ICYItem)));
    TmrNilAll.Enabled := true;
  end;
  if IcyItemName = c_ICYName then begin
    if length(ICYItem) > 39 then
      label24.Caption := copy(ICYItem, 1, 75) + '...'
    else
      label24.Caption := copy(ICYItem, 1, length(ICYItem));
    label24.Hint := copy(ICYItem, 1, length(ICYItem));
  end;
  if IcyItemName = c_ICYGenre then begin
    if length(ICYItem) > 39 then
      label25.Caption := copy(ICYItem, 1, 75) + '...'
    else
      label25.Caption := copy(ICYItem, 1, length(ICYItem)); ;

    label25.Hint := copy(ICYItem, 1, length(ICYItem));
  end;
  if IcyItemName = c_ICYURL then begin
    if length(ICYItem) > 30 then
      label26.Caption := copy(ICYItem, 1, 75) + '...'
    else
      label26.Caption := copy(ICYItem, 1, length(ICYItem));
    label26.Hint := copy(ICYItem, 1, length(ICYItem));
  end;
  if IcyItemName = c_ICYBitrate then
    label27.Caption := copy(ICYItem, 1, length(ICYItem));
  Result := S_OK;
end;

function TForm1.AsyncExSockError(ErrString: PChar): HRESULT; stdcall;
begin
  ListBox1.Enabled := false;
  Button6.Enabled := false;
  showmessage('can not connect to URL'#13#10#13#10 +
    'Reason:'#13#10 + copy(ErrString, 1, length(ErrString)));
  //NilAll;
  TmrNilAll.Enabled := true;
  Result := S_OK;
end;

function TForm1.AsyncExMetaData(Title: PChar; URL: PChar): HRESULT; stdcall;
begin
  if length(Title) > 50 then
    Label18.Caption := copy(Title, 1, 45) + '...'
  else
    Label18.Caption := copy(Title, 1, length(Title));
  Label18.Hint := copy(Title, 1, length(Title));
  if length(URL) > 50 then
    Label19.Caption := copy(URL, 1, 45) + '...'
  else
    Label19.Caption := copy(URL, 1, length(URL));
  Label19.Hint := copy(URL, 1, length(URL));
  Result := S_OK;
end;

procedure TForm1.refreshripstream();
begin
  if CheckBox1.Checked then begin
    if assigned(m_BTAsyncExControl) then
      m_BTAsyncExControl.SetRipStream(true, PChar(m_ripdir), PChar(Edit4.Text));
  end else begin
    if assigned(m_BTAsyncExControl) then
      m_BTAsyncExControl.SetRipStream(false, PChar(m_ripdir), PChar(Edit4.Text));
  end;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  refreshripstream();
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  Label3.Caption := inttostr(TrackBar2.Position) + ' kb';
  if assigned(m_BTAsyncExControl) then
    m_BTAsyncExControl.SetBuffersize(TrackBar2.Position * 1000);
end;

procedure TForm1.ListBox1DblClick(Sender: TObject);
begin
 nilall;
 TmrOpenUrl.Enabled := true;
end;

procedure TForm1.TmrNilAllTimer(Sender: TObject);
begin
  TmrNilAll.Enabled := false;
  nilall;
  ListBox1.Enabled := true;
  Button6.Enabled := true;
end;

procedure TForm1.TmrOpenUrlTimer(Sender: TObject);
begin
 TmrOpenUrl.Enabled := false;
 OpenURL;
end;

procedure TForm1.TmrCloseAppTimer(Sender: TObject);
begin
 Close;
end;

end.

