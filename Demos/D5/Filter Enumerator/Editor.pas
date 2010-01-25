unit Editor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DSUTIL, DirectShow9, ComCtrls, DSPack, Menus, BaseFilterEditor;

type
  TFormEditor = class(TForm)
    PageControl1: TPageControl;
    Selector: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    cbCategories: TComboBox;
    lbFilters: TListBox;
    Interfaces: TListBox;
    Label3: TLabel;
    FilterGraph: TFilterGraph;
    Filter: TFilter;
    Pins: TTabSheet;
    lbPins: TListBox;
    Label4: TLabel;
    PinInterfaces: TListBox;
    Label5: TLabel;
    MediaTypes: TListBox;
    PopupMenu: TPopupMenu;
    PropertyPage: TMenuItem;
    VFWDisplay: TMenuItem;
    VFWFormat: TMenuItem;
    VFWSource: TMenuItem;
    VFWConfig: TMenuItem;
    VFWCapture: TMenuItem;
    Config1: TMenuItem;
    VFWAbout: TMenuItem;
    PinMenu: TPopupMenu;
    PinProperty: TMenuItem;
    procedure cbCategoriesChange(Sender: TObject);
    procedure lbFiltersClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PinsShow(Sender: TObject);
    procedure lbPinsClick(Sender: TObject);
    procedure InterfacesDblClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure PinInterfacesDblClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure PropertyPageClick(Sender: TObject);
    procedure VFWDisplayClick(Sender: TObject);
    procedure VFWFormatClick(Sender: TObject);
    procedure VFWSourceClick(Sender: TObject);
    procedure Config1Click(Sender: TObject);
    procedure VFWAboutClick(Sender: TObject);
    procedure PinMenuPopup(Sender: TObject);
    procedure PinPropertyClick(Sender: TObject);
  public
    SysDevEnum: TSysDevEnum;
    PinList: TPinList;
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure FindFilterInterfaces;
  end;

  TDSitf = record
    name : string;
    itf  : TGUID;
  end;


var
  FormEditor: TFormEditor;


implementation
uses activex, comobj;

{$R *.dfm}

  constructor TFormEditor.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    SysDevEnum := TSysDevEnum.Create;
    PinList:= TPinList.Create;
  end;

  destructor  TFormEditor.Destroy;
  begin
    SysDevEnum.Free;
    PinList.Free;
    inherited destroy;
  end;

  procedure TFormEditor.cbCategoriesChange(Sender: TObject);
  var i: integer;
  begin
    lbFilters.Clear;
    Interfaces.Clear;
    SysDevEnum.SelectIndexCategory(cbCategories.ItemIndex);
    if SysDevEnum.CountFilters > 0 then
      for i := 0 to SysDevEnum.CountFilters - 1 do
        lbFilters.Items.Add(SysDevEnum.Filters[i].FriendlyName);
  end;

  procedure TFormEditor.lbFiltersClick(Sender: TObject);
  begin
    Filter.BaseFilter.Moniker := SysDevEnum.GetMoniker(lbFilters.ItemIndex);
    IFilter(Filter).NotifyFilter(foRefresh);
    FindFilterInterfaces;
  end;

  procedure TFormEditor.FormShow(Sender: TObject);
  var
    i, j: integer;
    AMoniker, MyMoniker: IMoniker;
    PropBag: IPropertyBag;
    AVariant: OleVariant;
    CLSID: TGUID;
    Found: boolean;
  begin
    for i := 0 to SysDevEnum.CountCategories - 1 do
      cbCategories.Items.Add(SysDevEnum.Categories[i].FriendlyName);
    Found := false;
    j := 0;
    MyMoniker := Filter.BaseFilter.Moniker;
    if MyMoniker = nil then exit;
    MyMoniker.BindToStorage(nil,nil,IPropertyBag, PropBag);
    if PropBag.Read('CLSID',AVariant,nil) = S_OK then
         CLSID := StringToGUID(AVariant)
    else CLSID := GUID_NULL;
    for i := 0 to SysDevEnum.CountCategories - 1 do
    begin
      SysDevEnum.SelectIndexCategory(i);
      if SysDevEnum.CountFilters > 0 then
        for j := 0 to SysDevEnum.CountFilters - 1 do
        begin
          if IsEqualGUID(CLSID, SysDevEnum.Filters[j].CLSID) then
            begin
              AMoniker := SysDevEnum.GetMoniker(j);
              Found := AMoniker.IsEqual(MyMoniker) = S_OK;
              AMoniker := nil;
            end;
          if Found then Break;
        end;
      if Found then
      begin
        cbCategories.ItemIndex := i;
        cbCategoriesChange(nil);
        lbFilters.ItemIndex := j;
        lbFiltersClick(nil);
        break;
      end;
    end;
    PropBag := nil;
    MyMoniker := nil;
  end;

procedure TFormEditor.FindFilterInterfaces;
var
  i: integer;
  unk: IUnknown;
begin
  Interfaces.Clear;
  if lbFilters.ItemIndex <> -1 then
  try
    with Filter.BaseFilter.CreateFilter do
      for i := 0 to length(DSItfs)-1 do
        if Succeeded(QueryInterface(DSItfs[i].itf, unk)) then
          Interfaces.Items.Add(DSItfs[i].name);
  finally
    unk := nil;
  end
end;

procedure TFormEditor.PinsShow(Sender: TObject);
var
  i: integer;
  PinInfo: TPinInfo;
  BaseF: IBaseFilter;
begin
  lbPins.Clear;
  PinInterfaces.Clear;
  MediaTypes.Clear;
  if Succeeded(Filter.QueryInterface(IBaseFilter, BaseF)) then
  begin
    PinList.Assign(BaseF);
    if PinList.Count > 0 then
      for i := 0 to PinList.Count - 1 do
      begin
        PinInfo := PinList.PinInfo[i];
        case PinInfo.dir of
          PINDIR_INPUT  : lbPins.Items.Add(format('%s (input)',[PinInfo.achName]));
          PINDIR_OUTPUT : lbPins.Items.Add(format('%s (output)',[PinInfo.achName]));
        end;
        PinInfo.pFilter := nil;
      end;
    BaseF := nil;
  end;

end;

procedure TFormEditor.lbPinsClick(Sender: TObject);
var
  i: integer;
  unk: IUnknown;
  EnumMT : TEnumMediaType;
begin
  PinInterfaces.Clear;
  if lbPins.ItemIndex <> -1 then
  try
    with PinList.Items[lbPins.ItemIndex] do
      for i := 0 to length(DSItfs)-1 do
        if Succeeded(QueryInterface(DSItfs[i].itf, unk)) then
          PinInterfaces.Items.Add(DSItfs[i].name);
  finally
    unk := nil;
  end;

  MediaTypes.Clear;
  if lbPins.ItemIndex <> -1 then
  begin
    EnumMT:= TEnumMediaType.Create(PinList.Items[lbPins.ItemIndex]);
    try
      if EnumMT.Count > 0 then
        for i := 0 to EnumMT.Count - 1 do
          MediaTypes.Items.Add(EnumMt.MediaDescription[i]);
    finally
      EnumMT.Free;
    end;
  end;

end;

procedure TFormEditor.InterfacesDblClick(Sender: TObject);
var AFilter: IBaseFilter;
begin
  Filter.QueryInterface(IBaseFilter, AFilter);
  if Interfaces.ItemIndex <> -1 then
  if Interfaces.Items.Strings[Interfaces.ItemIndex] = 'ISpecifyPropertyPages' then
    ShowFilterPropertyPage(Self.Handle, AFilter);
  AFilter := nil;
end;

procedure TFormEditor.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  FilterGraph.ClearGraph;
  FilterGraph.Active := false;
end;


procedure TFormEditor.PinInterfacesDblClick(Sender: TObject);
begin
  if PinInterfaces.ItemIndex <> -1 then
  if PinInterfaces.Items.Strings[PinInterfaces.ItemIndex] = 'ISpecifyPropertyPages' then
    ShowPinPropertyPage(Self.Handle, PinList.Items[lbPins.ItemIndex]);
end;

procedure TFormEditor.PopupMenuPopup(Sender: TObject);
var AFilter: IBaseFilter;
begin
  Filter.QueryInterface(IBaseFilter, AFilter);
  PopupMenu.Items.Items[0].Enabled := false;
  PopupMenu.Items.Items[1].Items[0].Enabled := false;
  PopupMenu.Items.Items[1].Items[1].Enabled := false;
  PopupMenu.Items.Items[1].Items[2].Enabled := false;
  PopupMenu.Items.Items[2].Items[0].Enabled := false;
  PopupMenu.Items.Items[2].Items[1].Enabled := false;
  if lbFilters.ItemIndex = -1 then exit;
  if HaveFilterPropertyPage(AFilter, ppDefault)       then PopupMenu.Items.Items[0].Enabled := true;
  if HaveFilterPropertyPage(AFilter, ppVFWCapFormat)  then PopupMenu.Items.Items[1].Items[0].Enabled := true;
  if HaveFilterPropertyPage(AFilter, ppVFWCapSource)  then PopupMenu.Items.Items[1].Items[1].Enabled := true;
  if HaveFilterPropertyPage(AFilter, ppVFWCapDisplay) then PopupMenu.Items.Items[1].Items[2].Enabled := true;
  if HaveFilterPropertyPage(AFilter, ppVFWCompConfig) then PopupMenu.Items.Items[2].Items[0].Enabled := true;
  if HaveFilterPropertyPage(AFilter, ppVFWCompAbout)  then PopupMenu.Items.Items[2].Items[1].Enabled := true;
  AFilter := nil;
end;

procedure TFormEditor.PropertyPageClick(Sender: TObject);
var AFilter: IBaseFilter;
begin
  Filter.QueryInterface(IBaseFilter, AFilter);
  ShowFilterPropertyPage(Self.Handle, AFilter, ppDefault);
  AFilter := nil;
end;

procedure TFormEditor.VFWDisplayClick(Sender: TObject);
var AFilter: IBaseFilter;
begin
  Filter.QueryInterface(IBaseFilter, AFilter);
  ShowFilterPropertyPage(Self.Handle, AFilter, ppVFWCapDisplay);
  AFilter := nil;
end;

procedure TFormEditor.VFWFormatClick(Sender: TObject);
var AFilter: IBaseFilter;
begin
  Filter.QueryInterface(IBaseFilter, AFilter);
  ShowFilterPropertyPage(Self.Handle, AFilter, ppVFWCapFormat);
  AFilter := nil;
end;

procedure TFormEditor.VFWSourceClick(Sender: TObject);
var AFilter: IBaseFilter;
begin
  Filter.QueryInterface(IBaseFilter, AFilter);
  ShowFilterPropertyPage(Self.Handle, AFilter, ppVFWCapSource);
  AFilter := nil;
end;

procedure TFormEditor.Config1Click(Sender: TObject);
var AFilter: IBaseFilter;
begin
  Filter.QueryInterface(IBaseFilter, AFilter);
  ShowFilterPropertyPage(Self.Handle, AFilter, ppVFWCompConfig);
  AFilter := nil;
end;

procedure TFormEditor.VFWAboutClick(Sender: TObject);
var AFilter: IBaseFilter;
begin
  Filter.QueryInterface(IBaseFilter, AFilter);
  ShowFilterPropertyPage(Self.Handle, AFilter, ppVFWCompAbout);
  AFilter := nil;
end;

procedure TFormEditor.PinMenuPopup(Sender: TObject);
begin
  if lbPins.ItemIndex = -1 then abort;
end;

procedure TFormEditor.PinPropertyClick(Sender: TObject);
begin
  ShowPinPropertyPage(Self.Handle, PinList.Items[lbPins.ItemIndex]);
end;

end.


