unit prop;

interface

uses
  Windows, DirectShow9, BaseClass, ActiveX, DXSUtil, Messages;

const
  CLSID_NullIPPropertyPage: TGuid = '{40953494-25BD-40CC-879B-1CCC8B13F70B}';

type
  TNullIPProperties = class(TBCBasePropertyPage)
  private
    FListBox: THandle;
    FPin: IPin;
    procedure UpdateListbox;
  public
    constructor CreateFromFactory(Factory: TBCClassFactory; const Controller: IInterface); override;
    function OnReceiveMessage(hwndDlg: Cardinal; uMsg: Cardinal; wParam: Integer; lParam: Integer): Integer; override;
    function OnConnect(Unknown: IInterface): HRESULT; override;
    function OnDisconnect: HRESULT; override;
  end;

implementation

uses
  main;

const
  IDC_LISTBOX = 1000;

constructor TNullIPProperties.CreateFromFactory(Factory: TBCClassFactory; const Controller: IInterface);
begin
  Create(Factory.Name, Controller, 101, 'Property Editor');
end;

function TNullIPProperties.OnReceiveMessage(hwndDlg: Cardinal; uMsg: Cardinal; wParam: Integer; lParam: Integer): Integer;
begin
  case uMsg of
    WM_INITDIALOG:
    begin
      FListBox := GetDlgItem(hwndDlg, IDC_LISTBOX);
      UpdateListbox;
    end;
  end;

  Result := inherited OnReceiveMessage(hwndDlg, uMsg, wParam, lParam);
end;  

function TNullIPProperties.OnConnect(Unknown: IInterface): HRESULT;
var
  NullFilter: INullIPP;
begin
  Unknown.QueryInterface(INullIPP, NullFilter);
  if Assigned(NullFilter) then
  begin
    NullFilter.get_IPin(FPin);
    NullFilter := nil;
  end;
  result := NOERROR;
end;

function TNullIPProperties.OnDisconnect: HRESULT;
begin
  FPin := nil;
  Result := S_OK;
end;

procedure TNullIPProperties.UpdateListbox;
var
  i: integer;
  enum: TEnumMediaType;
begin
  if (FListBox = 0) or (FPin = nil)
    then Exit;

  enum := TEnumMediaType.Create(FPin);
  if enum.Count > 0 then
    for i := 0 to enum.Count - 1 do
      SendMessage(FListBox, LB_ADDSTRING, 0, LPARAM(PChar(enum.MediaDescription[i])));
  enum.Free;
end;

initialization

  TBCClassFactory.CreatePropertyPage(TNullIPProperties, CLSID_NullIPPropertyPage);

end.
