unit PropEdit;

interface

uses SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls,
  ExtCtrls, Forms, BaseClass, ComObj, StdVcl, AxCtrls, DirectShow9, dsutil;

type
  TFormPropEdit = class(TFormPropertyPage)
    ListBox: TListBox;
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  public
    pin: IPin;
    Enum: TEnumMediaType;
    function OnConnect(Unknown: IUnknown): HRESULT; override;
    function OnDisconnect: HRESULT; override;
    function OnApplyChanges: HRESULT; override;
  end;

const
    CLSID_NullIPPropertyPage : TGUID = '{8928AD20-2FEE-11cf-BCB1-444553540000}';

implementation
uses main;

{$R *.DFM}


{ TFormPropEdit }

function TFormPropEdit.OnConnect(Unknown: IInterface): HRESULT;
var NullFilter: INullIPP;
begin
  Unknown.QueryInterface(INullIPP, NullFilter);
  NullFilter.get_IPin(pin);
  result := NOERROR;
end;

function TFormPropEdit.OnDisconnect: HRESULT;
begin
  Pin := nil;
  result := NOERROR;
end;

procedure TFormPropEdit.FormActivate(Sender: TObject);
var
  i: integer;
begin
  ListBox.Clear;
  if Pin = nil then exit;
  Enum:= TEnumMediaType.Create(pin);
  if Enum.Count > 0 then
    for i := 0 to Enum.Count - 1 do
      ListBox.Items.Add(Enum.MediaDescription[i]);
end;

procedure TFormPropEdit.FormDeactivate(Sender: TObject);
begin
  Enum.Free;
end;

function TFormPropEdit.OnApplyChanges: HRESULT;
begin
  result := NOERROR;
end;

initialization

  TBCClassFactory.CreatePropertyPage(TFormPropEdit, CLSID_NullIPPropertyPage);

end.
 