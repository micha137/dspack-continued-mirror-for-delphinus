
    (*********************************************************************
     *                                                                   *
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
     * (C) 2004 Milenko Mitrovic <dcoder@dsp-worx.de>                    *
     *                                                                   *
     *********************************************************************)

unit PropSettings;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls,
  ExtCtrls, Forms, BaseClass, ComObj, StdVcl, AxCtrls, DirectShow9, Dialogs,
  ShellAPI, DSoundDevices, ActiveX;

const
  CLSID_PropPageSettings : TGUID = '{7115E09B-F38B-4FE7-97FD-B99B2BF99B35}';

type
  TfrmSettings = class(TFormPropertyPage)
    cmbDevices: TComboBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure cmbDevicesChange(Sender: TObject);
  public
    function OnConnect(Unknown: IUnknown): HRESULT; override;
    function OnDisconnect: HRESULT; override;
  end;

implementation
{$R *.DFM}
function TfrmSettings.OnConnect(Unknown: IUnKnown): HRESULT;
begin
  result := NOERROR;
end;

function TfrmSettings.OnDisconnect: HRESULT;
begin
  result := NOERROR;
end;

procedure TfrmSettings.FormCreate(Sender: TObject);
var
  i : integer;
  Guid : TGuid;
begin
  cmbDevices.Clear;

  if DSDeviceList.Count < 1 then Exit;
  for i := 0 to DSDeviceList.Count -1 do
  begin
    cmbDevices.Items.Add(DSDeviceList.Items[i].DeviceName);
  end;

  Guid := GetSavedDevice;
  if IsEqualGUID(GUID_NULL,Guid) then
  begin
    cmbDevices.ItemIndex := 0;
  end else
  begin
    for i := 0 to DSDeviceList.Count -1 do
    begin
      if IsEqualGUID(DSDeviceList.Items[i].DeviceGUID,Guid) then
      begin
        cmbDevices.ItemIndex := i;
        break;
      end;
    end;
  end;
end;

procedure TfrmSettings.cmbDevicesChange(Sender: TObject);
begin
  SaveDevice(DSDeviceList.Items[cmbDevices.ItemIndex].DeviceGUID);
end;

initialization
  TBCClassFactory.CreatePropertyPage(TfrmSettings, CLSID_PropPageSettings);
end.
