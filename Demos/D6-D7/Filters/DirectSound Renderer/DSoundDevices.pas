
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

unit DSoundDevices;

interface

uses
  Windows, Classes, DirectSound, SysUtils, IniFiles, ActiveX, DSUtil;

type
  TDSDeviceItem  = class(TCollectionItem)
  public
    DeviceName : String;
    DeviceGUID : TGUID;
  end;

  TDSDeviceList = class(TCollection)
  private
    function  GetItem(Index : Integer) : TDSDeviceItem;
    procedure SetItem(Index : Integer; Value : TDSDeviceItem);
  public
    function Add : TDSDeviceItem;
    property Items[Index: Integer]: TDSDeviceItem read GetItem write SetItem; default;
  end;

  procedure SaveDevice(Guid : TGUID);
  function GetSavedDevice : TGUID;

var
  DSDeviceList : TDSDeviceList;

implementation
{*** Device Enumeration Callback **********************************************}
function DSEnumCallback(Guid : PGUID; lpstrDescription : PChar; lpstrModule : PChar; lpContext : Pointer) : BOOL; stdcall;
begin
  DSDeviceList.Add;
  DSDeviceList.Items[DSDeviceList.Count -1].DeviceName := lpstrDescription;
  if Assigned(GUID) then DSDeviceList.Items[DSDeviceList.Count -1].DeviceGUID := Guid^;
  Result := True;
end;
{*** TDSDeviceList ************************************************************}
function TDSDeviceList.Add : TDSDeviceItem;
begin
  Result := TDSDeviceItem(inherited Add);
end;

function TDSDeviceList.GetItem(Index : Integer) : TDSDeviceItem;
begin
  Result := TDSDeviceItem(inherited GetItem(Index));
end;

procedure TDSDeviceList.SetItem(Index : Integer; Value : TDSDeviceItem);
begin
  inherited SetItem(Index, Value);
end;
{******************************************************************************}
function GetSavedDevice : TGUID;
var
  Buf : array[0..MAX_PATH -1] of Char;
  str : String;
  i : integer;
  Guid : TGuid;
begin
  Result := GUID_NULL;
  if DSDeviceList.Count = 0 then Exit;
  if GetModuleFileName(HInstance,Buf,MAX_PATH) > 0 then
  begin
    str := ExtractFilePath(Buf) + 'Device.ini';
    with TIniFile.Create(str) do
    begin
      if ValueExists('Device','Default') then
      begin
        str := ReadString('Device','Default',GUIDToString(GUID_NULL));
        Guid := StringToGUID(str);
        for i := 0 to DSDeviceList.Count -1 do
        begin
          if IsEqualGUID(DSDeviceList.Items[i].DeviceGUID,Guid) then
          begin
            Result := Guid;
            break;
          end;
        end;
      end;
      Free;
    end;
  end;
end;

procedure SaveDevice(Guid : TGUID);
var
  Buf : array[0..MAX_PATH -1] of Char;
  str : String;
begin
  if GetModuleFileName(HInstance,Buf,MAX_PATH) > 0 then
  begin
    str := ExtractFilePath(Buf) + 'Device.ini';
    with TIniFile.Create(str) do
    begin
      WriteString('Device','Default',GUIDToString(Guid));
      Free;
    end;
  end;
end;
{******************************************************************************}
initialization
  DSDeviceList := TDSDeviceList.Create(TDSDeviceItem);
  DirectSoundEnumerate(DSEnumCallback, nil);

finalization
  FreeAndNil(DSDeviceList);

end.
