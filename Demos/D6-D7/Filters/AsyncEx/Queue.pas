unit Queue;

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
@lastmod(Sep 09, 2004)
}

interface

uses
  Classes, Windows;

type
  PAsyncRequest = ^TAsyncRequest;
  TAsyncRequest = record
    FPos: LONGLONG;
    FAligned: BOOL;
    FLength: Integer;
    FBuffer: Pointer;
    FContext: Pointer;
    FUser: DWORD;
    Fhr: HRESULT;
  end;

type
  TQueue = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push(AItem: Pointer);
    function Pop: Pointer;
  end;

implementation

constructor TQueue.Create;
begin
  inherited Create;
end;

destructor TQueue.Destroy;
begin
  inherited Destroy;
end;

function TQueue.Pop: Pointer;
begin
  if Count > 0 then
  begin
    Result := Items[0];
    Delete(0);
  end
  else
    Result := nil;
end;

procedure TQueue.Push(AItem: Pointer);
begin
  Add(AItem);
end;

end.

 