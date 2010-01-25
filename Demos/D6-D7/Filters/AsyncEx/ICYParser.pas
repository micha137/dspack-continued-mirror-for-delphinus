unit ICYParser;

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

// ICY Item Names
const
  ICYMetaInt = 'icy-metaint:';
  ICYName = 'icy-name:';
  ICYGenre = 'icy-genre:';
  ICYURL = 'icy-url:';
  ICYBitrate = 'icy-br:';
  ICYError = 'icy-error:';

  // functions return error " N/A (String)"," 0 (integer)"," false (Boolean)"
function GetICYItem(ICYItemName: string; Streamheader: string): string;
function GetServerICYInt(Streamheader: string): integer;
function GetServerICYName(StreamHeader: string): string;
function GetServerICYGenre(StreamHeader: string): string;
function GetServerICYURL(StreamHeader: string): string;
function GetServerICYBitRate(StreamHeader: string): string;
function GetICYSuccessfullyConnected(StreamHeader: string;
  out ErrMessage: string): boolean;

implementation

uses
  SysUtils;

function GetICYSuccessfullyConnected(StreamHeader: string;
  out ErrMessage: string): boolean;
var
  Pos1: integer;
begin
  Pos1 := Pos('200 OK', StreamHeader);
  if Pos1 = 0 then
  begin
    ErrMessage := copy(StreamHeader, 1, length(StreamHeader));
    result := false;
    exit;
  end;
  ErrMessage := '';
  result := true;
end;

function GetICYItem(ICYItemName: string; Streamheader: string): string;
var
  Pos1: integer;
  ICYItem: string;
  i: integer;
begin
  Pos1 := Pos(ICYItemName, Streamheader);
  if Pos1 = 0 then
  begin
    result := 'N/A';
    exit;
  end;
  Streamheader := copy(Streamheader, Pos1 + length(ICYItemName),
    length(Streamheader) - Pos1 + length(ICYItemName));
  Pos1 := Pos(#13#10, Streamheader);
  Streamheader := copy(Streamheader, 0, Pos1);
  ICYItem := '';
  for i := 1 to length(Streamheader) - 1 do
    if Streamheader[i] <> ' ' then
      ICYItem := ICYItem + Streamheader[i];
  result := ICYItem;
end;

function GetServerICYName(StreamHeader: string): string;
begin
  result := GetICYItem(ICYName, StreamHeader);
end;

function GetServerICYGenre(StreamHeader: string): string;
begin
  result := GetICYItem(ICYGenre, StreamHeader);
end;

function GetServerICYURL(StreamHeader: string): string;
begin
  result := GetICYItem(ICYURL, StreamHeader);
end;

function GetServerICYBitRate(StreamHeader: string): string;
begin
  result := GetICYItem(ICYBitrate, StreamHeader);
end;

function GetServerICYInt(Streamheader: string): integer;
var
  ResultS: string;
begin
  ResultS := GetICYItem(ICYMetaInt, Streamheader);
  if ResultS = 'N/A' then
  begin
    Result := 0;
    exit;
  end;
  Result := strtoint(ResultS);
end;

end.

