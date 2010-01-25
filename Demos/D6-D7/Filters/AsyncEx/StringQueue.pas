unit StringQueue;

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
  classes, SyncObjs;

type
  TStringQueue = class
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push(AItem: string);
    function Pop: string;
    procedure InsertItem(AItem: string; Pos: integer);
    function GetItem(Count: integer): string;
    function GetAllItems: TStrings;
    function GetCount: integer;
  private
    FItemlist: TStringlist;
    FCriticalSection: TCriticalsection;
  end;

implementation

procedure TStringQueue.InsertItem(AItem: string; Pos: integer);
begin
  FCriticalSection.Enter;
  FItemlist.Insert(Pos, AItem);
  FCriticalsection.Leave;
end;

function TStringQueue.GetCount: integer;
begin        
  FCriticalsection.Enter;
  Result := FItemlist.Count;
  FCriticalsection.Leave;
end;          

function TStringQueue.GetItem(Count: integer): string;
begin
  FCriticalsection.Enter;
  Result := FItemlist[Count];
  FCriticalsection.Leave;
end;

constructor TStringQueue.Create;
begin
  inherited Create;
  FItemlist := TStringList.Create;
  FCriticalsection := TCriticalSection.Create;
end;

destructor TStringQueue.Destroy;
begin
  inherited Destroy;
  FItemlist.Destroy;
  FCriticalsection.Destroy;
end;

function TStringQueue.Pop: string;
begin
  FCriticalsection.Enter;
  Result := FItemlist[0];
  FItemlist.Delete(0);
  FCriticalsection.Leave;
end;

procedure TStringQueue.Push(AItem: string);
begin
  FCriticalsection.Enter;
  FItemlist.Add(AItem);
  FCriticalsection.Leave;
end;

function TStringQueue.GetAllItems: TStrings;
begin
  FCriticalsection.Enter;
  result := FItemlist;
  FCriticalsection.Leave;
end;

end.

