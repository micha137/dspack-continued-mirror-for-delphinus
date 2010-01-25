library AsyncEx;

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

uses
  BaseClass,
  Filter in 'Filter.pas',
  StreamOutPin in 'StreamOutPin.pas',
  AsyncReader in 'AsyncReader.pas',
  WorkerThread in 'WorkerThread.pas',
  Queue in 'Queue.pas',
  StringQueue in 'StringQueue.pas',
  ShoutCastStream in 'ShoutCastStream.pas',
  ICYParser in 'ICYParser.pas',
  config in 'Config.pas',
  Sock in 'Sock.pas';

{$E ax}
exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;
begin

end.

