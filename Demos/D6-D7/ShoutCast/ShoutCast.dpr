program ShoutCast;

(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *  Copyright (C) 2005 Martin Offenwanger                                    *
 *  Mail: coder@dsplayer.de                                                  *
 *  Web:  http://www.dsplayer.de                                             *
 *                                                                           *
 *  This Program is free software; you can redistribute it and/or modify     *
 *  it under the terms of the GNU General Public License as published by     *
 *  the Free Software Foundation; either version 2, or (at your option)      *
 *  any later version.                                                       *
 *                                                                           *
 *  This Program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the             *
 *  GNU General Public License for more details.                             *
 *                                                                           *
 *  You should have received a copy of the GNU General Public License        *
 *  along with GNU Make; see the file COPYING.  If not, write to             *
 *  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.    *
 *  http://www.gnu.org/copyleft/gpl.html                                     *
 *                                                                           *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)
{
@author(Martin Offenwanger: coder@dsplayer.de)
@created(Apr 02, 2005)
@lastmod(Apr 03, 2005)
}

uses
  Forms,
  Main in 'Main.pas' {Form1},
  Filter in '..\Filters\AsyncEx\Filter.pas',
  Definitions in 'Definitions.pas',
  WorkerThread in '..\Filters\AsyncEx\WorkerThread.pas',
  AsyncReader in '..\Filters\AsyncEx\AsyncReader.pas',
  config in '..\Filters\AsyncEx\Config.pas',
  ICYParser in '..\Filters\AsyncEx\ICYParser.pas',
  Queue in '..\Filters\AsyncEx\Queue.pas',
  ShoutCastStream in '..\Filters\AsyncEx\ShoutCastStream.pas',
  Sock in '..\Filters\AsyncEx\Sock.pas',
  StreamOutPin in '..\Filters\AsyncEx\StreamOutPin.pas',
  StringQueue in '..\Filters\AsyncEx\StringQueue.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

