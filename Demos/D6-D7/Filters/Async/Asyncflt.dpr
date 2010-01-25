//------------------------------------------------------------------------------
//
// Desc: Source for Async Filter
// Last modified:
//
// The Async sample filter does not support AVI files, because it
// cannot connect to the AVI Splitter filter. The Async filter's output pin
// proposes MEDIATYPE_Stream and MEDIASUBTYPE_NULL for the media type.
// The input pin on the AVI Splitter filter does not accept MEDIASUBTYPE_NULL,
// and does not propose any types of its own. Therefore, the pin connection
// fails. The Async filter could be enhanced to offer MEDIASUBTYPE_Avi when
// appropriate. For example, it could examine the file format, or use the file
// extension.
//
// Converted to Delphi by
// Andriy Nevhasymyy (a.n@email.com), Milenko Mitrovic (dcoder@dsp-worx.de)
//
// Portions created by Microsoft are
// Copyright (c) 1992 - 2000, Microsoft Corporation.  All rights reserved.
//
//------------------------------------------------------------------------------

library Asyncflt;

uses
  BaseClass,
  UAsyncFlt in 'UAsyncFlt.pas',
  UAsyncRdr in 'UAsyncRdr.pas',
  UAsyncIo in 'UAsyncIo.pas';

{$E ax}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

begin
end.
