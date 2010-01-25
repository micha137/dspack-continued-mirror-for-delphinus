//------------------------------------------------------------------------------
//
// Desc: AlphaRenderer - a rendering filter that will perform an
//       alpha blend onto a checkerboard background, which is based
//       on the incoming video alpha. This uses the new media subtype
//       MEDIASUBTYPE_ARGB32.
//
// Converted to Delphi by Nevhasymyy Andriy (a.n@email.com)
// Thanks to Henri Gourvest (hgourvest@progdigy.com)
//
// Portions created by Microsoft are
// Copyright (c) Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

library AlphaRenderer;

{%ToDo 'AlphaRenderer.todo'}

uses
  BaseClass,
  UAlphaRenderer in 'UAlphaRenderer.pas';

{$E ax}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

begin
end.

