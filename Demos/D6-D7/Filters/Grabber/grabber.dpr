//------------------------------------------------------------------------------
// File: Grabber.cpp
//
// Desc: DirectShow sample code - Implementation file for the SampleGrabber
//       example filter
//
// Copyright (c) Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

// Delphi Conversion (C) Milenko Mitrovic (dcoder@dsp-worx.de)

library grabber;

{$E ax}

uses
  BaseClass,
  GrabberFilter in 'GrabberFilter.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

begin

end.

