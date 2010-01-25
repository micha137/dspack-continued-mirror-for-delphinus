//-----------------------------------------------------------------------------
// Name: DirectShow Sample -- PushSource Filter Set
//

// Description
// ============

// Set of three source filters that provide the following source data as a video stream:

//    TBCPushSourceBitmap    - Single bitmap  (loaded from current directory)
//    TBCPushSourceBitmapSet - Set of bitmaps (loaded from current directory)
//    TBCPushSourceDesktop   - Copy of current desktop image (GDI only)


// Path
// ====

// Source: (SDK root)\Samples\C++\DirectShow\Filters\PushSource

// User's Guide
// ============

// This filter appears in GraphEdit as "_ PushSource Bitmap Filter",
// "_ PushSource BitmapSet Filter", and "_ PushSource Desktop Filter".

// To use a filter, load it into GraphEdit and render its output pin.  This will
// connect a video renderer (and possibly a Color Space Convertor filter) and allow
// you to display the output.  If you want to render the output to an AVI file,
// load the AVI Mux, load a File Writer Filter, provide an output name to the
// File Writer, and render the PushSource filter's output pin.  You can also
// load and connect video compressors, video effects, etc.

// NOTE: The desktop capture filter does not support hardware overlays, so it
// will not capture video rendered to an overlay surface or cursors displayed via
// hardware overlay.  It uses GDI to convert the current desktop image into a
// bitmap, which is passed to the output pin as a media sample.


// Converted to Delphi by Nevhasymyy Andriy (E-Mail: a.n@email.com)
// Thanks to Henri Gourvest (hgourvest@progdigy.com)
//
// Desc: DirectShow sample code - In-memory push mode source filter
//
// Portions created by Microsoft are
// Copyright (c) 1992-2002 Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

{
  @abstract(In-memory push mode source filter Delphi conversion)
  @author(Andriy Nevhasymyy: a.n@email.com)
  @created(Jun 28, 2003)
  @lastmod(Jul 23, 2003)
}
library PushSource;

uses
  BaseClass,
  UPushSource in 'UPushSource.pas';

{$E ax}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

begin
end.
