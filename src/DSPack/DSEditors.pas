
    (*********************************************************************
     *  DSPack 2.3.3                                                     *
     *                                                                   *
     *  home page : http://www.progdigy.com                              *
     *  email     : hgourvest@progdigy.com                               *
     *   Thanks to Michael Andersen. (DSVideoWindowEx)                   *
     *                                                                   *
     *  date      : 21-02-2003                                           *
     *                                                                   *
     *  The contents of this file are used with permission, subject to   *
     *  the Mozilla Public License Version 1.1 (the "License"); you may  *
     *  not use this file except in compliance with the License. You may *
     *  obtain a copy of the License at                                  *
     *  http://www.mozilla.org/MPL/MPL-1.1.html                          *
     *                                                                   *
     *  Software distributed under the License is distributed on an      *
     *  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   *
     *  implied. See the License for the specific language governing     *
     *  rights and limitations under the License.                        *
     *                                                                   *
     *********************************************************************)

unit DSEditors;

{$IFDEF VER150}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

interface
{$I dspack.inc}

uses

{$IFDEF COMPILER6_UP} DesignIntf, DesignEditors, {$ELSE} DsgnIntf, {$ENDIF}
 Forms, Controls, DXSUtil, DSPack;

type

// *****************************************************************************
//  TMediaTypePropertyClass
// *****************************************************************************

  TMediaTypePropertyClass = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

// *****************************************************************************
//  TBaseFilterPropertyClass
// *****************************************************************************

  TBaseFilterPropertyClass = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  procedure Register;

implementation

uses MediaTypeEditor, BaseFilterEditor, Classes;

// *****************************************************************************
//  TMediaTypePropertyClass
// *****************************************************************************

  procedure TMediaTypePropertyClass.Edit;
  var
    Dlg: TFormMediaType;
  begin
    Dlg := TFormMediaType.create(Application);
    try
      Dlg.MediaType.Assign(TMediaType(GetOrdValue));
      if Dlg.ShowModal = mrOk then
      begin
        TMediaType(GetOrdValue).Assign(Dlg.MediaType);
        if (GetComponent(0) is TSampleGrabber) then
          IFilter(GetComponent(0) as TSampleGrabber).NotifyFilter(foRefresh);
        Modified;
      end;
    finally
      Dlg.Free;
    end;
  end;

  function TMediaTypePropertyClass.GetAttributes: TPropertyAttributes;
  begin
    Result := [paDialog];
  end;

// *****************************************************************************
//  TBaseFilterPropertyClass
// *****************************************************************************

  procedure TBaseFilterPropertyClass.Edit;
  var
    Dlg: TFormBaseFilter;
  begin
    Dlg := TFormBaseFilter.create(Application);
    try
      Dlg.Filter.BaseFilter.Assign(TBaseFilter(GetOrdValue));
      if Dlg.ShowModal = mrOk then
      begin
        TBaseFilter(GetOrdValue).Assign(Dlg.Filter.BaseFilter);
        if (GetComponent(0) is TFilter) then
          IFilter(GetComponent(0) as TFilter).NotifyFilter(foRefresh);
        Modified;
      end;
    finally
      Dlg.Free;
    end;
  end;

  function TBaseFilterPropertyClass.GetAttributes: TPropertyAttributes;
  begin
    Result := [paDialog];
  end;

  procedure Register;
  begin
    RegisterComponents('DSPack', [TFilterGraph, TVideoWindow, TSampleGrabber,
      TFilter, TASFWriter, TDSTrackBar, TDSVideoWindowEx2]);  
    RegisterPropertyEditor(TypeInfo(TMediaType), nil, '', TMediaTypePropertyClass);
    RegisterPropertyEditor(TypeInfo(TBaseFilter), nil, '', TBaseFilterPropertyClass);
  end;

end.
