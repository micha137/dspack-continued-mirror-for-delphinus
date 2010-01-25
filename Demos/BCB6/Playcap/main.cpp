//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "DSPack"
#pragma resource "*.dfm"
TMainForm *MainForm;
TSysDevEnum *SysDev;

//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormCreate(TObject *Sender)
{
  SysDev = new TSysDevEnum(CLSID_VideoInputDeviceCategory);
  if (SysDev->CountFilters > 0) {
    int i;
    TMenuItem *Device;
    for(i = 0; i < SysDev->CountFilters; i++) {
      Device = new TMenuItem(Devices);
      Device->Caption = SysDev->Filters[i].FriendlyName;
      Device->Tag = i;
      Device->OnClick = DevicesClick;
      Devices->Add(Device);
    }
  };
}
//---------------------------------------------------------------------------


void __fastcall TMainForm::DevicesClick(TObject *Sender)
{
  FilterGraph->ClearGraph();
  FilterGraph->Active = false;
  Filter->BaseFilter->Moniker = SysDev->GetMoniker(((TMenuItem *)Sender)->Tag);
  FilterGraph->Active = true;
  ICaptureGraphBuilder2 *Graph = NULL;
  IBaseFilter *SourceFilter = NULL;
  IBaseFilter *VideoFilter = NULL;
  CheckDSError(FilterGraph->QueryInterface(IID_ICaptureGraphBuilder2, &Graph));
  CheckDSError(VideoWindow->QueryInterface(IID_IBaseFilter, &VideoFilter));
  CheckDSError(Filter->QueryInterface(IID_IBaseFilter, &SourceFilter));
  Graph->RenderStream(&PIN_CATEGORY_PREVIEW, NULL, SourceFilter, NULL, VideoFilter);
  FilterGraph->Play();
  Graph->Release();
  VideoFilter->Release();
  SourceFilter->Release();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::FormDestroy(TObject *Sender)
{
  delete SysDev;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  FilterGraph->Active = false;        
}
//---------------------------------------------------------------------------

