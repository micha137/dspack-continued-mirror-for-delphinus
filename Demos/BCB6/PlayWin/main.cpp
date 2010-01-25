//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "DSPack"
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::MenuOpenClick(TObject *Sender)
{
  if (OpenDialog->Execute()) {
        FilterGraph->Active = FALSE;
        FilterGraph->Active = TRUE;
        FilterGraph->RenderFile(OpenDialog->FileName);
        FilterGraph->Play();
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  FilterGraph->Active = FALSE;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::VideoWindowDblClick(TObject *Sender)
{
  VideoWindow->FullScreen = !VideoWindow->FullScreen;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::tbPlayClick(TObject *Sender)
{
  FilterGraph->Play();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::tbPauseClick(TObject *Sender)
{
  FilterGraph->Pause();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::tbStopClick(TObject *Sender)
{
  FilterGraph->Stop();
}

//---------------------------------------------------------------------------

