//---------------------------------------------------------------------------

#ifndef mainH
#define mainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "DSPack.hpp"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <Menus.hpp>
#include <ToolWin.hpp>
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
        TVideoWindow *VideoWindow;
        TMainMenu *MainMenu;
        TDSTrackBar *DSTrackBar1;
        TFilterGraph *FilterGraph;
        TOpenDialog *OpenDialog;
        TMenuItem *MenuFile;
        TMenuItem *MenuOpen;
        TToolBar *ToolBar;
        TToolButton *tbPlay;
        TToolButton *tbPause;
        TToolButton *tbStop;
        TImageList *ImageList;
        void __fastcall MenuOpenClick(TObject *Sender);
        void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
        void __fastcall VideoWindowDblClick(TObject *Sender);
        void __fastcall tbPlayClick(TObject *Sender);
        void __fastcall tbPauseClick(TObject *Sender);
        void __fastcall tbStopClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
