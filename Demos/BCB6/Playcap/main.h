//---------------------------------------------------------------------------

#ifndef mainH
#define mainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "DSPack.hpp"
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
        TVideoWindow *VideoWindow;
        TMainMenu *MainMenu;
        TMenuItem *Devices;
        TFilterGraph *FilterGraph;
        TFilter *Filter;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall DevicesClick(TObject *Sender);
        void __fastcall FormDestroy(TObject *Sender);
        void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
private:	// User declarations
public:		// User declarations
        __fastcall TMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
