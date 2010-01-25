unit SelectURL;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormSelectURL = class(TForm)
    btOK: TButton;
    btCancel: TButton;
    URL: TEdit;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FormSelectURL: TFormSelectURL;

implementation

{$R *.dfm}

end.
