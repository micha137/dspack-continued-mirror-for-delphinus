// This Sample show how to use Property Pages that are located
// within Resource Files.
//
// To compile it you must disable WITH_PROPERTY_PAGE in
// BaseClass.pas and include your Resource File here.

library NullIP;

uses
  BaseClass,
  main in 'main.pas',
  prop in 'prop.pas';

{$E ax}
{$R prop.res} // The Resource File with the Dialog

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

begin

end.
