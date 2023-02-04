{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SpringLib;

{$warn 5023 off : no warning about unused units}
interface

uses
  EN10270, EN13906, EN15800, UtilsBase, GraphBase, UnitOfMeasurement, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('SpringLib', @Register);
end.
