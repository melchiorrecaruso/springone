{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit NewSpin;

{$warn 5023 off : no warning about unused units}
interface

uses
  NewFloatSpinEdit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('NewFloatSpinEdit', @NewFloatSpinEdit.Register);
end;

initialization
  RegisterPackage('NewSpin', @Register);
end.
