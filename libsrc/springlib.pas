{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SpringLib;

{$warn 5023 off : no warning about unused units}
interface

uses
  BaseGraphics, BaseUtils, NewFloatSpinEdit, SpringMaterials, SpringSolvers, 
  SpringTolerances, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('NewFloatSpinEdit', @NewFloatSpinEdit.Register);
end;

initialization
  RegisterPackage('SpringLib', @Register);
end.
