{ Spring library linking

  Copyright (C) 2022-2025 Melchiorre Caruso <melchiorrecaruso@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}

unit LibLink;

{$mode ObjFPC}{$H+}
{$i defines.inc}

interface

uses
  Classes, SysUtils, SpringSolvers, SpringTolerances, SpringMaterials;

var
  {$IFDEF MODULE1}
  SpringTolerance: TEN15800;
  SpringSolver: TCompressionSpringSolver;
  {$ENDIF}
  {$IFDEF MODULE3}
  SpringTolerance: TDIN2194;
  SpringSolver: TTorsionSpringSolver;
  {$ENDIF}

implementation

initialization
begin
  MAT := TMaterial.Create;
  {$IFDEF MODULE1}
  SpringTolerance := TEN15800.Create;
  SpringSolver    := TCompressionSpringSolver.Create;
  {$ENDIF}
  {$IFDEF MODULE3}
  SpringTolerance := TDIN2194.Create;
  SpringSolver    := TTorsionSpringSolver.Create;
  {$ENDIF}
end;

finalization
begin
  {$IFDEF MODULE1}
  SpringTolerance.Destroy;
  SpringSolver.Destroy;
  {$ENDIF}
  {$IFDEF MODULE3}
  SpringTolerance.Destroy;
  SpringSolver.Destroy;
  {$ENDIF}
  MAT.Destroy;
end;

end.

