{
  Description: Simulated annealing optimizer.

  Copyright (C) 2022 Melchiorre Caruso <melchiorrecaruso@gmail.com>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

unit simulatedannealing;

{$mode objfpc}

interface

uses
  Classes, DateUtils, SysUtils;

type
  // tsimulatedannealing

  TSimulatedAnnealing = class
  private
    fCoolingRate: double;
    fExecutionTime: longint;
    fInitialTemperature: double;
    procedure CopySolution(const Source: tbytes; var Dest: tbytes);
    function AcceptanceProbability(
      const Energy, NeighbourEnergy, Temperature: double): double;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute(var BestSolution: tbytes);
    procedure CreateSolution(var Neighbour: tbytes); virtual; abstract;
    function CalculateEnergy(const Solution: tbytes): double; virtual; abstract;

    procedure PrintSolution(const Solution: tbytes); virtual; abstract;
  published
    property CoolingRate: double read fCoolingRate write fCoolingRate;
    property InitialTemperature: double read fInitialTemperature
      write fInitialTemperature;
    property ExecutionTime: longint read fExecutionTime write fExecutionTime;
  end;


implementation

// TSimulatedAnnealing

constructor TSimulatedAnnealing.Create;
begin
  inherited Create;
  fCoolingRate := 0.001; // cooling rate
  fExecutionTime := 10;
  fInitialTemperature := 10; // set initial temp
end;

destructor TSimulatedAnnealing.Destroy;
begin
  inherited Destroy;
end;

function TSimulatedAnnealing.AcceptanceProbability(
  const Energy, NeighbourEnergy, Temperature: double): double;
begin
  // if the new solution is better, accept it
  if (NeighbourEnergy < Energy) then
  begin
    Result := 1.0;
  end
  else
    // if the new solution is worse, calculate an acceptance probability
  begin
    Result := exp((Energy - NeighbourEnergy) / Temperature);
  end;
end;

procedure TSimulatedAnnealing.CopySolution(const Source: tbytes; var Dest: tbytes);
begin
  system.move(Source[0], Dest[0], system.length(Source));
end;

procedure TSimulatedAnnealing.Execute(var BestSolution: tbytes);
var
  BestEnergy: double;
  CurrentEnergy: double;
  CurrentSolution: tbytes = nil;
  NeighbourEnergy: double;
  NeighbourSolution: tbytes = nil;
  StartTime: tdatetime;
  Temperature: double;
begin
  // initialize temperature
  Temperature := fInitialTemperature;
  // initialize current solution
  SetLength(CurrentSolution, system.length(BestSolution));
  CopySolution(BestSolution, CurrentSolution);
  // initalize best solution
  BestEnergy := CalculateEnergy(BestSolution);
  // initialize NeighbourSolution
  setlength(NeighbourSolution, system.length(BestSolution));
  // loop until system has cooled
  StartTime := Now;
  while SecondsBetween(Now, StartTime) < fExecutionTime do
  begin
    // create new neighbour BestSolution
    CopySolution(CurrentSolution, NeighbourSolution);
    CreateSolution(NeighbourSolution);
    // get BestEnergy of solutions
    CurrentEnergy := CalculateEnergy(CurrentSolution);
    NeighbourEnergy := CalculateEnergy(NeighbourSolution);
    // decide if we should accept the neighbour
    if AcceptanceProbability(CurrentEnergy, NeighbourEnergy, Temperature) > random then
    begin
      CopySolution(NeighbourSolution, CurrentSolution);
    end;
    // keep track of the best bestsolution found
    CurrentEnergy := CalculateEnergy(CurrentSolution);
    BestEnergy := CalculateEnergy(BestSolution);
    if CurrentEnergy < BestEnergy then
    begin
      CopySolution(CurrentSolution, BestSolution);
      PrintSolution(BestSolution);
    end;
    // cool system
    Temperature := Temperature * (1 - fCoolingRate);
  end;
  SetLength(NeighbourSolution, 0);
  SetLength(CurrentSolution, 0);
end;

end.
