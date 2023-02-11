unit UnitOfMeasurement;

{$H+}{$modeSwitch advancedRecords}  

interface

uses
  SysUtils;

type
  { TBaseUnit }

  TBaseUnit = class
    class function GetSymbol: string; virtual; abstract;
  end;

  { TDimension }

  generic TDimension<U: TBaseUnit> = record
    type TSelf = specialize TDimension<U>;
  private
  public
    Value: double;
    function ToString: string;
    class operator +  (const ALeft, ARight: TSelf): TSelf;
    class operator -  (const ALeft, ARight: TSelf): TSelf;
    class operator *  (const Factor: double; const AValue: TSelf): TSelf;
    class operator *  (const AValue: TSelf; const Factor: double): TSelf;
    class operator /  (const AValue: TSelf; const Factor: double): TSelf;
    class operator /  (const ALeft, ARight: TSelf): double;
    class operator =  (const ALeft, ARight: TSelf): boolean;
    class operator <  (const ALeft, ARight: TSelf): boolean;
    class operator >  (const ALeft, ARight: TSelf): boolean;
    class operator <= (const ALeft, ARight: TSelf): boolean;
    class operator >= (const ALeft, ARight: TSelf): boolean;
    class operator mod(const ALeft, ARight: TSelf): TSelf;
  end;

  { TDimensionUnit }

  generic TDimensionUnit<BaseUnit: TBaseUnit> = record
    type TSelf = specialize TDimensionUnit<BaseUnit>;
    type TDimension = specialize TDimension<BaseUnit>;
  public
    Factor: double;
    Symbol: string;
    function Format(const Dimension: TDimension): string;
    function Value (const Dimension: TDimension): double;
    class operator *(const AValue: double; const {%H-}Me: TSelf): TDimension;
  end;

{ Units of Meter }

type
  TMeter = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TLength     = specialize TDimension    <TMeter>;
  TLengthUnit = specialize TDimensionUnit<TMeter>;

const
  m      : specialize TDimensionUnit<TMeter> = (Factor: 1              ; Symbol: 'm');
  dm     : specialize TDimensionUnit<TMeter> = (Factor: 1/10           ; Symbol: 'dm');
  cm     : specialize TDimensionUnit<TMeter> = (Factor: 1/100          ; Symbol: 'cm');
  mm     : specialize TDimensionUnit<TMeter> = (Factor: 1/1000         ; Symbol: 'mm');
  inch   : specialize TDimensionUnit<TMeter> = (Factor: 127/5000       ; Symbol: 'in');

{ Units of Meter2 }

type
  TMeter2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TArea     = specialize TDimension    <TMeter2>;
  TAreaUnit = specialize TDimensionUnit<TMeter2>;

const
  m2     : specialize TDimensionUnit<TMeter2> = (Factor: 1              ; Symbol: 'm2');
  dm2    : specialize TDimensionUnit<TMeter2> = (Factor: 1/100          ; Symbol: 'dm2');
  cm2    : specialize TDimensionUnit<TMeter2> = (Factor: 1/10000        ; Symbol: 'cm2');
  mm2    : specialize TDimensionUnit<TMeter2> = (Factor: 1/1000000      ; Symbol: 'mm2');

{ Units of Meter3 }

type
  TMeter3 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TVolume     = specialize TDimension    <TMeter3>;
  TVolumeUnit = specialize TDimensionUnit<TMeter3>;

const
  m3     : specialize TDimensionUnit<TMeter3> = (Factor: 1              ; Symbol: 'm3');
  dm3    : specialize TDimensionUnit<TMeter3> = (Factor: 1/1000         ; Symbol: 'dm3');
  cm3    : specialize TDimensionUnit<TMeter3> = (Factor: 1/1000000      ; Symbol: 'dm3');
  mm3    : specialize TDimensionUnit<TMeter3> = (Factor: 1/1000000000   ; Symbol: 'mm3');

{ Units of Meter4 }

type
  TMeter4 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TMomentOfArea     = specialize TDimension    <TMeter4>;
  TMomentOfAreaUnit = specialize TDimensionUnit<TMeter4>;

const
  m4     : specialize TDimensionUnit<TMeter4> = (Factor: 1              ; Symbol: 'm4');
  dm4    : specialize TDimensionUnit<TMeter4> = (Factor: 1/10000        ; Symbol: 'dm4');
  cm4    : specialize TDimensionUnit<TMeter4> = (Factor: 1/100000000    ; Symbol: 'cm4');
  mm4    : specialize TDimensionUnit<TMeter4> = (Factor: 1/1000000000000; Symbol: 'mm4');

{ Units of Kilogram }

type
  TKilogram = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TMass     = specialize TDimension    <TKilogram>;
  TMassUnit = specialize TDimensionUnit<TKilogram>;

const
  kg     : specialize TDimensionUnit<TKilogram> = (Factor: 1              ; Symbol: 'kg');
  hg     : specialize TDimensionUnit<TKilogram> = (Factor: 1/10           ; Symbol: 'hg');
  dag    : specialize TDimensionUnit<TKilogram> = (Factor: 1/100          ; Symbol: 'dag');
  g      : specialize TDimensionUnit<TKilogram> = (Factor: 1/1000         ; Symbol: 'g');

{ Units of KilogramMeterPerSecond2 }

type
  TKilogramMeterPerSecond2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TForce     = specialize TDimension    <TKilogramMeterPerSecond2>;
  TForceUnit = specialize TDimensionUnit<TKilogramMeterPerSecond2>;

const
  kN     : specialize TDimensionUnit<TKilogramMeterPerSecond2> = (Factor: 1000           ; Symbol: 'kN');
  hN     : specialize TDimensionUnit<TKilogramMeterPerSecond2> = (Factor: 100            ; Symbol: 'hN');
  daN    : specialize TDimensionUnit<TKilogramMeterPerSecond2> = (Factor: 10             ; Symbol: 'daN');
  N      : specialize TDimensionUnit<TKilogramMeterPerSecond2> = (Factor: 1              ; Symbol: 'N');

{ Units of KilogramPerMeterSecond2 }

type
  TKilogramPerMeterSecond2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TPressure     = specialize TDimension    <TKilogramPerMeterSecond2>;
  TPressureUnit = specialize TDimensionUnit<TKilogramPerMeterSecond2>;

const
  MPa    : specialize TDimensionUnit<TKilogramPerMeterSecond2> = (Factor: 1000000        ; Symbol: 'MPa');
  kPa    : specialize TDimensionUnit<TKilogramPerMeterSecond2> = (Factor: 1000           ; Symbol: 'kPa');
  hPa    : specialize TDimensionUnit<TKilogramPerMeterSecond2> = (Factor: 100            ; Symbol: 'hPa');
  daPa   : specialize TDimensionUnit<TKilogramPerMeterSecond2> = (Factor: 10             ; Symbol: 'daPa');
  Pa     : specialize TDimensionUnit<TKilogramPerMeterSecond2> = (Factor: 1              ; Symbol: 'Pa');

{ Units of KilogramPerSecond2 }

type
  TKilogramPerSecond2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TStiffness     = specialize TDimension    <TKilogramPerSecond2>;
  TStiffnessUnit = specialize TDimensionUnit<TKilogramPerSecond2>;

const
  N_m    : specialize TDimensionUnit<TKilogramPerSecond2> = (Factor: 1              ; Symbol: 'N/m');
  N_dm   : specialize TDimensionUnit<TKilogramPerSecond2> = (Factor: 1/10           ; Symbol: 'N/dm');
  N_cm   : specialize TDimensionUnit<TKilogramPerSecond2> = (Factor: 1/100          ; Symbol: 'N/cm');
  N_mm   : specialize TDimensionUnit<TKilogramPerSecond2> = (Factor: 1/1000         ; Symbol: 'N/mm');

{ Units of KilogramPerMeter3 }

type
  TKilogramPerMeter3 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TDensity     = specialize TDimension    <TKilogramPerMeter3>;
  TDensityUnit = specialize TDimensionUnit<TKilogramPerMeter3>;

const
  kg_m3  : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 1              ; Symbol: 'kg/m3');
  kg_dm3 : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 1/1000         ; Symbol: 'kg/dm3');
  kg_cm3 : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 1/1000000      ; Symbol: 'kg/cm3');
  kg_mm3 : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 1/1000000000   ; Symbol: 'kg/mm3');
  g_m3   : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 1000           ; Symbol: 'g/m3');
  g_dm3  : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 100            ; Symbol: 'g/dm3');
  g_cm3  : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 10             ; Symbol: 'g/cm3');
  g_mm3  : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 1              ; Symbol: 'g/mm3');

{ Units of UnitPerSecond }

type
  TUnitPerSecond = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TFrequency     = specialize TDimension    <TUnitPerSecond>;
  TFrequencyUnit = specialize TDimensionUnit<TUnitPerSecond>;

const
  Hz     : specialize TDimensionUnit<TUnitPerSecond> = (Factor: 1              ; Symbol: 'Hz');

{ Units of KilogramMeter2PerSecond2 }

type
  TKilogramMeter2PerSecond2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TWork     = specialize TDimension    <TKilogramMeter2PerSecond2>;
  TWorkUnit = specialize TDimensionUnit<TKilogramMeter2PerSecond2>;

const
  Nm     : specialize TDimensionUnit<TKilogramMeter2PerSecond2> = (Factor: 1              ; Symbol: 'Nm');
  Nmm    : specialize TDimensionUnit<TKilogramMeter2PerSecond2> = (Factor: 1000           ; Symbol: 'Nmm');

{ Units of MeterPerSecond }

type
  TMeterPerSecond = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TSpeed     = specialize TDimension    <TMeterPerSecond>;
  TSpeedUnit = specialize TDimensionUnit<TMeterPerSecond>;

const
  m_s    : specialize TDimensionUnit<TMeterPerSecond> = (Factor: 1              ; Symbol: 'm/s');

{ Units of Second }

type
  TSecond = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TTime     = specialize TDimension    <TSecond>;
  TTimeUnit = specialize TDimensionUnit<TSecond>;

const
  hr     : specialize TDimensionUnit<TSecond> = (Factor: 3600           ; Symbol: 'hr');
  mn     : specialize TDimensionUnit<TSecond> = (Factor: 60             ; Symbol: 'mn');
  s      : specialize TDimensionUnit<TSecond> = (Factor: 1              ; Symbol: 's');
  ms     : specialize TDimensionUnit<TSecond> = (Factor: 1000           ; Symbol: 'ms');

{ Units of UnitPerMeter }

type
  TUnitPerMeter = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TInvLength     = specialize TDimension    <TUnitPerMeter>;
  TInvLengthUnit = specialize TDimensionUnit<TUnitPerMeter>;

{ Units of UnitPerMeter2 }

type
  TUnitPerMeter2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TInvArea     = specialize TDimension    <TUnitPerMeter2>;
  TInvAreaUnit = specialize TDimensionUnit<TUnitPerMeter2>;

{ Operators }

operator *(const ALeft: TLengthUnit; const ARight: TLengthUnit): TAreaUnit; inline;
operator *(const ALeft: TLength;     const ARight: TLength    ): TArea; inline;
operator *(const ALeft: TLengthUnit; const ARight: TAreaUnit): TVolumeUnit; inline;
operator *(const ALeft: TLength;     const ARight: TArea    ): TVolume; inline;
operator *(const ALeft: TAreaUnit; const ARight: TLengthUnit): TVolumeUnit; inline;
operator *(const ALeft: TArea;     const ARight: TLength    ): TVolume; inline;
operator *(const ALeft: TLengthUnit; const ARight: TVolumeUnit): TMomentOfAreaUnit; inline;
operator *(const ALeft: TLength;     const ARight: TVolume    ): TMomentOfArea; inline;
operator *(const ALeft: TVolumeUnit; const ARight: TLengthUnit): TMomentOfAreaUnit; inline;
operator *(const ALeft: TVolume;     const ARight: TLength    ): TMomentOfArea; inline;
operator *(const ALeft: TLengthUnit; const ARight: TForceUnit): TWorkUnit; inline;
operator *(const ALeft: TLength;     const ARight: TForce    ): TWork; inline;
operator *(const ALeft: TForceUnit; const ARight: TLengthUnit): TWorkUnit; inline;
operator *(const ALeft: TForce;     const ARight: TLength    ): TWork; inline;
operator *(const ALeft: TLengthUnit; const ARight: TPressureUnit): TStiffnessUnit; inline;
operator *(const ALeft: TLength;     const ARight: TPressure    ): TStiffness; inline;
operator *(const ALeft: TPressureUnit; const ARight: TLengthUnit): TStiffnessUnit; inline;
operator *(const ALeft: TPressure;     const ARight: TLength    ): TStiffness; inline;
operator *(const ALeft: TLengthUnit; const ARight: TStiffnessUnit): TForceUnit; inline;
operator *(const ALeft: TLength;     const ARight: TStiffness    ): TForce; inline;
operator *(const ALeft: TStiffnessUnit; const ARight: TLengthUnit): TForceUnit; inline;
operator *(const ALeft: TStiffness;     const ARight: TLength    ): TForce; inline;
operator *(const ALeft: TLengthUnit; const ARight: TFrequencyUnit): TSpeedUnit; inline;
operator *(const ALeft: TLength;     const ARight: TFrequency    ): TSpeed; inline;
operator *(const ALeft: TFrequencyUnit; const ARight: TLengthUnit): TSpeedUnit; inline;
operator *(const ALeft: TFrequency;     const ARight: TLength    ): TSpeed; inline;
operator *(const ALeft: TLengthUnit; const ARight: TInvAreaUnit): TInvLengthUnit; inline;
operator *(const ALeft: TLength;     const ARight: TInvArea    ): TInvLength; inline;
operator *(const ALeft: TInvAreaUnit; const ARight: TLengthUnit): TInvLengthUnit; inline;
operator *(const ALeft: TInvArea;     const ARight: TLength    ): TInvLength; inline;
operator /(const ALeft: TLengthUnit; const ARight: TAreaUnit): TInvLengthUnit; inline;
operator /(const ALeft: TLength;     const ARight: TArea    ): TInvLength; inline;
operator /(const ALeft: TLengthUnit; const ARight: TVolumeUnit): TInvAreaUnit; inline;
operator /(const ALeft: TLength;     const ARight: TVolume    ): TInvArea; inline;
operator /(const ALeft: TLengthUnit; const ARight: TSpeedUnit): TTimeUnit; inline;
operator /(const ALeft: TLength;     const ARight: TSpeed    ): TTime; inline;
operator /(const ALeft: TLengthUnit; const ARight: TTimeUnit): TSpeedUnit; inline;
operator /(const ALeft: TLength;     const ARight: TTime    ): TSpeed; inline;
operator /(const ALeft: TLengthUnit; const ARight: TInvLengthUnit): TAreaUnit; inline;
operator /(const ALeft: TLength;     const ARight: TInvLength    ): TArea; inline;
operator /(const ALeft: TLengthUnit; const ARight: TInvAreaUnit): TVolumeUnit; inline;
operator /(const ALeft: TLength;     const ARight: TInvArea    ): TVolume; inline;
operator *(const ALeft: TAreaUnit; const ARight: TAreaUnit): TMomentOfAreaUnit; inline;
operator *(const ALeft: TArea;     const ARight: TArea    ): TMomentOfArea; inline;
operator *(const ALeft: TAreaUnit; const ARight: TPressureUnit): TForceUnit; inline;
operator *(const ALeft: TArea;     const ARight: TPressure    ): TForce; inline;
operator *(const ALeft: TPressureUnit; const ARight: TAreaUnit): TForceUnit; inline;
operator *(const ALeft: TPressure;     const ARight: TArea    ): TForce; inline;
operator *(const ALeft: TAreaUnit; const ARight: TStiffnessUnit): TWorkUnit; inline;
operator *(const ALeft: TArea;     const ARight: TStiffness    ): TWork; inline;
operator *(const ALeft: TStiffnessUnit; const ARight: TAreaUnit): TWorkUnit; inline;
operator *(const ALeft: TStiffness;     const ARight: TArea    ): TWork; inline;
operator /(const ALeft: TAreaUnit; const ARight: TLengthUnit): TLengthUnit; inline;
operator /(const ALeft: TArea;     const ARight: TLength    ): TLength; inline;
operator /(const ALeft: TAreaUnit; const ARight: TVolumeUnit): TInvLengthUnit; inline;
operator /(const ALeft: TArea;     const ARight: TVolume    ): TInvLength; inline;
operator /(const ALeft: TAreaUnit; const ARight: TMomentOfAreaUnit): TInvAreaUnit; inline;
operator /(const ALeft: TArea;     const ARight: TMomentOfArea    ): TInvArea; inline;
operator *(const ALeft: TVolumeUnit; const ARight: TPressureUnit): TWorkUnit; inline;
operator *(const ALeft: TVolume;     const ARight: TPressure    ): TWork; inline;
operator *(const ALeft: TPressureUnit; const ARight: TVolumeUnit): TWorkUnit; inline;
operator *(const ALeft: TPressure;     const ARight: TVolume    ): TWork; inline;
operator *(const ALeft: TVolumeUnit; const ARight: TDensityUnit): TMassUnit; inline;
operator *(const ALeft: TVolume;     const ARight: TDensity    ): TMass; inline;
operator *(const ALeft: TDensityUnit; const ARight: TVolumeUnit): TMassUnit; inline;
operator *(const ALeft: TDensity;     const ARight: TVolume    ): TMass; inline;
operator /(const ALeft: TVolumeUnit; const ARight: TLengthUnit): TAreaUnit; inline;
operator /(const ALeft: TVolume;     const ARight: TLength    ): TArea; inline;
operator /(const ALeft: TVolumeUnit; const ARight: TAreaUnit): TLengthUnit; inline;
operator /(const ALeft: TVolume;     const ARight: TArea    ): TLength; inline;
operator /(const ALeft: TVolumeUnit; const ARight: TMomentOfAreaUnit): TInvLengthUnit; inline;
operator /(const ALeft: TVolume;     const ARight: TMomentOfArea    ): TInvLength; inline;
operator /(const ALeft: TMomentOfAreaUnit; const ARight: TLengthUnit): TVolumeUnit; inline;
operator /(const ALeft: TMomentOfArea;     const ARight: TLength    ): TVolume; inline;
operator /(const ALeft: TMomentOfAreaUnit; const ARight: TAreaUnit): TAreaUnit; inline;
operator /(const ALeft: TMomentOfArea;     const ARight: TArea    ): TArea; inline;
operator /(const ALeft: TMomentOfAreaUnit; const ARight: TVolumeUnit): TLengthUnit; inline;
operator /(const ALeft: TMomentOfArea;     const ARight: TVolume    ): TLength; inline;
operator /(const ALeft: TMassUnit; const ARight: TVolumeUnit): TDensityUnit; inline;
operator /(const ALeft: TMass;     const ARight: TVolume    ): TDensity; inline;
operator /(const ALeft: TMassUnit; const ARight: TDensityUnit): TVolumeUnit; inline;
operator /(const ALeft: TMass;     const ARight: TDensity    ): TVolume; inline;
operator *(const ALeft: TForceUnit; const ARight: TInvAreaUnit): TPressureUnit; inline;
operator *(const ALeft: TForce;     const ARight: TInvArea    ): TPressure; inline;
operator *(const ALeft: TInvAreaUnit; const ARight: TForceUnit): TPressureUnit; inline;
operator *(const ALeft: TInvArea;     const ARight: TForce    ): TPressure; inline;
operator /(const ALeft: TForceUnit; const ARight: TLengthUnit): TStiffnessUnit; inline;
operator /(const ALeft: TForce;     const ARight: TLength    ): TStiffness; inline;
operator /(const ALeft: TForceUnit; const ARight: TAreaUnit): TPressureUnit; inline;
operator /(const ALeft: TForce;     const ARight: TArea    ): TPressure; inline;
operator /(const ALeft: TForceUnit; const ARight: TPressureUnit): TAreaUnit; inline;
operator /(const ALeft: TForce;     const ARight: TPressure    ): TArea; inline;
operator /(const ALeft: TForceUnit; const ARight: TStiffnessUnit): TLengthUnit; inline;
operator /(const ALeft: TForce;     const ARight: TStiffness    ): TLength; inline;
operator /(const ALeft: TForceUnit; const ARight: TWorkUnit): TInvLengthUnit; inline;
operator /(const ALeft: TForce;     const ARight: TWork    ): TInvLength; inline;
operator /(const ALeft: TPressureUnit; const ARight: TForceUnit): TInvAreaUnit; inline;
operator /(const ALeft: TPressure;     const ARight: TForce    ): TInvArea; inline;
operator /(const ALeft: TPressureUnit; const ARight: TStiffnessUnit): TInvLengthUnit; inline;
operator /(const ALeft: TPressure;     const ARight: TStiffness    ): TInvLength; inline;
operator /(const ALeft: TStiffnessUnit; const ARight: TLengthUnit): TPressureUnit; inline;
operator /(const ALeft: TStiffness;     const ARight: TLength    ): TPressure; inline;
operator /(const ALeft: TStiffnessUnit; const ARight: TForceUnit): TInvLengthUnit; inline;
operator /(const ALeft: TStiffness;     const ARight: TForce    ): TInvLength; inline;
operator /(const ALeft: TStiffnessUnit; const ARight: TPressureUnit): TLengthUnit; inline;
operator /(const ALeft: TStiffness;     const ARight: TPressure    ): TLength; inline;
operator /(const ALeft: TStiffnessUnit; const ARight: TWorkUnit): TInvAreaUnit; inline;
operator /(const ALeft: TStiffness;     const ARight: TWork    ): TInvArea; inline;
operator /(const ALeft: TFrequencyUnit; const ARight: TSpeedUnit): TInvLengthUnit; inline;
operator /(const ALeft: TFrequency;     const ARight: TSpeed    ): TInvLength; inline;
operator /(const ALeft: TWorkUnit; const ARight: TLengthUnit): TForceUnit; inline;
operator /(const ALeft: TWork;     const ARight: TLength    ): TForce; inline;
operator /(const ALeft: TWorkUnit; const ARight: TAreaUnit): TStiffnessUnit; inline;
operator /(const ALeft: TWork;     const ARight: TArea    ): TStiffness; inline;
operator /(const ALeft: TWorkUnit; const ARight: TVolumeUnit): TPressureUnit; inline;
operator /(const ALeft: TWork;     const ARight: TVolume    ): TPressure; inline;
operator /(const ALeft: TWorkUnit; const ARight: TForceUnit): TLengthUnit; inline;
operator /(const ALeft: TWork;     const ARight: TForce    ): TLength; inline;
operator /(const ALeft: TWorkUnit; const ARight: TPressureUnit): TVolumeUnit; inline;
operator /(const ALeft: TWork;     const ARight: TPressure    ): TVolume; inline;
operator /(const ALeft: TWorkUnit; const ARight: TStiffnessUnit): TAreaUnit; inline;
operator /(const ALeft: TWork;     const ARight: TStiffness    ): TArea; inline;
operator *(const ALeft: TSpeedUnit; const ARight: TTimeUnit): TLengthUnit; inline;
operator *(const ALeft: TSpeed;     const ARight: TTime    ): TLength; inline;
operator *(const ALeft: TTimeUnit; const ARight: TSpeedUnit): TLengthUnit; inline;
operator *(const ALeft: TTime;     const ARight: TSpeed    ): TLength; inline;
operator /(const ALeft: TSpeedUnit; const ARight: TLengthUnit): TFrequencyUnit; inline;
operator /(const ALeft: TSpeed;     const ARight: TLength    ): TFrequency; inline;
operator /(const ALeft: TSpeedUnit; const ARight: TFrequencyUnit): TLengthUnit; inline;
operator /(const ALeft: TSpeed;     const ARight: TFrequency    ): TLength; inline;
operator /(const ALeft: TInvLengthUnit; const ARight: TLengthUnit): TInvAreaUnit; inline;
operator /(const ALeft: TInvLength;     const ARight: TLength    ): TInvArea; inline;

implementation

uses
  Math;

{ TDimensionUnit }

function TDimensionUnit.Format(const Dimension: TDimension): string;
begin
  result := FloatToStr(Dimension.Value / Factor) + ' ' + Symbol;
end;

function TDimensionUnit.Value(const Dimension: TDimension): double;
begin
  result := Dimension.Value / Factor;
end;

class operator TDimensionUnit.*(const AValue: double; const {%H-}Me: TSelf): TDimension;
begin
  result.Value := AValue * Me.Factor;
end;

{ TDimension }

function TDimension.ToString: string;
begin
  result := FloatToStr(Value) + ' ' + U.GetSymbol;
end;

class operator TDimension.+(const ALeft, ARight: TSelf): TSelf;
begin
  result.Value := ALeft.Value + ARight.Value;
end;

class operator TDimension.-(const ALeft, ARight: TSelf): TSelf;
begin
  result.Value := ALeft.Value - ARight.Value;
end;

class operator TDimension.*(const Factor: double; const AValue: TSelf): TSelf;
begin
  result.Value := Factor * AValue.Value;
end;
class operator TDimension.*(const AValue: TSelf; const Factor: double): TSelf;
begin
  result.Value := AValue.Value * Factor;
end;

class operator TDimension./(const AValue: TSelf; const Factor: double): TSelf;
begin
  result.Value := AValue.Value / Factor;
end;

class operator TDimension./(const ALeft, ARight: TSelf): double;
begin
  result := ALeft.Value / ARight.Value;
end;

class operator TDimension.=(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.Value = ARight.Value;
end;

class operator TDimension.<(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.Value < ARight.Value;
end;

class operator TDimension.>(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.Value > ARight.Value;
end;

class operator TDimension.<=(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.Value <= ARight.Value;
end;

class operator TDimension.>=(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.Value >= ARight.Value;
end;

class operator TDimension.mod(const ALeft, ARight: TSelf): TSelf;
begin
  result.Value := ALeft.Value mod ARight.Value;
end;

{ Units of Meter }

class function TMeter.GetSymbol: string;
begin
  result := 'm';
end;

{ Units of Meter2 }

class function TMeter2.GetSymbol: string;
begin
  result := 'm2';
end;

{ Units of Meter3 }

class function TMeter3.GetSymbol: string;
begin
  result := 'm3';
end;

{ Units of Meter4 }

class function TMeter4.GetSymbol: string;
begin
  result := 'm4';
end;

{ Units of Kilogram }

class function TKilogram.GetSymbol: string;
begin
  result := 'kg';
end;

{ Units of KilogramMeterPerSecond2 }

class function TKilogramMeterPerSecond2.GetSymbol: string;
begin
  result := 'kgm/s2';
end;

{ Units of KilogramPerMeterSecond2 }

class function TKilogramPerMeterSecond2.GetSymbol: string;
begin
  result := 'kg/ms2';
end;

{ Units of KilogramPerSecond2 }

class function TKilogramPerSecond2.GetSymbol: string;
begin
  result := 'kg/s2';
end;

{ Units of KilogramPerMeter3 }

class function TKilogramPerMeter3.GetSymbol: string;
begin
  result := 'kg/m3';
end;

{ Units of UnitPerSecond }

class function TUnitPerSecond.GetSymbol: string;
begin
  result := '1/s';
end;

{ Units of KilogramMeter2PerSecond2 }

class function TKilogramMeter2PerSecond2.GetSymbol: string;
begin
  result := 'kgm2/s2';
end;

{ Units of MeterPerSecond }

class function TMeterPerSecond.GetSymbol: string;
begin
  result := 'm/s';
end;

{ Units of Second }

class function TSecond.GetSymbol: string;
begin
  result := 's';
end;

{ Units of UnitPerMeter }

class function TUnitPerMeter.GetSymbol: string;
begin
  result := '1/m';
end;

{ Units of UnitPerMeter2 }

class function TUnitPerMeter2.GetSymbol: string;
begin
  result := '1/m2';
end;

{ Operators }

operator *(const ALeft: TLengthUnit; const ARight: TLengthUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TLength;     const ARight: TLength    ): TArea;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TLengthUnit; const ARight: TAreaUnit): TVolumeUnit;
begin
  result.Symbol := 'm3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TLength;     const ARight: TArea    ): TVolume;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAreaUnit; const ARight: TLengthUnit): TVolumeUnit;
begin
  result.Symbol := 'm3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TArea;     const ARight: TLength    ): TVolume;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TLengthUnit; const ARight: TVolumeUnit): TMomentOfAreaUnit;
begin
  result.Symbol := 'm4';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TLength;     const ARight: TVolume    ): TMomentOfArea;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TVolumeUnit; const ARight: TLengthUnit): TMomentOfAreaUnit;
begin
  result.Symbol := 'm4';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TVolume;     const ARight: TLength    ): TMomentOfArea;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TLengthUnit; const ARight: TForceUnit): TWorkUnit;
begin
  result.Symbol := 'kgm2/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TLength;     const ARight: TForce    ): TWork;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TForceUnit; const ARight: TLengthUnit): TWorkUnit;
begin
  result.Symbol := 'kgm2/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TForce;     const ARight: TLength    ): TWork;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TLengthUnit; const ARight: TPressureUnit): TStiffnessUnit;
begin
  result.Symbol := 'kg/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TLength;     const ARight: TPressure    ): TStiffness;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TPressureUnit; const ARight: TLengthUnit): TStiffnessUnit;
begin
  result.Symbol := 'kg/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TPressure;     const ARight: TLength    ): TStiffness;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TLengthUnit; const ARight: TStiffnessUnit): TForceUnit;
begin
  result.Symbol := 'kgm/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TLength;     const ARight: TStiffness    ): TForce;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TStiffnessUnit; const ARight: TLengthUnit): TForceUnit;
begin
  result.Symbol := 'kgm/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TStiffness;     const ARight: TLength    ): TForce;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TLengthUnit; const ARight: TFrequencyUnit): TSpeedUnit;
begin
  result.Symbol := 'm/s';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TLength;     const ARight: TFrequency    ): TSpeed;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TFrequencyUnit; const ARight: TLengthUnit): TSpeedUnit;
begin
  result.Symbol := 'm/s';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TFrequency;     const ARight: TLength    ): TSpeed;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TLengthUnit; const ARight: TInvAreaUnit): TInvLengthUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TLength;     const ARight: TInvArea    ): TInvLength;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TInvAreaUnit; const ARight: TLengthUnit): TInvLengthUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TInvArea;     const ARight: TLength    ): TInvLength;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TLengthUnit; const ARight: TAreaUnit): TInvLengthUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TLength;     const ARight: TArea    ): TInvLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TLengthUnit; const ARight: TVolumeUnit): TInvAreaUnit;
begin
  result.Symbol := '1/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TLength;     const ARight: TVolume    ): TInvArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TLengthUnit; const ARight: TSpeedUnit): TTimeUnit;
begin
  result.Symbol := 's';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TLength;     const ARight: TSpeed    ): TTime;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TLengthUnit; const ARight: TTimeUnit): TSpeedUnit;
begin
  result.Symbol := 'm/s';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TLength;     const ARight: TTime    ): TSpeed;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TLengthUnit; const ARight: TInvLengthUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TLength;     const ARight: TInvLength    ): TArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TLengthUnit; const ARight: TInvAreaUnit): TVolumeUnit;
begin
  result.Symbol := 'm3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TLength;     const ARight: TInvArea    ): TVolume;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TAreaUnit; const ARight: TAreaUnit): TMomentOfAreaUnit;
begin
  result.Symbol := 'm4';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TArea;     const ARight: TArea    ): TMomentOfArea;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAreaUnit; const ARight: TPressureUnit): TForceUnit;
begin
  result.Symbol := 'kgm/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TArea;     const ARight: TPressure    ): TForce;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TPressureUnit; const ARight: TAreaUnit): TForceUnit;
begin
  result.Symbol := 'kgm/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TPressure;     const ARight: TArea    ): TForce;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAreaUnit; const ARight: TStiffnessUnit): TWorkUnit;
begin
  result.Symbol := 'kgm2/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TArea;     const ARight: TStiffness    ): TWork;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TStiffnessUnit; const ARight: TAreaUnit): TWorkUnit;
begin
  result.Symbol := 'kgm2/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TStiffness;     const ARight: TArea    ): TWork;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TAreaUnit; const ARight: TLengthUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TArea;     const ARight: TLength    ): TLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TAreaUnit; const ARight: TVolumeUnit): TInvLengthUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TArea;     const ARight: TVolume    ): TInvLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TAreaUnit; const ARight: TMomentOfAreaUnit): TInvAreaUnit;
begin
  result.Symbol := '1/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TArea;     const ARight: TMomentOfArea    ): TInvArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TVolumeUnit; const ARight: TPressureUnit): TWorkUnit;
begin
  result.Symbol := 'kgm2/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TVolume;     const ARight: TPressure    ): TWork;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TPressureUnit; const ARight: TVolumeUnit): TWorkUnit;
begin
  result.Symbol := 'kgm2/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TPressure;     const ARight: TVolume    ): TWork;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TVolumeUnit; const ARight: TDensityUnit): TMassUnit;
begin
  result.Symbol := 'kg';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TVolume;     const ARight: TDensity    ): TMass;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TDensityUnit; const ARight: TVolumeUnit): TMassUnit;
begin
  result.Symbol := 'kg';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TDensity;     const ARight: TVolume    ): TMass;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TVolumeUnit; const ARight: TLengthUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TVolume;     const ARight: TLength    ): TArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TVolumeUnit; const ARight: TAreaUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TVolume;     const ARight: TArea    ): TLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TVolumeUnit; const ARight: TMomentOfAreaUnit): TInvLengthUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TVolume;     const ARight: TMomentOfArea    ): TInvLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMomentOfAreaUnit; const ARight: TLengthUnit): TVolumeUnit;
begin
  result.Symbol := 'm3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMomentOfArea;     const ARight: TLength    ): TVolume;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMomentOfAreaUnit; const ARight: TAreaUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMomentOfArea;     const ARight: TArea    ): TArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMomentOfAreaUnit; const ARight: TVolumeUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMomentOfArea;     const ARight: TVolume    ): TLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMassUnit; const ARight: TVolumeUnit): TDensityUnit;
begin
  result.Symbol := 'kg/m3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMass;     const ARight: TVolume    ): TDensity;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMassUnit; const ARight: TDensityUnit): TVolumeUnit;
begin
  result.Symbol := 'm3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMass;     const ARight: TDensity    ): TVolume;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TForceUnit; const ARight: TInvAreaUnit): TPressureUnit;
begin
  result.Symbol := 'kg/ms2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TForce;     const ARight: TInvArea    ): TPressure;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TInvAreaUnit; const ARight: TForceUnit): TPressureUnit;
begin
  result.Symbol := 'kg/ms2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TInvArea;     const ARight: TForce    ): TPressure;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TForceUnit; const ARight: TLengthUnit): TStiffnessUnit;
begin
  result.Symbol := 'kg/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TForce;     const ARight: TLength    ): TStiffness;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TForceUnit; const ARight: TAreaUnit): TPressureUnit;
begin
  result.Symbol := 'kg/ms2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TForce;     const ARight: TArea    ): TPressure;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TForceUnit; const ARight: TPressureUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TForce;     const ARight: TPressure    ): TArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TForceUnit; const ARight: TStiffnessUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TForce;     const ARight: TStiffness    ): TLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TForceUnit; const ARight: TWorkUnit): TInvLengthUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TForce;     const ARight: TWork    ): TInvLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TPressureUnit; const ARight: TForceUnit): TInvAreaUnit;
begin
  result.Symbol := '1/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TPressure;     const ARight: TForce    ): TInvArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TPressureUnit; const ARight: TStiffnessUnit): TInvLengthUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TPressure;     const ARight: TStiffness    ): TInvLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TStiffnessUnit; const ARight: TLengthUnit): TPressureUnit;
begin
  result.Symbol := 'kg/ms2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TStiffness;     const ARight: TLength    ): TPressure;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TStiffnessUnit; const ARight: TForceUnit): TInvLengthUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TStiffness;     const ARight: TForce    ): TInvLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TStiffnessUnit; const ARight: TPressureUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TStiffness;     const ARight: TPressure    ): TLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TStiffnessUnit; const ARight: TWorkUnit): TInvAreaUnit;
begin
  result.Symbol := '1/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TStiffness;     const ARight: TWork    ): TInvArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TFrequencyUnit; const ARight: TSpeedUnit): TInvLengthUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TFrequency;     const ARight: TSpeed    ): TInvLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWorkUnit; const ARight: TLengthUnit): TForceUnit;
begin
  result.Symbol := 'kgm/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TWork;     const ARight: TLength    ): TForce;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWorkUnit; const ARight: TAreaUnit): TStiffnessUnit;
begin
  result.Symbol := 'kg/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TWork;     const ARight: TArea    ): TStiffness;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWorkUnit; const ARight: TVolumeUnit): TPressureUnit;
begin
  result.Symbol := 'kg/ms2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TWork;     const ARight: TVolume    ): TPressure;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWorkUnit; const ARight: TForceUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TWork;     const ARight: TForce    ): TLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWorkUnit; const ARight: TPressureUnit): TVolumeUnit;
begin
  result.Symbol := 'm3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TWork;     const ARight: TPressure    ): TVolume;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWorkUnit; const ARight: TStiffnessUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TWork;     const ARight: TStiffness    ): TArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSpeedUnit; const ARight: TTimeUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TSpeed;     const ARight: TTime    ): TLength;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TTimeUnit; const ARight: TSpeedUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TTime;     const ARight: TSpeed    ): TLength;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSpeedUnit; const ARight: TLengthUnit): TFrequencyUnit;
begin
  result.Symbol := '1/s';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TSpeed;     const ARight: TLength    ): TFrequency;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSpeedUnit; const ARight: TFrequencyUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TSpeed;     const ARight: TFrequency    ): TLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TInvLengthUnit; const ARight: TLengthUnit): TInvAreaUnit;
begin
  result.Symbol := '1/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TInvLength;     const ARight: TLength    ): TInvArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;


end.
