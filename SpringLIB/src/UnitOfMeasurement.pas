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
    class operator +  (const v1, v2: TSelf): TSelf;
    class operator -  (const v1, v2: TSelf): TSelf;
    class operator *  (const Factor: double; const AValue: TSelf): TSelf;
    class operator *  (const AValue: TSelf; const Factor: double): TSelf;
    class operator /  (const AValue: TSelf; const Factor: double): TSelf;
    class operator /  (const v1, v2: TSelf): double;
    class operator =  (const v1, v2: TSelf): boolean;
    class operator <  (const v1, v2: TSelf): boolean;
    class operator >  (const v1, v2: TSelf): boolean;
    class operator <= (const v1, v2: TSelf): boolean;
    class operator >= (const v1, v2: TSelf): boolean;
    class operator mod(const v1, v2: TSelf): TSelf;
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
  km     : specialize TDimensionUnit<TMeter> = (Factor: 1000           ; Symbol: 'km');
  hm     : specialize TDimensionUnit<TMeter> = (Factor: 100            ; Symbol: 'hm');
  dam    : specialize TDimensionUnit<TMeter> = (Factor: 10             ; Symbol: 'dam');
  m      : specialize TDimensionUnit<TMeter> = (Factor: 1              ; Symbol: 'm');
  dm     : specialize TDimensionUnit<TMeter> = (Factor: 1/10           ; Symbol: 'dm');
  cm     : specialize TDimensionUnit<TMeter> = (Factor: 1/100          ; Symbol: 'cm');
  mm     : specialize TDimensionUnit<TMeter> = (Factor: 1/1000         ; Symbol: 'mm');
  yd     : specialize TDimensionUnit<TMeter> = (Factor: 1143/1250      ; Symbol: 'yd');
  ft     : specialize TDimensionUnit<TMeter> = (Factor: 381/1250       ; Symbol: 'ft');
  inch   : specialize TDimensionUnit<TMeter> = (Factor: 127/5000       ; Symbol: 'in');

{ Units of Meter2 }

type
  TMeter2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TArea     = specialize TDimension    <TMeter2>;
  TAreaUnit = specialize TDimensionUnit<TMeter2>;

const
  km2    : specialize TDimensionUnit<TMeter2> = (Factor: 1000000        ; Symbol: 'km2');
  hm2    : specialize TDimensionUnit<TMeter2> = (Factor: 10000          ; Symbol: 'hm2');
  dam2   : specialize TDimensionUnit<TMeter2> = (Factor: 100            ; Symbol: 'dam2');
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
  km3    : specialize TDimensionUnit<TMeter3> = (Factor: 1000000000     ; Symbol: 'km3');
  hm3    : specialize TDimensionUnit<TMeter3> = (Factor: 1000000        ; Symbol: 'hm3');
  dam3   : specialize TDimensionUnit<TMeter3> = (Factor: 1000           ; Symbol: 'dam3');
  m3     : specialize TDimensionUnit<TMeter3> = (Factor: 1              ; Symbol: 'm3');
  dm3    : specialize TDimensionUnit<TMeter3> = (Factor: 1/1000         ; Symbol: 'dm3');
  cm3    : specialize TDimensionUnit<TMeter3> = (Factor: 1/1000000      ; Symbol: 'cm3');
  mm3    : specialize TDimensionUnit<TMeter3> = (Factor: 1/1000000000   ; Symbol: 'mm3');
  kl     : specialize TDimensionUnit<TMeter3> = (Factor: 1              ; Symbol: 'kl');
  hl     : specialize TDimensionUnit<TMeter3> = (Factor: 1/10           ; Symbol: 'hl');
  dal    : specialize TDimensionUnit<TMeter3> = (Factor: 1/100          ; Symbol: 'dal');
  l      : specialize TDimensionUnit<TMeter3> = (Factor: 1/1000         ; Symbol: 'l');
  dl     : specialize TDimensionUnit<TMeter3> = (Factor: 1/10000        ; Symbol: 'dl');
  cl     : specialize TDimensionUnit<TMeter3> = (Factor: 1/100000       ; Symbol: 'cl');
  ml     : specialize TDimensionUnit<TMeter3> = (Factor: 1/1000000      ; Symbol: 'ml');

{ Units of Meter4 }

type
  TMeter4 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TAreaSquare     = specialize TDimension    <TMeter4>;
  TAreaSquareUnit = specialize TDimensionUnit<TMeter4>;

const
  km4    : specialize TDimensionUnit<TMeter4> = (Factor: 1000000000000  ; Symbol: 'km4');
  hm4    : specialize TDimensionUnit<TMeter4> = (Factor: 100000000      ; Symbol: 'hm4');
  dam4   : specialize TDimensionUnit<TMeter4> = (Factor: 10000          ; Symbol: 'dam4');
  m4     : specialize TDimensionUnit<TMeter4> = (Factor: 1              ; Symbol: 'm4');
  dm4    : specialize TDimensionUnit<TMeter4> = (Factor: 1/10000        ; Symbol: 'dm4');
  cm4    : specialize TDimensionUnit<TMeter4> = (Factor: 1/100000000    ; Symbol: 'cm4');
  mm4    : specialize TDimensionUnit<TMeter4> = (Factor: 1/1000000000000; Symbol: 'mm4');

{ Units of UnitPerSecond }

type
  TUnitPerSecond = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TFrequency     = specialize TDimension    <TUnitPerSecond>;
  TFrequencyUnit = specialize TDimensionUnit<TUnitPerSecond>;

const
  GHz    : specialize TDimensionUnit<TUnitPerSecond> = (Factor: 1000000000     ; Symbol: 'GHz');
  MHz    : specialize TDimensionUnit<TUnitPerSecond> = (Factor: 1000000        ; Symbol: 'MHz');
  kHz    : specialize TDimensionUnit<TUnitPerSecond> = (Factor: 1000           ; Symbol: 'kHz');
  hHz    : specialize TDimensionUnit<TUnitPerSecond> = (Factor: 100            ; Symbol: 'hHz');
  daHz   : specialize TDimensionUnit<TUnitPerSecond> = (Factor: 10             ; Symbol: 'daHz');
  Hz     : specialize TDimensionUnit<TUnitPerSecond> = (Factor: 1              ; Symbol: 'Hz');

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
  N_mm2  : specialize TDimensionUnit<TKilogramPerMeterSecond2> = (Factor: 1000000        ; Symbol: 'N/mm2');
  N_cm2  : specialize TDimensionUnit<TKilogramPerMeterSecond2> = (Factor: 10000          ; Symbol: 'N/cm2');
  N_m2   : specialize TDimensionUnit<TKilogramPerMeterSecond2> = (Factor: 1              ; Symbol: 'N/m2');

{ Units of KilogramMeter2PerSecond2 }

type
  TKilogramMeter2PerSecond2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TWork     = specialize TDimension    <TKilogramMeter2PerSecond2>;
  TWorkUnit = specialize TDimensionUnit<TKilogramMeter2PerSecond2>;

const
  kNm    : specialize TDimensionUnit<TKilogramMeter2PerSecond2> = (Factor: 1000           ; Symbol: 'kNm');
  hNm    : specialize TDimensionUnit<TKilogramMeter2PerSecond2> = (Factor: 100            ; Symbol: 'hNm');
  daNm   : specialize TDimensionUnit<TKilogramMeter2PerSecond2> = (Factor: 10             ; Symbol: 'daNm');
  Nm     : specialize TDimensionUnit<TKilogramMeter2PerSecond2> = (Factor: 1              ; Symbol: 'Nm');
  dNm    : specialize TDimensionUnit<TKilogramMeter2PerSecond2> = (Factor: 1/10           ; Symbol: 'dNm');
  cNm    : specialize TDimensionUnit<TKilogramMeter2PerSecond2> = (Factor: 1/100          ; Symbol: 'cNm');
  mNm    : specialize TDimensionUnit<TKilogramMeter2PerSecond2> = (Factor: 1/1000         ; Symbol: 'mNm');
  Nmm    : specialize TDimensionUnit<TKilogramMeter2PerSecond2> = (Factor: 1/1000         ; Symbol: 'Nmm');

{ Units of KilogramMeter2PerSecond3 }

type
  TKilogramMeter2PerSecond3 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TPower     = specialize TDimension    <TKilogramMeter2PerSecond3>;
  TPowerUnit = specialize TDimensionUnit<TKilogramMeter2PerSecond3>;

{ Units of MeterPerSecond }

type
  TMeterPerSecond = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TSpeed     = specialize TDimension    <TMeterPerSecond>;
  TSpeedUnit = specialize TDimensionUnit<TMeterPerSecond>;

const
  km_h   : specialize TDimensionUnit<TMeterPerSecond> = (Factor: 5/18           ; Symbol: 'km/h');
  m_s    : specialize TDimensionUnit<TMeterPerSecond> = (Factor: 1              ; Symbol: 'm/s');

{ Units of Meter2PerSecond2 }

type
  TMeter2PerSecond2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TSpeedSquare     = specialize TDimension    <TMeter2PerSecond2>;
  TSpeedSquareUnit = specialize TDimensionUnit<TMeter2PerSecond2>;

{ Units of MeterPerSecond2 }

type
  TMeterPerSecond2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TAcceleration     = specialize TDimension    <TMeterPerSecond2>;
  TAccelerationUnit = specialize TDimensionUnit<TMeterPerSecond2>;

const
  km_h_s : specialize TDimensionUnit<TMeterPerSecond2> = (Factor: 5/18           ; Symbol: 'km/h/s');
  km_s2  : specialize TDimensionUnit<TMeterPerSecond2> = (Factor: 1000           ; Symbol: 'm/s2');

{ Units of Meter2PerSecond4 }

type
  TMeter2PerSecond4 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TAccelerationSquare     = specialize TDimension    <TMeter2PerSecond4>;
  TAccelerationSquareUnit = specialize TDimensionUnit<TMeter2PerSecond4>;

{ Units of KilogramMeter2PerSecond2 }

type
  TTorque     = specialize TDimension    <TKilogramMeter2PerSecond2>;
  TTorqueUnit = specialize TDimensionUnit<TKilogramMeter2PerSecond2>;

{ Units of KilogramPerMeter }

type
  TKilogramPerMeter = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TDensity1D     = specialize TDimension    <TKilogramPerMeter>;
  TDensity1DUnit = specialize TDimensionUnit<TKilogramPerMeter>;

{ Units of KilogramPerMeter2 }

type
  TKilogramPerMeter2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TDensity2D     = specialize TDimension    <TKilogramPerMeter2>;
  TDensity2DUnit = specialize TDimensionUnit<TKilogramPerMeter2>;

{ Units of KilogramPerMeter3 }

type
  TKilogramPerMeter3 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TDensity3D     = specialize TDimension    <TKilogramPerMeter3>;
  TDensity3DUnit = specialize TDimensionUnit<TKilogramPerMeter3>;

const
  kg_m3  : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 1              ; Symbol: 'kg/m3');
  hg_m3  : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 1/10           ; Symbol: 'hg/m3');
  dag_m3 : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 1/100          ; Symbol: 'dag/m3');
  g_m3   : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 1/1000         ; Symbol: 'g/m3');
  dg_m3  : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 1/10000        ; Symbol: 'dg/m3');
  cg_m3  : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 1/100000       ; Symbol: 'cg/m3');
  mg_m3  : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 1/1000000      ; Symbol: 'mg/m3');
  kg_dm3 : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 1000           ; Symbol: 'kg/dm3');
  hg_dm3 : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 100            ; Symbol: 'hg/dm3');
  dag_dm3 : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 10             ; Symbol: 'dag/dm3');
  g_dm3  : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 1              ; Symbol: 'g/dm3');
  dg_dm3 : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 1/10           ; Symbol: 'dg/dm3');
  cg_dm3 : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 1/100          ; Symbol: 'cg/dm3');
  mg_dm3 : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 1/1000         ; Symbol: 'mg/dm3');
  kg_cm3 : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 1000000        ; Symbol: 'kg/cm3');
  hg_cm3 : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 100000         ; Symbol: 'hg/cm3');
  dag_cm3 : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 10000          ; Symbol: 'dag/cm3');
  g_cm3  : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 1000           ; Symbol: 'g/cm3');
  dg_cm3 : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 100            ; Symbol: 'dg/cm3');
  cg_cm3 : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 10             ; Symbol: 'cg/cm3');
  mg_cm3 : specialize TDimensionUnit<TKilogramPerMeter3> = (Factor: 1              ; Symbol: 'mg/cm3');

{ Units of UnitPerSecond2 }

type
  TUnitPerSecond2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TFrequencySquare     = specialize TDimension    <TUnitPerSecond2>;
  TFrequencySquareUnit = specialize TDimensionUnit<TUnitPerSecond2>;

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
  dg     : specialize TDimensionUnit<TKilogram> = (Factor: 1/10000        ; Symbol: 'dg');
  cg     : specialize TDimensionUnit<TKilogram> = (Factor: 1/100000       ; Symbol: 'cg');
  mg     : specialize TDimensionUnit<TKilogram> = (Factor: 1/1000000      ; Symbol: 'mg');

{ Units of KilogramPerSecond2 }

type
  TKilogramPerSecond2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TStiffness     = specialize TDimension    <TKilogramPerSecond2>;
  TStiffnessUnit = specialize TDimensionUnit<TKilogramPerSecond2>;

const
  N_km   : specialize TDimensionUnit<TKilogramPerSecond2> = (Factor: 1/1000         ; Symbol: 'N/km');
  N_hm   : specialize TDimensionUnit<TKilogramPerSecond2> = (Factor: 1/100          ; Symbol: 'N/hm');
  N_dam  : specialize TDimensionUnit<TKilogramPerSecond2> = (Factor: 1/10           ; Symbol: 'N/dam');
  N_m    : specialize TDimensionUnit<TKilogramPerSecond2> = (Factor: 1              ; Symbol: 'N/m');
  N_dm   : specialize TDimensionUnit<TKilogramPerSecond2> = (Factor: 10             ; Symbol: 'N/dm');
  N_cm   : specialize TDimensionUnit<TKilogramPerSecond2> = (Factor: 100            ; Symbol: 'N/cm');
  N_mm   : specialize TDimensionUnit<TKilogramPerSecond2> = (Factor: 1000           ; Symbol: 'N/mm');
  kN_m   : specialize TDimensionUnit<TKilogramPerSecond2> = (Factor: 1000           ; Symbol: 'kN/m');
  hN_m   : specialize TDimensionUnit<TKilogramPerSecond2> = (Factor: 100            ; Symbol: 'hN/m');
  daN_m  : specialize TDimensionUnit<TKilogramPerSecond2> = (Factor: 10             ; Symbol: 'daN/m');
  dN_m   : specialize TDimensionUnit<TKilogramPerSecond2> = (Factor: 1/10           ; Symbol: 'dN/m');
  cN_m   : specialize TDimensionUnit<TKilogramPerSecond2> = (Factor: 1/100          ; Symbol: 'cN/m');
  mN_m   : specialize TDimensionUnit<TKilogramPerSecond2> = (Factor: 1/1000         ; Symbol: 'mN/m');

{ Units of SecondAmpere }

type
  TSecondAmpere = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TElettricCharge     = specialize TDimension    <TSecondAmpere>;
  TElettricChargeUnit = specialize TDimensionUnit<TSecondAmpere>;

{ Units of KilogramMeter2PerAmpereSecond3 }

type
  TKilogramMeter2PerAmpereSecond3 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TEletricPotential     = specialize TDimension    <TKilogramMeter2PerAmpereSecond3>;
  TEletricPotentialUnit = specialize TDimensionUnit<TKilogramMeter2PerAmpereSecond3>;

{ Units of Second4Ampere2PerKilogramMeter2 }

type
  TSecond4Ampere2PerKilogramMeter2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TCapacitance     = specialize TDimension    <TSecond4Ampere2PerKilogramMeter2>;
  TCapacitanceUnit = specialize TDimensionUnit<TSecond4Ampere2PerKilogramMeter2>;

{ Units of KilogramMeter2PerSecond3Ampere2 }

type
  TKilogramMeter2PerSecond3Ampere2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TResistance     = specialize TDimension    <TKilogramMeter2PerSecond3Ampere2>;
  TResistanceUnit = specialize TDimensionUnit<TKilogramMeter2PerSecond3Ampere2>;

{ Units of Second3Ampere2PerKilogramMeter2 }

type
  TSecond3Ampere2PerKilogramMeter2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TEletricalConductance     = specialize TDimension    <TSecond3Ampere2PerKilogramMeter2>;
  TEletricalConductanceUnit = specialize TDimensionUnit<TSecond3Ampere2PerKilogramMeter2>;

{ Units of KilogramMeter2PerAmpereSecond2 }

type
  TKilogramMeter2PerAmpereSecond2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TMagneticFlux     = specialize TDimension    <TKilogramMeter2PerAmpereSecond2>;
  TMagneticFluxUnit = specialize TDimensionUnit<TKilogramMeter2PerAmpereSecond2>;

{ Units of KilogramPerAmpereSecond2 }

type
  TKilogramPerAmpereSecond2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TMagneticFluxDensity     = specialize TDimension    <TKilogramPerAmpereSecond2>;
  TMagneticFluxDensityUnit = specialize TDimensionUnit<TKilogramPerAmpereSecond2>;

{ Units of KilogramMeter2PerSecond2Ampere2 }

type
  TKilogramMeter2PerSecond2Ampere2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TInductance     = specialize TDimension    <TKilogramMeter2PerSecond2Ampere2>;
  TInductanceUnit = specialize TDimensionUnit<TKilogramMeter2PerSecond2Ampere2>;

{ Units of Kelvin }

type
  TKelvin = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TTemperature     = specialize TDimension    <TKelvin>;
  TTemperatureUnit = specialize TDimensionUnit<TKelvin>;

{ Units of Candela }

type
  TCandela = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TLuminousFlux     = specialize TDimension    <TCandela>;
  TLuminousFluxUnit = specialize TDimensionUnit<TCandela>;

{ Units of CandelaPerMeter2 }

type
  TCandelaPerMeter2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TIlluminance     = specialize TDimension    <TCandelaPerMeter2>;
  TIlluminanceUnit = specialize TDimensionUnit<TCandelaPerMeter2>;

{ Units of Meter2PerSecond2 }

type
  TAbsorbedDose     = specialize TDimension    <TMeter2PerSecond2>;
  TAbsorbedDoseUnit = specialize TDimensionUnit<TMeter2PerSecond2>;

{ Units of Meter2PerSecond2 }

type
  TEquivalentDose     = specialize TDimension    <TMeter2PerSecond2>;
  TEquivalentDoseUnit = specialize TDimensionUnit<TMeter2PerSecond2>;

{ Units of MolePerSecond }

type
  TMolePerSecond = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TCatalyticActivity     = specialize TDimension    <TMolePerSecond>;
  TCatalyticActivityUnit = specialize TDimensionUnit<TMolePerSecond>;

{ Units of UnitPerMeter }

type
  TUnitPerMeter = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TWaveNumber     = specialize TDimension    <TUnitPerMeter>;
  TWaveNumberUnit = specialize TDimensionUnit<TUnitPerMeter>;

{ Units of Meter3PerKilogram }

type
  TMeter3PerKilogram = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TSpecificVolume     = specialize TDimension    <TMeter3PerKilogram>;
  TSpecificVolumeUnit = specialize TDimensionUnit<TMeter3PerKilogram>;

{ Units of AmperePerMeter2 }

type
  TAmperePerMeter2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TCurrentDensity     = specialize TDimension    <TAmperePerMeter2>;
  TCurrentDensityUnit = specialize TDimensionUnit<TAmperePerMeter2>;

{ Units of Second }

type
  TSecond = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TTime     = specialize TDimension    <TSecond>;
  TTimeUnit = specialize TDimensionUnit<TSecond>;

const
  day    : specialize TDimensionUnit<TSecond> = (Factor: 60*60*24       ; Symbol: 'day');
  hr     : specialize TDimensionUnit<TSecond> = (Factor: 60*60          ; Symbol: 'hr');
  mn     : specialize TDimensionUnit<TSecond> = (Factor: 60             ; Symbol: 'mn');
  s      : specialize TDimensionUnit<TSecond> = (Factor: 1              ; Symbol: 's');
  ds     : specialize TDimensionUnit<TSecond> = (Factor: 1/10           ; Symbol: 'ds');
  cs     : specialize TDimensionUnit<TSecond> = (Factor: 1/100          ; Symbol: 'cs');
  ms     : specialize TDimensionUnit<TSecond> = (Factor: 1/1000         ; Symbol: 'ms');

{ Units of Second2 }

type
  TSecond2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TTimeSquare     = specialize TDimension    <TSecond2>;
  TTimeSquareUnit = specialize TDimensionUnit<TSecond2>;

const
  day2   : specialize TDimensionUnit<TSecond2> = (Factor: 7464960000     ; Symbol: 'day2');
  hr2    : specialize TDimensionUnit<TSecond2> = (Factor: 12960000       ; Symbol: 'hr2');
  mn2    : specialize TDimensionUnit<TSecond2> = (Factor: 3600           ; Symbol: 'mn2');
  s2     : specialize TDimensionUnit<TSecond2> = (Factor: 1              ; Symbol: 's2');
  ds2    : specialize TDimensionUnit<TSecond2> = (Factor: 1/100          ; Symbol: 'ds2');
  cs2    : specialize TDimensionUnit<TSecond2> = (Factor: 1/10000        ; Symbol: 'cs2');
  ms2    : specialize TDimensionUnit<TSecond2> = (Factor: 1/1000000      ; Symbol: 'ms2');

{ Units of Second3 }

type
  TSecond3 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TTimeCube     = specialize TDimension    <TSecond3>;
  TTimeCubeUnit = specialize TDimensionUnit<TSecond3>;

const
  day3   : specialize TDimensionUnit<TSecond3> = (Factor: 644972544000000; Symbol: 'day3');
  hr3    : specialize TDimensionUnit<TSecond3> = (Factor: 46656000000    ; Symbol: 'hr3');
  mn3    : specialize TDimensionUnit<TSecond3> = (Factor: 216000         ; Symbol: 'mn3');
  s3     : specialize TDimensionUnit<TSecond3> = (Factor: 1              ; Symbol: 's3');
  ds3    : specialize TDimensionUnit<TSecond3> = (Factor: 1/1000         ; Symbol: 'ds3');
  cs3    : specialize TDimensionUnit<TSecond3> = (Factor: 1/1000000      ; Symbol: 'cs3');
  ms3    : specialize TDimensionUnit<TSecond3> = (Factor: 1/1000000000   ; Symbol: 'ms3');

{ Units of UnitPerMeter3 }

type
  TUnitPerMeter3 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TInvVolume     = specialize TDimension    <TUnitPerMeter3>;
  TInvVolumeUnit = specialize TDimensionUnit<TUnitPerMeter3>;

{ Units of UnitPerMeter2 }

type
  TUnitPerMeter2 = class(TBaseUnit)
    class function GetSymbol: string; override;
  end;
  TInvArea     = specialize TDimension    <TUnitPerMeter2>;
  TInvAreaUnit = specialize TDimensionUnit<TUnitPerMeter2>;

{ Units of UnitPerMeter }

type
  TInvLength     = specialize TDimension    <TUnitPerMeter>;
  TInvLengthUnit = specialize TDimensionUnit<TUnitPerMeter>;

{ Operators }

operator *(const ALeft: TLengthUnit; const ARight: TLengthUnit): TAreaUnit; inline;
operator *(const ALeft: TLength;     const ARight: TLength    ): TArea; inline;
operator *(const ALeft: TLengthUnit; const ARight: TAreaUnit): TVolumeUnit; inline;
operator *(const ALeft: TLength;     const ARight: TArea    ): TVolume; inline;
operator *(const ALeft: TAreaUnit; const ARight: TLengthUnit): TVolumeUnit; inline;
operator *(const ALeft: TArea;     const ARight: TLength    ): TVolume; inline;
operator *(const ALeft: TLengthUnit; const ARight: TVolumeUnit): TAreaSquareUnit; inline;
operator *(const ALeft: TLength;     const ARight: TVolume    ): TAreaSquare; inline;
operator *(const ALeft: TVolumeUnit; const ARight: TLengthUnit): TAreaSquareUnit; inline;
operator *(const ALeft: TVolume;     const ARight: TLength    ): TAreaSquare; inline;
operator *(const ALeft: TLengthUnit; const ARight: TFrequencyUnit): TSpeedUnit; inline;
operator *(const ALeft: TLength;     const ARight: TFrequency    ): TSpeed; inline;
operator *(const ALeft: TFrequencyUnit; const ARight: TLengthUnit): TSpeedUnit; inline;
operator *(const ALeft: TFrequency;     const ARight: TLength    ): TSpeed; inline;
operator *(const ALeft: TLengthUnit; const ARight: TForceUnit): TWorkUnit; inline;
operator *(const ALeft: TLength;     const ARight: TForce    ): TWork; inline;
operator *(const ALeft: TForceUnit; const ARight: TLengthUnit): TWorkUnit; inline;
operator *(const ALeft: TForce;     const ARight: TLength    ): TWork; inline;
operator *(const ALeft: TLengthUnit; const ARight: TPressureUnit): TStiffnessUnit; inline;
operator *(const ALeft: TLength;     const ARight: TPressure    ): TStiffness; inline;
operator *(const ALeft: TPressureUnit; const ARight: TLengthUnit): TStiffnessUnit; inline;
operator *(const ALeft: TPressure;     const ARight: TLength    ): TStiffness; inline;
operator *(const ALeft: TLengthUnit; const ARight: TAccelerationUnit): TSpeedSquareUnit; inline;
operator *(const ALeft: TLength;     const ARight: TAcceleration    ): TSpeedSquare; inline;
operator *(const ALeft: TAccelerationUnit; const ARight: TLengthUnit): TSpeedSquareUnit; inline;
operator *(const ALeft: TAcceleration;     const ARight: TLength    ): TSpeedSquare; inline;
operator *(const ALeft: TLengthUnit; const ARight: TDensity1DUnit): TMassUnit; inline;
operator *(const ALeft: TLength;     const ARight: TDensity1D    ): TMass; inline;
operator *(const ALeft: TDensity1DUnit; const ARight: TLengthUnit): TMassUnit; inline;
operator *(const ALeft: TDensity1D;     const ARight: TLength    ): TMass; inline;
operator *(const ALeft: TLengthUnit; const ARight: TDensity2DUnit): TDensity1DUnit; inline;
operator *(const ALeft: TLength;     const ARight: TDensity2D    ): TDensity1D; inline;
operator *(const ALeft: TDensity2DUnit; const ARight: TLengthUnit): TDensity1DUnit; inline;
operator *(const ALeft: TDensity2D;     const ARight: TLength    ): TDensity1D; inline;
operator *(const ALeft: TLengthUnit; const ARight: TDensity3DUnit): TDensity2DUnit; inline;
operator *(const ALeft: TLength;     const ARight: TDensity3D    ): TDensity2D; inline;
operator *(const ALeft: TDensity3DUnit; const ARight: TLengthUnit): TDensity2DUnit; inline;
operator *(const ALeft: TDensity3D;     const ARight: TLength    ): TDensity2D; inline;
operator *(const ALeft: TLengthUnit; const ARight: TFrequencySquareUnit): TAccelerationUnit; inline;
operator *(const ALeft: TLength;     const ARight: TFrequencySquare    ): TAcceleration; inline;
operator *(const ALeft: TFrequencySquareUnit; const ARight: TLengthUnit): TAccelerationUnit; inline;
operator *(const ALeft: TFrequencySquare;     const ARight: TLength    ): TAcceleration; inline;
operator *(const ALeft: TLengthUnit; const ARight: TStiffnessUnit): TForceUnit; inline;
operator *(const ALeft: TLength;     const ARight: TStiffness    ): TForce; inline;
operator *(const ALeft: TStiffnessUnit; const ARight: TLengthUnit): TForceUnit; inline;
operator *(const ALeft: TStiffness;     const ARight: TLength    ): TForce; inline;
operator *(const ALeft: TLengthUnit; const ARight: TInvVolumeUnit): TInvAreaUnit; inline;
operator *(const ALeft: TLength;     const ARight: TInvVolume    ): TInvArea; inline;
operator *(const ALeft: TInvVolumeUnit; const ARight: TLengthUnit): TInvAreaUnit; inline;
operator *(const ALeft: TInvVolume;     const ARight: TLength    ): TInvArea; inline;
operator *(const ALeft: TLengthUnit; const ARight: TInvAreaUnit): TWaveNumberUnit; inline;
operator *(const ALeft: TLength;     const ARight: TInvArea    ): TWaveNumber; inline;
operator *(const ALeft: TInvAreaUnit; const ARight: TLengthUnit): TWaveNumberUnit; inline;
operator *(const ALeft: TInvArea;     const ARight: TLength    ): TWaveNumber; inline;
operator /(const ALeft: TLengthUnit; const ARight: TAreaUnit): TWaveNumberUnit; inline;
operator /(const ALeft: TLength;     const ARight: TArea    ): TWaveNumber; inline;
operator /(const ALeft: TLengthUnit; const ARight: TVolumeUnit): TInvAreaUnit; inline;
operator /(const ALeft: TLength;     const ARight: TVolume    ): TInvArea; inline;
operator /(const ALeft: TLengthUnit; const ARight: TAreaSquareUnit): TInvVolumeUnit; inline;
operator /(const ALeft: TLength;     const ARight: TAreaSquare    ): TInvVolume; inline;
operator /(const ALeft: TLengthUnit; const ARight: TSpeedUnit): TTimeUnit; inline;
operator /(const ALeft: TLength;     const ARight: TSpeed    ): TTime; inline;
operator /(const ALeft: TLengthUnit; const ARight: TAccelerationUnit): TTimeSquareUnit; inline;
operator /(const ALeft: TLength;     const ARight: TAcceleration    ): TTimeSquare; inline;
operator /(const ALeft: TLengthUnit; const ARight: TDensity2DUnit): TSpecificVolumeUnit; inline;
operator /(const ALeft: TLength;     const ARight: TDensity2D    ): TSpecificVolume; inline;
operator /(const ALeft: TLengthUnit; const ARight: TWaveNumberUnit): TAreaUnit; inline;
operator /(const ALeft: TLength;     const ARight: TWaveNumber    ): TArea; inline;
operator /(const ALeft: TLengthUnit; const ARight: TSpecificVolumeUnit): TDensity2DUnit; inline;
operator /(const ALeft: TLength;     const ARight: TSpecificVolume    ): TDensity2D; inline;
operator /(const ALeft: TLengthUnit; const ARight: TTimeUnit): TSpeedUnit; inline;
operator /(const ALeft: TLength;     const ARight: TTime    ): TSpeed; inline;
operator /(const ALeft: TLengthUnit; const ARight: TTimeSquareUnit): TAccelerationUnit; inline;
operator /(const ALeft: TLength;     const ARight: TTimeSquare    ): TAcceleration; inline;
operator /(const ALeft: TLengthUnit; const ARight: TInvVolumeUnit): TAreaSquareUnit; inline;
operator /(const ALeft: TLength;     const ARight: TInvVolume    ): TAreaSquare; inline;
operator /(const ALeft: TLengthUnit; const ARight: TInvAreaUnit): TVolumeUnit; inline;
operator /(const ALeft: TLength;     const ARight: TInvArea    ): TVolume; inline;
operator *(const ALeft: TAreaUnit; const ARight: TAreaUnit): TAreaSquareUnit; inline;
operator *(const ALeft: TArea;     const ARight: TArea    ): TAreaSquare; inline;
operator *(const ALeft: TAreaUnit; const ARight: TPressureUnit): TForceUnit; inline;
operator *(const ALeft: TArea;     const ARight: TPressure    ): TForce; inline;
operator *(const ALeft: TPressureUnit; const ARight: TAreaUnit): TForceUnit; inline;
operator *(const ALeft: TPressure;     const ARight: TArea    ): TForce; inline;
operator *(const ALeft: TAreaUnit; const ARight: TDensity2DUnit): TMassUnit; inline;
operator *(const ALeft: TArea;     const ARight: TDensity2D    ): TMass; inline;
operator *(const ALeft: TDensity2DUnit; const ARight: TAreaUnit): TMassUnit; inline;
operator *(const ALeft: TDensity2D;     const ARight: TArea    ): TMass; inline;
operator *(const ALeft: TAreaUnit; const ARight: TDensity3DUnit): TDensity1DUnit; inline;
operator *(const ALeft: TArea;     const ARight: TDensity3D    ): TDensity1D; inline;
operator *(const ALeft: TDensity3DUnit; const ARight: TAreaUnit): TDensity1DUnit; inline;
operator *(const ALeft: TDensity3D;     const ARight: TArea    ): TDensity1D; inline;
operator *(const ALeft: TAreaUnit; const ARight: TFrequencySquareUnit): TSpeedSquareUnit; inline;
operator *(const ALeft: TArea;     const ARight: TFrequencySquare    ): TSpeedSquare; inline;
operator *(const ALeft: TFrequencySquareUnit; const ARight: TAreaUnit): TSpeedSquareUnit; inline;
operator *(const ALeft: TFrequencySquare;     const ARight: TArea    ): TSpeedSquare; inline;
operator *(const ALeft: TAreaUnit; const ARight: TStiffnessUnit): TWorkUnit; inline;
operator *(const ALeft: TArea;     const ARight: TStiffness    ): TWork; inline;
operator *(const ALeft: TStiffnessUnit; const ARight: TAreaUnit): TWorkUnit; inline;
operator *(const ALeft: TStiffness;     const ARight: TArea    ): TWork; inline;
operator *(const ALeft: TAreaUnit; const ARight: TMagneticFluxDensityUnit): TMagneticFluxUnit; inline;
operator *(const ALeft: TArea;     const ARight: TMagneticFluxDensity    ): TMagneticFlux; inline;
operator *(const ALeft: TMagneticFluxDensityUnit; const ARight: TAreaUnit): TMagneticFluxUnit; inline;
operator *(const ALeft: TMagneticFluxDensity;     const ARight: TArea    ): TMagneticFlux; inline;
operator *(const ALeft: TAreaUnit; const ARight: TIlluminanceUnit): TLuminousFluxUnit; inline;
operator *(const ALeft: TArea;     const ARight: TIlluminance    ): TLuminousFlux; inline;
operator *(const ALeft: TIlluminanceUnit; const ARight: TAreaUnit): TLuminousFluxUnit; inline;
operator *(const ALeft: TIlluminance;     const ARight: TArea    ): TLuminousFlux; inline;
operator *(const ALeft: TAreaUnit; const ARight: TWaveNumberUnit): TLengthUnit; inline;
operator *(const ALeft: TArea;     const ARight: TWaveNumber    ): TLength; inline;
operator *(const ALeft: TWaveNumberUnit; const ARight: TAreaUnit): TLengthUnit; inline;
operator *(const ALeft: TWaveNumber;     const ARight: TArea    ): TLength; inline;
operator *(const ALeft: TAreaUnit; const ARight: TInvVolumeUnit): TWaveNumberUnit; inline;
operator *(const ALeft: TArea;     const ARight: TInvVolume    ): TWaveNumber; inline;
operator *(const ALeft: TInvVolumeUnit; const ARight: TAreaUnit): TWaveNumberUnit; inline;
operator *(const ALeft: TInvVolume;     const ARight: TArea    ): TWaveNumber; inline;
operator /(const ALeft: TAreaUnit; const ARight: TLengthUnit): TLengthUnit; inline;
operator /(const ALeft: TArea;     const ARight: TLength    ): TLength; inline;
operator /(const ALeft: TAreaUnit; const ARight: TVolumeUnit): TWaveNumberUnit; inline;
operator /(const ALeft: TArea;     const ARight: TVolume    ): TWaveNumber; inline;
operator /(const ALeft: TAreaUnit; const ARight: TAreaSquareUnit): TInvAreaUnit; inline;
operator /(const ALeft: TArea;     const ARight: TAreaSquare    ): TInvArea; inline;
operator /(const ALeft: TAreaUnit; const ARight: TSpeedSquareUnit): TTimeSquareUnit; inline;
operator /(const ALeft: TArea;     const ARight: TSpeedSquare    ): TTimeSquare; inline;
operator /(const ALeft: TAreaUnit; const ARight: TDensity1DUnit): TSpecificVolumeUnit; inline;
operator /(const ALeft: TArea;     const ARight: TDensity1D    ): TSpecificVolume; inline;
operator /(const ALeft: TAreaUnit; const ARight: TWaveNumberUnit): TVolumeUnit; inline;
operator /(const ALeft: TArea;     const ARight: TWaveNumber    ): TVolume; inline;
operator /(const ALeft: TAreaUnit; const ARight: TSpecificVolumeUnit): TDensity1DUnit; inline;
operator /(const ALeft: TArea;     const ARight: TSpecificVolume    ): TDensity1D; inline;
operator /(const ALeft: TAreaUnit; const ARight: TTimeSquareUnit): TSpeedSquareUnit; inline;
operator /(const ALeft: TArea;     const ARight: TTimeSquare    ): TSpeedSquare; inline;
operator /(const ALeft: TAreaUnit; const ARight: TInvAreaUnit): TAreaSquareUnit; inline;
operator /(const ALeft: TArea;     const ARight: TInvArea    ): TAreaSquare; inline;
operator *(const ALeft: TVolumeUnit; const ARight: TPressureUnit): TWorkUnit; inline;
operator *(const ALeft: TVolume;     const ARight: TPressure    ): TWork; inline;
operator *(const ALeft: TPressureUnit; const ARight: TVolumeUnit): TWorkUnit; inline;
operator *(const ALeft: TPressure;     const ARight: TVolume    ): TWork; inline;
operator *(const ALeft: TVolumeUnit; const ARight: TDensity3DUnit): TMassUnit; inline;
operator *(const ALeft: TVolume;     const ARight: TDensity3D    ): TMass; inline;
operator *(const ALeft: TDensity3DUnit; const ARight: TVolumeUnit): TMassUnit; inline;
operator *(const ALeft: TDensity3D;     const ARight: TVolume    ): TMass; inline;
operator *(const ALeft: TVolumeUnit; const ARight: TWaveNumberUnit): TAreaUnit; inline;
operator *(const ALeft: TVolume;     const ARight: TWaveNumber    ): TArea; inline;
operator *(const ALeft: TWaveNumberUnit; const ARight: TVolumeUnit): TAreaUnit; inline;
operator *(const ALeft: TWaveNumber;     const ARight: TVolume    ): TArea; inline;
operator *(const ALeft: TVolumeUnit; const ARight: TInvAreaUnit): TLengthUnit; inline;
operator *(const ALeft: TVolume;     const ARight: TInvArea    ): TLength; inline;
operator *(const ALeft: TInvAreaUnit; const ARight: TVolumeUnit): TLengthUnit; inline;
operator *(const ALeft: TInvArea;     const ARight: TVolume    ): TLength; inline;
operator /(const ALeft: TVolumeUnit; const ARight: TLengthUnit): TAreaUnit; inline;
operator /(const ALeft: TVolume;     const ARight: TLength    ): TArea; inline;
operator /(const ALeft: TVolumeUnit; const ARight: TAreaUnit): TLengthUnit; inline;
operator /(const ALeft: TVolume;     const ARight: TArea    ): TLength; inline;
operator /(const ALeft: TVolumeUnit; const ARight: TAreaSquareUnit): TWaveNumberUnit; inline;
operator /(const ALeft: TVolume;     const ARight: TAreaSquare    ): TWaveNumber; inline;
operator /(const ALeft: TVolumeUnit; const ARight: TMassUnit): TSpecificVolumeUnit; inline;
operator /(const ALeft: TVolume;     const ARight: TMass    ): TSpecificVolume; inline;
operator /(const ALeft: TVolumeUnit; const ARight: TWaveNumberUnit): TAreaSquareUnit; inline;
operator /(const ALeft: TVolume;     const ARight: TWaveNumber    ): TAreaSquare; inline;
operator /(const ALeft: TVolumeUnit; const ARight: TSpecificVolumeUnit): TMassUnit; inline;
operator /(const ALeft: TVolume;     const ARight: TSpecificVolume    ): TMass; inline;
operator *(const ALeft: TAreaSquareUnit; const ARight: TWaveNumberUnit): TVolumeUnit; inline;
operator *(const ALeft: TAreaSquare;     const ARight: TWaveNumber    ): TVolume; inline;
operator *(const ALeft: TWaveNumberUnit; const ARight: TAreaSquareUnit): TVolumeUnit; inline;
operator *(const ALeft: TWaveNumber;     const ARight: TAreaSquare    ): TVolume; inline;
operator *(const ALeft: TAreaSquareUnit; const ARight: TInvVolumeUnit): TLengthUnit; inline;
operator *(const ALeft: TAreaSquare;     const ARight: TInvVolume    ): TLength; inline;
operator *(const ALeft: TInvVolumeUnit; const ARight: TAreaSquareUnit): TLengthUnit; inline;
operator *(const ALeft: TInvVolume;     const ARight: TAreaSquare    ): TLength; inline;
operator *(const ALeft: TAreaSquareUnit; const ARight: TInvAreaUnit): TAreaUnit; inline;
operator *(const ALeft: TAreaSquare;     const ARight: TInvArea    ): TArea; inline;
operator *(const ALeft: TInvAreaUnit; const ARight: TAreaSquareUnit): TAreaUnit; inline;
operator *(const ALeft: TInvArea;     const ARight: TAreaSquare    ): TArea; inline;
operator /(const ALeft: TAreaSquareUnit; const ARight: TLengthUnit): TVolumeUnit; inline;
operator /(const ALeft: TAreaSquare;     const ARight: TLength    ): TVolume; inline;
operator /(const ALeft: TAreaSquareUnit; const ARight: TAreaUnit): TAreaUnit; inline;
operator /(const ALeft: TAreaSquare;     const ARight: TArea    ): TArea; inline;
operator /(const ALeft: TAreaSquareUnit; const ARight: TVolumeUnit): TLengthUnit; inline;
operator /(const ALeft: TAreaSquare;     const ARight: TVolume    ): TLength; inline;
operator *(const ALeft: TFrequencyUnit; const ARight: TFrequencyUnit): TFrequencySquareUnit; inline;
operator *(const ALeft: TFrequency;     const ARight: TFrequency    ): TFrequencySquare; inline;
operator *(const ALeft: TFrequencyUnit; const ARight: TWorkUnit): TPowerUnit; inline;
operator *(const ALeft: TFrequency;     const ARight: TWork    ): TPower; inline;
operator *(const ALeft: TWorkUnit; const ARight: TFrequencyUnit): TPowerUnit; inline;
operator *(const ALeft: TWork;     const ARight: TFrequency    ): TPower; inline;
operator *(const ALeft: TFrequencyUnit; const ARight: TSpeedUnit): TAccelerationUnit; inline;
operator *(const ALeft: TFrequency;     const ARight: TSpeed    ): TAcceleration; inline;
operator *(const ALeft: TSpeedUnit; const ARight: TFrequencyUnit): TAccelerationUnit; inline;
operator *(const ALeft: TSpeed;     const ARight: TFrequency    ): TAcceleration; inline;
operator *(const ALeft: TFrequencyUnit; const ARight: TCapacitanceUnit): TEletricalConductanceUnit; inline;
operator *(const ALeft: TFrequency;     const ARight: TCapacitance    ): TEletricalConductance; inline;
operator *(const ALeft: TCapacitanceUnit; const ARight: TFrequencyUnit): TEletricalConductanceUnit; inline;
operator *(const ALeft: TCapacitance;     const ARight: TFrequency    ): TEletricalConductance; inline;
operator *(const ALeft: TFrequencyUnit; const ARight: TMagneticFluxUnit): TEletricPotentialUnit; inline;
operator *(const ALeft: TFrequency;     const ARight: TMagneticFlux    ): TEletricPotential; inline;
operator *(const ALeft: TMagneticFluxUnit; const ARight: TFrequencyUnit): TEletricPotentialUnit; inline;
operator *(const ALeft: TMagneticFlux;     const ARight: TFrequency    ): TEletricPotential; inline;
operator *(const ALeft: TFrequencyUnit; const ARight: TInductanceUnit): TResistanceUnit; inline;
operator *(const ALeft: TFrequency;     const ARight: TInductance    ): TResistance; inline;
operator *(const ALeft: TInductanceUnit; const ARight: TFrequencyUnit): TResistanceUnit; inline;
operator *(const ALeft: TInductance;     const ARight: TFrequency    ): TResistance; inline;
operator *(const ALeft: TFrequencyUnit; const ARight: TTimeSquareUnit): TTimeUnit; inline;
operator *(const ALeft: TFrequency;     const ARight: TTimeSquare    ): TTime; inline;
operator *(const ALeft: TTimeSquareUnit; const ARight: TFrequencyUnit): TTimeUnit; inline;
operator *(const ALeft: TTimeSquare;     const ARight: TFrequency    ): TTime; inline;
operator *(const ALeft: TFrequencyUnit; const ARight: TTimeCubeUnit): TTimeSquareUnit; inline;
operator *(const ALeft: TFrequency;     const ARight: TTimeCube    ): TTimeSquare; inline;
operator *(const ALeft: TTimeCubeUnit; const ARight: TFrequencyUnit): TTimeSquareUnit; inline;
operator *(const ALeft: TTimeCube;     const ARight: TFrequency    ): TTimeSquare; inline;
operator /(const ALeft: TFrequencyUnit; const ARight: TSpeedUnit): TWaveNumberUnit; inline;
operator /(const ALeft: TFrequency;     const ARight: TSpeed    ): TWaveNumber; inline;
operator /(const ALeft: TFrequencyUnit; const ARight: TFrequencySquareUnit): TTimeUnit; inline;
operator /(const ALeft: TFrequency;     const ARight: TFrequencySquare    ): TTime; inline;
operator /(const ALeft: TFrequencyUnit; const ARight: TWaveNumberUnit): TSpeedUnit; inline;
operator /(const ALeft: TFrequency;     const ARight: TWaveNumber    ): TSpeed; inline;
operator /(const ALeft: TFrequencyUnit; const ARight: TTimeUnit): TFrequencySquareUnit; inline;
operator /(const ALeft: TFrequency;     const ARight: TTime    ): TFrequencySquare; inline;
operator *(const ALeft: TForceUnit; const ARight: TSpeedUnit): TPowerUnit; inline;
operator *(const ALeft: TForce;     const ARight: TSpeed    ): TPower; inline;
operator *(const ALeft: TSpeedUnit; const ARight: TForceUnit): TPowerUnit; inline;
operator *(const ALeft: TSpeed;     const ARight: TForce    ): TPower; inline;
operator *(const ALeft: TForceUnit; const ARight: TWaveNumberUnit): TStiffnessUnit; inline;
operator *(const ALeft: TForce;     const ARight: TWaveNumber    ): TStiffness; inline;
operator *(const ALeft: TWaveNumberUnit; const ARight: TForceUnit): TStiffnessUnit; inline;
operator *(const ALeft: TWaveNumber;     const ARight: TForce    ): TStiffness; inline;
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
operator /(const ALeft: TForceUnit; const ARight: TWorkUnit): TWaveNumberUnit; inline;
operator /(const ALeft: TForce;     const ARight: TWork    ): TWaveNumber; inline;
operator /(const ALeft: TForceUnit; const ARight: TSpeedSquareUnit): TDensity1DUnit; inline;
operator /(const ALeft: TForce;     const ARight: TSpeedSquare    ): TDensity1D; inline;
operator /(const ALeft: TForceUnit; const ARight: TAccelerationUnit): TMassUnit; inline;
operator /(const ALeft: TForce;     const ARight: TAcceleration    ): TMass; inline;
operator /(const ALeft: TForceUnit; const ARight: TDensity1DUnit): TSpeedSquareUnit; inline;
operator /(const ALeft: TForce;     const ARight: TDensity1D    ): TSpeedSquare; inline;
operator /(const ALeft: TForceUnit; const ARight: TMassUnit): TAccelerationUnit; inline;
operator /(const ALeft: TForce;     const ARight: TMass    ): TAcceleration; inline;
operator /(const ALeft: TForceUnit; const ARight: TStiffnessUnit): TLengthUnit; inline;
operator /(const ALeft: TForce;     const ARight: TStiffness    ): TLength; inline;
operator /(const ALeft: TForceUnit; const ARight: TWaveNumberUnit): TWorkUnit; inline;
operator /(const ALeft: TForce;     const ARight: TWaveNumber    ): TWork; inline;
operator *(const ALeft: TPressureUnit; const ARight: TSpecificVolumeUnit): TSpeedSquareUnit; inline;
operator *(const ALeft: TPressure;     const ARight: TSpecificVolume    ): TSpeedSquare; inline;
operator *(const ALeft: TSpecificVolumeUnit; const ARight: TPressureUnit): TSpeedSquareUnit; inline;
operator *(const ALeft: TSpecificVolume;     const ARight: TPressure    ): TSpeedSquare; inline;
operator *(const ALeft: TPressureUnit; const ARight: TTimeSquareUnit): TDensity1DUnit; inline;
operator *(const ALeft: TPressure;     const ARight: TTimeSquare    ): TDensity1D; inline;
operator *(const ALeft: TTimeSquareUnit; const ARight: TPressureUnit): TDensity1DUnit; inline;
operator *(const ALeft: TTimeSquare;     const ARight: TPressure    ): TDensity1D; inline;
operator /(const ALeft: TPressureUnit; const ARight: TForceUnit): TInvAreaUnit; inline;
operator /(const ALeft: TPressure;     const ARight: TForce    ): TInvArea; inline;
operator /(const ALeft: TPressureUnit; const ARight: TWorkUnit): TInvVolumeUnit; inline;
operator /(const ALeft: TPressure;     const ARight: TWork    ): TInvVolume; inline;
operator /(const ALeft: TPressureUnit; const ARight: TSpeedSquareUnit): TDensity3DUnit; inline;
operator /(const ALeft: TPressure;     const ARight: TSpeedSquare    ): TDensity3D; inline;
operator /(const ALeft: TPressureUnit; const ARight: TAccelerationUnit): TDensity2DUnit; inline;
operator /(const ALeft: TPressure;     const ARight: TAcceleration    ): TDensity2D; inline;
operator /(const ALeft: TPressureUnit; const ARight: TDensity1DUnit): TFrequencySquareUnit; inline;
operator /(const ALeft: TPressure;     const ARight: TDensity1D    ): TFrequencySquare; inline;
operator /(const ALeft: TPressureUnit; const ARight: TDensity2DUnit): TAccelerationUnit; inline;
operator /(const ALeft: TPressure;     const ARight: TDensity2D    ): TAcceleration; inline;
operator /(const ALeft: TPressureUnit; const ARight: TDensity3DUnit): TSpeedSquareUnit; inline;
operator /(const ALeft: TPressure;     const ARight: TDensity3D    ): TSpeedSquare; inline;
operator /(const ALeft: TPressureUnit; const ARight: TFrequencySquareUnit): TDensity1DUnit; inline;
operator /(const ALeft: TPressure;     const ARight: TFrequencySquare    ): TDensity1D; inline;
operator /(const ALeft: TPressureUnit; const ARight: TStiffnessUnit): TWaveNumberUnit; inline;
operator /(const ALeft: TPressure;     const ARight: TStiffness    ): TWaveNumber; inline;
operator /(const ALeft: TPressureUnit; const ARight: TWaveNumberUnit): TStiffnessUnit; inline;
operator /(const ALeft: TPressure;     const ARight: TWaveNumber    ): TStiffness; inline;
operator /(const ALeft: TPressureUnit; const ARight: TInvVolumeUnit): TWorkUnit; inline;
operator /(const ALeft: TPressure;     const ARight: TInvVolume    ): TWork; inline;
operator /(const ALeft: TPressureUnit; const ARight: TInvAreaUnit): TForceUnit; inline;
operator /(const ALeft: TPressure;     const ARight: TInvArea    ): TForce; inline;
operator *(const ALeft: TWorkUnit; const ARight: TWaveNumberUnit): TForceUnit; inline;
operator *(const ALeft: TWork;     const ARight: TWaveNumber    ): TForce; inline;
operator *(const ALeft: TWaveNumberUnit; const ARight: TWorkUnit): TForceUnit; inline;
operator *(const ALeft: TWaveNumber;     const ARight: TWork    ): TForce; inline;
operator *(const ALeft: TWorkUnit; const ARight: TInvVolumeUnit): TPressureUnit; inline;
operator *(const ALeft: TWork;     const ARight: TInvVolume    ): TPressure; inline;
operator *(const ALeft: TInvVolumeUnit; const ARight: TWorkUnit): TPressureUnit; inline;
operator *(const ALeft: TInvVolume;     const ARight: TWork    ): TPressure; inline;
operator *(const ALeft: TWorkUnit; const ARight: TInvAreaUnit): TStiffnessUnit; inline;
operator *(const ALeft: TWork;     const ARight: TInvArea    ): TStiffness; inline;
operator *(const ALeft: TInvAreaUnit; const ARight: TWorkUnit): TStiffnessUnit; inline;
operator *(const ALeft: TInvArea;     const ARight: TWork    ): TStiffness; inline;
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
operator /(const ALeft: TWorkUnit; const ARight: TPowerUnit): TTimeUnit; inline;
operator /(const ALeft: TWork;     const ARight: TPower    ): TTime; inline;
operator /(const ALeft: TWorkUnit; const ARight: TSpeedSquareUnit): TMassUnit; inline;
operator /(const ALeft: TWork;     const ARight: TSpeedSquare    ): TMass; inline;
operator /(const ALeft: TWorkUnit; const ARight: TMassUnit): TSpeedSquareUnit; inline;
operator /(const ALeft: TWork;     const ARight: TMass    ): TSpeedSquare; inline;
operator /(const ALeft: TWorkUnit; const ARight: TStiffnessUnit): TAreaUnit; inline;
operator /(const ALeft: TWork;     const ARight: TStiffness    ): TArea; inline;
operator /(const ALeft: TWorkUnit; const ARight: TElettricChargeUnit): TEletricPotentialUnit; inline;
operator /(const ALeft: TWork;     const ARight: TElettricCharge    ): TEletricPotential; inline;
operator /(const ALeft: TWorkUnit; const ARight: TEletricPotentialUnit): TElettricChargeUnit; inline;
operator /(const ALeft: TWork;     const ARight: TEletricPotential    ): TElettricCharge; inline;
operator /(const ALeft: TWorkUnit; const ARight: TTimeUnit): TPowerUnit; inline;
operator /(const ALeft: TWork;     const ARight: TTime    ): TPower; inline;
operator *(const ALeft: TPowerUnit; const ARight: TTimeUnit): TWorkUnit; inline;
operator *(const ALeft: TPower;     const ARight: TTime    ): TWork; inline;
operator *(const ALeft: TTimeUnit; const ARight: TPowerUnit): TWorkUnit; inline;
operator *(const ALeft: TTime;     const ARight: TPower    ): TWork; inline;
operator /(const ALeft: TPowerUnit; const ARight: TFrequencyUnit): TWorkUnit; inline;
operator /(const ALeft: TPower;     const ARight: TFrequency    ): TWork; inline;
operator /(const ALeft: TPowerUnit; const ARight: TForceUnit): TSpeedUnit; inline;
operator /(const ALeft: TPower;     const ARight: TForce    ): TSpeed; inline;
operator /(const ALeft: TPowerUnit; const ARight: TWorkUnit): TFrequencyUnit; inline;
operator /(const ALeft: TPower;     const ARight: TWork    ): TFrequency; inline;
operator /(const ALeft: TPowerUnit; const ARight: TSpeedUnit): TForceUnit; inline;
operator /(const ALeft: TPower;     const ARight: TSpeed    ): TForce; inline;
operator *(const ALeft: TSpeedUnit; const ARight: TSpeedUnit): TSpeedSquareUnit; inline;
operator *(const ALeft: TSpeed;     const ARight: TSpeed    ): TSpeedSquare; inline;
operator *(const ALeft: TSpeedUnit; const ARight: TWaveNumberUnit): TFrequencyUnit; inline;
operator *(const ALeft: TSpeed;     const ARight: TWaveNumber    ): TFrequency; inline;
operator *(const ALeft: TWaveNumberUnit; const ARight: TSpeedUnit): TFrequencyUnit; inline;
operator *(const ALeft: TWaveNumber;     const ARight: TSpeed    ): TFrequency; inline;
operator *(const ALeft: TSpeedUnit; const ARight: TTimeUnit): TLengthUnit; inline;
operator *(const ALeft: TSpeed;     const ARight: TTime    ): TLength; inline;
operator *(const ALeft: TTimeUnit; const ARight: TSpeedUnit): TLengthUnit; inline;
operator *(const ALeft: TTime;     const ARight: TSpeed    ): TLength; inline;
operator /(const ALeft: TSpeedUnit; const ARight: TLengthUnit): TFrequencyUnit; inline;
operator /(const ALeft: TSpeed;     const ARight: TLength    ): TFrequency; inline;
operator /(const ALeft: TSpeedUnit; const ARight: TFrequencyUnit): TLengthUnit; inline;
operator /(const ALeft: TSpeed;     const ARight: TFrequency    ): TLength; inline;
operator /(const ALeft: TSpeedUnit; const ARight: TAccelerationUnit): TTimeUnit; inline;
operator /(const ALeft: TSpeed;     const ARight: TAcceleration    ): TTime; inline;
operator /(const ALeft: TSpeedUnit; const ARight: TTimeUnit): TAccelerationUnit; inline;
operator /(const ALeft: TSpeed;     const ARight: TTime    ): TAcceleration; inline;
operator *(const ALeft: TSpeedSquareUnit; const ARight: TDensity1DUnit): TForceUnit; inline;
operator *(const ALeft: TSpeedSquare;     const ARight: TDensity1D    ): TForce; inline;
operator *(const ALeft: TDensity1DUnit; const ARight: TSpeedSquareUnit): TForceUnit; inline;
operator *(const ALeft: TDensity1D;     const ARight: TSpeedSquare    ): TForce; inline;
operator *(const ALeft: TSpeedSquareUnit; const ARight: TDensity2DUnit): TStiffnessUnit; inline;
operator *(const ALeft: TSpeedSquare;     const ARight: TDensity2D    ): TStiffness; inline;
operator *(const ALeft: TDensity2DUnit; const ARight: TSpeedSquareUnit): TStiffnessUnit; inline;
operator *(const ALeft: TDensity2D;     const ARight: TSpeedSquare    ): TStiffness; inline;
operator *(const ALeft: TSpeedSquareUnit; const ARight: TDensity3DUnit): TPressureUnit; inline;
operator *(const ALeft: TSpeedSquare;     const ARight: TDensity3D    ): TPressure; inline;
operator *(const ALeft: TDensity3DUnit; const ARight: TSpeedSquareUnit): TPressureUnit; inline;
operator *(const ALeft: TDensity3D;     const ARight: TSpeedSquare    ): TPressure; inline;
operator *(const ALeft: TSpeedSquareUnit; const ARight: TFrequencySquareUnit): TAccelerationSquareUnit; inline;
operator *(const ALeft: TSpeedSquare;     const ARight: TFrequencySquare    ): TAccelerationSquare; inline;
operator *(const ALeft: TFrequencySquareUnit; const ARight: TSpeedSquareUnit): TAccelerationSquareUnit; inline;
operator *(const ALeft: TFrequencySquare;     const ARight: TSpeedSquare    ): TAccelerationSquare; inline;
operator *(const ALeft: TSpeedSquareUnit; const ARight: TMassUnit): TWorkUnit; inline;
operator *(const ALeft: TSpeedSquare;     const ARight: TMass    ): TWork; inline;
operator *(const ALeft: TMassUnit; const ARight: TSpeedSquareUnit): TWorkUnit; inline;
operator *(const ALeft: TMass;     const ARight: TSpeedSquare    ): TWork; inline;
operator *(const ALeft: TSpeedSquareUnit; const ARight: TWaveNumberUnit): TAccelerationUnit; inline;
operator *(const ALeft: TSpeedSquare;     const ARight: TWaveNumber    ): TAcceleration; inline;
operator *(const ALeft: TWaveNumberUnit; const ARight: TSpeedSquareUnit): TAccelerationUnit; inline;
operator *(const ALeft: TWaveNumber;     const ARight: TSpeedSquare    ): TAcceleration; inline;
operator *(const ALeft: TSpeedSquareUnit; const ARight: TTimeSquareUnit): TAreaUnit; inline;
operator *(const ALeft: TSpeedSquare;     const ARight: TTimeSquare    ): TArea; inline;
operator *(const ALeft: TTimeSquareUnit; const ARight: TSpeedSquareUnit): TAreaUnit; inline;
operator *(const ALeft: TTimeSquare;     const ARight: TSpeedSquare    ): TArea; inline;
operator *(const ALeft: TSpeedSquareUnit; const ARight: TInvAreaUnit): TFrequencySquareUnit; inline;
operator *(const ALeft: TSpeedSquare;     const ARight: TInvArea    ): TFrequencySquare; inline;
operator *(const ALeft: TInvAreaUnit; const ARight: TSpeedSquareUnit): TFrequencySquareUnit; inline;
operator *(const ALeft: TInvArea;     const ARight: TSpeedSquare    ): TFrequencySquare; inline;
operator /(const ALeft: TSpeedSquareUnit; const ARight: TLengthUnit): TAccelerationUnit; inline;
operator /(const ALeft: TSpeedSquare;     const ARight: TLength    ): TAcceleration; inline;
operator /(const ALeft: TSpeedSquareUnit; const ARight: TAreaUnit): TFrequencySquareUnit; inline;
operator /(const ALeft: TSpeedSquare;     const ARight: TArea    ): TFrequencySquare; inline;
operator /(const ALeft: TSpeedSquareUnit; const ARight: TPressureUnit): TSpecificVolumeUnit; inline;
operator /(const ALeft: TSpeedSquare;     const ARight: TPressure    ): TSpecificVolume; inline;
operator /(const ALeft: TSpeedSquareUnit; const ARight: TSpeedUnit): TSpeedUnit; inline;
operator /(const ALeft: TSpeedSquare;     const ARight: TSpeed    ): TSpeed; inline;
operator /(const ALeft: TSpeedSquareUnit; const ARight: TAccelerationUnit): TLengthUnit; inline;
operator /(const ALeft: TSpeedSquare;     const ARight: TAcceleration    ): TLength; inline;
operator /(const ALeft: TSpeedSquareUnit; const ARight: TAccelerationSquareUnit): TTimeSquareUnit; inline;
operator /(const ALeft: TSpeedSquare;     const ARight: TAccelerationSquare    ): TTimeSquare; inline;
operator /(const ALeft: TSpeedSquareUnit; const ARight: TFrequencySquareUnit): TAreaUnit; inline;
operator /(const ALeft: TSpeedSquare;     const ARight: TFrequencySquare    ): TArea; inline;
operator /(const ALeft: TSpeedSquareUnit; const ARight: TSpecificVolumeUnit): TPressureUnit; inline;
operator /(const ALeft: TSpeedSquare;     const ARight: TSpecificVolume    ): TPressure; inline;
operator /(const ALeft: TSpeedSquareUnit; const ARight: TTimeSquareUnit): TAccelerationSquareUnit; inline;
operator /(const ALeft: TSpeedSquare;     const ARight: TTimeSquare    ): TAccelerationSquare; inline;
operator *(const ALeft: TAccelerationUnit; const ARight: TAccelerationUnit): TAccelerationSquareUnit; inline;
operator *(const ALeft: TAcceleration;     const ARight: TAcceleration    ): TAccelerationSquare; inline;
operator *(const ALeft: TAccelerationUnit; const ARight: TDensity1DUnit): TStiffnessUnit; inline;
operator *(const ALeft: TAcceleration;     const ARight: TDensity1D    ): TStiffness; inline;
operator *(const ALeft: TDensity1DUnit; const ARight: TAccelerationUnit): TStiffnessUnit; inline;
operator *(const ALeft: TDensity1D;     const ARight: TAcceleration    ): TStiffness; inline;
operator *(const ALeft: TAccelerationUnit; const ARight: TDensity2DUnit): TPressureUnit; inline;
operator *(const ALeft: TAcceleration;     const ARight: TDensity2D    ): TPressure; inline;
operator *(const ALeft: TDensity2DUnit; const ARight: TAccelerationUnit): TPressureUnit; inline;
operator *(const ALeft: TDensity2D;     const ARight: TAcceleration    ): TPressure; inline;
operator *(const ALeft: TAccelerationUnit; const ARight: TMassUnit): TForceUnit; inline;
operator *(const ALeft: TAcceleration;     const ARight: TMass    ): TForce; inline;
operator *(const ALeft: TMassUnit; const ARight: TAccelerationUnit): TForceUnit; inline;
operator *(const ALeft: TMass;     const ARight: TAcceleration    ): TForce; inline;
operator *(const ALeft: TAccelerationUnit; const ARight: TWaveNumberUnit): TFrequencySquareUnit; inline;
operator *(const ALeft: TAcceleration;     const ARight: TWaveNumber    ): TFrequencySquare; inline;
operator *(const ALeft: TWaveNumberUnit; const ARight: TAccelerationUnit): TFrequencySquareUnit; inline;
operator *(const ALeft: TWaveNumber;     const ARight: TAcceleration    ): TFrequencySquare; inline;
operator *(const ALeft: TAccelerationUnit; const ARight: TTimeUnit): TSpeedUnit; inline;
operator *(const ALeft: TAcceleration;     const ARight: TTime    ): TSpeed; inline;
operator *(const ALeft: TTimeUnit; const ARight: TAccelerationUnit): TSpeedUnit; inline;
operator *(const ALeft: TTime;     const ARight: TAcceleration    ): TSpeed; inline;
operator *(const ALeft: TAccelerationUnit; const ARight: TTimeSquareUnit): TLengthUnit; inline;
operator *(const ALeft: TAcceleration;     const ARight: TTimeSquare    ): TLength; inline;
operator *(const ALeft: TTimeSquareUnit; const ARight: TAccelerationUnit): TLengthUnit; inline;
operator *(const ALeft: TTimeSquare;     const ARight: TAcceleration    ): TLength; inline;
operator /(const ALeft: TAccelerationUnit; const ARight: TLengthUnit): TFrequencySquareUnit; inline;
operator /(const ALeft: TAcceleration;     const ARight: TLength    ): TFrequencySquare; inline;
operator /(const ALeft: TAccelerationUnit; const ARight: TFrequencyUnit): TSpeedUnit; inline;
operator /(const ALeft: TAcceleration;     const ARight: TFrequency    ): TSpeed; inline;
operator /(const ALeft: TAccelerationUnit; const ARight: TSpeedUnit): TFrequencyUnit; inline;
operator /(const ALeft: TAcceleration;     const ARight: TSpeed    ): TFrequency; inline;
operator /(const ALeft: TAccelerationUnit; const ARight: TSpeedSquareUnit): TWaveNumberUnit; inline;
operator /(const ALeft: TAcceleration;     const ARight: TSpeedSquare    ): TWaveNumber; inline;
operator /(const ALeft: TAccelerationUnit; const ARight: TFrequencySquareUnit): TLengthUnit; inline;
operator /(const ALeft: TAcceleration;     const ARight: TFrequencySquare    ): TLength; inline;
operator /(const ALeft: TAccelerationUnit; const ARight: TWaveNumberUnit): TSpeedSquareUnit; inline;
operator /(const ALeft: TAcceleration;     const ARight: TWaveNumber    ): TSpeedSquare; inline;
operator *(const ALeft: TAccelerationSquareUnit; const ARight: TTimeSquareUnit): TSpeedSquareUnit; inline;
operator *(const ALeft: TAccelerationSquare;     const ARight: TTimeSquare    ): TSpeedSquare; inline;
operator *(const ALeft: TTimeSquareUnit; const ARight: TAccelerationSquareUnit): TSpeedSquareUnit; inline;
operator *(const ALeft: TTimeSquare;     const ARight: TAccelerationSquare    ): TSpeedSquare; inline;
operator /(const ALeft: TAccelerationSquareUnit; const ARight: TSpeedSquareUnit): TFrequencySquareUnit; inline;
operator /(const ALeft: TAccelerationSquare;     const ARight: TSpeedSquare    ): TFrequencySquare; inline;
operator /(const ALeft: TAccelerationSquareUnit; const ARight: TAccelerationUnit): TAccelerationUnit; inline;
operator /(const ALeft: TAccelerationSquare;     const ARight: TAcceleration    ): TAcceleration; inline;
operator /(const ALeft: TAccelerationSquareUnit; const ARight: TFrequencySquareUnit): TSpeedSquareUnit; inline;
operator /(const ALeft: TAccelerationSquare;     const ARight: TFrequencySquare    ): TSpeedSquare; inline;
operator *(const ALeft: TDensity1DUnit; const ARight: TFrequencySquareUnit): TPressureUnit; inline;
operator *(const ALeft: TDensity1D;     const ARight: TFrequencySquare    ): TPressure; inline;
operator *(const ALeft: TFrequencySquareUnit; const ARight: TDensity1DUnit): TPressureUnit; inline;
operator *(const ALeft: TFrequencySquare;     const ARight: TDensity1D    ): TPressure; inline;
operator *(const ALeft: TDensity1DUnit; const ARight: TWaveNumberUnit): TDensity2DUnit; inline;
operator *(const ALeft: TDensity1D;     const ARight: TWaveNumber    ): TDensity2D; inline;
operator *(const ALeft: TWaveNumberUnit; const ARight: TDensity1DUnit): TDensity2DUnit; inline;
operator *(const ALeft: TWaveNumber;     const ARight: TDensity1D    ): TDensity2D; inline;
operator *(const ALeft: TDensity1DUnit; const ARight: TSpecificVolumeUnit): TAreaUnit; inline;
operator *(const ALeft: TDensity1D;     const ARight: TSpecificVolume    ): TArea; inline;
operator *(const ALeft: TSpecificVolumeUnit; const ARight: TDensity1DUnit): TAreaUnit; inline;
operator *(const ALeft: TSpecificVolume;     const ARight: TDensity1D    ): TArea; inline;
operator *(const ALeft: TDensity1DUnit; const ARight: TInvAreaUnit): TDensity3DUnit; inline;
operator *(const ALeft: TDensity1D;     const ARight: TInvArea    ): TDensity3D; inline;
operator *(const ALeft: TInvAreaUnit; const ARight: TDensity1DUnit): TDensity3DUnit; inline;
operator *(const ALeft: TInvArea;     const ARight: TDensity1D    ): TDensity3D; inline;
operator /(const ALeft: TDensity1DUnit; const ARight: TLengthUnit): TDensity2DUnit; inline;
operator /(const ALeft: TDensity1D;     const ARight: TLength    ): TDensity2D; inline;
operator /(const ALeft: TDensity1DUnit; const ARight: TAreaUnit): TDensity3DUnit; inline;
operator /(const ALeft: TDensity1D;     const ARight: TArea    ): TDensity3D; inline;
operator /(const ALeft: TDensity1DUnit; const ARight: TPressureUnit): TTimeSquareUnit; inline;
operator /(const ALeft: TDensity1D;     const ARight: TPressure    ): TTimeSquare; inline;
operator /(const ALeft: TDensity1DUnit; const ARight: TDensity2DUnit): TLengthUnit; inline;
operator /(const ALeft: TDensity1D;     const ARight: TDensity2D    ): TLength; inline;
operator /(const ALeft: TDensity1DUnit; const ARight: TDensity3DUnit): TAreaUnit; inline;
operator /(const ALeft: TDensity1D;     const ARight: TDensity3D    ): TArea; inline;
operator /(const ALeft: TDensity1DUnit; const ARight: TMassUnit): TWaveNumberUnit; inline;
operator /(const ALeft: TDensity1D;     const ARight: TMass    ): TWaveNumber; inline;
operator /(const ALeft: TDensity1DUnit; const ARight: TWaveNumberUnit): TMassUnit; inline;
operator /(const ALeft: TDensity1D;     const ARight: TWaveNumber    ): TMass; inline;
operator /(const ALeft: TDensity1DUnit; const ARight: TTimeSquareUnit): TPressureUnit; inline;
operator /(const ALeft: TDensity1D;     const ARight: TTimeSquare    ): TPressure; inline;
operator *(const ALeft: TDensity2DUnit; const ARight: TWaveNumberUnit): TDensity3DUnit; inline;
operator *(const ALeft: TDensity2D;     const ARight: TWaveNumber    ): TDensity3D; inline;
operator *(const ALeft: TWaveNumberUnit; const ARight: TDensity2DUnit): TDensity3DUnit; inline;
operator *(const ALeft: TWaveNumber;     const ARight: TDensity2D    ): TDensity3D; inline;
operator *(const ALeft: TDensity2DUnit; const ARight: TSpecificVolumeUnit): TLengthUnit; inline;
operator *(const ALeft: TDensity2D;     const ARight: TSpecificVolume    ): TLength; inline;
operator *(const ALeft: TSpecificVolumeUnit; const ARight: TDensity2DUnit): TLengthUnit; inline;
operator *(const ALeft: TSpecificVolume;     const ARight: TDensity2D    ): TLength; inline;
operator /(const ALeft: TDensity2DUnit; const ARight: TLengthUnit): TDensity3DUnit; inline;
operator /(const ALeft: TDensity2D;     const ARight: TLength    ): TDensity3D; inline;
operator /(const ALeft: TDensity2DUnit; const ARight: TDensity1DUnit): TWaveNumberUnit; inline;
operator /(const ALeft: TDensity2D;     const ARight: TDensity1D    ): TWaveNumber; inline;
operator /(const ALeft: TDensity2DUnit; const ARight: TDensity3DUnit): TLengthUnit; inline;
operator /(const ALeft: TDensity2D;     const ARight: TDensity3D    ): TLength; inline;
operator /(const ALeft: TDensity2DUnit; const ARight: TMassUnit): TInvAreaUnit; inline;
operator /(const ALeft: TDensity2D;     const ARight: TMass    ): TInvArea; inline;
operator /(const ALeft: TDensity2DUnit; const ARight: TWaveNumberUnit): TDensity1DUnit; inline;
operator /(const ALeft: TDensity2D;     const ARight: TWaveNumber    ): TDensity1D; inline;
operator /(const ALeft: TDensity2DUnit; const ARight: TInvAreaUnit): TMassUnit; inline;
operator /(const ALeft: TDensity2D;     const ARight: TInvArea    ): TMass; inline;
operator /(const ALeft: TDensity3DUnit; const ARight: TDensity1DUnit): TInvAreaUnit; inline;
operator /(const ALeft: TDensity3D;     const ARight: TDensity1D    ): TInvArea; inline;
operator /(const ALeft: TDensity3DUnit; const ARight: TDensity2DUnit): TWaveNumberUnit; inline;
operator /(const ALeft: TDensity3D;     const ARight: TDensity2D    ): TWaveNumber; inline;
operator /(const ALeft: TDensity3DUnit; const ARight: TMassUnit): TInvVolumeUnit; inline;
operator /(const ALeft: TDensity3D;     const ARight: TMass    ): TInvVolume; inline;
operator /(const ALeft: TDensity3DUnit; const ARight: TWaveNumberUnit): TDensity2DUnit; inline;
operator /(const ALeft: TDensity3D;     const ARight: TWaveNumber    ): TDensity2D; inline;
operator /(const ALeft: TDensity3DUnit; const ARight: TInvVolumeUnit): TMassUnit; inline;
operator /(const ALeft: TDensity3D;     const ARight: TInvVolume    ): TMass; inline;
operator /(const ALeft: TDensity3DUnit; const ARight: TInvAreaUnit): TDensity1DUnit; inline;
operator /(const ALeft: TDensity3D;     const ARight: TInvArea    ): TDensity1D; inline;
operator *(const ALeft: TFrequencySquareUnit; const ARight: TMassUnit): TStiffnessUnit; inline;
operator *(const ALeft: TFrequencySquare;     const ARight: TMass    ): TStiffness; inline;
operator *(const ALeft: TMassUnit; const ARight: TFrequencySquareUnit): TStiffnessUnit; inline;
operator *(const ALeft: TMass;     const ARight: TFrequencySquare    ): TStiffness; inline;
operator *(const ALeft: TFrequencySquareUnit; const ARight: TTimeUnit): TFrequencyUnit; inline;
operator *(const ALeft: TFrequencySquare;     const ARight: TTime    ): TFrequency; inline;
operator *(const ALeft: TTimeUnit; const ARight: TFrequencySquareUnit): TFrequencyUnit; inline;
operator *(const ALeft: TTime;     const ARight: TFrequencySquare    ): TFrequency; inline;
operator *(const ALeft: TFrequencySquareUnit; const ARight: TTimeCubeUnit): TTimeUnit; inline;
operator *(const ALeft: TFrequencySquare;     const ARight: TTimeCube    ): TTime; inline;
operator *(const ALeft: TTimeCubeUnit; const ARight: TFrequencySquareUnit): TTimeUnit; inline;
operator *(const ALeft: TTimeCube;     const ARight: TFrequencySquare    ): TTime; inline;
operator /(const ALeft: TFrequencySquareUnit; const ARight: TFrequencyUnit): TFrequencyUnit; inline;
operator /(const ALeft: TFrequencySquare;     const ARight: TFrequency    ): TFrequency; inline;
operator /(const ALeft: TFrequencySquareUnit; const ARight: TSpeedSquareUnit): TInvAreaUnit; inline;
operator /(const ALeft: TFrequencySquare;     const ARight: TSpeedSquare    ): TInvArea; inline;
operator /(const ALeft: TFrequencySquareUnit; const ARight: TAccelerationUnit): TWaveNumberUnit; inline;
operator /(const ALeft: TFrequencySquare;     const ARight: TAcceleration    ): TWaveNumber; inline;
operator /(const ALeft: TFrequencySquareUnit; const ARight: TWaveNumberUnit): TAccelerationUnit; inline;
operator /(const ALeft: TFrequencySquare;     const ARight: TWaveNumber    ): TAcceleration; inline;
operator /(const ALeft: TFrequencySquareUnit; const ARight: TInvAreaUnit): TSpeedSquareUnit; inline;
operator /(const ALeft: TFrequencySquare;     const ARight: TInvArea    ): TSpeedSquare; inline;
operator *(const ALeft: TMassUnit; const ARight: TWaveNumberUnit): TDensity1DUnit; inline;
operator *(const ALeft: TMass;     const ARight: TWaveNumber    ): TDensity1D; inline;
operator *(const ALeft: TWaveNumberUnit; const ARight: TMassUnit): TDensity1DUnit; inline;
operator *(const ALeft: TWaveNumber;     const ARight: TMass    ): TDensity1D; inline;
operator *(const ALeft: TMassUnit; const ARight: TSpecificVolumeUnit): TVolumeUnit; inline;
operator *(const ALeft: TMass;     const ARight: TSpecificVolume    ): TVolume; inline;
operator *(const ALeft: TSpecificVolumeUnit; const ARight: TMassUnit): TVolumeUnit; inline;
operator *(const ALeft: TSpecificVolume;     const ARight: TMass    ): TVolume; inline;
operator *(const ALeft: TMassUnit; const ARight: TInvVolumeUnit): TDensity3DUnit; inline;
operator *(const ALeft: TMass;     const ARight: TInvVolume    ): TDensity3D; inline;
operator *(const ALeft: TInvVolumeUnit; const ARight: TMassUnit): TDensity3DUnit; inline;
operator *(const ALeft: TInvVolume;     const ARight: TMass    ): TDensity3D; inline;
operator *(const ALeft: TMassUnit; const ARight: TInvAreaUnit): TDensity2DUnit; inline;
operator *(const ALeft: TMass;     const ARight: TInvArea    ): TDensity2D; inline;
operator *(const ALeft: TInvAreaUnit; const ARight: TMassUnit): TDensity2DUnit; inline;
operator *(const ALeft: TInvArea;     const ARight: TMass    ): TDensity2D; inline;
operator /(const ALeft: TMassUnit; const ARight: TLengthUnit): TDensity1DUnit; inline;
operator /(const ALeft: TMass;     const ARight: TLength    ): TDensity1D; inline;
operator /(const ALeft: TMassUnit; const ARight: TAreaUnit): TDensity2DUnit; inline;
operator /(const ALeft: TMass;     const ARight: TArea    ): TDensity2D; inline;
operator /(const ALeft: TMassUnit; const ARight: TVolumeUnit): TDensity3DUnit; inline;
operator /(const ALeft: TMass;     const ARight: TVolume    ): TDensity3D; inline;
operator /(const ALeft: TMassUnit; const ARight: TDensity1DUnit): TLengthUnit; inline;
operator /(const ALeft: TMass;     const ARight: TDensity1D    ): TLength; inline;
operator /(const ALeft: TMassUnit; const ARight: TDensity2DUnit): TAreaUnit; inline;
operator /(const ALeft: TMass;     const ARight: TDensity2D    ): TArea; inline;
operator /(const ALeft: TMassUnit; const ARight: TDensity3DUnit): TVolumeUnit; inline;
operator /(const ALeft: TMass;     const ARight: TDensity3D    ): TVolume; inline;
operator /(const ALeft: TMassUnit; const ARight: TStiffnessUnit): TTimeSquareUnit; inline;
operator /(const ALeft: TMass;     const ARight: TStiffness    ): TTimeSquare; inline;
operator /(const ALeft: TMassUnit; const ARight: TTimeSquareUnit): TStiffnessUnit; inline;
operator /(const ALeft: TMass;     const ARight: TTimeSquare    ): TStiffness; inline;
operator *(const ALeft: TStiffnessUnit; const ARight: TWaveNumberUnit): TPressureUnit; inline;
operator *(const ALeft: TStiffness;     const ARight: TWaveNumber    ): TPressure; inline;
operator *(const ALeft: TWaveNumberUnit; const ARight: TStiffnessUnit): TPressureUnit; inline;
operator *(const ALeft: TWaveNumber;     const ARight: TStiffness    ): TPressure; inline;
operator *(const ALeft: TStiffnessUnit; const ARight: TTimeSquareUnit): TMassUnit; inline;
operator *(const ALeft: TStiffness;     const ARight: TTimeSquare    ): TMass; inline;
operator *(const ALeft: TTimeSquareUnit; const ARight: TStiffnessUnit): TMassUnit; inline;
operator *(const ALeft: TTimeSquare;     const ARight: TStiffness    ): TMass; inline;
operator /(const ALeft: TStiffnessUnit; const ARight: TLengthUnit): TPressureUnit; inline;
operator /(const ALeft: TStiffness;     const ARight: TLength    ): TPressure; inline;
operator /(const ALeft: TStiffnessUnit; const ARight: TForceUnit): TWaveNumberUnit; inline;
operator /(const ALeft: TStiffness;     const ARight: TForce    ): TWaveNumber; inline;
operator /(const ALeft: TStiffnessUnit; const ARight: TPressureUnit): TLengthUnit; inline;
operator /(const ALeft: TStiffness;     const ARight: TPressure    ): TLength; inline;
operator /(const ALeft: TStiffnessUnit; const ARight: TWorkUnit): TInvAreaUnit; inline;
operator /(const ALeft: TStiffness;     const ARight: TWork    ): TInvArea; inline;
operator /(const ALeft: TStiffnessUnit; const ARight: TSpeedSquareUnit): TDensity2DUnit; inline;
operator /(const ALeft: TStiffness;     const ARight: TSpeedSquare    ): TDensity2D; inline;
operator /(const ALeft: TStiffnessUnit; const ARight: TAccelerationUnit): TDensity1DUnit; inline;
operator /(const ALeft: TStiffness;     const ARight: TAcceleration    ): TDensity1D; inline;
operator /(const ALeft: TStiffnessUnit; const ARight: TDensity1DUnit): TAccelerationUnit; inline;
operator /(const ALeft: TStiffness;     const ARight: TDensity1D    ): TAcceleration; inline;
operator /(const ALeft: TStiffnessUnit; const ARight: TDensity2DUnit): TSpeedSquareUnit; inline;
operator /(const ALeft: TStiffness;     const ARight: TDensity2D    ): TSpeedSquare; inline;
operator /(const ALeft: TStiffnessUnit; const ARight: TFrequencySquareUnit): TMassUnit; inline;
operator /(const ALeft: TStiffness;     const ARight: TFrequencySquare    ): TMass; inline;
operator /(const ALeft: TStiffnessUnit; const ARight: TMassUnit): TFrequencySquareUnit; inline;
operator /(const ALeft: TStiffness;     const ARight: TMass    ): TFrequencySquare; inline;
operator /(const ALeft: TStiffnessUnit; const ARight: TMagneticFluxUnit): TCurrentDensityUnit; inline;
operator /(const ALeft: TStiffness;     const ARight: TMagneticFlux    ): TCurrentDensity; inline;
operator /(const ALeft: TStiffnessUnit; const ARight: TWaveNumberUnit): TForceUnit; inline;
operator /(const ALeft: TStiffness;     const ARight: TWaveNumber    ): TForce; inline;
operator /(const ALeft: TStiffnessUnit; const ARight: TCurrentDensityUnit): TMagneticFluxUnit; inline;
operator /(const ALeft: TStiffness;     const ARight: TCurrentDensity    ): TMagneticFlux; inline;
operator /(const ALeft: TStiffnessUnit; const ARight: TInvAreaUnit): TWorkUnit; inline;
operator /(const ALeft: TStiffness;     const ARight: TInvArea    ): TWork; inline;
operator *(const ALeft: TElettricChargeUnit; const ARight: TEletricPotentialUnit): TWorkUnit; inline;
operator *(const ALeft: TElettricCharge;     const ARight: TEletricPotential    ): TWork; inline;
operator *(const ALeft: TEletricPotentialUnit; const ARight: TElettricChargeUnit): TWorkUnit; inline;
operator *(const ALeft: TEletricPotential;     const ARight: TElettricCharge    ): TWork; inline;
operator *(const ALeft: TElettricChargeUnit; const ARight: TResistanceUnit): TMagneticFluxUnit; inline;
operator *(const ALeft: TElettricCharge;     const ARight: TResistance    ): TMagneticFlux; inline;
operator *(const ALeft: TResistanceUnit; const ARight: TElettricChargeUnit): TMagneticFluxUnit; inline;
operator *(const ALeft: TResistance;     const ARight: TElettricCharge    ): TMagneticFlux; inline;
operator /(const ALeft: TElettricChargeUnit; const ARight: TEletricPotentialUnit): TCapacitanceUnit; inline;
operator /(const ALeft: TElettricCharge;     const ARight: TEletricPotential    ): TCapacitance; inline;
operator /(const ALeft: TElettricChargeUnit; const ARight: TCapacitanceUnit): TEletricPotentialUnit; inline;
operator /(const ALeft: TElettricCharge;     const ARight: TCapacitance    ): TEletricPotential; inline;
operator /(const ALeft: TElettricChargeUnit; const ARight: TEletricalConductanceUnit): TMagneticFluxUnit; inline;
operator /(const ALeft: TElettricCharge;     const ARight: TEletricalConductance    ): TMagneticFlux; inline;
operator /(const ALeft: TElettricChargeUnit; const ARight: TMagneticFluxUnit): TEletricalConductanceUnit; inline;
operator /(const ALeft: TElettricCharge;     const ARight: TMagneticFlux    ): TEletricalConductance; inline;
operator *(const ALeft: TEletricPotentialUnit; const ARight: TCapacitanceUnit): TElettricChargeUnit; inline;
operator *(const ALeft: TEletricPotential;     const ARight: TCapacitance    ): TElettricCharge; inline;
operator *(const ALeft: TCapacitanceUnit; const ARight: TEletricPotentialUnit): TElettricChargeUnit; inline;
operator *(const ALeft: TCapacitance;     const ARight: TEletricPotential    ): TElettricCharge; inline;
operator *(const ALeft: TEletricPotentialUnit; const ARight: TTimeUnit): TMagneticFluxUnit; inline;
operator *(const ALeft: TEletricPotential;     const ARight: TTime    ): TMagneticFlux; inline;
operator *(const ALeft: TTimeUnit; const ARight: TEletricPotentialUnit): TMagneticFluxUnit; inline;
operator *(const ALeft: TTime;     const ARight: TEletricPotential    ): TMagneticFlux; inline;
operator /(const ALeft: TEletricPotentialUnit; const ARight: TFrequencyUnit): TMagneticFluxUnit; inline;
operator /(const ALeft: TEletricPotential;     const ARight: TFrequency    ): TMagneticFlux; inline;
operator /(const ALeft: TEletricPotentialUnit; const ARight: TMagneticFluxUnit): TFrequencyUnit; inline;
operator /(const ALeft: TEletricPotential;     const ARight: TMagneticFlux    ): TFrequency; inline;
operator *(const ALeft: TCapacitanceUnit; const ARight: TResistanceUnit): TTimeUnit; inline;
operator *(const ALeft: TCapacitance;     const ARight: TResistance    ): TTime; inline;
operator *(const ALeft: TResistanceUnit; const ARight: TCapacitanceUnit): TTimeUnit; inline;
operator *(const ALeft: TResistance;     const ARight: TCapacitance    ): TTime; inline;
operator *(const ALeft: TCapacitanceUnit; const ARight: TInductanceUnit): TTimeSquareUnit; inline;
operator *(const ALeft: TCapacitance;     const ARight: TInductance    ): TTimeSquare; inline;
operator *(const ALeft: TInductanceUnit; const ARight: TCapacitanceUnit): TTimeSquareUnit; inline;
operator *(const ALeft: TInductance;     const ARight: TCapacitance    ): TTimeSquare; inline;
operator /(const ALeft: TCapacitanceUnit; const ARight: TEletricalConductanceUnit): TTimeUnit; inline;
operator /(const ALeft: TCapacitance;     const ARight: TEletricalConductance    ): TTime; inline;
operator /(const ALeft: TCapacitanceUnit; const ARight: TTimeUnit): TEletricalConductanceUnit; inline;
operator /(const ALeft: TCapacitance;     const ARight: TTime    ): TEletricalConductance; inline;
operator *(const ALeft: TResistanceUnit; const ARight: TTimeUnit): TInductanceUnit; inline;
operator *(const ALeft: TResistance;     const ARight: TTime    ): TInductance; inline;
operator *(const ALeft: TTimeUnit; const ARight: TResistanceUnit): TInductanceUnit; inline;
operator *(const ALeft: TTime;     const ARight: TResistance    ): TInductance; inline;
operator /(const ALeft: TResistanceUnit; const ARight: TFrequencyUnit): TInductanceUnit; inline;
operator /(const ALeft: TResistance;     const ARight: TFrequency    ): TInductance; inline;
operator /(const ALeft: TResistanceUnit; const ARight: TInductanceUnit): TFrequencyUnit; inline;
operator /(const ALeft: TResistance;     const ARight: TInductance    ): TFrequency; inline;
operator *(const ALeft: TEletricalConductanceUnit; const ARight: TMagneticFluxUnit): TElettricChargeUnit; inline;
operator *(const ALeft: TEletricalConductance;     const ARight: TMagneticFlux    ): TElettricCharge; inline;
operator *(const ALeft: TMagneticFluxUnit; const ARight: TEletricalConductanceUnit): TElettricChargeUnit; inline;
operator *(const ALeft: TMagneticFlux;     const ARight: TEletricalConductance    ): TElettricCharge; inline;
operator *(const ALeft: TEletricalConductanceUnit; const ARight: TInductanceUnit): TTimeUnit; inline;
operator *(const ALeft: TEletricalConductance;     const ARight: TInductance    ): TTime; inline;
operator *(const ALeft: TInductanceUnit; const ARight: TEletricalConductanceUnit): TTimeUnit; inline;
operator *(const ALeft: TInductance;     const ARight: TEletricalConductance    ): TTime; inline;
operator *(const ALeft: TEletricalConductanceUnit; const ARight: TTimeUnit): TCapacitanceUnit; inline;
operator *(const ALeft: TEletricalConductance;     const ARight: TTime    ): TCapacitance; inline;
operator *(const ALeft: TTimeUnit; const ARight: TEletricalConductanceUnit): TCapacitanceUnit; inline;
operator *(const ALeft: TTime;     const ARight: TEletricalConductance    ): TCapacitance; inline;
operator /(const ALeft: TEletricalConductanceUnit; const ARight: TFrequencyUnit): TCapacitanceUnit; inline;
operator /(const ALeft: TEletricalConductance;     const ARight: TFrequency    ): TCapacitance; inline;
operator /(const ALeft: TEletricalConductanceUnit; const ARight: TCapacitanceUnit): TFrequencyUnit; inline;
operator /(const ALeft: TEletricalConductance;     const ARight: TCapacitance    ): TFrequency; inline;
operator *(const ALeft: TMagneticFluxUnit; const ARight: TCurrentDensityUnit): TStiffnessUnit; inline;
operator *(const ALeft: TMagneticFlux;     const ARight: TCurrentDensity    ): TStiffness; inline;
operator *(const ALeft: TCurrentDensityUnit; const ARight: TMagneticFluxUnit): TStiffnessUnit; inline;
operator *(const ALeft: TCurrentDensity;     const ARight: TMagneticFlux    ): TStiffness; inline;
operator *(const ALeft: TMagneticFluxUnit; const ARight: TInvAreaUnit): TMagneticFluxDensityUnit; inline;
operator *(const ALeft: TMagneticFlux;     const ARight: TInvArea    ): TMagneticFluxDensity; inline;
operator *(const ALeft: TInvAreaUnit; const ARight: TMagneticFluxUnit): TMagneticFluxDensityUnit; inline;
operator *(const ALeft: TInvArea;     const ARight: TMagneticFlux    ): TMagneticFluxDensity; inline;
operator /(const ALeft: TMagneticFluxUnit; const ARight: TAreaUnit): TMagneticFluxDensityUnit; inline;
operator /(const ALeft: TMagneticFlux;     const ARight: TArea    ): TMagneticFluxDensity; inline;
operator /(const ALeft: TMagneticFluxUnit; const ARight: TElettricChargeUnit): TResistanceUnit; inline;
operator /(const ALeft: TMagneticFlux;     const ARight: TElettricCharge    ): TResistance; inline;
operator /(const ALeft: TMagneticFluxUnit; const ARight: TEletricPotentialUnit): TTimeUnit; inline;
operator /(const ALeft: TMagneticFlux;     const ARight: TEletricPotential    ): TTime; inline;
operator /(const ALeft: TMagneticFluxUnit; const ARight: TResistanceUnit): TElettricChargeUnit; inline;
operator /(const ALeft: TMagneticFlux;     const ARight: TResistance    ): TElettricCharge; inline;
operator /(const ALeft: TMagneticFluxUnit; const ARight: TMagneticFluxDensityUnit): TAreaUnit; inline;
operator /(const ALeft: TMagneticFlux;     const ARight: TMagneticFluxDensity    ): TArea; inline;
operator /(const ALeft: TMagneticFluxUnit; const ARight: TTimeUnit): TEletricPotentialUnit; inline;
operator /(const ALeft: TMagneticFlux;     const ARight: TTime    ): TEletricPotential; inline;
operator /(const ALeft: TMagneticFluxDensityUnit; const ARight: TMagneticFluxUnit): TInvAreaUnit; inline;
operator /(const ALeft: TMagneticFluxDensity;     const ARight: TMagneticFlux    ): TInvArea; inline;
operator /(const ALeft: TMagneticFluxDensityUnit; const ARight: TInductanceUnit): TCurrentDensityUnit; inline;
operator /(const ALeft: TMagneticFluxDensity;     const ARight: TInductance    ): TCurrentDensity; inline;
operator /(const ALeft: TMagneticFluxDensityUnit; const ARight: TCurrentDensityUnit): TInductanceUnit; inline;
operator /(const ALeft: TMagneticFluxDensity;     const ARight: TCurrentDensity    ): TInductance; inline;
operator /(const ALeft: TMagneticFluxDensityUnit; const ARight: TInvAreaUnit): TMagneticFluxUnit; inline;
operator /(const ALeft: TMagneticFluxDensity;     const ARight: TInvArea    ): TMagneticFlux; inline;
operator *(const ALeft: TInductanceUnit; const ARight: TCurrentDensityUnit): TMagneticFluxDensityUnit; inline;
operator *(const ALeft: TInductance;     const ARight: TCurrentDensity    ): TMagneticFluxDensity; inline;
operator *(const ALeft: TCurrentDensityUnit; const ARight: TInductanceUnit): TMagneticFluxDensityUnit; inline;
operator *(const ALeft: TCurrentDensity;     const ARight: TInductance    ): TMagneticFluxDensity; inline;
operator /(const ALeft: TInductanceUnit; const ARight: TResistanceUnit): TTimeUnit; inline;
operator /(const ALeft: TInductance;     const ARight: TResistance    ): TTime; inline;
operator /(const ALeft: TInductanceUnit; const ARight: TTimeUnit): TResistanceUnit; inline;
operator /(const ALeft: TInductance;     const ARight: TTime    ): TResistance; inline;
operator *(const ALeft: TLuminousFluxUnit; const ARight: TInvAreaUnit): TIlluminanceUnit; inline;
operator *(const ALeft: TLuminousFlux;     const ARight: TInvArea    ): TIlluminance; inline;
operator *(const ALeft: TInvAreaUnit; const ARight: TLuminousFluxUnit): TIlluminanceUnit; inline;
operator *(const ALeft: TInvArea;     const ARight: TLuminousFlux    ): TIlluminance; inline;
operator /(const ALeft: TLuminousFluxUnit; const ARight: TAreaUnit): TIlluminanceUnit; inline;
operator /(const ALeft: TLuminousFlux;     const ARight: TArea    ): TIlluminance; inline;
operator /(const ALeft: TLuminousFluxUnit; const ARight: TIlluminanceUnit): TAreaUnit; inline;
operator /(const ALeft: TLuminousFlux;     const ARight: TIlluminance    ): TArea; inline;
operator /(const ALeft: TIlluminanceUnit; const ARight: TLuminousFluxUnit): TInvAreaUnit; inline;
operator /(const ALeft: TIlluminance;     const ARight: TLuminousFlux    ): TInvArea; inline;
operator /(const ALeft: TIlluminanceUnit; const ARight: TInvAreaUnit): TLuminousFluxUnit; inline;
operator /(const ALeft: TIlluminance;     const ARight: TInvArea    ): TLuminousFlux; inline;
operator *(const ALeft: TWaveNumberUnit; const ARight: TWaveNumberUnit): TInvAreaUnit; inline;
operator *(const ALeft: TWaveNumber;     const ARight: TWaveNumber    ): TInvArea; inline;
operator *(const ALeft: TWaveNumberUnit; const ARight: TInvAreaUnit): TInvVolumeUnit; inline;
operator *(const ALeft: TWaveNumber;     const ARight: TInvArea    ): TInvVolume; inline;
operator *(const ALeft: TInvAreaUnit; const ARight: TWaveNumberUnit): TInvVolumeUnit; inline;
operator *(const ALeft: TInvArea;     const ARight: TWaveNumber    ): TInvVolume; inline;
operator /(const ALeft: TWaveNumberUnit; const ARight: TLengthUnit): TInvAreaUnit; inline;
operator /(const ALeft: TWaveNumber;     const ARight: TLength    ): TInvArea; inline;
operator /(const ALeft: TWaveNumberUnit; const ARight: TAreaUnit): TInvVolumeUnit; inline;
operator /(const ALeft: TWaveNumber;     const ARight: TArea    ): TInvVolume; inline;
operator /(const ALeft: TWaveNumberUnit; const ARight: TInvVolumeUnit): TAreaUnit; inline;
operator /(const ALeft: TWaveNumber;     const ARight: TInvVolume    ): TArea; inline;
operator /(const ALeft: TWaveNumberUnit; const ARight: TInvAreaUnit): TLengthUnit; inline;
operator /(const ALeft: TWaveNumber;     const ARight: TInvArea    ): TLength; inline;
operator *(const ALeft: TTimeUnit; const ARight: TTimeUnit): TTimeSquareUnit; inline;
operator *(const ALeft: TTime;     const ARight: TTime    ): TTimeSquare; inline;
operator *(const ALeft: TTimeUnit; const ARight: TTimeSquareUnit): TTimeCubeUnit; inline;
operator *(const ALeft: TTime;     const ARight: TTimeSquare    ): TTimeCube; inline;
operator *(const ALeft: TTimeSquareUnit; const ARight: TTimeUnit): TTimeCubeUnit; inline;
operator *(const ALeft: TTimeSquare;     const ARight: TTime    ): TTimeCube; inline;
operator /(const ALeft: TTimeUnit; const ARight: TFrequencyUnit): TTimeSquareUnit; inline;
operator /(const ALeft: TTime;     const ARight: TFrequency    ): TTimeSquare; inline;
operator /(const ALeft: TTimeUnit; const ARight: TFrequencySquareUnit): TTimeCubeUnit; inline;
operator /(const ALeft: TTime;     const ARight: TFrequencySquare    ): TTimeCube; inline;
operator /(const ALeft: TTimeUnit; const ARight: TCapacitanceUnit): TResistanceUnit; inline;
operator /(const ALeft: TTime;     const ARight: TCapacitance    ): TResistance; inline;
operator /(const ALeft: TTimeUnit; const ARight: TResistanceUnit): TCapacitanceUnit; inline;
operator /(const ALeft: TTime;     const ARight: TResistance    ): TCapacitance; inline;
operator /(const ALeft: TTimeUnit; const ARight: TEletricalConductanceUnit): TInductanceUnit; inline;
operator /(const ALeft: TTime;     const ARight: TEletricalConductance    ): TInductance; inline;
operator /(const ALeft: TTimeUnit; const ARight: TInductanceUnit): TEletricalConductanceUnit; inline;
operator /(const ALeft: TTime;     const ARight: TInductance    ): TEletricalConductance; inline;
operator /(const ALeft: TTimeUnit; const ARight: TTimeSquareUnit): TFrequencyUnit; inline;
operator /(const ALeft: TTime;     const ARight: TTimeSquare    ): TFrequency; inline;
operator /(const ALeft: TTimeUnit; const ARight: TTimeCubeUnit): TFrequencySquareUnit; inline;
operator /(const ALeft: TTime;     const ARight: TTimeCube    ): TFrequencySquare; inline;
operator /(const ALeft: TTimeSquareUnit; const ARight: TFrequencyUnit): TTimeCubeUnit; inline;
operator /(const ALeft: TTimeSquare;     const ARight: TFrequency    ): TTimeCube; inline;
operator /(const ALeft: TTimeSquareUnit; const ARight: TCapacitanceUnit): TInductanceUnit; inline;
operator /(const ALeft: TTimeSquare;     const ARight: TCapacitance    ): TInductance; inline;
operator /(const ALeft: TTimeSquareUnit; const ARight: TInductanceUnit): TCapacitanceUnit; inline;
operator /(const ALeft: TTimeSquare;     const ARight: TInductance    ): TCapacitance; inline;
operator /(const ALeft: TTimeSquareUnit; const ARight: TTimeUnit): TTimeUnit; inline;
operator /(const ALeft: TTimeSquare;     const ARight: TTime    ): TTime; inline;
operator /(const ALeft: TTimeSquareUnit; const ARight: TTimeCubeUnit): TFrequencyUnit; inline;
operator /(const ALeft: TTimeSquare;     const ARight: TTimeCube    ): TFrequency; inline;
operator /(const ALeft: TTimeCubeUnit; const ARight: TTimeUnit): TTimeSquareUnit; inline;
operator /(const ALeft: TTimeCube;     const ARight: TTime    ): TTimeSquare; inline;
operator /(const ALeft: TTimeCubeUnit; const ARight: TTimeSquareUnit): TTimeUnit; inline;
operator /(const ALeft: TTimeCube;     const ARight: TTimeSquare    ): TTime; inline;
operator /(const ALeft: TInvVolumeUnit; const ARight: TWaveNumberUnit): TInvAreaUnit; inline;
operator /(const ALeft: TInvVolume;     const ARight: TWaveNumber    ): TInvArea; inline;
operator /(const ALeft: TInvVolumeUnit; const ARight: TInvAreaUnit): TWaveNumberUnit; inline;
operator /(const ALeft: TInvVolume;     const ARight: TInvArea    ): TWaveNumber; inline;
operator /(const ALeft: TInvAreaUnit; const ARight: TLengthUnit): TInvVolumeUnit; inline;
operator /(const ALeft: TInvArea;     const ARight: TLength    ): TInvVolume; inline;
operator /(const ALeft: TInvAreaUnit; const ARight: TWaveNumberUnit): TWaveNumberUnit; inline;
operator /(const ALeft: TInvArea;     const ARight: TWaveNumber    ): TWaveNumber; inline;
operator /(const ALeft: TInvAreaUnit; const ARight: TInvVolumeUnit): TLengthUnit; inline;
operator /(const ALeft: TInvArea;     const ARight: TInvVolume    ): TLength; inline;

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

class operator TDimension.+(const v1, v2: TSelf): TSelf;
begin
  result.Value := v1.Value + v2.Value;
end;

class operator TDimension.-(const v1, v2: TSelf): TSelf;
begin
  result.Value := v1.Value - v2.Value;
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

class operator TDimension./(const v1, v2: TSelf): double;
begin
  result := v1.Value / v2.Value;
end;

class operator TDimension.=(const v1, v2: TSelf): boolean;
begin
  result := v1.Value = v2.Value;
end;

class operator TDimension.<(const v1, v2: TSelf): boolean;
begin
  result := v1.Value < v2.Value;
end;

class operator TDimension.>(const v1, v2: TSelf): boolean;
begin
  result := v1.Value > v2.Value;
end;

class operator TDimension.<=(const v1, v2: TSelf): boolean;
begin
  result := v1.Value <= v2.Value;
end;

class operator TDimension.>=(const v1, v2: TSelf): boolean;
begin
  result := v1.Value >= v2.Value;
end;

class operator TDimension.mod(const v1, v2: TSelf): TSelf;
begin
  result.Value := v1.Value mod v2.Value;
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

{ Units of UnitPerSecond }

class function TUnitPerSecond.GetSymbol: string;
begin
  result := '1/s';
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

{ Units of KilogramMeter2PerSecond2 }

class function TKilogramMeter2PerSecond2.GetSymbol: string;
begin
  result := 'kgm2/s2';
end;

{ Units of KilogramMeter2PerSecond3 }

class function TKilogramMeter2PerSecond3.GetSymbol: string;
begin
  result := 'kgm2/s3';
end;

{ Units of MeterPerSecond }

class function TMeterPerSecond.GetSymbol: string;
begin
  result := 'm/s';
end;

{ Units of Meter2PerSecond2 }

class function TMeter2PerSecond2.GetSymbol: string;
begin
  result := 'm2/s2';
end;

{ Units of MeterPerSecond2 }

class function TMeterPerSecond2.GetSymbol: string;
begin
  result := 'm/s2';
end;

{ Units of Meter2PerSecond4 }

class function TMeter2PerSecond4.GetSymbol: string;
begin
  result := 'm2/s4';
end;

{ Units of KilogramPerMeter }

class function TKilogramPerMeter.GetSymbol: string;
begin
  result := 'kg/m';
end;

{ Units of KilogramPerMeter2 }

class function TKilogramPerMeter2.GetSymbol: string;
begin
  result := 'kg/m2';
end;

{ Units of KilogramPerMeter3 }

class function TKilogramPerMeter3.GetSymbol: string;
begin
  result := 'kg/m3';
end;

{ Units of UnitPerSecond2 }

class function TUnitPerSecond2.GetSymbol: string;
begin
  result := '1/s2';
end;

{ Units of Kilogram }

class function TKilogram.GetSymbol: string;
begin
  result := 'kg';
end;

{ Units of KilogramPerSecond2 }

class function TKilogramPerSecond2.GetSymbol: string;
begin
  result := 'kg/s2';
end;

{ Units of SecondAmpere }

class function TSecondAmpere.GetSymbol: string;
begin
  result := 'sA';
end;

{ Units of KilogramMeter2PerAmpereSecond3 }

class function TKilogramMeter2PerAmpereSecond3.GetSymbol: string;
begin
  result := 'kgm2/As3';
end;

{ Units of Second4Ampere2PerKilogramMeter2 }

class function TSecond4Ampere2PerKilogramMeter2.GetSymbol: string;
begin
  result := 's4A2/kgm2';
end;

{ Units of KilogramMeter2PerSecond3Ampere2 }

class function TKilogramMeter2PerSecond3Ampere2.GetSymbol: string;
begin
  result := 'kgm2/s3A2';
end;

{ Units of Second3Ampere2PerKilogramMeter2 }

class function TSecond3Ampere2PerKilogramMeter2.GetSymbol: string;
begin
  result := 's3A2/kgm2';
end;

{ Units of KilogramMeter2PerAmpereSecond2 }

class function TKilogramMeter2PerAmpereSecond2.GetSymbol: string;
begin
  result := 'kgm2/As2';
end;

{ Units of KilogramPerAmpereSecond2 }

class function TKilogramPerAmpereSecond2.GetSymbol: string;
begin
  result := 'kg/As2';
end;

{ Units of KilogramMeter2PerSecond2Ampere2 }

class function TKilogramMeter2PerSecond2Ampere2.GetSymbol: string;
begin
  result := 'kgm2/s2A2';
end;

{ Units of Kelvin }

class function TKelvin.GetSymbol: string;
begin
  result := 'K';
end;

{ Units of Candela }

class function TCandela.GetSymbol: string;
begin
  result := 'cd';
end;

{ Units of CandelaPerMeter2 }

class function TCandelaPerMeter2.GetSymbol: string;
begin
  result := 'cd/m2';
end;

{ Units of MolePerSecond }

class function TMolePerSecond.GetSymbol: string;
begin
  result := 'mol/s';
end;

{ Units of UnitPerMeter }

class function TUnitPerMeter.GetSymbol: string;
begin
  result := '1/m';
end;

{ Units of Meter3PerKilogram }

class function TMeter3PerKilogram.GetSymbol: string;
begin
  result := 'm3/kg';
end;

{ Units of AmperePerMeter2 }

class function TAmperePerMeter2.GetSymbol: string;
begin
  result := 'A/m2';
end;

{ Units of Second }

class function TSecond.GetSymbol: string;
begin
  result := 's';
end;

{ Units of Second2 }

class function TSecond2.GetSymbol: string;
begin
  result := 's2';
end;

{ Units of Second3 }

class function TSecond3.GetSymbol: string;
begin
  result := 's3';
end;

{ Units of UnitPerMeter3 }

class function TUnitPerMeter3.GetSymbol: string;
begin
  result := '1/m3';
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

operator *(const ALeft: TLengthUnit; const ARight: TVolumeUnit): TAreaSquareUnit;
begin
  result.Symbol := 'm4';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TLength;     const ARight: TVolume    ): TAreaSquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TVolumeUnit; const ARight: TLengthUnit): TAreaSquareUnit;
begin
  result.Symbol := 'm4';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TVolume;     const ARight: TLength    ): TAreaSquare;
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

operator *(const ALeft: TLengthUnit; const ARight: TAccelerationUnit): TSpeedSquareUnit;
begin
  result.Symbol := 'm2/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TLength;     const ARight: TAcceleration    ): TSpeedSquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAccelerationUnit; const ARight: TLengthUnit): TSpeedSquareUnit;
begin
  result.Symbol := 'm2/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TAcceleration;     const ARight: TLength    ): TSpeedSquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TLengthUnit; const ARight: TDensity1DUnit): TMassUnit;
begin
  result.Symbol := 'kg';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TLength;     const ARight: TDensity1D    ): TMass;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TDensity1DUnit; const ARight: TLengthUnit): TMassUnit;
begin
  result.Symbol := 'kg';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TDensity1D;     const ARight: TLength    ): TMass;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TLengthUnit; const ARight: TDensity2DUnit): TDensity1DUnit;
begin
  result.Symbol := 'kg/m';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TLength;     const ARight: TDensity2D    ): TDensity1D;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TDensity2DUnit; const ARight: TLengthUnit): TDensity1DUnit;
begin
  result.Symbol := 'kg/m';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TDensity2D;     const ARight: TLength    ): TDensity1D;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TLengthUnit; const ARight: TDensity3DUnit): TDensity2DUnit;
begin
  result.Symbol := 'kg/m2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TLength;     const ARight: TDensity3D    ): TDensity2D;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TDensity3DUnit; const ARight: TLengthUnit): TDensity2DUnit;
begin
  result.Symbol := 'kg/m2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TDensity3D;     const ARight: TLength    ): TDensity2D;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TLengthUnit; const ARight: TFrequencySquareUnit): TAccelerationUnit;
begin
  result.Symbol := 'm/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TLength;     const ARight: TFrequencySquare    ): TAcceleration;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TFrequencySquareUnit; const ARight: TLengthUnit): TAccelerationUnit;
begin
  result.Symbol := 'm/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TFrequencySquare;     const ARight: TLength    ): TAcceleration;
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

operator *(const ALeft: TLengthUnit; const ARight: TInvVolumeUnit): TInvAreaUnit;
begin
  result.Symbol := '1/m2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TLength;     const ARight: TInvVolume    ): TInvArea;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TInvVolumeUnit; const ARight: TLengthUnit): TInvAreaUnit;
begin
  result.Symbol := '1/m2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TInvVolume;     const ARight: TLength    ): TInvArea;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TLengthUnit; const ARight: TInvAreaUnit): TWaveNumberUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TLength;     const ARight: TInvArea    ): TWaveNumber;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TInvAreaUnit; const ARight: TLengthUnit): TWaveNumberUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TInvArea;     const ARight: TLength    ): TWaveNumber;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TLengthUnit; const ARight: TAreaUnit): TWaveNumberUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TLength;     const ARight: TArea    ): TWaveNumber;
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

operator /(const ALeft: TLengthUnit; const ARight: TAreaSquareUnit): TInvVolumeUnit;
begin
  result.Symbol := '1/m3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TLength;     const ARight: TAreaSquare    ): TInvVolume;
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

operator /(const ALeft: TLengthUnit; const ARight: TAccelerationUnit): TTimeSquareUnit;
begin
  result.Symbol := 's2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TLength;     const ARight: TAcceleration    ): TTimeSquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TLengthUnit; const ARight: TDensity2DUnit): TSpecificVolumeUnit;
begin
  result.Symbol := 'm3/kg';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TLength;     const ARight: TDensity2D    ): TSpecificVolume;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TLengthUnit; const ARight: TWaveNumberUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TLength;     const ARight: TWaveNumber    ): TArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TLengthUnit; const ARight: TSpecificVolumeUnit): TDensity2DUnit;
begin
  result.Symbol := 'kg/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TLength;     const ARight: TSpecificVolume    ): TDensity2D;
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

operator /(const ALeft: TLengthUnit; const ARight: TTimeSquareUnit): TAccelerationUnit;
begin
  result.Symbol := 'm/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TLength;     const ARight: TTimeSquare    ): TAcceleration;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TLengthUnit; const ARight: TInvVolumeUnit): TAreaSquareUnit;
begin
  result.Symbol := 'm4';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TLength;     const ARight: TInvVolume    ): TAreaSquare;
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

operator *(const ALeft: TAreaUnit; const ARight: TAreaUnit): TAreaSquareUnit;
begin
  result.Symbol := 'm4';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TArea;     const ARight: TArea    ): TAreaSquare;
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

operator *(const ALeft: TAreaUnit; const ARight: TDensity2DUnit): TMassUnit;
begin
  result.Symbol := 'kg';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TArea;     const ARight: TDensity2D    ): TMass;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TDensity2DUnit; const ARight: TAreaUnit): TMassUnit;
begin
  result.Symbol := 'kg';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TDensity2D;     const ARight: TArea    ): TMass;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAreaUnit; const ARight: TDensity3DUnit): TDensity1DUnit;
begin
  result.Symbol := 'kg/m';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TArea;     const ARight: TDensity3D    ): TDensity1D;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TDensity3DUnit; const ARight: TAreaUnit): TDensity1DUnit;
begin
  result.Symbol := 'kg/m';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TDensity3D;     const ARight: TArea    ): TDensity1D;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAreaUnit; const ARight: TFrequencySquareUnit): TSpeedSquareUnit;
begin
  result.Symbol := 'm2/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TArea;     const ARight: TFrequencySquare    ): TSpeedSquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TFrequencySquareUnit; const ARight: TAreaUnit): TSpeedSquareUnit;
begin
  result.Symbol := 'm2/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TFrequencySquare;     const ARight: TArea    ): TSpeedSquare;
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

operator *(const ALeft: TAreaUnit; const ARight: TMagneticFluxDensityUnit): TMagneticFluxUnit;
begin
  result.Symbol := 'kgm2/As2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TArea;     const ARight: TMagneticFluxDensity    ): TMagneticFlux;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMagneticFluxDensityUnit; const ARight: TAreaUnit): TMagneticFluxUnit;
begin
  result.Symbol := 'kgm2/As2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TMagneticFluxDensity;     const ARight: TArea    ): TMagneticFlux;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAreaUnit; const ARight: TIlluminanceUnit): TLuminousFluxUnit;
begin
  result.Symbol := 'cd';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TArea;     const ARight: TIlluminance    ): TLuminousFlux;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TIlluminanceUnit; const ARight: TAreaUnit): TLuminousFluxUnit;
begin
  result.Symbol := 'cd';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TIlluminance;     const ARight: TArea    ): TLuminousFlux;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAreaUnit; const ARight: TWaveNumberUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TArea;     const ARight: TWaveNumber    ): TLength;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TWaveNumberUnit; const ARight: TAreaUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TWaveNumber;     const ARight: TArea    ): TLength;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAreaUnit; const ARight: TInvVolumeUnit): TWaveNumberUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TArea;     const ARight: TInvVolume    ): TWaveNumber;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TInvVolumeUnit; const ARight: TAreaUnit): TWaveNumberUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TInvVolume;     const ARight: TArea    ): TWaveNumber;
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

operator /(const ALeft: TAreaUnit; const ARight: TVolumeUnit): TWaveNumberUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TArea;     const ARight: TVolume    ): TWaveNumber;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TAreaUnit; const ARight: TAreaSquareUnit): TInvAreaUnit;
begin
  result.Symbol := '1/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TArea;     const ARight: TAreaSquare    ): TInvArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TAreaUnit; const ARight: TSpeedSquareUnit): TTimeSquareUnit;
begin
  result.Symbol := 's2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TArea;     const ARight: TSpeedSquare    ): TTimeSquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TAreaUnit; const ARight: TDensity1DUnit): TSpecificVolumeUnit;
begin
  result.Symbol := 'm3/kg';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TArea;     const ARight: TDensity1D    ): TSpecificVolume;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TAreaUnit; const ARight: TWaveNumberUnit): TVolumeUnit;
begin
  result.Symbol := 'm3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TArea;     const ARight: TWaveNumber    ): TVolume;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TAreaUnit; const ARight: TSpecificVolumeUnit): TDensity1DUnit;
begin
  result.Symbol := 'kg/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TArea;     const ARight: TSpecificVolume    ): TDensity1D;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TAreaUnit; const ARight: TTimeSquareUnit): TSpeedSquareUnit;
begin
  result.Symbol := 'm2/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TArea;     const ARight: TTimeSquare    ): TSpeedSquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TAreaUnit; const ARight: TInvAreaUnit): TAreaSquareUnit;
begin
  result.Symbol := 'm4';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TArea;     const ARight: TInvArea    ): TAreaSquare;
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

operator *(const ALeft: TVolumeUnit; const ARight: TDensity3DUnit): TMassUnit;
begin
  result.Symbol := 'kg';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TVolume;     const ARight: TDensity3D    ): TMass;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TDensity3DUnit; const ARight: TVolumeUnit): TMassUnit;
begin
  result.Symbol := 'kg';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TDensity3D;     const ARight: TVolume    ): TMass;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TVolumeUnit; const ARight: TWaveNumberUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TVolume;     const ARight: TWaveNumber    ): TArea;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TWaveNumberUnit; const ARight: TVolumeUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TWaveNumber;     const ARight: TVolume    ): TArea;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TVolumeUnit; const ARight: TInvAreaUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TVolume;     const ARight: TInvArea    ): TLength;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TInvAreaUnit; const ARight: TVolumeUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TInvArea;     const ARight: TVolume    ): TLength;
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

operator /(const ALeft: TVolumeUnit; const ARight: TAreaSquareUnit): TWaveNumberUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TVolume;     const ARight: TAreaSquare    ): TWaveNumber;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TVolumeUnit; const ARight: TMassUnit): TSpecificVolumeUnit;
begin
  result.Symbol := 'm3/kg';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TVolume;     const ARight: TMass    ): TSpecificVolume;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TVolumeUnit; const ARight: TWaveNumberUnit): TAreaSquareUnit;
begin
  result.Symbol := 'm4';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TVolume;     const ARight: TWaveNumber    ): TAreaSquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TVolumeUnit; const ARight: TSpecificVolumeUnit): TMassUnit;
begin
  result.Symbol := 'kg';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TVolume;     const ARight: TSpecificVolume    ): TMass;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TAreaSquareUnit; const ARight: TWaveNumberUnit): TVolumeUnit;
begin
  result.Symbol := 'm3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TAreaSquare;     const ARight: TWaveNumber    ): TVolume;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TWaveNumberUnit; const ARight: TAreaSquareUnit): TVolumeUnit;
begin
  result.Symbol := 'm3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TWaveNumber;     const ARight: TAreaSquare    ): TVolume;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAreaSquareUnit; const ARight: TInvVolumeUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TAreaSquare;     const ARight: TInvVolume    ): TLength;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TInvVolumeUnit; const ARight: TAreaSquareUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TInvVolume;     const ARight: TAreaSquare    ): TLength;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAreaSquareUnit; const ARight: TInvAreaUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TAreaSquare;     const ARight: TInvArea    ): TArea;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TInvAreaUnit; const ARight: TAreaSquareUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TInvArea;     const ARight: TAreaSquare    ): TArea;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TAreaSquareUnit; const ARight: TLengthUnit): TVolumeUnit;
begin
  result.Symbol := 'm3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TAreaSquare;     const ARight: TLength    ): TVolume;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TAreaSquareUnit; const ARight: TAreaUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TAreaSquare;     const ARight: TArea    ): TArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TAreaSquareUnit; const ARight: TVolumeUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TAreaSquare;     const ARight: TVolume    ): TLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TFrequencyUnit; const ARight: TFrequencyUnit): TFrequencySquareUnit;
begin
  result.Symbol := '1/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TFrequency;     const ARight: TFrequency    ): TFrequencySquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TFrequencyUnit; const ARight: TWorkUnit): TPowerUnit;
begin
  result.Symbol := 'kgm2/s3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TFrequency;     const ARight: TWork    ): TPower;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TWorkUnit; const ARight: TFrequencyUnit): TPowerUnit;
begin
  result.Symbol := 'kgm2/s3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TWork;     const ARight: TFrequency    ): TPower;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TFrequencyUnit; const ARight: TSpeedUnit): TAccelerationUnit;
begin
  result.Symbol := 'm/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TFrequency;     const ARight: TSpeed    ): TAcceleration;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSpeedUnit; const ARight: TFrequencyUnit): TAccelerationUnit;
begin
  result.Symbol := 'm/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TSpeed;     const ARight: TFrequency    ): TAcceleration;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TFrequencyUnit; const ARight: TCapacitanceUnit): TEletricalConductanceUnit;
begin
  result.Symbol := 's3A2/kgm2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TFrequency;     const ARight: TCapacitance    ): TEletricalConductance;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TCapacitanceUnit; const ARight: TFrequencyUnit): TEletricalConductanceUnit;
begin
  result.Symbol := 's3A2/kgm2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TCapacitance;     const ARight: TFrequency    ): TEletricalConductance;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TFrequencyUnit; const ARight: TMagneticFluxUnit): TEletricPotentialUnit;
begin
  result.Symbol := 'kgm2/As3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TFrequency;     const ARight: TMagneticFlux    ): TEletricPotential;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMagneticFluxUnit; const ARight: TFrequencyUnit): TEletricPotentialUnit;
begin
  result.Symbol := 'kgm2/As3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TMagneticFlux;     const ARight: TFrequency    ): TEletricPotential;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TFrequencyUnit; const ARight: TInductanceUnit): TResistanceUnit;
begin
  result.Symbol := 'kgm2/s3A2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TFrequency;     const ARight: TInductance    ): TResistance;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TInductanceUnit; const ARight: TFrequencyUnit): TResistanceUnit;
begin
  result.Symbol := 'kgm2/s3A2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TInductance;     const ARight: TFrequency    ): TResistance;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TFrequencyUnit; const ARight: TTimeSquareUnit): TTimeUnit;
begin
  result.Symbol := 's';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TFrequency;     const ARight: TTimeSquare    ): TTime;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TTimeSquareUnit; const ARight: TFrequencyUnit): TTimeUnit;
begin
  result.Symbol := 's';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TTimeSquare;     const ARight: TFrequency    ): TTime;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TFrequencyUnit; const ARight: TTimeCubeUnit): TTimeSquareUnit;
begin
  result.Symbol := 's2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TFrequency;     const ARight: TTimeCube    ): TTimeSquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TTimeCubeUnit; const ARight: TFrequencyUnit): TTimeSquareUnit;
begin
  result.Symbol := 's2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TTimeCube;     const ARight: TFrequency    ): TTimeSquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TFrequencyUnit; const ARight: TSpeedUnit): TWaveNumberUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TFrequency;     const ARight: TSpeed    ): TWaveNumber;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TFrequencyUnit; const ARight: TFrequencySquareUnit): TTimeUnit;
begin
  result.Symbol := 's';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TFrequency;     const ARight: TFrequencySquare    ): TTime;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TFrequencyUnit; const ARight: TWaveNumberUnit): TSpeedUnit;
begin
  result.Symbol := 'm/s';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TFrequency;     const ARight: TWaveNumber    ): TSpeed;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TFrequencyUnit; const ARight: TTimeUnit): TFrequencySquareUnit;
begin
  result.Symbol := '1/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TFrequency;     const ARight: TTime    ): TFrequencySquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TForceUnit; const ARight: TSpeedUnit): TPowerUnit;
begin
  result.Symbol := 'kgm2/s3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TForce;     const ARight: TSpeed    ): TPower;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSpeedUnit; const ARight: TForceUnit): TPowerUnit;
begin
  result.Symbol := 'kgm2/s3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TSpeed;     const ARight: TForce    ): TPower;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TForceUnit; const ARight: TWaveNumberUnit): TStiffnessUnit;
begin
  result.Symbol := 'kg/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TForce;     const ARight: TWaveNumber    ): TStiffness;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TWaveNumberUnit; const ARight: TForceUnit): TStiffnessUnit;
begin
  result.Symbol := 'kg/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TWaveNumber;     const ARight: TForce    ): TStiffness;
begin
  result.Value := ALeft.Value * ARight.Value;
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

operator /(const ALeft: TForceUnit; const ARight: TWorkUnit): TWaveNumberUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TForce;     const ARight: TWork    ): TWaveNumber;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TForceUnit; const ARight: TSpeedSquareUnit): TDensity1DUnit;
begin
  result.Symbol := 'kg/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TForce;     const ARight: TSpeedSquare    ): TDensity1D;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TForceUnit; const ARight: TAccelerationUnit): TMassUnit;
begin
  result.Symbol := 'kg';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TForce;     const ARight: TAcceleration    ): TMass;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TForceUnit; const ARight: TDensity1DUnit): TSpeedSquareUnit;
begin
  result.Symbol := 'm2/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TForce;     const ARight: TDensity1D    ): TSpeedSquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TForceUnit; const ARight: TMassUnit): TAccelerationUnit;
begin
  result.Symbol := 'm/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TForce;     const ARight: TMass    ): TAcceleration;
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

operator /(const ALeft: TForceUnit; const ARight: TWaveNumberUnit): TWorkUnit;
begin
  result.Symbol := 'kgm2/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TForce;     const ARight: TWaveNumber    ): TWork;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TPressureUnit; const ARight: TSpecificVolumeUnit): TSpeedSquareUnit;
begin
  result.Symbol := 'm2/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TPressure;     const ARight: TSpecificVolume    ): TSpeedSquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSpecificVolumeUnit; const ARight: TPressureUnit): TSpeedSquareUnit;
begin
  result.Symbol := 'm2/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TSpecificVolume;     const ARight: TPressure    ): TSpeedSquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TPressureUnit; const ARight: TTimeSquareUnit): TDensity1DUnit;
begin
  result.Symbol := 'kg/m';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TPressure;     const ARight: TTimeSquare    ): TDensity1D;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TTimeSquareUnit; const ARight: TPressureUnit): TDensity1DUnit;
begin
  result.Symbol := 'kg/m';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TTimeSquare;     const ARight: TPressure    ): TDensity1D;
begin
  result.Value := ALeft.Value * ARight.Value;
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

operator /(const ALeft: TPressureUnit; const ARight: TWorkUnit): TInvVolumeUnit;
begin
  result.Symbol := '1/m3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TPressure;     const ARight: TWork    ): TInvVolume;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TPressureUnit; const ARight: TSpeedSquareUnit): TDensity3DUnit;
begin
  result.Symbol := 'kg/m3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TPressure;     const ARight: TSpeedSquare    ): TDensity3D;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TPressureUnit; const ARight: TAccelerationUnit): TDensity2DUnit;
begin
  result.Symbol := 'kg/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TPressure;     const ARight: TAcceleration    ): TDensity2D;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TPressureUnit; const ARight: TDensity1DUnit): TFrequencySquareUnit;
begin
  result.Symbol := '1/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TPressure;     const ARight: TDensity1D    ): TFrequencySquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TPressureUnit; const ARight: TDensity2DUnit): TAccelerationUnit;
begin
  result.Symbol := 'm/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TPressure;     const ARight: TDensity2D    ): TAcceleration;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TPressureUnit; const ARight: TDensity3DUnit): TSpeedSquareUnit;
begin
  result.Symbol := 'm2/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TPressure;     const ARight: TDensity3D    ): TSpeedSquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TPressureUnit; const ARight: TFrequencySquareUnit): TDensity1DUnit;
begin
  result.Symbol := 'kg/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TPressure;     const ARight: TFrequencySquare    ): TDensity1D;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TPressureUnit; const ARight: TStiffnessUnit): TWaveNumberUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TPressure;     const ARight: TStiffness    ): TWaveNumber;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TPressureUnit; const ARight: TWaveNumberUnit): TStiffnessUnit;
begin
  result.Symbol := 'kg/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TPressure;     const ARight: TWaveNumber    ): TStiffness;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TPressureUnit; const ARight: TInvVolumeUnit): TWorkUnit;
begin
  result.Symbol := 'kgm2/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TPressure;     const ARight: TInvVolume    ): TWork;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TPressureUnit; const ARight: TInvAreaUnit): TForceUnit;
begin
  result.Symbol := 'kgm/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TPressure;     const ARight: TInvArea    ): TForce;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TWorkUnit; const ARight: TWaveNumberUnit): TForceUnit;
begin
  result.Symbol := 'kgm/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TWork;     const ARight: TWaveNumber    ): TForce;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TWaveNumberUnit; const ARight: TWorkUnit): TForceUnit;
begin
  result.Symbol := 'kgm/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TWaveNumber;     const ARight: TWork    ): TForce;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TWorkUnit; const ARight: TInvVolumeUnit): TPressureUnit;
begin
  result.Symbol := 'kg/ms2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TWork;     const ARight: TInvVolume    ): TPressure;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TInvVolumeUnit; const ARight: TWorkUnit): TPressureUnit;
begin
  result.Symbol := 'kg/ms2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TInvVolume;     const ARight: TWork    ): TPressure;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TWorkUnit; const ARight: TInvAreaUnit): TStiffnessUnit;
begin
  result.Symbol := 'kg/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TWork;     const ARight: TInvArea    ): TStiffness;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TInvAreaUnit; const ARight: TWorkUnit): TStiffnessUnit;
begin
  result.Symbol := 'kg/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TInvArea;     const ARight: TWork    ): TStiffness;
begin
  result.Value := ALeft.Value * ARight.Value;
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

operator /(const ALeft: TWorkUnit; const ARight: TPowerUnit): TTimeUnit;
begin
  result.Symbol := 's';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TWork;     const ARight: TPower    ): TTime;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWorkUnit; const ARight: TSpeedSquareUnit): TMassUnit;
begin
  result.Symbol := 'kg';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TWork;     const ARight: TSpeedSquare    ): TMass;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWorkUnit; const ARight: TMassUnit): TSpeedSquareUnit;
begin
  result.Symbol := 'm2/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TWork;     const ARight: TMass    ): TSpeedSquare;
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

operator /(const ALeft: TWorkUnit; const ARight: TElettricChargeUnit): TEletricPotentialUnit;
begin
  result.Symbol := 'kgm2/As3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TWork;     const ARight: TElettricCharge    ): TEletricPotential;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWorkUnit; const ARight: TEletricPotentialUnit): TElettricChargeUnit;
begin
  result.Symbol := 'sA';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TWork;     const ARight: TEletricPotential    ): TElettricCharge;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWorkUnit; const ARight: TTimeUnit): TPowerUnit;
begin
  result.Symbol := 'kgm2/s3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TWork;     const ARight: TTime    ): TPower;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TPowerUnit; const ARight: TTimeUnit): TWorkUnit;
begin
  result.Symbol := 'kgm2/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TPower;     const ARight: TTime    ): TWork;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TTimeUnit; const ARight: TPowerUnit): TWorkUnit;
begin
  result.Symbol := 'kgm2/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TTime;     const ARight: TPower    ): TWork;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TPowerUnit; const ARight: TFrequencyUnit): TWorkUnit;
begin
  result.Symbol := 'kgm2/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TPower;     const ARight: TFrequency    ): TWork;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TPowerUnit; const ARight: TForceUnit): TSpeedUnit;
begin
  result.Symbol := 'm/s';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TPower;     const ARight: TForce    ): TSpeed;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TPowerUnit; const ARight: TWorkUnit): TFrequencyUnit;
begin
  result.Symbol := '1/s';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TPower;     const ARight: TWork    ): TFrequency;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TPowerUnit; const ARight: TSpeedUnit): TForceUnit;
begin
  result.Symbol := 'kgm/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TPower;     const ARight: TSpeed    ): TForce;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSpeedUnit; const ARight: TSpeedUnit): TSpeedSquareUnit;
begin
  result.Symbol := 'm2/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TSpeed;     const ARight: TSpeed    ): TSpeedSquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSpeedUnit; const ARight: TWaveNumberUnit): TFrequencyUnit;
begin
  result.Symbol := '1/s';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TSpeed;     const ARight: TWaveNumber    ): TFrequency;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TWaveNumberUnit; const ARight: TSpeedUnit): TFrequencyUnit;
begin
  result.Symbol := '1/s';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TWaveNumber;     const ARight: TSpeed    ): TFrequency;
begin
  result.Value := ALeft.Value * ARight.Value;
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

operator /(const ALeft: TSpeedUnit; const ARight: TAccelerationUnit): TTimeUnit;
begin
  result.Symbol := 's';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TSpeed;     const ARight: TAcceleration    ): TTime;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSpeedUnit; const ARight: TTimeUnit): TAccelerationUnit;
begin
  result.Symbol := 'm/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TSpeed;     const ARight: TTime    ): TAcceleration;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSpeedSquareUnit; const ARight: TDensity1DUnit): TForceUnit;
begin
  result.Symbol := 'kgm/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TSpeedSquare;     const ARight: TDensity1D    ): TForce;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TDensity1DUnit; const ARight: TSpeedSquareUnit): TForceUnit;
begin
  result.Symbol := 'kgm/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TDensity1D;     const ARight: TSpeedSquare    ): TForce;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSpeedSquareUnit; const ARight: TDensity2DUnit): TStiffnessUnit;
begin
  result.Symbol := 'kg/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TSpeedSquare;     const ARight: TDensity2D    ): TStiffness;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TDensity2DUnit; const ARight: TSpeedSquareUnit): TStiffnessUnit;
begin
  result.Symbol := 'kg/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TDensity2D;     const ARight: TSpeedSquare    ): TStiffness;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSpeedSquareUnit; const ARight: TDensity3DUnit): TPressureUnit;
begin
  result.Symbol := 'kg/ms2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TSpeedSquare;     const ARight: TDensity3D    ): TPressure;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TDensity3DUnit; const ARight: TSpeedSquareUnit): TPressureUnit;
begin
  result.Symbol := 'kg/ms2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TDensity3D;     const ARight: TSpeedSquare    ): TPressure;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSpeedSquareUnit; const ARight: TFrequencySquareUnit): TAccelerationSquareUnit;
begin
  result.Symbol := 'm2/s4';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TSpeedSquare;     const ARight: TFrequencySquare    ): TAccelerationSquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TFrequencySquareUnit; const ARight: TSpeedSquareUnit): TAccelerationSquareUnit;
begin
  result.Symbol := 'm2/s4';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TFrequencySquare;     const ARight: TSpeedSquare    ): TAccelerationSquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSpeedSquareUnit; const ARight: TMassUnit): TWorkUnit;
begin
  result.Symbol := 'kgm2/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TSpeedSquare;     const ARight: TMass    ): TWork;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMassUnit; const ARight: TSpeedSquareUnit): TWorkUnit;
begin
  result.Symbol := 'kgm2/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TMass;     const ARight: TSpeedSquare    ): TWork;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSpeedSquareUnit; const ARight: TWaveNumberUnit): TAccelerationUnit;
begin
  result.Symbol := 'm/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TSpeedSquare;     const ARight: TWaveNumber    ): TAcceleration;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TWaveNumberUnit; const ARight: TSpeedSquareUnit): TAccelerationUnit;
begin
  result.Symbol := 'm/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TWaveNumber;     const ARight: TSpeedSquare    ): TAcceleration;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSpeedSquareUnit; const ARight: TTimeSquareUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TSpeedSquare;     const ARight: TTimeSquare    ): TArea;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TTimeSquareUnit; const ARight: TSpeedSquareUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TTimeSquare;     const ARight: TSpeedSquare    ): TArea;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSpeedSquareUnit; const ARight: TInvAreaUnit): TFrequencySquareUnit;
begin
  result.Symbol := '1/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TSpeedSquare;     const ARight: TInvArea    ): TFrequencySquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TInvAreaUnit; const ARight: TSpeedSquareUnit): TFrequencySquareUnit;
begin
  result.Symbol := '1/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TInvArea;     const ARight: TSpeedSquare    ): TFrequencySquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSpeedSquareUnit; const ARight: TLengthUnit): TAccelerationUnit;
begin
  result.Symbol := 'm/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TSpeedSquare;     const ARight: TLength    ): TAcceleration;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSpeedSquareUnit; const ARight: TAreaUnit): TFrequencySquareUnit;
begin
  result.Symbol := '1/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TSpeedSquare;     const ARight: TArea    ): TFrequencySquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSpeedSquareUnit; const ARight: TPressureUnit): TSpecificVolumeUnit;
begin
  result.Symbol := 'm3/kg';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TSpeedSquare;     const ARight: TPressure    ): TSpecificVolume;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSpeedSquareUnit; const ARight: TSpeedUnit): TSpeedUnit;
begin
  result.Symbol := 'm/s';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TSpeedSquare;     const ARight: TSpeed    ): TSpeed;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSpeedSquareUnit; const ARight: TAccelerationUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TSpeedSquare;     const ARight: TAcceleration    ): TLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSpeedSquareUnit; const ARight: TAccelerationSquareUnit): TTimeSquareUnit;
begin
  result.Symbol := 's2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TSpeedSquare;     const ARight: TAccelerationSquare    ): TTimeSquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSpeedSquareUnit; const ARight: TFrequencySquareUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TSpeedSquare;     const ARight: TFrequencySquare    ): TArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSpeedSquareUnit; const ARight: TSpecificVolumeUnit): TPressureUnit;
begin
  result.Symbol := 'kg/ms2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TSpeedSquare;     const ARight: TSpecificVolume    ): TPressure;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSpeedSquareUnit; const ARight: TTimeSquareUnit): TAccelerationSquareUnit;
begin
  result.Symbol := 'm2/s4';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TSpeedSquare;     const ARight: TTimeSquare    ): TAccelerationSquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TAccelerationUnit; const ARight: TAccelerationUnit): TAccelerationSquareUnit;
begin
  result.Symbol := 'm2/s4';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TAcceleration;     const ARight: TAcceleration    ): TAccelerationSquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAccelerationUnit; const ARight: TDensity1DUnit): TStiffnessUnit;
begin
  result.Symbol := 'kg/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TAcceleration;     const ARight: TDensity1D    ): TStiffness;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TDensity1DUnit; const ARight: TAccelerationUnit): TStiffnessUnit;
begin
  result.Symbol := 'kg/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TDensity1D;     const ARight: TAcceleration    ): TStiffness;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAccelerationUnit; const ARight: TDensity2DUnit): TPressureUnit;
begin
  result.Symbol := 'kg/ms2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TAcceleration;     const ARight: TDensity2D    ): TPressure;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TDensity2DUnit; const ARight: TAccelerationUnit): TPressureUnit;
begin
  result.Symbol := 'kg/ms2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TDensity2D;     const ARight: TAcceleration    ): TPressure;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAccelerationUnit; const ARight: TMassUnit): TForceUnit;
begin
  result.Symbol := 'kgm/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TAcceleration;     const ARight: TMass    ): TForce;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMassUnit; const ARight: TAccelerationUnit): TForceUnit;
begin
  result.Symbol := 'kgm/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TMass;     const ARight: TAcceleration    ): TForce;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAccelerationUnit; const ARight: TWaveNumberUnit): TFrequencySquareUnit;
begin
  result.Symbol := '1/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TAcceleration;     const ARight: TWaveNumber    ): TFrequencySquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TWaveNumberUnit; const ARight: TAccelerationUnit): TFrequencySquareUnit;
begin
  result.Symbol := '1/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TWaveNumber;     const ARight: TAcceleration    ): TFrequencySquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAccelerationUnit; const ARight: TTimeUnit): TSpeedUnit;
begin
  result.Symbol := 'm/s';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TAcceleration;     const ARight: TTime    ): TSpeed;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TTimeUnit; const ARight: TAccelerationUnit): TSpeedUnit;
begin
  result.Symbol := 'm/s';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TTime;     const ARight: TAcceleration    ): TSpeed;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAccelerationUnit; const ARight: TTimeSquareUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TAcceleration;     const ARight: TTimeSquare    ): TLength;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TTimeSquareUnit; const ARight: TAccelerationUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TTimeSquare;     const ARight: TAcceleration    ): TLength;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TAccelerationUnit; const ARight: TLengthUnit): TFrequencySquareUnit;
begin
  result.Symbol := '1/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TAcceleration;     const ARight: TLength    ): TFrequencySquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TAccelerationUnit; const ARight: TFrequencyUnit): TSpeedUnit;
begin
  result.Symbol := 'm/s';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TAcceleration;     const ARight: TFrequency    ): TSpeed;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TAccelerationUnit; const ARight: TSpeedUnit): TFrequencyUnit;
begin
  result.Symbol := '1/s';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TAcceleration;     const ARight: TSpeed    ): TFrequency;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TAccelerationUnit; const ARight: TSpeedSquareUnit): TWaveNumberUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TAcceleration;     const ARight: TSpeedSquare    ): TWaveNumber;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TAccelerationUnit; const ARight: TFrequencySquareUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TAcceleration;     const ARight: TFrequencySquare    ): TLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TAccelerationUnit; const ARight: TWaveNumberUnit): TSpeedSquareUnit;
begin
  result.Symbol := 'm2/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TAcceleration;     const ARight: TWaveNumber    ): TSpeedSquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TAccelerationSquareUnit; const ARight: TTimeSquareUnit): TSpeedSquareUnit;
begin
  result.Symbol := 'm2/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TAccelerationSquare;     const ARight: TTimeSquare    ): TSpeedSquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TTimeSquareUnit; const ARight: TAccelerationSquareUnit): TSpeedSquareUnit;
begin
  result.Symbol := 'm2/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TTimeSquare;     const ARight: TAccelerationSquare    ): TSpeedSquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TAccelerationSquareUnit; const ARight: TSpeedSquareUnit): TFrequencySquareUnit;
begin
  result.Symbol := '1/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TAccelerationSquare;     const ARight: TSpeedSquare    ): TFrequencySquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TAccelerationSquareUnit; const ARight: TAccelerationUnit): TAccelerationUnit;
begin
  result.Symbol := 'm/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TAccelerationSquare;     const ARight: TAcceleration    ): TAcceleration;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TAccelerationSquareUnit; const ARight: TFrequencySquareUnit): TSpeedSquareUnit;
begin
  result.Symbol := 'm2/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TAccelerationSquare;     const ARight: TFrequencySquare    ): TSpeedSquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TDensity1DUnit; const ARight: TFrequencySquareUnit): TPressureUnit;
begin
  result.Symbol := 'kg/ms2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TDensity1D;     const ARight: TFrequencySquare    ): TPressure;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TFrequencySquareUnit; const ARight: TDensity1DUnit): TPressureUnit;
begin
  result.Symbol := 'kg/ms2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TFrequencySquare;     const ARight: TDensity1D    ): TPressure;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TDensity1DUnit; const ARight: TWaveNumberUnit): TDensity2DUnit;
begin
  result.Symbol := 'kg/m2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TDensity1D;     const ARight: TWaveNumber    ): TDensity2D;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TWaveNumberUnit; const ARight: TDensity1DUnit): TDensity2DUnit;
begin
  result.Symbol := 'kg/m2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TWaveNumber;     const ARight: TDensity1D    ): TDensity2D;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TDensity1DUnit; const ARight: TSpecificVolumeUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TDensity1D;     const ARight: TSpecificVolume    ): TArea;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSpecificVolumeUnit; const ARight: TDensity1DUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TSpecificVolume;     const ARight: TDensity1D    ): TArea;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TDensity1DUnit; const ARight: TInvAreaUnit): TDensity3DUnit;
begin
  result.Symbol := 'kg/m3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TDensity1D;     const ARight: TInvArea    ): TDensity3D;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TInvAreaUnit; const ARight: TDensity1DUnit): TDensity3DUnit;
begin
  result.Symbol := 'kg/m3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TInvArea;     const ARight: TDensity1D    ): TDensity3D;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TDensity1DUnit; const ARight: TLengthUnit): TDensity2DUnit;
begin
  result.Symbol := 'kg/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TDensity1D;     const ARight: TLength    ): TDensity2D;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TDensity1DUnit; const ARight: TAreaUnit): TDensity3DUnit;
begin
  result.Symbol := 'kg/m3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TDensity1D;     const ARight: TArea    ): TDensity3D;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TDensity1DUnit; const ARight: TPressureUnit): TTimeSquareUnit;
begin
  result.Symbol := 's2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TDensity1D;     const ARight: TPressure    ): TTimeSquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TDensity1DUnit; const ARight: TDensity2DUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TDensity1D;     const ARight: TDensity2D    ): TLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TDensity1DUnit; const ARight: TDensity3DUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TDensity1D;     const ARight: TDensity3D    ): TArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TDensity1DUnit; const ARight: TMassUnit): TWaveNumberUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TDensity1D;     const ARight: TMass    ): TWaveNumber;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TDensity1DUnit; const ARight: TWaveNumberUnit): TMassUnit;
begin
  result.Symbol := 'kg';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TDensity1D;     const ARight: TWaveNumber    ): TMass;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TDensity1DUnit; const ARight: TTimeSquareUnit): TPressureUnit;
begin
  result.Symbol := 'kg/ms2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TDensity1D;     const ARight: TTimeSquare    ): TPressure;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TDensity2DUnit; const ARight: TWaveNumberUnit): TDensity3DUnit;
begin
  result.Symbol := 'kg/m3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TDensity2D;     const ARight: TWaveNumber    ): TDensity3D;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TWaveNumberUnit; const ARight: TDensity2DUnit): TDensity3DUnit;
begin
  result.Symbol := 'kg/m3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TWaveNumber;     const ARight: TDensity2D    ): TDensity3D;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TDensity2DUnit; const ARight: TSpecificVolumeUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TDensity2D;     const ARight: TSpecificVolume    ): TLength;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSpecificVolumeUnit; const ARight: TDensity2DUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TSpecificVolume;     const ARight: TDensity2D    ): TLength;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TDensity2DUnit; const ARight: TLengthUnit): TDensity3DUnit;
begin
  result.Symbol := 'kg/m3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TDensity2D;     const ARight: TLength    ): TDensity3D;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TDensity2DUnit; const ARight: TDensity1DUnit): TWaveNumberUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TDensity2D;     const ARight: TDensity1D    ): TWaveNumber;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TDensity2DUnit; const ARight: TDensity3DUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TDensity2D;     const ARight: TDensity3D    ): TLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TDensity2DUnit; const ARight: TMassUnit): TInvAreaUnit;
begin
  result.Symbol := '1/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TDensity2D;     const ARight: TMass    ): TInvArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TDensity2DUnit; const ARight: TWaveNumberUnit): TDensity1DUnit;
begin
  result.Symbol := 'kg/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TDensity2D;     const ARight: TWaveNumber    ): TDensity1D;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TDensity2DUnit; const ARight: TInvAreaUnit): TMassUnit;
begin
  result.Symbol := 'kg';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TDensity2D;     const ARight: TInvArea    ): TMass;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TDensity3DUnit; const ARight: TDensity1DUnit): TInvAreaUnit;
begin
  result.Symbol := '1/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TDensity3D;     const ARight: TDensity1D    ): TInvArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TDensity3DUnit; const ARight: TDensity2DUnit): TWaveNumberUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TDensity3D;     const ARight: TDensity2D    ): TWaveNumber;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TDensity3DUnit; const ARight: TMassUnit): TInvVolumeUnit;
begin
  result.Symbol := '1/m3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TDensity3D;     const ARight: TMass    ): TInvVolume;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TDensity3DUnit; const ARight: TWaveNumberUnit): TDensity2DUnit;
begin
  result.Symbol := 'kg/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TDensity3D;     const ARight: TWaveNumber    ): TDensity2D;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TDensity3DUnit; const ARight: TInvVolumeUnit): TMassUnit;
begin
  result.Symbol := 'kg';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TDensity3D;     const ARight: TInvVolume    ): TMass;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TDensity3DUnit; const ARight: TInvAreaUnit): TDensity1DUnit;
begin
  result.Symbol := 'kg/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TDensity3D;     const ARight: TInvArea    ): TDensity1D;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TFrequencySquareUnit; const ARight: TMassUnit): TStiffnessUnit;
begin
  result.Symbol := 'kg/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TFrequencySquare;     const ARight: TMass    ): TStiffness;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMassUnit; const ARight: TFrequencySquareUnit): TStiffnessUnit;
begin
  result.Symbol := 'kg/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TMass;     const ARight: TFrequencySquare    ): TStiffness;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TFrequencySquareUnit; const ARight: TTimeUnit): TFrequencyUnit;
begin
  result.Symbol := '1/s';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TFrequencySquare;     const ARight: TTime    ): TFrequency;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TTimeUnit; const ARight: TFrequencySquareUnit): TFrequencyUnit;
begin
  result.Symbol := '1/s';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TTime;     const ARight: TFrequencySquare    ): TFrequency;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TFrequencySquareUnit; const ARight: TTimeCubeUnit): TTimeUnit;
begin
  result.Symbol := 's';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TFrequencySquare;     const ARight: TTimeCube    ): TTime;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TTimeCubeUnit; const ARight: TFrequencySquareUnit): TTimeUnit;
begin
  result.Symbol := 's';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TTimeCube;     const ARight: TFrequencySquare    ): TTime;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TFrequencySquareUnit; const ARight: TFrequencyUnit): TFrequencyUnit;
begin
  result.Symbol := '1/s';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TFrequencySquare;     const ARight: TFrequency    ): TFrequency;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TFrequencySquareUnit; const ARight: TSpeedSquareUnit): TInvAreaUnit;
begin
  result.Symbol := '1/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TFrequencySquare;     const ARight: TSpeedSquare    ): TInvArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TFrequencySquareUnit; const ARight: TAccelerationUnit): TWaveNumberUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TFrequencySquare;     const ARight: TAcceleration    ): TWaveNumber;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TFrequencySquareUnit; const ARight: TWaveNumberUnit): TAccelerationUnit;
begin
  result.Symbol := 'm/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TFrequencySquare;     const ARight: TWaveNumber    ): TAcceleration;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TFrequencySquareUnit; const ARight: TInvAreaUnit): TSpeedSquareUnit;
begin
  result.Symbol := 'm2/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TFrequencySquare;     const ARight: TInvArea    ): TSpeedSquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMassUnit; const ARight: TWaveNumberUnit): TDensity1DUnit;
begin
  result.Symbol := 'kg/m';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TMass;     const ARight: TWaveNumber    ): TDensity1D;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TWaveNumberUnit; const ARight: TMassUnit): TDensity1DUnit;
begin
  result.Symbol := 'kg/m';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TWaveNumber;     const ARight: TMass    ): TDensity1D;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMassUnit; const ARight: TSpecificVolumeUnit): TVolumeUnit;
begin
  result.Symbol := 'm3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TMass;     const ARight: TSpecificVolume    ): TVolume;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSpecificVolumeUnit; const ARight: TMassUnit): TVolumeUnit;
begin
  result.Symbol := 'm3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TSpecificVolume;     const ARight: TMass    ): TVolume;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMassUnit; const ARight: TInvVolumeUnit): TDensity3DUnit;
begin
  result.Symbol := 'kg/m3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TMass;     const ARight: TInvVolume    ): TDensity3D;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TInvVolumeUnit; const ARight: TMassUnit): TDensity3DUnit;
begin
  result.Symbol := 'kg/m3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TInvVolume;     const ARight: TMass    ): TDensity3D;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMassUnit; const ARight: TInvAreaUnit): TDensity2DUnit;
begin
  result.Symbol := 'kg/m2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TMass;     const ARight: TInvArea    ): TDensity2D;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TInvAreaUnit; const ARight: TMassUnit): TDensity2DUnit;
begin
  result.Symbol := 'kg/m2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TInvArea;     const ARight: TMass    ): TDensity2D;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TMassUnit; const ARight: TLengthUnit): TDensity1DUnit;
begin
  result.Symbol := 'kg/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMass;     const ARight: TLength    ): TDensity1D;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMassUnit; const ARight: TAreaUnit): TDensity2DUnit;
begin
  result.Symbol := 'kg/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMass;     const ARight: TArea    ): TDensity2D;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMassUnit; const ARight: TVolumeUnit): TDensity3DUnit;
begin
  result.Symbol := 'kg/m3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMass;     const ARight: TVolume    ): TDensity3D;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMassUnit; const ARight: TDensity1DUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMass;     const ARight: TDensity1D    ): TLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMassUnit; const ARight: TDensity2DUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMass;     const ARight: TDensity2D    ): TArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMassUnit; const ARight: TDensity3DUnit): TVolumeUnit;
begin
  result.Symbol := 'm3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMass;     const ARight: TDensity3D    ): TVolume;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMassUnit; const ARight: TStiffnessUnit): TTimeSquareUnit;
begin
  result.Symbol := 's2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMass;     const ARight: TStiffness    ): TTimeSquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMassUnit; const ARight: TTimeSquareUnit): TStiffnessUnit;
begin
  result.Symbol := 'kg/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMass;     const ARight: TTimeSquare    ): TStiffness;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TStiffnessUnit; const ARight: TWaveNumberUnit): TPressureUnit;
begin
  result.Symbol := 'kg/ms2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TStiffness;     const ARight: TWaveNumber    ): TPressure;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TWaveNumberUnit; const ARight: TStiffnessUnit): TPressureUnit;
begin
  result.Symbol := 'kg/ms2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TWaveNumber;     const ARight: TStiffness    ): TPressure;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TStiffnessUnit; const ARight: TTimeSquareUnit): TMassUnit;
begin
  result.Symbol := 'kg';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TStiffness;     const ARight: TTimeSquare    ): TMass;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TTimeSquareUnit; const ARight: TStiffnessUnit): TMassUnit;
begin
  result.Symbol := 'kg';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TTimeSquare;     const ARight: TStiffness    ): TMass;
begin
  result.Value := ALeft.Value * ARight.Value;
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

operator /(const ALeft: TStiffnessUnit; const ARight: TForceUnit): TWaveNumberUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TStiffness;     const ARight: TForce    ): TWaveNumber;
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

operator /(const ALeft: TStiffnessUnit; const ARight: TSpeedSquareUnit): TDensity2DUnit;
begin
  result.Symbol := 'kg/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TStiffness;     const ARight: TSpeedSquare    ): TDensity2D;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TStiffnessUnit; const ARight: TAccelerationUnit): TDensity1DUnit;
begin
  result.Symbol := 'kg/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TStiffness;     const ARight: TAcceleration    ): TDensity1D;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TStiffnessUnit; const ARight: TDensity1DUnit): TAccelerationUnit;
begin
  result.Symbol := 'm/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TStiffness;     const ARight: TDensity1D    ): TAcceleration;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TStiffnessUnit; const ARight: TDensity2DUnit): TSpeedSquareUnit;
begin
  result.Symbol := 'm2/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TStiffness;     const ARight: TDensity2D    ): TSpeedSquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TStiffnessUnit; const ARight: TFrequencySquareUnit): TMassUnit;
begin
  result.Symbol := 'kg';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TStiffness;     const ARight: TFrequencySquare    ): TMass;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TStiffnessUnit; const ARight: TMassUnit): TFrequencySquareUnit;
begin
  result.Symbol := '1/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TStiffness;     const ARight: TMass    ): TFrequencySquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TStiffnessUnit; const ARight: TMagneticFluxUnit): TCurrentDensityUnit;
begin
  result.Symbol := 'A/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TStiffness;     const ARight: TMagneticFlux    ): TCurrentDensity;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TStiffnessUnit; const ARight: TWaveNumberUnit): TForceUnit;
begin
  result.Symbol := 'kgm/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TStiffness;     const ARight: TWaveNumber    ): TForce;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TStiffnessUnit; const ARight: TCurrentDensityUnit): TMagneticFluxUnit;
begin
  result.Symbol := 'kgm2/As2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TStiffness;     const ARight: TCurrentDensity    ): TMagneticFlux;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TStiffnessUnit; const ARight: TInvAreaUnit): TWorkUnit;
begin
  result.Symbol := 'kgm2/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TStiffness;     const ARight: TInvArea    ): TWork;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TElettricChargeUnit; const ARight: TEletricPotentialUnit): TWorkUnit;
begin
  result.Symbol := 'kgm2/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TElettricCharge;     const ARight: TEletricPotential    ): TWork;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TEletricPotentialUnit; const ARight: TElettricChargeUnit): TWorkUnit;
begin
  result.Symbol := 'kgm2/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TEletricPotential;     const ARight: TElettricCharge    ): TWork;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TElettricChargeUnit; const ARight: TResistanceUnit): TMagneticFluxUnit;
begin
  result.Symbol := 'kgm2/As2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TElettricCharge;     const ARight: TResistance    ): TMagneticFlux;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TResistanceUnit; const ARight: TElettricChargeUnit): TMagneticFluxUnit;
begin
  result.Symbol := 'kgm2/As2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TResistance;     const ARight: TElettricCharge    ): TMagneticFlux;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TElettricChargeUnit; const ARight: TEletricPotentialUnit): TCapacitanceUnit;
begin
  result.Symbol := 's4A2/kgm2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TElettricCharge;     const ARight: TEletricPotential    ): TCapacitance;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TElettricChargeUnit; const ARight: TCapacitanceUnit): TEletricPotentialUnit;
begin
  result.Symbol := 'kgm2/As3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TElettricCharge;     const ARight: TCapacitance    ): TEletricPotential;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TElettricChargeUnit; const ARight: TEletricalConductanceUnit): TMagneticFluxUnit;
begin
  result.Symbol := 'kgm2/As2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TElettricCharge;     const ARight: TEletricalConductance    ): TMagneticFlux;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TElettricChargeUnit; const ARight: TMagneticFluxUnit): TEletricalConductanceUnit;
begin
  result.Symbol := 's3A2/kgm2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TElettricCharge;     const ARight: TMagneticFlux    ): TEletricalConductance;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TEletricPotentialUnit; const ARight: TCapacitanceUnit): TElettricChargeUnit;
begin
  result.Symbol := 'sA';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TEletricPotential;     const ARight: TCapacitance    ): TElettricCharge;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TCapacitanceUnit; const ARight: TEletricPotentialUnit): TElettricChargeUnit;
begin
  result.Symbol := 'sA';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TCapacitance;     const ARight: TEletricPotential    ): TElettricCharge;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TEletricPotentialUnit; const ARight: TTimeUnit): TMagneticFluxUnit;
begin
  result.Symbol := 'kgm2/As2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TEletricPotential;     const ARight: TTime    ): TMagneticFlux;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TTimeUnit; const ARight: TEletricPotentialUnit): TMagneticFluxUnit;
begin
  result.Symbol := 'kgm2/As2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TTime;     const ARight: TEletricPotential    ): TMagneticFlux;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TEletricPotentialUnit; const ARight: TFrequencyUnit): TMagneticFluxUnit;
begin
  result.Symbol := 'kgm2/As2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TEletricPotential;     const ARight: TFrequency    ): TMagneticFlux;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TEletricPotentialUnit; const ARight: TMagneticFluxUnit): TFrequencyUnit;
begin
  result.Symbol := '1/s';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TEletricPotential;     const ARight: TMagneticFlux    ): TFrequency;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TCapacitanceUnit; const ARight: TResistanceUnit): TTimeUnit;
begin
  result.Symbol := 's';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TCapacitance;     const ARight: TResistance    ): TTime;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TResistanceUnit; const ARight: TCapacitanceUnit): TTimeUnit;
begin
  result.Symbol := 's';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TResistance;     const ARight: TCapacitance    ): TTime;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TCapacitanceUnit; const ARight: TInductanceUnit): TTimeSquareUnit;
begin
  result.Symbol := 's2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TCapacitance;     const ARight: TInductance    ): TTimeSquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TInductanceUnit; const ARight: TCapacitanceUnit): TTimeSquareUnit;
begin
  result.Symbol := 's2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TInductance;     const ARight: TCapacitance    ): TTimeSquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TCapacitanceUnit; const ARight: TEletricalConductanceUnit): TTimeUnit;
begin
  result.Symbol := 's';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TCapacitance;     const ARight: TEletricalConductance    ): TTime;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TCapacitanceUnit; const ARight: TTimeUnit): TEletricalConductanceUnit;
begin
  result.Symbol := 's3A2/kgm2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TCapacitance;     const ARight: TTime    ): TEletricalConductance;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TResistanceUnit; const ARight: TTimeUnit): TInductanceUnit;
begin
  result.Symbol := 'kgm2/s2A2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TResistance;     const ARight: TTime    ): TInductance;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TTimeUnit; const ARight: TResistanceUnit): TInductanceUnit;
begin
  result.Symbol := 'kgm2/s2A2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TTime;     const ARight: TResistance    ): TInductance;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TResistanceUnit; const ARight: TFrequencyUnit): TInductanceUnit;
begin
  result.Symbol := 'kgm2/s2A2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TResistance;     const ARight: TFrequency    ): TInductance;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TResistanceUnit; const ARight: TInductanceUnit): TFrequencyUnit;
begin
  result.Symbol := '1/s';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TResistance;     const ARight: TInductance    ): TFrequency;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TEletricalConductanceUnit; const ARight: TMagneticFluxUnit): TElettricChargeUnit;
begin
  result.Symbol := 'sA';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TEletricalConductance;     const ARight: TMagneticFlux    ): TElettricCharge;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMagneticFluxUnit; const ARight: TEletricalConductanceUnit): TElettricChargeUnit;
begin
  result.Symbol := 'sA';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TMagneticFlux;     const ARight: TEletricalConductance    ): TElettricCharge;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TEletricalConductanceUnit; const ARight: TInductanceUnit): TTimeUnit;
begin
  result.Symbol := 's';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TEletricalConductance;     const ARight: TInductance    ): TTime;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TInductanceUnit; const ARight: TEletricalConductanceUnit): TTimeUnit;
begin
  result.Symbol := 's';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TInductance;     const ARight: TEletricalConductance    ): TTime;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TEletricalConductanceUnit; const ARight: TTimeUnit): TCapacitanceUnit;
begin
  result.Symbol := 's4A2/kgm2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TEletricalConductance;     const ARight: TTime    ): TCapacitance;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TTimeUnit; const ARight: TEletricalConductanceUnit): TCapacitanceUnit;
begin
  result.Symbol := 's4A2/kgm2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TTime;     const ARight: TEletricalConductance    ): TCapacitance;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TEletricalConductanceUnit; const ARight: TFrequencyUnit): TCapacitanceUnit;
begin
  result.Symbol := 's4A2/kgm2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TEletricalConductance;     const ARight: TFrequency    ): TCapacitance;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TEletricalConductanceUnit; const ARight: TCapacitanceUnit): TFrequencyUnit;
begin
  result.Symbol := '1/s';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TEletricalConductance;     const ARight: TCapacitance    ): TFrequency;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMagneticFluxUnit; const ARight: TCurrentDensityUnit): TStiffnessUnit;
begin
  result.Symbol := 'kg/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TMagneticFlux;     const ARight: TCurrentDensity    ): TStiffness;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TCurrentDensityUnit; const ARight: TMagneticFluxUnit): TStiffnessUnit;
begin
  result.Symbol := 'kg/s2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TCurrentDensity;     const ARight: TMagneticFlux    ): TStiffness;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMagneticFluxUnit; const ARight: TInvAreaUnit): TMagneticFluxDensityUnit;
begin
  result.Symbol := 'kg/As2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TMagneticFlux;     const ARight: TInvArea    ): TMagneticFluxDensity;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TInvAreaUnit; const ARight: TMagneticFluxUnit): TMagneticFluxDensityUnit;
begin
  result.Symbol := 'kg/As2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TInvArea;     const ARight: TMagneticFlux    ): TMagneticFluxDensity;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TMagneticFluxUnit; const ARight: TAreaUnit): TMagneticFluxDensityUnit;
begin
  result.Symbol := 'kg/As2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMagneticFlux;     const ARight: TArea    ): TMagneticFluxDensity;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMagneticFluxUnit; const ARight: TElettricChargeUnit): TResistanceUnit;
begin
  result.Symbol := 'kgm2/s3A2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMagneticFlux;     const ARight: TElettricCharge    ): TResistance;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMagneticFluxUnit; const ARight: TEletricPotentialUnit): TTimeUnit;
begin
  result.Symbol := 's';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMagneticFlux;     const ARight: TEletricPotential    ): TTime;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMagneticFluxUnit; const ARight: TResistanceUnit): TElettricChargeUnit;
begin
  result.Symbol := 'sA';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMagneticFlux;     const ARight: TResistance    ): TElettricCharge;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMagneticFluxUnit; const ARight: TMagneticFluxDensityUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMagneticFlux;     const ARight: TMagneticFluxDensity    ): TArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMagneticFluxUnit; const ARight: TTimeUnit): TEletricPotentialUnit;
begin
  result.Symbol := 'kgm2/As3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMagneticFlux;     const ARight: TTime    ): TEletricPotential;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMagneticFluxDensityUnit; const ARight: TMagneticFluxUnit): TInvAreaUnit;
begin
  result.Symbol := '1/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMagneticFluxDensity;     const ARight: TMagneticFlux    ): TInvArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMagneticFluxDensityUnit; const ARight: TInductanceUnit): TCurrentDensityUnit;
begin
  result.Symbol := 'A/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMagneticFluxDensity;     const ARight: TInductance    ): TCurrentDensity;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMagneticFluxDensityUnit; const ARight: TCurrentDensityUnit): TInductanceUnit;
begin
  result.Symbol := 'kgm2/s2A2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMagneticFluxDensity;     const ARight: TCurrentDensity    ): TInductance;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMagneticFluxDensityUnit; const ARight: TInvAreaUnit): TMagneticFluxUnit;
begin
  result.Symbol := 'kgm2/As2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TMagneticFluxDensity;     const ARight: TInvArea    ): TMagneticFlux;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TInductanceUnit; const ARight: TCurrentDensityUnit): TMagneticFluxDensityUnit;
begin
  result.Symbol := 'kg/As2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TInductance;     const ARight: TCurrentDensity    ): TMagneticFluxDensity;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TCurrentDensityUnit; const ARight: TInductanceUnit): TMagneticFluxDensityUnit;
begin
  result.Symbol := 'kg/As2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TCurrentDensity;     const ARight: TInductance    ): TMagneticFluxDensity;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TInductanceUnit; const ARight: TResistanceUnit): TTimeUnit;
begin
  result.Symbol := 's';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TInductance;     const ARight: TResistance    ): TTime;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TInductanceUnit; const ARight: TTimeUnit): TResistanceUnit;
begin
  result.Symbol := 'kgm2/s3A2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TInductance;     const ARight: TTime    ): TResistance;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TLuminousFluxUnit; const ARight: TInvAreaUnit): TIlluminanceUnit;
begin
  result.Symbol := 'cd/m2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TLuminousFlux;     const ARight: TInvArea    ): TIlluminance;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TInvAreaUnit; const ARight: TLuminousFluxUnit): TIlluminanceUnit;
begin
  result.Symbol := 'cd/m2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TInvArea;     const ARight: TLuminousFlux    ): TIlluminance;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TLuminousFluxUnit; const ARight: TAreaUnit): TIlluminanceUnit;
begin
  result.Symbol := 'cd/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TLuminousFlux;     const ARight: TArea    ): TIlluminance;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TLuminousFluxUnit; const ARight: TIlluminanceUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TLuminousFlux;     const ARight: TIlluminance    ): TArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TIlluminanceUnit; const ARight: TLuminousFluxUnit): TInvAreaUnit;
begin
  result.Symbol := '1/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TIlluminance;     const ARight: TLuminousFlux    ): TInvArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TIlluminanceUnit; const ARight: TInvAreaUnit): TLuminousFluxUnit;
begin
  result.Symbol := 'cd';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TIlluminance;     const ARight: TInvArea    ): TLuminousFlux;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TWaveNumberUnit; const ARight: TWaveNumberUnit): TInvAreaUnit;
begin
  result.Symbol := '1/m2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TWaveNumber;     const ARight: TWaveNumber    ): TInvArea;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TWaveNumberUnit; const ARight: TInvAreaUnit): TInvVolumeUnit;
begin
  result.Symbol := '1/m3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TWaveNumber;     const ARight: TInvArea    ): TInvVolume;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TInvAreaUnit; const ARight: TWaveNumberUnit): TInvVolumeUnit;
begin
  result.Symbol := '1/m3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TInvArea;     const ARight: TWaveNumber    ): TInvVolume;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TWaveNumberUnit; const ARight: TLengthUnit): TInvAreaUnit;
begin
  result.Symbol := '1/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TWaveNumber;     const ARight: TLength    ): TInvArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWaveNumberUnit; const ARight: TAreaUnit): TInvVolumeUnit;
begin
  result.Symbol := '1/m3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TWaveNumber;     const ARight: TArea    ): TInvVolume;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWaveNumberUnit; const ARight: TInvVolumeUnit): TAreaUnit;
begin
  result.Symbol := 'm2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TWaveNumber;     const ARight: TInvVolume    ): TArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWaveNumberUnit; const ARight: TInvAreaUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TWaveNumber;     const ARight: TInvArea    ): TLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TTimeUnit; const ARight: TTimeUnit): TTimeSquareUnit;
begin
  result.Symbol := 's2';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TTime;     const ARight: TTime    ): TTimeSquare;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TTimeUnit; const ARight: TTimeSquareUnit): TTimeCubeUnit;
begin
  result.Symbol := 's3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TTime;     const ARight: TTimeSquare    ): TTimeCube;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TTimeSquareUnit; const ARight: TTimeUnit): TTimeCubeUnit;
begin
  result.Symbol := 's3';
  result.Factor := ALeft.Factor * ARight.Factor
end;

operator *(const ALeft: TTimeSquare;     const ARight: TTime    ): TTimeCube;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TTimeUnit; const ARight: TFrequencyUnit): TTimeSquareUnit;
begin
  result.Symbol := 's2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TTime;     const ARight: TFrequency    ): TTimeSquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TTimeUnit; const ARight: TFrequencySquareUnit): TTimeCubeUnit;
begin
  result.Symbol := 's3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TTime;     const ARight: TFrequencySquare    ): TTimeCube;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TTimeUnit; const ARight: TCapacitanceUnit): TResistanceUnit;
begin
  result.Symbol := 'kgm2/s3A2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TTime;     const ARight: TCapacitance    ): TResistance;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TTimeUnit; const ARight: TResistanceUnit): TCapacitanceUnit;
begin
  result.Symbol := 's4A2/kgm2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TTime;     const ARight: TResistance    ): TCapacitance;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TTimeUnit; const ARight: TEletricalConductanceUnit): TInductanceUnit;
begin
  result.Symbol := 'kgm2/s2A2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TTime;     const ARight: TEletricalConductance    ): TInductance;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TTimeUnit; const ARight: TInductanceUnit): TEletricalConductanceUnit;
begin
  result.Symbol := 's3A2/kgm2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TTime;     const ARight: TInductance    ): TEletricalConductance;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TTimeUnit; const ARight: TTimeSquareUnit): TFrequencyUnit;
begin
  result.Symbol := '1/s';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TTime;     const ARight: TTimeSquare    ): TFrequency;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TTimeUnit; const ARight: TTimeCubeUnit): TFrequencySquareUnit;
begin
  result.Symbol := '1/s2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TTime;     const ARight: TTimeCube    ): TFrequencySquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TTimeSquareUnit; const ARight: TFrequencyUnit): TTimeCubeUnit;
begin
  result.Symbol := 's3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TTimeSquare;     const ARight: TFrequency    ): TTimeCube;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TTimeSquareUnit; const ARight: TCapacitanceUnit): TInductanceUnit;
begin
  result.Symbol := 'kgm2/s2A2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TTimeSquare;     const ARight: TCapacitance    ): TInductance;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TTimeSquareUnit; const ARight: TInductanceUnit): TCapacitanceUnit;
begin
  result.Symbol := 's4A2/kgm2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TTimeSquare;     const ARight: TInductance    ): TCapacitance;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TTimeSquareUnit; const ARight: TTimeUnit): TTimeUnit;
begin
  result.Symbol := 's';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TTimeSquare;     const ARight: TTime    ): TTime;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TTimeSquareUnit; const ARight: TTimeCubeUnit): TFrequencyUnit;
begin
  result.Symbol := '1/s';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TTimeSquare;     const ARight: TTimeCube    ): TFrequency;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TTimeCubeUnit; const ARight: TTimeUnit): TTimeSquareUnit;
begin
  result.Symbol := 's2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TTimeCube;     const ARight: TTime    ): TTimeSquare;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TTimeCubeUnit; const ARight: TTimeSquareUnit): TTimeUnit;
begin
  result.Symbol := 's';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TTimeCube;     const ARight: TTimeSquare    ): TTime;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TInvVolumeUnit; const ARight: TWaveNumberUnit): TInvAreaUnit;
begin
  result.Symbol := '1/m2';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TInvVolume;     const ARight: TWaveNumber    ): TInvArea;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TInvVolumeUnit; const ARight: TInvAreaUnit): TWaveNumberUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TInvVolume;     const ARight: TInvArea    ): TWaveNumber;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TInvAreaUnit; const ARight: TLengthUnit): TInvVolumeUnit;
begin
  result.Symbol := '1/m3';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TInvArea;     const ARight: TLength    ): TInvVolume;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TInvAreaUnit; const ARight: TWaveNumberUnit): TWaveNumberUnit;
begin
  result.Symbol := '1/m';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TInvArea;     const ARight: TWaveNumber    ): TWaveNumber;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TInvAreaUnit; const ARight: TInvVolumeUnit): TLengthUnit;
begin
  result.Symbol := 'm';
  result.Factor := ALeft.Factor / ARight.Factor
end;

operator /(const ALeft: TInvArea;     const ARight: TInvVolume    ): TLength;
begin
  result.Value := ALeft.Value / ARight.Value;
end;


end.
