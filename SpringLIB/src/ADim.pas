{
  Description: ADimPas library.

  Copyright (C) 2023 Melchiorre Caruso <melchiorrecaruso@gmail.com>

  This library is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit ADim;

{$H+}{$modeSwitch advancedRecords}
{$WARN NO_RETVAL OFF}

interface

uses SysUtils;

type
  { Prefix }
  TPrefix = (pQuetta, pRonna, pYotta, pZetta, pExa, pPeta, pTera, pGiga, pMega, pKilo, pHecto, pDeca,
    pNone, pDeci, pCenti, pMilli, pMicro, pNano, pPico, pFemto, pAtto, pZepto, pYocto, pRonto, pQuecto);

type
  { Prefixes }
  TPrefixes = array of TPrefix;

  { TQuantity }
  generic TQuantity<U> = record
    type TSelf = specialize TQuantity<U>;
  private
    FValue: double;
  public
    function Abs: TSelf;
    function Value: double;
    function Value(const Prefixes: TPrefixes): double;
    function ToString: string;
    function ToVerboseString: string;
    function ToString(Precision, Digits: longint; const Prefixes: TPrefixes): string;
    function ToVerboseString(Precision, Digits: longint; const Prefixes: TPrefixes): string;
    class operator +  (const ALeft, ARight: TSelf): TSelf;
    class operator -  (const ALeft, ARight: TSelf): TSelf;
    class operator *  (const AValue: double; const ASelf: TSelf): TSelf;
    class operator *  (const ASelf: TSelf; const AValue: double): TSelf;
    class operator /  (const ASelf: TSelf; const AValue: double): TSelf;
    class operator /  (const ALeft, ARight: TSelf): double;
    class operator mod(const ALeft, ARight: TSelf): TSelf;
    class operator =  (const ALeft, ARight: TSelf): boolean;
    class operator <  (const ALeft, ARight: TSelf): boolean;
    class operator >  (const ALeft, ARight: TSelf): boolean;
    class operator <= (const ALeft, ARight: TSelf): boolean;
    class operator >= (const ALeft, ARight: TSelf): boolean;
  end;

  { TUnitId }
  generic TUnitId<U> = record
    type TSelf = specialize TUnitId<U>;
    type TBaseQuantity = specialize TQuantity<U>;
  public
    class function From(const AQuantity: TBaseQuantity): TBaseQuantity; inline; static;
    class operator *(const AValue: double; const {%H-}ASelf: TSelf): TBaseQuantity;
  end;

type
  { Unit of Second }
  TSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSeconds = specialize TQuantity<TSecondUnit>;
  TSecondUnitId = specialize TUnitId<TSecondUnit>;

var s: TSecondUnitId;

const quettas: specialize TQuantity<TSecondUnit> = (FValue: 1E+30);
const  ronnas: specialize TQuantity<TSecondUnit> = (FValue: 1E+27);
const  yottas: specialize TQuantity<TSecondUnit> = (FValue: 1E+24);
const  zettas: specialize TQuantity<TSecondUnit> = (FValue: 1E+21);
const      Es: specialize TQuantity<TSecondUnit> = (FValue: 1E+18);
const   petas: specialize TQuantity<TSecondUnit> = (FValue: 1E+15);
const      Ts: specialize TQuantity<TSecondUnit> = (FValue: 1E+12);
const      Gs: specialize TQuantity<TSecondUnit> = (FValue: 1E+09);
const   megas: specialize TQuantity<TSecondUnit> = (FValue: 1E+06);
const      ks: specialize TQuantity<TSecondUnit> = (FValue: 1E+03);
const      hs: specialize TQuantity<TSecondUnit> = (FValue: 1E+02);
const     das: specialize TQuantity<TSecondUnit> = (FValue: 1E+01);
const      ds: specialize TQuantity<TSecondUnit> = (FValue: 1E-01);
const      cs: specialize TQuantity<TSecondUnit> = (FValue: 1E-02);
const      ms: specialize TQuantity<TSecondUnit> = (FValue: 1E-03);
const     mis: specialize TQuantity<TSecondUnit> = (FValue: 1E-06);
const      ns: specialize TQuantity<TSecondUnit> = (FValue: 1E-09);
const      ps: specialize TQuantity<TSecondUnit> = (FValue: 1E-12);
const      fs: specialize TQuantity<TSecondUnit> = (FValue: 1E-15);
const   attos: specialize TQuantity<TSecondUnit> = (FValue: 1E-18);
const  zeptos: specialize TQuantity<TSecondUnit> = (FValue: 1E-21);
const  yoctos: specialize TQuantity<TSecondUnit> = (FValue: 1E-24);
const  rontos: specialize TQuantity<TSecondUnit> = (FValue: 1E-27);
const quectos: specialize TQuantity<TSecondUnit> = (FValue: 1E-30);

type
  { Unit of Day }
  TDayUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
    const Factor = 86400;
  end;
  TDays = specialize TQuantity<TSecondUnit>;
  TDayUnitId = specialize TUnitId<TDayUnit>;

const day: specialize TQuantity<TSecondUnit> = (FValue: 86400);

type
  { Unit of Hour }
  THourUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
    const Factor = 3600;
  end;
  THours = specialize TQuantity<TSecondUnit>;
  THourUnitId = specialize TUnitId<THourUnit>;

const hr: specialize TQuantity<TSecondUnit> = (FValue: 3600);

type
  { Unit of Minute }
  TMinuteUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
    const Factor = 60;
  end;
  TMinutes = specialize TQuantity<TSecondUnit>;
  TMinuteUnitId = specialize TUnitId<TMinuteUnit>;

const minute: specialize TQuantity<TSecondUnit> = (FValue: 60);

type
  { Unit of SquareSecond }
  TSquareSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSquareSeconds = specialize TQuantity<TSquareSecondUnit>;
  TSquareSecondUnitId = specialize TUnitId<TSquareSecondUnit>;

var s2: TSquareSecondUnitId;

const quettas2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E+60);
const  ronnas2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E+54);
const  yottas2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E+48);
const  zettas2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E+42);
const      Es2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E+36);
const   petas2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E+30);
const      Ts2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E+24);
const      Gs2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E+18);
const   megas2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E+12);
const      ks2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E+06);
const      hs2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E+04);
const     das2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E+02);
const      ds2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E-02);
const      cs2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E-04);
const      ms2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E-06);
const     mis2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E-12);
const      ns2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E-18);
const      ps2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E-24);
const      fs2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E-30);
const      as2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E-36);
const  zeptos2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E-42);
const  yoctos2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E-48);
const  rontos2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E-54);
const quectos2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E-60);

// main definition [ s2 ] = [ s ] * [ s ]
operator *(const ALeft: TSeconds; const ARight: TSeconds): TSquareSeconds; inline;
operator /(const ALeft: TSquareSeconds; const ARight: TSeconds): TSeconds; inline;

type
  { Unit of SquareDay }
  TSquareDayUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
    const Factor = 7464960000;
  end;
  TSquareDays = specialize TQuantity<TSquareSecondUnit>;
  TSquareDayUnitId = specialize TUnitId<TSquareDayUnit>;

const day2: specialize TQuantity<TSquareSecondUnit> = (FValue: 7464960000);

type
  { Unit of SquareHour }
  TSquareHourUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
    const Factor = 12960000;
  end;
  TSquareHours = specialize TQuantity<TSquareSecondUnit>;
  TSquareHourUnitId = specialize TUnitId<TSquareHourUnit>;

const hr2: specialize TQuantity<TSquareSecondUnit> = (FValue: 12960000);

type
  { Unit of SquareMinute }
  TSquareMinuteUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
    const Factor = 3600;
  end;
  TSquareMinutes = specialize TQuantity<TSquareSecondUnit>;
  TSquareMinuteUnitId = specialize TUnitId<TSquareMinuteUnit>;

const minute2: specialize TQuantity<TSquareSecondUnit> = (FValue: 3600);

type
  { Unit of Meter }
  TMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TMeters = specialize TQuantity<TMeterUnit>;
  TMeterUnitId = specialize TUnitId<TMeterUnit>;

var m: TMeterUnitId;

const quettam: specialize TQuantity<TMeterUnit> = (FValue: 1E+30);
const  ronnam: specialize TQuantity<TMeterUnit> = (FValue: 1E+27);
const  yottam: specialize TQuantity<TMeterUnit> = (FValue: 1E+24);
const  zettam: specialize TQuantity<TMeterUnit> = (FValue: 1E+21);
const      Em: specialize TQuantity<TMeterUnit> = (FValue: 1E+18);
const   petam: specialize TQuantity<TMeterUnit> = (FValue: 1E+15);
const      Tm: specialize TQuantity<TMeterUnit> = (FValue: 1E+12);
const      Gm: specialize TQuantity<TMeterUnit> = (FValue: 1E+09);
const   megam: specialize TQuantity<TMeterUnit> = (FValue: 1E+06);
const      km: specialize TQuantity<TMeterUnit> = (FValue: 1E+03);
const      hm: specialize TQuantity<TMeterUnit> = (FValue: 1E+02);
const     dam: specialize TQuantity<TMeterUnit> = (FValue: 1E+01);
const      dm: specialize TQuantity<TMeterUnit> = (FValue: 1E-01);
const      cm: specialize TQuantity<TMeterUnit> = (FValue: 1E-02);
const      mm: specialize TQuantity<TMeterUnit> = (FValue: 1E-03);
const     mim: specialize TQuantity<TMeterUnit> = (FValue: 1E-06);
const      nm: specialize TQuantity<TMeterUnit> = (FValue: 1E-09);
const      pm: specialize TQuantity<TMeterUnit> = (FValue: 1E-12);
const      fm: specialize TQuantity<TMeterUnit> = (FValue: 1E-15);
const      am: specialize TQuantity<TMeterUnit> = (FValue: 1E-18);
const  zeptom: specialize TQuantity<TMeterUnit> = (FValue: 1E-21);
const  yoctom: specialize TQuantity<TMeterUnit> = (FValue: 1E-24);
const  rontom: specialize TQuantity<TMeterUnit> = (FValue: 1E-27);
const quectom: specialize TQuantity<TMeterUnit> = (FValue: 1E-30);

type
  { Unit of Astronomical }
  TAstronomicalUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
    const Factor = 149597870691;
  end;
  TAstronomical = specialize TQuantity<TMeterUnit>;
  TAstronomicalUnitId = specialize TUnitId<TAstronomicalUnit>;

const au: specialize TQuantity<TMeterUnit> = (FValue: 149597870691);

type
  { Unit of SquareMeter }
  TSquareMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSquareMeters = specialize TQuantity<TSquareMeterUnit>;
  TSquareMeterUnitId = specialize TUnitId<TSquareMeterUnit>;

var m2: TSquareMeterUnitId;

const quettam2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E+60);
const  ronnam2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E+54);
const  yottam2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E+48);
const  zettam2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E+42);
const      Em2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E+36);
const   petam2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E+30);
const      Tm2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E+24);
const      Gm2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E+18);
const   megam2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E+12);
const      km2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E+06);
const      hm2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E+04);
const     dam2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E+02);
const      dm2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E-02);
const      cm2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E-04);
const      mm2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E-06);
const     mim2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E-12);
const      nm2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E-18);
const      pm2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E-24);
const      fm2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E-30);
const      am2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E-36);
const  zeptom2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E-42);
const  yoctom2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E-48);
const  rontom2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E-54);
const quectom2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E-60);

// main definition [ m2 ] = [ m ] * [ m ]
operator *(const ALeft: TMeters; const ARight: TMeters): TSquareMeters; inline;
operator /(const ALeft: TSquareMeters; const ARight: TMeters): TMeters; inline;

type
  { Unit of CubicMeter }
  TCubicMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TCubicMeters = specialize TQuantity<TCubicMeterUnit>;
  TCubicMeterUnitId = specialize TUnitId<TCubicMeterUnit>;

var m3: TCubicMeterUnitId;

const quettam3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E+90);
const  ronnam3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E+81);
const  yottam3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E+72);
const  zettam3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E+63);
const      Em3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E+54);
const   petam3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E+45);
const      Tm3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E+36);
const      Gm3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E+27);
const   megam3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E+18);
const      km3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E+09);
const      hm3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E+06);
const     dam3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E+03);
const      dm3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03);
const      cm3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-06);
const      mm3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-09);
const     mim3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-18);
const      nm3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-27);
const      pm3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-36);
const      fm3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-45);
const      am3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-54);
const  zeptom3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-63);
const  yoctom3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-72);
const  rontom3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-81);
const quectom3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-90);

// main definition [ m3 ] = [ m2 ] * [ m ]
operator *(const ALeft: TSquareMeters; const ARight: TMeters): TCubicMeters; inline;
operator *(const ALeft: TMeters; const ARight: TSquareMeters): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TSquareMeters): TMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TMeters): TSquareMeters; inline;

type
  { Unit of Litre }
  TLitreUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
    const Factor = 1E-03;
  end;
  TLitres = specialize TQuantity<TCubicMeterUnit>;
  TLitreUnitId = specialize TUnitId<TLitreUnit>;

const L: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03);

const quettaL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E+30);
const  ronnaL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E+27);
const  yottaL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E+24);
const  zettaL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E+21);
const      EL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E+18);
const   petaL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E+15);
const      TL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E+12);
const      GL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E+09);
const   megaL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E+06);
const      kL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E+03);
const      hL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E+02);
const     daL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E+01);
const      dL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E-01);
const      cL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E-02);
const      mL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E-03);
const     miL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E-06);
const      nL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E-09);
const   picoL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E-12);
const      fL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E-15);
const      aL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E-18);
const  zeptoL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E-21);
const  yoctoL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E-24);
const  rontoL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E-27);
const quectoL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E-30);

type
  { Unit of QuarticMeter }
  TQuarticMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TQuarticMeters = specialize TQuantity<TQuarticMeterUnit>;
  TQuarticMeterUnitId = specialize TUnitId<TQuarticMeterUnit>;

var m4: TQuarticMeterUnitId;

const quettam4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E+120);
const  ronnam4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E+108);
const  yottam4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E+96);
const  zettam4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E+84);
const      Em4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E+72);
const   petam4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E+60);
const      Tm4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E+48);
const      Gm4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E+36);
const   megam4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E+24);
const      km4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E+12);
const      hm4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E+08);
const     dam4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E+04);
const      dm4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E-04);
const      cm4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E-08);
const      mm4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E-12);
const     mim4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E-24);
const      nm4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E-36);
const      pm4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E-48);
const      fm4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E-60);
const      am4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E-72);
const  zeptom4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E-84);
const  yoctom4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E-96);
const  rontom4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E-108);
const quectom4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E-120);

// main definition [ m4 ] = [ m3 ] * [ m ]
operator *(const ALeft: TCubicMeters; const ARight: TMeters): TQuarticMeters; inline;
operator *(const ALeft: TMeters; const ARight: TCubicMeters): TQuarticMeters; inline;
operator /(const ALeft: TQuarticMeters; const ARight: TCubicMeters): TMeters; inline;
operator /(const ALeft: TQuarticMeters; const ARight: TMeters): TCubicMeters; inline;

// alternative definition [ m4 ] = [ m2 ] * [ m2 ]
operator *(const ALeft: TSquareMeters; const ARight: TSquareMeters): TQuarticMeters; inline;
operator /(const ALeft: TQuarticMeters; const ARight: TSquareMeters): TSquareMeters; inline;

type
  { Unit of QuinticMeter }
  TQuinticMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TQuinticMeters = specialize TQuantity<TQuinticMeterUnit>;
  TQuinticMeterUnitId = specialize TUnitId<TQuinticMeterUnit>;

var m5: TQuinticMeterUnitId;

const quettam5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E+150);
const  ronnam5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E+135);
const  yottam5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E+120);
const  zettam5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E+105);
const      Em5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E+90);
const   petam5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E+75);
const      Tm5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E+60);
const      Gm5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E+45);
const   megam5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E+30);
const      km5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E+15);
const      hm5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E+10);
const     dam5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E+05);
const      dm5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E-05);
const      cm5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E-10);
const      mm5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E-15);
const     mim5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E-30);
const      nm5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E-45);
const      pm5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E-60);
const      fm5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E-75);
const      am5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E-90);
const  zeptom5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E-105);
const  yoctom5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E-120);
const  rontom5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E-135);
const quectom5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E-150);

// main definition [ m5 ] = [ m4 ] * [ m ]
operator *(const ALeft: TQuarticMeters; const ARight: TMeters): TQuinticMeters; inline;
operator *(const ALeft: TMeters; const ARight: TQuarticMeters): TQuinticMeters; inline;
operator /(const ALeft: TQuinticMeters; const ARight: TQuarticMeters): TMeters; inline;
operator /(const ALeft: TQuinticMeters; const ARight: TMeters): TQuarticMeters; inline;

// alternative definition [ m5 ] = [ m3 ] * [ m2 ]
operator *(const ALeft: TCubicMeters; const ARight: TSquareMeters): TQuinticMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TCubicMeters): TQuinticMeters; inline;
operator /(const ALeft: TQuinticMeters; const ARight: TCubicMeters): TSquareMeters; inline;
operator /(const ALeft: TQuinticMeters; const ARight: TSquareMeters): TCubicMeters; inline;

type
  { Unit of SexticMeter }
  TSexticMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSexticMeters = specialize TQuantity<TSexticMeterUnit>;
  TSexticMeterUnitId = specialize TUnitId<TSexticMeterUnit>;

var m6: TSexticMeterUnitId;

const quettam6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E+180);
const  ronnam6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E+162);
const  yottam6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E+144);
const  zettam6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E+126);
const      Em6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E+108);
const   petam6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E+90);
const      Tm6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E+72);
const      Gm6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E+54);
const   megam6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E+36);
const      km6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E+18);
const      hm6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E+12);
const     dam6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E+06);
const      dm6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E-06);
const      cm6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E-12);
const      mm6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E-18);
const     mim6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E-36);
const      nm6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E-54);
const      pm6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E-72);
const      fm6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E-90);
const      am6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E-108);
const  zeptom6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E-126);
const  yoctom6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E-144);
const  rontom6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E-162);
const quectom6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E-180);

// main definition [ m6 ] = [ m5 ] * [ m ]
operator *(const ALeft: TQuinticMeters; const ARight: TMeters): TSexticMeters; inline;
operator *(const ALeft: TMeters; const ARight: TQuinticMeters): TSexticMeters; inline;
operator /(const ALeft: TSexticMeters; const ARight: TQuinticMeters): TMeters; inline;
operator /(const ALeft: TSexticMeters; const ARight: TMeters): TQuinticMeters; inline;

// alternative definition [ m6 ] = [ m4 ] * [ m2 ]
operator *(const ALeft: TQuarticMeters; const ARight: TSquareMeters): TSexticMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TQuarticMeters): TSexticMeters; inline;
operator /(const ALeft: TSexticMeters; const ARight: TQuarticMeters): TSquareMeters; inline;
operator /(const ALeft: TSexticMeters; const ARight: TSquareMeters): TQuarticMeters; inline;

// alternative definition [ m6 ] = [ m3 ] * [ m3 ]
operator *(const ALeft: TCubicMeters; const ARight: TCubicMeters): TSexticMeters; inline;
operator /(const ALeft: TSexticMeters; const ARight: TCubicMeters): TCubicMeters; inline;

type
  { Unit of Kilogram }
  TKilogramUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TKilograms = specialize TQuantity<TKilogramUnit>;
  TKilogramUnitId = specialize TUnitId<TKilogramUnit>;

var kg: TKilogramUnitId;

const  hg: specialize TQuantity<TKilogramUnit> = (FValue: 1E-01);
const dag: specialize TQuantity<TKilogramUnit> = (FValue: 1E-02);
const   g: specialize TQuantity<TKilogramUnit> = (FValue: 1E-03);
const  dg: specialize TQuantity<TKilogramUnit> = (FValue: 1E-04);
const  cg: specialize TQuantity<TKilogramUnit> = (FValue: 1E-05);
const  mg: specialize TQuantity<TKilogramUnit> = (FValue: 1E-06);
const mig: specialize TQuantity<TKilogramUnit> = (FValue: 1E-09);
const  ng: specialize TQuantity<TKilogramUnit> = (FValue: 1E-12);
const  pg: specialize TQuantity<TKilogramUnit> = (FValue: 1E-15);

type
  { Unit of Tonne }
  TTonneUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
    const Factor = 1E+03;
  end;
  TTonnes = specialize TQuantity<TKilogramUnit>;
  TTonneUnitId = specialize TUnitId<TTonneUnit>;

const tonne: specialize TQuantity<TKilogramUnit> = (FValue: 1E+03);

const   gigatonne: specialize TQuantity<TKilogramUnit> = (FValue: 1E+03 * 1E+09);
const   megatonne: specialize TQuantity<TKilogramUnit> = (FValue: 1E+03 * 1E+06);
const   kilotonne: specialize TQuantity<TKilogramUnit> = (FValue: 1E+03 * 1E+03);

type
  { Unit of SquareKilogram }
  TSquareKilogramUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSquareKilograms = specialize TQuantity<TSquareKilogramUnit>;
  TSquareKilogramUnitId = specialize TUnitId<TSquareKilogramUnit>;

var kg2: TSquareKilogramUnitId;

const  hg2: specialize TQuantity<TSquareKilogramUnit> = (FValue: 1E-02);
const dag2: specialize TQuantity<TSquareKilogramUnit> = (FValue: 1E-04);
const   g2: specialize TQuantity<TSquareKilogramUnit> = (FValue: 1E-06);
const  dg2: specialize TQuantity<TSquareKilogramUnit> = (FValue: 1E-08);
const  cg2: specialize TQuantity<TSquareKilogramUnit> = (FValue: 1E-10);
const  mg2: specialize TQuantity<TSquareKilogramUnit> = (FValue: 1E-12);
const mig2: specialize TQuantity<TSquareKilogramUnit> = (FValue: 1E-18);
const  ng2: specialize TQuantity<TSquareKilogramUnit> = (FValue: 1E-24);
const  pg2: specialize TQuantity<TSquareKilogramUnit> = (FValue: 1E-30);

// main definition [ kg2 ] = [ kg ] * [ kg ]
operator *(const ALeft: TKilograms; const ARight: TKilograms): TSquareKilograms; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TKilograms): TKilograms; inline;

type
  { Unit of Ampere }
  TAmpereUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TAmperes = specialize TQuantity<TAmpereUnit>;
  TAmpereUnitId = specialize TUnitId<TAmpereUnit>;

var A: TAmpereUnitId;

const quettaA: specialize TQuantity<TAmpereUnit> = (FValue: 1E+30);
const  ronnaA: specialize TQuantity<TAmpereUnit> = (FValue: 1E+27);
const  yottaA: specialize TQuantity<TAmpereUnit> = (FValue: 1E+24);
const  zettaA: specialize TQuantity<TAmpereUnit> = (FValue: 1E+21);
const      EA: specialize TQuantity<TAmpereUnit> = (FValue: 1E+18);
const   petaA: specialize TQuantity<TAmpereUnit> = (FValue: 1E+15);
const      TA: specialize TQuantity<TAmpereUnit> = (FValue: 1E+12);
const      GA: specialize TQuantity<TAmpereUnit> = (FValue: 1E+09);
const   megaA: specialize TQuantity<TAmpereUnit> = (FValue: 1E+06);
const      kA: specialize TQuantity<TAmpereUnit> = (FValue: 1E+03);
const      hA: specialize TQuantity<TAmpereUnit> = (FValue: 1E+02);
const     daA: specialize TQuantity<TAmpereUnit> = (FValue: 1E+01);
const      dA: specialize TQuantity<TAmpereUnit> = (FValue: 1E-01);
const      cA: specialize TQuantity<TAmpereUnit> = (FValue: 1E-02);
const      mA: specialize TQuantity<TAmpereUnit> = (FValue: 1E-03);
const     miA: specialize TQuantity<TAmpereUnit> = (FValue: 1E-06);
const      nA: specialize TQuantity<TAmpereUnit> = (FValue: 1E-09);
const   picoA: specialize TQuantity<TAmpereUnit> = (FValue: 1E-12);
const      fA: specialize TQuantity<TAmpereUnit> = (FValue: 1E-15);
const      aA: specialize TQuantity<TAmpereUnit> = (FValue: 1E-18);
const  zeptoA: specialize TQuantity<TAmpereUnit> = (FValue: 1E-21);
const  yoctoA: specialize TQuantity<TAmpereUnit> = (FValue: 1E-24);
const  rontoA: specialize TQuantity<TAmpereUnit> = (FValue: 1E-27);
const quectoA: specialize TQuantity<TAmpereUnit> = (FValue: 1E-30);

type
  { Unit of SquareAmpere }
  TSquareAmpereUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSquareAmperes = specialize TQuantity<TSquareAmpereUnit>;
  TSquareAmpereUnitId = specialize TUnitId<TSquareAmpereUnit>;

var A2: TSquareAmpereUnitId;

const quettaA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E+60);
const  ronnaA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E+54);
const  yottaA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E+48);
const  zettaA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E+42);
const      EA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E+36);
const   petaA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E+30);
const      TA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E+24);
const      GA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E+18);
const   megaA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E+12);
const      kA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E+06);
const      hA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E+04);
const     daA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E+02);
const      dA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E-02);
const      cA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E-04);
const      mA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E-06);
const     miA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E-12);
const      nA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E-18);
const   picoA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E-24);
const      fA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E-30);
const      aA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E-36);
const  zeptoA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E-42);
const  yoctoA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E-48);
const  rontoA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E-54);
const quectoA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E-60);

// main definition [ A2 ] = [ A ] * [ A ]
operator *(const ALeft: TAmperes; const ARight: TAmperes): TSquareAmperes; inline;
operator /(const ALeft: TSquareAmperes; const ARight: TAmperes): TAmperes; inline;

type
  { Unit of Kelvin }
  TKelvinUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TKelvins = specialize TQuantity<TKelvinUnit>;
  TKelvinUnitId = specialize TUnitId<TKelvinUnit>;

var K: TKelvinUnitId;

const quettaK: specialize TQuantity<TKelvinUnit> = (FValue: 1E+30);
const  ronnaK: specialize TQuantity<TKelvinUnit> = (FValue: 1E+27);
const  yottaK: specialize TQuantity<TKelvinUnit> = (FValue: 1E+24);
const  zettaK: specialize TQuantity<TKelvinUnit> = (FValue: 1E+21);
const      EK: specialize TQuantity<TKelvinUnit> = (FValue: 1E+18);
const   petaK: specialize TQuantity<TKelvinUnit> = (FValue: 1E+15);
const      TK: specialize TQuantity<TKelvinUnit> = (FValue: 1E+12);
const      GK: specialize TQuantity<TKelvinUnit> = (FValue: 1E+09);
const   megaK: specialize TQuantity<TKelvinUnit> = (FValue: 1E+06);
const      kK: specialize TQuantity<TKelvinUnit> = (FValue: 1E+03);
const      hK: specialize TQuantity<TKelvinUnit> = (FValue: 1E+02);
const     daK: specialize TQuantity<TKelvinUnit> = (FValue: 1E+01);
const      dK: specialize TQuantity<TKelvinUnit> = (FValue: 1E-01);
const      cK: specialize TQuantity<TKelvinUnit> = (FValue: 1E-02);
const      mK: specialize TQuantity<TKelvinUnit> = (FValue: 1E-03);
const     miK: specialize TQuantity<TKelvinUnit> = (FValue: 1E-06);
const      nK: specialize TQuantity<TKelvinUnit> = (FValue: 1E-09);
const      pK: specialize TQuantity<TKelvinUnit> = (FValue: 1E-12);
const      fK: specialize TQuantity<TKelvinUnit> = (FValue: 1E-15);
const      aK: specialize TQuantity<TKelvinUnit> = (FValue: 1E-18);
const  zeptoK: specialize TQuantity<TKelvinUnit> = (FValue: 1E-21);
const  yoctoK: specialize TQuantity<TKelvinUnit> = (FValue: 1E-24);
const  rontoK: specialize TQuantity<TKelvinUnit> = (FValue: 1E-27);
const quectoK: specialize TQuantity<TKelvinUnit> = (FValue: 1E-30);

type
  { Unit of DegreeCelsius }
  TDegreeCelsiusUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TDegreesCelsius = specialize TQuantity<TDegreeCelsiusUnit>;
  TDegreeCelsiusUnitId = specialize TUnitId<TDegreeCelsiusUnit>;

var degC: TDegreeCelsiusUnitId;

type
  { Unit of DegreeFahrenheit }
  TDegreeFahrenheitUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TDegreesFahrenheit = specialize TQuantity<TDegreeFahrenheitUnit>;
  TDegreeFahrenheitUnitId = specialize TUnitId<TDegreeFahrenheitUnit>;

var degF: TDegreeFahrenheitUnitId;

type
  { Unit of SquareKelvin }
  TSquareKelvinUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSquareKelvins = specialize TQuantity<TSquareKelvinUnit>;
  TSquareKelvinUnitId = specialize TUnitId<TSquareKelvinUnit>;

var K2: TSquareKelvinUnitId;

const quettaK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E+60);
const  ronnaK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E+54);
const  yottaK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E+48);
const  zettaK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E+42);
const      EK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E+36);
const   petaK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E+30);
const      TK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E+24);
const      GK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E+18);
const   megaK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E+12);
const      kK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E+06);
const      hK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E+04);
const     daK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E+02);
const      dK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E-02);
const      cK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E-04);
const      mK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E-06);
const     miK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E-12);
const      nK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E-18);
const      pK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E-24);
const      fK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E-30);
const      aK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E-36);
const  zeptoK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E-42);
const  yoctoK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E-48);
const  rontoK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E-54);
const quectoK2: specialize TQuantity<TSquareKelvinUnit> = (FValue: 1E-60);

// main definition [ K2 ] = [ K ] * [ K ]
operator *(const ALeft: TKelvins; const ARight: TKelvins): TSquareKelvins; inline;
operator /(const ALeft: TSquareKelvins; const ARight: TKelvins): TKelvins; inline;

type
  { Unit of CubicKelvin }
  TCubicKelvinUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TCubicKelvins = specialize TQuantity<TCubicKelvinUnit>;
  TCubicKelvinUnitId = specialize TUnitId<TCubicKelvinUnit>;

var K3: TCubicKelvinUnitId;

const quettaK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E+90);
const  ronnaK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E+81);
const  yottaK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E+72);
const  zettaK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E+63);
const      EK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E+54);
const   petaK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E+45);
const      TK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E+36);
const      GK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E+27);
const   megaK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E+18);
const      kK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E+09);
const      hK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E+06);
const     daK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E+03);
const      dK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E-03);
const      cK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E-06);
const      mK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E-09);
const     miK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E-18);
const      nK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E-27);
const      pK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E-36);
const      fK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E-45);
const      aK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E-54);
const  zeptoK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E-63);
const  yoctoK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E-72);
const  rontoK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E-81);
const quectoK3: specialize TQuantity<TCubicKelvinUnit> = (FValue: 1E-90);

// main definition [ K3 ] = [ K2 ] * [ K ]
operator *(const ALeft: TSquareKelvins; const ARight: TKelvins): TCubicKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TSquareKelvins): TCubicKelvins; inline;
operator /(const ALeft: TCubicKelvins; const ARight: TSquareKelvins): TKelvins; inline;
operator /(const ALeft: TCubicKelvins; const ARight: TKelvins): TSquareKelvins; inline;

type
  { Unit of QuarticKelvin }
  TQuarticKelvinUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TQuarticKelvins = specialize TQuantity<TQuarticKelvinUnit>;
  TQuarticKelvinUnitId = specialize TUnitId<TQuarticKelvinUnit>;

var K4: TQuarticKelvinUnitId;

const quettaK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E+120);
const  ronnaK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E+108);
const  yottaK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E+96);
const  zettaK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E+84);
const      EK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E+72);
const   petaK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E+60);
const      TK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E+48);
const      GK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E+36);
const   megaK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E+24);
const      kK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E+12);
const      hK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E+08);
const     daK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E+04);
const      dK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E-04);
const      cK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E-08);
const      mK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E-12);
const     miK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E-24);
const      nK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E-36);
const      pK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E-48);
const      fK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E-60);
const      aK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E-72);
const  zeptoK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E-84);
const  yoctoK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E-96);
const  rontoK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E-108);
const quectoK4: specialize TQuantity<TQuarticKelvinUnit> = (FValue: 1E-120);

// main definition [ K4 ] = [ K3 ] * [ K ]
operator *(const ALeft: TCubicKelvins; const ARight: TKelvins): TQuarticKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TCubicKelvins): TQuarticKelvins; inline;
operator /(const ALeft: TQuarticKelvins; const ARight: TCubicKelvins): TKelvins; inline;
operator /(const ALeft: TQuarticKelvins; const ARight: TKelvins): TCubicKelvins; inline;

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]
operator *(const ALeft: TSquareKelvins; const ARight: TSquareKelvins): TQuarticKelvins; inline;
operator /(const ALeft: TQuarticKelvins; const ARight: TSquareKelvins): TSquareKelvins; inline;

type
  { Unit of Mole }
  TMoleUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TMoles = specialize TQuantity<TMoleUnit>;
  TMoleUnitId = specialize TUnitId<TMoleUnit>;

var mol: TMoleUnitId;

const quettamol: specialize TQuantity<TMoleUnit> = (FValue: 1E+30);
const  ronnamol: specialize TQuantity<TMoleUnit> = (FValue: 1E+27);
const  yottamol: specialize TQuantity<TMoleUnit> = (FValue: 1E+24);
const  zettamol: specialize TQuantity<TMoleUnit> = (FValue: 1E+21);
const      Emol: specialize TQuantity<TMoleUnit> = (FValue: 1E+18);
const   petamol: specialize TQuantity<TMoleUnit> = (FValue: 1E+15);
const      Tmol: specialize TQuantity<TMoleUnit> = (FValue: 1E+12);
const      Gmol: specialize TQuantity<TMoleUnit> = (FValue: 1E+09);
const   megamol: specialize TQuantity<TMoleUnit> = (FValue: 1E+06);
const      kmol: specialize TQuantity<TMoleUnit> = (FValue: 1E+03);
const      hmol: specialize TQuantity<TMoleUnit> = (FValue: 1E+02);
const     damol: specialize TQuantity<TMoleUnit> = (FValue: 1E+01);
const      dmol: specialize TQuantity<TMoleUnit> = (FValue: 1E-01);
const      cmol: specialize TQuantity<TMoleUnit> = (FValue: 1E-02);
const      mmol: specialize TQuantity<TMoleUnit> = (FValue: 1E-03);
const     mimol: specialize TQuantity<TMoleUnit> = (FValue: 1E-06);
const      nmol: specialize TQuantity<TMoleUnit> = (FValue: 1E-09);
const      pmol: specialize TQuantity<TMoleUnit> = (FValue: 1E-12);
const      fmol: specialize TQuantity<TMoleUnit> = (FValue: 1E-15);
const      amol: specialize TQuantity<TMoleUnit> = (FValue: 1E-18);
const  zeptomol: specialize TQuantity<TMoleUnit> = (FValue: 1E-21);
const  yoctomol: specialize TQuantity<TMoleUnit> = (FValue: 1E-24);
const  rontomol: specialize TQuantity<TMoleUnit> = (FValue: 1E-27);
const quectomol: specialize TQuantity<TMoleUnit> = (FValue: 1E-30);

type
  { Unit of Candela }
  TCandelaUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TCandelas = specialize TQuantity<TCandelaUnit>;
  TCandelaUnitId = specialize TUnitId<TCandelaUnit>;

var cd: TCandelaUnitId;

const quettacd: specialize TQuantity<TCandelaUnit> = (FValue: 1E+30);
const  ronnacd: specialize TQuantity<TCandelaUnit> = (FValue: 1E+27);
const  yottacd: specialize TQuantity<TCandelaUnit> = (FValue: 1E+24);
const  zettacd: specialize TQuantity<TCandelaUnit> = (FValue: 1E+21);
const      Ecd: specialize TQuantity<TCandelaUnit> = (FValue: 1E+18);
const   petacd: specialize TQuantity<TCandelaUnit> = (FValue: 1E+15);
const      Tcd: specialize TQuantity<TCandelaUnit> = (FValue: 1E+12);
const      Gcd: specialize TQuantity<TCandelaUnit> = (FValue: 1E+09);
const   megacd: specialize TQuantity<TCandelaUnit> = (FValue: 1E+06);
const      kcd: specialize TQuantity<TCandelaUnit> = (FValue: 1E+03);
const      hcd: specialize TQuantity<TCandelaUnit> = (FValue: 1E+02);
const     dacd: specialize TQuantity<TCandelaUnit> = (FValue: 1E+01);
const      dcd: specialize TQuantity<TCandelaUnit> = (FValue: 1E-01);
const      ccd: specialize TQuantity<TCandelaUnit> = (FValue: 1E-02);
const      mcd: specialize TQuantity<TCandelaUnit> = (FValue: 1E-03);
const     micd: specialize TQuantity<TCandelaUnit> = (FValue: 1E-06);
const      ncd: specialize TQuantity<TCandelaUnit> = (FValue: 1E-09);
const      pcd: specialize TQuantity<TCandelaUnit> = (FValue: 1E-12);
const      fcd: specialize TQuantity<TCandelaUnit> = (FValue: 1E-15);
const      acd: specialize TQuantity<TCandelaUnit> = (FValue: 1E-18);
const  zeptocd: specialize TQuantity<TCandelaUnit> = (FValue: 1E-21);
const  yoctocd: specialize TQuantity<TCandelaUnit> = (FValue: 1E-24);
const  rontocd: specialize TQuantity<TCandelaUnit> = (FValue: 1E-27);
const quectocd: specialize TQuantity<TCandelaUnit> = (FValue: 1E-30);

type
  { Unit of Radian }
  TRadianUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TRadians = specialize TQuantity<TRadianUnit>;
  TRadianUnitId = specialize TUnitId<TRadianUnit>;

var rad: TRadianUnitId;

const quettarad: specialize TQuantity<TRadianUnit> = (FValue: 1E+30);
const  ronnarad: specialize TQuantity<TRadianUnit> = (FValue: 1E+27);
const  yottarad: specialize TQuantity<TRadianUnit> = (FValue: 1E+24);
const  zettarad: specialize TQuantity<TRadianUnit> = (FValue: 1E+21);
const      Erad: specialize TQuantity<TRadianUnit> = (FValue: 1E+18);
const   petarad: specialize TQuantity<TRadianUnit> = (FValue: 1E+15);
const      Trad: specialize TQuantity<TRadianUnit> = (FValue: 1E+12);
const      Grad: specialize TQuantity<TRadianUnit> = (FValue: 1E+09);
const   megarad: specialize TQuantity<TRadianUnit> = (FValue: 1E+06);
const      krad: specialize TQuantity<TRadianUnit> = (FValue: 1E+03);
const      hrad: specialize TQuantity<TRadianUnit> = (FValue: 1E+02);
const     darad: specialize TQuantity<TRadianUnit> = (FValue: 1E+01);
const      drad: specialize TQuantity<TRadianUnit> = (FValue: 1E-01);
const      crad: specialize TQuantity<TRadianUnit> = (FValue: 1E-02);
const      mrad: specialize TQuantity<TRadianUnit> = (FValue: 1E-03);
const     mirad: specialize TQuantity<TRadianUnit> = (FValue: 1E-06);
const      nrad: specialize TQuantity<TRadianUnit> = (FValue: 1E-09);
const      prad: specialize TQuantity<TRadianUnit> = (FValue: 1E-12);
const      frad: specialize TQuantity<TRadianUnit> = (FValue: 1E-15);
const      arad: specialize TQuantity<TRadianUnit> = (FValue: 1E-18);
const  zeptorad: specialize TQuantity<TRadianUnit> = (FValue: 1E-21);
const  yoctorad: specialize TQuantity<TRadianUnit> = (FValue: 1E-24);
const  rontorad: specialize TQuantity<TRadianUnit> = (FValue: 1E-27);
const quectorad: specialize TQuantity<TRadianUnit> = (FValue: 1E-30);

type
  { Unit of Degree }
  TDegreeUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
    const Factor = Pi/180;
  end;
  TDegrees = specialize TQuantity<TRadianUnit>;
  TDegreeUnitId = specialize TUnitId<TDegreeUnit>;

const deg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180);

const quettadeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E+30);
const  ronnadeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E+27);
const  yottadeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E+24);
const  zettadeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E+21);
const      Edeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E+18);
const   petadeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E+15);
const      Tdeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E+12);
const      Gdeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E+09);
const   megadeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E+06);
const      kdeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E+03);
const      hdeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E+02);
const     dadeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E+01);
const      ddeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E-01);
const      cdeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E-02);
const      mdeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E-03);
const     mideg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E-06);
const      ndeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E-09);
const      pdeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E-12);
const      fdeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E-15);
const      adeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E-18);
const  zeptodeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E-21);
const  yoctodeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E-24);
const  rontodeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E-27);
const quectodeg: specialize TQuantity<TRadianUnit> = (FValue: Pi/180 * 1E-30);

type
  { Unit of Steradian }
  TSteradianUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSteradians = specialize TQuantity<TSteradianUnit>;
  TSteradianUnitId = specialize TUnitId<TSteradianUnit>;

var sr: TSteradianUnitId;

const quettasr: specialize TQuantity<TSteradianUnit> = (FValue: 1E+30);
const  ronnasr: specialize TQuantity<TSteradianUnit> = (FValue: 1E+27);
const  yottasr: specialize TQuantity<TSteradianUnit> = (FValue: 1E+24);
const  zettasr: specialize TQuantity<TSteradianUnit> = (FValue: 1E+21);
const      Esr: specialize TQuantity<TSteradianUnit> = (FValue: 1E+18);
const   petasr: specialize TQuantity<TSteradianUnit> = (FValue: 1E+15);
const      Tsr: specialize TQuantity<TSteradianUnit> = (FValue: 1E+12);
const      Gsr: specialize TQuantity<TSteradianUnit> = (FValue: 1E+09);
const   megasr: specialize TQuantity<TSteradianUnit> = (FValue: 1E+06);
const      ksr: specialize TQuantity<TSteradianUnit> = (FValue: 1E+03);
const      hsr: specialize TQuantity<TSteradianUnit> = (FValue: 1E+02);
const     dasr: specialize TQuantity<TSteradianUnit> = (FValue: 1E+01);
const      dsr: specialize TQuantity<TSteradianUnit> = (FValue: 1E-01);
const      csr: specialize TQuantity<TSteradianUnit> = (FValue: 1E-02);
const      msr: specialize TQuantity<TSteradianUnit> = (FValue: 1E-03);
const     misr: specialize TQuantity<TSteradianUnit> = (FValue: 1E-06);
const      nsr: specialize TQuantity<TSteradianUnit> = (FValue: 1E-09);
const      psr: specialize TQuantity<TSteradianUnit> = (FValue: 1E-12);
const      fsr: specialize TQuantity<TSteradianUnit> = (FValue: 1E-15);
const      asr: specialize TQuantity<TSteradianUnit> = (FValue: 1E-18);
const  zeptosr: specialize TQuantity<TSteradianUnit> = (FValue: 1E-21);
const  yoctosr: specialize TQuantity<TSteradianUnit> = (FValue: 1E-24);
const  rontosr: specialize TQuantity<TSteradianUnit> = (FValue: 1E-27);
const quectosr: specialize TQuantity<TSteradianUnit> = (FValue: 1E-30);

// main definition [ sr ] = [ rad ] * [ rad ]
operator *(const ALeft: TRadians; const ARight: TRadians): TSteradians; inline;
operator /(const ALeft: TSteradians; const ARight: TRadians): TRadians; inline;

type
  { Unit of Hertz }
  THertzUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  THertz = specialize TQuantity<THertzUnit>;
  THertzUnitId = specialize TUnitId<THertzUnit>;

var Hz: THertzUnitId;

const quettaHz: specialize TQuantity<THertzUnit> = (FValue: 1E+30);
const  ronnaHz: specialize TQuantity<THertzUnit> = (FValue: 1E+27);
const  yottaHz: specialize TQuantity<THertzUnit> = (FValue: 1E+24);
const  zettaHz: specialize TQuantity<THertzUnit> = (FValue: 1E+21);
const      EHz: specialize TQuantity<THertzUnit> = (FValue: 1E+18);
const   petaHz: specialize TQuantity<THertzUnit> = (FValue: 1E+15);
const      THz: specialize TQuantity<THertzUnit> = (FValue: 1E+12);
const      GHz: specialize TQuantity<THertzUnit> = (FValue: 1E+09);
const   megaHz: specialize TQuantity<THertzUnit> = (FValue: 1E+06);
const      kHz: specialize TQuantity<THertzUnit> = (FValue: 1E+03);
const      hHz: specialize TQuantity<THertzUnit> = (FValue: 1E+02);
const     daHz: specialize TQuantity<THertzUnit> = (FValue: 1E+01);
const      dHz: specialize TQuantity<THertzUnit> = (FValue: 1E-01);
const      cHz: specialize TQuantity<THertzUnit> = (FValue: 1E-02);
const      mHz: specialize TQuantity<THertzUnit> = (FValue: 1E-03);
const     miHz: specialize TQuantity<THertzUnit> = (FValue: 1E-06);
const      nHz: specialize TQuantity<THertzUnit> = (FValue: 1E-09);
const      pHz: specialize TQuantity<THertzUnit> = (FValue: 1E-12);
const      fHz: specialize TQuantity<THertzUnit> = (FValue: 1E-15);
const      aHz: specialize TQuantity<THertzUnit> = (FValue: 1E-18);
const  zeptoHz: specialize TQuantity<THertzUnit> = (FValue: 1E-21);
const  yoctoHz: specialize TQuantity<THertzUnit> = (FValue: 1E-24);
const  rontoHz: specialize TQuantity<THertzUnit> = (FValue: 1E-27);
const quectoHz: specialize TQuantity<THertzUnit> = (FValue: 1E-30);

// main definition [ Hz ] = [ 1 ] / [ s ]
operator /(const ALeft: double; const ARight: TSeconds): THertz; inline;
operator *(const ALeft: TSeconds; const ARight: THertz): double; inline;
operator *(const ALeft: THertz; const ARight: TSeconds): double; inline;
operator /(const ALeft: double; const ARight: THertz): TSeconds; inline;
operator /(const ALeft: double; const {%H-}ARight: TSecondUnitId): THertz; inline;

type
  { Unit of SquareHertz }
  TSquareHertzUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSquareHertz = specialize TQuantity<TSquareHertzUnit>;
  TSquareHertzUnitId = specialize TUnitId<TSquareHertzUnit>;

var Hz2: TSquareHertzUnitId;

const quettaHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E+60);
const  ronnaHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E+54);
const  yottaHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E+48);
const  zettaHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E+42);
const      EHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E+36);
const   petaHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E+30);
const      THz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E+24);
const      GHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E+18);
const   megaHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E+12);
const      kHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E+06);
const      hHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E+04);
const     daHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E+02);
const      dHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E-02);
const      cHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E-04);
const      mHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E-06);
const     miHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E-12);
const      nHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E-18);
const      pHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E-24);
const      fHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E-30);
const      aHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E-36);
const  zeptoHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E-42);
const  yoctoHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E-48);
const  rontoHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E-54);
const quectoHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E-60);

// main definition [ Hz2 ] = [ 1 ] / [ s2 ]
operator /(const ALeft: double; const ARight: TSquareSeconds): TSquareHertz; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TSquareHertz): double; inline;
operator *(const ALeft: TSquareHertz; const ARight: TSquareSeconds): double; inline;
operator /(const ALeft: double; const ARight: TSquareHertz): TSquareSeconds; inline;
operator /(const ALeft: double; const {%H-}ARight: TSquareSecondUnitId): TSquareHertz; inline;

// alternative definition [ Hz2 ] = [ Hz ] / [ s ]
operator /(const ALeft: THertz; const ARight: TSeconds): TSquareHertz; inline;
operator *(const ALeft: TSeconds; const ARight: TSquareHertz): THertz; inline;
operator *(const ALeft: TSquareHertz; const ARight: TSeconds): THertz; inline;
operator /(const ALeft: THertz; const ARight: TSquareHertz): TSeconds; inline;

// alternative definition [ Hz2 ] = [ Hz ] * [ Hz ]
operator *(const ALeft: THertz; const ARight: THertz): TSquareHertz; inline;
operator /(const ALeft: TSquareHertz; const ARight: THertz): THertz; inline;

type
  { Unit of RadianPerSecond }
  TRadianPerSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TRadiansPerSecond = specialize TQuantity<TRadianPerSecondUnit>;
  TRadianPerSecondUnitId = specialize TUnitId<TRadianPerSecondUnit>;

// main definition [ rad/s ] = [ rad ] / [ s ]
operator /(const ALeft: TRadians; const ARight: TSeconds): TRadiansPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TRadiansPerSecond): TRadians; inline;
operator *(const ALeft: TRadiansPerSecond; const ARight: TSeconds): TRadians; inline;
operator /(const ALeft: TRadians; const ARight: TRadiansPerSecond): TSeconds; inline;
operator /(const ALeft: TRadians; const {%H-}ARight: TSecondUnitId): TRadiansPerSecond; inline;

// alternative definition [ rad/s ] = [ rad ] * [ Hz ]
operator *(const ALeft: TRadians; const ARight: THertz): TRadiansPerSecond; inline;
operator *(const ALeft: THertz; const ARight: TRadians): TRadiansPerSecond; inline;
operator /(const ALeft: TRadiansPerSecond; const ARight: TRadians): THertz; inline;
operator /(const ALeft: TRadiansPerSecond; const ARight: THertz): TRadians; inline;

operator :=(const AQuantity: TRadiansPerSecond): THertz; inline;
operator :=(const AQuantity: THertz): TRadiansPerSecond; inline;

type
  { Unit of RadianPerSquareSecond }
  TRadianPerSquareSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TRadiansPerSquareSecond = specialize TQuantity<TSquareHertzUnit>;
  TRadianPerSquareSecondUnitId = specialize TUnitId<TSquareHertzUnit>;

// main definition [ rad/s2 ] = [ rad ] / [ s2 ]
operator /(const ALeft: TRadians; const ARight: TSquareSeconds): TRadiansPerSquareSecond; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TRadiansPerSquareSecond): TRadians; inline;
operator *(const ALeft: TRadiansPerSquareSecond; const ARight: TSquareSeconds): TRadians; inline;
operator /(const ALeft: TRadians; const ARight: TRadiansPerSquareSecond): TSquareSeconds; inline;
operator /(const ALeft: TRadians; const {%H-}ARight: TSquareSecondUnitId): TRadiansPerSquareSecond; inline;

// alternative definition [ rad/s2 ] = [ rad ] * [ Hz2 ]
operator *(const ALeft: TRadians; const ARight: TSquareHertz): TRadiansPerSquareSecond; inline;
operator *(const ALeft: TSquareHertz; const ARight: TRadians): TRadiansPerSquareSecond; inline;
operator /(const ALeft: TRadiansPerSquareSecond; const ARight: TRadians): TSquareHertz; inline;
operator /(const ALeft: TRadiansPerSquareSecond; const ARight: TSquareHertz): TRadians; inline;

type
  { Unit of SteradianPerSquareSecond }
  TSteradianPerSquareSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSteradiansPerSquareSecond = specialize TQuantity<TSquareHertzUnit>;
  TSteradianPerSquareSecondUnitId = specialize TUnitId<TSquareHertzUnit>;

// main definition [ sr/s2 ] = [ sr ] / [ s2 ]
operator /(const ALeft: TSteradians; const ARight: TSquareSeconds): TSteradiansPerSquareSecond; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TSteradiansPerSquareSecond): TSteradians; inline;
operator *(const ALeft: TSteradiansPerSquareSecond; const ARight: TSquareSeconds): TSteradians; inline;
operator /(const ALeft: TSteradians; const ARight: TSteradiansPerSquareSecond): TSquareSeconds; inline;
operator /(const ALeft: TSteradians; const {%H-}ARight: TSquareSecondUnitId): TSteradiansPerSquareSecond; inline;

// alternative definition [ sr/s2 ] = [ sr ] * [ Hz2 ]
operator *(const ALeft: TSteradians; const ARight: TSquareHertz): TSteradiansPerSquareSecond; inline;
operator *(const ALeft: TSquareHertz; const ARight: TSteradians): TSteradiansPerSquareSecond; inline;
operator /(const ALeft: TSteradiansPerSquareSecond; const ARight: TSteradians): TSquareHertz; inline;
operator /(const ALeft: TSteradiansPerSquareSecond; const ARight: TSquareHertz): TSteradians; inline;

type
  { Unit of MeterPerSecond }
  TMeterPerSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TMetersPerSecond = specialize TQuantity<TMeterPerSecondUnit>;
  TMeterPerSecondUnitId = specialize TUnitId<TMeterPerSecondUnit>;

// main definition [ m/s ] = [ m ] / [ s ]
operator /(const ALeft: TMeters; const ARight: TSeconds): TMetersPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TMetersPerSecond): TMeters; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TSeconds): TMeters; inline;
operator /(const ALeft: TMeters; const ARight: TMetersPerSecond): TSeconds; inline;
operator /(const ALeft: TMeters; const {%H-}ARight: TSecondUnitId): TMetersPerSecond; inline;

// alternative definition [ m/s ] = [ m ] * [ Hz ]
operator *(const ALeft: TMeters; const ARight: THertz): TMetersPerSecond; inline;
operator *(const ALeft: THertz; const ARight: TMeters): TMetersPerSecond; inline;
operator /(const ALeft: TMetersPerSecond; const ARight: TMeters): THertz; inline;
operator /(const ALeft: TMetersPerSecond; const ARight: THertz): TMeters; inline;
operator *(const ALeft: TMeters; const {%H-}ARight: THertzUnitId): TMetersPerSecond; inline;

type
  { Unit of MeterPerHour }
  TMeterPerHourUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
    const Factor = 1/3600;
  end;
  TMetersPerHour = specialize TQuantity<TMeterPerSecondUnit>;
  TMeterPerHourUnitId = specialize TUnitId<TMeterPerHourUnit>;

type
  { Unit of MeterPerSquareSecond }
  TMeterPerSquareSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TMetersPerSquareSecond = specialize TQuantity<TMeterPerSquareSecondUnit>;
  TMeterPerSquareSecondUnitId = specialize TUnitId<TMeterPerSquareSecondUnit>;

// main definition [ m/s2 ] = [ m ] / [ s2 ]
operator /(const ALeft: TMeters; const ARight: TSquareSeconds): TMetersPerSquareSecond; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TMetersPerSquareSecond): TMeters; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TSquareSeconds): TMeters; inline;
operator /(const ALeft: TMeters; const ARight: TMetersPerSquareSecond): TSquareSeconds; inline;
operator /(const ALeft: TMeters; const {%H-}ARight: TSquareSecondUnitId): TMetersPerSquareSecond; inline;

// alternative definition [ m/s2 ] = [ m/s ] / [ s ]
operator /(const ALeft: TMetersPerSecond; const ARight: TSeconds): TMetersPerSquareSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TMetersPerSquareSecond): TMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TSeconds): TMetersPerSecond; inline;
operator /(const ALeft: TMetersPerSecond; const ARight: TMetersPerSquareSecond): TSeconds; inline;
operator /(const ALeft: TMetersPerSecond; const {%H-}ARight: TSecondUnitId): TMetersPerSquareSecond; inline;

// alternative definition [ m/s2 ] = [ Hz2 ] * [ m ]
operator *(const ALeft: TSquareHertz; const ARight: TMeters): TMetersPerSquareSecond; inline;
operator *(const ALeft: TMeters; const ARight: TSquareHertz): TMetersPerSquareSecond; inline;
operator /(const ALeft: TMetersPerSquareSecond; const ARight: TSquareHertz): TMeters; inline;
operator /(const ALeft: TMetersPerSquareSecond; const ARight: TMeters): TSquareHertz; inline;

type
  { Unit of MeterPerSecondPerSecond }
  TMeterPerSecondPerSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TMetersPerSecondPerSecond = specialize TQuantity<TMeterPerSquareSecondUnit>;
  TMeterPerSecondPerSecondUnitId = specialize TUnitId<TMeterPerSquareSecondUnit>;

type
  { Unit of MeterPerHourPerSecond }
  TMeterPerHourPerSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
    const Factor = 1/3600;
  end;
  TMetersPerHourPerSecond = specialize TQuantity<TMeterPerSquareSecondUnit>;
  TMeterPerHourPerSecondUnitId = specialize TUnitId<TMeterPerHourPerSecondUnit>;

type
  { Unit of SquareMeterPerSquareSecond }
  TSquareMeterPerSquareSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSquareMetersPerSquareSecond = specialize TQuantity<TSquareMeterPerSquareSecondUnit>;
  TSquareMeterPerSquareSecondUnitId = specialize TUnitId<TSquareMeterPerSquareSecondUnit>;

// main definition [ m2/s2 ] = [ m2 ] / [ s2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareSeconds): TSquareMetersPerSquareSecond; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TSquareMetersPerSquareSecond): TSquareMeters; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TSquareSeconds): TSquareMeters; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareSecond): TSquareSeconds; inline;
operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSquareSecondUnitId): TSquareMetersPerSquareSecond; inline;

// alternative definition [ m2/s2 ] = [ m/s ] / [ m/s ]
operator *(const ALeft: TMetersPerSecond; const ARight: TMetersPerSecond): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSecond): TMetersPerSecond; inline;

// alternative definition [ m2/s2 ] = [ m/s2 ] * [ m ]
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TMeters): TSquareMetersPerSquareSecond; inline;
operator *(const ALeft: TMeters; const ARight: TMetersPerSquareSecond): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSquareSecond): TMeters; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMeters): TMetersPerSquareSecond; inline;

type
  { Unit of KilogramMeterPerSecond }
  TKilogramMeterPerSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TKilogramMetersPerSecond = specialize TQuantity<TKilogramMeterPerSecondUnit>;
  TKilogramMeterPerSecondUnitId = specialize TUnitId<TKilogramMeterPerSecondUnit>;

// main definition [ kg*m/s ] = [kg ] * [ m/s ]
operator *(const ALeft: TKilograms; const ARight: TMetersPerSecond): TKilogramMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TKilograms): TKilogramMetersPerSecond; inline;
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TKilograms): TMetersPerSecond; inline;
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TMetersPerSecond): TKilograms; inline;
operator *(const ALeft: TKilograms; const {%H-}ARight: TMeterPerSecondUnitId): TKilogramMetersPerSecond; inline;

type
  { Unit of NewtonSecond }
  TNewtonSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TNewtonSeconds = specialize TQuantity<TKilogramMeterPerSecondUnit>;
  TNewtonSecondUnitId = specialize TUnitId<TKilogramMeterPerSecondUnit>;

type
  { Unit of KilogramSquareMeter }
  TKilogramSquareMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TKilogramSquareMeters = specialize TQuantity<TKilogramSquareMeterUnit>;
  TKilogramSquareMeterUnitId = specialize TUnitId<TKilogramSquareMeterUnit>;

// main definition [ kg*m2 ] = [ kg ] * [ m2 ]
operator *(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TKilograms): TKilogramSquareMeters; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TKilograms): TSquareMeters; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TSquareMeters): TKilograms; inline;
operator *(const ALeft: TKilograms; const {%H-}ARight: TSquareMeterUnitId): TKilogramSquareMeters; inline;

type
  { Unit of KilogramSquareMeterPerSecond }
  TKilogramSquareMeterPerSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TKilogramSquareMetersPerSecond = specialize TQuantity<TKilogramSquareMeterPerSecondUnit>;
  TKilogramSquareMeterPerSecondUnitId = specialize TUnitId<TKilogramSquareMeterPerSecondUnit>;

// main definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]
operator /(const ALeft: TKilogramSquareMeters; const ARight: TSeconds): TKilogramSquareMetersPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TKilogramSquareMetersPerSecond): TKilogramSquareMeters; inline;
operator *(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TSeconds): TKilogramSquareMeters; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TKilogramSquareMetersPerSecond): TSeconds; inline;
operator /(const ALeft: TKilogramSquareMeters; const {%H-}ARight: TSecondUnitId): TKilogramSquareMetersPerSecond; inline;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] * [ Hz ]
operator *(const ALeft: TKilogramSquareMeters; const ARight: THertz): TKilogramSquareMetersPerSecond; inline;
operator *(const ALeft: THertz; const ARight: TKilogramSquareMeters): TKilogramSquareMetersPerSecond; inline;
operator /(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TKilogramSquareMeters): THertz; inline;
operator /(const ALeft: TKilogramSquareMetersPerSecond; const ARight: THertz): TKilogramSquareMeters; inline;

type
  { Unit of KilogramPerMeter }
  TKilogramPerMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TKilogramsPerMeter = specialize TQuantity<TKilogramPerMeterUnit>;
  TKilogramPerMeterUnitId = specialize TUnitId<TKilogramPerMeterUnit>;

// main definition [ kg/m ] = [ kg ] / [ m ]
operator /(const ALeft: TKilograms; const ARight: TMeters): TKilogramsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TKilogramsPerMeter): TKilograms; inline;
operator *(const ALeft: TKilogramsPerMeter; const ARight: TMeters): TKilograms; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerMeter): TMeters; inline;
operator /(const ALeft: TKilograms; const {%H-}ARight: TMeterUnitId): TKilogramsPerMeter; inline;

type
  { Unit of KilogramPerSquareMeter }
  TKilogramPerSquareMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TKilogramsPerSquareMeter = specialize TQuantity<TKilogramPerSquareMeterUnit>;
  TKilogramPerSquareMeterUnitId = specialize TUnitId<TKilogramPerSquareMeterUnit>;

// main definition [ kg/m2 ] = [ kg ] / [ m2 ]
operator /(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramsPerSquareMeter; inline;
operator *(const ALeft: TSquareMeters; const ARight: TKilogramsPerSquareMeter): TKilograms; inline;
operator *(const ALeft: TKilogramsPerSquareMeter; const ARight: TSquareMeters): TKilograms; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerSquareMeter): TSquareMeters; inline;
operator /(const ALeft: TKilograms; const {%H-}ARight: TSquareMeterUnitId): TKilogramsPerSquareMeter; inline;

type
  { Unit of KilogramPerCubicMeter }
  TKilogramPerCubicMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TKilogramsPerCubicMeter = specialize TQuantity<TKilogramPerCubicMeterUnit>;
  TKilogramPerCubicMeterUnitId = specialize TUnitId<TKilogramPerCubicMeterUnit>;

// main definition [ kg/m3 ] = [ kg ] / [ m3 ]
operator /(const ALeft: TKilograms; const ARight: TCubicMeters): TKilogramsPerCubicMeter; inline;
operator *(const ALeft: TCubicMeters; const ARight: TKilogramsPerCubicMeter): TKilograms; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TCubicMeters): TKilograms; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerCubicMeter): TCubicMeters; inline;
operator /(const ALeft: TKilograms; const {%H-}ARight: TCubicMeterUnitId): TKilogramsPerCubicMeter; inline;

// alternative definition [ kg/m3 ] = [ kg/m2 ] / [ m ]
operator /(const ALeft: TKilogramsPerSquareMeter; const ARight: TMeters): TKilogramsPerCubicMeter; inline;
operator *(const ALeft: TMeters; const ARight: TKilogramsPerCubicMeter): TKilogramsPerSquareMeter; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TMeters): TKilogramsPerSquareMeter; inline;
operator /(const ALeft: TKilogramsPerSquareMeter; const ARight: TKilogramsPerCubicMeter): TMeters; inline;

type
  { Unit of Newton }
  TNewtonUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TNewtons = specialize TQuantity<TNewtonUnit>;
  TNewtonUnitId = specialize TUnitId<TNewtonUnit>;

var N: TNewtonUnitId;

const quettaN: specialize TQuantity<TNewtonUnit> = (FValue: 1E+30);
const  ronnaN: specialize TQuantity<TNewtonUnit> = (FValue: 1E+27);
const  yottaN: specialize TQuantity<TNewtonUnit> = (FValue: 1E+24);
const  zettaN: specialize TQuantity<TNewtonUnit> = (FValue: 1E+21);
const      EN: specialize TQuantity<TNewtonUnit> = (FValue: 1E+18);
const   petaN: specialize TQuantity<TNewtonUnit> = (FValue: 1E+15);
const      TN: specialize TQuantity<TNewtonUnit> = (FValue: 1E+12);
const      GN: specialize TQuantity<TNewtonUnit> = (FValue: 1E+09);
const   megaN: specialize TQuantity<TNewtonUnit> = (FValue: 1E+06);
const      kN: specialize TQuantity<TNewtonUnit> = (FValue: 1E+03);
const      hN: specialize TQuantity<TNewtonUnit> = (FValue: 1E+02);
const     daN: specialize TQuantity<TNewtonUnit> = (FValue: 1E+01);
const      dN: specialize TQuantity<TNewtonUnit> = (FValue: 1E-01);
const      cN: specialize TQuantity<TNewtonUnit> = (FValue: 1E-02);
const      mN: specialize TQuantity<TNewtonUnit> = (FValue: 1E-03);
const     miN: specialize TQuantity<TNewtonUnit> = (FValue: 1E-06);
const      nN: specialize TQuantity<TNewtonUnit> = (FValue: 1E-09);
const      pN: specialize TQuantity<TNewtonUnit> = (FValue: 1E-12);
const      fN: specialize TQuantity<TNewtonUnit> = (FValue: 1E-15);
const      aN: specialize TQuantity<TNewtonUnit> = (FValue: 1E-18);
const  zeptoN: specialize TQuantity<TNewtonUnit> = (FValue: 1E-21);
const  yoctoN: specialize TQuantity<TNewtonUnit> = (FValue: 1E-24);
const  rontoN: specialize TQuantity<TNewtonUnit> = (FValue: 1E-27);
const quectoN: specialize TQuantity<TNewtonUnit> = (FValue: 1E-30);

// main definition [ N ] = [ kg ] * [ m/s2 ]
operator *(const ALeft: TKilograms; const ARight: TMetersPerSquareSecond): TNewtons; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TKilograms): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TKilograms): TMetersPerSquareSecond; inline;
operator /(const ALeft: TNewtons; const ARight: TMetersPerSquareSecond): TKilograms; inline;
operator *(const ALeft: TKilograms; const {%H-}ARight: TMeterPerSquareSecondUnitId): TNewtons; inline;

// alternative definition [ N ] = [ kg/m ] * [ m2/s2 ]
operator *(const ALeft: TKilogramsPerMeter; const ARight: TSquareMetersPerSquareSecond): TNewtons; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilogramsPerMeter): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TKilogramsPerMeter): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareSecond): TKilogramsPerMeter; inline;

// alternative definition [ N ] = [ kg*m/s ] / [ s ]
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TSeconds): TNewtons; inline;
operator *(const ALeft: TSeconds; const ARight: TNewtons): TKilogramMetersPerSecond; inline;
operator *(const ALeft: TNewtons; const ARight: TSeconds): TKilogramMetersPerSecond; inline;
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TNewtons): TSeconds; inline;

type
  { Unit of Pascal }
  TPascalUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TPascals = specialize TQuantity<TPascalUnit>;
  TPascalUnitId = specialize TUnitId<TPascalUnit>;

var Pa: TPascalUnitId;

const quettaPa: specialize TQuantity<TPascalUnit> = (FValue: 1E+30);
const  ronnaPa: specialize TQuantity<TPascalUnit> = (FValue: 1E+27);
const  yottaPa: specialize TQuantity<TPascalUnit> = (FValue: 1E+24);
const  zettaPa: specialize TQuantity<TPascalUnit> = (FValue: 1E+21);
const      EPa: specialize TQuantity<TPascalUnit> = (FValue: 1E+18);
const   petaPa: specialize TQuantity<TPascalUnit> = (FValue: 1E+15);
const      TPa: specialize TQuantity<TPascalUnit> = (FValue: 1E+12);
const      GPa: specialize TQuantity<TPascalUnit> = (FValue: 1E+09);
const      MPa: specialize TQuantity<TPascalUnit> = (FValue: 1E+06);
const      kPa: specialize TQuantity<TPascalUnit> = (FValue: 1E+03);
const      hPa: specialize TQuantity<TPascalUnit> = (FValue: 1E+02);
const     daPa: specialize TQuantity<TPascalUnit> = (FValue: 1E+01);
const      dPa: specialize TQuantity<TPascalUnit> = (FValue: 1E-01);
const      cPa: specialize TQuantity<TPascalUnit> = (FValue: 1E-02);
const  milliPa: specialize TQuantity<TPascalUnit> = (FValue: 1E-03);
const     miPa: specialize TQuantity<TPascalUnit> = (FValue: 1E-06);
const      nPa: specialize TQuantity<TPascalUnit> = (FValue: 1E-09);
const      pPa: specialize TQuantity<TPascalUnit> = (FValue: 1E-12);
const      fPa: specialize TQuantity<TPascalUnit> = (FValue: 1E-15);
const      aPa: specialize TQuantity<TPascalUnit> = (FValue: 1E-18);
const  zeptoPa: specialize TQuantity<TPascalUnit> = (FValue: 1E-21);
const  yoctoPa: specialize TQuantity<TPascalUnit> = (FValue: 1E-24);
const  rontoPa: specialize TQuantity<TPascalUnit> = (FValue: 1E-27);
const quectoPa: specialize TQuantity<TPascalUnit> = (FValue: 1E-30);

// main definition [ Pa ] = [ N ] / [ m2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareMeters): TPascals; inline;
operator *(const ALeft: TSquareMeters; const ARight: TPascals): TNewtons; inline;
operator *(const ALeft: TPascals; const ARight: TSquareMeters): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TPascals): TSquareMeters; inline;
operator /(const ALeft: TNewtons; const {%H-}ARight: TSquareMeterUnitId): TPascals; inline;

// alternative definition [ Pa ] = [ kg/m3 ] * [ m2/s2 ]
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSquareSecond): TPascals; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilogramsPerCubicMeter): TPascals; inline;
operator /(const ALeft: TPascals; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TPascals; const ARight: TSquareMetersPerSquareSecond): TKilogramsPerCubicMeter; inline;

type
  { Unit of Joule }
  TJouleUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TJoules = specialize TQuantity<TJouleUnit>;
  TJouleUnitId = specialize TUnitId<TJouleUnit>;

var J: TJouleUnitId;

const quettaJ: specialize TQuantity<TJouleUnit> = (FValue: 1E+30);
const  ronnaJ: specialize TQuantity<TJouleUnit> = (FValue: 1E+27);
const  yottaJ: specialize TQuantity<TJouleUnit> = (FValue: 1E+24);
const  zettaJ: specialize TQuantity<TJouleUnit> = (FValue: 1E+21);
const      EJ: specialize TQuantity<TJouleUnit> = (FValue: 1E+18);
const   petaJ: specialize TQuantity<TJouleUnit> = (FValue: 1E+15);
const      TJ: specialize TQuantity<TJouleUnit> = (FValue: 1E+12);
const      GJ: specialize TQuantity<TJouleUnit> = (FValue: 1E+09);
const   megaJ: specialize TQuantity<TJouleUnit> = (FValue: 1E+06);
const      kJ: specialize TQuantity<TJouleUnit> = (FValue: 1E+03);
const      hJ: specialize TQuantity<TJouleUnit> = (FValue: 1E+02);
const     daJ: specialize TQuantity<TJouleUnit> = (FValue: 1E+01);
const      dJ: specialize TQuantity<TJouleUnit> = (FValue: 1E-01);
const      cJ: specialize TQuantity<TJouleUnit> = (FValue: 1E-02);
const      mJ: specialize TQuantity<TJouleUnit> = (FValue: 1E-03);
const     miJ: specialize TQuantity<TJouleUnit> = (FValue: 1E-06);
const      nJ: specialize TQuantity<TJouleUnit> = (FValue: 1E-09);
const      pJ: specialize TQuantity<TJouleUnit> = (FValue: 1E-12);
const      fJ: specialize TQuantity<TJouleUnit> = (FValue: 1E-15);
const      aJ: specialize TQuantity<TJouleUnit> = (FValue: 1E-18);
const  zeptoJ: specialize TQuantity<TJouleUnit> = (FValue: 1E-21);
const  yoctoJ: specialize TQuantity<TJouleUnit> = (FValue: 1E-24);
const  rontoJ: specialize TQuantity<TJouleUnit> = (FValue: 1E-27);
const quectoJ: specialize TQuantity<TJouleUnit> = (FValue: 1E-30);

// main definition [ J ] = [ N ] * [ m ]
operator *(const ALeft: TNewtons; const ARight: TMeters): TJoules; inline;
operator *(const ALeft: TMeters; const ARight: TNewtons): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TNewtons): TMeters; inline;
operator /(const ALeft: TJoules; const ARight: TMeters): TNewtons; inline;
operator *(const ALeft: TNewtons; const {%H-}ARight: TMeterUnitId): TJoules; inline;

// alternative definition [ J ] = [ Pa ] * [ m3 ]
operator *(const ALeft: TPascals; const ARight: TCubicMeters): TJoules; inline;
operator *(const ALeft: TCubicMeters; const ARight: TPascals): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TPascals): TCubicMeters; inline;
operator /(const ALeft: TJoules; const ARight: TCubicMeters): TPascals; inline;

// alternative definition [ J ] = [ kg*m/s ] * [ m/s ]
operator *(const ALeft: TKilogramMetersPerSecond; const ARight: TMetersPerSecond): TJoules; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TKilogramMetersPerSecond): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TKilogramMetersPerSecond): TMetersPerSecond; inline;
operator /(const ALeft: TJoules; const ARight: TMetersPerSecond): TKilogramMetersPerSecond; inline;

// alternative definition [ J ] = [ kg ] * [ m2/s2 ]
operator *(const ALeft: TKilograms; const ARight: TSquareMetersPerSquareSecond): TJoules; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilograms): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TKilograms): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TJoules; const ARight: TSquareMetersPerSquareSecond): TKilograms; inline;
operator /(const ALeft: TJoules; const {%H-}ARight: TKilogramUnitId): TSquareMetersPerSquareSecond; inline;

// alternative definition [ J ] = [ kg*m2 ] * [ Hz2 ]
operator *(const ALeft: TKilogramSquareMeters; const ARight: TSquareHertz): TJoules; inline;
operator *(const ALeft: TSquareHertz; const ARight: TKilogramSquareMeters): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TKilogramSquareMeters): TSquareHertz; inline;
operator /(const ALeft: TJoules; const ARight: TSquareHertz): TKilogramSquareMeters; inline;

// alternative definition [ J ] = [ kg*m2 ] / [ s2 ]
operator /(const ALeft: TKilogramSquareMeters; const ARight: TSquareSeconds): TJoules; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TJoules): TKilogramSquareMeters; inline;
operator *(const ALeft: TJoules; const ARight: TSquareSeconds): TKilogramSquareMeters; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TJoules): TSquareSeconds; inline;

type
  { Unit of Elettronvolt }
  TElettronvoltUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
    const Factor = 1.60217742320523E-019;
  end;
  TElettronvolts = specialize TQuantity<TJouleUnit>;
  TElettronvoltUnitId = specialize TUnitId<TElettronvoltUnit>;

const eV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019);

const quettaeV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E+30);
const  ronnaeV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E+27);
const  yottaeV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E+24);
const  zettaeV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E+21);
const      EeV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E+18);
const   petaeV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E+15);
const      TeV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E+12);
const      GeV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E+09);
const   megaeV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E+06);
const      keV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E+03);
const      heV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E+02);
const     daeV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E+01);
const      deV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E-01);
const      ceV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E-02);
const      meV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E-03);
const     mieV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E-06);
const      neV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E-09);
const      peV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E-12);
const      feV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E-15);
const      aeV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E-18);
const  zeptoeV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E-21);
const  yoctoeV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E-24);
const  rontoeV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E-27);
const quectoeV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E-30);

type
  { Unit of Watt }
  TWattUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TWatts = specialize TQuantity<TWattUnit>;
  TWattUnitId = specialize TUnitId<TWattUnit>;

var W: TWattUnitId;

const quettaW: specialize TQuantity<TWattUnit> = (FValue: 1E+30);
const  ronnaW: specialize TQuantity<TWattUnit> = (FValue: 1E+27);
const  yottaW: specialize TQuantity<TWattUnit> = (FValue: 1E+24);
const  zettaW: specialize TQuantity<TWattUnit> = (FValue: 1E+21);
const      EW: specialize TQuantity<TWattUnit> = (FValue: 1E+18);
const   petaW: specialize TQuantity<TWattUnit> = (FValue: 1E+15);
const      TW: specialize TQuantity<TWattUnit> = (FValue: 1E+12);
const      GW: specialize TQuantity<TWattUnit> = (FValue: 1E+09);
const   megaW: specialize TQuantity<TWattUnit> = (FValue: 1E+06);
const      kW: specialize TQuantity<TWattUnit> = (FValue: 1E+03);
const      hW: specialize TQuantity<TWattUnit> = (FValue: 1E+02);
const     daW: specialize TQuantity<TWattUnit> = (FValue: 1E+01);
const      dW: specialize TQuantity<TWattUnit> = (FValue: 1E-01);
const      cW: specialize TQuantity<TWattUnit> = (FValue: 1E-02);
const      mW: specialize TQuantity<TWattUnit> = (FValue: 1E-03);
const     miW: specialize TQuantity<TWattUnit> = (FValue: 1E-06);
const      nW: specialize TQuantity<TWattUnit> = (FValue: 1E-09);
const      pW: specialize TQuantity<TWattUnit> = (FValue: 1E-12);
const      fW: specialize TQuantity<TWattUnit> = (FValue: 1E-15);
const      aW: specialize TQuantity<TWattUnit> = (FValue: 1E-18);
const  zeptoW: specialize TQuantity<TWattUnit> = (FValue: 1E-21);
const  yoctoW: specialize TQuantity<TWattUnit> = (FValue: 1E-24);
const  rontoW: specialize TQuantity<TWattUnit> = (FValue: 1E-27);
const quectoW: specialize TQuantity<TWattUnit> = (FValue: 1E-30);

// main definition [ W ] = [ J ] / [ s ]
operator /(const ALeft: TJoules; const ARight: TSeconds): TWatts; inline;
operator *(const ALeft: TSeconds; const ARight: TWatts): TJoules; inline;
operator *(const ALeft: TWatts; const ARight: TSeconds): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TWatts): TSeconds; inline;
operator /(const ALeft: TJoules; const {%H-}ARight: TSecondUnitId): TWatts; inline;

// alternative definition [ W ] = [ J ] * [ Hz ]
operator *(const ALeft: TJoules; const ARight: THertz): TWatts; inline;
operator *(const ALeft: THertz; const ARight: TJoules): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TJoules): THertz; inline;
operator /(const ALeft: TWatts; const ARight: THertz): TJoules; inline;

// alternative definition [ W ] = [ N ] * [ m/s ]
operator *(const ALeft: TNewtons; const ARight: TMetersPerSecond): TWatts; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TNewtons): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TNewtons): TMetersPerSecond; inline;
operator /(const ALeft: TWatts; const ARight: TMetersPerSecond): TNewtons; inline;

type
  { Unit of Coulomb }
  TCoulombUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TCoulombs = specialize TQuantity<TCoulombUnit>;
  TCoulombUnitId = specialize TUnitId<TCoulombUnit>;

var C: TCoulombUnitId;

const quettaC: specialize TQuantity<TCoulombUnit> = (FValue: 1E+30);
const  ronnaC: specialize TQuantity<TCoulombUnit> = (FValue: 1E+27);
const  yottaC: specialize TQuantity<TCoulombUnit> = (FValue: 1E+24);
const  zettaC: specialize TQuantity<TCoulombUnit> = (FValue: 1E+21);
const      EC: specialize TQuantity<TCoulombUnit> = (FValue: 1E+18);
const   petaC: specialize TQuantity<TCoulombUnit> = (FValue: 1E+15);
const      TC: specialize TQuantity<TCoulombUnit> = (FValue: 1E+12);
const      GC: specialize TQuantity<TCoulombUnit> = (FValue: 1E+09);
const   megaC: specialize TQuantity<TCoulombUnit> = (FValue: 1E+06);
const      kC: specialize TQuantity<TCoulombUnit> = (FValue: 1E+03);
const      hC: specialize TQuantity<TCoulombUnit> = (FValue: 1E+02);
const     daC: specialize TQuantity<TCoulombUnit> = (FValue: 1E+01);
const      dC: specialize TQuantity<TCoulombUnit> = (FValue: 1E-01);
const      cC: specialize TQuantity<TCoulombUnit> = (FValue: 1E-02);
const      mC: specialize TQuantity<TCoulombUnit> = (FValue: 1E-03);
const     miC: specialize TQuantity<TCoulombUnit> = (FValue: 1E-06);
const      nC: specialize TQuantity<TCoulombUnit> = (FValue: 1E-09);
const      pC: specialize TQuantity<TCoulombUnit> = (FValue: 1E-12);
const      fC: specialize TQuantity<TCoulombUnit> = (FValue: 1E-15);
const      aC: specialize TQuantity<TCoulombUnit> = (FValue: 1E-18);
const  zeptoC: specialize TQuantity<TCoulombUnit> = (FValue: 1E-21);
const  yoctoC: specialize TQuantity<TCoulombUnit> = (FValue: 1E-24);
const  rontoC: specialize TQuantity<TCoulombUnit> = (FValue: 1E-27);
const quectoC: specialize TQuantity<TCoulombUnit> = (FValue: 1E-30);

// main definition [ C ] = [ s ] * [ A ]
operator *(const ALeft: TSeconds; const ARight: TAmperes): TCoulombs; inline;
operator *(const ALeft: TAmperes; const ARight: TSeconds): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TSeconds): TAmperes; inline;
operator /(const ALeft: TCoulombs; const ARight: TAmperes): TSeconds; inline;
operator *(const ALeft: TSeconds; const {%H-}ARight: TAmpereUnitId): TCoulombs; inline;

type
  { Unit of SquareCoulomb }
  TSquareCoulombUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSquareCoulombs = specialize TQuantity<TSquareCoulombUnit>;
  TSquareCoulombUnitId = specialize TUnitId<TSquareCoulombUnit>;

var C2: TSquareCoulombUnitId;

const quettaC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E+60);
const  ronnaC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E+54);
const  yottaC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E+48);
const  zettaC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E+42);
const      EC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E+36);
const   petaC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E+30);
const      TC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E+24);
const      GC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E+18);
const   megaC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E+12);
const      kC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E+06);
const      hC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E+04);
const     daC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E+02);
const      dC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E-02);
const      cC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E-04);
const      mC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E-06);
const     miC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E-12);
const      nC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E-18);
const      pC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E-24);
const      fC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E-30);
const      aC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E-36);
const  zeptoC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E-42);
const  yoctoC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E-48);
const  rontoC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E-54);
const quectoC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E-60);

// main definition [ C2 ] = [ C ] * [ C ]
operator *(const ALeft: TCoulombs; const ARight: TCoulombs): TSquareCoulombs; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TCoulombs): TCoulombs; inline;

type
  { Unit of Volt }
  TVoltUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TVolts = specialize TQuantity<TVoltUnit>;
  TVoltUnitId = specialize TUnitId<TVoltUnit>;

var V: TVoltUnitId;

const quettaV: specialize TQuantity<TVoltUnit> = (FValue: 1E+30);
const  ronnaV: specialize TQuantity<TVoltUnit> = (FValue: 1E+27);
const  yottaV: specialize TQuantity<TVoltUnit> = (FValue: 1E+24);
const  zettaV: specialize TQuantity<TVoltUnit> = (FValue: 1E+21);
const    exaV: specialize TQuantity<TVoltUnit> = (FValue: 1E+18);
const   petaV: specialize TQuantity<TVoltUnit> = (FValue: 1E+15);
const      TV: specialize TQuantity<TVoltUnit> = (FValue: 1E+12);
const      GV: specialize TQuantity<TVoltUnit> = (FValue: 1E+09);
const   megaV: specialize TQuantity<TVoltUnit> = (FValue: 1E+06);
const      kV: specialize TQuantity<TVoltUnit> = (FValue: 1E+03);
const      hV: specialize TQuantity<TVoltUnit> = (FValue: 1E+02);
const     daV: specialize TQuantity<TVoltUnit> = (FValue: 1E+01);
const      dV: specialize TQuantity<TVoltUnit> = (FValue: 1E-01);
const      cV: specialize TQuantity<TVoltUnit> = (FValue: 1E-02);
const      mV: specialize TQuantity<TVoltUnit> = (FValue: 1E-03);
const     miV: specialize TQuantity<TVoltUnit> = (FValue: 1E-06);
const      nV: specialize TQuantity<TVoltUnit> = (FValue: 1E-09);
const      pV: specialize TQuantity<TVoltUnit> = (FValue: 1E-12);
const      fV: specialize TQuantity<TVoltUnit> = (FValue: 1E-15);
const      aV: specialize TQuantity<TVoltUnit> = (FValue: 1E-18);
const  zeptoV: specialize TQuantity<TVoltUnit> = (FValue: 1E-21);
const  yoctoV: specialize TQuantity<TVoltUnit> = (FValue: 1E-24);
const  rontoV: specialize TQuantity<TVoltUnit> = (FValue: 1E-27);
const quectoV: specialize TQuantity<TVoltUnit> = (FValue: 1E-30);

// main definition [ V ] = [ W ] / [ A ]
operator /(const ALeft: TWatts; const ARight: TAmperes): TVolts; inline;
operator *(const ALeft: TAmperes; const ARight: TVolts): TWatts; inline;
operator *(const ALeft: TVolts; const ARight: TAmperes): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TVolts): TAmperes; inline;
operator /(const ALeft: TWatts; const {%H-}ARight: TAmpereUnitId): TVolts; inline;

// alternative definition [ V ] = [ J ] / [ C ]
operator /(const ALeft: TJoules; const ARight: TCoulombs): TVolts; inline;
operator *(const ALeft: TCoulombs; const ARight: TVolts): TJoules; inline;
operator *(const ALeft: TVolts; const ARight: TCoulombs): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TVolts): TCoulombs; inline;
operator /(const ALeft: TJoules; const {%H-}ARight: TCoulombUnitId): TVolts; inline;

type
  { Unit of SquareVolt }
  TSquareVoltUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSquareVolts = specialize TQuantity<TSquareVoltUnit>;
  TSquareVoltUnitId = specialize TUnitId<TSquareVoltUnit>;

var V2: TSquareVoltUnitId;

const quettaV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E+60);
const  ronnaV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E+54);
const  yottaV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E+48);
const  zettaV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E+42);
const      EV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E+36);
const   petaV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E+30);
const      TV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E+24);
const      GV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E+18);
const   megaV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E+12);
const      kV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E+06);
const      hV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E+04);
const     daV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E+02);
const      dV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E-02);
const      cV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E-04);
const      mV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E-06);
const     miV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E-12);
const      nV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E-18);
const      pV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E-24);
const      fV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E-30);
const      aV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E-36);
const  zeptoV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E-42);
const  yoctoV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E-48);
const  rontoV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E-54);
const quectoV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E-60);

// main definition [ V2 ] = [ V ] * [ V ]
operator *(const ALeft: TVolts; const ARight: TVolts): TSquareVolts; inline;
operator /(const ALeft: TSquareVolts; const ARight: TVolts): TVolts; inline;

type
  { Unit of Farad }
  TFaradUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TFarads = specialize TQuantity<TFaradUnit>;
  TFaradUnitId = specialize TUnitId<TFaradUnit>;

var F: TFaradUnitId;

const quettaF: specialize TQuantity<TFaradUnit> = (FValue: 1E+30);
const  ronnaF: specialize TQuantity<TFaradUnit> = (FValue: 1E+27);
const  yottaF: specialize TQuantity<TFaradUnit> = (FValue: 1E+24);
const  zettaF: specialize TQuantity<TFaradUnit> = (FValue: 1E+21);
const      EF: specialize TQuantity<TFaradUnit> = (FValue: 1E+18);
const   petaF: specialize TQuantity<TFaradUnit> = (FValue: 1E+15);
const      TF: specialize TQuantity<TFaradUnit> = (FValue: 1E+12);
const      GF: specialize TQuantity<TFaradUnit> = (FValue: 1E+09);
const   megaF: specialize TQuantity<TFaradUnit> = (FValue: 1E+06);
const      kF: specialize TQuantity<TFaradUnit> = (FValue: 1E+03);
const      hF: specialize TQuantity<TFaradUnit> = (FValue: 1E+02);
const     daF: specialize TQuantity<TFaradUnit> = (FValue: 1E+01);
const      dF: specialize TQuantity<TFaradUnit> = (FValue: 1E-01);
const      cF: specialize TQuantity<TFaradUnit> = (FValue: 1E-02);
const      mF: specialize TQuantity<TFaradUnit> = (FValue: 1E-03);
const     miF: specialize TQuantity<TFaradUnit> = (FValue: 1E-06);
const      nF: specialize TQuantity<TFaradUnit> = (FValue: 1E-09);
const      pF: specialize TQuantity<TFaradUnit> = (FValue: 1E-12);
const      fF: specialize TQuantity<TFaradUnit> = (FValue: 1E-15);
const      aF: specialize TQuantity<TFaradUnit> = (FValue: 1E-18);
const  zeptoF: specialize TQuantity<TFaradUnit> = (FValue: 1E-21);
const  yoctoF: specialize TQuantity<TFaradUnit> = (FValue: 1E-24);
const  rontoF: specialize TQuantity<TFaradUnit> = (FValue: 1E-27);
const quectoF: specialize TQuantity<TFaradUnit> = (FValue: 1E-30);

// main definition [ F ] = [ C ] / [ V ]
operator /(const ALeft: TCoulombs; const ARight: TVolts): TFarads; inline;
operator *(const ALeft: TVolts; const ARight: TFarads): TCoulombs; inline;
operator *(const ALeft: TFarads; const ARight: TVolts): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TFarads): TVolts; inline;
operator /(const ALeft: TCoulombs; const {%H-}ARight: TVoltUnitId): TFarads; inline;

// alternative definition [ F ] = [ C2 ] / [ J ]
operator /(const ALeft: TSquareCoulombs; const ARight: TJoules): TFarads; inline;
operator *(const ALeft: TJoules; const ARight: TFarads): TSquareCoulombs; inline;
operator *(const ALeft: TFarads; const ARight: TJoules): TSquareCoulombs; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TFarads): TJoules; inline;

type
  { Unit of Ohm }
  TOhmUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TOhms = specialize TQuantity<TOhmUnit>;
  TOhmUnitId = specialize TUnitId<TOhmUnit>;

var ohm: TOhmUnitId;

const quettaohm: specialize TQuantity<TOhmUnit> = (FValue: 1E+30);
const  ronnaohm: specialize TQuantity<TOhmUnit> = (FValue: 1E+27);
const  yottaohm: specialize TQuantity<TOhmUnit> = (FValue: 1E+24);
const  zettaohm: specialize TQuantity<TOhmUnit> = (FValue: 1E+21);
const      Eohm: specialize TQuantity<TOhmUnit> = (FValue: 1E+18);
const   petaohm: specialize TQuantity<TOhmUnit> = (FValue: 1E+15);
const      Tohm: specialize TQuantity<TOhmUnit> = (FValue: 1E+12);
const      Gohm: specialize TQuantity<TOhmUnit> = (FValue: 1E+09);
const   megaohm: specialize TQuantity<TOhmUnit> = (FValue: 1E+06);
const      kohm: specialize TQuantity<TOhmUnit> = (FValue: 1E+03);
const      hohm: specialize TQuantity<TOhmUnit> = (FValue: 1E+02);
const     daohm: specialize TQuantity<TOhmUnit> = (FValue: 1E+01);
const      dohm: specialize TQuantity<TOhmUnit> = (FValue: 1E-01);
const      cohm: specialize TQuantity<TOhmUnit> = (FValue: 1E-02);
const      mohm: specialize TQuantity<TOhmUnit> = (FValue: 1E-03);
const     miohm: specialize TQuantity<TOhmUnit> = (FValue: 1E-06);
const      nohm: specialize TQuantity<TOhmUnit> = (FValue: 1E-09);
const      pohm: specialize TQuantity<TOhmUnit> = (FValue: 1E-12);
const      fohm: specialize TQuantity<TOhmUnit> = (FValue: 1E-15);
const      aohm: specialize TQuantity<TOhmUnit> = (FValue: 1E-18);
const  zeptoohm: specialize TQuantity<TOhmUnit> = (FValue: 1E-21);
const  yoctoohm: specialize TQuantity<TOhmUnit> = (FValue: 1E-24);
const  rontoohm: specialize TQuantity<TOhmUnit> = (FValue: 1E-27);
const quectoohm: specialize TQuantity<TOhmUnit> = (FValue: 1E-30);

// main definition [ Ω ] = [ V ] / [ A ]
operator /(const ALeft: TVolts; const ARight: TAmperes): TOhms; inline;
operator *(const ALeft: TAmperes; const ARight: TOhms): TVolts; inline;
operator *(const ALeft: TOhms; const ARight: TAmperes): TVolts; inline;
operator /(const ALeft: TVolts; const ARight: TOhms): TAmperes; inline;
operator /(const ALeft: TVolts; const {%H-}ARight: TAmpereUnitId): TOhms; inline;

// alternative definition [ Ω ] = [ s ] / [ F ]
operator /(const ALeft: TSeconds; const ARight: TFarads): TOhms; inline;
operator *(const ALeft: TFarads; const ARight: TOhms): TSeconds; inline;
operator *(const ALeft: TOhms; const ARight: TFarads): TSeconds; inline;
operator /(const ALeft: TSeconds; const ARight: TOhms): TFarads; inline;

// alternative definition [ Ω ] = [ W ] / [ A2 ]
operator /(const ALeft: TWatts; const ARight: TSquareAmperes): TOhms; inline;
operator *(const ALeft: TSquareAmperes; const ARight: TOhms): TWatts; inline;
operator *(const ALeft: TOhms; const ARight: TSquareAmperes): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TOhms): TSquareAmperes; inline;

// alternative definition [ Ω ] = [ V2 ] / [ W ]
operator /(const ALeft: TSquareVolts; const ARight: TWatts): TOhms; inline;
operator *(const ALeft: TWatts; const ARight: TOhms): TSquareVolts; inline;
operator *(const ALeft: TOhms; const ARight: TWatts): TSquareVolts; inline;
operator /(const ALeft: TSquareVolts; const ARight: TOhms): TWatts; inline;

type
  { Unit of Siemens }
  TSiemensUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSiemens = specialize TQuantity<TSiemensUnit>;
  TSiemensUnitId = specialize TUnitId<TSiemensUnit>;

var siemens: TSiemensUnitId;

const quettasiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E+30);
const  ronnasiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E+27);
const  yottasiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E+24);
const  zettasiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E+21);
const      Esiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E+18);
const   petasiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E+15);
const   terasiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E+12);
const   gigasiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E+09);
const   megasiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E+06);
const   kilosiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E+03);
const  hectosiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E+02);
const   decasiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E+01);
const   decisiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E-01);
const  centisiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E-02);
const  millisiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E-03);
const  microsiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E-06);
const   nanosiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E-09);
const   picosiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E-12);
const      fsiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E-15);
const      asiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E-18);
const  zeptosiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E-21);
const  yoctosiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E-24);
const  rontosiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E-27);
const quectosiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E-30);

// main definition [ S ] = 1 / [ Ω ]
operator /(const ALeft: double; const ARight: TOhms): TSiemens; inline;
operator *(const ALeft: TOhms; const ARight: TSiemens): double; inline;
operator *(const ALeft: TSiemens; const ARight: TOhms): double; inline;
operator /(const ALeft: double; const ARight: TSiemens): TOhms; inline;
operator /(const ALeft: double; const {%H-}ARight: TOhmUnitId): TSiemens; inline;

type
  { Unit of Weber }
  TWeberUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TWebers = specialize TQuantity<TWeberUnit>;
  TWeberUnitId = specialize TUnitId<TWeberUnit>;

var Wb: TWeberUnitId;

const quettaWb: specialize TQuantity<TWeberUnit> = (FValue: 1E+30);
const  ronnaWb: specialize TQuantity<TWeberUnit> = (FValue: 1E+27);
const  yottaWb: specialize TQuantity<TWeberUnit> = (FValue: 1E+24);
const  zettaWb: specialize TQuantity<TWeberUnit> = (FValue: 1E+21);
const      EWb: specialize TQuantity<TWeberUnit> = (FValue: 1E+18);
const   petaWb: specialize TQuantity<TWeberUnit> = (FValue: 1E+15);
const      TWb: specialize TQuantity<TWeberUnit> = (FValue: 1E+12);
const      GWb: specialize TQuantity<TWeberUnit> = (FValue: 1E+09);
const   megaWb: specialize TQuantity<TWeberUnit> = (FValue: 1E+06);
const      kWb: specialize TQuantity<TWeberUnit> = (FValue: 1E+03);
const      hWb: specialize TQuantity<TWeberUnit> = (FValue: 1E+02);
const     daWb: specialize TQuantity<TWeberUnit> = (FValue: 1E+01);
const      dWb: specialize TQuantity<TWeberUnit> = (FValue: 1E-01);
const      cWb: specialize TQuantity<TWeberUnit> = (FValue: 1E-02);
const      mWb: specialize TQuantity<TWeberUnit> = (FValue: 1E-03);
const     miWb: specialize TQuantity<TWeberUnit> = (FValue: 1E-06);
const      nWb: specialize TQuantity<TWeberUnit> = (FValue: 1E-09);
const      pWb: specialize TQuantity<TWeberUnit> = (FValue: 1E-12);
const      fWb: specialize TQuantity<TWeberUnit> = (FValue: 1E-15);
const      aWb: specialize TQuantity<TWeberUnit> = (FValue: 1E-18);
const  zeptoWb: specialize TQuantity<TWeberUnit> = (FValue: 1E-21);
const  yoctoWb: specialize TQuantity<TWeberUnit> = (FValue: 1E-24);
const  rontoWb: specialize TQuantity<TWeberUnit> = (FValue: 1E-27);
const quectoWb: specialize TQuantity<TWeberUnit> = (FValue: 1E-30);

// main definition [ Wb ] = [ V ] * [ s ]
operator *(const ALeft: TVolts; const ARight: TSeconds): TWebers; inline;
operator *(const ALeft: TSeconds; const ARight: TVolts): TWebers; inline;
operator /(const ALeft: TWebers; const ARight: TVolts): TSeconds; inline;
operator /(const ALeft: TWebers; const ARight: TSeconds): TVolts; inline;
operator *(const ALeft: TVolts; const {%H-}ARight: TSecondUnitId): TWebers; inline;

type
  { Unit of Tesla }
  TTeslaUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TTeslas = specialize TQuantity<TTeslaUnit>;
  TTeslaUnitId = specialize TUnitId<TTeslaUnit>;

var T: TTeslaUnitId;

const quettaT: specialize TQuantity<TTeslaUnit> = (FValue: 1E+30);
const  ronnaT: specialize TQuantity<TTeslaUnit> = (FValue: 1E+27);
const  yottaT: specialize TQuantity<TTeslaUnit> = (FValue: 1E+24);
const  zettaT: specialize TQuantity<TTeslaUnit> = (FValue: 1E+21);
const      ET: specialize TQuantity<TTeslaUnit> = (FValue: 1E+18);
const   petaT: specialize TQuantity<TTeslaUnit> = (FValue: 1E+15);
const      TT: specialize TQuantity<TTeslaUnit> = (FValue: 1E+12);
const      GT: specialize TQuantity<TTeslaUnit> = (FValue: 1E+09);
const   megaT: specialize TQuantity<TTeslaUnit> = (FValue: 1E+06);
const      kT: specialize TQuantity<TTeslaUnit> = (FValue: 1E+03);
const      hT: specialize TQuantity<TTeslaUnit> = (FValue: 1E+02);
const     daT: specialize TQuantity<TTeslaUnit> = (FValue: 1E+01);
const      dT: specialize TQuantity<TTeslaUnit> = (FValue: 1E-01);
const      cT: specialize TQuantity<TTeslaUnit> = (FValue: 1E-02);
const      mT: specialize TQuantity<TTeslaUnit> = (FValue: 1E-03);
const     miT: specialize TQuantity<TTeslaUnit> = (FValue: 1E-06);
const      nT: specialize TQuantity<TTeslaUnit> = (FValue: 1E-09);
const      pT: specialize TQuantity<TTeslaUnit> = (FValue: 1E-12);
const      fT: specialize TQuantity<TTeslaUnit> = (FValue: 1E-15);
const      aT: specialize TQuantity<TTeslaUnit> = (FValue: 1E-18);
const  zeptoT: specialize TQuantity<TTeslaUnit> = (FValue: 1E-21);
const  yoctoT: specialize TQuantity<TTeslaUnit> = (FValue: 1E-24);
const  rontoT: specialize TQuantity<TTeslaUnit> = (FValue: 1E-27);
const quectoT: specialize TQuantity<TTeslaUnit> = (FValue: 1E-30);

// main definition [ T ] = [ Wb ] / [ m2 ]
operator /(const ALeft: TWebers; const ARight: TSquareMeters): TTeslas; inline;
operator *(const ALeft: TSquareMeters; const ARight: TTeslas): TWebers; inline;
operator *(const ALeft: TTeslas; const ARight: TSquareMeters): TWebers; inline;
operator /(const ALeft: TWebers; const ARight: TTeslas): TSquareMeters; inline;
operator /(const ALeft: TWebers; const {%H-}ARight: TSquareMeterUnitId): TTeslas; inline;

type
  { Unit of Henry }
  THenryUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  THenrys = specialize TQuantity<THenryUnit>;
  THenryUnitId = specialize TUnitId<THenryUnit>;

var H: THenryUnitId;

const quettaH: specialize TQuantity<THenryUnit> = (FValue: 1E+30);
const  ronnaH: specialize TQuantity<THenryUnit> = (FValue: 1E+27);
const  yottaH: specialize TQuantity<THenryUnit> = (FValue: 1E+24);
const  zettaH: specialize TQuantity<THenryUnit> = (FValue: 1E+21);
const      EH: specialize TQuantity<THenryUnit> = (FValue: 1E+18);
const   petaH: specialize TQuantity<THenryUnit> = (FValue: 1E+15);
const      TH: specialize TQuantity<THenryUnit> = (FValue: 1E+12);
const      GH: specialize TQuantity<THenryUnit> = (FValue: 1E+09);
const   megaH: specialize TQuantity<THenryUnit> = (FValue: 1E+06);
const      kH: specialize TQuantity<THenryUnit> = (FValue: 1E+03);
const      hH: specialize TQuantity<THenryUnit> = (FValue: 1E+02);
const     daH: specialize TQuantity<THenryUnit> = (FValue: 1E+01);
const      dH: specialize TQuantity<THenryUnit> = (FValue: 1E-01);
const      cH: specialize TQuantity<THenryUnit> = (FValue: 1E-02);
const      mH: specialize TQuantity<THenryUnit> = (FValue: 1E-03);
const     miH: specialize TQuantity<THenryUnit> = (FValue: 1E-06);
const      nH: specialize TQuantity<THenryUnit> = (FValue: 1E-09);
const      pH: specialize TQuantity<THenryUnit> = (FValue: 1E-12);
const      fH: specialize TQuantity<THenryUnit> = (FValue: 1E-15);
const      aH: specialize TQuantity<THenryUnit> = (FValue: 1E-18);
const  zeptoH: specialize TQuantity<THenryUnit> = (FValue: 1E-21);
const  yoctoH: specialize TQuantity<THenryUnit> = (FValue: 1E-24);
const  rontoH: specialize TQuantity<THenryUnit> = (FValue: 1E-27);
const quectoH: specialize TQuantity<THenryUnit> = (FValue: 1E-30);

// main definition [ H ] = [ Wb ] / [ A ]
operator /(const ALeft: TWebers; const ARight: TAmperes): THenrys; inline;
operator *(const ALeft: TAmperes; const ARight: THenrys): TWebers; inline;
operator *(const ALeft: THenrys; const ARight: TAmperes): TWebers; inline;
operator /(const ALeft: TWebers; const ARight: THenrys): TAmperes; inline;
operator /(const ALeft: TWebers; const {%H-}ARight: TAmpereUnitId): THenrys; inline;

// alternative definition [ H ] = [ Ω ] * [ s ]
operator *(const ALeft: TOhms; const ARight: TSeconds): THenrys; inline;
operator *(const ALeft: TSeconds; const ARight: TOhms): THenrys; inline;
operator /(const ALeft: THenrys; const ARight: TOhms): TSeconds; inline;
operator /(const ALeft: THenrys; const ARight: TSeconds): TOhms; inline;

// alternative definition [ H ] = [ Ω ] / [ Hz ]
operator /(const ALeft: TOhms; const ARight: THertz): THenrys; inline;
operator *(const ALeft: THertz; const ARight: THenrys): TOhms; inline;
operator *(const ALeft: THenrys; const ARight: THertz): TOhms; inline;
operator /(const ALeft: TOhms; const ARight: THenrys): THertz; inline;

type
  { Unit of Lumen }
  TLumenUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TLumens = specialize TQuantity<TLumenUnit>;
  TLumenUnitId = specialize TUnitId<TLumenUnit>;

var lm: TLumenUnitId;

const quettalm: specialize TQuantity<TLumenUnit> = (FValue: 1E+30);
const  ronnalm: specialize TQuantity<TLumenUnit> = (FValue: 1E+27);
const  yottalm: specialize TQuantity<TLumenUnit> = (FValue: 1E+24);
const  zettalm: specialize TQuantity<TLumenUnit> = (FValue: 1E+21);
const      Elm: specialize TQuantity<TLumenUnit> = (FValue: 1E+18);
const   petalm: specialize TQuantity<TLumenUnit> = (FValue: 1E+15);
const      Tlm: specialize TQuantity<TLumenUnit> = (FValue: 1E+12);
const      Glm: specialize TQuantity<TLumenUnit> = (FValue: 1E+09);
const   megalm: specialize TQuantity<TLumenUnit> = (FValue: 1E+06);
const      klm: specialize TQuantity<TLumenUnit> = (FValue: 1E+03);
const      hlm: specialize TQuantity<TLumenUnit> = (FValue: 1E+02);
const     dalm: specialize TQuantity<TLumenUnit> = (FValue: 1E+01);
const      dlm: specialize TQuantity<TLumenUnit> = (FValue: 1E-01);
const      clm: specialize TQuantity<TLumenUnit> = (FValue: 1E-02);
const      mlm: specialize TQuantity<TLumenUnit> = (FValue: 1E-03);
const     milm: specialize TQuantity<TLumenUnit> = (FValue: 1E-06);
const      nlm: specialize TQuantity<TLumenUnit> = (FValue: 1E-09);
const      plm: specialize TQuantity<TLumenUnit> = (FValue: 1E-12);
const      flm: specialize TQuantity<TLumenUnit> = (FValue: 1E-15);
const      alm: specialize TQuantity<TLumenUnit> = (FValue: 1E-18);
const  zeptolm: specialize TQuantity<TLumenUnit> = (FValue: 1E-21);
const  yoctolm: specialize TQuantity<TLumenUnit> = (FValue: 1E-24);
const  rontolm: specialize TQuantity<TLumenUnit> = (FValue: 1E-27);
const quectolm: specialize TQuantity<TLumenUnit> = (FValue: 1E-30);

// main definition [ lm ] = [ cd ] * [ sr ]
operator *(const ALeft: TCandelas; const ARight: TSteradians): TLumens; inline;
operator *(const ALeft: TSteradians; const ARight: TCandelas): TLumens; inline;
operator /(const ALeft: TLumens; const ARight: TCandelas): TSteradians; inline;
operator /(const ALeft: TLumens; const ARight: TSteradians): TCandelas; inline;
operator *(const ALeft: TCandelas; const {%H-}ARight: TSteradianUnitId): TLumens; inline;

type
  { Unit of Lux }
  TLuxUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TLux = specialize TQuantity<TLuxUnit>;
  TLuxUnitId = specialize TUnitId<TLuxUnit>;

var lx: TLuxUnitId;

const quettalx: specialize TQuantity<TLuxUnit> = (FValue: 1E+30);
const  ronnalx: specialize TQuantity<TLuxUnit> = (FValue: 1E+27);
const  yottalx: specialize TQuantity<TLuxUnit> = (FValue: 1E+24);
const  zettalx: specialize TQuantity<TLuxUnit> = (FValue: 1E+21);
const      Elx: specialize TQuantity<TLuxUnit> = (FValue: 1E+18);
const   petalx: specialize TQuantity<TLuxUnit> = (FValue: 1E+15);
const      Tlx: specialize TQuantity<TLuxUnit> = (FValue: 1E+12);
const      Glx: specialize TQuantity<TLuxUnit> = (FValue: 1E+09);
const   megalx: specialize TQuantity<TLuxUnit> = (FValue: 1E+06);
const      klx: specialize TQuantity<TLuxUnit> = (FValue: 1E+03);
const      hlx: specialize TQuantity<TLuxUnit> = (FValue: 1E+02);
const     dalx: specialize TQuantity<TLuxUnit> = (FValue: 1E+01);
const      dlx: specialize TQuantity<TLuxUnit> = (FValue: 1E-01);
const      clx: specialize TQuantity<TLuxUnit> = (FValue: 1E-02);
const      mlx: specialize TQuantity<TLuxUnit> = (FValue: 1E-03);
const     milx: specialize TQuantity<TLuxUnit> = (FValue: 1E-06);
const      nlx: specialize TQuantity<TLuxUnit> = (FValue: 1E-09);
const      plx: specialize TQuantity<TLuxUnit> = (FValue: 1E-12);
const      flx: specialize TQuantity<TLuxUnit> = (FValue: 1E-15);
const      alx: specialize TQuantity<TLuxUnit> = (FValue: 1E-18);
const  zeptolx: specialize TQuantity<TLuxUnit> = (FValue: 1E-21);
const  yoctolx: specialize TQuantity<TLuxUnit> = (FValue: 1E-24);
const  rontolx: specialize TQuantity<TLuxUnit> = (FValue: 1E-27);
const quectolx: specialize TQuantity<TLuxUnit> = (FValue: 1E-30);

// main definition [ lx ] = [ lm ] / [ m2 ]
operator /(const ALeft: TLumens; const ARight: TSquareMeters): TLux; inline;
operator *(const ALeft: TSquareMeters; const ARight: TLux): TLumens; inline;
operator *(const ALeft: TLux; const ARight: TSquareMeters): TLumens; inline;
operator /(const ALeft: TLumens; const ARight: TLux): TSquareMeters; inline;
operator /(const ALeft: TLumens; const {%H-}ARight: TSquareMeterUnitId): TLux; inline;

type
  { Unit of Bequerel }
  TBequerelUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TBequerels = specialize TQuantity<THertzUnit>;
  TBequerelUnitId = specialize TUnitId<THertzUnit>;

var Bq: TBequerelUnitId;

const quettaBq: specialize TQuantity<THertzUnit> = (FValue: 1E+30);
const  ronnaBq: specialize TQuantity<THertzUnit> = (FValue: 1E+27);
const  yottaBq: specialize TQuantity<THertzUnit> = (FValue: 1E+24);
const  zettaBq: specialize TQuantity<THertzUnit> = (FValue: 1E+21);
const      EBq: specialize TQuantity<THertzUnit> = (FValue: 1E+18);
const   petaBq: specialize TQuantity<THertzUnit> = (FValue: 1E+15);
const      TBq: specialize TQuantity<THertzUnit> = (FValue: 1E+12);
const      GBq: specialize TQuantity<THertzUnit> = (FValue: 1E+09);
const   megaBq: specialize TQuantity<THertzUnit> = (FValue: 1E+06);
const      kBq: specialize TQuantity<THertzUnit> = (FValue: 1E+03);
const      hBq: specialize TQuantity<THertzUnit> = (FValue: 1E+02);
const     daBq: specialize TQuantity<THertzUnit> = (FValue: 1E+01);
const      dBq: specialize TQuantity<THertzUnit> = (FValue: 1E-01);
const      cBq: specialize TQuantity<THertzUnit> = (FValue: 1E-02);
const      mBq: specialize TQuantity<THertzUnit> = (FValue: 1E-03);
const     miBq: specialize TQuantity<THertzUnit> = (FValue: 1E-06);
const      nBq: specialize TQuantity<THertzUnit> = (FValue: 1E-09);
const      pBq: specialize TQuantity<THertzUnit> = (FValue: 1E-12);
const      fBq: specialize TQuantity<THertzUnit> = (FValue: 1E-15);
const      aBq: specialize TQuantity<THertzUnit> = (FValue: 1E-18);
const  zeptoBq: specialize TQuantity<THertzUnit> = (FValue: 1E-21);
const  yoctoBq: specialize TQuantity<THertzUnit> = (FValue: 1E-24);
const  rontoBq: specialize TQuantity<THertzUnit> = (FValue: 1E-27);
const quectoBq: specialize TQuantity<THertzUnit> = (FValue: 1E-30);

type
  { Unit of Gray }
  TGrayUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TGrays = specialize TQuantity<TSquareMeterPerSquareSecondUnit>;
  TGrayUnitId = specialize TUnitId<TSquareMeterPerSquareSecondUnit>;

var Gy: TGrayUnitId;

const quettaGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+30);
const  ronnaGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+27);
const  yottaGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+24);
const  zettaGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+21);
const      EGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+18);
const   petaGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+15);
const      TGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+12);
const      GGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+09);
const   megaGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+06);
const      kGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+03);
const      hGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+02);
const     daGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+01);
const      dGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-01);
const      cGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-02);
const      mGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-03);
const     miGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-06);
const      nGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-09);
const      pGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-12);
const      fGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-15);
const      aGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-18);
const  zeptoGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-21);
const  yoctoGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-24);
const  rontoGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-27);
const quectoGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-30);

type
  { Unit of Sievert }
  TSievertUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSieverts = specialize TQuantity<TSquareMeterPerSquareSecondUnit>;
  TSievertUnitId = specialize TUnitId<TSquareMeterPerSquareSecondUnit>;

var Sv: TSievertUnitId;

const quettaSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+30);
const  ronnaSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+27);
const  yottaSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+24);
const  zettaSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+21);
const      ESv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+18);
const   petaSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+15);
const      TSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+12);
const      GSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+09);
const   megaSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+06);
const      kSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+03);
const      hSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+02);
const     daSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+01);
const      dSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-01);
const      cSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-02);
const      mSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-03);
const     miSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-06);
const      nSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-09);
const      pSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-12);
const      fSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-15);
const      aSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-18);
const  zeptoSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-21);
const  yoctoSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-24);
const  rontoSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-27);
const quectoSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-30);

type
  { Unit of Katal }
  TKatalUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TKatals = specialize TQuantity<TKatalUnit>;
  TKatalUnitId = specialize TUnitId<TKatalUnit>;

var kat: TKatalUnitId;

const quettakat: specialize TQuantity<TKatalUnit> = (FValue: 1E+30);
const  ronnakat: specialize TQuantity<TKatalUnit> = (FValue: 1E+27);
const  yottakat: specialize TQuantity<TKatalUnit> = (FValue: 1E+24);
const  zettakat: specialize TQuantity<TKatalUnit> = (FValue: 1E+21);
const      Ekat: specialize TQuantity<TKatalUnit> = (FValue: 1E+18);
const   petakat: specialize TQuantity<TKatalUnit> = (FValue: 1E+15);
const      Tkat: specialize TQuantity<TKatalUnit> = (FValue: 1E+12);
const      Gkat: specialize TQuantity<TKatalUnit> = (FValue: 1E+09);
const   megakat: specialize TQuantity<TKatalUnit> = (FValue: 1E+06);
const      kkat: specialize TQuantity<TKatalUnit> = (FValue: 1E+03);
const      hkat: specialize TQuantity<TKatalUnit> = (FValue: 1E+02);
const     dakat: specialize TQuantity<TKatalUnit> = (FValue: 1E+01);
const      dkat: specialize TQuantity<TKatalUnit> = (FValue: 1E-01);
const      ckat: specialize TQuantity<TKatalUnit> = (FValue: 1E-02);
const      mkat: specialize TQuantity<TKatalUnit> = (FValue: 1E-03);
const     mikat: specialize TQuantity<TKatalUnit> = (FValue: 1E-06);
const      nkat: specialize TQuantity<TKatalUnit> = (FValue: 1E-09);
const      pkat: specialize TQuantity<TKatalUnit> = (FValue: 1E-12);
const      fkat: specialize TQuantity<TKatalUnit> = (FValue: 1E-15);
const      akat: specialize TQuantity<TKatalUnit> = (FValue: 1E-18);
const  zeptokat: specialize TQuantity<TKatalUnit> = (FValue: 1E-21);
const  yoctokat: specialize TQuantity<TKatalUnit> = (FValue: 1E-24);
const  rontokat: specialize TQuantity<TKatalUnit> = (FValue: 1E-27);
const quectokat: specialize TQuantity<TKatalUnit> = (FValue: 1E-30);

// main definition [ kat ] = [ mol ] / [ s ]
operator /(const ALeft: TMoles; const ARight: TSeconds): TKatals; inline;
operator *(const ALeft: TSeconds; const ARight: TKatals): TMoles; inline;
operator *(const ALeft: TKatals; const ARight: TSeconds): TMoles; inline;
operator /(const ALeft: TMoles; const ARight: TKatals): TSeconds; inline;
operator /(const ALeft: TMoles; const {%H-}ARight: TSecondUnitId): TKatals; inline;

type
  { Unit of NewtonMeter }
  TNewtonMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TNewtonMeters = specialize TQuantity<TJouleUnit>;
  TNewtonMeterUnitId = specialize TUnitId<TJouleUnit>;

type
  { Unit of JoulePerRadian }
  TJoulePerRadianUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TJoulesPerRadian = specialize TQuantity<TJoulePerRadianUnit>;
  TJoulePerRadianUnitId = specialize TUnitId<TJoulePerRadianUnit>;

// main definition [ J/rad ] = [ J ] / [ rad ]
operator /(const ALeft: TJoules; const ARight: TRadians): TJoulesPerRadian; inline;
operator *(const ALeft: TRadians; const ARight: TJoulesPerRadian): TJoules; inline;
operator *(const ALeft: TJoulesPerRadian; const ARight: TRadians): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerRadian): TRadians; inline;
operator /(const ALeft: TJoules; const {%H-}ARight: TRadianUnitId): TJoulesPerRadian; inline;

type
  { Unit of JoulePerDegree }
  TJoulePerDegreeUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
    const Factor = 180/Pi;
  end;
  TJoulesPerDegree = specialize TQuantity<TJoulePerRadianUnit>;
  TJoulePerDegreeUnitId = specialize TUnitId<TJoulePerDegreeUnit>;

type
  { Unit of NewtonMeterPerRadian }
  TNewtonMeterPerRadianUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TNewtonMetersPerRadian = specialize TQuantity<TJoulePerRadianUnit>;
  TNewtonMeterPerRadianUnitId = specialize TUnitId<TJoulePerRadianUnit>;

type
  { Unit of NewtonMeterPerDegree }
  TNewtonMeterPerDegreeUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
    const Factor = 180/Pi;
  end;
  TNewtonMetersPerDegree = specialize TQuantity<TJoulePerRadianUnit>;
  TNewtonMeterPerDegreeUnitId = specialize TUnitId<TNewtonMeterPerDegreeUnit>;

type
  { Unit of NewtonPerCubicMeter }
  TNewtonPerCubicMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TNewtonsPerCubicMeter = specialize TQuantity<TNewtonPerCubicMeterUnit>;
  TNewtonPerCubicMeterUnitId = specialize TUnitId<TNewtonPerCubicMeterUnit>;

// main definition [ N/m3 ] = [ N ] / [ m3 ]
operator /(const ALeft: TNewtons; const ARight: TCubicMeters): TNewtonsPerCubicMeter; inline;
operator *(const ALeft: TCubicMeters; const ARight: TNewtonsPerCubicMeter): TNewtons; inline;
operator *(const ALeft: TNewtonsPerCubicMeter; const ARight: TCubicMeters): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerCubicMeter): TCubicMeters; inline;
operator /(const ALeft: TNewtons; const {%H-}ARight: TCubicMeterUnitId): TNewtonsPerCubicMeter; inline;

// alternative definition [ N/m3 ] = [ Pa ] / [ m ]
operator /(const ALeft: TPascals; const ARight: TMeters): TNewtonsPerCubicMeter; inline;
operator *(const ALeft: TMeters; const ARight: TNewtonsPerCubicMeter): TPascals; inline;
operator *(const ALeft: TNewtonsPerCubicMeter; const ARight: TMeters): TPascals; inline;
operator /(const ALeft: TPascals; const ARight: TNewtonsPerCubicMeter): TMeters; inline;

// alternative definition [ N/m3 ] = [ kg/m3 ] * [ m/s2 ]
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TMetersPerSquareSecond): TNewtonsPerCubicMeter; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TKilogramsPerCubicMeter): TNewtonsPerCubicMeter; inline;
operator /(const ALeft: TNewtonsPerCubicMeter; const ARight: TKilogramsPerCubicMeter): TMetersPerSquareSecond; inline;
operator /(const ALeft: TNewtonsPerCubicMeter; const ARight: TMetersPerSquareSecond): TKilogramsPerCubicMeter; inline;

type
  { Unit of NewtonPerMeter }
  TNewtonPerMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TNewtonsPerMeter = specialize TQuantity<TNewtonPerMeterUnit>;
  TNewtonPerMeterUnitId = specialize TUnitId<TNewtonPerMeterUnit>;

// main definition [ N/m ] = [ N ] / [ m ]
operator /(const ALeft: TNewtons; const ARight: TMeters): TNewtonsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TNewtonsPerMeter): TNewtons; inline;
operator *(const ALeft: TNewtonsPerMeter; const ARight: TMeters): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerMeter): TMeters; inline;
operator /(const ALeft: TNewtons; const {%H-}ARight: TMeterUnitId): TNewtonsPerMeter; inline;

// alternative definition [ N/m ] = [ J ] / [ m2 ]
operator /(const ALeft: TJoules; const ARight: TSquareMeters): TNewtonsPerMeter; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerMeter): TJoules; inline;
operator *(const ALeft: TNewtonsPerMeter; const ARight: TSquareMeters): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TNewtonsPerMeter): TSquareMeters; inline;

// alternative definition [ N/m ] = [ Pa ] * [ m ]
operator *(const ALeft: TPascals; const ARight: TMeters): TNewtonsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TPascals): TNewtonsPerMeter; inline;
operator /(const ALeft: TNewtonsPerMeter; const ARight: TPascals): TMeters; inline;
operator /(const ALeft: TNewtonsPerMeter; const ARight: TMeters): TPascals; inline;

// alternative definition [ N/m ] = [ kg ] * [ Hz2 ]
operator *(const ALeft: TKilograms; const ARight: TSquareHertz): TNewtonsPerMeter; inline;
operator *(const ALeft: TSquareHertz; const ARight: TKilograms): TNewtonsPerMeter; inline;
operator /(const ALeft: TNewtonsPerMeter; const ARight: TKilograms): TSquareHertz; inline;
operator /(const ALeft: TNewtonsPerMeter; const ARight: TSquareHertz): TKilograms; inline;

type
  { Unit of CubicMeterPerSecond }
  TCubicMeterPerSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TCubicMetersPerSecond = specialize TQuantity<TCubicMeterPerSecondUnit>;
  TCubicMeterPerSecondUnitId = specialize TUnitId<TCubicMeterPerSecondUnit>;

// main definition [ m3/s ] = [ m3 ] / [ s ]
operator /(const ALeft: TCubicMeters; const ARight: TSeconds): TCubicMetersPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TCubicMetersPerSecond): TCubicMeters; inline;
operator *(const ALeft: TCubicMetersPerSecond; const ARight: TSeconds): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerSecond): TSeconds; inline;
operator /(const ALeft: TCubicMeters; const {%H-}ARight: TSecondUnitId): TCubicMetersPerSecond; inline;

// alternative definition [ m3/s ] = [ m2 ] * [ m/s ]
operator *(const ALeft: TSquareMeters; const ARight: TMetersPerSecond): TCubicMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TSquareMeters): TCubicMetersPerSecond; inline;
operator /(const ALeft: TCubicMetersPerSecond; const ARight: TSquareMeters): TMetersPerSecond; inline;
operator /(const ALeft: TCubicMetersPerSecond; const ARight: TMetersPerSecond): TSquareMeters; inline;

type
  { Unit of KilogramPerSecond }
  TKilogramPerSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TKilogramsPerSecond = specialize TQuantity<TKilogramPerSecondUnit>;
  TKilogramPerSecondUnitId = specialize TUnitId<TKilogramPerSecondUnit>;

// main definition [ kg/s ] = [ kg ] / [ s ]
operator /(const ALeft: TKilograms; const ARight: TSeconds): TKilogramsPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TKilogramsPerSecond): TKilograms; inline;
operator *(const ALeft: TKilogramsPerSecond; const ARight: TSeconds): TKilograms; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerSecond): TSeconds; inline;
operator /(const ALeft: TKilograms; const {%H-}ARight: TSecondUnitId): TKilogramsPerSecond; inline;

// alternative definition [ kg/s ] = [ N ] / [ m/s ]
operator /(const ALeft: TNewtons; const ARight: TMetersPerSecond): TKilogramsPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TKilogramsPerSecond): TNewtons; inline;
operator *(const ALeft: TKilogramsPerSecond; const ARight: TMetersPerSecond): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TKilogramsPerSecond): TMetersPerSecond; inline;

type
  { Unit of Poiseuille }
  TPoiseuilleUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TPoiseuilles = specialize TQuantity<TPoiseuilleUnit>;
  TPoiseuilleUnitId = specialize TUnitId<TPoiseuilleUnit>;

var Pl: TPoiseuilleUnitId;

const quettaPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E+30);
const  ronnaPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E+27);
const  yottaPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E+24);
const  zettaPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E+21);
const      EPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E+18);
const   petaPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E+15);
const      TPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E+12);
const      GPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E+09);
const   megaPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E+06);
const      kPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E+03);
const      hPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E+02);
const     daPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E+01);
const      dPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E-01);
const      cPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E-02);
const      mPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E-03);
const     miPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E-06);
const      nPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E-09);
const      pPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E-12);
const      fPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E-15);
const      aPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E-18);
const  zeptoPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E-21);
const  yoctoPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E-24);
const  rontoPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E-27);
const quectoPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E-30);

// main definition [ Pl ] = [ Pa ] * [ s ]
operator *(const ALeft: TPascals; const ARight: TSeconds): TPoiseuilles; inline;
operator *(const ALeft: TSeconds; const ARight: TPascals): TPoiseuilles; inline;
operator /(const ALeft: TPoiseuilles; const ARight: TPascals): TSeconds; inline;
operator /(const ALeft: TPoiseuilles; const ARight: TSeconds): TPascals; inline;
operator *(const ALeft: TPascals; const {%H-}ARight: TSecondUnitId): TPoiseuilles; inline;

// alternative definition [ Pl ] = [ kg/m2 ] * [ m/s ]
operator *(const ALeft: TKilogramsPerSquareMeter; const ARight: TMetersPerSecond): TPoiseuilles; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TKilogramsPerSquareMeter): TPoiseuilles; inline;
operator /(const ALeft: TPoiseuilles; const ARight: TKilogramsPerSquareMeter): TMetersPerSecond; inline;
operator /(const ALeft: TPoiseuilles; const ARight: TMetersPerSecond): TKilogramsPerSquareMeter; inline;

// alternative definition [ Pl ] = [ kg/s ] / [ m ]
operator /(const ALeft: TKilogramsPerSecond; const ARight: TMeters): TPoiseuilles; inline;
operator *(const ALeft: TMeters; const ARight: TPoiseuilles): TKilogramsPerSecond; inline;
operator *(const ALeft: TPoiseuilles; const ARight: TMeters): TKilogramsPerSecond; inline;
operator /(const ALeft: TKilogramsPerSecond; const ARight: TPoiseuilles): TMeters; inline;
operator *(const ALeft: TPoiseuilles; const {%H-}ARight: TMeterUnitId): TKilogramsPerSecond; inline;

type
  { Unit of PascalSecond }
  TPascalSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TPascalSeconds = specialize TQuantity<TPoiseuilleUnit>;
  TPascalSecondUnitId = specialize TUnitId<TPoiseuilleUnit>;

type
  { Unit of SquareMeterPerSecond }
  TSquareMeterPerSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSquareMetersPerSecond = specialize TQuantity<TSquareMeterPerSecondUnit>;
  TSquareMeterPerSecondUnitId = specialize TUnitId<TSquareMeterPerSecondUnit>;

// main definition [ m2/s ] = [ m2 ] / [ s ]
operator /(const ALeft: TSquareMeters; const ARight: TSeconds): TSquareMetersPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TSquareMetersPerSecond): TSquareMeters; inline;
operator *(const ALeft: TSquareMetersPerSecond; const ARight: TSeconds): TSquareMeters; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSecond): TSeconds; inline;
operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSecondUnitId): TSquareMetersPerSecond; inline;

// alternative definition [ m2/s ] = [ Pl ] / [ kg/m3 ]
operator /(const ALeft: TPoiseuilles; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSecond; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSecond): TPoiseuilles; inline;
operator *(const ALeft: TSquareMetersPerSecond; const ARight: TKilogramsPerCubicMeter): TPoiseuilles; inline;
operator /(const ALeft: TPoiseuilles; const ARight: TSquareMetersPerSecond): TKilogramsPerCubicMeter; inline;

type
  { Unit of KilogramPerQuarticMeter }
  TKilogramPerQuarticMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TKilogramsPerQuarticMeter = specialize TQuantity<TKilogramPerQuarticMeterUnit>;
  TKilogramPerQuarticMeterUnitId = specialize TUnitId<TKilogramPerQuarticMeterUnit>;

// main definition [ kg/m4 ] = [ kg ] / [ m4 ]
operator /(const ALeft: TKilograms; const ARight: TQuarticMeters): TKilogramsPerQuarticMeter; inline;
operator *(const ALeft: TQuarticMeters; const ARight: TKilogramsPerQuarticMeter): TKilograms; inline;
operator *(const ALeft: TKilogramsPerQuarticMeter; const ARight: TQuarticMeters): TKilograms; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerQuarticMeter): TQuarticMeters; inline;
operator /(const ALeft: TKilograms; const {%H-}ARight: TQuarticMeterUnitId): TKilogramsPerQuarticMeter; inline;

type
  { Unit of QuarticMeterSecond }
  TQuarticMeterSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TQuarticMeterSeconds = specialize TQuantity<TQuarticMeterSecondUnit>;
  TQuarticMeterSecondUnitId = specialize TUnitId<TQuarticMeterSecondUnit>;

// main definition [ m4*s ] = [ m4 ] * [ s ]
operator *(const ALeft: TQuarticMeters; const ARight: TSeconds): TQuarticMeterSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TQuarticMeters): TQuarticMeterSeconds; inline;
operator /(const ALeft: TQuarticMeterSeconds; const ARight: TQuarticMeters): TSeconds; inline;
operator /(const ALeft: TQuarticMeterSeconds; const ARight: TSeconds): TQuarticMeters; inline;
operator *(const ALeft: TQuarticMeters; const {%H-}ARight: TSecondUnitId): TQuarticMeterSeconds; inline;

type
  { Unit of KilogramPerQuarticMeterPerSecond }
  TKilogramPerQuarticMeterPerSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TKilogramsPerQuarticMeterPerSecond = specialize TQuantity<TKilogramPerQuarticMeterPerSecondUnit>;
  TKilogramPerQuarticMeterPerSecondUnitId = specialize TUnitId<TKilogramPerQuarticMeterPerSecondUnit>;

// main definition [ kg/m4/s ] = [ kg/s ] / [ m4 ]
operator /(const ALeft: TKilogramsPerSecond; const ARight: TQuarticMeters): TKilogramsPerQuarticMeterPerSecond; inline;
operator *(const ALeft: TQuarticMeters; const ARight: TKilogramsPerQuarticMeterPerSecond): TKilogramsPerSecond; inline;
operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TQuarticMeters): TKilogramsPerSecond; inline;
operator /(const ALeft: TKilogramsPerSecond; const ARight: TKilogramsPerQuarticMeterPerSecond): TQuarticMeters; inline;
operator /(const ALeft: TKilogramsPerSecond; const {%H-}ARight: TQuarticMeterUnitId): TKilogramsPerQuarticMeterPerSecond; inline;

// alternative definition [ kg/m4/s ] = [ kg/m4 ] / [ s ]
operator /(const ALeft: TKilogramsPerQuarticMeter; const ARight: TSeconds): TKilogramsPerQuarticMeterPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TKilogramsPerQuarticMeterPerSecond): TKilogramsPerQuarticMeter; inline;
operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TSeconds): TKilogramsPerQuarticMeter; inline;
operator /(const ALeft: TKilogramsPerQuarticMeter; const ARight: TKilogramsPerQuarticMeterPerSecond): TSeconds; inline;
operator /(const ALeft: TKilogramsPerQuarticMeter; const {%H-}ARight: TSecondUnitId): TKilogramsPerQuarticMeterPerSecond; inline;

// alternative definition [ kg/m4/s ] = [ kg ] / [ m4*s ]
operator /(const ALeft: TKilograms; const ARight: TQuarticMeterSeconds): TKilogramsPerQuarticMeterPerSecond; inline;
operator *(const ALeft: TQuarticMeterSeconds; const ARight: TKilogramsPerQuarticMeterPerSecond): TKilograms; inline;
operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TQuarticMeterSeconds): TKilograms; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerQuarticMeterPerSecond): TQuarticMeterSeconds; inline;

// alternative definition [ kg/m4/s ] = [ Pa ] / [ m3/s ]
operator /(const ALeft: TPascals; const ARight: TCubicMetersPerSecond): TKilogramsPerQuarticMeterPerSecond; inline;
operator *(const ALeft: TCubicMetersPerSecond; const ARight: TKilogramsPerQuarticMeterPerSecond): TPascals; inline;
operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TCubicMetersPerSecond): TPascals; inline;
operator /(const ALeft: TPascals; const ARight: TKilogramsPerQuarticMeterPerSecond): TCubicMetersPerSecond; inline;

type
  { Unit of CubicMeterPerKilogram }
  TCubicMeterPerKilogramUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TCubicMetersPerKilogram = specialize TQuantity<TCubicMeterPerKilogramUnit>;
  TCubicMeterPerKilogramUnitId = specialize TUnitId<TCubicMeterPerKilogramUnit>;

// main definition [ m3/kg ] = [ m3 ] / [ kg ]
operator /(const ALeft: TCubicMeters; const ARight: TKilograms): TCubicMetersPerKilogram; inline;
operator *(const ALeft: TKilograms; const ARight: TCubicMetersPerKilogram): TCubicMeters; inline;
operator *(const ALeft: TCubicMetersPerKilogram; const ARight: TKilograms): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerKilogram): TKilograms; inline;
operator /(const ALeft: TCubicMeters; const {%H-}ARight: TKilogramUnitId): TCubicMetersPerKilogram; inline;

// alternative definition [ m3/kg ] = 1 / [ kg/m3 ]
operator /(const ALeft: double; const ARight: TKilogramsPerCubicMeter): TCubicMetersPerKilogram; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TCubicMetersPerKilogram): double; inline;
operator *(const ALeft: TCubicMetersPerKilogram; const ARight: TKilogramsPerCubicMeter): double; inline;
operator /(const ALeft: double; const ARight: TCubicMetersPerKilogram): TKilogramsPerCubicMeter; inline;

type
  { Unit of KilogramSquareSecond }
  TKilogramSquareSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TKilogramSquareSeconds = specialize TQuantity<TKilogramSquareSecondUnit>;
  TKilogramSquareSecondUnitId = specialize TUnitId<TKilogramSquareSecondUnit>;

// main definition [ kg*s2 ] = [ kg ] * [ s2 ]
operator *(const ALeft: TKilograms; const ARight: TSquareSeconds): TKilogramSquareSeconds; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TKilograms): TKilogramSquareSeconds; inline;
operator /(const ALeft: TKilogramSquareSeconds; const ARight: TKilograms): TSquareSeconds; inline;
operator /(const ALeft: TKilogramSquareSeconds; const ARight: TSquareSeconds): TKilograms; inline;
operator *(const ALeft: TKilograms; const {%H-}ARight: TSquareSecondUnitId): TKilogramSquareSeconds; inline;

type
  { Unit of CubicMeterPerSquareSecond }
  TCubicMeterPerSquareSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TCubicMetersPerSquareSecond = specialize TQuantity<TCubicMeterPerSquareSecondUnit>;
  TCubicMeterPerSquareSecondUnitId = specialize TUnitId<TCubicMeterPerSquareSecondUnit>;

// main definitio [ m3/s2 ] = [ m3 ] / [ s2 ]
operator /(const ALeft: TCubicMeters; const ARight: TSquareSeconds): TCubicMetersPerSquareSecond; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TCubicMetersPerSquareSecond): TCubicMeters; inline;
operator *(const ALeft: TCubicMetersPerSquareSecond; const ARight: TSquareSeconds): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerSquareSecond): TSquareSeconds; inline;
operator /(const ALeft: TCubicMeters; const {%H-}ARight: TSquareSecondUnitId): TCubicMetersPerSquareSecond; inline;

// alternative definition [ m3/s2 ] = [ m/s2 ] * [ m2 ]
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TSquareMeters): TCubicMetersPerSquareSecond; inline;
operator *(const ALeft: TSquareMeters; const ARight: TMetersPerSquareSecond): TCubicMetersPerSquareSecond; inline;
operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TMetersPerSquareSecond): TSquareMeters; inline;
operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TSquareMeters): TMetersPerSquareSecond; inline;

type
  { Unit of NewtonSquareMeter }
  TNewtonSquareMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TNewtonSquareMeters = specialize TQuantity<TNewtonSquareMeterUnit>;
  TNewtonSquareMeterUnitId = specialize TUnitId<TNewtonSquareMeterUnit>;

// main definition [ N*m2 ] = [ N ] * [ m2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMeters): TNewtonSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtons): TNewtonSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtons): TSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareMeters): TNewtons; inline;
operator *(const ALeft: TNewtons; const {%H-}ARight: TSquareMeterUnitId): TNewtonSquareMeters; inline;

// alternative definition [ N*m2 ] = [ J ] * [ m ]
operator *(const ALeft: TJoules; const ARight: TMeters): TNewtonSquareMeters; inline;
operator *(const ALeft: TMeters; const ARight: TJoules): TNewtonSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TJoules): TMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TMeters): TJoules; inline;

// alternative definition [ N*m2 ] = [ Pa ] * [ m4 ]
operator *(const ALeft: TPascals; const ARight: TQuarticMeters): TNewtonSquareMeters; inline;
operator *(const ALeft: TQuarticMeters; const ARight: TPascals): TNewtonSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TPascals): TQuarticMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TQuarticMeters): TPascals; inline;

type
  { Unit of NewtonPerSquareKilogram }
  TNewtonPerSquareKilogramUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TNewtonsPerSquareKilogram = specialize TQuantity<TNewtonPerSquareKilogramUnit>;
  TNewtonPerSquareKilogramUnitId = specialize TUnitId<TNewtonPerSquareKilogramUnit>;

// main definition [ N/kg2 ] = [ N ] / [ kg2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareKilograms): TNewtonsPerSquareKilogram; inline;
operator *(const ALeft: TSquareKilograms; const ARight: TNewtonsPerSquareKilogram): TNewtons; inline;
operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareKilograms): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareKilogram): TSquareKilograms; inline;
operator /(const ALeft: TNewtons; const {%H-}ARight: TSquareKilogramUnitId): TNewtonsPerSquareKilogram; inline;

type
  { Unit of SquareKilogramPerMeter }
  TSquareKilogramPerMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSquareKilogramsPerMeter = specialize TQuantity<TSquareKilogramPerMeterUnit>;
  TSquareKilogramPerMeterUnitId = specialize TUnitId<TSquareKilogramPerMeterUnit>;

// main definition [ kg2/m ] = [ kg2 ] / [ m ]
operator /(const ALeft: TSquareKilograms; const ARight: TMeters): TSquareKilogramsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TSquareKilogramsPerMeter): TSquareKilograms; inline;
operator *(const ALeft: TSquareKilogramsPerMeter; const ARight: TMeters): TSquareKilograms; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerMeter): TMeters; inline;
operator /(const ALeft: TSquareKilograms; const {%H-}ARight: TMeterUnitId): TSquareKilogramsPerMeter; inline;

type
  { Unit of SquareKilogramPerSquareMeter }
  TSquareKilogramPerSquareMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSquareKilogramsPerSquareMeter = specialize TQuantity<TSquareKilogramPerSquareMeterUnit>;
  TSquareKilogramPerSquareMeterUnitId = specialize TUnitId<TSquareKilogramPerSquareMeterUnit>;

// main definition [ kg2/m2 ] = [ kg2 ] / [ m2 ]
operator /(const ALeft: TSquareKilograms; const ARight: TSquareMeters): TSquareKilogramsPerSquareMeter; inline;
operator *(const ALeft: TSquareMeters; const ARight: TSquareKilogramsPerSquareMeter): TSquareKilograms; inline;
operator *(const ALeft: TSquareKilogramsPerSquareMeter; const ARight: TSquareMeters): TSquareKilograms; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerSquareMeter): TSquareMeters; inline;
operator /(const ALeft: TSquareKilograms; const {%H-}ARight: TSquareMeterUnitId): TSquareKilogramsPerSquareMeter; inline;

type
  { Unit of SquareMeterPerSquareKilogram }
  TSquareMeterPerSquareKilogramUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSquareMetersPerSquareKilogram = specialize TQuantity<TSquareMeterPerSquareKilogramUnit>;
  TSquareMeterPerSquareKilogramUnitId = specialize TUnitId<TSquareMeterPerSquareKilogramUnit>;

// main definition [ m2/kg2 ] = [ m2 ] / [ kg2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareKilograms): TSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TSquareKilograms; const ARight: TSquareMetersPerSquareKilogram): TSquareMeters; inline;
operator *(const ALeft: TSquareMetersPerSquareKilogram; const ARight: TSquareKilograms): TSquareMeters; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareKilogram): TSquareKilograms; inline;
operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSquareKilogramUnitId): TSquareMetersPerSquareKilogram; inline;

type
  { Unit of NewtonSquareMeterPerSquareKilogram }
  TNewtonSquareMeterPerSquareKilogramUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TNewtonSquareMetersPerSquareKilogram = specialize TQuantity<TNewtonSquareMeterPerSquareKilogramUnit>;
  TNewtonSquareMeterPerSquareKilogramUnitId = specialize TUnitId<TNewtonSquareMeterPerSquareKilogramUnit>;

// main definition [ N*m2/kg2 ] = [ N ] * [ m2/kg2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareKilogram): TNewtonSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TSquareMetersPerSquareKilogram; const ARight: TNewtons): TNewtonSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TNewtons): TSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareMetersPerSquareKilogram): TNewtons; inline;

// alternative definition [ N*m2/kg2 ] = [ N ] / [ kg2/m2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareKilogramsPerSquareMeter): TNewtonSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TSquareKilogramsPerSquareMeter; const ARight: TNewtonSquareMetersPerSquareKilogram): TNewtons; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilogramsPerSquareMeter): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilogramsPerSquareMeter; inline;

// alternative definition [ N*m2/kg2 ] = [ N*m2 ] / [ kg2 ]
operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareKilograms): TNewtonSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TSquareKilograms; const ARight: TNewtonSquareMetersPerSquareKilogram): TNewtonSquareMeters; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilograms): TNewtonSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilograms; inline;
operator /(const ALeft: TNewtonSquareMeters; const {%H-}ARight: TSquareKilogramUnitId): TNewtonSquareMetersPerSquareKilogram; inline;

// alternative definition [ N*m2/kg2 ] = [ N/kg2 ] * [ m2 ]
operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareMeters): TNewtonSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareKilogram): TNewtonSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TNewtonsPerSquareKilogram): TSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareMeters): TNewtonsPerSquareKilogram; inline;
operator *(const ALeft: TNewtonsPerSquareKilogram; const {%H-}ARight: TSquareMeterUnitId): TNewtonSquareMetersPerSquareKilogram; inline;

// alternative definition [ N*m2/kg2 ] = [ J ] / [ kg2/m ]
operator /(const ALeft: TJoules; const ARight: TSquareKilogramsPerMeter): TNewtonSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TSquareKilogramsPerMeter; const ARight: TNewtonSquareMetersPerSquareKilogram): TJoules; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilogramsPerMeter): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilogramsPerMeter; inline;

// alternative definition [ N*m2/kg2 ] = [ m3/kg ] / [ s2 ]
operator /(const ALeft: TCubicMetersPerKilogram; const ARight: TSquareSeconds): TNewtonSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TNewtonSquareMetersPerSquareKilogram): TCubicMetersPerKilogram; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareSeconds): TCubicMetersPerKilogram; inline;
operator /(const ALeft: TCubicMetersPerKilogram; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareSeconds; inline;

// alternative definition [ N*m2/kg2 ] = [ m3 ] / [ kg*s2 ]
operator /(const ALeft: TCubicMeters; const ARight: TKilogramSquareSeconds): TNewtonSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TKilogramSquareSeconds; const ARight: TNewtonSquareMetersPerSquareKilogram): TCubicMeters; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TKilogramSquareSeconds): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TNewtonSquareMetersPerSquareKilogram): TKilogramSquareSeconds; inline;

// alternative definition [ N*m2/kg2 ] = [ m3/s2 ] / [ kg ]
operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TKilograms): TNewtonSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TKilograms; const ARight: TNewtonSquareMetersPerSquareKilogram): TCubicMetersPerSquareSecond; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TKilograms): TCubicMetersPerSquareSecond; inline;
operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TNewtonSquareMetersPerSquareKilogram): TKilograms; inline;

type
  { Unit of ReciprocalKelvin }
  TReciprocalKelvinUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TReciprocalKelvins = specialize TQuantity<TReciprocalKelvinUnit>;
  TReciprocalKelvinUnitId = specialize TUnitId<TReciprocalKelvinUnit>;

// main definition [ 1/K ] = 1 / [ K ]
operator /(const ALeft: double; const ARight: TKelvins): TReciprocalKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TReciprocalKelvins): double; inline;
operator *(const ALeft: TReciprocalKelvins; const ARight: TKelvins): double; inline;
operator /(const ALeft: double; const ARight: TReciprocalKelvins): TKelvins; inline;
operator /(const ALeft: double; const {%H-}ARight: TKelvinUnitId): TReciprocalKelvins; inline;

type
  { Unit of KilogramKelvin }
  TKilogramKelvinUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TKilogramKelvins = specialize TQuantity<TKilogramKelvinUnit>;
  TKilogramKelvinUnitId = specialize TUnitId<TKilogramKelvinUnit>;

// main definition [ kg*K] = [ kg ] * [ K ]
operator *(const ALeft: TKilograms; const ARight: TKelvins): TKilogramKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TKilograms): TKilogramKelvins; inline;
operator /(const ALeft: TKilogramKelvins; const ARight: TKilograms): TKelvins; inline;
operator /(const ALeft: TKilogramKelvins; const ARight: TKelvins): TKilograms; inline;
operator *(const ALeft: TKilograms; const {%H-}ARight: TKelvinUnitId): TKilogramKelvins; inline;

type
  { Unit of JoulePerKelvin }
  TJoulePerKelvinUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TJoulesPerKelvin = specialize TQuantity<TJoulePerKelvinUnit>;
  TJoulePerKelvinUnitId = specialize TUnitId<TJoulePerKelvinUnit>;

// main definition [ J/K ] = [ J ] / [ K ]
operator /(const ALeft: TJoules; const ARight: TKelvins): TJoulesPerKelvin; inline;
operator *(const ALeft: TKelvins; const ARight: TJoulesPerKelvin): TJoules; inline;
operator *(const ALeft: TJoulesPerKelvin; const ARight: TKelvins): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerKelvin): TKelvins; inline;
operator /(const ALeft: TJoules; const {%H-}ARight: TKelvinUnitId): TJoulesPerKelvin; inline;

type
  { Unit of JoulePerKilogram }
  TJoulePerKilogramUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TJoulesPerKilogram = specialize TQuantity<TSquareMeterPerSquareKilogramUnit>;
  TJoulePerKilogramUnitId = specialize TUnitId<TSquareMeterPerSquareKilogramUnit>;

type
  { Unit of JoulePerKilogramPerKelvin }
  TJoulePerKilogramPerKelvinUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TJoulesPerKilogramPerKelvin = specialize TQuantity<TJoulePerKilogramPerKelvinUnit>;
  TJoulePerKilogramPerKelvinUnitId = specialize TUnitId<TJoulePerKilogramPerKelvinUnit>;

// main definition [ J/kg/K ] = [ J ] / [ kg*K ]
operator /(const ALeft: TJoules; const ARight: TKilogramKelvins): TJoulesPerKilogramPerKelvin; inline;
operator *(const ALeft: TKilogramKelvins; const ARight: TJoulesPerKilogramPerKelvin): TJoules; inline;
operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKilogramKelvins): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerKilogramPerKelvin): TKilogramKelvins; inline;

// alternative definition [ J/kg/K ] = [ J/kg ] / [ K ]
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKelvins): TJoulesPerKilogramPerKelvin; inline;
operator *(const ALeft: TKelvins; const ARight: TJoulesPerKilogramPerKelvin): TSquareMetersPerSquareSecond; inline;
operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKelvins): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TJoulesPerKilogramPerKelvin): TKelvins; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const {%H-}ARight: TKelvinUnitId): TJoulesPerKilogramPerKelvin; inline;

// alternative definition [ J/kg/K ] = [ J/K ] / [ kg ]
operator /(const ALeft: TJoulesPerKelvin; const ARight: TKilograms): TJoulesPerKilogramPerKelvin; inline;
operator *(const ALeft: TKilograms; const ARight: TJoulesPerKilogramPerKelvin): TJoulesPerKelvin; inline;
operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKilograms): TJoulesPerKelvin; inline;
operator /(const ALeft: TJoulesPerKelvin; const ARight: TJoulesPerKilogramPerKelvin): TKilograms; inline;
operator /(const ALeft: TJoulesPerKelvin; const {%H-}ARight: TKilogramUnitId): TJoulesPerKilogramPerKelvin; inline;

type
  { Unit of MeterKelvin }
  TMeterKelvinUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TMeterKelvins = specialize TQuantity<TMeterKelvinUnit>;
  TMeterKelvinUnitId = specialize TUnitId<TMeterKelvinUnit>;

// main definition [ m*K ] = [ m ] * [ K ]
operator *(const ALeft: TMeters; const ARight: TKelvins): TMeterKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TMeters): TMeterKelvins; inline;
operator /(const ALeft: TMeterKelvins; const ARight: TMeters): TKelvins; inline;
operator /(const ALeft: TMeterKelvins; const ARight: TKelvins): TMeters; inline;
operator *(const ALeft: TMeters; const {%H-}ARight: TKelvinUnitId): TMeterKelvins; inline;

type
  { Unit of KelvinPerMeter }
  TKelvinPerMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TKelvinsPerMeter = specialize TQuantity<TKelvinPerMeterUnit>;
  TKelvinPerMeterUnitId = specialize TUnitId<TKelvinPerMeterUnit>;

// main definition [ K/m ] = [ K ] / [ m ]
operator /(const ALeft: TKelvins; const ARight: TMeters): TKelvinsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TKelvinsPerMeter): TKelvins; inline;
operator *(const ALeft: TKelvinsPerMeter; const ARight: TMeters): TKelvins; inline;
operator /(const ALeft: TKelvins; const ARight: TKelvinsPerMeter): TMeters; inline;
operator /(const ALeft: TKelvins; const {%H-}ARight: TMeterUnitId): TKelvinsPerMeter; inline;

type
  { Unit of WattPerMeter }
  TWattPerMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TWattsPerMeter = specialize TQuantity<TWattPerMeterUnit>;
  TWattPerMeterUnitId = specialize TUnitId<TWattPerMeterUnit>;

// main definition [ W/m ] = [ W ] / [ m ]
operator /(const ALeft: TWatts; const ARight: TMeters): TWattsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TWattsPerMeter): TWatts; inline;
operator *(const ALeft: TWattsPerMeter; const ARight: TMeters): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerMeter): TMeters; inline;
operator /(const ALeft: TWatts; const {%H-}ARight: TMeterUnitId): TWattsPerMeter; inline;

type
  { Unit of WattPerSquareMeter }
  TWattPerSquareMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TWattsPerSquareMeter = specialize TQuantity<TWattPerSquareMeterUnit>;
  TWattPerSquareMeterUnitId = specialize TUnitId<TWattPerSquareMeterUnit>;

// main definition [ W/m2 ] = [ W ] / [ m2 ]
operator /(const ALeft: TWatts; const ARight: TSquareMeters): TWattsPerSquareMeter; inline;
operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeter): TWatts; inline;
operator *(const ALeft: TWattsPerSquareMeter; const ARight: TSquareMeters): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeter): TSquareMeters; inline;
operator /(const ALeft: TWatts; const {%H-}ARight: TSquareMeterUnitId): TWattsPerSquareMeter; inline;

type
  { Unit of WattPerKelvin }
  TWattPerKelvinUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TWattsPerKelvin = specialize TQuantity<TWattPerKelvinUnit>;
  TWattPerKelvinUnitId = specialize TUnitId<TWattPerKelvinUnit>;

// main definition [ W/K ] = [ W ] / [ K ]
operator /(const ALeft: TWatts; const ARight: TKelvins): TWattsPerKelvin; inline;
operator *(const ALeft: TKelvins; const ARight: TWattsPerKelvin): TWatts; inline;
operator *(const ALeft: TWattsPerKelvin; const ARight: TKelvins): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerKelvin): TKelvins; inline;
operator /(const ALeft: TWatts; const {%H-}ARight: TKelvinUnitId): TWattsPerKelvin; inline;

type
  { Unit of WattPerMeterPerKelvin }
  TWattPerMeterPerKelvinUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TWattsPerMeterPerKelvin = specialize TQuantity<TWattPerMeterPerKelvinUnit>;
  TWattPerMeterPerKelvinUnitId = specialize TUnitId<TWattPerMeterPerKelvinUnit>;

// main definition [ W/m/K ] = [ W ] / [ m*K ]
operator /(const ALeft: TWatts; const ARight: TMeterKelvins): TWattsPerMeterPerKelvin; inline;
operator *(const ALeft: TMeterKelvins; const ARight: TWattsPerMeterPerKelvin): TWatts; inline;
operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TMeterKelvins): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerMeterPerKelvin): TMeterKelvins; inline;

// alternative definition [ W/m/K ] = [ W/m ] / [ K ]
operator /(const ALeft: TWattsPerMeter; const ARight: TKelvins): TWattsPerMeterPerKelvin; inline;
operator *(const ALeft: TKelvins; const ARight: TWattsPerMeterPerKelvin): TWattsPerMeter; inline;
operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TKelvins): TWattsPerMeter; inline;
operator /(const ALeft: TWattsPerMeter; const ARight: TWattsPerMeterPerKelvin): TKelvins; inline;
operator /(const ALeft: TWattsPerMeter; const {%H-}ARight: TKelvinUnitId): TWattsPerMeterPerKelvin; inline;

// alternative definition [ W/m/K ] = [ W/K ] / [ m ]
operator /(const ALeft: TWattsPerKelvin; const ARight: TMeters): TWattsPerMeterPerKelvin; inline;
operator *(const ALeft: TMeters; const ARight: TWattsPerMeterPerKelvin): TWattsPerKelvin; inline;
operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TMeters): TWattsPerKelvin; inline;
operator /(const ALeft: TWattsPerKelvin; const ARight: TWattsPerMeterPerKelvin): TMeters; inline;
operator /(const ALeft: TWattsPerKelvin; const {%H-}ARight: TMeterUnitId): TWattsPerMeterPerKelvin; inline;

// alternative definition [ W/m/K ] = [ W/m2 ] / [ K/m ]
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TKelvinsPerMeter): TWattsPerMeterPerKelvin; inline;
operator *(const ALeft: TKelvinsPerMeter; const ARight: TWattsPerMeterPerKelvin): TWattsPerSquareMeter; inline;
operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TKelvinsPerMeter): TWattsPerSquareMeter; inline;
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerMeterPerKelvin): TKelvinsPerMeter; inline;

type
  { Unit of SquareMeterKelvin }
  TSquareMeterKelvinUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSquareMeterKelvins = specialize TQuantity<TSquareMeterKelvinUnit>;
  TSquareMeterKelvinUnitId = specialize TUnitId<TSquareMeterKelvinUnit>;

// main definition [ m2*K ] = [ m2 ] * [ K ]
operator *(const ALeft: TSquareMeters; const ARight: TKelvins): TSquareMeterKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TSquareMeters): TSquareMeterKelvins; inline;
operator /(const ALeft: TSquareMeterKelvins; const ARight: TSquareMeters): TKelvins; inline;
operator /(const ALeft: TSquareMeterKelvins; const ARight: TKelvins): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const {%H-}ARight: TKelvinUnitId): TSquareMeterKelvins; inline;

type
  { Unit of WattPerSquareMeterPerKelvin }
  TWattPerSquareMeterPerKelvinUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TWattsPerSquareMeterPerKelvin = specialize TQuantity<TWattPerSquareMeterPerKelvinUnit>;
  TWattPerSquareMeterPerKelvinUnitId = specialize TUnitId<TWattPerSquareMeterPerKelvinUnit>;

// main definition [ W/m2/K ] = [ W ] / [ m2*K ]
operator /(const ALeft: TWatts; const ARight: TSquareMeterKelvins): TWattsPerSquareMeterPerKelvin; inline;
operator *(const ALeft: TSquareMeterKelvins; const ARight: TWattsPerSquareMeterPerKelvin): TWatts; inline;
operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TSquareMeterKelvins): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerKelvin): TSquareMeterKelvins; inline;

// alternative definition [ W/m2/K ] = [ W/m2 ] / [ K ]
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TKelvins): TWattsPerSquareMeterPerKelvin; inline;
operator *(const ALeft: TKelvins; const ARight: TWattsPerSquareMeterPerKelvin): TWattsPerSquareMeter; inline;
operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TKelvins): TWattsPerSquareMeter; inline;
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerSquareMeterPerKelvin): TKelvins; inline;
operator /(const ALeft: TWattsPerSquareMeter; const {%H-}ARight: TKelvinUnitId): TWattsPerSquareMeterPerKelvin; inline;

// alternative definition [ W/m2/K ] = [ W/K ] / [ m2 ]
operator /(const ALeft: TWattsPerKelvin; const ARight: TSquareMeters): TWattsPerSquareMeterPerKelvin; inline;
operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerKelvin): TWattsPerKelvin; inline;
operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TSquareMeters): TWattsPerKelvin; inline;
operator /(const ALeft: TWattsPerKelvin; const ARight: TWattsPerSquareMeterPerKelvin): TSquareMeters; inline;
operator /(const ALeft: TWattsPerKelvin; const {%H-}ARight: TSquareMeterUnitId): TWattsPerSquareMeterPerKelvin; inline;

type
  { Unit of SquareMeterQuarticKelvin }
  TSquareMeterQuarticKelvinUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSquareMeterQuarticKelvins = specialize TQuantity<TSquareMeterQuarticKelvinUnit>;
  TSquareMeterQuarticKelvinUnitId = specialize TUnitId<TSquareMeterQuarticKelvinUnit>;

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]
operator *(const ALeft: TSquareMeters; const ARight: TQuarticKelvins): TSquareMeterQuarticKelvins; inline;
operator *(const ALeft: TQuarticKelvins; const ARight: TSquareMeters): TSquareMeterQuarticKelvins; inline;
operator /(const ALeft: TSquareMeterQuarticKelvins; const ARight: TSquareMeters): TQuarticKelvins; inline;
operator /(const ALeft: TSquareMeterQuarticKelvins; const ARight: TQuarticKelvins): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const {%H-}ARight: TQuarticKelvinUnitId): TSquareMeterQuarticKelvins; inline;

type
  { Unit of WattPerQuarticKelvin }
  TWattPerQuarticKelvinUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TWattsPerQuarticKelvin = specialize TQuantity<TWattPerQuarticKelvinUnit>;
  TWattPerQuarticKelvinUnitId = specialize TUnitId<TWattPerQuarticKelvinUnit>;

// main definition [ W/K4 ] = [ W ] / [ K4 ]
operator /(const ALeft: TWatts; const ARight: TQuarticKelvins): TWattsPerQuarticKelvin; inline;
operator *(const ALeft: TQuarticKelvins; const ARight: TWattsPerQuarticKelvin): TWatts; inline;
operator *(const ALeft: TWattsPerQuarticKelvin; const ARight: TQuarticKelvins): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerQuarticKelvin): TQuarticKelvins; inline;
operator /(const ALeft: TWatts; const {%H-}ARight: TQuarticKelvinUnitId): TWattsPerQuarticKelvin; inline;

type
  { Unit of WattPerSquareMeterPerQuarticKelvin }
  TWattPerSquareMeterPerQuarticKelvinUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TWattsPerSquareMeterPerQuarticKelvin = specialize TQuantity<TWattPerSquareMeterPerQuarticKelvinUnit>;
  TWattPerSquareMeterPerQuarticKelvinUnitId = specialize TUnitId<TWattPerSquareMeterPerQuarticKelvinUnit>;

// main definition [ W/m2/K4 ] = [ W ] / [ m2*K4 ]
operator /(const ALeft: TWatts; const ARight: TSquareMeterQuarticKelvins): TWattsPerSquareMeterPerQuarticKelvin; inline;
operator *(const ALeft: TSquareMeterQuarticKelvins; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWatts; inline;
operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TSquareMeterQuarticKelvins): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TSquareMeterQuarticKelvins; inline;

// alternative definition [ W/m2/K4 ] = [ W/m2 ] / [ K4 ]
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TQuarticKelvins): TWattsPerSquareMeterPerQuarticKelvin; inline;
operator *(const ALeft: TQuarticKelvins; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWattsPerSquareMeter; inline;
operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TQuarticKelvins): TWattsPerSquareMeter; inline;
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TQuarticKelvins; inline;
operator /(const ALeft: TWattsPerSquareMeter; const {%H-}ARight: TQuarticKelvinUnitId): TWattsPerSquareMeterPerQuarticKelvin; inline;

// alternative definition [ W/m2/K4 ] = [ W/K4 ] / [ m2 ]
operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TSquareMeters): TWattsPerSquareMeterPerQuarticKelvin; inline;
operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWattsPerQuarticKelvin; inline;
operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TSquareMeters): TWattsPerQuarticKelvin; inline;
operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TSquareMeters; inline;
operator /(const ALeft: TWattsPerQuarticKelvin; const {%H-}ARight: TSquareMeterUnitId): TWattsPerSquareMeterPerQuarticKelvin; inline;

type
  { Unit of JoulePerMole }
  TJoulePerMoleUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TJoulesPerMole = specialize TQuantity<TJoulePerMoleUnit>;
  TJoulePerMoleUnitId = specialize TUnitId<TJoulePerMoleUnit>;

// main definition [ J/mol ] = [ J ] / [ mol ]
operator /(const ALeft: TJoules; const ARight: TMoles): TJoulesPerMole; inline;
operator *(const ALeft: TMoles; const ARight: TJoulesPerMole): TJoules; inline;
operator *(const ALeft: TJoulesPerMole; const ARight: TMoles): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerMole): TMoles; inline;
operator /(const ALeft: TJoules; const {%H-}ARight: TMoleUnitId): TJoulesPerMole; inline;

type
  { Unit of MoleKelvin }
  TMoleKelvinUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TMoleKelvins = specialize TQuantity<TMoleKelvinUnit>;
  TMoleKelvinUnitId = specialize TUnitId<TMoleKelvinUnit>;

// main definition [ mol*K ] = [ mol ] * [ K ]
operator *(const ALeft: TMoles; const ARight: TKelvins): TMoleKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TMoles): TMoleKelvins; inline;
operator /(const ALeft: TMoleKelvins; const ARight: TMoles): TKelvins; inline;
operator /(const ALeft: TMoleKelvins; const ARight: TKelvins): TMoles; inline;
operator *(const ALeft: TMoles; const {%H-}ARight: TKelvinUnitId): TMoleKelvins; inline;

type
  { Unit of JoulePerMolePerKelvin }
  TJoulePerMolePerKelvinUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TJoulesPerMolePerKelvin = specialize TQuantity<TJoulePerMolePerKelvinUnit>;
  TJoulePerMolePerKelvinUnitId = specialize TUnitId<TJoulePerMolePerKelvinUnit>;

// main definition [ J/mol/K ] = [ J ] / [ mol * K ]
operator /(const ALeft: TJoules; const ARight: TMoleKelvins): TJoulesPerMolePerKelvin; inline;
operator *(const ALeft: TMoleKelvins; const ARight: TJoulesPerMolePerKelvin): TJoules; inline;
operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TMoleKelvins): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerMolePerKelvin): TMoleKelvins; inline;

// alternative definition [ J/mol/K ] = [ J/K ] / [ mol ]
operator /(const ALeft: TJoulesPerKelvin; const ARight: TMoles): TJoulesPerMolePerKelvin; inline;
operator *(const ALeft: TMoles; const ARight: TJoulesPerMolePerKelvin): TJoulesPerKelvin; inline;
operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TMoles): TJoulesPerKelvin; inline;
operator /(const ALeft: TJoulesPerKelvin; const ARight: TJoulesPerMolePerKelvin): TMoles; inline;
operator /(const ALeft: TJoulesPerKelvin; const {%H-}ARight: TMoleUnitId): TJoulesPerMolePerKelvin; inline;

// alternative definition [ J/mol/K ] = [ J/mol ] / [ K ]
operator /(const ALeft: TJoulesPerMole; const ARight: TKelvins): TJoulesPerMolePerKelvin; inline;
operator *(const ALeft: TKelvins; const ARight: TJoulesPerMolePerKelvin): TJoulesPerMole; inline;
operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TKelvins): TJoulesPerMole; inline;
operator /(const ALeft: TJoulesPerMole; const ARight: TJoulesPerMolePerKelvin): TKelvins; inline;
operator /(const ALeft: TJoulesPerMole; const {%H-}ARight: TKelvinUnitId): TJoulesPerMolePerKelvin; inline;

type
  { Unit of OhmMeter }
  TOhmMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TOhmMeters = specialize TQuantity<TOhmMeterUnit>;
  TOhmMeterUnitId = specialize TUnitId<TOhmMeterUnit>;

// main definition [ Ω*m ] = [ Ω ] * [ m ]
operator *(const ALeft: TOhms; const ARight: TMeters): TOhmMeters; inline;
operator *(const ALeft: TMeters; const ARight: TOhms): TOhmMeters; inline;
operator /(const ALeft: TOhmMeters; const ARight: TOhms): TMeters; inline;
operator /(const ALeft: TOhmMeters; const ARight: TMeters): TOhms; inline;
operator *(const ALeft: TOhms; const {%H-}ARight: TMeterUnitId): TOhmMeters; inline;

type
  { Unit of VoltPerMeter }
  TVoltPerMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TVoltsPerMeter = specialize TQuantity<TVoltPerMeterUnit>;
  TVoltPerMeterUnitId = specialize TUnitId<TVoltPerMeterUnit>;

// main definition [ V/m ] = [ V ] / [ m ]
operator /(const ALeft: TVolts; const ARight: TMeters): TVoltsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TVoltsPerMeter): TVolts; inline;
operator *(const ALeft: TVoltsPerMeter; const ARight: TMeters): TVolts; inline;
operator /(const ALeft: TVolts; const ARight: TVoltsPerMeter): TMeters; inline;
operator /(const ALeft: TVolts; const {%H-}ARight: TMeterUnitId): TVoltsPerMeter; inline;

// alternative definition [ V/m ] = [ N ] / [ C ]
operator /(const ALeft: TNewtons; const ARight: TCoulombs): TVoltsPerMeter; inline;
operator *(const ALeft: TCoulombs; const ARight: TVoltsPerMeter): TNewtons; inline;
operator *(const ALeft: TVoltsPerMeter; const ARight: TCoulombs): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TVoltsPerMeter): TCoulombs; inline;
operator /(const ALeft: TNewtons; const {%H-}ARight: TCoulombUnitId): TVoltsPerMeter; inline;

// alternative definition [ V/m ] = [ T ] * [ m/s ]
operator *(const ALeft: TTeslas; const ARight: TMetersPerSecond): TVoltsPerMeter; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TTeslas): TVoltsPerMeter; inline;
operator /(const ALeft: TVoltsPerMeter; const ARight: TTeslas): TMetersPerSecond; inline;
operator /(const ALeft: TVoltsPerMeter; const ARight: TMetersPerSecond): TTeslas; inline;

type
  { Unit of NewtonPerCoulomb }
  TNewtonPerCoulombUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TNewtonsPerCoulomb = specialize TQuantity<TVoltPerMeterUnit>;
  TNewtonPerCoulombUnitId = specialize TUnitId<TVoltPerMeterUnit>;

type
  { Unit of CoulombPerMeter }
  TCoulombPerMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TCoulombsPerMeter = specialize TQuantity<TCoulombPerMeterUnit>;
  TCoulombPerMeterUnitId = specialize TUnitId<TCoulombPerMeterUnit>;

// main definition [ C/m ] = [ C ] / [ m ]
operator /(const ALeft: TCoulombs; const ARight: TMeters): TCoulombsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TCoulombsPerMeter): TCoulombs; inline;
operator *(const ALeft: TCoulombsPerMeter; const ARight: TMeters): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerMeter): TMeters; inline;
operator /(const ALeft: TCoulombs; const {%H-}ARight: TMeterUnitId): TCoulombsPerMeter; inline;

type
  { Unit of SquareCoulombPerMeter }
  TSquareCoulombPerMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSquareCoulombsPerMeter = specialize TQuantity<TSquareCoulombPerMeterUnit>;
  TSquareCoulombPerMeterUnitId = specialize TUnitId<TSquareCoulombPerMeterUnit>;

// main definition [ C2/m ] = [ C2 ] / [ m ]
operator /(const ALeft: TSquareCoulombs; const ARight: TMeters): TSquareCoulombsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TSquareCoulombsPerMeter): TSquareCoulombs; inline;
operator *(const ALeft: TSquareCoulombsPerMeter; const ARight: TMeters): TSquareCoulombs; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TSquareCoulombsPerMeter): TMeters; inline;
operator /(const ALeft: TSquareCoulombs; const {%H-}ARight: TMeterUnitId): TSquareCoulombsPerMeter; inline;

// alternative definition [ C2/m ] = [ C/m ] * [ C ]
operator *(const ALeft: TCoulombsPerMeter; const ARight: TCoulombs): TSquareCoulombsPerMeter; inline;
operator *(const ALeft: TCoulombs; const ARight: TCoulombsPerMeter): TSquareCoulombsPerMeter; inline;
operator /(const ALeft: TSquareCoulombsPerMeter; const ARight: TCoulombsPerMeter): TCoulombs; inline;
operator /(const ALeft: TSquareCoulombsPerMeter; const ARight: TCoulombs): TCoulombsPerMeter; inline;

type
  { Unit of CoulombPerSquareMeter }
  TCoulombPerSquareMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TCoulombsPerSquareMeter = specialize TQuantity<TCoulombPerSquareMeterUnit>;
  TCoulombPerSquareMeterUnitId = specialize TUnitId<TCoulombPerSquareMeterUnit>;

// main definition [ C/m2 ] = [ C ] / [ m2 ]
operator /(const ALeft: TCoulombs; const ARight: TSquareMeters): TCoulombsPerSquareMeter; inline;
operator *(const ALeft: TSquareMeters; const ARight: TCoulombsPerSquareMeter): TCoulombs; inline;
operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TSquareMeters): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerSquareMeter): TSquareMeters; inline;
operator /(const ALeft: TCoulombs; const {%H-}ARight: TSquareMeterUnitId): TCoulombsPerSquareMeter; inline;

// alternative definition [ C/m2 ] = [ C/m ] / [ m ]
operator /(const ALeft: TCoulombsPerMeter; const ARight: TMeters): TCoulombsPerSquareMeter; inline;
operator *(const ALeft: TMeters; const ARight: TCoulombsPerSquareMeter): TCoulombsPerMeter; inline;
operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TMeters): TCoulombsPerMeter; inline;
operator /(const ALeft: TCoulombsPerMeter; const ARight: TCoulombsPerSquareMeter): TMeters; inline;

type
  { Unit of SquareMeterPerSquareCoulomb }
  TSquareMeterPerSquareCoulombUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSquareMetersPerSquareCoulomb = specialize TQuantity<TSquareMeterPerSquareCoulombUnit>;
  TSquareMeterPerSquareCoulombUnitId = specialize TUnitId<TSquareMeterPerSquareCoulombUnit>;

// main definition [ m2/C2 ] = [ m2 ] / [ C2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareCoulombs): TSquareMetersPerSquareCoulomb; inline;
operator *(const ALeft: TSquareCoulombs; const ARight: TSquareMetersPerSquareCoulomb): TSquareMeters; inline;
operator *(const ALeft: TSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombs): TSquareMeters; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareCoulomb): TSquareCoulombs; inline;
operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSquareCoulombUnitId): TSquareMetersPerSquareCoulomb; inline;

type
  { Unit of NewtonPerSquareCoulomb }
  TNewtonPerSquareCoulombUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TNewtonsPerSquareCoulomb = specialize TQuantity<TNewtonPerSquareCoulombUnit>;
  TNewtonPerSquareCoulombUnitId = specialize TUnitId<TNewtonPerSquareCoulombUnit>;

// main definition [ N/C2 ] = [ N ] / [ C2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareCoulombs): TNewtonsPerSquareCoulomb; inline;
operator *(const ALeft: TSquareCoulombs; const ARight: TNewtonsPerSquareCoulomb): TNewtons; inline;
operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareCoulombs): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareCoulomb): TSquareCoulombs; inline;
operator /(const ALeft: TNewtons; const {%H-}ARight: TSquareCoulombUnitId): TNewtonsPerSquareCoulomb; inline;

type
  { Unit of NewtonSquareMeterPerSquareCoulomb }
  TNewtonSquareMeterPerSquareCoulombUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TNewtonSquareMetersPerSquareCoulomb = specialize TQuantity<TNewtonSquareMeterPerSquareCoulombUnit>;
  TNewtonSquareMeterPerSquareCoulombUnitId = specialize TUnitId<TNewtonSquareMeterPerSquareCoulombUnit>;

// main definition [ N*m2/C2 ] = [ N ] * [ m2/C2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareCoulomb): TNewtonSquareMetersPerSquareCoulomb; inline;
operator *(const ALeft: TSquareMetersPerSquareCoulomb; const ARight: TNewtons): TNewtonSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TNewtons): TSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareMetersPerSquareCoulomb): TNewtons; inline;

// alternative definition [ N*m2/C2 ] = [ N*m2 ] / [ C2 ]
operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareCoulombs): TNewtonSquareMetersPerSquareCoulomb; inline;
operator *(const ALeft: TSquareCoulombs; const ARight: TNewtonSquareMetersPerSquareCoulomb): TNewtonSquareMeters; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombs): TNewtonSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtonSquareMetersPerSquareCoulomb): TSquareCoulombs; inline;
operator /(const ALeft: TNewtonSquareMeters; const {%H-}ARight: TSquareCoulombUnitId): TNewtonSquareMetersPerSquareCoulomb; inline;

// alternative definition [ N*m2/C2 ] = [ N/C2 ] * [ m2 ]
operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareMeters): TNewtonSquareMetersPerSquareCoulomb; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareCoulomb): TNewtonSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TNewtonsPerSquareCoulomb): TSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareMeters): TNewtonsPerSquareCoulomb; inline;
operator *(const ALeft: TNewtonsPerSquareCoulomb; const {%H-}ARight: TSquareMeterUnitId): TNewtonSquareMetersPerSquareCoulomb; inline;

// alternative definition [ N*m2/C2 ] = [ V/m ] / [ C/m2 ]
operator /(const ALeft: TVoltsPerMeter; const ARight: TCoulombsPerSquareMeter): TNewtonSquareMetersPerSquareCoulomb; inline;
operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TVoltsPerMeter; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TCoulombsPerSquareMeter): TVoltsPerMeter; inline;
operator /(const ALeft: TVoltsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TCoulombsPerSquareMeter; inline;

// alternative definition [ N*m2/C2 ] = [ J ] / [ C2/m ]
operator /(const ALeft: TJoules; const ARight: TSquareCoulombsPerMeter): TNewtonSquareMetersPerSquareCoulomb; inline;
operator *(const ALeft: TSquareCoulombsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TJoules; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombsPerMeter): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TNewtonSquareMetersPerSquareCoulomb): TSquareCoulombsPerMeter; inline;

type
  { Unit of VoltMeter }
  TVoltMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TVoltMeters = specialize TQuantity<TVoltMeterUnit>;
  TVoltMeterUnitId = specialize TUnitId<TVoltMeterUnit>;

// main definition [ V*m ] = [ V ] * [ m ]
operator *(const ALeft: TVolts; const ARight: TMeters): TVoltMeters; inline;
operator *(const ALeft: TMeters; const ARight: TVolts): TVoltMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TVolts): TMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TMeters): TVolts; inline;
operator *(const ALeft: TVolts; const {%H-}ARight: TMeterUnitId): TVoltMeters; inline;

// alternative definition [ V*m ] = [ V/m ] * [ m2 ]
operator *(const ALeft: TVoltsPerMeter; const ARight: TSquareMeters): TVoltMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TVoltsPerMeter): TVoltMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TVoltsPerMeter): TSquareMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TSquareMeters): TVoltsPerMeter; inline;

type
  { Unit of NewtonSquareMeterPerCoulomb }
  TNewtonSquareMeterPerCoulombUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TNewtonSquareMetersPerCoulomb = specialize TQuantity<TVoltMeterUnit>;
  TNewtonSquareMeterPerCoulombUnitId = specialize TUnitId<TVoltMeterUnit>;

type
  { Unit of VoltMeterPerSecond }
  TVoltMeterPerSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TVoltMetersPerSecond = specialize TQuantity<TVoltMeterPerSecondUnit>;
  TVoltMeterPerSecondUnitId = specialize TUnitId<TVoltMeterPerSecondUnit>;

// main definition [ V*m/s ] = [ V*m ] / [ s ]
operator /(const ALeft: TVoltMeters; const ARight: TSeconds): TVoltMetersPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TVoltMetersPerSecond): TVoltMeters; inline;
operator *(const ALeft: TVoltMetersPerSecond; const ARight: TSeconds): TVoltMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TVoltMetersPerSecond): TSeconds; inline;
operator /(const ALeft: TVoltMeters; const {%H-}ARight: TSecondUnitId): TVoltMetersPerSecond; inline;

type
  { Unit of FaradPerMeter }
  TFaradPerMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TFaradsPerMeter = specialize TQuantity<TFaradPerMeterUnit>;
  TFaradPerMeterUnitId = specialize TUnitId<TFaradPerMeterUnit>;

// main definition [ F/m ] = [ F ] / [ m ]
operator /(const ALeft: TFarads; const ARight: TMeters): TFaradsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TFaradsPerMeter): TFarads; inline;
operator *(const ALeft: TFaradsPerMeter; const ARight: TMeters): TFarads; inline;
operator /(const ALeft: TFarads; const ARight: TFaradsPerMeter): TMeters; inline;
operator /(const ALeft: TFarads; const {%H-}ARight: TMeterUnitId): TFaradsPerMeter; inline;

// alternative definition [ F/m ] = [ C ] / [ V*m ]
operator /(const ALeft: TCoulombs; const ARight: TVoltMeters): TFaradsPerMeter; inline;
operator *(const ALeft: TVoltMeters; const ARight: TFaradsPerMeter): TCoulombs; inline;
operator *(const ALeft: TFaradsPerMeter; const ARight: TVoltMeters): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TFaradsPerMeter): TVoltMeters; inline;

// alternative definition [ F/m ] = [ C/m2 ] / [ N/C ]
operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TVoltsPerMeter): TFaradsPerMeter; inline;
operator *(const ALeft: TVoltsPerMeter; const ARight: TFaradsPerMeter): TCoulombsPerSquareMeter; inline;
operator *(const ALeft: TFaradsPerMeter; const ARight: TVoltsPerMeter): TCoulombsPerSquareMeter; inline;
operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TFaradsPerMeter): TVoltsPerMeter; inline;

// alternative definition [ F/m ] = [ 1 ] / [ N*m2/C2 ]
operator /(const ALeft: double; const ARight: TNewtonSquareMetersPerSquareCoulomb): TFaradsPerMeter; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TFaradsPerMeter): double; inline;
operator *(const ALeft: TFaradsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): double; inline;
operator /(const ALeft: double; const ARight: TFaradsPerMeter): TNewtonSquareMetersPerSquareCoulomb; inline;

type
  { Unit of AmperePerMeter }
  TAmperePerMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TAmperesPerMeter = specialize TQuantity<TAmperePerMeterUnit>;
  TAmperePerMeterUnitId = specialize TUnitId<TAmperePerMeterUnit>;

// main definition [ A/m ] = [ A ] / [ m ]
operator /(const ALeft: TAmperes; const ARight: TMeters): TAmperesPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TAmperesPerMeter): TAmperes; inline;
operator *(const ALeft: TAmperesPerMeter; const ARight: TMeters): TAmperes; inline;
operator /(const ALeft: TAmperes; const ARight: TAmperesPerMeter): TMeters; inline;
operator /(const ALeft: TAmperes; const {%H-}ARight: TMeterUnitId): TAmperesPerMeter; inline;

type
  { Unit of MeterPerAmpere }
  TMeterPerAmpereUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TMetersPerAmpere = specialize TQuantity<TMeterPerAmpereUnit>;
  TMeterPerAmpereUnitId = specialize TUnitId<TMeterPerAmpereUnit>;

// main definition [ m/A ] = [ m ] / [ A ]
operator /(const ALeft: TMeters; const ARight: TAmperes): TMetersPerAmpere; inline;
operator *(const ALeft: TAmperes; const ARight: TMetersPerAmpere): TMeters; inline;
operator *(const ALeft: TMetersPerAmpere; const ARight: TAmperes): TMeters; inline;
operator /(const ALeft: TMeters; const ARight: TMetersPerAmpere): TAmperes; inline;
operator /(const ALeft: TMeters; const {%H-}ARight: TAmpereUnitId): TMetersPerAmpere; inline;

type
  { Unit of TeslaMeter }
  TTeslaMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TTeslaMeters = specialize TQuantity<TTeslaMeterUnit>;
  TTeslaMeterUnitId = specialize TUnitId<TTeslaMeterUnit>;

// main definition [ T*m ] = [ T ] * [ m ]
operator *(const ALeft: TTeslas; const ARight: TMeters): TTeslaMeters; inline;
operator *(const ALeft: TMeters; const ARight: TTeslas): TTeslaMeters; inline;
operator /(const ALeft: TTeslaMeters; const ARight: TTeslas): TMeters; inline;
operator /(const ALeft: TTeslaMeters; const ARight: TMeters): TTeslas; inline;
operator *(const ALeft: TTeslas; const {%H-}ARight: TMeterUnitId): TTeslaMeters; inline;

// alternative definition [ T*m ] = [ N/A ] = [ N ] / [ A ]
operator /(const ALeft: TNewtons; const ARight: TAmperes): TTeslaMeters; inline;
operator *(const ALeft: TAmperes; const ARight: TTeslaMeters): TNewtons; inline;
operator *(const ALeft: TTeslaMeters; const ARight: TAmperes): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TTeslaMeters): TAmperes; inline;
operator /(const ALeft: TNewtons; const {%H-}ARight: TAmpereUnitId): TTeslaMeters; inline;

type
  { Unit of NewtonPerAmpere }
  TNewtonPerAmpereUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TNewtonsPerAmpere = specialize TQuantity<TTeslaMeterUnit>;
  TNewtonPerAmpereUnitId = specialize TUnitId<TTeslaMeterUnit>;

type
  { Unit of TeslaPerAmpere }
  TTeslaPerAmpereUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TTeslasPerAmpere = specialize TQuantity<TTeslaPerAmpereUnit>;
  TTeslaPerAmpereUnitId = specialize TUnitId<TTeslaPerAmpereUnit>;

// main definition [ T/A ] = [ T ] / [ A ]
operator /(const ALeft: TTeslas; const ARight: TAmperes): TTeslasPerAmpere; inline;
operator *(const ALeft: TAmperes; const ARight: TTeslasPerAmpere): TTeslas; inline;
operator *(const ALeft: TTeslasPerAmpere; const ARight: TAmperes): TTeslas; inline;
operator /(const ALeft: TTeslas; const ARight: TTeslasPerAmpere): TAmperes; inline;
operator /(const ALeft: TTeslas; const {%H-}ARight: TAmpereUnitId): TTeslasPerAmpere; inline;

type
  { Unit of HenryPerMeter }
  THenryPerMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  THenrysPerMeter = specialize TQuantity<THenryPerMeterUnit>;
  THenryPerMeterUnitId = specialize TUnitId<THenryPerMeterUnit>;

// main definition [ H/m ] = [ H ] / [ m ]
operator /(const ALeft: THenrys; const ARight: TMeters): THenrysPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: THenrysPerMeter): THenrys; inline;
operator *(const ALeft: THenrysPerMeter; const ARight: TMeters): THenrys; inline;
operator /(const ALeft: THenrys; const ARight: THenrysPerMeter): TMeters; inline;
operator /(const ALeft: THenrys; const {%H-}ARight: TMeterUnitId): THenrysPerMeter; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T*m ] / [ A ]
operator /(const ALeft: TTeslaMeters; const ARight: TAmperes): THenrysPerMeter; inline;
operator *(const ALeft: TAmperes; const ARight: THenrysPerMeter): TTeslaMeters; inline;
operator *(const ALeft: THenrysPerMeter; const ARight: TAmperes): TTeslaMeters; inline;
operator /(const ALeft: TTeslaMeters; const ARight: THenrysPerMeter): TAmperes; inline;
operator /(const ALeft: TTeslaMeters; const {%H-}ARight: TAmpereUnitId): THenrysPerMeter; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T/A ] * [ m ]
operator *(const ALeft: TTeslasPerAmpere; const ARight: TMeters): THenrysPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TTeslasPerAmpere): THenrysPerMeter; inline;
operator /(const ALeft: THenrysPerMeter; const ARight: TTeslasPerAmpere): TMeters; inline;
operator /(const ALeft: THenrysPerMeter; const ARight: TMeters): TTeslasPerAmpere; inline;
operator *(const ALeft: TTeslasPerAmpere; const {%H-}ARight: TMeterUnitId): THenrysPerMeter; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] * [ m/A ]
operator *(const ALeft: TTeslas; const ARight: TMetersPerAmpere): THenrysPerMeter; inline;
operator *(const ALeft: TMetersPerAmpere; const ARight: TTeslas): THenrysPerMeter; inline;
operator /(const ALeft: THenrysPerMeter; const ARight: TTeslas): TMetersPerAmpere; inline;
operator /(const ALeft: THenrysPerMeter; const ARight: TMetersPerAmpere): TTeslas; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] / [ A/m ]
operator /(const ALeft: TTeslas; const ARight: TAmperesPerMeter): THenrysPerMeter; inline;
operator *(const ALeft: TAmperesPerMeter; const ARight: THenrysPerMeter): TTeslas; inline;
operator *(const ALeft: THenrysPerMeter; const ARight: TAmperesPerMeter): TTeslas; inline;
operator /(const ALeft: TTeslas; const ARight: THenrysPerMeter): TAmperesPerMeter; inline;

// alternative definition [ H/m ] = [ N/A2 ] = [ N ] / [ A2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareAmperes): THenrysPerMeter; inline;
operator *(const ALeft: TSquareAmperes; const ARight: THenrysPerMeter): TNewtons; inline;
operator *(const ALeft: THenrysPerMeter; const ARight: TSquareAmperes): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: THenrysPerMeter): TSquareAmperes; inline;

type
  { Unit of TeslaMeterPerAmpere }
  TTeslaMeterPerAmpereUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TTeslaMetersPerAmpere = specialize TQuantity<THenryPerMeterUnit>;
  TTeslaMeterPerAmpereUnitId = specialize TUnitId<THenryPerMeterUnit>;

type
  { Unit of NewtonPerSquareAmpere }
  TNewtonPerSquareAmpereUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TNewtonsPerSquareAmpere = specialize TQuantity<THenryPerMeterUnit>;
  TNewtonPerSquareAmpereUnitId = specialize TUnitId<THenryPerMeterUnit>;

type
  { Unit of RadianPerMeter }
  TRadianPerMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TRadiansPerMeter = specialize TQuantity<TRadianPerMeterUnit>;
  TRadianPerMeterUnitId = specialize TUnitId<TRadianPerMeterUnit>;

// main definition [ rad/m ] = [ rad ] / [ m ]
operator /(const ALeft: TRadians; const ARight: TMeters): TRadiansPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TRadiansPerMeter): TRadians; inline;
operator *(const ALeft: TRadiansPerMeter; const ARight: TMeters): TRadians; inline;
operator /(const ALeft: TRadians; const ARight: TRadiansPerMeter): TMeters; inline;
operator /(const ALeft: TRadians; const {%H-}ARight: TMeterUnitId): TRadiansPerMeter; inline;

type
  { Unit of SquareKilogramPerSquareSecond }
  TSquareKilogramPerSquareSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSquareKilogramsPerSquareSecond = specialize TQuantity<TSquareKilogramPerSquareSecondUnit>;
  TSquareKilogramPerSquareSecondUnitId = specialize TUnitId<TSquareKilogramPerSquareSecondUnit>;

// main definition [ kg2/s2 ] = [ kg2 ] / [ s2 ]
operator /(const ALeft: TSquareKilograms; const ARight: TSquareSeconds): TSquareKilogramsPerSquareSecond; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TSquareKilogramsPerSquareSecond): TSquareKilograms; inline;
operator *(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TSquareSeconds): TSquareKilograms; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerSquareSecond): TSquareSeconds; inline;
operator /(const ALeft: TSquareKilograms; const {%H-}ARight: TSquareSecondUnitId): TSquareKilogramsPerSquareSecond; inline;

// alternative definition [ kg2/s2 ] = [ kg/s ] * [ kg/s ]
operator *(const ALeft: TKilogramsPerSecond; const ARight: TKilogramsPerSecond): TSquareKilogramsPerSquareSecond; inline;
operator /(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TKilogramsPerSecond): TKilogramsPerSecond; inline;

// alternative definition [ kg2/s2 ] = [ kg ] * [ N/m ]
operator *(const ALeft: TKilograms; const ARight: TNewtonsPerMeter): TSquareKilogramsPerSquareSecond; inline;
operator *(const ALeft: TNewtonsPerMeter; const ARight: TKilograms): TSquareKilogramsPerSquareSecond; inline;
operator /(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TKilograms): TNewtonsPerMeter; inline;
operator /(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TNewtonsPerMeter): TKilograms; inline;

type
  { Unit of SquareSecondPerSquareMeter }
  TSquareSecondPerSquareMeterUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSquareSecondsPerSquareMeter = specialize TQuantity<TSquareSecondPerSquareMeterUnit>;
  TSquareSecondPerSquareMeterUnitId = specialize TUnitId<TSquareSecondPerSquareMeterUnit>;

// main definition [ s2/m2 ] = [ s2 ] / [ m2 ]
operator /(const ALeft: TSquareSeconds; const ARight: TSquareMeters): TSquareSecondsPerSquareMeter; inline;
operator *(const ALeft: TSquareMeters; const ARight: TSquareSecondsPerSquareMeter): TSquareSeconds; inline;
operator *(const ALeft: TSquareSecondsPerSquareMeter; const ARight: TSquareMeters): TSquareSeconds; inline;
operator /(const ALeft: TSquareSeconds; const ARight: TSquareSecondsPerSquareMeter): TSquareMeters; inline;
operator /(const ALeft: TSquareSeconds; const {%H-}ARight: TSquareMeterUnitId): TSquareSecondsPerSquareMeter; inline;

// alternative definition [ s2/m2 ] = [ 1 ] / [ m2/s2 ]
operator /(const ALeft: double; const ARight: TSquareMetersPerSquareSecond): TSquareSecondsPerSquareMeter; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TSquareSecondsPerSquareMeter): double; inline;
operator *(const ALeft: TSquareSecondsPerSquareMeter; const ARight: TSquareMetersPerSquareSecond): double; inline;
operator /(const ALeft: double; const ARight: TSquareSecondsPerSquareMeter): TSquareMetersPerSquareSecond; inline;

// alternative definition [ s2/m2 ] = [ F/m ] * [ H/m ]
operator *(const ALeft: TFaradsPerMeter; const ARight: THenrysPerMeter): TSquareSecondsPerSquareMeter; inline;
operator *(const ALeft: THenrysPerMeter; const ARight: TFaradsPerMeter): TSquareSecondsPerSquareMeter; inline;
operator /(const ALeft: TSquareSecondsPerSquareMeter; const ARight: TFaradsPerMeter): THenrysPerMeter; inline;
operator /(const ALeft: TSquareSecondsPerSquareMeter; const ARight: THenrysPerMeter): TFaradsPerMeter; inline;

type
  { Unit of SquareJoule }
  TSquareJouleUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TSquareJoules = specialize TQuantity<TSquareJouleUnit>;
  TSquareJouleUnitId = specialize TUnitId<TSquareJouleUnit>;

var J2: TSquareJouleUnitId;

const quettaJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E+60);
const  ronnaJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E+54);
const  yottaJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E+48);
const  zettaJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E+42);
const      EJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E+36);
const   petaJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E+30);
const      TJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E+24);
const      GJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E+18);
const   megaJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E+12);
const      kJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E+06);
const      hJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E+04);
const     daJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E+02);
const      dJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E-02);
const      cJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E-04);
const      mJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E-06);
const     miJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E-12);
const      nJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E-18);
const      pJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E-24);
const      fJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E-30);
const      aJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E-36);
const  zeptoJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E-42);
const  yoctoJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E-48);
const  rontoJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E-54);
const quectoJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E-60);

// main definition [ J2 ] = [ J ] * [ J ]
operator *(const ALeft: TJoules; const ARight: TJoules): TSquareJoules; inline;
operator /(const ALeft: TSquareJoules; const ARight: TJoules): TJoules; inline;

type
  { Unit of JouleSecond }
  TJouleSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
  end;
  TJouleSeconds = specialize TQuantity<TJouleSecondUnit>;
  TJouleSecondUnitId = specialize TUnitId<TJouleSecondUnit>;

// main definition [ J*s ] = [ J ] * [ s ]
operator *(const ALeft: TJoules; const ARight: TSeconds): TJouleSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TJoules): TJouleSeconds; inline;
operator /(const ALeft: TJouleSeconds; const ARight: TJoules): TSeconds; inline;
operator /(const ALeft: TJouleSeconds; const ARight: TSeconds): TJoules; inline;
operator *(const ALeft: TJoules; const {%H-}ARight: TSecondUnitId): TJouleSeconds; inline;

// alternative definition [ J*s ] = [ J ] / [ Hz ]
operator /(const ALeft: TJoules; const ARight: THertz): TJouleSeconds; inline;
operator *(const ALeft: THertz; const ARight: TJouleSeconds): TJoules; inline;
operator *(const ALeft: TJouleSeconds; const ARight: THertz): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TJouleSeconds): THertz; inline;
operator /(const ALeft: TJoules; const {%H-}ARight: THertzUnitId): TJouleSeconds; inline;

// alternative definition [ J*s ] = [ kg*m/s ] * [ m ]
operator *(const ALeft: TKilogramMetersPerSecond; const ARight: TMeters): TJouleSeconds; inline;
operator *(const ALeft: TMeters; const ARight: TKilogramMetersPerSecond): TJouleSeconds; inline;
operator /(const ALeft: TJouleSeconds; const ARight: TKilogramMetersPerSecond): TMeters; inline;
operator /(const ALeft: TJouleSeconds; const ARight: TMeters): TKilogramMetersPerSecond; inline;

type
  { Unit of ElettronvoltSecond }
  TElettronvoltSecondUnit = record
    class function GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
    class function GetName  (const {%H-}APrefixes: TPrefixes): string; static;
    class function GetValue (const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
    const Factor = 1.60217742320523E-019;
  end;
  TElettronvoltSeconds = specialize TQuantity<TJouleSecondUnit>;
  TElettronvoltSecondUnitId = specialize TUnitId<TElettronvoltSecondUnit>;

{ Helpers }

type
  TSecondHelper = record helper for TSeconds
    function ToMinute: specialize TQuantity<TMinuteUnit>;
    function ToHour: specialize TQuantity<THourUnit>;
    function ToDay: specialize TQuantity<TDayUnit>;
  end;

type
  TSquareSecondHelper = record helper for TSquareSeconds
    function ToSquareMinute: specialize TQuantity<TSquareMinuteUnit>;
    function ToSquareHour: specialize TQuantity<TSquareHourUnit>;
    function ToSquareDay: specialize TQuantity<TSquareDayUnit>;
  end;

type
  TMeterHelper = record helper for TMeters
    function ToAstronomical: specialize TQuantity<TAstronomicalUnit>;
  end;

type
  TCubicMeterHelper = record helper for TCubicMeters
    function ToLitre: specialize TQuantity<TLitreUnit>;
  end;

type
  TKilogramHelper = record helper for TKilograms
    function ToTonne: specialize TQuantity<TTonneUnit>;
  end;

type
  TDegreeCelsiusHelper = record helper for TDegreesCelsius
    function ToKelvin: specialize TQuantity<TKelvinUnit>;
  end;

type
  TKelvinHelper = record helper for TKelvins
    function ToDegreeFahrenheit: specialize TQuantity<TDegreeFahrenheitUnit>;
    function ToDegreeCelsius: specialize TQuantity<TDegreeCelsiusUnit>;
  end;

type
  TDegreeFahrenheitHelper = record helper for TDegreesFahrenheit
    function ToKelvin: specialize TQuantity<TKelvinUnit>;
  end;

type
  TRadianHelper = record helper for TRadians
    function ToDegree: specialize TQuantity<TDegreeUnit>;
  end;

type
  TSquareHertzHelper = record helper for TSquareHertz
    function ToSteradianPerSquareSecond: specialize TQuantity<TSteradianPerSquareSecondUnit>;
    function ToRadianPerSquareSecond: specialize TQuantity<TRadianPerSquareSecondUnit>;
  end;

type
  TMeterPerSecondHelper = record helper for TMetersPerSecond
    function ToMeterPerHour: specialize TQuantity<TMeterPerHourUnit>;
  end;

type
  TMeterPerSquareSecondHelper = record helper for TMetersPerSquareSecond
    function ToMeterPerHourPerSecond: specialize TQuantity<TMeterPerHourPerSecondUnit>;
    function ToMeterPerSecondPerSecond: specialize TQuantity<TMeterPerSecondPerSecondUnit>;
  end;

type
  TKilogramMeterPerSecondHelper = record helper for TKilogramMetersPerSecond
    function ToNewtonSecond: specialize TQuantity<TNewtonSecondUnit>;
  end;

type
  TJouleHelper = record helper for TJoules
    function ToNewtonMeter: specialize TQuantity<TNewtonMeterUnit>;
    function ToElettronvolt: specialize TQuantity<TElettronvoltUnit>;
  end;

type
  THertzHelper = record helper for THertz
    function ToBequerel: specialize TQuantity<TBequerelUnit>;
  end;

type
  TSquareMeterPerSquareSecondHelper = record helper for TSquareMetersPerSquareSecond
    function ToSievert: specialize TQuantity<TSievertUnit>;
    function ToGray: specialize TQuantity<TGrayUnit>;
  end;

type
  TJoulePerRadianHelper = record helper for TJoulesPerRadian
    function ToNewtonMeterPerDegree: specialize TQuantity<TNewtonMeterPerDegreeUnit>;
    function ToNewtonMeterPerRadian: specialize TQuantity<TNewtonMeterPerRadianUnit>;
    function ToJoulePerDegree: specialize TQuantity<TJoulePerDegreeUnit>;
  end;

type
  TPoiseuilleHelper = record helper for TPoiseuilles
    function ToPascalSecond: specialize TQuantity<TPascalSecondUnit>;
  end;

type
  TSquareMeterPerSquareKilogramHelper = record helper for TSquareMetersPerSquareKilogram
    function ToJoulePerKilogram: specialize TQuantity<TJoulePerKilogramUnit>;
  end;

type
  TVoltPerMeterHelper = record helper for TVoltsPerMeter
    function ToNewtonPerCoulomb: specialize TQuantity<TNewtonPerCoulombUnit>;
  end;

type
  TVoltMeterHelper = record helper for TVoltMeters
    function ToNewtonSquareMeterPerCoulomb: specialize TQuantity<TNewtonSquareMeterPerCoulombUnit>;
  end;

type
  TTeslaMeterHelper = record helper for TTeslaMeters
    function ToNewtonPerAmpere: specialize TQuantity<TNewtonPerAmpereUnit>;
  end;

type
  THenryPerMeterHelper = record helper for THenrysPerMeter
    function ToNewtonPerSquareAmpere: specialize TQuantity<TNewtonPerSquareAmpereUnit>;
    function ToTeslaMeterPerAmpere: specialize TQuantity<TTeslaMeterPerAmpereUnit>;
  end;

type
  TJouleSecondHelper = record helper for TJouleSeconds
    function ToElettronvoltSecond: specialize TQuantity<TElettronvoltSecondUnit>;
  end;

{ Power units }

function SquarePower(AQuantity: TSeconds): TSquareSeconds;
function SquareRoot(AQuantity: TSquareSeconds): TSeconds;
function SquarePower(AQuantity: TMeters): TSquareMeters;
function SquareRoot(AQuantity: TSquareMeters): TMeters;
function CubicPower(AQuantity: TMeters): TCubicMeters;
function CubicRoot(AQuantity: TCubicMeters): TMeters;
function SquarePower(AQuantity: TSquareMeters): TQuarticMeters;
function SquareRoot(AQuantity: TQuarticMeters): TSquareMeters;
function QuarticPower(AQuantity: TMeters): TQuarticMeters;
function QuarticRoot(AQuantity: TQuarticMeters): TMeters;
function QuinticPower(AQuantity: TMeters): TQuinticMeters;
function QuinticRoot(AQuantity: TQuinticMeters): TMeters;
function SquarePower(AQuantity: TCubicMeters): TSexticMeters;
function SquareRoot(AQuantity: TSexticMeters): TCubicMeters;
function CubicPower(AQuantity: TSquareMeters): TSexticMeters;
function CubicRoot(AQuantity: TSexticMeters): TSquareMeters;
function SexticPower(AQuantity: TMeters): TSexticMeters;
function SexticRoot(AQuantity: TSexticMeters): TMeters;
function SquarePower(AQuantity: TAmperes): TSquareAmperes;
function SquareRoot(AQuantity: TSquareAmperes): TAmperes;
function SquarePower(AQuantity: TKelvins): TSquareKelvins;
function SquareRoot(AQuantity: TSquareKelvins): TKelvins;
function CubicPower(AQuantity: TKelvins): TCubicKelvins;
function CubicRoot(AQuantity: TCubicKelvins): TKelvins;
function SquarePower(AQuantity: TSquareKelvins): TQuarticKelvins;
function SquareRoot(AQuantity: TQuarticKelvins): TSquareKelvins;
function QuarticPower(AQuantity: TKelvins): TQuarticKelvins;
function QuarticRoot(AQuantity: TQuarticKelvins): TKelvins;
function SquarePower(AQuantity: TRadians): TSteradians;
function SquareRoot(AQuantity: TSteradians): TRadians;
function SquarePower(AQuantity: THertz): TSquareHertz;
function SquareRoot(AQuantity: TSquareHertz): THertz;
function SquarePower(AQuantity: TMetersPerSecond): TSquareMetersPerSquareSecond;
function SquareRoot(AQuantity: TSquareMetersPerSquareSecond): TMetersPerSecond;
function SquarePower(AQuantity: TCoulombs): TSquareCoulombs;
function SquareRoot(AQuantity: TSquareCoulombs): TCoulombs;
function SquarePower(AQuantity: TVolts): TSquareVolts;
function SquareRoot(AQuantity: TSquareVolts): TVolts;
function SquarePower(AQuantity: TKilogramsPerSecond): TSquareKilogramsPerSquareSecond;
function SquareRoot(AQuantity: TSquareKilogramsPerSquareSecond): TKilogramsPerSecond;
function SquarePower(AQuantity: TJoules): TSquareJoules;
function SquareRoot(AQuantity: TSquareJoules): TJoules;

{ Trigonometric functions }

function Cos(const AQuantity: TRadians): double;
function Sin(const AQuantity: TRadians): double;
function Tan(const AQuantity: TRadians): double;
function Cotan(const AQuantity: TRadians): double;
function Secant(const AQuantity: TRadians): double;
function Cosecant(const AQuantity: TRadians): double;

function ArcCos(const AValue: double): TRadians;
function ArcSin(const AValue: double): TRadians;
function ArcTan(const AValue: double): TRadians;
function ArcTan2(const x, y: double): TRadians;

const
  PrefixTable: array[pQuetta..pQuecto] of
    record  Symbol, Name: string; Factor: double end = (
    (Symbol: 'Q';   Name: 'quetta';  Factor: 1E+30),
    (Symbol: 'R';   Name: 'ronna';   Factor: 1E+27),
    (Symbol: 'Y';   Name: 'yotta';   Factor: 1E+24),
    (Symbol: 'Z';   Name: 'zetta';   Factor: 1E+21),
    (Symbol: 'E';   Name: 'exa';     Factor: 1E+18),
    (Symbol: 'P';   Name: 'peta';    Factor: 1E+15),
    (Symbol: 'T';   Name: 'tera';    Factor: 1E+12),
    (Symbol: 'G';   Name: 'giga';    Factor: 1E+09),
    (Symbol: 'M';   Name: 'mega';    Factor: 1E+06),
    (Symbol: 'k';   Name: 'kilo';    Factor: 1E+03),
    (Symbol: 'h';   Name: 'hecto';   Factor: 1E+02),
    (Symbol: 'da';  Name: 'deca';    Factor: 1E+01),
    (Symbol: '';    Name:  '';       Factor: 1E+00),
    (Symbol: 'd';   Name: 'deci';    Factor: 1E-01),
    (Symbol: 'c';   Name: 'centi';   Factor: 1E-02),
    (Symbol: 'm';   Name: 'milli';   Factor: 1E-03),
    (Symbol: 'μ';   Name: 'micro';   Factor: 1E-06),
    (Symbol: 'n';   Name: 'nano';    Factor: 1E-09),
    (Symbol: 'p';   Name: 'pico';    Factor: 1E-12),
    (Symbol: 'f';   Name: 'femto';   Factor: 1E-15),
    (Symbol: 'a';   Name: 'atto';    Factor: 1E-18),
    (Symbol: 'z';   Name: 'zepto';   Factor: 1E-21),
    (Symbol: 'y';   Name: 'yocto';   Factor: 1E-24),
    (Symbol: 'r';   Name: 'ronto';   Factor: 1E-27),
    (Symbol: 'q';   Name: 'quecto';  Factor: 1E-30)
  );

implementation

uses Math;

{ TQuantity }

function TQuantity.Abs: TSelf;
begin
  result.FValue := System.Abs(FValue);
end;

function TQuantity.Value: double;
begin
  result := FValue;
end;

function TQuantity.Value(const Prefixes: TPrefixes): double;
begin
  result := U.GetValue(FValue, Prefixes);
end;

function TQuantity.ToString: string;
begin
  result := FloatToStr(FValue) + ' ' + U.GetSymbol([]);
end;

function TQuantity.ToVerboseString: string;
begin
  result := FloatToStr(FValue) + ' ' + U.GetName([]);
end;

function TQuantity.ToString(Precision, Digits: longint; const Prefixes: TPrefixes): string;
begin
  result := FloatToStrF(U.GetValue(FValue, Prefixes), ffGeneral, Precision, Digits) + ' ' + U.GetSymbol(Prefixes);
end;

function TQuantity.ToVerboseString(Precision, Digits: longint; const Prefixes: TPrefixes): string;
begin
  result := FloatToStrF(U.GetValue(FValue, Prefixes), ffGeneral, Precision, Digits) + ' ' + U.GetName(Prefixes);
end;

class operator TQuantity.+(const ALeft, ARight: TSelf): TSelf;
begin
  result.FValue := ALeft.FValue + ARight.FValue;
end;

class operator TQuantity.-(const ALeft, ARight: TSelf): TSelf;
begin
  result.FValue := ALeft.FValue - ARight.FValue;
end;

class operator TQuantity.*(const AValue: double; const ASelf: TSelf): TSelf;
begin
  result.FValue := AValue * ASelf.FValue;
end;

class operator TQuantity.*(const ASelf: TSelf; const AValue: double): TSelf;
begin
  result.FValue := ASelf.FValue * AValue;
end;

class operator TQuantity./(const ASelf: TSelf; const AValue: double): TSelf;
begin
  result.FValue := ASelf.FValue / AValue;
end;

class operator TQuantity./(const ALeft, ARight: TSelf): double;
begin
  result := ALeft.FValue / ARight.FValue;
end;

class operator TQuantity.mod(const ALeft, ARight: TSelf): TSelf;
begin
  result.FValue := ALeft.FValue mod ARight.FValue;
end;

class operator TQuantity.=(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.FValue = ARight.FValue;
end;

class operator TQuantity.<(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.FValue < ARight.FValue;
end;

class operator TQuantity.>(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.FValue > ARight.FValue;
end;

class operator TQuantity.<=(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.FValue <= ARight.FValue;
end;

class operator TQuantity.>=(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.FValue >= ARight.FValue;
end;

{ TUnitId }

class function TUnitId.From(const AQuantity: TBaseQuantity): TBaseQuantity;
begin
  result.FValue := AQuantity.FValue;
end;

class operator TUnitId.*(const AValue: double; const ASelf: TSelf): TBaseQuantity;
begin
  result.FValue := AValue;
end;

{ Unit of Second }

class function TSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%ss', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 's';
end;

class function TSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%ssecond', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'second';
end;

class function TSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

{ Unit of Day }

class function TDayUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'd';
end;

class function TDayUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'day';
end;

class function TDayUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
end;

{ Unit of Hour }

class function THourUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'h';
end;

class function THourUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'hour';
end;

class function THourUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
end;

{ Unit of Minute }

class function TMinuteUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'min';
end;

class function TMinuteUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'minute';
end;

class function TMinuteUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
end;

{ Unit of SquareSecond }

class function TSquareSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%ss2', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 's2';
end;

class function TSquareSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('square %ssecond', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'square second';
end;

class function TSquareSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 2);
  end;
end;

// main definition [ s2 ] = [ s ] * [ s ]

operator *(const ALeft: TSeconds; const ARight: TSeconds): TSquareSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareSeconds; const ARight: TSeconds): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of SquareDay }

class function TSquareDayUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'd2';
end;

class function TSquareDayUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'square day';
end;

class function TSquareDayUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
end;

{ Unit of SquareHour }

class function TSquareHourUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'h2';
end;

class function TSquareHourUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'square hour';
end;

class function TSquareHourUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
end;

{ Unit of SquareMinute }

class function TSquareMinuteUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'min2';
end;

class function TSquareMinuteUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'square minute';
end;

class function TSquareMinuteUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
end;

{ Unit of Meter }

class function TMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sm', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'm';
end;

class function TMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%smeter', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'meter';
end;

class function TMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

{ Unit of Astronomical }

class function TAstronomicalUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'au';
end;

class function TAstronomicalUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'astronomical unit';
end;

class function TAstronomicalUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
end;

{ Unit of SquareMeter }

class function TSquareMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sm2', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'm2';
end;

class function TSquareMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('square %smeter', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'square meter';
end;

class function TSquareMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 2);
  end;
end;

// main definition [ m2 ] = [ m ] * [ m ]

operator *(const ALeft: TMeters; const ARight: TMeters): TSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMeters; const ARight: TMeters): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of CubicMeter }

class function TCubicMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sm3', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'm3';
end;

class function TCubicMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('cubic %smeter', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'cubic meter';
end;

class function TCubicMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 3);
  end;
end;

// main definition [ m3 ] = [ m2 ] * [ m ]

operator *(const ALeft: TSquareMeters; const ARight: TMeters): TCubicMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TSquareMeters): TCubicMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMeters; const ARight: TSquareMeters): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCubicMeters; const ARight: TMeters): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of Litre }

class function TLitreUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sL', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'L';
end;

class function TLitreUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%slitre', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'litre';
end;

class function TLitreUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

{ Unit of QuarticMeter }

class function TQuarticMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sm4', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'm4';
end;

class function TQuarticMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('quartic %smeter', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'quartic meter';
end;

class function TQuarticMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 4);
  end;
end;

// main definition [ m4 ] = [ m3 ] * [ m ]

operator *(const ALeft: TCubicMeters; const ARight: TMeters): TQuarticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TCubicMeters): TQuarticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TQuarticMeters; const ARight: TCubicMeters): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TQuarticMeters; const ARight: TMeters): TCubicMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ m4 ] = [ m2 ] * [ m2 ]

operator *(const ALeft: TSquareMeters; const ARight: TSquareMeters): TQuarticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TQuarticMeters; const ARight: TSquareMeters): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of QuinticMeter }

class function TQuinticMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sm5', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'm5';
end;

class function TQuinticMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('quintic %smeter', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'quintic meter';
end;

class function TQuinticMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 5);
  end;
end;

// main definition [ m5 ] = [ m4 ] * [ m ]

operator *(const ALeft: TQuarticMeters; const ARight: TMeters): TQuinticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TQuarticMeters): TQuinticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TQuinticMeters; const ARight: TQuarticMeters): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TQuinticMeters; const ARight: TMeters): TQuarticMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ m5 ] = [ m3 ] * [ m2 ]

operator *(const ALeft: TCubicMeters; const ARight: TSquareMeters): TQuinticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TCubicMeters): TQuinticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TQuinticMeters; const ARight: TCubicMeters): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TQuinticMeters; const ARight: TSquareMeters): TCubicMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of SexticMeter }

class function TSexticMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sm6', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'm6';
end;

class function TSexticMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('sextic %smeter', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'sextic meter';
end;

class function TSexticMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 6);
  end;
end;

// main definition [ m6 ] = [ m5 ] * [ m ]

operator *(const ALeft: TQuinticMeters; const ARight: TMeters): TSexticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TQuinticMeters): TSexticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSexticMeters; const ARight: TQuinticMeters): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSexticMeters; const ARight: TMeters): TQuinticMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ m6 ] = [ m4 ] * [ m2 ]

operator *(const ALeft: TQuarticMeters; const ARight: TSquareMeters): TSexticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TQuarticMeters): TSexticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSexticMeters; const ARight: TQuarticMeters): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSexticMeters; const ARight: TSquareMeters): TQuarticMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ m6 ] = [ m3 ] * [ m3 ]

operator *(const ALeft: TCubicMeters; const ARight: TCubicMeters): TSexticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSexticMeters; const ARight: TCubicMeters): TCubicMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of Kilogram }

class function TKilogramUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sg', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'kg';
end;

class function TKilogramUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sgram', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'kilogram';
end;

class function TKilogramUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pKilo) then
      result := result * 1E+03 / PrefixTable[APrefixes[0]].Factor;
  end;
end;

{ Unit of Tonne }

class function TTonneUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%st', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 't';
end;

class function TTonneUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%stonne', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'tonne';
end;

class function TTonneUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

{ Unit of SquareKilogram }

class function TSquareKilogramUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sg2', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'kg2';
end;

class function TSquareKilogramUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('square %sgram', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'square kilogram';
end;

class function TSquareKilogramUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pKilo) then
      result := result * 1E+06 / IntPower(PrefixTable[APrefixes[0]].Factor, 2);
  end;
end;

// main definition [ kg2 ] = [ kg ] * [ kg ]

operator *(const ALeft: TKilograms; const ARight: TKilograms): TSquareKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareKilograms; const ARight: TKilograms): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of Ampere }

class function TAmpereUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sA', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'A';
end;

class function TAmpereUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sampere', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'ampere';
end;

class function TAmpereUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

{ Unit of SquareAmpere }

class function TSquareAmpereUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sA2', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'A2';
end;

class function TSquareAmpereUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('square %sampere', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'square ampere';
end;

class function TSquareAmpereUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 2);
  end;
end;

// main definition [ A2 ] = [ A ] * [ A ]

operator *(const ALeft: TAmperes; const ARight: TAmperes): TSquareAmperes;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareAmperes; const ARight: TAmperes): TAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of Kelvin }

class function TKelvinUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sK', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'K';
end;

class function TKelvinUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%skelvin', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'kelvin';
end;

class function TKelvinUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

{ Unit of DegreeCelsius }

class function TDegreeCelsiusUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'ºC';
end;

class function TDegreeCelsiusUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'degree Celsius';
end;

class function TDegreeCelsiusUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
end;

{ Unit of DegreeFahrenheit }

class function TDegreeFahrenheitUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'ºF';
end;

class function TDegreeFahrenheitUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'degree Fahrenheit';
end;

class function TDegreeFahrenheitUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
end;

{ Unit of SquareKelvin }

class function TSquareKelvinUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sK2', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'K2';
end;

class function TSquareKelvinUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('square %skelvin', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'square kelvin';
end;

class function TSquareKelvinUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 2);
  end;
end;

// main definition [ K2 ] = [ K ] * [ K ]

operator *(const ALeft: TKelvins; const ARight: TKelvins): TSquareKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareKelvins; const ARight: TKelvins): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of CubicKelvin }

class function TCubicKelvinUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sK3', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'K3';
end;

class function TCubicKelvinUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('cubic %skelvin', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'cubic kelvin';
end;

class function TCubicKelvinUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 3);
  end;
end;

// main definition [ K3 ] = [ K2 ] * [ K ]

operator *(const ALeft: TSquareKelvins; const ARight: TKelvins): TCubicKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TSquareKelvins): TCubicKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicKelvins; const ARight: TSquareKelvins): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCubicKelvins; const ARight: TKelvins): TSquareKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of QuarticKelvin }

class function TQuarticKelvinUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sK4', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'K4';
end;

class function TQuarticKelvinUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('quartic %skelvin', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'quartic kelvin';
end;

class function TQuarticKelvinUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 4);
  end;
end;

// main definition [ K4 ] = [ K3 ] * [ K ]

operator *(const ALeft: TCubicKelvins; const ARight: TKelvins): TQuarticKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TCubicKelvins): TQuarticKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TQuarticKelvins; const ARight: TCubicKelvins): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TQuarticKelvins; const ARight: TKelvins): TCubicKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]

operator *(const ALeft: TSquareKelvins; const ARight: TSquareKelvins): TQuarticKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TQuarticKelvins; const ARight: TSquareKelvins): TSquareKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of Mole }

class function TMoleUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%smol', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'mol';
end;

class function TMoleUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%smole', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'mole';
end;

class function TMoleUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

{ Unit of Candela }

class function TCandelaUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%scd', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'cd';
end;

class function TCandelaUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%scandela', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'candela';
end;

class function TCandelaUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

{ Unit of Radian }

class function TRadianUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'rad';
end;

class function TRadianUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'radian';
end;

class function TRadianUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
end;

{ Unit of Degree }

class function TDegreeUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'deg';
end;

class function TDegreeUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'degree';
end;

class function TDegreeUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
end;

{ Unit of Steradian }

class function TSteradianUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'sr';
end;

class function TSteradianUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  result := 'steradian';
end;

class function TSteradianUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
end;

// main definition [ sr ] = [ rad ] * [ rad ]

operator *(const ALeft: TRadians; const ARight: TRadians): TSteradians;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSteradians; const ARight: TRadians): TRadians;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of Hertz }

class function THertzUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sHz', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'Hz';
end;

class function THertzUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%shertz', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'hertz';
end;

class function THertzUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

// main definition [ Hz ] = [ 1 ] / [ s ]

operator /(const ALeft: double; const ARight: TSeconds): THertz;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: THertz): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THertz; const ARight: TSeconds): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: double; const ARight: THertz): TSeconds;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator /(const ALeft: double; const {%H-}ARight: TSecondUnitId): THertz;
begin
  result.FValue := ALeft;
end;

{ Unit of SquareHertz }

class function TSquareHertzUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sHz2', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'Hz2';
end;

class function TSquareHertzUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('square %shertz', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'square hertz';
end;

class function TSquareHertzUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 2);
  end;
end;

// main definition [ Hz2 ] = [ 1 ] / [ s2 ]

operator /(const ALeft: double; const ARight: TSquareSeconds): TSquareHertz;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TSquareHertz): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareHertz; const ARight: TSquareSeconds): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TSquareHertz): TSquareSeconds;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator /(const ALeft: double; const {%H-}ARight: TSquareSecondUnitId): TSquareHertz;
begin
  result.FValue := ALeft;
end;

// alternative definition [ Hz2 ] = [ Hz ] / [ s ]

operator /(const ALeft: THertz; const ARight: TSeconds): TSquareHertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TSquareHertz): THertz;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareHertz; const ARight: TSeconds): THertz;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: THertz; const ARight: TSquareHertz): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ Hz2 ] = [ Hz ] * [ Hz ]

operator *(const ALeft: THertz; const ARight: THertz): TSquareHertz;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareHertz; const ARight: THertz): THertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of RadianPerSecond }

class function TRadianPerSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('rad/%ss', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'rad/s';
end;

class function TRadianPerSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('radian per %ssecond', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'radian per second';
end;

class function TRadianPerSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result * PrefixTable[APrefixes[0]].Factor;
  end;
end;

// main definition [ rad/s ] = [ rad ] / [ s ]

operator /(const ALeft: TRadians; const ARight: TSeconds): TRadiansPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TRadiansPerSecond): TRadians;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TRadiansPerSecond; const ARight: TSeconds): TRadians;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TRadians; const ARight: TRadiansPerSecond): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TRadians; const {%H-}ARight: TSecondUnitId): TRadiansPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ rad/s ] = [ rad ] * [ Hz ]

operator *(const ALeft: TRadians; const ARight: THertz): TRadiansPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THertz; const ARight: TRadians): TRadiansPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TRadiansPerSecond; const ARight: TRadians): THertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TRadiansPerSecond; const ARight: THertz): TRadians;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator :=(const AQuantity: TRadiansPerSecond): THertz; inline;
begin
  result.FValue := AQuantity.FValue;
end;

operator :=(const AQuantity: THertz): TRadiansPerSecond; inline;
begin
  result.FValue := AQuantity.FValue;
end;

{ Unit of RadianPerSquareSecond }

class function TRadianPerSquareSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('rad/%ss2', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'rad/s2';
end;

class function TRadianPerSquareSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('radian per square %ssecond', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'radian per square second';
end;

class function TRadianPerSquareSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[0]].Factor, 2);
  end;
end;

// main definition [ rad/s2 ] = [ rad ] / [ s2 ]

operator /(const ALeft: TRadians; const ARight: TSquareSeconds): TRadiansPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TRadiansPerSquareSecond): TRadians;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TRadiansPerSquareSecond; const ARight: TSquareSeconds): TRadians;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TRadians; const ARight: TRadiansPerSquareSecond): TSquareSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TRadians; const {%H-}ARight: TSquareSecondUnitId): TRadiansPerSquareSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ rad/s2 ] = [ rad ] * [ Hz2 ]

operator *(const ALeft: TRadians; const ARight: TSquareHertz): TRadiansPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareHertz; const ARight: TRadians): TRadiansPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TRadiansPerSquareSecond; const ARight: TRadians): TSquareHertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TRadiansPerSquareSecond; const ARight: TSquareHertz): TRadians;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of SteradianPerSquareSecond }

class function TSteradianPerSquareSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('rad2/%ss2', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'rad2/s2';
end;

class function TSteradianPerSquareSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('square rad per square %ssecond', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'square rad per square second';
end;

class function TSteradianPerSquareSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[0]].Factor, 2);
  end;
end;

// main definition [ sr/s2 ] = [ sr ] / [ s2 ]

operator /(const ALeft: TSteradians; const ARight: TSquareSeconds): TSteradiansPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TSteradiansPerSquareSecond): TSteradians;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSteradiansPerSquareSecond; const ARight: TSquareSeconds): TSteradians;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSteradians; const ARight: TSteradiansPerSquareSecond): TSquareSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSteradians; const {%H-}ARight: TSquareSecondUnitId): TSteradiansPerSquareSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ sr/s2 ] = [ sr ] * [ Hz2 ]

operator *(const ALeft: TSteradians; const ARight: TSquareHertz): TSteradiansPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareHertz; const ARight: TSteradians): TSteradiansPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSteradiansPerSquareSecond; const ARight: TSteradians): TSquareHertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSteradiansPerSquareSecond; const ARight: TSquareHertz): TSteradians;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of MeterPerSecond }

class function TMeterPerSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sm/%ss', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'm/s';
end;

class function TMeterPerSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%smeter per %ssecond', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'meter per second';
end;

class function TMeterPerSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ m/s ] = [ m ] / [ s ]

operator /(const ALeft: TMeters; const ARight: TSeconds): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TMetersPerSecond): TMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TSeconds): TMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TMeters; const ARight: TMetersPerSecond): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMeters; const {%H-}ARight: TSecondUnitId): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ m/s ] = [ m ] * [ Hz ]

operator *(const ALeft: TMeters; const ARight: THertz): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THertz; const ARight: TMeters): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TMetersPerSecond; const ARight: TMeters): THertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMetersPerSecond; const ARight: THertz): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const {%H-}ARight: THertzUnitId): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of MeterPerHour }

class function TMeterPerHourUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sm/h', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'm/h';
end;

class function TMeterPerHourUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%smeter per hour', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'meter per hour';
end;

class function TMeterPerHourUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

{ Unit of MeterPerSquareSecond }

class function TMeterPerSquareSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sm/%ss2', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'm/s2';
end;

class function TMeterPerSquareSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%smeter per square %ssecond', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'meter per square second';
end;

class function TMeterPerSquareSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[1]].Factor, 2);
  end;
end;

// main definition [ m/s2 ] = [ m ] / [ s2 ]

operator /(const ALeft: TMeters; const ARight: TSquareSeconds): TMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TMetersPerSquareSecond): TMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSquareSecond; const ARight: TSquareSeconds): TMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TMeters; const ARight: TMetersPerSquareSecond): TSquareSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMeters; const {%H-}ARight: TSquareSecondUnitId): TMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ m/s2 ] = [ m/s ] / [ s ]

operator /(const ALeft: TMetersPerSecond; const ARight: TSeconds): TMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TMetersPerSquareSecond): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSquareSecond; const ARight: TSeconds): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TMetersPerSecond; const ARight: TMetersPerSquareSecond): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMetersPerSecond; const {%H-}ARight: TSecondUnitId): TMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ m/s2 ] = [ Hz2 ] * [ m ]

operator *(const ALeft: TSquareHertz; const ARight: TMeters): TMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TSquareHertz): TMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TMetersPerSquareSecond; const ARight: TSquareHertz): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMetersPerSquareSecond; const ARight: TMeters): TSquareHertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of MeterPerSecondPerSecond }

class function TMeterPerSecondPerSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%sm/%ss/%ss', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol, PrefixTable[APrefixes[2]].Symbol])
  else
    result := 'm/s/s';
end;

class function TMeterPerSecondPerSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%smeter per %ssecond per %ssecond', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name, PrefixTable[APrefixes[2]].Name])
  else
    result := 'meter per second per second';
end;

class function TMeterPerSecondPerSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 3 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
    if (APrefixes[2] <> pNone) then result := result * PrefixTable[APrefixes[2]].Factor;
  end;
end;

{ Unit of MeterPerHourPerSecond }

class function TMeterPerHourPerSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sm/h/%ss', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'm/h/s';
end;

class function TMeterPerHourPerSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%smeter per hour per %ssecond', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'meter per hour per second';
end;

class function TMeterPerHourPerSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

{ Unit of SquareMeterPerSquareSecond }

class function TSquareMeterPerSquareSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sm2/%ss2', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'm2/s2';
end;

class function TSquareMeterPerSquareSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('square %smeter per square %ssecond', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'square meter per square second';
end;

class function TSquareMeterPerSquareSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 2);
    if (APrefixes[1] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[1]].Factor, 2);
  end;
end;

// main definition [ m2/s2 ] = [ m2 ] / [ s2 ]

operator /(const ALeft: TSquareMeters; const ARight: TSquareSeconds): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TSquareMetersPerSquareSecond): TSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TSquareSeconds): TSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareSecond): TSquareSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSquareSecondUnitId): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ m2/s2 ] = [ m/s ] / [ m/s ]

operator *(const ALeft: TMetersPerSecond; const ARight: TMetersPerSecond): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSecond): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ m2/s2 ] = [ m/s2 ] * [ m ]

operator *(const ALeft: TMetersPerSquareSecond; const ARight: TMeters): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TMetersPerSquareSecond): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSquareSecond): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMeters): TMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of KilogramMeterPerSecond }

class function TKilogramMeterPerSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%sg.%sm/%ss', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol, PrefixTable[APrefixes[2]].Symbol])
  else
    result := 'kg.m/s';
end;

class function TKilogramMeterPerSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%sgram %smeter per %ssecond', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name, PrefixTable[APrefixes[2]].Name])
  else
    result := 'kilogram meter per second';
end;

class function TKilogramMeterPerSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 3 then
  begin
    if (APrefixes[0] <> pKilo) then
      result := result * 1E+03 / PrefixTable[APrefixes[0]].Factor;

    if (APrefixes[1] <> pNone) then result := result / PrefixTable[APrefixes[1]].Factor;
    if (APrefixes[2] <> pNone) then result := result * PrefixTable[APrefixes[2]].Factor;
  end;
end;

// main definition [ kg*m/s ] = [kg ] * [ m/s ]

operator *(const ALeft: TKilograms; const ARight: TMetersPerSecond): TKilogramMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TKilograms): TKilogramMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TKilograms): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TMetersPerSecond): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilograms; const {%H-}ARight: TMeterPerSecondUnitId): TKilogramMetersPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of NewtonSecond }

class function TNewtonSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sN.%ss', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'N.s';
end;

class function TNewtonSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%snewton %ssecond', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'newton second';
end;

class function TNewtonSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result / PrefixTable[APrefixes[1]].Factor;
  end;
end;

{ Unit of KilogramSquareMeter }

class function TKilogramSquareMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sg.%sm2', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'kg.m2';
end;

class function TKilogramSquareMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sgram square %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'kilogram square meter';
end;

class function TKilogramSquareMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pKilo) then
      result := result * 1E+03 / PrefixTable[APrefixes[0]].Factor;

    if (APrefixes[1] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[1]].Factor, 2);
  end;
end;

// main definition [ kg*m2 ] = [ kg ] * [ m2 ]

operator *(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TKilograms): TKilogramSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMeters; const ARight: TKilograms): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMeters; const ARight: TSquareMeters): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilograms; const {%H-}ARight: TSquareMeterUnitId): TKilogramSquareMeters;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of KilogramSquareMeterPerSecond }

class function TKilogramSquareMeterPerSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%sg.%sm2/%ss', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol, PrefixTable[APrefixes[2]].Symbol])
  else
    result := 'kg.m2/s';
end;

class function TKilogramSquareMeterPerSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%sgram square %smeter per %ssecond', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name, PrefixTable[APrefixes[2]].Name])
  else
    result := 'kilogram square meter per second';
end;

class function TKilogramSquareMeterPerSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 3 then
  begin
    if (APrefixes[0] <> pKilo) then
      result := result * 1E+03 / PrefixTable[APrefixes[0]].Factor;

    if (APrefixes[1] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[1]].Factor, 2);
    if (APrefixes[2] <> pNone) then result := result * PrefixTable[APrefixes[2]].Factor;
  end;
end;

// main definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]

operator /(const ALeft: TKilogramSquareMeters; const ARight: TSeconds): TKilogramSquareMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TKilogramSquareMetersPerSecond): TKilogramSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TSeconds): TKilogramSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMeters; const ARight: TKilogramSquareMetersPerSecond): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMeters; const {%H-}ARight: TSecondUnitId): TKilogramSquareMetersPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] * [ Hz ]

operator *(const ALeft: TKilogramSquareMeters; const ARight: THertz): TKilogramSquareMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THertz; const ARight: TKilogramSquareMeters): TKilogramSquareMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TKilogramSquareMeters): THertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMetersPerSecond; const ARight: THertz): TKilogramSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of KilogramPerMeter }

class function TKilogramPerMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sg/%sm', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'kg/m';
end;

class function TKilogramPerMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sgram per %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'kilogram per meter';
end;

class function TKilogramPerMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pKilo) then
      result := result * 1E+03 / PrefixTable[APrefixes[0]].Factor;

    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ kg/m ] = [ kg ] / [ m ]

operator /(const ALeft: TKilograms; const ARight: TMeters): TKilogramsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TKilogramsPerMeter): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerMeter; const ARight: TMeters): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilograms; const ARight: TKilogramsPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilograms; const {%H-}ARight: TMeterUnitId): TKilogramsPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of KilogramPerSquareMeter }

class function TKilogramPerSquareMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sg/%sm2', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'kg/m2';
end;

class function TKilogramPerSquareMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sgram per square %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'kilogram per square meter';
end;

class function TKilogramPerSquareMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pKilo) then
      result := result * 1E+03 / PrefixTable[APrefixes[0]].Factor;

    if (APrefixes[1] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[1]].Factor, 2);
  end;
end;

// main definition [ kg/m2 ] = [ kg ] / [ m2 ]

operator /(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramsPerSquareMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TKilogramsPerSquareMeter): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerSquareMeter; const ARight: TSquareMeters): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilograms; const ARight: TKilogramsPerSquareMeter): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilograms; const {%H-}ARight: TSquareMeterUnitId): TKilogramsPerSquareMeter;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of KilogramPerCubicMeter }

class function TKilogramPerCubicMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sg/%sm3', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'kg/m3';
end;

class function TKilogramPerCubicMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sgram per cubic %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'kilogram per cubic meter';
end;

class function TKilogramPerCubicMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pKilo) then
      result := result * 1E+03 / PrefixTable[APrefixes[0]].Factor;

    if (APrefixes[1] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[1]].Factor, 3);
  end;
end;

// main definition [ kg/m3 ] = [ kg ] / [ m3 ]

operator /(const ALeft: TKilograms; const ARight: TCubicMeters): TKilogramsPerCubicMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TCubicMeters; const ARight: TKilogramsPerCubicMeter): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TCubicMeters): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilograms; const ARight: TKilogramsPerCubicMeter): TCubicMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilograms; const {%H-}ARight: TCubicMeterUnitId): TKilogramsPerCubicMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ kg/m3 ] = [ kg/m2 ] / [ m ]

operator /(const ALeft: TKilogramsPerSquareMeter; const ARight: TMeters): TKilogramsPerCubicMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TKilogramsPerCubicMeter): TKilogramsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TMeters): TKilogramsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramsPerSquareMeter; const ARight: TKilogramsPerCubicMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of Newton }

class function TNewtonUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sN', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'N';
end;

class function TNewtonUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%snewton', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'newton';
end;

class function TNewtonUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

// main definition [ N ] = [ kg ] * [ m/s2 ]

operator *(const ALeft: TKilograms; const ARight: TMetersPerSquareSecond): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSquareSecond; const ARight: TKilograms): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TKilograms): TMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TMetersPerSquareSecond): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilograms; const {%H-}ARight: TMeterPerSquareSecondUnitId): TNewtons;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ N ] = [ kg/m ] * [ m2/s2 ]

operator *(const ALeft: TKilogramsPerMeter; const ARight: TSquareMetersPerSquareSecond): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilogramsPerMeter): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TKilogramsPerMeter): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareSecond): TKilogramsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N ] = [ kg*m/s ] / [ s ]

operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TSeconds): TNewtons;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TNewtons): TKilogramMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtons; const ARight: TSeconds): TKilogramMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TNewtons): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of Pascal }

class function TPascalUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sPa', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'Pa';
end;

class function TPascalUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%spascal', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'pascal';
end;

class function TPascalUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

// main definition [ Pa ] = [ N ] / [ m2 ]

operator /(const ALeft: TNewtons; const ARight: TSquareMeters): TPascals;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TPascals): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TPascals; const ARight: TSquareMeters): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TPascals): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TSquareMeterUnitId): TPascals;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ Pa ] = [ kg/m3 ] * [ m2/s2 ]

operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSquareSecond): TPascals;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilogramsPerCubicMeter): TPascals;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TPascals; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TPascals; const ARight: TSquareMetersPerSquareSecond): TKilogramsPerCubicMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of Joule }

class function TJouleUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sJ', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'J';
end;

class function TJouleUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sjoule', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'joule';
end;

class function TJouleUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

// main definition [ J ] = [ N ] * [ m ]

operator *(const ALeft: TNewtons; const ARight: TMeters): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TNewtons): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TNewtons): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TMeters): TNewtons;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TNewtons; const {%H-}ARight: TMeterUnitId): TJoules;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ J ] = [ Pa ] * [ m3 ]

operator *(const ALeft: TPascals; const ARight: TCubicMeters): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TCubicMeters; const ARight: TPascals): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TPascals): TCubicMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TCubicMeters): TPascals;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ J ] = [ kg*m/s ] * [ m/s ]

operator *(const ALeft: TKilogramMetersPerSecond; const ARight: TMetersPerSecond): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TKilogramMetersPerSecond): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TKilogramMetersPerSecond): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TMetersPerSecond): TKilogramMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ J ] = [ kg ] * [ m2/s2 ]

operator *(const ALeft: TKilograms; const ARight: TSquareMetersPerSquareSecond): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilograms): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TKilograms): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TSquareMetersPerSquareSecond): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const {%H-}ARight: TKilogramUnitId): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ J ] = [ kg*m2 ] * [ Hz2 ]

operator *(const ALeft: TKilogramSquareMeters; const ARight: TSquareHertz): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareHertz; const ARight: TKilogramSquareMeters): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TKilogramSquareMeters): TSquareHertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TSquareHertz): TKilogramSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ J ] = [ kg*m2 ] / [ s2 ]

operator /(const ALeft: TKilogramSquareMeters; const ARight: TSquareSeconds): TJoules;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TJoules): TKilogramSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJoules; const ARight: TSquareSeconds): TKilogramSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMeters; const ARight: TJoules): TSquareSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of Elettronvolt }

class function TElettronvoltUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%seV', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'eV';
end;

class function TElettronvoltUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%selettronvolt ', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'elettronvolt ';
end;

class function TElettronvoltUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

{ Unit of Watt }

class function TWattUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sW', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'W';
end;

class function TWattUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%swatt', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'watt';
end;

class function TWattUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

// main definition [ W ] = [ J ] / [ s ]

operator /(const ALeft: TJoules; const ARight: TSeconds): TWatts;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TWatts): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWatts; const ARight: TSeconds): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TWatts): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const {%H-}ARight: TSecondUnitId): TWatts;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ W ] = [ J ] * [ Hz ]

operator *(const ALeft: TJoules; const ARight: THertz): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THertz; const ARight: TJoules): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TJoules): THertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: THertz): TJoules;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ W ] = [ N ] * [ m/s ]

operator *(const ALeft: TNewtons; const ARight: TMetersPerSecond): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TNewtons): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TNewtons): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TMetersPerSecond): TNewtons;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of Coulomb }

class function TCoulombUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sC', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'C';
end;

class function TCoulombUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%scoulomb', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'coulomb';
end;

class function TCoulombUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

// main definition [ C ] = [ s ] * [ A ]

operator *(const ALeft: TSeconds; const ARight: TAmperes): TCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TAmperes; const ARight: TSeconds): TCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCoulombs; const ARight: TSeconds): TAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombs; const ARight: TAmperes): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const {%H-}ARight: TAmpereUnitId): TCoulombs;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of SquareCoulomb }

class function TSquareCoulombUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sC2', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'C2';
end;

class function TSquareCoulombUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('square %scoulomb', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'square coulomb';
end;

class function TSquareCoulombUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 2);
  end;
end;

// main definition [ C2 ] = [ C ] * [ C ]

operator *(const ALeft: TCoulombs; const ARight: TCoulombs): TSquareCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareCoulombs; const ARight: TCoulombs): TCoulombs;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of Volt }

class function TVoltUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sV', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'V';
end;

class function TVoltUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%svolt', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'volt';
end;

class function TVoltUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

// main definition [ V ] = [ W ] / [ A ]

operator /(const ALeft: TWatts; const ARight: TAmperes): TVolts;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TAmperes; const ARight: TVolts): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TVolts; const ARight: TAmperes): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TVolts): TAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWatts; const {%H-}ARight: TAmpereUnitId): TVolts;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ V ] = [ J ] / [ C ]

operator /(const ALeft: TJoules; const ARight: TCoulombs): TVolts;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TCoulombs; const ARight: TVolts): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TVolts; const ARight: TCoulombs): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TVolts): TCoulombs;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const {%H-}ARight: TCoulombUnitId): TVolts;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of SquareVolt }

class function TSquareVoltUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sV2', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'V2';
end;

class function TSquareVoltUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('square %svolt', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'square volt';
end;

class function TSquareVoltUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 2);
  end;
end;

// main definition [ V2 ] = [ V ] * [ V ]

operator *(const ALeft: TVolts; const ARight: TVolts): TSquareVolts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareVolts; const ARight: TVolts): TVolts;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of Farad }

class function TFaradUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sF', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'F';
end;

class function TFaradUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sfarad', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'farad';
end;

class function TFaradUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

// main definition [ F ] = [ C ] / [ V ]

operator /(const ALeft: TCoulombs; const ARight: TVolts): TFarads;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TVolts; const ARight: TFarads): TCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TFarads; const ARight: TVolts): TCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCoulombs; const ARight: TFarads): TVolts;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombs; const {%H-}ARight: TVoltUnitId): TFarads;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ F ] = [ C2 ] / [ J ]

operator /(const ALeft: TSquareCoulombs; const ARight: TJoules): TFarads;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TJoules; const ARight: TFarads): TSquareCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TFarads; const ARight: TJoules): TSquareCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareCoulombs; const ARight: TFarads): TJoules;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of Ohm }

class function TOhmUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sΩ', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'Ω';
end;

class function TOhmUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sohm', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'ohm';
end;

class function TOhmUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

// main definition [ Ω ] = [ V ] / [ A ]

operator /(const ALeft: TVolts; const ARight: TAmperes): TOhms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TAmperes; const ARight: TOhms): TVolts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TOhms; const ARight: TAmperes): TVolts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TVolts; const ARight: TOhms): TAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TVolts; const {%H-}ARight: TAmpereUnitId): TOhms;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ Ω ] = [ s ] / [ F ]

operator /(const ALeft: TSeconds; const ARight: TFarads): TOhms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TFarads; const ARight: TOhms): TSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TOhms; const ARight: TFarads): TSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSeconds; const ARight: TOhms): TFarads;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ Ω ] = [ W ] / [ A2 ]

operator /(const ALeft: TWatts; const ARight: TSquareAmperes): TOhms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareAmperes; const ARight: TOhms): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TOhms; const ARight: TSquareAmperes): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TOhms): TSquareAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ Ω ] = [ V2 ] / [ W ]

operator /(const ALeft: TSquareVolts; const ARight: TWatts): TOhms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TWatts; const ARight: TOhms): TSquareVolts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TOhms; const ARight: TWatts): TSquareVolts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareVolts; const ARight: TOhms): TWatts;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of Siemens }

class function TSiemensUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sS', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'S';
end;

class function TSiemensUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%ssiemens', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'siemens';
end;

class function TSiemensUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

// main definition [ S ] = 1 / [ Ω ]

operator /(const ALeft: double; const ARight: TOhms): TSiemens;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator *(const ALeft: TOhms; const ARight: TSiemens): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSiemens; const ARight: TOhms): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TSiemens): TOhms;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator /(const ALeft: double; const {%H-}ARight: TOhmUnitId): TSiemens;
begin
  result.FValue := ALeft;
end;

{ Unit of Weber }

class function TWeberUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sWb', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'Wb';
end;

class function TWeberUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sweber', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'weber';
end;

class function TWeberUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

// main definition [ Wb ] = [ V ] * [ s ]

operator *(const ALeft: TVolts; const ARight: TSeconds): TWebers;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TVolts): TWebers;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWebers; const ARight: TVolts): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWebers; const ARight: TSeconds): TVolts;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TVolts; const {%H-}ARight: TSecondUnitId): TWebers;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of Tesla }

class function TTeslaUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sT', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'T';
end;

class function TTeslaUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%stesla', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'tesla';
end;

class function TTeslaUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

// main definition [ T ] = [ Wb ] / [ m2 ]

operator /(const ALeft: TWebers; const ARight: TSquareMeters): TTeslas;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TTeslas): TWebers;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TTeslas; const ARight: TSquareMeters): TWebers;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWebers; const ARight: TTeslas): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWebers; const {%H-}ARight: TSquareMeterUnitId): TTeslas;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of Henry }

class function THenryUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sH', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'H';
end;

class function THenryUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%shenry', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'henry';
end;

class function THenryUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

// main definition [ H ] = [ Wb ] / [ A ]

operator /(const ALeft: TWebers; const ARight: TAmperes): THenrys;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TAmperes; const ARight: THenrys): TWebers;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THenrys; const ARight: TAmperes): TWebers;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWebers; const ARight: THenrys): TAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWebers; const {%H-}ARight: TAmpereUnitId): THenrys;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ H ] = [ Ω ] * [ s ]

operator *(const ALeft: TOhms; const ARight: TSeconds): THenrys;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TOhms): THenrys;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: THenrys; const ARight: TOhms): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: THenrys; const ARight: TSeconds): TOhms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ H ] = [ Ω ] / [ Hz ]

operator /(const ALeft: TOhms; const ARight: THertz): THenrys;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: THertz; const ARight: THenrys): TOhms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THenrys; const ARight: THertz): TOhms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TOhms; const ARight: THenrys): THertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of Lumen }

class function TLumenUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%slm', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'lm';
end;

class function TLumenUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%slumen', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'lumen';
end;

class function TLumenUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

// main definition [ lm ] = [ cd ] * [ sr ]

operator *(const ALeft: TCandelas; const ARight: TSteradians): TLumens;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSteradians; const ARight: TCandelas): TLumens;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TLumens; const ARight: TCandelas): TSteradians;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TLumens; const ARight: TSteradians): TCandelas;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TCandelas; const {%H-}ARight: TSteradianUnitId): TLumens;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of Lux }

class function TLuxUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%slx', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'lx';
end;

class function TLuxUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%slux', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'lux';
end;

class function TLuxUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

// main definition [ lx ] = [ lm ] / [ m2 ]

operator /(const ALeft: TLumens; const ARight: TSquareMeters): TLux;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TLux): TLumens;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TLux; const ARight: TSquareMeters): TLumens;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TLumens; const ARight: TLux): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TLumens; const {%H-}ARight: TSquareMeterUnitId): TLux;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of Bequerel }

class function TBequerelUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sBq', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'Bq';
end;

class function TBequerelUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sbequerel', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'bequerel';
end;

class function TBequerelUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

{ Unit of Gray }

class function TGrayUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sGy', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'Gy';
end;

class function TGrayUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sgray', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'gray';
end;

class function TGrayUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

{ Unit of Sievert }

class function TSievertUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sSv', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'Sv';
end;

class function TSievertUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%ssievert', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'sievert';
end;

class function TSievertUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

{ Unit of Katal }

class function TKatalUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%skat', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'kat';
end;

class function TKatalUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%skatal', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'katal';
end;

class function TKatalUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

// main definition [ kat ] = [ mol ] / [ s ]

operator /(const ALeft: TMoles; const ARight: TSeconds): TKatals;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TKatals): TMoles;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKatals; const ARight: TSeconds): TMoles;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TMoles; const ARight: TKatals): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMoles; const {%H-}ARight: TSecondUnitId): TKatals;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of NewtonMeter }

class function TNewtonMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sN.%sm', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'N.m';
end;

class function TNewtonMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%snewton %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'newton meter';
end;

class function TNewtonMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result / PrefixTable[APrefixes[1]].Factor;
  end;
end;

{ Unit of JoulePerRadian }

class function TJoulePerRadianUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sJ/rad', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'J/rad';
end;

class function TJoulePerRadianUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sjoule per radian', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'joule per radian';
end;

class function TJoulePerRadianUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

// main definition [ J/rad ] = [ J ] / [ rad ]

operator /(const ALeft: TJoules; const ARight: TRadians): TJoulesPerRadian;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TRadians; const ARight: TJoulesPerRadian): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJoulesPerRadian; const ARight: TRadians): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerRadian): TRadians;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const {%H-}ARight: TRadianUnitId): TJoulesPerRadian;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of JoulePerDegree }

class function TJoulePerDegreeUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sJ/deg', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'J/deg';
end;

class function TJoulePerDegreeUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sjoule per degree', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'joule per degree';
end;

class function TJoulePerDegreeUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

{ Unit of NewtonMeterPerRadian }

class function TNewtonMeterPerRadianUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sN.%sm/rad', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'N.m/rad';
end;

class function TNewtonMeterPerRadianUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%snewton %smeter per radian', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'newton meter per radian';
end;

class function TNewtonMeterPerRadianUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result / PrefixTable[APrefixes[1]].Factor;
  end;
end;

{ Unit of NewtonMeterPerDegree }

class function TNewtonMeterPerDegreeUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sN.%sm/deg', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'N.m/deg';
end;

class function TNewtonMeterPerDegreeUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%snewton %smeter per degree', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'newton meter per degree';
end;

class function TNewtonMeterPerDegreeUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result / PrefixTable[APrefixes[1]].Factor;
  end;
end;

{ Unit of NewtonPerCubicMeter }

class function TNewtonPerCubicMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sN/%sm3', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'N/m3';
end;

class function TNewtonPerCubicMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%snewton per cubic %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'newton per cubic meter';
end;

class function TNewtonPerCubicMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[1]].Factor, 3);
  end;
end;

// main definition [ N/m3 ] = [ N ] / [ m3 ]

operator /(const ALeft: TNewtons; const ARight: TCubicMeters): TNewtonsPerCubicMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TCubicMeters; const ARight: TNewtonsPerCubicMeter): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonsPerCubicMeter; const ARight: TCubicMeters): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TNewtonsPerCubicMeter): TCubicMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TCubicMeterUnitId): TNewtonsPerCubicMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ N/m3 ] = [ Pa ] / [ m ]

operator /(const ALeft: TPascals; const ARight: TMeters): TNewtonsPerCubicMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TNewtonsPerCubicMeter): TPascals;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonsPerCubicMeter; const ARight: TMeters): TPascals;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TPascals; const ARight: TNewtonsPerCubicMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N/m3 ] = [ kg/m3 ] * [ m/s2 ]

operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TMetersPerSquareSecond): TNewtonsPerCubicMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSquareSecond; const ARight: TKilogramsPerCubicMeter): TNewtonsPerCubicMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonsPerCubicMeter; const ARight: TKilogramsPerCubicMeter): TMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonsPerCubicMeter; const ARight: TMetersPerSquareSecond): TKilogramsPerCubicMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of NewtonPerMeter }

class function TNewtonPerMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sN/%sm', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'N/m';
end;

class function TNewtonPerMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%snewton per %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'newton per meter';
end;

class function TNewtonPerMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ N/m ] = [ N ] / [ m ]

operator /(const ALeft: TNewtons; const ARight: TMeters): TNewtonsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TNewtonsPerMeter): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonsPerMeter; const ARight: TMeters): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TNewtonsPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TMeterUnitId): TNewtonsPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ N/m ] = [ J ] / [ m2 ]

operator /(const ALeft: TJoules; const ARight: TSquareMeters): TNewtonsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerMeter): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonsPerMeter; const ARight: TSquareMeters): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TNewtonsPerMeter): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N/m ] = [ Pa ] * [ m ]

operator *(const ALeft: TPascals; const ARight: TMeters): TNewtonsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TPascals): TNewtonsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonsPerMeter; const ARight: TPascals): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonsPerMeter; const ARight: TMeters): TPascals;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N/m ] = [ kg ] * [ Hz2 ]

operator *(const ALeft: TKilograms; const ARight: TSquareHertz): TNewtonsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareHertz; const ARight: TKilograms): TNewtonsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonsPerMeter; const ARight: TKilograms): TSquareHertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonsPerMeter; const ARight: TSquareHertz): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of CubicMeterPerSecond }

class function TCubicMeterPerSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sm3/%ss', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'm3/s';
end;

class function TCubicMeterPerSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('cubic %smeter per %ssecond', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'cubic meter per second';
end;

class function TCubicMeterPerSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 3);
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ m3/s ] = [ m3 ] / [ s ]

operator /(const ALeft: TCubicMeters; const ARight: TSeconds): TCubicMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TCubicMetersPerSecond): TCubicMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TCubicMetersPerSecond; const ARight: TSeconds): TCubicMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerSecond): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCubicMeters; const {%H-}ARight: TSecondUnitId): TCubicMetersPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ m3/s ] = [ m2 ] * [ m/s ]

operator *(const ALeft: TSquareMeters; const ARight: TMetersPerSecond): TCubicMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TSquareMeters): TCubicMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMetersPerSecond; const ARight: TSquareMeters): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCubicMetersPerSecond; const ARight: TMetersPerSecond): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of KilogramPerSecond }

class function TKilogramPerSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sg/%ss', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'kg/s';
end;

class function TKilogramPerSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sgram per %ssecond', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'kilogram per second';
end;

class function TKilogramPerSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pKilo) then
      result := result * 1E+03 / PrefixTable[APrefixes[0]].Factor;

    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ kg/s ] = [ kg ] / [ s ]

operator /(const ALeft: TKilograms; const ARight: TSeconds): TKilogramsPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TKilogramsPerSecond): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerSecond; const ARight: TSeconds): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilograms; const ARight: TKilogramsPerSecond): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilograms; const {%H-}ARight: TSecondUnitId): TKilogramsPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ kg/s ] = [ N ] / [ m/s ]

operator /(const ALeft: TNewtons; const ARight: TMetersPerSecond): TKilogramsPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TKilogramsPerSecond): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerSecond; const ARight: TMetersPerSecond): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TKilogramsPerSecond): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of Poiseuille }

class function TPoiseuilleUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sPl', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'Pl';
end;

class function TPoiseuilleUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%spoiseuille ', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'poiseuille ';
end;

class function TPoiseuilleUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
  end;
end;

// main definition [ Pl ] = [ Pa ] * [ s ]

operator *(const ALeft: TPascals; const ARight: TSeconds): TPoiseuilles;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TPascals): TPoiseuilles;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TPoiseuilles; const ARight: TPascals): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TPoiseuilles; const ARight: TSeconds): TPascals;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TPascals; const {%H-}ARight: TSecondUnitId): TPoiseuilles;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ Pl ] = [ kg/m2 ] * [ m/s ]

operator *(const ALeft: TKilogramsPerSquareMeter; const ARight: TMetersPerSecond): TPoiseuilles;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TKilogramsPerSquareMeter): TPoiseuilles;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TPoiseuilles; const ARight: TKilogramsPerSquareMeter): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TPoiseuilles; const ARight: TMetersPerSecond): TKilogramsPerSquareMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ Pl ] = [ kg/s ] / [ m ]

operator /(const ALeft: TKilogramsPerSecond; const ARight: TMeters): TPoiseuilles;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TPoiseuilles): TKilogramsPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TPoiseuilles; const ARight: TMeters): TKilogramsPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramsPerSecond; const ARight: TPoiseuilles): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TPoiseuilles; const {%H-}ARight: TMeterUnitId): TKilogramsPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of PascalSecond }

class function TPascalSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sPa.%ss', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'Pa.s';
end;

class function TPascalSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%spascal %ssecond', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'pascal second';
end;

class function TPascalSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result / PrefixTable[APrefixes[1]].Factor;
  end;
end;

{ Unit of SquareMeterPerSecond }

class function TSquareMeterPerSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sm2/%ss', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'm2/s';
end;

class function TSquareMeterPerSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('square %smeter per %ssecond', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'square meter per second';
end;

class function TSquareMeterPerSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 2);
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ m2/s ] = [ m2 ] / [ s ]

operator /(const ALeft: TSquareMeters; const ARight: TSeconds): TSquareMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TSquareMetersPerSecond): TSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSecond; const ARight: TSeconds): TSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSecond): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSecondUnitId): TSquareMetersPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ m2/s ] = [ Pl ] / [ kg/m3 ]

operator /(const ALeft: TPoiseuilles; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSecond): TPoiseuilles;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSecond; const ARight: TKilogramsPerCubicMeter): TPoiseuilles;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TPoiseuilles; const ARight: TSquareMetersPerSecond): TKilogramsPerCubicMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of KilogramPerQuarticMeter }

class function TKilogramPerQuarticMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sg/%sm4', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'kg/m4';
end;

class function TKilogramPerQuarticMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sgram per quartic %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'kilogram per quartic meter';
end;

class function TKilogramPerQuarticMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pKilo) then
      result := result * 1E+03 / PrefixTable[APrefixes[0]].Factor;

    if (APrefixes[1] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[1]].Factor, 4);
  end;
end;

// main definition [ kg/m4 ] = [ kg ] / [ m4 ]

operator /(const ALeft: TKilograms; const ARight: TQuarticMeters): TKilogramsPerQuarticMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TQuarticMeters; const ARight: TKilogramsPerQuarticMeter): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerQuarticMeter; const ARight: TQuarticMeters): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilograms; const ARight: TKilogramsPerQuarticMeter): TQuarticMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilograms; const {%H-}ARight: TQuarticMeterUnitId): TKilogramsPerQuarticMeter;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of QuarticMeterSecond }

class function TQuarticMeterSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sm4.%ss', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'm4.s';
end;

class function TQuarticMeterSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('quartic %smeter %ssecond', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'quartic meter second';
end;

class function TQuarticMeterSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 4);
    if (APrefixes[1] <> pNone) then result := result / PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ m4*s ] = [ m4 ] * [ s ]

operator *(const ALeft: TQuarticMeters; const ARight: TSeconds): TQuarticMeterSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TQuarticMeters): TQuarticMeterSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TQuarticMeterSeconds; const ARight: TQuarticMeters): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TQuarticMeterSeconds; const ARight: TSeconds): TQuarticMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TQuarticMeters; const {%H-}ARight: TSecondUnitId): TQuarticMeterSeconds;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of KilogramPerQuarticMeterPerSecond }

class function TKilogramPerQuarticMeterPerSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%sg/%sm4/%ss', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol, PrefixTable[APrefixes[2]].Symbol])
  else
    result := 'kg/m4/s';
end;

class function TKilogramPerQuarticMeterPerSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%sgram per quartic %smeter per %ssecond', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name, PrefixTable[APrefixes[2]].Name])
  else
    result := 'kilogram per quartic meter per second';
end;

class function TKilogramPerQuarticMeterPerSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 3 then
  begin
    if (APrefixes[0] <> pKilo) then
      result := result * 1E+03 / PrefixTable[APrefixes[0]].Factor;

    if (APrefixes[1] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[1]].Factor, 4);
    if (APrefixes[2] <> pNone) then result := result * PrefixTable[APrefixes[2]].Factor;
  end;
end;

// main definition [ kg/m4/s ] = [ kg/s ] / [ m4 ]

operator /(const ALeft: TKilogramsPerSecond; const ARight: TQuarticMeters): TKilogramsPerQuarticMeterPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TQuarticMeters; const ARight: TKilogramsPerQuarticMeterPerSecond): TKilogramsPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TQuarticMeters): TKilogramsPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramsPerSecond; const ARight: TKilogramsPerQuarticMeterPerSecond): TQuarticMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramsPerSecond; const {%H-}ARight: TQuarticMeterUnitId): TKilogramsPerQuarticMeterPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ kg/m4/s ] = [ kg/m4 ] / [ s ]

operator /(const ALeft: TKilogramsPerQuarticMeter; const ARight: TSeconds): TKilogramsPerQuarticMeterPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TKilogramsPerQuarticMeterPerSecond): TKilogramsPerQuarticMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TSeconds): TKilogramsPerQuarticMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramsPerQuarticMeter; const ARight: TKilogramsPerQuarticMeterPerSecond): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramsPerQuarticMeter; const {%H-}ARight: TSecondUnitId): TKilogramsPerQuarticMeterPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ kg/m4/s ] = [ kg ] / [ m4*s ]

operator /(const ALeft: TKilograms; const ARight: TQuarticMeterSeconds): TKilogramsPerQuarticMeterPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TQuarticMeterSeconds; const ARight: TKilogramsPerQuarticMeterPerSecond): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TQuarticMeterSeconds): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilograms; const ARight: TKilogramsPerQuarticMeterPerSecond): TQuarticMeterSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ kg/m4/s ] = [ Pa ] / [ m3/s ]

operator /(const ALeft: TPascals; const ARight: TCubicMetersPerSecond): TKilogramsPerQuarticMeterPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TCubicMetersPerSecond; const ARight: TKilogramsPerQuarticMeterPerSecond): TPascals;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TCubicMetersPerSecond): TPascals;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TPascals; const ARight: TKilogramsPerQuarticMeterPerSecond): TCubicMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of CubicMeterPerKilogram }

class function TCubicMeterPerKilogramUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sm3/%sg', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'm3/kg';
end;

class function TCubicMeterPerKilogramUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('cubic %smeter per %sgram', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'cubic meter per kilogram';
end;

class function TCubicMeterPerKilogramUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 3);
    if (APrefixes[1] <> pKilo) then
      result := result * 1E+03 * PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ m3/kg ] = [ m3 ] / [ kg ]

operator /(const ALeft: TCubicMeters; const ARight: TKilograms): TCubicMetersPerKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilograms; const ARight: TCubicMetersPerKilogram): TCubicMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TCubicMetersPerKilogram; const ARight: TKilograms): TCubicMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerKilogram): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCubicMeters; const {%H-}ARight: TKilogramUnitId): TCubicMetersPerKilogram;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ m3/kg ] = 1 / [ kg/m3 ]

operator /(const ALeft: double; const ARight: TKilogramsPerCubicMeter): TCubicMetersPerKilogram;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TCubicMetersPerKilogram): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TCubicMetersPerKilogram; const ARight: TKilogramsPerCubicMeter): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TCubicMetersPerKilogram): TKilogramsPerCubicMeter;
begin
  result.FValue := ALeft / ARight.FValue;
end;

{ Unit of KilogramSquareSecond }

class function TKilogramSquareSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sg.%ss2', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'kg.s2';
end;

class function TKilogramSquareSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sgram square %ssecond', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'kilogram square second';
end;

class function TKilogramSquareSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pKilo) then
      result := result * 1E+03 / PrefixTable[APrefixes[0]].Factor;

    if (APrefixes[1] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[1]].Factor, 2);
  end;
end;

// main definition [ kg*s2 ] = [ kg ] * [ s2 ]

operator *(const ALeft: TKilograms; const ARight: TSquareSeconds): TKilogramSquareSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TKilograms): TKilogramSquareSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareSeconds; const ARight: TKilograms): TSquareSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareSeconds; const ARight: TSquareSeconds): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilograms; const {%H-}ARight: TSquareSecondUnitId): TKilogramSquareSeconds;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of CubicMeterPerSquareSecond }

class function TCubicMeterPerSquareSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sm3/%ss2', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'm3/s2';
end;

class function TCubicMeterPerSquareSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('cubic %smeter per square %ssecond', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'cubic meter per square second';
end;

class function TCubicMeterPerSquareSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 3);
    if (APrefixes[1] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[1]].Factor, 2);
  end;
end;

// main definitio [ m3/s2 ] = [ m3 ] / [ s2 ]

operator /(const ALeft: TCubicMeters; const ARight: TSquareSeconds): TCubicMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TCubicMetersPerSquareSecond): TCubicMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TCubicMetersPerSquareSecond; const ARight: TSquareSeconds): TCubicMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerSquareSecond): TSquareSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCubicMeters; const {%H-}ARight: TSquareSecondUnitId): TCubicMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ m3/s2 ] = [ m/s2 ] * [ m2 ]

operator *(const ALeft: TMetersPerSquareSecond; const ARight: TSquareMeters): TCubicMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TMetersPerSquareSecond): TCubicMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TMetersPerSquareSecond): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TSquareMeters): TMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of NewtonSquareMeter }

class function TNewtonSquareMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sN.%sm2', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'N.m2';
end;

class function TNewtonSquareMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%snewton square %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'newton square meter';
end;

class function TNewtonSquareMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[1]].Factor, 2);
  end;
end;

// main definition [ N*m2 ] = [ N ] * [ m2 ]

operator *(const ALeft: TNewtons; const ARight: TSquareMeters): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TNewtons): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtons): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareMeters): TNewtons;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TNewtons; const {%H-}ARight: TSquareMeterUnitId): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ N*m2 ] = [ J ] * [ m ]

operator *(const ALeft: TJoules; const ARight: TMeters): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TJoules): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TJoules): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TMeters): TJoules;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N*m2 ] = [ Pa ] * [ m4 ]

operator *(const ALeft: TPascals; const ARight: TQuarticMeters): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TQuarticMeters; const ARight: TPascals): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TPascals): TQuarticMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TQuarticMeters): TPascals;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of NewtonPerSquareKilogram }

class function TNewtonPerSquareKilogramUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sN/%sg2', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'N/kg2';
end;

class function TNewtonPerSquareKilogramUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%snewton per square %sgram', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'newton per square kilogram';
end;

class function TNewtonPerSquareKilogramUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pKilo) then
      result := result * 1E+06 * IntPower(PrefixTable[APrefixes[1]].Factor, 2);
  end;
end;

// main definition [ N/kg2 ] = [ N ] / [ kg2 ]

operator /(const ALeft: TNewtons; const ARight: TSquareKilograms): TNewtonsPerSquareKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareKilograms; const ARight: TNewtonsPerSquareKilogram): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareKilograms): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareKilogram): TSquareKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TSquareKilogramUnitId): TNewtonsPerSquareKilogram;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of SquareKilogramPerMeter }

class function TSquareKilogramPerMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sg2/%sm', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'kg2/m';
end;

class function TSquareKilogramPerMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('square %sgram per %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'square kilogram per meter';
end;

class function TSquareKilogramPerMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pKilo) then
      result := result * 1E+06 / IntPower(PrefixTable[APrefixes[0]].Factor, 2);

    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ kg2/m ] = [ kg2 ] / [ m ]

operator /(const ALeft: TSquareKilograms; const ARight: TMeters): TSquareKilogramsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TSquareKilogramsPerMeter): TSquareKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareKilogramsPerMeter; const ARight: TMeters): TSquareKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareKilograms; const {%H-}ARight: TMeterUnitId): TSquareKilogramsPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of SquareKilogramPerSquareMeter }

class function TSquareKilogramPerSquareMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sg2/%sm2', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'kg2/m2';
end;

class function TSquareKilogramPerSquareMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('square %sgram per square %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'square kilogram per square meter';
end;

class function TSquareKilogramPerSquareMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pKilo) then
      result := result * 1E+06 / IntPower(PrefixTable[APrefixes[0]].Factor, 2);

    if (APrefixes[1] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[1]].Factor, 2);
  end;
end;

// main definition [ kg2/m2 ] = [ kg2 ] / [ m2 ]

operator /(const ALeft: TSquareKilograms; const ARight: TSquareMeters): TSquareKilogramsPerSquareMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TSquareKilogramsPerSquareMeter): TSquareKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareKilogramsPerSquareMeter; const ARight: TSquareMeters): TSquareKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerSquareMeter): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareKilograms; const {%H-}ARight: TSquareMeterUnitId): TSquareKilogramsPerSquareMeter;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of SquareMeterPerSquareKilogram }

class function TSquareMeterPerSquareKilogramUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sm2/%sg2', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'm2/kg2';
end;

class function TSquareMeterPerSquareKilogramUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('square %smeter per square %sgram', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'square meter per square kilogram';
end;

class function TSquareMeterPerSquareKilogramUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 2);
    if (APrefixes[1] <> pKilo) then
      result := result * 1E+06 * IntPower(PrefixTable[APrefixes[1]].Factor, 2);
  end;
end;

// main definition [ m2/kg2 ] = [ m2 ] / [ kg2 ]

operator /(const ALeft: TSquareMeters; const ARight: TSquareKilograms): TSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareKilograms; const ARight: TSquareMetersPerSquareKilogram): TSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSquareKilogram; const ARight: TSquareKilograms): TSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareKilogram): TSquareKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSquareKilogramUnitId): TSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of NewtonSquareMeterPerSquareKilogram }

class function TNewtonSquareMeterPerSquareKilogramUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%sN.%sm2/%sg2', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol, PrefixTable[APrefixes[2]].Symbol])
  else
    result := 'N.m2/kg2';
end;

class function TNewtonSquareMeterPerSquareKilogramUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%snewton square %smeter per square %sgram', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name, PrefixTable[APrefixes[2]].Name])
  else
    result := 'newton square meter per square kilogram';
end;

class function TNewtonSquareMeterPerSquareKilogramUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 3 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[1]].Factor, 2);
    if (APrefixes[2] <> pKilo) then
      result := result * 1E+06 * IntPower(PrefixTable[APrefixes[2]].Factor, 2);
  end;
end;

// main definition [ N*m2/kg2 ] = [ N ] * [ m2/kg2 ]

operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareKilogram): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSquareKilogram; const ARight: TNewtons): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TNewtons): TSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareMetersPerSquareKilogram): TNewtons;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N*m2/kg2 ] = [ N ] / [ kg2/m2 ]

operator /(const ALeft: TNewtons; const ARight: TSquareKilogramsPerSquareMeter): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareKilogramsPerSquareMeter; const ARight: TNewtonSquareMetersPerSquareKilogram): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilogramsPerSquareMeter): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilogramsPerSquareMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N*m2/kg2 ] = [ N*m2 ] / [ kg2 ]

operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareKilograms): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareKilograms; const ARight: TNewtonSquareMetersPerSquareKilogram): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilograms): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeters; const {%H-}ARight: TSquareKilogramUnitId): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ N*m2/kg2 ] = [ N/kg2 ] * [ m2 ]

operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareMeters): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareKilogram): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TNewtonsPerSquareKilogram): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareMeters): TNewtonsPerSquareKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TNewtonsPerSquareKilogram; const {%H-}ARight: TSquareMeterUnitId): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ N*m2/kg2 ] = [ J ] / [ kg2/m ]

operator /(const ALeft: TJoules; const ARight: TSquareKilogramsPerMeter): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareKilogramsPerMeter; const ARight: TNewtonSquareMetersPerSquareKilogram): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilogramsPerMeter): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilogramsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N*m2/kg2 ] = [ m3/kg ] / [ s2 ]

operator /(const ALeft: TCubicMetersPerKilogram; const ARight: TSquareSeconds): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TNewtonSquareMetersPerSquareKilogram): TCubicMetersPerKilogram;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareSeconds): TCubicMetersPerKilogram;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMetersPerKilogram; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N*m2/kg2 ] = [ m3 ] / [ kg*s2 ]

operator /(const ALeft: TCubicMeters; const ARight: TKilogramSquareSeconds): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilogramSquareSeconds; const ARight: TNewtonSquareMetersPerSquareKilogram): TCubicMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TKilogramSquareSeconds): TCubicMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMeters; const ARight: TNewtonSquareMetersPerSquareKilogram): TKilogramSquareSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N*m2/kg2 ] = [ m3/s2 ] / [ kg ]

operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TKilograms): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilograms; const ARight: TNewtonSquareMetersPerSquareKilogram): TCubicMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TKilograms): TCubicMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TNewtonSquareMetersPerSquareKilogram): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of ReciprocalKelvin }

class function TReciprocalKelvinUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('1/%sK', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := '1/K';
end;

class function TReciprocalKelvinUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('reciprocal %skelvin', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'reciprocal kelvin';
end;

class function TReciprocalKelvinUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result * PrefixTable[APrefixes[0]].Factor;
  end;
end;

// main definition [ 1/K ] = 1 / [ K ]

operator /(const ALeft: double; const ARight: TKelvins): TReciprocalKelvins;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TReciprocalKelvins): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TReciprocalKelvins; const ARight: TKelvins): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TReciprocalKelvins): TKelvins;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator /(const ALeft: double; const {%H-}ARight: TKelvinUnitId): TReciprocalKelvins;
begin
  result.FValue := ALeft;
end;

{ Unit of KilogramKelvin }

class function TKilogramKelvinUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sg.%sK', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'kg.K';
end;

class function TKilogramKelvinUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sgram %skelvin', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'kilogram kelvin';
end;

class function TKilogramKelvinUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pKilo) then
      result := result * 1E+03 / PrefixTable[APrefixes[0]].Factor;

    if (APrefixes[1] <> pNone) then result := result / PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ kg*K] = [ kg ] * [ K ]

operator *(const ALeft: TKilograms; const ARight: TKelvins): TKilogramKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TKilograms): TKilogramKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramKelvins; const ARight: TKilograms): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramKelvins; const ARight: TKelvins): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilograms; const {%H-}ARight: TKelvinUnitId): TKilogramKelvins;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of JoulePerKelvin }

class function TJoulePerKelvinUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sJ/%sK', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'J/K';
end;

class function TJoulePerKelvinUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sjoule per %skelvin', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'joule per kelvin';
end;

class function TJoulePerKelvinUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ J/K ] = [ J ] / [ K ]

operator /(const ALeft: TJoules; const ARight: TKelvins): TJoulesPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TJoulesPerKelvin): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJoulesPerKelvin; const ARight: TKelvins): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerKelvin): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const {%H-}ARight: TKelvinUnitId): TJoulesPerKelvin;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of JoulePerKilogram }

class function TJoulePerKilogramUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sJ/%sg', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'J/kg';
end;

class function TJoulePerKilogramUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sjoule per %sgram', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'joule per kilogram';
end;

class function TJoulePerKilogramUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pKilo) then
      result := result * 1E+03 * PrefixTable[APrefixes[1]].Factor;
  end;
end;

{ Unit of JoulePerKilogramPerKelvin }

class function TJoulePerKilogramPerKelvinUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%sJ/%sg/%sK', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol, PrefixTable[APrefixes[2]].Symbol])
  else
    result := 'J/kg/K';
end;

class function TJoulePerKilogramPerKelvinUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%sjoule per %sgram per %skelvin', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name, PrefixTable[APrefixes[2]].Name])
  else
    result := 'joule per kilogram per kelvin';
end;

class function TJoulePerKilogramPerKelvinUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 3 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pKilo) then
      result := result * 1E+03 * PrefixTable[APrefixes[1]].Factor;

    if (APrefixes[2] <> pNone) then result := result * PrefixTable[APrefixes[2]].Factor;
  end;
end;

// main definition [ J/kg/K ] = [ J ] / [ kg*K ]

operator /(const ALeft: TJoules; const ARight: TKilogramKelvins): TJoulesPerKilogramPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilogramKelvins; const ARight: TJoulesPerKilogramPerKelvin): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKilogramKelvins): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerKilogramPerKelvin): TKilogramKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ J/kg/K ] = [ J/kg ] / [ K ]

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKelvins): TJoulesPerKilogramPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TJoulesPerKilogramPerKelvin): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKelvins): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TJoulesPerKilogramPerKelvin): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const {%H-}ARight: TKelvinUnitId): TJoulesPerKilogramPerKelvin;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ J/kg/K ] = [ J/K ] / [ kg ]

operator /(const ALeft: TJoulesPerKelvin; const ARight: TKilograms): TJoulesPerKilogramPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilograms; const ARight: TJoulesPerKilogramPerKelvin): TJoulesPerKelvin;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKilograms): TJoulesPerKelvin;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoulesPerKelvin; const ARight: TJoulesPerKilogramPerKelvin): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoulesPerKelvin; const {%H-}ARight: TKilogramUnitId): TJoulesPerKilogramPerKelvin;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of MeterKelvin }

class function TMeterKelvinUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sm.%sK', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'm.K';
end;

class function TMeterKelvinUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%smeter %skelvin', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'meter kelvin';
end;

class function TMeterKelvinUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result / PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ m*K ] = [ m ] * [ K ]

operator *(const ALeft: TMeters; const ARight: TKelvins): TMeterKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TMeters): TMeterKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TMeterKelvins; const ARight: TMeters): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMeterKelvins; const ARight: TKelvins): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const {%H-}ARight: TKelvinUnitId): TMeterKelvins;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of KelvinPerMeter }

class function TKelvinPerMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sK/%sm', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'K/m';
end;

class function TKelvinPerMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%skelvin per %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'kelvin per meter';
end;

class function TKelvinPerMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ K/m ] = [ K ] / [ m ]

operator /(const ALeft: TKelvins; const ARight: TMeters): TKelvinsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TKelvinsPerMeter): TKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKelvinsPerMeter; const ARight: TMeters): TKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKelvins; const ARight: TKelvinsPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKelvins; const {%H-}ARight: TMeterUnitId): TKelvinsPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of WattPerMeter }

class function TWattPerMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sW/%sm', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'W/m';
end;

class function TWattPerMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%swatt per %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'watt per meter';
end;

class function TWattPerMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ W/m ] = [ W ] / [ m ]

operator /(const ALeft: TWatts; const ARight: TMeters): TWattsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TWattsPerMeter): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerMeter; const ARight: TMeters): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWatts; const {%H-}ARight: TMeterUnitId): TWattsPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of WattPerSquareMeter }

class function TWattPerSquareMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sW/%sm2', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'W/m2';
end;

class function TWattPerSquareMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%swatt per square %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'watt per square meter';
end;

class function TWattPerSquareMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[1]].Factor, 2);
  end;
end;

// main definition [ W/m2 ] = [ W ] / [ m2 ]

operator /(const ALeft: TWatts; const ARight: TSquareMeters): TWattsPerSquareMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeter): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerSquareMeter; const ARight: TSquareMeters): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeter): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWatts; const {%H-}ARight: TSquareMeterUnitId): TWattsPerSquareMeter;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of WattPerKelvin }

class function TWattPerKelvinUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sW/%sK', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'W/K';
end;

class function TWattPerKelvinUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%swatt per %skelvin', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'watt per kelvin';
end;

class function TWattPerKelvinUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ W/K ] = [ W ] / [ K ]

operator /(const ALeft: TWatts; const ARight: TKelvins): TWattsPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TWattsPerKelvin): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerKelvin; const ARight: TKelvins): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerKelvin): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWatts; const {%H-}ARight: TKelvinUnitId): TWattsPerKelvin;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of WattPerMeterPerKelvin }

class function TWattPerMeterPerKelvinUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%sW/%sm/%sK', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol, PrefixTable[APrefixes[2]].Symbol])
  else
    result := 'W/m/K';
end;

class function TWattPerMeterPerKelvinUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%swatt per %smeter per %skelvin', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name, PrefixTable[APrefixes[2]].Name])
  else
    result := 'watt per meter per kelvin';
end;

class function TWattPerMeterPerKelvinUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 3 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
    if (APrefixes[2] <> pNone) then result := result * PrefixTable[APrefixes[2]].Factor;
  end;
end;

// main definition [ W/m/K ] = [ W ] / [ m*K ]

operator /(const ALeft: TWatts; const ARight: TMeterKelvins): TWattsPerMeterPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeterKelvins; const ARight: TWattsPerMeterPerKelvin): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TMeterKelvins): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerMeterPerKelvin): TMeterKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ W/m/K ] = [ W/m ] / [ K ]

operator /(const ALeft: TWattsPerMeter; const ARight: TKelvins): TWattsPerMeterPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TWattsPerMeterPerKelvin): TWattsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TKelvins): TWattsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWattsPerMeter; const ARight: TWattsPerMeterPerKelvin): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattsPerMeter; const {%H-}ARight: TKelvinUnitId): TWattsPerMeterPerKelvin;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ W/m/K ] = [ W/K ] / [ m ]

operator /(const ALeft: TWattsPerKelvin; const ARight: TMeters): TWattsPerMeterPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TWattsPerMeterPerKelvin): TWattsPerKelvin;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TMeters): TWattsPerKelvin;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWattsPerKelvin; const ARight: TWattsPerMeterPerKelvin): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattsPerKelvin; const {%H-}ARight: TMeterUnitId): TWattsPerMeterPerKelvin;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ W/m/K ] = [ W/m2 ] / [ K/m ]

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TKelvinsPerMeter): TWattsPerMeterPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKelvinsPerMeter; const ARight: TWattsPerMeterPerKelvin): TWattsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TKelvinsPerMeter): TWattsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerMeterPerKelvin): TKelvinsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of SquareMeterKelvin }

class function TSquareMeterKelvinUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sm2.%sK', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'm2.K';
end;

class function TSquareMeterKelvinUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('square %smeter %skelvin', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'square meter kelvin';
end;

class function TSquareMeterKelvinUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 2);
    if (APrefixes[1] <> pNone) then result := result / PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ m2*K ] = [ m2 ] * [ K ]

operator *(const ALeft: TSquareMeters; const ARight: TKelvins): TSquareMeterKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TSquareMeters): TSquareMeterKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMeterKelvins; const ARight: TSquareMeters): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeterKelvins; const ARight: TKelvins): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const {%H-}ARight: TKelvinUnitId): TSquareMeterKelvins;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of WattPerSquareMeterPerKelvin }

class function TWattPerSquareMeterPerKelvinUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%sW/%sm2/%sK', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol, PrefixTable[APrefixes[2]].Symbol])
  else
    result := 'W/m2/K';
end;

class function TWattPerSquareMeterPerKelvinUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%swatt per square %smeter per %skelvin', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name, PrefixTable[APrefixes[2]].Name])
  else
    result := 'watt per square meter per kelvin';
end;

class function TWattPerSquareMeterPerKelvinUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 3 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[1]].Factor, 2);
    if (APrefixes[2] <> pNone) then result := result * PrefixTable[APrefixes[2]].Factor;
  end;
end;

// main definition [ W/m2/K ] = [ W ] / [ m2*K ]

operator /(const ALeft: TWatts; const ARight: TSquareMeterKelvins): TWattsPerSquareMeterPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeterKelvins; const ARight: TWattsPerSquareMeterPerKelvin): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TSquareMeterKelvins): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerKelvin): TSquareMeterKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ W/m2/K ] = [ W/m2 ] / [ K ]

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TKelvins): TWattsPerSquareMeterPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TWattsPerSquareMeterPerKelvin): TWattsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TKelvins): TWattsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerSquareMeterPerKelvin): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattsPerSquareMeter; const {%H-}ARight: TKelvinUnitId): TWattsPerSquareMeterPerKelvin;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ W/m2/K ] = [ W/K ] / [ m2 ]

operator /(const ALeft: TWattsPerKelvin; const ARight: TSquareMeters): TWattsPerSquareMeterPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerKelvin): TWattsPerKelvin;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TSquareMeters): TWattsPerKelvin;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWattsPerKelvin; const ARight: TWattsPerSquareMeterPerKelvin): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattsPerKelvin; const {%H-}ARight: TSquareMeterUnitId): TWattsPerSquareMeterPerKelvin;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of SquareMeterQuarticKelvin }

class function TSquareMeterQuarticKelvinUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sm2.%sK4', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'm2.K4';
end;

class function TSquareMeterQuarticKelvinUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('square %smeter quartic %skelvin', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'square meter quartic kelvin';
end;

class function TSquareMeterQuarticKelvinUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 2);
    if (APrefixes[1] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[1]].Factor, 4);
  end;
end;

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]

operator *(const ALeft: TSquareMeters; const ARight: TQuarticKelvins): TSquareMeterQuarticKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TQuarticKelvins; const ARight: TSquareMeters): TSquareMeterQuarticKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMeterQuarticKelvins; const ARight: TSquareMeters): TQuarticKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeterQuarticKelvins; const ARight: TQuarticKelvins): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const {%H-}ARight: TQuarticKelvinUnitId): TSquareMeterQuarticKelvins;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of WattPerQuarticKelvin }

class function TWattPerQuarticKelvinUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sW/%sK4', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'W/K4';
end;

class function TWattPerQuarticKelvinUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%swatt per quartic %skelvin', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'watt per quartic kelvin';
end;

class function TWattPerQuarticKelvinUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[1]].Factor, 4);
  end;
end;

// main definition [ W/K4 ] = [ W ] / [ K4 ]

operator /(const ALeft: TWatts; const ARight: TQuarticKelvins): TWattsPerQuarticKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TQuarticKelvins; const ARight: TWattsPerQuarticKelvin): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerQuarticKelvin; const ARight: TQuarticKelvins): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerQuarticKelvin): TQuarticKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWatts; const {%H-}ARight: TQuarticKelvinUnitId): TWattsPerQuarticKelvin;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of WattPerSquareMeterPerQuarticKelvin }

class function TWattPerSquareMeterPerQuarticKelvinUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%sW/%sm2/%sK4', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol, PrefixTable[APrefixes[2]].Symbol])
  else
    result := 'W/m2/K4';
end;

class function TWattPerSquareMeterPerQuarticKelvinUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%swatt per square %smeter per quartic %skelvin', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name, PrefixTable[APrefixes[2]].Name])
  else
    result := 'watt per square meter per quartic kelvin';
end;

class function TWattPerSquareMeterPerQuarticKelvinUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 3 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[1]].Factor, 2);
    if (APrefixes[2] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[2]].Factor, 4);
  end;
end;

// main definition [ W/m2/K4 ] = [ W ] / [ m2*K4 ]

operator /(const ALeft: TWatts; const ARight: TSquareMeterQuarticKelvins): TWattsPerSquareMeterPerQuarticKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeterQuarticKelvins; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TSquareMeterQuarticKelvins): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TSquareMeterQuarticKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ W/m2/K4 ] = [ W/m2 ] / [ K4 ]

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TQuarticKelvins): TWattsPerSquareMeterPerQuarticKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TQuarticKelvins; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWattsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TQuarticKelvins): TWattsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TQuarticKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattsPerSquareMeter; const {%H-}ARight: TQuarticKelvinUnitId): TWattsPerSquareMeterPerQuarticKelvin;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ W/m2/K4 ] = [ W/K4 ] / [ m2 ]

operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TSquareMeters): TWattsPerSquareMeterPerQuarticKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWattsPerQuarticKelvin;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TSquareMeters): TWattsPerQuarticKelvin;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattsPerQuarticKelvin; const {%H-}ARight: TSquareMeterUnitId): TWattsPerSquareMeterPerQuarticKelvin;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of JoulePerMole }

class function TJoulePerMoleUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sJ/%smol', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'J/mol';
end;

class function TJoulePerMoleUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sjoule per %smole', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'joule per mole';
end;

class function TJoulePerMoleUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ J/mol ] = [ J ] / [ mol ]

operator /(const ALeft: TJoules; const ARight: TMoles): TJoulesPerMole;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMoles; const ARight: TJoulesPerMole): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJoulesPerMole; const ARight: TMoles): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerMole): TMoles;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const {%H-}ARight: TMoleUnitId): TJoulesPerMole;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of MoleKelvin }

class function TMoleKelvinUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%smol.%sK', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'mol.K';
end;

class function TMoleKelvinUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%smole %skelvin', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'mole kelvin';
end;

class function TMoleKelvinUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result / PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ mol*K ] = [ mol ] * [ K ]

operator *(const ALeft: TMoles; const ARight: TKelvins): TMoleKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TMoles): TMoleKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TMoleKelvins; const ARight: TMoles): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMoleKelvins; const ARight: TKelvins): TMoles;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMoles; const {%H-}ARight: TKelvinUnitId): TMoleKelvins;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of JoulePerMolePerKelvin }

class function TJoulePerMolePerKelvinUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%sJ/%smol/%sK', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol, PrefixTable[APrefixes[2]].Symbol])
  else
    result := 'J/mol/K';
end;

class function TJoulePerMolePerKelvinUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%sjoule per %smole per %skelvin', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name, PrefixTable[APrefixes[2]].Name])
  else
    result := 'joule per mole per kelvin';
end;

class function TJoulePerMolePerKelvinUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 3 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
    if (APrefixes[2] <> pNone) then result := result * PrefixTable[APrefixes[2]].Factor;
  end;
end;

// main definition [ J/mol/K ] = [ J ] / [ mol * K ]

operator /(const ALeft: TJoules; const ARight: TMoleKelvins): TJoulesPerMolePerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMoleKelvins; const ARight: TJoulesPerMolePerKelvin): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TMoleKelvins): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerMolePerKelvin): TMoleKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ J/mol/K ] = [ J/K ] / [ mol ]

operator /(const ALeft: TJoulesPerKelvin; const ARight: TMoles): TJoulesPerMolePerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMoles; const ARight: TJoulesPerMolePerKelvin): TJoulesPerKelvin;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TMoles): TJoulesPerKelvin;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoulesPerKelvin; const ARight: TJoulesPerMolePerKelvin): TMoles;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoulesPerKelvin; const {%H-}ARight: TMoleUnitId): TJoulesPerMolePerKelvin;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ J/mol/K ] = [ J/mol ] / [ K ]

operator /(const ALeft: TJoulesPerMole; const ARight: TKelvins): TJoulesPerMolePerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TJoulesPerMolePerKelvin): TJoulesPerMole;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TKelvins): TJoulesPerMole;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoulesPerMole; const ARight: TJoulesPerMolePerKelvin): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoulesPerMole; const {%H-}ARight: TKelvinUnitId): TJoulesPerMolePerKelvin;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of OhmMeter }

class function TOhmMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sΩ.%sm', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'Ω.m';
end;

class function TOhmMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sohm %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'ohm meter';
end;

class function TOhmMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result / PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ Ω*m ] = [ Ω ] * [ m ]

operator *(const ALeft: TOhms; const ARight: TMeters): TOhmMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TOhms): TOhmMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TOhmMeters; const ARight: TOhms): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TOhmMeters; const ARight: TMeters): TOhms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TOhms; const {%H-}ARight: TMeterUnitId): TOhmMeters;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of VoltPerMeter }

class function TVoltPerMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sV/%sm', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'V/m';
end;

class function TVoltPerMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%svolt per %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'volt per meter';
end;

class function TVoltPerMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ V/m ] = [ V ] / [ m ]

operator /(const ALeft: TVolts; const ARight: TMeters): TVoltsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TVoltsPerMeter): TVolts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TVoltsPerMeter; const ARight: TMeters): TVolts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TVolts; const ARight: TVoltsPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TVolts; const {%H-}ARight: TMeterUnitId): TVoltsPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ V/m ] = [ N ] / [ C ]

operator /(const ALeft: TNewtons; const ARight: TCoulombs): TVoltsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TCoulombs; const ARight: TVoltsPerMeter): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TVoltsPerMeter; const ARight: TCoulombs): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TVoltsPerMeter): TCoulombs;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TCoulombUnitId): TVoltsPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ V/m ] = [ T ] * [ m/s ]

operator *(const ALeft: TTeslas; const ARight: TMetersPerSecond): TVoltsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TTeslas): TVoltsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TVoltsPerMeter; const ARight: TTeslas): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TVoltsPerMeter; const ARight: TMetersPerSecond): TTeslas;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of NewtonPerCoulomb }

class function TNewtonPerCoulombUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sN/%sC', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'N/C';
end;

class function TNewtonPerCoulombUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%snewton per %scoulomb', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'newton per coulomb';
end;

class function TNewtonPerCoulombUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

{ Unit of CoulombPerMeter }

class function TCoulombPerMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sC/%sm', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'C/m';
end;

class function TCoulombPerMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%scoulomb per %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'coulomb per meter';
end;

class function TCoulombPerMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ C/m ] = [ C ] / [ m ]

operator /(const ALeft: TCoulombs; const ARight: TMeters): TCoulombsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TCoulombsPerMeter): TCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TCoulombsPerMeter; const ARight: TMeters): TCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombs; const {%H-}ARight: TMeterUnitId): TCoulombsPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of SquareCoulombPerMeter }

class function TSquareCoulombPerMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sC2/%sm', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'C2/m';
end;

class function TSquareCoulombPerMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('square %scoulomb per %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'square coulomb per meter';
end;

class function TSquareCoulombPerMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 2);
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ C2/m ] = [ C2 ] / [ m ]

operator /(const ALeft: TSquareCoulombs; const ARight: TMeters): TSquareCoulombsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TSquareCoulombsPerMeter): TSquareCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareCoulombsPerMeter; const ARight: TMeters): TSquareCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareCoulombs; const ARight: TSquareCoulombsPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareCoulombs; const {%H-}ARight: TMeterUnitId): TSquareCoulombsPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ C2/m ] = [ C/m ] * [ C ]

operator *(const ALeft: TCoulombsPerMeter; const ARight: TCoulombs): TSquareCoulombsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TCoulombs; const ARight: TCoulombsPerMeter): TSquareCoulombsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareCoulombsPerMeter; const ARight: TCoulombsPerMeter): TCoulombs;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareCoulombsPerMeter; const ARight: TCoulombs): TCoulombsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of CoulombPerSquareMeter }

class function TCoulombPerSquareMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sC/%sm2', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'C/m2';
end;

class function TCoulombPerSquareMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%scoulomb per square %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'coulomb per square meter';
end;

class function TCoulombPerSquareMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[1]].Factor, 2);
  end;
end;

// main definition [ C/m2 ] = [ C ] / [ m2 ]

operator /(const ALeft: TCoulombs; const ARight: TSquareMeters): TCoulombsPerSquareMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TCoulombsPerSquareMeter): TCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TSquareMeters): TCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerSquareMeter): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombs; const {%H-}ARight: TSquareMeterUnitId): TCoulombsPerSquareMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ C/m2 ] = [ C/m ] / [ m ]

operator /(const ALeft: TCoulombsPerMeter; const ARight: TMeters): TCoulombsPerSquareMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TCoulombsPerSquareMeter): TCoulombsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TMeters): TCoulombsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCoulombsPerMeter; const ARight: TCoulombsPerSquareMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of SquareMeterPerSquareCoulomb }

class function TSquareMeterPerSquareCoulombUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sm2/%sC2', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'm2/C2';
end;

class function TSquareMeterPerSquareCoulombUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('square %smeter per square %scoulomb', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'square meter per square coulomb';
end;

class function TSquareMeterPerSquareCoulombUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 2);
    if (APrefixes[1] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[1]].Factor, 2);
  end;
end;

// main definition [ m2/C2 ] = [ m2 ] / [ C2 ]

operator /(const ALeft: TSquareMeters; const ARight: TSquareCoulombs): TSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareCoulombs; const ARight: TSquareMetersPerSquareCoulomb): TSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombs): TSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareCoulomb): TSquareCoulombs;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSquareCoulombUnitId): TSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of NewtonPerSquareCoulomb }

class function TNewtonPerSquareCoulombUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sN/%sC2', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'N/C2';
end;

class function TNewtonPerSquareCoulombUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%snewton per square %scoulomb', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'newton per square coulomb';
end;

class function TNewtonPerSquareCoulombUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[1]].Factor, 2);
  end;
end;

// main definition [ N/C2 ] = [ N ] / [ C2 ]

operator /(const ALeft: TNewtons; const ARight: TSquareCoulombs): TNewtonsPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareCoulombs; const ARight: TNewtonsPerSquareCoulomb): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareCoulombs): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareCoulomb): TSquareCoulombs;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TSquareCoulombUnitId): TNewtonsPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of NewtonSquareMeterPerSquareCoulomb }

class function TNewtonSquareMeterPerSquareCoulombUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%sN.%sm2/%sC2', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol, PrefixTable[APrefixes[2]].Symbol])
  else
    result := 'N.m2/C2';
end;

class function TNewtonSquareMeterPerSquareCoulombUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%snewton square %smeter per square %scoulomb', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name, PrefixTable[APrefixes[2]].Name])
  else
    result := 'newton square meter per square coulomb';
end;

class function TNewtonSquareMeterPerSquareCoulombUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 3 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[1]].Factor, 2);
    if (APrefixes[2] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[2]].Factor, 2);
  end;
end;

// main definition [ N*m2/C2 ] = [ N ] * [ m2/C2 ]

operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareCoulomb): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSquareCoulomb; const ARight: TNewtons): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TNewtons): TSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareMetersPerSquareCoulomb): TNewtons;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N*m2/C2 ] = [ N*m2 ] / [ C2 ]

operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareCoulombs): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareCoulombs; const ARight: TNewtonSquareMetersPerSquareCoulomb): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombs): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtonSquareMetersPerSquareCoulomb): TSquareCoulombs;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeters; const {%H-}ARight: TSquareCoulombUnitId): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ N*m2/C2 ] = [ N/C2 ] * [ m2 ]

operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareMeters): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareCoulomb): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TNewtonsPerSquareCoulomb): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareMeters): TNewtonsPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TNewtonsPerSquareCoulomb; const {%H-}ARight: TSquareMeterUnitId): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ N*m2/C2 ] = [ V/m ] / [ C/m2 ]

operator /(const ALeft: TVoltsPerMeter; const ARight: TCoulombsPerSquareMeter): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TVoltsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TCoulombsPerSquareMeter): TVoltsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TVoltsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TCoulombsPerSquareMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N*m2/C2 ] = [ J ] / [ C2/m ]

operator /(const ALeft: TJoules; const ARight: TSquareCoulombsPerMeter): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareCoulombsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombsPerMeter): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TNewtonSquareMetersPerSquareCoulomb): TSquareCoulombsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of VoltMeter }

class function TVoltMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sV.%sm', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'V.m';
end;

class function TVoltMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%svolt %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'volt meter';
end;

class function TVoltMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result / PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ V*m ] = [ V ] * [ m ]

operator *(const ALeft: TVolts; const ARight: TMeters): TVoltMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TVolts): TVoltMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TVoltMeters; const ARight: TVolts): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TVoltMeters; const ARight: TMeters): TVolts;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TVolts; const {%H-}ARight: TMeterUnitId): TVoltMeters;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ V*m ] = [ V/m ] * [ m2 ]

operator *(const ALeft: TVoltsPerMeter; const ARight: TSquareMeters): TVoltMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TVoltsPerMeter): TVoltMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TVoltMeters; const ARight: TVoltsPerMeter): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TVoltMeters; const ARight: TSquareMeters): TVoltsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of NewtonSquareMeterPerCoulomb }

class function TNewtonSquareMeterPerCoulombUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%sN.%sm2/%sC', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol, PrefixTable[APrefixes[2]].Symbol])
  else
    result := 'N.m2/C';
end;

class function TNewtonSquareMeterPerCoulombUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%snewton square %smeter per %scoulomb', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name, PrefixTable[APrefixes[2]].Name])
  else
    result := 'newton square meter per coulomb';
end;

class function TNewtonSquareMeterPerCoulombUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 3 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[1]].Factor, 2);
    if (APrefixes[2] <> pNone) then result := result * PrefixTable[APrefixes[2]].Factor;
  end;
end;

{ Unit of VoltMeterPerSecond }

class function TVoltMeterPerSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%sV.%sm/%ss', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol, PrefixTable[APrefixes[2]].Symbol])
  else
    result := 'V.m/s';
end;

class function TVoltMeterPerSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%svolt %smeter per %ssecond', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name, PrefixTable[APrefixes[2]].Name])
  else
    result := 'volt meter per second';
end;

class function TVoltMeterPerSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 3 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result / PrefixTable[APrefixes[1]].Factor;
    if (APrefixes[2] <> pNone) then result := result * PrefixTable[APrefixes[2]].Factor;
  end;
end;

// main definition [ V*m/s ] = [ V*m ] / [ s ]

operator /(const ALeft: TVoltMeters; const ARight: TSeconds): TVoltMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TVoltMetersPerSecond): TVoltMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TVoltMetersPerSecond; const ARight: TSeconds): TVoltMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TVoltMeters; const ARight: TVoltMetersPerSecond): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TVoltMeters; const {%H-}ARight: TSecondUnitId): TVoltMetersPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of FaradPerMeter }

class function TFaradPerMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sF/%sm', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'F/m';
end;

class function TFaradPerMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sfarad per %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'farad per meter';
end;

class function TFaradPerMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ F/m ] = [ F ] / [ m ]

operator /(const ALeft: TFarads; const ARight: TMeters): TFaradsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TFaradsPerMeter): TFarads;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TFaradsPerMeter; const ARight: TMeters): TFarads;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TFarads; const ARight: TFaradsPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TFarads; const {%H-}ARight: TMeterUnitId): TFaradsPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ F/m ] = [ C ] / [ V*m ]

operator /(const ALeft: TCoulombs; const ARight: TVoltMeters): TFaradsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TVoltMeters; const ARight: TFaradsPerMeter): TCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TFaradsPerMeter; const ARight: TVoltMeters): TCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCoulombs; const ARight: TFaradsPerMeter): TVoltMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ F/m ] = [ C/m2 ] / [ N/C ]

operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TVoltsPerMeter): TFaradsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TVoltsPerMeter; const ARight: TFaradsPerMeter): TCoulombsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TFaradsPerMeter; const ARight: TVoltsPerMeter): TCoulombsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TFaradsPerMeter): TVoltsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ F/m ] = [ 1 ] / [ N*m2/C2 ]

operator /(const ALeft: double; const ARight: TNewtonSquareMetersPerSquareCoulomb): TFaradsPerMeter;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TFaradsPerMeter): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TFaradsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TFaradsPerMeter): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft / ARight.FValue;
end;

{ Unit of AmperePerMeter }

class function TAmperePerMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sA/%sm', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'A/m';
end;

class function TAmperePerMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sampere per %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'ampere per meter';
end;

class function TAmperePerMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ A/m ] = [ A ] / [ m ]

operator /(const ALeft: TAmperes; const ARight: TMeters): TAmperesPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TAmperesPerMeter): TAmperes;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TAmperesPerMeter; const ARight: TMeters): TAmperes;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TAmperes; const ARight: TAmperesPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TAmperes; const {%H-}ARight: TMeterUnitId): TAmperesPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of MeterPerAmpere }

class function TMeterPerAmpereUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sm/%sA', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'm/A';
end;

class function TMeterPerAmpereUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%smeter per %sampere', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'meter per ampere';
end;

class function TMeterPerAmpereUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ m/A ] = [ m ] / [ A ]

operator /(const ALeft: TMeters; const ARight: TAmperes): TMetersPerAmpere;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TAmperes; const ARight: TMetersPerAmpere): TMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerAmpere; const ARight: TAmperes): TMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TMeters; const ARight: TMetersPerAmpere): TAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMeters; const {%H-}ARight: TAmpereUnitId): TMetersPerAmpere;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of TeslaMeter }

class function TTeslaMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sT.%sm', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'T.m';
end;

class function TTeslaMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%stesla %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'tesla meter';
end;

class function TTeslaMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result / PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ T*m ] = [ T ] * [ m ]

operator *(const ALeft: TTeslas; const ARight: TMeters): TTeslaMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TTeslas): TTeslaMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TTeslaMeters; const ARight: TTeslas): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TTeslaMeters; const ARight: TMeters): TTeslas;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TTeslas; const {%H-}ARight: TMeterUnitId): TTeslaMeters;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ T*m ] = [ N/A ] = [ N ] / [ A ]

operator /(const ALeft: TNewtons; const ARight: TAmperes): TTeslaMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TAmperes; const ARight: TTeslaMeters): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TTeslaMeters; const ARight: TAmperes): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TTeslaMeters): TAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TAmpereUnitId): TTeslaMeters;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of NewtonPerAmpere }

class function TNewtonPerAmpereUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sN/%sA', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'N/A';
end;

class function TNewtonPerAmpereUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%snewton per %sampere', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'newton per ampere';
end;

class function TNewtonPerAmpereUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

{ Unit of TeslaPerAmpere }

class function TTeslaPerAmpereUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sT/%sA', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'T/A';
end;

class function TTeslaPerAmpereUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%stesla per %sampere', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'tesla per ampere';
end;

class function TTeslaPerAmpereUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ T/A ] = [ T ] / [ A ]

operator /(const ALeft: TTeslas; const ARight: TAmperes): TTeslasPerAmpere;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TAmperes; const ARight: TTeslasPerAmpere): TTeslas;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TTeslasPerAmpere; const ARight: TAmperes): TTeslas;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TTeslas; const ARight: TTeslasPerAmpere): TAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TTeslas; const {%H-}ARight: TAmpereUnitId): TTeslasPerAmpere;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of HenryPerMeter }

class function THenryPerMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sH/%sm', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'H/m';
end;

class function THenryPerMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%shenry per %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'henry per meter';
end;

class function THenryPerMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ H/m ] = [ H ] / [ m ]

operator /(const ALeft: THenrys; const ARight: TMeters): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: THenrysPerMeter): THenrys;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THenrysPerMeter; const ARight: TMeters): THenrys;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: THenrys; const ARight: THenrysPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: THenrys; const {%H-}ARight: TMeterUnitId): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T*m ] / [ A ]

operator /(const ALeft: TTeslaMeters; const ARight: TAmperes): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TAmperes; const ARight: THenrysPerMeter): TTeslaMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THenrysPerMeter; const ARight: TAmperes): TTeslaMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TTeslaMeters; const ARight: THenrysPerMeter): TAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TTeslaMeters; const {%H-}ARight: TAmpereUnitId): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T/A ] * [ m ]

operator *(const ALeft: TTeslasPerAmpere; const ARight: TMeters): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TTeslasPerAmpere): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: THenrysPerMeter; const ARight: TTeslasPerAmpere): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: THenrysPerMeter; const ARight: TMeters): TTeslasPerAmpere;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TTeslasPerAmpere; const {%H-}ARight: TMeterUnitId): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] * [ m/A ]

operator *(const ALeft: TTeslas; const ARight: TMetersPerAmpere): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerAmpere; const ARight: TTeslas): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: THenrysPerMeter; const ARight: TTeslas): TMetersPerAmpere;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: THenrysPerMeter; const ARight: TMetersPerAmpere): TTeslas;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] / [ A/m ]

operator /(const ALeft: TTeslas; const ARight: TAmperesPerMeter): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TAmperesPerMeter; const ARight: THenrysPerMeter): TTeslas;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THenrysPerMeter; const ARight: TAmperesPerMeter): TTeslas;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TTeslas; const ARight: THenrysPerMeter): TAmperesPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ H/m ] = [ N/A2 ] = [ N ] / [ A2 ]

operator /(const ALeft: TNewtons; const ARight: TSquareAmperes): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareAmperes; const ARight: THenrysPerMeter): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THenrysPerMeter; const ARight: TSquareAmperes): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: THenrysPerMeter): TSquareAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of TeslaMeterPerAmpere }

class function TTeslaMeterPerAmpereUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%sT.%sm/%sA', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol, PrefixTable[APrefixes[2]].Symbol])
  else
    result := 'T.m/A';
end;

class function TTeslaMeterPerAmpereUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 3 then
    result := Format('%stesla %smeter per %sampere', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name, PrefixTable[APrefixes[2]].Name])
  else
    result := 'tesla meter per ampere';
end;

class function TTeslaMeterPerAmpereUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 3 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result / PrefixTable[APrefixes[1]].Factor;
    if (APrefixes[2] <> pNone) then result := result * PrefixTable[APrefixes[2]].Factor;
  end;
end;

{ Unit of NewtonPerSquareAmpere }

class function TNewtonPerSquareAmpereUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sN/%sA2', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'N/A2';
end;

class function TNewtonPerSquareAmpereUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%snewton per square %sampere', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'newton per square ampere';
end;

class function TNewtonPerSquareAmpereUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[1]].Factor, 2);
  end;
end;

{ Unit of RadianPerMeter }

class function TRadianPerMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('rad/%sm', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'rad/m';
end;

class function TRadianPerMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('radian per %smeter', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'radian per meter';
end;

class function TRadianPerMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result * PrefixTable[APrefixes[0]].Factor;
  end;
end;

// main definition [ rad/m ] = [ rad ] / [ m ]

operator /(const ALeft: TRadians; const ARight: TMeters): TRadiansPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TRadiansPerMeter): TRadians;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TRadiansPerMeter; const ARight: TMeters): TRadians;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TRadians; const ARight: TRadiansPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TRadians; const {%H-}ARight: TMeterUnitId): TRadiansPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

{ Unit of SquareKilogramPerSquareSecond }

class function TSquareKilogramPerSquareSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sg2/%ss2', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'kg2/s2';
end;

class function TSquareKilogramPerSquareSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('square %sgram per square %ssecond', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'square kilogram per square second';
end;

class function TSquareKilogramPerSquareSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pKilo) then
      result := result * 1E+06 / IntPower(PrefixTable[APrefixes[0]].Factor, 2);

    if (APrefixes[1] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[1]].Factor, 2);
  end;
end;

// main definition [ kg2/s2 ] = [ kg2 ] / [ s2 ]

operator /(const ALeft: TSquareKilograms; const ARight: TSquareSeconds): TSquareKilogramsPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TSquareKilogramsPerSquareSecond): TSquareKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TSquareSeconds): TSquareKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerSquareSecond): TSquareSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareKilograms; const {%H-}ARight: TSquareSecondUnitId): TSquareKilogramsPerSquareSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ kg2/s2 ] = [ kg/s ] * [ kg/s ]

operator *(const ALeft: TKilogramsPerSecond; const ARight: TKilogramsPerSecond): TSquareKilogramsPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TKilogramsPerSecond): TKilogramsPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ kg2/s2 ] = [ kg ] * [ N/m ]

operator *(const ALeft: TKilograms; const ARight: TNewtonsPerMeter): TSquareKilogramsPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonsPerMeter; const ARight: TKilograms): TSquareKilogramsPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TKilograms): TNewtonsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TNewtonsPerMeter): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of SquareSecondPerSquareMeter }

class function TSquareSecondPerSquareMeterUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%ss2/%sm2', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 's2/m2';
end;

class function TSquareSecondPerSquareMeterUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('square %ssecond per square %smeter', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'square second per square meter';
end;

class function TSquareSecondPerSquareMeterUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 2);
    if (APrefixes[1] <> pNone) then result := result * IntPower(PrefixTable[APrefixes[1]].Factor, 2);
  end;
end;

// main definition [ s2/m2 ] = [ s2 ] / [ m2 ]

operator /(const ALeft: TSquareSeconds; const ARight: TSquareMeters): TSquareSecondsPerSquareMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TSquareSecondsPerSquareMeter): TSquareSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareSecondsPerSquareMeter; const ARight: TSquareMeters): TSquareSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareSeconds; const ARight: TSquareSecondsPerSquareMeter): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareSeconds; const {%H-}ARight: TSquareMeterUnitId): TSquareSecondsPerSquareMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ s2/m2 ] = [ 1 ] / [ m2/s2 ]

operator /(const ALeft: double; const ARight: TSquareMetersPerSquareSecond): TSquareSecondsPerSquareMeter;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TSquareSecondsPerSquareMeter): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareSecondsPerSquareMeter; const ARight: TSquareMetersPerSquareSecond): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TSquareSecondsPerSquareMeter): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft / ARight.FValue;
end;

// alternative definition [ s2/m2 ] = [ F/m ] * [ H/m ]

operator *(const ALeft: TFaradsPerMeter; const ARight: THenrysPerMeter): TSquareSecondsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THenrysPerMeter; const ARight: TFaradsPerMeter): TSquareSecondsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareSecondsPerSquareMeter; const ARight: TFaradsPerMeter): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareSecondsPerSquareMeter; const ARight: THenrysPerMeter): TFaradsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of SquareJoule }

class function TSquareJouleUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('%sJ2', [PrefixTable[APrefixes[0]].Symbol])
  else
    result := 'J2';
end;

class function TSquareJouleUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 1 then
    result := Format('square %sjoule', [PrefixTable[APrefixes[0]].Name])
  else
    result := 'square joule';
end;

class function TSquareJouleUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 1 then
  begin
    if (APrefixes[0] <> pNone) then result := result / IntPower(PrefixTable[APrefixes[0]].Factor, 2);
  end;
end;

// main definition [ J2 ] = [ J ] * [ J ]

operator *(const ALeft: TJoules; const ARight: TJoules): TSquareJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareJoules; const ARight: TJoules): TJoules;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of JouleSecond }

class function TJouleSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sJ.%ss', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'J.s';
end;

class function TJouleSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%sjoule %ssecond', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'joule second';
end;

class function TJouleSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result / PrefixTable[APrefixes[1]].Factor;
  end;
end;

// main definition [ J*s ] = [ J ] * [ s ]

operator *(const ALeft: TJoules; const ARight: TSeconds): TJouleSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TJoules): TJouleSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJouleSeconds; const ARight: TJoules): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleSeconds; const ARight: TSeconds): TJoules;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TJoules; const {%H-}ARight: TSecondUnitId): TJouleSeconds;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ J*s ] = [ J ] / [ Hz ]

operator /(const ALeft: TJoules; const ARight: THertz): TJouleSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: THertz; const ARight: TJouleSeconds): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJouleSeconds; const ARight: THertz): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TJouleSeconds): THertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const {%H-}ARight: THertzUnitId): TJouleSeconds;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ J*s ] = [ kg*m/s ] * [ m ]

operator *(const ALeft: TKilogramMetersPerSecond; const ARight: TMeters): TJouleSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TKilogramMetersPerSecond): TJouleSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJouleSeconds; const ARight: TKilogramMetersPerSecond): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleSeconds; const ARight: TMeters): TKilogramMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Unit of ElettronvoltSecond }

class function TElettronvoltSecondUnit.GetSymbol(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%seV.%ss', [PrefixTable[APrefixes[0]].Symbol, PrefixTable[APrefixes[1]].Symbol])
  else
    result := 'eV.s';
end;

class function TElettronvoltSecondUnit.GetName(const {%H-}APrefixes: TPrefixes): string; static;
begin
  if Length(APrefixes) = 2 then
    result := Format('%selettronvolt %ssecond', [PrefixTable[APrefixes[0]].Name, PrefixTable[APrefixes[1]].Name])
  else
    result := 'elettronvolt second';
end;

class function TElettronvoltSecondUnit.GetValue(const AValue: double; const {%H-}APrefixes: TPrefixes): double; static;
begin
  result := AValue;
  if Length(APrefixes) = 2 then
  begin
    if (APrefixes[0] <> pNone) then result := result / PrefixTable[APrefixes[0]].Factor;
    if (APrefixes[1] <> pNone) then result := result / PrefixTable[APrefixes[1]].Factor;
  end;
end;

{ Helpers }

function TSecondHelper.ToDay: specialize TQuantity<TDayUnit>;
begin
  result.FValue := FValue / TDayUnit.Factor;
end;

function TSecondHelper.ToHour: specialize TQuantity<THourUnit>;
begin
  result.FValue := FValue / THourUnit.Factor;
end;

function TSecondHelper.ToMinute: specialize TQuantity<TMinuteUnit>;
begin
  result.FValue := FValue / TMinuteUnit.Factor;
end;

function TSquareSecondHelper.ToSquareDay: specialize TQuantity<TSquareDayUnit>;
begin
  result.FValue := FValue / TSquareDayUnit.Factor;
end;

function TSquareSecondHelper.ToSquareHour: specialize TQuantity<TSquareHourUnit>;
begin
  result.FValue := FValue / TSquareHourUnit.Factor;
end;

function TSquareSecondHelper.ToSquareMinute: specialize TQuantity<TSquareMinuteUnit>;
begin
  result.FValue := FValue / TSquareMinuteUnit.Factor;
end;

function TMeterHelper.ToAstronomical: specialize TQuantity<TAstronomicalUnit>;
begin
  result.FValue := FValue / TAstronomicalUnit.Factor;
end;

function TCubicMeterHelper.ToLitre: specialize TQuantity<TLitreUnit>;
begin
  result.FValue := FValue / TLitreUnit.Factor;
end;

function TKilogramHelper.ToTonne: specialize TQuantity<TTonneUnit>;
begin
  result.FValue := FValue / TTonneUnit.Factor;
end;

function TDegreeCelsiusHelper.ToKelvin: specialize TQuantity<TKelvinUnit>;
begin
  result.FValue := FValue + 273.15;
end;

function TKelvinHelper.ToDegreeCelsius: specialize TQuantity<TDegreeCelsiusUnit>;
begin
  result.FValue := FValue - 273.15;
end;

function TDegreeFahrenheitHelper.ToKelvin: specialize TQuantity<TKelvinUnit>;
begin
  result.FValue := 5/9 * (FValue - 32) + 273.15;
end;

function TKelvinHelper.ToDegreeFahrenheit: specialize TQuantity<TDegreeFahrenheitUnit>;
begin
  result.FValue := 9/5 * FValue - 459.67;
end;

function TRadianHelper.ToDegree: specialize TQuantity<TDegreeUnit>;
begin
  result.FValue := FValue / TDegreeUnit.Factor;
end;

function TSquareHertzHelper.ToRadianPerSquareSecond: specialize TQuantity<TRadianPerSquareSecondUnit>;
begin
  result.FValue := FValue;
end;

function TSquareHertzHelper.ToSteradianPerSquareSecond: specialize TQuantity<TSteradianPerSquareSecondUnit>;
begin
  result.FValue := FValue;
end;

function TMeterPerSecondHelper.ToMeterPerHour: specialize TQuantity<TMeterPerHourUnit>;
begin
  result.FValue := FValue / TMeterPerHourUnit.Factor;
end;

function TMeterPerSquareSecondHelper.ToMeterPerSecondPerSecond: specialize TQuantity<TMeterPerSecondPerSecondUnit>;
begin
  result.FValue := FValue;
end;

function TMeterPerSquareSecondHelper.ToMeterPerHourPerSecond: specialize TQuantity<TMeterPerHourPerSecondUnit>;
begin
  result.FValue := FValue / TMeterPerHourPerSecondUnit.Factor;
end;

function TKilogramMeterPerSecondHelper.ToNewtonSecond: specialize TQuantity<TNewtonSecondUnit>;
begin
  result.FValue := FValue;
end;

function TJouleHelper.ToElettronvolt: specialize TQuantity<TElettronvoltUnit>;
begin
  result.FValue := FValue / TElettronvoltUnit.Factor;
end;

function THertzHelper.ToBequerel: specialize TQuantity<TBequerelUnit>;
begin
  result.FValue := FValue;
end;

function TSquareMeterPerSquareSecondHelper.ToGray: specialize TQuantity<TGrayUnit>;
begin
  result.FValue := FValue;
end;

function TSquareMeterPerSquareSecondHelper.ToSievert: specialize TQuantity<TSievertUnit>;
begin
  result.FValue := FValue;
end;

function TJouleHelper.ToNewtonMeter: specialize TQuantity<TNewtonMeterUnit>;
begin
  result.FValue := FValue;
end;

function TJoulePerRadianHelper.ToJoulePerDegree: specialize TQuantity<TJoulePerDegreeUnit>;
begin
  result.FValue := FValue / TJoulePerDegreeUnit.Factor;
end;

function TJoulePerRadianHelper.ToNewtonMeterPerRadian: specialize TQuantity<TNewtonMeterPerRadianUnit>;
begin
  result.FValue := FValue;
end;

function TJoulePerRadianHelper.ToNewtonMeterPerDegree: specialize TQuantity<TNewtonMeterPerDegreeUnit>;
begin
  result.FValue := FValue / TNewtonMeterPerDegreeUnit.Factor;
end;

function TPoiseuilleHelper.ToPascalSecond: specialize TQuantity<TPascalSecondUnit>;
begin
  result.FValue := FValue;
end;

function TSquareMeterPerSquareKilogramHelper.ToJoulePerKilogram: specialize TQuantity<TJoulePerKilogramUnit>;
begin
  result.FValue := FValue;
end;

function TVoltPerMeterHelper.ToNewtonPerCoulomb: specialize TQuantity<TNewtonPerCoulombUnit>;
begin
  result.FValue := FValue;
end;

function TVoltMeterHelper.ToNewtonSquareMeterPerCoulomb: specialize TQuantity<TNewtonSquareMeterPerCoulombUnit>;
begin
  result.FValue := FValue;
end;

function TTeslaMeterHelper.ToNewtonPerAmpere: specialize TQuantity<TNewtonPerAmpereUnit>;
begin
  result.FValue := FValue;
end;

function THenryPerMeterHelper.ToTeslaMeterPerAmpere: specialize TQuantity<TTeslaMeterPerAmpereUnit>;
begin
  result.FValue := FValue;
end;

function THenryPerMeterHelper.ToNewtonPerSquareAmpere: specialize TQuantity<TNewtonPerSquareAmpereUnit>;
begin
  result.FValue := FValue;
end;

function TJouleSecondHelper.ToElettronvoltSecond: specialize TQuantity<TElettronvoltSecondUnit>;
begin
  result.FValue := FValue / TElettronvoltSecondUnit.Factor;
end;

{ Power quantities }

function SquarePower(AQuantity: TSeconds): TSquareSeconds;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareSeconds): TSeconds;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TMeters): TSquareMeters;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareMeters): TMeters;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function CubicPower(AQuantity: TMeters): TCubicMeters;
begin
  result.FValue := Power(AQuantity.FValue, 3);
end;

function CubicRoot(AQuantity: TCubicMeters): TMeters;
begin
  result.FValue := Power(AQuantity.FValue, 1/3);
end;

function SquarePower(AQuantity: TSquareMeters): TQuarticMeters;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TQuarticMeters): TSquareMeters;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function QuarticPower(AQuantity: TMeters): TQuarticMeters;
begin
  result.FValue := Power(AQuantity.FValue, 4);
end;

function QuarticRoot(AQuantity: TQuarticMeters): TMeters;
begin
  result.FValue := Power(AQuantity.FValue, 1/4);
end;

function QuinticPower(AQuantity: TMeters): TQuinticMeters;
begin
  result.FValue := Power(AQuantity.FValue, 5);
end;

function QuinticRoot(AQuantity: TQuinticMeters): TMeters;
begin
  result.FValue := Power(AQuantity.FValue, 1/5);
end;

function SquarePower(AQuantity: TCubicMeters): TSexticMeters;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSexticMeters): TCubicMeters;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function CubicPower(AQuantity: TSquareMeters): TSexticMeters;
begin
  result.FValue := Power(AQuantity.FValue, 3);
end;

function CubicRoot(AQuantity: TSexticMeters): TSquareMeters;
begin
  result.FValue := Power(AQuantity.FValue, 1/3);
end;

function SexticPower(AQuantity: TMeters): TSexticMeters;
begin
  result.FValue := Power(AQuantity.FValue, 6);
end;

function SexticRoot(AQuantity: TSexticMeters): TMeters;
begin
  result.FValue := Power(AQuantity.FValue, 1/6);
end;

function SquarePower(AQuantity: TAmperes): TSquareAmperes;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareAmperes): TAmperes;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TKelvins): TSquareKelvins;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareKelvins): TKelvins;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function CubicPower(AQuantity: TKelvins): TCubicKelvins;
begin
  result.FValue := Power(AQuantity.FValue, 3);
end;

function CubicRoot(AQuantity: TCubicKelvins): TKelvins;
begin
  result.FValue := Power(AQuantity.FValue, 1/3);
end;

function SquarePower(AQuantity: TSquareKelvins): TQuarticKelvins;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TQuarticKelvins): TSquareKelvins;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function QuarticPower(AQuantity: TKelvins): TQuarticKelvins;
begin
  result.FValue := Power(AQuantity.FValue, 4);
end;

function QuarticRoot(AQuantity: TQuarticKelvins): TKelvins;
begin
  result.FValue := Power(AQuantity.FValue, 1/4);
end;

function SquarePower(AQuantity: TRadians): TSteradians;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSteradians): TRadians;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: THertz): TSquareHertz;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareHertz): THertz;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TMetersPerSecond): TSquareMetersPerSquareSecond;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareMetersPerSquareSecond): TMetersPerSecond;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TCoulombs): TSquareCoulombs;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareCoulombs): TCoulombs;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TVolts): TSquareVolts;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareVolts): TVolts;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TKilogramsPerSecond): TSquareKilogramsPerSquareSecond;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareKilogramsPerSquareSecond): TKilogramsPerSecond;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TJoules): TSquareJoules;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareJoules): TJoules;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

{ Trigonometric functions }

function Cos(const AQuantity: TRadians): double;
begin
  result := System.Cos(AQuantity.FValue);
end;

function Sin(const AQuantity: TRadians): double;
begin
  result := System.Sin(AQuantity.FValue);
end;

function Tan(const AQuantity: TRadians): double;
begin
  result := Math.Tan(AQuantity.FValue);
end;

function Cotan(const AQuantity: TRadians): double;
begin
  result := Math.Cotan(AQuantity.FValue);
end;

function Secant(const AQuantity: TRadians): double;
begin
  result := Math.Secant(AQuantity.FValue);
end;

function Cosecant(const AQuantity: TRadians): double;
begin
  result := Math.Cosecant(AQuantity.FValue);
end;

function ArcCos(const AValue: double): TRadians;
begin
  result.FValue := Math.ArcCos(AValue);
end;

function ArcSin(const AValue: double): TRadians;
begin
  result.FValue := Math.ArcSin(AValue);
end;

function ArcTan(const AValue: double): TRadians;
begin
  result.FValue := System.ArcTan(AValue);
end;

function ArcTan2(const x, y: double): TRadians;
begin
  result.FValue := Math.ArcTan2(x, y);
end;

end.
