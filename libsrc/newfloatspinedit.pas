unit NewFloatSpinEdit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Spin;

type
  TCustomNewFloatSpinEdit = class(TCustomFloatSpinEdit)
  private
    FRemoveTrailingZeros: boolean;
    procedure SetRemoveTrailingZeros(AValue: boolean);
  protected

  public
    function ValueToStr(const AValue: double): string; override;
  published
    property RemoveTrailingZeros : boolean read FRemoveTrailingZeros write SetRemoveTrailingZeros default True;
  end;

  { TNewFloatSpinEdit }

   TNewFloatSpinEdit = class(TCustomNewFloatSpinEdit)
   public
     property AutoSelected;
   published
     property Align;
     property Alignment;
     property Anchors;
     property AutoSelect;
     property AutoSize;
     property BorderSpacing;
     property Color;
     property Constraints;
     property DecimalPlaces;
     property EditorEnabled;
     property Enabled;
     property Font;
     property Increment;
     property MaxValue;
     property MinValue;
     property ParentColor;
     property ParentFont;
     property ParentShowHint;
     property PopupMenu;
     property ReadOnly;
     property RemoveTrailingZeros;
     property ShowHint;
     property TabStop;
     property TabOrder;
     property Value;
     property Visible;

     property OnChange;
     property OnChangeBounds;
     property OnClick;
     property OnEditingDone;
     property OnEnter;
     property OnExit;
     property OnKeyDown;
     property OnKeyPress;
     property OnKeyUp;
     property OnMouseDown;
     property OnMouseEnter;
     property OnMouseLeave;
     property OnMouseMove;
     property OnMouseUp;
     property OnMouseWheel;
     property OnMouseWheelDown;
     property OnMouseWheelUp;
     property OnMouseWheelHorz;
     property OnMouseWheelLeft;
     property OnMouseWheelRight;
     property OnResize;
     property OnUTF8KeyPress;
   end;

procedure Register;

implementation

uses
  StrUtils;

procedure TCustomNewFloatSpinEdit.SetRemoveTrailingZeros(AValue: boolean);
begin
  if FRemoveTrailingZeros = AValue then Exit;
  FRemoveTrailingZeros := AValue;
  Invalidate;
end;

function TCustomNewFloatSpinEdit.ValueToStr(const AValue: Double): string;
var
  NewLength: longint;
begin
  Result := inherited ValueToStr(AValue);

  if FRemoveTrailingZeros and (DecimalPlaces > 0) then
  begin
    NewLength := Length(Result);
    while Result[NewLength] = '0' do
      Dec(NewLength);

    if Result[NewLength] = DefaultformatSettings.DecimalSeparator then
      Dec(NewLength);

    if NewLength <> Length(Result) then
      SetLength(Result, NewLength);
  end;
end;

procedure Register;
begin
  RegisterComponents('Misc',[TNewFloatSpinEdit]);
end;

end.
