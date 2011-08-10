package body MPFR.Generic_FR is
	
	function To_MP_Float (X : Long_Long_Float) return MP_Float is
	begin
		return To_MP_Float (X, Precision, Rounding);
	end To_MP_Float;
	
	function To_Long_Long_Float (X : MP_Float) return Long_Long_Float is
	begin
		return To_Long_Long_Float (X, Rounding);
	end To_Long_Long_Float;
	
	function Image (Value : MP_Float; Base : Number_Base := 10) return String is
	begin
		return Image (Value, Base, Rounding);
	end Image;
	
	function Value (Image : String; Base : Number_Base := 10) return MP_Float is
	begin
		return Value (Image, Base, Precision, Rounding);
	end Value;
	
	function "+" (Right : MP_Float) return MP_Float is
	begin
		return Copy (Right, Precision, Rounding);
	end "+";
	
	function "-" (Right : MP_Float) return MP_Float is
	begin
		return Negative (Right, Precision, Rounding);
	end "-";
	
	function "+" (Left, Right : MP_Float) return MP_Float is
	begin
		return Add (Left, Right, Precision, Rounding);
	end "+";
	
	function "+" (Left : MP_Float; Right : Long_Long_Float) return MP_Float is
	begin
		return Add (Left, Right, Precision, Rounding);
	end "+";
	
	function "+" (Left : Long_Long_Float; Right : MP_Float) return MP_Float is
	begin
		return Add (Left, Right, Precision, Rounding);
	end "+";
	
	function "-" (Left, Right : MP_Float) return MP_Float is
	begin
		return Subtract (Left, Right, Precision, Rounding);
	end "-";
	
	function "-" (Left : MP_Float; Right : Long_Long_Float) return MP_Float is
	begin
		return Subtract (Left, Right, Precision, Rounding);
	end "-";
	
	function "-" (Left : Long_Long_Float; Right : MP_Float) return MP_Float is
	begin
		return Subtract (Left, Right, Precision, Rounding);
	end "-";
	
	function "*" (Left, Right : MP_Float) return MP_Float is
	begin
		return Multiply (Left, Right, Precision, Rounding);
	end "*";
	
	function "*" (Left : MP_Float; Right : Long_Long_Float) return MP_Float is
	begin
		return Multiply (Left, Right, Precision, Rounding);
	end "*";
	
	function "*" (Left : Long_Long_Float; Right : MP_Float) return MP_Float is
	begin
		return Multiply (Left, Right, Precision, Rounding);
	end "*";
	
	function "/" (Left, Right : MP_Float) return MP_Float is
	begin
		return Divide (Left, Right, Precision, Rounding);
	end "/";
	
	function "/" (Left : MP_Float; Right : Long_Long_Float) return MP_Float is
	begin
		return Divide (Left, Right, Precision, Rounding);
	end "/";
	
	function "/" (Left : Long_Long_Float; Right : MP_Float) return MP_Float is
	begin
		return Divide (Left, Right, Precision, Rounding);
	end "/";
	
	function "**" (Left : MP_Float; Right : Integer) return MP_Float is
	begin
		return Power (Left, Right, Precision, Rounding);
	end "**";
	
	function Sqrt (X : MP_Float) return MP_Float is
	begin
		return Sqrt (X, Precision, Rounding);
	end Sqrt;
	
	function NaN return MP_Float is
	begin
		return NaN (Precision);
	end NaN;
	
	function Infinity return MP_Float is
	begin
		return Infinity (Precision);
	end Infinity;
	
end MPFR.Generic_FR;
