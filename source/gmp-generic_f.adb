package body GMP.Generic_F is
	
	function To_MP_Float (X : Long_Float) return MP_Float is
	begin
		return To_MP_Float (X, Precision);
	end To_MP_Float;
	
	function Value (
		Image : String;
		Base : Number_Base := 10)
		return MP_Float is
	begin
		return Value (Image, Base, Precision);
	end Value;
	
	function "+" (Right : MP_Float) return MP_Float is
	begin
		return Copy (Right, Precision);
	end "+";
	
	function "-" (Right : MP_Float) return MP_Float is
	begin
		return Negative (Right, Precision);
	end "-";
	
	function "+" (Left, Right : MP_Float) return MP_Float is
	begin
		return Add (Left, Right, Precision);
	end "+";
	
	function "-" (Left, Right : MP_Float) return MP_Float is
	begin
		return Subtract (Left, Right, Precision);
	end "-";
	
	function "*" (Left, Right : MP_Float) return MP_Float is
	begin
		return Multiply (Left, Right, Precision);
	end "*";
	
	function "/" (Left, Right : MP_Float) return MP_Float is
	begin
		return Divide (Left, Right, Precision);
	end "/";
	
	function "**" (Left : MP_Float; Right : Integer) return MP_Float is
	begin
		return Power (Left, Right, Precision);
	end "**";
	
end GMP.Generic_F;
