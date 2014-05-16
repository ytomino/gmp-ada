with MPFR.Root_FR;
generic
	Precision : in MPFR.Precision := Default_Precision;
	Rounding : in MPFR.Rounding := Default_Rounding;
package MPFR.Generic_FR is
	pragma Preelaborate;
	
	type MP_Float is new Root_FR.MP_Float (Precision);
	
	-- conversions
	
	function To_MP_Float (X : Long_Long_Float) return MP_Float;
	pragma Inline (To_MP_Float);
	function "+" (Right : Long_Long_Float) return MP_Float renames To_MP_Float;
	
	function To_Long_Long_Float (X : MP_Float) return Long_Long_Float;
	pragma Inline (To_Long_Long_Float);
	function "+" (Right : MP_Float) return Long_Long_Float
		renames To_Long_Long_Float;
	
	-- formatting
	
	function Image (Value : MP_Float; Base : Number_Base := 10) return String;
	pragma Inline (Image);
	function Value (Image : String; Base : Number_Base := 10) return MP_Float;
	pragma Inline (Value);
	
	-- relational operators are inherited
	
	-- unary adding operators
	
	function "+" (Right : MP_Float) return MP_Float;
	pragma Inline ("+");
	function "-" (Right : MP_Float) return MP_Float;
	pragma Inline ("-");
	
	-- binary adding operators
	
	function "+" (Left, Right : MP_Float) return MP_Float;
	pragma Inline ("+");
	function "+" (Left : MP_Float; Right : Long_Long_Float) return MP_Float;
	pragma Inline ("+");
	function "+" (Left : Long_Long_Float; Right : MP_Float) return MP_Float;
	pragma Inline ("+");
	
	function "-" (Left, Right : MP_Float) return MP_Float;
	pragma Inline ("-");
	function "-" (Left : MP_Float; Right : Long_Long_Float) return MP_Float;
	pragma Inline ("-");
	function "-" (Left : Long_Long_Float; Right : MP_Float) return MP_Float;
	pragma Inline ("-");
	
	-- multiplying operators
	
	function "*" (Left, Right : MP_Float) return MP_Float;
	pragma Inline ("*");
	function "*" (Left : MP_Float; Right : Long_Long_Float) return MP_Float;
	pragma Inline ("*");
	function "*" (Left : Long_Long_Float; Right : MP_Float) return MP_Float;
	pragma Inline ("*");
	
	function "/" (Left, Right : MP_Float) return MP_Float;
	pragma Inline ("/");
	function "/" (Left : MP_Float; Right : Long_Long_Float) return MP_Float;
	pragma Inline ("/");
	function "/" (Left : Long_Long_Float; Right : MP_Float) return MP_Float;
	pragma Inline ("/");
	
	-- highest precedence operators
	
	function "**" (Left : MP_Float; Right : Integer) return MP_Float;
	pragma Inline ("**");
	
	-- subprograms of a scalar type
	
	function Sqrt (X : MP_Float) return MP_Float;
	pragma Inline (Sqrt);

	-- miscellany
	
	function NaN return MP_Float;
	pragma Inline (NaN);
	function Infinity return MP_Float;
	pragma Inline (Infinity);
	
end MPFR.Generic_FR;
