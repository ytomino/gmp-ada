with GMP.Root_F;
generic
	Precision : in GMP.Precision := Default_Precision;
package GMP.Generic_F is
	pragma Preelaborate;
	
	type MP_Float is new Root_F.MP_Float (Precision);
	
	-- conversions
	
	function To_MP_Float (X : Long_Float) return MP_Float;
--	function To_Long_Float (X : MP_Float) return Long_Float;
	-- this function is inherited
	
	pragma Inline (To_MP_Float);
	
	-- formatting
	
--	function Image (Value : MP_Float; Base : Number_Base := 10) return String;
	-- this function is inherited
	function Value (Image : String; Base : Number_Base := 10) return MP_Float;
	
	pragma Inline (Value);
	
	-- relational operators are inherited
	
	-- unary adding operators
	
	function "+" (Right : MP_Float) return MP_Float;
	function "-" (Right : MP_Float) return MP_Float;
	
	pragma Inline ("+");
	pragma Inline ("-");
	
	-- binary adding operators
	
	function "+" (Left, Right : MP_Float) return MP_Float;
	function "-" (Left, Right : MP_Float) return MP_Float;
	
	pragma Inline ("+");
	pragma Inline ("-");
	
	-- multiplying operators
	
	function "*" (Left, Right : MP_Float) return MP_Float;
	function "/" (Left, Right : MP_Float) return MP_Float;
	
	pragma Inline ("*");
	pragma Inline ("/");
	
	-- highest precedence operators
	
	function "**" (Left : MP_Float; Right : Integer) return MP_Float;
	
	pragma Inline ("**");

end GMP.Generic_F;
