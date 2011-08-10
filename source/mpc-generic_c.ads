with MPFR.Generic_FR;
with MPC.Root_C;
generic
	with package Real_FR is new MPFR.Generic_FR (others => <>);
	with package Imaginary_FR is new MPFR.Generic_FR (others => <>);
package MPC.Generic_C is
	pragma Preelaborate;
	
	function Rounding return MPC.Rounding;
	pragma Inline (Rounding);
	
	type MP_Complex is new Root_C.MP_Complex (
		Real_FR.Precision,
		Imaginary_FR.Precision);
	
	type MP_Imaginary is private;
	
	function i return MP_Imaginary;
	pragma Inline (i);
	function j return MP_Imaginary renames i;
	
	function Re (X : MP_Complex) return Real_FR.MP_Float;
	pragma Inline (Re);
	function Im (X : MP_Complex) return Imaginary_FR.MP_Float;
	pragma Inline (Im);
	
	function Compose (
		Re : Real_FR.MP_Float;
		Im : Imaginary_FR.MP_Float)
		return MP_Complex;
	pragma Inline (Compose);
	
	function Image (
		Value : MP_Complex;
		Base : Number_Base := 10)
		return String;
	pragma Inline (Image);
	function Value (
		Image : String;
		Base : Number_Base := 10)
		return MP_Complex;
	pragma Inline (Value);
	
	function "+" (Right : MP_Complex) return MP_Complex;
	pragma Inline ("+");
	function "-" (Right : MP_Complex) return MP_Complex;
	pragma Inline ("-");
	
	function "+" (Left, Right : MP_Complex) return MP_Complex;
	pragma Inline ("+");
	function "+" (Left : Long_Long_Float; Right : MP_Imaginary)
		return MP_Complex;
	pragma Inline ("+");
	
	function "-" (Left, Right : MP_Complex) return MP_Complex;
	pragma Inline ("-");
	function "-" (Left : Long_Long_Float; Right : MP_Imaginary)
		return MP_Complex;
	pragma Inline ("-");
	
	function "*" (Left, Right : MP_Complex) return MP_Complex;
	pragma Inline ("*");
	
	function "/" (Left, Right : MP_Complex) return MP_Complex;
	pragma Inline ("/");
	
	function "*" (Left : Long_Long_Float; Right : MP_Imaginary)
		return MP_Imaginary;
	pragma Inline ("*");
	
private
	
	type MP_Imaginary is new Imaginary_FR.MP_Float;
	
end MPC.Generic_C;
