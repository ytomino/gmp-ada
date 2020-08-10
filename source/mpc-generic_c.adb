with MPFR.Root_FR;
package body MPC.Generic_C is
	use type Imaginary_FR.MP_Float;
	
	function Rounding return MPC.Rounding is
	begin
		return Compose (
			Real_FR.Rounding,
			Imaginary_FR.Rounding);
	end Rounding;
	
	function i return MP_Imaginary is
	begin
		return To_MP_Float (1.0);
	end i;
	
	function Re (X : MP_Complex) return Real_FR.MP_Float is
	begin
		return Real_FR.MP_Float (Root_C.Re (Root_C.MP_Complex (X)));
	end Re;
	
	function Im (X : MP_Complex) return Imaginary_FR.MP_Float is
	begin
		return Imaginary_FR.MP_Float (Root_C.Im (Root_C.MP_Complex (X)));
	end Im;
	
	function Compose (
		Re : Real_FR.MP_Float;
		Im : Imaginary_FR.MP_Float)
		return MP_Complex is
	begin
		return Compose (MPFR.Root_FR.MP_Float (Re), MPFR.Root_FR.MP_Float (Im));
	end Compose;
	
	function Image (
		Value : MP_Complex;
		Base : Number_Base := 10)
		return String is
	begin
		return Image (Value, Base, Rounding);
	end Image;
	
	function Value (
		Image : String;
		Base : Number_Base := 10)
		return MP_Complex is
	begin
		return Value (
			Image => Image,
			Base => Base,
			Real_Precision => Real_FR.Precision,
			Imaginary_Precision => Imaginary_FR.Precision,
			Rounding => Rounding);
	end Value;
	
	function "+" (Right : MP_Complex) return MP_Complex is
	begin
		return Copy (
			Right,
			Real_Precision => Real_FR.Precision,
			Imaginary_Precision => Imaginary_FR.Precision,
			Rounding => Rounding);
	end "+";
	
	function "-" (Right : MP_Complex) return MP_Complex is
	begin
		return Negative (
			Right,
			Real_Precision => Real_FR.Precision,
			Imaginary_Precision => Imaginary_FR.Precision,
			Rounding => Rounding);
	end "-";
	
	function "+" (Left, Right : MP_Complex) return MP_Complex is
	begin
		return Add (
			Left,
			Right,
			Real_Precision => Real_FR.Precision,
			Imaginary_Precision => Imaginary_FR.Precision,
			Rounding => Rounding);
	end "+";
	
	function "+" (Left : Long_Long_Float; Right : MP_Imaginary)
		return MP_Complex is
	begin
		return Compose (Left, Real_FR.Precision, MPFR.Root_FR.MP_Float (Right));
	end "+";
	
	function "-" (Left, Right : MP_Complex) return MP_Complex is
	begin
		return Subtract (
			Left,
			Right,
			Real_Precision => Real_FR.Precision,
			Imaginary_Precision => Imaginary_FR.Precision,
			Rounding => Rounding);
	end "-";
	
	function "-" (Left : Long_Long_Float; Right : MP_Imaginary)
		return MP_Complex is
	begin
		return Compose (Left, Real_FR.Precision, MPFR.Root_FR.MP_Float (-Right));
	end "-";
	
	function "*" (Left, Right : MP_Complex) return MP_Complex is
	begin
		return Multiply (
			Left,
			Right,
			Real_Precision => Real_FR.Precision,
			Imaginary_Precision => Imaginary_FR.Precision,
			Rounding => Rounding);
	end "*";
	
	function "/" (Left, Right : MP_Complex) return MP_Complex is
	begin
		return Divide (
			Left,
			Right,
			Real_Precision => Real_FR.Precision,
			Imaginary_Precision => Imaginary_FR.Precision,
			Rounding => Rounding);
	end "/";
	
	function "**" (Left : MP_Complex; Right : Integer) return MP_Complex is
	begin
		return Power (
			Left,
			Right,
			Real_Precision => Real_FR.Precision,
			Imaginary_Precision => Imaginary_FR.Precision,
			Rounding => Rounding);
	end "**";
	
	function "*" (Left : Long_Long_Float; Right : MP_Imaginary)
		return MP_Imaginary is
	begin
		return MP_Imaginary (Left * Imaginary_FR.MP_Float (Right));
	end "*";
	
end MPC.Generic_C;
