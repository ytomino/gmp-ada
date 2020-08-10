with MPFR.Root_FR;
private with Ada.Finalization;
package MPC.Root_C is
	pragma Preelaborate;
	
	type MP_Complex (
		Real_Precision : MPFR.Precision;
		Imaginary_Precision : MPFR.Precision) is private;
	
	function Re (X : MP_Complex) return MPFR.Root_FR.MP_Float;
	function Im (X : MP_Complex) return MPFR.Root_FR.MP_Float;
	
	function Compose (Re, Im : MPFR.Root_FR.MP_Float) return MP_Complex;
	function Compose (
		Re : Long_Long_Float;
		Real_Precision : MPFR.Precision;
		Im : MPFR.Root_FR.MP_Float)
		return MP_Complex;
	
	function Image (
		Value : MP_Complex;
		Base : Number_Base := 10;
		Rounding : MPC.Rounding)
		return String;
	function Value (
		Image : String;
		Base : Number_Base := 10;
		Real_Precision : MPFR.Precision;
		Imaginary_Precision : MPFR.Precision;
		Rounding : MPC.Rounding)
		return MP_Complex;
	
	function "=" (Left, Right : MP_Complex) return Boolean;
	function "<" (Left, Right : MP_Complex) return Boolean;
	function ">" (Left, Right : MP_Complex) return Boolean;
	function "<=" (Left, Right : MP_Complex) return Boolean;
	function ">=" (Left, Right : MP_Complex) return Boolean;
	
	function Copy ( -- Positive
		Right : MP_Complex;
		Real_Precision : MPFR.Precision := MPFR.Default_Precision;
		Imaginary_Precision : MPFR.Precision := MPFR.Default_Precision;
		Rounding : MPC.Rounding := Default_Rounding)
		return MP_Complex;
	
	function Negative (
		Right : MP_Complex;
		Real_Precision : MPFR.Precision;
		Imaginary_Precision : MPFR.Precision;
		Rounding : MPC.Rounding)
		return MP_Complex;
	
	function Add (
		Left, Right : MP_Complex;
		Real_Precision : MPFR.Precision;
		Imaginary_Precision : MPFR.Precision;
		Rounding : MPC.Rounding)
		return MP_Complex;
	function Subtract (
		Left, Right : MP_Complex;
		Real_Precision : MPFR.Precision;
		Imaginary_Precision : MPFR.Precision;
		Rounding : MPC.Rounding)
		return MP_Complex;
	
	function Multiply (
		Left, Right : MP_Complex;
		Real_Precision : MPFR.Precision;
		Imaginary_Precision : MPFR.Precision;
		Rounding : MPC.Rounding)
		return MP_Complex;
	function Divide (
		Left, Right : MP_Complex;
		Real_Precision : MPFR.Precision;
		Imaginary_Precision : MPFR.Precision;
		Rounding : MPC.Rounding)
		return MP_Complex;
	
	function Power (
		Left : MP_Complex;
		Right : Integer;
		Real_Precision : MPFR.Precision;
		Imaginary_Precision : MPFR.Precision;
		Rounding : MPC.Rounding)
		return MP_Complex;
	
private
	
	package Controlled is
		
		type MP_Complex is private;
		
		function Create (
			Real_Precision : MPFR.Precision;
			Imaginary_Precision : MPFR.Precision)
			return MP_Complex;
		
		function Reference (Item : in out Root_C.MP_Complex)
			return not null access C.mpc.mpc_struct;
		function Constant_Reference (Item : Root_C.MP_Complex)
			return not null access constant C.mpc.mpc_struct;
		
		pragma Inline (Reference);
		pragma Inline (Constant_Reference);
		
	private
		
		type MP_Complex is new Ada.Finalization.Controlled
			with record
				Raw : aliased C.mpc.mpc_t :=
					(others => (others => (others => (others => <>))));
			end record;
		
		overriding procedure Initialize (Object : in out MP_Complex);
		overriding procedure Adjust (Object : in out MP_Complex);
		overriding procedure Finalize (Object : in out MP_Complex);
		
	end Controlled;
	
	-- [gcc-4.8/4.9/5.0] derivation with discriminants makes many problems.
	type MP_Complex (
		Real_Precision : MPFR.Precision;
		Imaginary_Precision : MPFR.Precision) is
	record
		Data : Controlled.MP_Complex :=
			Controlled.Create (Real_Precision, Imaginary_Precision);
	end record;
	
end MPC.Root_C;
