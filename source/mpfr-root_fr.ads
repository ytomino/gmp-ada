private with Ada.Finalization;
package MPFR.Root_FR is
	pragma Preelaborate;
	
	type MP_Float (Precision : MPFR.Precision) is private;
	
	function To_MP_Float (
		X : Long_Long_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float;
	
	function To_Long_Long_Float (X : MP_Float; Rounding : MPFR.Rounding)
		return Long_Long_Float;
	
	function Image (
		Value : MP_Float;
		Base : Number_Base := 10;
		Rounding : MPFR.Rounding)
		return String;
	function Value (
		Image : String;
		Base : Number_Base := 10;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float;
	
	function "=" (Left, Right : MP_Float) return Boolean;
	function "<" (Left, Right : MP_Float) return Boolean;
	function ">" (Left, Right : MP_Float) return Boolean;
	function "<=" (Left, Right : MP_Float) return Boolean;
	function ">=" (Left, Right : MP_Float) return Boolean;
	
	function Copy ( -- Positive
		Right : MP_Float;
		Precision : MPFR.Precision := Default_Precision;
		Rounding : MPFR.Rounding := Default_Rounding)
		return MP_Float;
	
	function Negative (
		Right : MP_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float;
	
	function Add (
		Left, Right : MP_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float;
	function Add (
		Left : MP_Float;
		Right : Long_Long_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float;
	function Add (
		Left : Long_Long_Float;
		Right : MP_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float;
	function Subtract (
		Left, Right : MP_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float;
	function Subtract (
		Left : MP_Float;
		Right : Long_Long_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float;
	function Subtract (
		Left : Long_Long_Float;
		Right : MP_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float;
	
	function Multiply (
		Left, Right : MP_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float;
	function Multiply (
		Left : MP_Float;
		Right : Long_Long_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float;
	function Multiply (
		Left : Long_Long_Float;
		Right : MP_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float;
	function Divide (
		Left, Right : MP_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float;
	function Divide (
		Left : MP_Float;
		Right : Long_Long_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float;
	function Divide (
		Left : Long_Long_Float;
		Right : MP_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float;
	
	function Power (
		Left : MP_Float;
		Right : Integer;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float;
	
	function Sqrt (
		X : MP_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float;
	
	function NaN (Precision : MPFR.Precision) return MP_Float;
	
	function Infinity (Precision : MPFR.Precision) return MP_Float;
	
private
	
	package Controlled is
		
		type MP_Float is private;
		
		function Create (Precision : MPFR.Precision) return MP_Float;
		
		function Reference (Item : in out Root_FR.MP_Float)
			return not null access C.mpfr.mpfr_struct;
		function Constant_Reference (Item : Root_FR.MP_Float)
			return not null access constant C.mpfr.mpfr_struct;
		
		pragma Inline (Reference);
		pragma Inline (Constant_Reference);
		
	private
		
		type MP_Float is new Ada.Finalization.Controlled
			with record
				Raw : aliased C.mpfr.mpfr_t := (others => (others => <>));
			end record;
		
		overriding procedure Initialize (Object : in out MP_Float);
		overriding procedure Adjust (Object : in out MP_Float);
		overriding procedure Finalize (Object : in out MP_Float);
		
	end Controlled;
	
	-- [gcc-4.8/4.9/5.0] derivation with discriminants makes many problems.
	type MP_Float (Precision : MPFR.Precision) is record
		Data : Controlled.MP_Float := Controlled.Create (Precision);
	end record;
	
end MPFR.Root_FR;
