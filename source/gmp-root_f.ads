private with Ada.Finalization;
package GMP.Root_F is
	pragma Preelaborate;
	
	type MP_Float (Precision : GMP.Precision) is private;
	
	function To_MP_Float (
		X : Long_Float;
		Precision : GMP.Precision)
		return MP_Float;
	
	function To_Long_Float (X : MP_Float) return Long_Float;
	
	function Image (
		Value : MP_Float;
		Base : Number_Base := 10)
		return String;
	function Value (
		Image : String;
		Base : Number_Base := 10;
		Precision : GMP.Precision)
		return MP_Float;
	
	function "=" (Left, Right : MP_Float) return Boolean;
	function "<" (Left, Right : MP_Float) return Boolean;
	function ">" (Left, Right : MP_Float) return Boolean;
	function "<=" (Left, Right : MP_Float) return Boolean;
	function ">=" (Left, Right : MP_Float) return Boolean;
	
	function Copy ( -- Positive
		Right : MP_Float;
		Precision : GMP.Precision := Default_Precision)
		return MP_Float;
	
	function Negative (
		Right : MP_Float;
		Precision : GMP.Precision)
		return MP_Float;
	
	function Add (
		Left, Right : MP_Float;
		Precision : GMP.Precision)
		return MP_Float;
	
	function Subtract (
		Left, Right : MP_Float;
		Precision : GMP.Precision)
		return MP_Float;
	
	function Multiply (
		Left, Right : MP_Float;
		Precision : GMP.Precision)
		return MP_Float;
	
	function Divide (
		Left, Right : MP_Float;
		Precision : GMP.Precision)
		return MP_Float;
	
private
	
	-- [gcc 4.6] avoiding bug
	type Controlled is new Ada.Finalization.Controlled with record
		Raw : aliased C.gmp.mpf_t;
	end record;
	
	function Create (Precision : GMP.Precision) return Controlled;
	
	overriding procedure Initialize (Object : in out Controlled);
	overriding procedure Adjust (Object : in out Controlled);
	overriding procedure Finalize (Object : in out Controlled);
	
	type MP_Float (Precision : GMP.Precision) is record
		Data : Controlled := Create (Precision);
	end record;
	
end GMP.Root_F;
