pragma Ada_2012;
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
	
	function Power (
		Left : MP_Float;
		Right : Integer;
		Precision : GMP.Precision)
		return MP_Float;
	
private
	
	package Controlled is
		
		type MP_Float is private;
		
		function Create (Precision : GMP.Precision) return MP_Float;
		
		function Reference (Item : in out Root_F.MP_Float)
			return not null access C.gmp.mpf_struct;
		function Constant_Reference (Item : Root_F.MP_Float)
			return not null access constant C.gmp.mpf_struct;
		
		pragma Inline (Reference);
		pragma Inline (Constant_Reference);
		
	private
		
		type MP_Float is new Ada.Finalization.Controlled with record
			Raw : aliased C.gmp.mpf_t := (others => (others => <>));
		end record;
		
		overriding procedure Initialize (Object : in out MP_Float);
		overriding procedure Adjust (Object : in out MP_Float);
		overriding procedure Finalize (Object : in out MP_Float);
		
	end Controlled;
	
	-- [gcc-4.8/4.9/5.0] derivation with discriminants makes many problems.
	type MP_Float (Precision : GMP.Precision) is record
		Data : Controlled.MP_Float := Controlled.Create (Precision);
	end record;
	
end GMP.Root_F;
