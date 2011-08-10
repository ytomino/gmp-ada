with GMP.Z;
private with Ada.Finalization;
private with Ada.Streams;
package GMP.Q is
	pragma Preelaborate;
	
	type MP_Rational is private;
	
	function Num (X : MP_Rational) return Z.MP_Integer;
	function Den (X : MP_Rational) return Z.MP_Integer;
	
	function Image (Value : MP_Rational; Base : Number_Base := 10)
		return String;
	function Value (Image : String; Base : Number_Base := 10)
		return MP_Rational;
	
	function "=" (Left, Right : MP_Rational) return Boolean;
	function "<" (Left, Right : MP_Rational) return Boolean;
	function ">" (Left, Right : MP_Rational) return Boolean;
	function "<=" (Left, Right : MP_Rational) return Boolean;
	function ">=" (Left, Right : MP_Rational) return Boolean;
	
	function "+" (Right : MP_Rational) return MP_Rational;
	function "-" (Right : MP_Rational) return MP_Rational;
	
	function "+" (Left, Right : MP_Rational) return MP_Rational;
	
	function "-" (Left, Right : MP_Rational) return MP_Rational;
	
	function "*" (Left, Right : MP_Rational) return MP_Rational;
	
	function "/" (Left, Right : MP_Rational) return MP_Rational;
	function "/" (Left, Right : Long_Long_Integer) return MP_Rational;
	function "/" (Left, Right : Z.MP_Integer) return MP_Rational;
	
private
	
	type Controlled is new Ada.Finalization.Controlled with record
		Raw : aliased C.gmp.mpq_t;
	end record;
	
	overriding procedure Initialize (Object : in out Controlled);
	overriding procedure Adjust (Object : in out Controlled);
	overriding procedure Finalize (Object : in out Controlled);
	
	type MP_Rational is record
		Data : Controlled;
	end record;
	
	procedure Read (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out MP_Rational);
	procedure Write (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : in MP_Rational);
	
	for MP_Rational'Read use Read;
	for MP_Rational'Write use Write;
	
end GMP.Q;
