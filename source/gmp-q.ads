with GMP.Z;
private with Ada.Finalization;
private with Ada.Streams;
package GMP.Q is
	pragma Preelaborate;
	
	type MP_Rational is private;
	
	function Num (X : MP_Rational) return Z.MP_Integer;
	function Den (X : MP_Rational) return Z.MP_Integer;
	
	-- formatting
	
	function Image (Value : MP_Rational; Base : Number_Base := 10)
		return String;
	function Value (Image : String; Base : Number_Base := 10)
		return MP_Rational;
	
	-- relational operators
	
	function "=" (Left, Right : MP_Rational) return Boolean;
	function "<" (Left, Right : MP_Rational) return Boolean;
	function ">" (Left, Right : MP_Rational) return Boolean;
	function "<=" (Left, Right : MP_Rational) return Boolean;
	function ">=" (Left, Right : MP_Rational) return Boolean;
	
	-- unary adding operators
	
	function "+" (Right : MP_Rational) return MP_Rational;
	function "-" (Right : MP_Rational) return MP_Rational;
	
	-- binary adding operators
	
	function "+" (Left, Right : MP_Rational) return MP_Rational;
	
	function "-" (Left, Right : MP_Rational) return MP_Rational;
	
	-- multiplying operators
	
	function "*" (Left, Right : MP_Rational) return MP_Rational;
	
	function "/" (Left, Right : MP_Rational) return MP_Rational;
	function "/" (Left, Right : Long_Long_Integer) return MP_Rational;
	function "/" (Left, Right : Z.MP_Integer) return MP_Rational;
	
	-- highest precedence operators
	
	function "**" (Left : MP_Rational; Right : Integer) return MP_Rational;
	
private
	
	package Controlled is
		
		type MP_Rational is private;
		
		function Reference (Item : in out Q.MP_Rational)
			return not null access C.gmp.mpq_struct;
		function Constant_Reference (Item : Q.MP_Rational)
			return not null access constant C.gmp.mpq_struct;
		
		pragma Inline (Reference);
		pragma Inline (Constant_Reference);
		
	private
		
		type MP_Rational is new Ada.Finalization.Controlled with record
			Raw : aliased C.gmp.mpq_t := (others => (others => (others => <>)));
		end record;
		
		overriding procedure Initialize (Object : in out MP_Rational);
		overriding procedure Adjust (Object : in out MP_Rational);
		overriding procedure Finalize (Object : in out MP_Rational);
	
	end Controlled;
	
	type MP_Rational is new Controlled.MP_Rational;
	
	procedure Read (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out MP_Rational);
	procedure Write (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : in MP_Rational);
	
	for MP_Rational'Read use Read;
	for MP_Rational'Write use Write;
	
end GMP.Q;
