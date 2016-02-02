pragma Ada_2012;
private with Ada.Finalization;
private with Ada.Streams;
package GMP.Z is
	pragma Preelaborate;
	
	type MP_Integer is private;
	
	-- conversions
	
	function To_MP_Integer (X : Long_Long_Integer) return MP_Integer;
	
	-- formatting
	
	function Image (Value : MP_Integer; Base : Number_Base := 10) return String;
	function Value (Image : String; Base : Number_Base := 10) return MP_Integer;
	
	-- relational operators
	
	function "=" (Left, Right : MP_Integer) return Boolean;
	function "<" (Left, Right : MP_Integer) return Boolean;
	function ">" (Left, Right : MP_Integer) return Boolean;
	function "<=" (Left, Right : MP_Integer) return Boolean;
	function ">=" (Left, Right : MP_Integer) return Boolean;
	
	-- unary adding operators
	
	function "+" (Right : MP_Integer) return MP_Integer;
	function "-" (Right : MP_Integer) return MP_Integer;
	
	-- binary adding operators
	
	function "+" (Left, Right : MP_Integer) return MP_Integer;
	
	function "-" (Left, Right : MP_Integer) return MP_Integer;
	
	-- multiplying operators
	
	function "*" (Left, Right : MP_Integer) return MP_Integer;
	
	function "/" (Left, Right : MP_Integer) return MP_Integer;
	
	-- highest precedence operators
	
	function "**" (Left : MP_Integer; Right : Natural) return MP_Integer;
	
	-- subprograms of a scalar type
	
	function Copy_Sign (Value, Sign : MP_Integer) return MP_Integer;
	function Copy_Sign (Value : MP_Integer; Sign : Integer) return MP_Integer;
	function Copy_Sign (Value : Integer; Sign : MP_Integer) return Integer;
	
private
	
	package Controlled is
		
		type MP_Integer is private;
		
		function Reference (Item : in out Z.MP_Integer)
			return not null access C.gmp.mpz_struct;
		function Constant_Reference (Item : Z.MP_Integer)
			return not null access constant C.gmp.mpz_struct;
		
		pragma Inline (Reference);
		pragma Inline (Constant_Reference);
		
	private
		
		type MP_Integer is new Ada.Finalization.Controlled with record
			Raw : aliased C.gmp.mpz_t := (others => (others => <>));
		end record;
		
		overriding procedure Initialize (Object : in out MP_Integer);
		overriding procedure Adjust (Object : in out MP_Integer);
		overriding procedure Finalize (Object : in out MP_Integer);
		
	end Controlled;
	
	type MP_Integer is new Controlled.MP_Integer;
	
	procedure Read (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out MP_Integer);
	procedure Write (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : in MP_Integer);
	
	for MP_Integer'Read use Read;
	for MP_Integer'Write use Write;
	
	procedure Read (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : not null access C.gmp.mpz_struct);
	procedure Write (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : not null access constant C.gmp.mpz_struct);
	
end GMP.Z;
