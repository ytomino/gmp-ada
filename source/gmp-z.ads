private with Ada.Finalization;
private with Ada.Streams;
package GMP.Z is
	pragma Preelaborate;
	
	type MP_Integer is private;
	
	function To_MP_Integer (X : Long_Long_Integer) return MP_Integer;
	
	function Image (Value : MP_Integer; Base : Number_Base := 10) return String;
	function Value (Image : String; Base : Number_Base := 10) return MP_Integer;
	
	function "=" (Left, Right : MP_Integer) return Boolean;
	function "<" (Left, Right : MP_Integer) return Boolean;
	function ">" (Left, Right : MP_Integer) return Boolean;
	function "<=" (Left, Right : MP_Integer) return Boolean;
	function ">=" (Left, Right : MP_Integer) return Boolean;
	
	function "+" (Right : MP_Integer) return MP_Integer;
	function "-" (Right : MP_Integer) return MP_Integer;
	
	function "+" (Left, Right : MP_Integer) return MP_Integer;
	
	function "-" (Left, Right : MP_Integer) return MP_Integer;
	
	function "*" (Left, Right : MP_Integer) return MP_Integer;
	
	function "/" (Left, Right : MP_Integer) return MP_Integer;
	
	function "**" (Left : MP_Integer; Right : Natural) return MP_Integer;
	
	function Copy_Sign (Value, Sign : MP_Integer) return MP_Integer;
	function Copy_Sign (Value : MP_Integer; Sign : Integer) return MP_Integer;
	function Copy_Sign (Value : Integer; Sign : MP_Integer) return Integer;
	
private
	
	type Controlled is new Ada.Finalization.Controlled with record
		Raw : aliased C.gmp.mpz_t := (others => (others => <>));
	end record;
	
	overriding procedure Initialize (Object : in out Controlled);
	overriding procedure Adjust (Object : in out Controlled);
	overriding procedure Finalize (Object : in out Controlled);
	
	type MP_Integer is record
		Data : Controlled;
	end record;
	
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
