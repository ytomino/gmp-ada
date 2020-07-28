with Ada.Streams;
with C.gmp;
package GMP.Z.Inside is
	pragma Preelaborate;
	
	function Reference (X : in out MP_Integer)
		return not null access C.gmp.mpz_struct;
	function Constant_Reference (X : MP_Integer)
		return not null access constant C.gmp.mpz_struct;
	
	pragma Inline (Reference); -- renamed
	pragma Inline (Constant_Reference); -- renamed
	
	procedure Read (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : not null access C.gmp.mpz_struct);
	procedure Write (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : not null access constant C.gmp.mpz_struct);
	
	pragma Inline (Read); -- renamed
	pragma Inline (Write); -- renamed
	
private	
	
	function Reference (X : in out MP_Integer)
		return not null access C.gmp.mpz_struct
		renames Controlled.Reference;
	function Constant_Reference (X : MP_Integer)
		return not null access constant C.gmp.mpz_struct
		renames Controlled.Constant_Reference;
	
	procedure Read (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : not null access C.gmp.mpz_struct)
		renames Z.Read;
	procedure Write (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : not null access constant C.gmp.mpz_struct)
		renames Z.Write;
	
end GMP.Z.Inside;
