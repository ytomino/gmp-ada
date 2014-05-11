pragma Ada_2012;
with Ada.Streams;
with C.gmp;
package GMP.Z.Inside is
	pragma Preelaborate;
	
	function Reference (X : aliased in out MP_Integer)
		return not null access C.gmp.mpz_struct;
	pragma Inline (Reference);
	function Constant_Reference (X : aliased in MP_Integer)
		return not null access constant C.gmp.mpz_struct;
	pragma Inline (Constant_Reference);
	
	procedure Read (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : not null access C.gmp.mpz_struct);
	procedure Write (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : not null access constant C.gmp.mpz_struct);
	
private	
	
	procedure Read (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : not null access C.gmp.mpz_struct)
		renames Z.Read;
	procedure Write (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : not null access constant C.gmp.mpz_struct)
		renames Z.Write;
	
end GMP.Z.Inside;
