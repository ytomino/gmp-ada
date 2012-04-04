with C.string;
package body GMP is
	
	function Version return String is
		S : constant C.char_const_ptr := C.gmp.qqgmp_version;
		Length : constant Natural := Natural (C.string.strlen (S));
		Result : String (1 .. Length);
		for Result'Address use S.all'Address;
	begin
		return Result;
	end Version;
	
	function Default_Precision return Precision is
	begin
		return Precision (C.gmp.gmpf_get_default_prec);
	end Default_Precision;
	
	procedure mpz_init_set_Long_Long_Integer (
		rop : not null access C.gmp.mpz_struct;
		op : in Long_Long_Integer)
	is
		subtype ui is Long_Long_Integer range
			Long_Long_Integer (C.unsigned_long'First) ..
			Long_Long_Integer (
				C.unsigned_long_long'Min (
					C.unsigned_long_long (C.unsigned_long'Last),
					C.unsigned_long_long (Long_Long_Integer'Last)));
		subtype si is Long_Long_Integer range
			Long_Long_Integer (C.signed_long'First) ..
			Long_Long_Integer (C.signed_long'Last);
	begin
		pragma Warnings (Off, "explicit membership test may be optimized away");
		if op in ui then
			C.gmp.mpz_init_set_ui (rop, C.unsigned_long (op));
		elsif op in si then
			C.gmp.mpz_init_set_si (rop, C.signed_long (op));
		else
			C.gmp.mpz_init_set_si (
				rop,
				C.signed_long (C.Shift_Right_Arithmetic (
					C.signed_long_long (op),
					C.unsigned_long'Size)));
			C.gmp.mpz_mul_2exp (
				rop,
				rop,
				C.unsigned_long'Size);
			C.gmp.mpz_add_ui (
				rop,
				rop,
				C.unsigned_long'Mod (op));
		end if;
		pragma Warnings (On, "explicit membership test may be optimized away");
	end mpz_init_set_Long_Long_Integer;
	
end GMP;
