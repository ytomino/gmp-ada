package body GMP is

	function Default_Precision return Precision is
	begin
		return Precision (C.gmp.gmpf_get_default_prec);
	end Default_Precision;
	
	procedure mpz_init_set_Long_Long_Integer (
		rop : not null access C.gmp.mpz_struct;
		op : in Long_Long_Integer) is
	begin
		if op in
			Long_Long_Integer (C.unsigned_long'First) ..
			Long_Long_Integer (C.unsigned_long'Last)
		then
			C.gmp.mpz_init_set_ui (rop, C.unsigned_long (op));
		elsif op in
			Long_Long_Integer (C.signed_long'First) ..
			Long_Long_Integer (C.signed_long'Last)
		then
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
	end mpz_init_set_Long_Long_Integer;
	
end GMP;
