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
	
	procedure mpz_set_Long_Long_Integer (
		rop : not null access C.gmp.mpz_struct;
		op : in Long_Long_Integer)
	is
		subtype ui is Long_Long_Integer range
			Long_Long_Integer (C.unsigned_long'First) ..
			Long_Long_Integer (
				C.unsigned_long_long'Min (
					C.unsigned_long_long (C.unsigned_long'Last),
					C.unsigned_long_long (Long_Long_Integer'Last)));
		op_in_ui : constant Boolean := op in ui;
	begin
		if op_in_ui then
			C.gmp.mpz_set_ui (rop, C.unsigned_long'Mod (op));
		else
			declare
				subtype si is Long_Long_Integer range
					Long_Long_Integer (C.signed_long'First) ..
					Long_Long_Integer (C.signed_long'Last);
				pragma Warnings (Off); -- always True in 64bit environment
				op_in_si : constant Boolean := op in si;
				pragma Warnings (On);
			begin
				if op_in_si then
					C.gmp.mpz_set_si (rop, C.signed_long (op));
				else
					C.gmp.mpz_set_si (
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
			end;
		end if;
	end mpz_set_Long_Long_Integer;
	
end GMP;
