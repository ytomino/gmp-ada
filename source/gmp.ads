private with C.gmp;
package GMP is
	pragma Preelaborate;
	pragma Linker_Options ("-lgmp");
	
	subtype Number_Base is Integer range 2 .. 16;
	
	type Exponent is new Long_Integer;
	type Precision is mod 2 ** GMP.Exponent'Size;
	
	function Default_Precision return Precision;
	
private
	
	pragma Compile_Time_Error (
		Exponent'Last /= Exponent (C.gmp.mp_exp_t'Last),
		"please adjust Exponent as mp_exp_t");
	pragma Compile_Time_Error (
		Precision'Last /= Precision (C.gmp.mp_bitcnt_t'Last),
		"please adjust Precision as mp_bitcnt_t");
	
	procedure mpz_init_set_Long_Long_Integer (
		rop : not null access C.gmp.mpz_struct;
		op : in Long_Long_Integer);
	
end GMP;
