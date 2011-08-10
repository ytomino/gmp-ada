package body MPFR is
	
	function Default_Precision return Precision is
	begin
		return Precision (C.mpfr.mpfr_get_default_prec);
	end Default_Precision;
	
	function Default_Rounding return Rounding is
		Repr : constant Integer :=
			C.mpfr.mpfr_rnd_t'Enum_Rep (C.mpfr.mpfr_get_default_rounding_mode);
	begin
		return Rounding'Enum_Val (Repr);
	end Default_Rounding;
	
end MPFR;
