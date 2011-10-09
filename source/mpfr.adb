with C.string;
package body MPFR is
	
	function Version return String is
		S : constant C.char_const_ptr := C.mpfr.mpfr_get_version;
		Length : constant Natural := Natural (C.string.strlen (S));
		Result : String (1 .. Length);
		for Result'Address use S.all'Address;
	begin
		return Result;
	end Version;
	
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
