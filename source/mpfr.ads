with GMP;
private with C.mpfr;
package MPFR is
	pragma Preelaborate;
	pragma Linker_Options ("-lmpfr");
	
	subtype Number_Base is GMP.Number_Base;
	
	type Precision is mod 2 ** GMP.Exponent'Size;
	
	type Rounding is (
		To_Nearest_With_Ties_Away_From_Zero,
		To_Nearest,
		Towards_Zero,
		Towards_Plus_Infinity,
		Towards_Minus_Infinity,
		Away_From_Zero,
		Faithful);
	
	function Default_Precision return Precision;
	function Default_Rounding return Rounding;
	
private
	
	pragma Compile_Time_Error (
		Precision'Last /= Precision (C.mpfr.mpfr_uprec_t'Last),
		"please adjust Precision as mpfr_uprec_t");
	
	for Rounding use (
		To_Nearest_With_Ties_Away_From_Zero =>
			C.mpfr.mpfr_rnd_t'Enum_Rep (C.mpfr.MPFR_RNDNA),
		To_Nearest =>
			C.mpfr.mpfr_rnd_t'Enum_Rep (C.mpfr.MPFR_RNDN),
		Towards_Zero =>
			C.mpfr.mpfr_rnd_t'Enum_Rep (C.mpfr.MPFR_RNDZ),
		Towards_Plus_Infinity =>
			C.mpfr.mpfr_rnd_t'Enum_Rep (C.mpfr.MPFR_RNDU),
		Towards_Minus_Infinity =>
			C.mpfr.mpfr_rnd_t'Enum_Rep (C.mpfr.MPFR_RNDD),
		Away_From_Zero =>
			C.mpfr.mpfr_rnd_t'Enum_Rep (C.mpfr.MPFR_RNDA),
		Faithful =>
			C.mpfr.mpfr_rnd_t'Enum_Rep (C.mpfr.MPFR_RNDF));
	
end MPFR;
