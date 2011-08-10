package body MPC is
	
	function Compose (
		Real_Rounding : MPFR.Rounding;
		Imaginary_Rounding : MPFR.Rounding)
		return Rounding is
	begin
		return MPFR.Rounding'Enum_Rep (Real_Rounding)
			+ MPFR.Rounding'Enum_Rep (Imaginary_Rounding) * (2 ** 4);
	end Compose;
	
	function Default_Rounding return Rounding is
	begin
		return Compose (MPFR.Default_Rounding, MPFR.Default_Rounding);
	end Default_Rounding;
	
end MPC;
