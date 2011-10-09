with C.string;
package body MPC is
	
	function Version return String is
		S : constant C.char_const_ptr := C.mpc.mpc_get_version;
		Length : constant Natural := Natural (C.string.strlen (S));
		Result : String (1 .. Length);
		for Result'Address use S.all'Address;
	begin
		return Result;
	end Version;
	
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
