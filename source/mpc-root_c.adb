with MPFR.Root_FR.Inside;
with C.mpfr;
with C.string;
package body MPC.Root_C is
	use type C.signed_int;
	
	function Re (X : MP_Complex) return MPFR.Root_FR.MP_Float is
		Source : C.mpfr.mpfr_t renames X.Data.Raw (0).re;
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
	begin
		return Result : MPFR.Root_FR.MP_Float (
			MPFR.Precision (C.mpfr.mpfr_get_prec (Source (0)'Access)))
		do
			Dummy := C.mpfr.mpfr_set4 (
				MPFR.Root_FR.Inside.Reference (Result'Unrestricted_Access.all),
				Source (0)'Access,
				C.mpfr.MPFR_RNDN,
				C.mpfr.mpfr_sgn (Source (0)'Access));
		end return;
	end Re;
	
	function Im (X : MP_Complex) return MPFR.Root_FR.MP_Float is
		Source : C.mpfr.mpfr_t renames X.Data.Raw (0).im;
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
	begin
		return Result : MPFR.Root_FR.MP_Float (
			MPFR.Precision (C.mpfr.mpfr_get_prec (Source (0)'Access)))
		do
			Dummy := C.mpfr.mpfr_set4 (
				MPFR.Root_FR.Inside.Reference (Result'Unrestricted_Access.all),
				Source (0)'Access,
				C.mpfr.MPFR_RNDN,
				C.mpfr.mpfr_sgn (Source (0)'Access));
		end return;
	end Im;
	
	function Compose (Re, Im : MPFR.Root_FR.MP_Float) return MP_Complex is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
	begin
		return Result : MP_Complex (Re.Precision, Im.Precision) do
			Dummy := C.mpc.mpc_set_fr_fr (
				Result.Data.Raw (0)'Access,
				MPFR.Root_FR.Inside.Constant_Reference (Re'Unrestricted_Access.all),
				MPFR.Root_FR.Inside.Constant_Reference (Im'Unrestricted_Access.all),
				C.mpc.MPC_RNDNN);
		end return;
	end Compose;
	
	function Compose (
		Re : Long_Long_Float;
		Real_Precision : MPFR.Precision;
		Im : MPFR.Root_FR.MP_Float)
		return MP_Complex
	is
		Im_Source : constant not null access constant C.mpfr.mpfr_struct :=
			MPFR.Root_FR.Inside.Constant_Reference (Im'Unrestricted_Access.all);
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
	begin
		return Result : MP_Complex (Real_Precision, Im.Precision) do
			Dummy := C.mpfr.mpfr_set_ld (
				Result.Data.Raw (0).re (0)'Access,
				C.long_double (Re),
				C.mpfr.MPFR_RNDN);
			Dummy := C.mpfr.mpfr_set4 (
				Result.Data.Raw (0).im (0)'Access,
				Im_Source,
				C.mpfr.MPFR_RNDN,
				C.mpfr.mpfr_sgn (Im_Source));
		end return;
	end Compose;
	
	function Image (
		Value : MP_Complex;
		Base : Number_Base := 10;
		Rounding : MPC.Rounding)
		return String
	is
		Image : constant C.char_ptr := C.mpc.mpc_get_str (
			C.signed_int (Base),
			0,
			Value.Data.Raw (0)'Access,
			C.mpc.mpc_rnd_t (Rounding));
		Length : constant Natural := Integer (C.string.strlen (Image));
		Ada_Image : String (1 .. Length);
		for Ada_Image'Address use Image.all'Address;
	begin
		return Result : String (1 .. Length) do
			Result := Ada_Image;
			C.mpc.mpc_free_str (Image);
		end return;
	end Image;
	
	function Value (
		Image : String;
		Base : Number_Base := 10;
		Real_Precision : MPFR.Precision;
		Imaginary_Precision : MPFR.Precision;
		Rounding : MPC.Rounding)
		return MP_Complex
	is
		Z_Image : constant String := Image & Character'Val (0);
		C_Image : C.char_array (0 .. Z_Image'Length);
		for C_Image'Address use Z_Image'Address;
	begin
		return Result : MP_Complex (Real_Precision, Imaginary_Precision) do
			if C.mpc.mpc_set_str (
				Result.Data.Raw (0)'Access,
				C_Image (C_Image'First)'Access,
				C.signed_int (Base),
				C.mpc.mpc_rnd_t (Rounding)) < 0
			then
				raise Constraint_Error;
			end if;
		end return;
	end Value;
	
	function "=" (Left, Right : MP_Complex) return Boolean is
	begin
		return C.mpc.mpc_cmp (
			Left.Data.Raw (0)'Access,
			Right.Data.Raw (0)'Access) = 0;
	end "=";
	
	function "<" (Left, Right : MP_Complex) return Boolean is
	begin
		return C.mpc.mpc_cmp (
			Left.Data.Raw (0)'Access,
			Right.Data.Raw (0)'Access) < 0;
	end "<";
	
	function ">" (Left, Right : MP_Complex) return Boolean is
	begin
		return C.mpc.mpc_cmp (
			Left.Data.Raw (0)'Access,
			Right.Data.Raw (0)'Access) > 0;
	end ">";
	
	function "<=" (Left, Right : MP_Complex) return Boolean is
	begin
		return C.mpc.mpc_cmp (
			Left.Data.Raw (0)'Access,
			Right.Data.Raw (0)'Access) <= 0;
	end "<=";
	
	function ">=" (Left, Right : MP_Complex) return Boolean is
	begin
		return C.mpc.mpc_cmp (
			Left.Data.Raw (0)'Access,
			Right.Data.Raw (0)'Access) >= 0;
	end ">=";
	
	function Copy (
		Right : MP_Complex;
		Real_Precision : MPFR.Precision := MPFR.Default_Precision;
		Imaginary_Precision : MPFR.Precision := MPFR.Default_Precision;
		Rounding : MPC.Rounding := Default_Rounding)
		return MP_Complex
	is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
	begin
		return Result : MP_Complex (Real_Precision, Imaginary_Precision) do
			Dummy := C.mpc.mpc_set (
				Result.Data.Raw (0)'Access,
				Right.Data.Raw (0)'Access,
				C.mpc.mpc_rnd_t (Rounding));
		end return;
	end Copy;
	
	function Negative (
		Right : MP_Complex;
		Real_Precision : MPFR.Precision;
		Imaginary_Precision : MPFR.Precision;
		Rounding : MPC.Rounding)
		return MP_Complex
	is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
	begin
		return Result : MP_Complex (Real_Precision, Imaginary_Precision) do
			Dummy := C.mpc.mpc_neg (
				Result.Data.Raw (0)'Access,
				Right.Data.Raw (0)'Access,
				C.mpc.mpc_rnd_t (Rounding));
		end return;
	end Negative;
	
	function Add (
		Left, Right : MP_Complex;
		Real_Precision : MPFR.Precision;
		Imaginary_Precision : MPFR.Precision;
		Rounding : MPC.Rounding)
		return MP_Complex
	is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
	begin
		return Result : MP_Complex (Real_Precision, Imaginary_Precision) do
			Dummy := C.mpc.mpc_add (
				Result.Data.Raw (0)'Access,
				Left.Data.Raw (0)'Access,
				Right.Data.Raw (0)'Access,
				C.mpc.mpc_rnd_t (Rounding));
		end return;
	end Add;
	
	function Subtract (
		Left, Right : MP_Complex;
		Real_Precision : MPFR.Precision;
		Imaginary_Precision : MPFR.Precision;
		Rounding : MPC.Rounding)
		return MP_Complex
	is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
	begin
		return Result : MP_Complex (Real_Precision, Imaginary_Precision) do
			Dummy := C.mpc.mpc_sub (
				Result.Data.Raw (0)'Access,
				Left.Data.Raw (0)'Access,
				Right.Data.Raw (0)'Access,
				C.mpc.mpc_rnd_t (Rounding));
		end return;
	end Subtract;
	
	function Multiply (
		Left, Right : MP_Complex;
		Real_Precision : MPFR.Precision;
		Imaginary_Precision : MPFR.Precision;
		Rounding : MPC.Rounding)
		return MP_Complex
	is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
	begin
		return Result : MP_Complex (Real_Precision, Imaginary_Precision) do
			Dummy := C.mpc.mpc_mul (
				Result.Data.Raw (0)'Access,
				Left.Data.Raw (0)'Access,
				Right.Data.Raw (0)'Access,
				C.mpc.mpc_rnd_t (Rounding));
		end return;
	end Multiply;
	
	function Divide (
		Left, Right : MP_Complex;
		Real_Precision : MPFR.Precision;
		Imaginary_Precision : MPFR.Precision;
		Rounding : MPC.Rounding)
		return MP_Complex
	is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
	begin
		return Result : MP_Complex (Real_Precision, Imaginary_Precision) do
			Dummy := C.mpc.mpc_div (
				Result.Data.Raw (0)'Access,
				Left.Data.Raw (0)'Access,
				Right.Data.Raw (0)'Access,
				C.mpc.mpc_rnd_t (Rounding));
		end return;
	end Divide;
	
	function Create (
		Real_Precision : MPFR.Precision;
		Imaginary_Precision : MPFR.Precision)
		return Controlled is
	begin
		return Result : Controlled := (Ada.Finalization.Controlled with Raw => <>) do
			C.mpc.mpc_init3 (
				Result.Raw (0)'Access,
				C.mpfr.mpfr_prec_t (Real_Precision),
				C.mpfr.mpfr_prec_t (Imaginary_Precision));
		end return;
	end Create;
	
	overriding procedure Initialize (Object : in out Controlled) is
	begin
		raise Program_Error;
	end Initialize;
	
	overriding procedure Adjust (Object : in out Controlled) is
		Source : constant C.mpc.mpc_t := Object.Raw; -- move
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
	begin
		C.mpc.mpc_init3 (
			Object.Raw (0)'Access,
			C.mpfr.mpfr_get_prec (Source (0).re (0)'Access),
			C.mpfr.mpfr_get_prec (Source (0).im (0)'Access));
		Dummy := C.mpc.mpc_set (
			Object.Raw (0)'Access,
			Source (0)'Access,
			C.mpc.MPC_RNDNN);
	end Adjust;
	
	overriding procedure Finalize (Object : in out Controlled) is
	begin
		C.mpc.mpc_clear (Object.Raw (0)'Access);
	end Finalize;
	
end MPC.Root_C;
