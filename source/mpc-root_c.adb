with MPFR.Root_FR.Inside;
with System;
with C.mpfr;
with C.string;
package body MPC.Root_C is
	use type C.signed_int;
	
	procedure memcpy (dst, src : System.Address; n : C.size_t)
		with Import, Convention => Intrinsic, External_Name => "__builtin_memcpy";
	
	-- implementation
	
	function Re (X : MP_Complex) return MPFR.Root_FR.MP_Float is
		Source : C.mpfr.mpfr_t renames Controlled.Constant_Reference (X).re;
		Dummy : C.signed_int;
	begin
		return Result :
			MPFR.Root_FR.MP_Float (
				MPFR.Precision (C.mpfr.mpfr_get_prec (Source (0)'Access)))
		do
			Dummy :=
				C.mpfr.mpfr_set4 (
					MPFR.Root_FR.Inside.Reference (Result),
					Source (0)'Access,
					C.mpfr.MPFR_RNDN,
					C.mpfr.mpfr_sgn (Source (0)'Access));
		end return;
	end Re;
	
	function Im (X : MP_Complex) return MPFR.Root_FR.MP_Float is
		Source : C.mpfr.mpfr_t renames Controlled.Constant_Reference (X).im;
		Dummy : C.signed_int;
	begin
		return Result :
			MPFR.Root_FR.MP_Float (
				MPFR.Precision (C.mpfr.mpfr_get_prec (Source (0)'Access)))
		do
			Dummy :=
				C.mpfr.mpfr_set4 (
					MPFR.Root_FR.Inside.Reference (Result),
					Source (0)'Access,
					C.mpfr.MPFR_RNDN,
					C.mpfr.mpfr_sgn (Source (0)'Access));
		end return;
	end Im;
	
	function Compose (Re, Im : MPFR.Root_FR.MP_Float) return MP_Complex is
		Dummy : C.signed_int;
	begin
		return Result : MP_Complex (Re.Precision, Im.Precision) do
			Dummy :=
				C.mpc.mpc_set_fr_fr (
					Controlled.Reference (Result),
					MPFR.Root_FR.Inside.Constant_Reference (Re),
					MPFR.Root_FR.Inside.Constant_Reference (Im),
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
			MPFR.Root_FR.Inside.Constant_Reference (Im);
		Dummy : C.signed_int;
	begin
		return Result : MP_Complex (Real_Precision, Im.Precision) do
			declare
				Raw_Result : constant not null access C.mpc.mpc_struct :=
					Controlled.Reference (Result);
			begin
				Dummy :=
					C.mpfr.mpfr_set_ld (
						Raw_Result.re (0)'Access,
						C.long_double (Re),
						C.mpfr.MPFR_RNDN);
				Dummy :=
					C.mpfr.mpfr_set4 (
						Raw_Result.im (0)'Access,
						Im_Source,
						C.mpfr.MPFR_RNDN,
						C.mpfr.mpfr_sgn (Im_Source));
			end;
		end return;
	end Compose;
	
	function Image (
		Value : MP_Complex;
		Base : Number_Base := 10;
		Rounding : MPC.Rounding)
		return String
	is
		Image : constant C.char_ptr :=
			C.mpc.mpc_get_str (
				C.signed_int (Base),
				0,
				Controlled.Constant_Reference (Value),
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
		Image_Length : constant C.size_t := Image'Length;
		C_Image : C.char_array (0 .. Image_Length); -- NUL
	begin
		memcpy (C_Image'Address, Image'Address, Image_Length);
		C_Image (Image_Length) := C.char'Val (0);
		return Result : MP_Complex (Real_Precision, Imaginary_Precision) do
			if C.mpc.mpc_set_str (
				Controlled.Reference (Result),
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
			Controlled.Constant_Reference (Left),
			Controlled.Constant_Reference (Right)) = 0;
	end "=";
	
	function "<" (Left, Right : MP_Complex) return Boolean is
	begin
		return C.mpc.mpc_cmp (
			Controlled.Constant_Reference (Left),
			Controlled.Constant_Reference (Right)) < 0;
	end "<";
	
	function ">" (Left, Right : MP_Complex) return Boolean is
	begin
		return C.mpc.mpc_cmp (
			Controlled.Constant_Reference (Left),
			Controlled.Constant_Reference (Right)) > 0;
	end ">";
	
	function "<=" (Left, Right : MP_Complex) return Boolean is
	begin
		return C.mpc.mpc_cmp (
			Controlled.Constant_Reference (Left),
			Controlled.Constant_Reference (Right)) <= 0;
	end "<=";
	
	function ">=" (Left, Right : MP_Complex) return Boolean is
	begin
		return C.mpc.mpc_cmp (
			Controlled.Constant_Reference (Left),
			Controlled.Constant_Reference (Right)) >= 0;
	end ">=";
	
	function Copy (
		Right : MP_Complex;
		Real_Precision : MPFR.Precision := MPFR.Default_Precision;
		Imaginary_Precision : MPFR.Precision := MPFR.Default_Precision;
		Rounding : MPC.Rounding := Default_Rounding)
		return MP_Complex
	is
		Dummy : C.signed_int;
	begin
		return Result : MP_Complex (Real_Precision, Imaginary_Precision) do
			Dummy :=
				C.mpc.mpc_set (
					Controlled.Reference (Result),
					Controlled.Constant_Reference (Right),
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
	begin
		return Result : MP_Complex (Real_Precision, Imaginary_Precision) do
			Dummy :=
				C.mpc.mpc_neg (
					Controlled.Reference (Result),
					Controlled.Constant_Reference (Right),
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
	begin
		return Result : MP_Complex (Real_Precision, Imaginary_Precision) do
			Dummy :=
				C.mpc.mpc_add (
					Controlled.Reference (Result),
					Controlled.Constant_Reference (Left),
					Controlled.Constant_Reference (Right),
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
	begin
		return Result : MP_Complex (Real_Precision, Imaginary_Precision) do
			Dummy :=
				C.mpc.mpc_sub (
					Controlled.Reference (Result),
					Controlled.Constant_Reference (Left),
					Controlled.Constant_Reference (Right),
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
	begin
		return Result : MP_Complex (Real_Precision, Imaginary_Precision) do
			Dummy :=
				C.mpc.mpc_mul (
					Controlled.Reference (Result),
					Controlled.Constant_Reference (Left),
					Controlled.Constant_Reference (Right),
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
	begin
		return Result : MP_Complex (Real_Precision, Imaginary_Precision) do
			Dummy :=
				C.mpc.mpc_div (
					Controlled.Reference (Result),
					Controlled.Constant_Reference (Left),
					Controlled.Constant_Reference (Right),
					C.mpc.mpc_rnd_t (Rounding));
		end return;
	end Divide;
	
	function Power (
		Left : MP_Complex;
		Right : Integer;
		Real_Precision : MPFR.Precision;
		Imaginary_Precision : MPFR.Precision;
		Rounding : MPC.Rounding)
		return MP_Complex
	is
		Dummy : C.signed_int;
	begin
		return Result : MP_Complex (Real_Precision, Imaginary_Precision) do
			Dummy :=
				C.mpc.mpc_pow_si (
					Controlled.Reference (Result),
					Controlled.Constant_Reference (Left),
					C.signed_long (Right),
					C.mpc.mpc_rnd_t (Rounding));
		end return;
	end Power;
	
	package body Controlled is
		
		function Create (
			Real_Precision : MPFR.Precision;
			Imaginary_Precision : MPFR.Precision)
			return MP_Complex is
		begin
			return Result : MP_Complex := (Ada.Finalization.Controlled with Raw => <>) do
				C.mpc.mpc_init3 (
					Result.Raw (0)'Access,
					C.mpfr.mpfr_prec_t (Real_Precision),
					C.mpfr.mpfr_prec_t (Imaginary_Precision));
			end return;
		end Create;
		
		function Reference (Item : in out Root_C.MP_Complex)
			return not null access C.mpc.mpc_struct is
		begin
			return Item.Data.Raw (0)'Unchecked_Access;
		end Reference;
		
		function Constant_Reference (Item : Root_C.MP_Complex)
			return not null access constant C.mpc.mpc_struct is
		begin
			return Item.Data.Raw (0)'Unchecked_Access;
		end Constant_Reference;
		
		overriding procedure Initialize (Object : in out MP_Complex) is
		begin
			raise Program_Error;
		end Initialize;
		
		overriding procedure Adjust (Object : in out MP_Complex) is
			Source : constant C.mpc.mpc_t := Object.Raw; -- move
			Dummy : C.signed_int;
		begin
			C.mpc.mpc_init3 (
				Object.Raw (0)'Access,
				C.mpfr.mpfr_get_prec (Source (0).re (0)'Access),
				C.mpfr.mpfr_get_prec (Source (0).im (0)'Access));
			Dummy :=
				C.mpc.mpc_set (Object.Raw (0)'Access, Source (0)'Access, C.mpc.MPC_RNDNN);
		end Adjust;
		
		overriding procedure Finalize (Object : in out MP_Complex) is
		begin
			C.mpc.mpc_clear (Object.Raw (0)'Access);
		end Finalize;
		
	end Controlled;
	
end MPC.Root_C;
