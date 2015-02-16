pragma Ada_2012;
with C.string;
package body MPFR.Root_FR is
	use type C.signed_int;
	
	function To_MP_Float (
		X : Long_Long_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float
	is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
	begin
		return Result : MP_Float (Precision) do
			Dummy := C.mpfr.mpfr_set_ld (
				Reference (Result),
				C.long_double (X),
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)));
		end return;
	end To_MP_Float;
	
	function To_Long_Long_Float (
		X : MP_Float;
		Rounding : MPFR.Rounding)
		return Long_Long_Float is
	begin
		return Long_Long_Float (C.mpfr.mpfr_get_ld (
			Constant_Reference (X),
			C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding))));
	end To_Long_Long_Float;
	
	function Image (
		Value : MP_Float;
		Base : Number_Base := 10;
		Rounding : MPFR.Rounding)
		return String is
	begin
		if C.mpfr.mpfr_nan_p (Constant_Reference (Value)) /= 0 then
			return "NAN";
		elsif C.mpfr.mpfr_inf_p (Constant_Reference (Value)) /= 0 then
			declare
				Image : constant String := "-INF";
				Sign : constant Integer range 0 .. 1 :=
					Integer (C.mpfr.mpfr_signbit (Constant_Reference (Value)));
			begin
				return Image (Image'First + 1 - Sign .. Image'Last);
			end;
		else
			declare
				Exponent : aliased C.mpfr.mpfr_exp_t;
				Image : constant C.char_ptr := C.mpfr.mpfr_get_str (
					null,
					Exponent'Access,
					C.signed_int (Base),
					0,
					Constant_Reference (Value),
					C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)));
				Length : constant Natural := Integer (C.string.strlen (Image));
				Ada_Image : String (1 .. Length);
				for Ada_Image'Address use Image.all'Address;
				Sign_Width : constant Natural := Boolean'Pos (
					Length > 0 and then Ada_Image (1) = '-');
				Exponent_P : constant Natural := Integer (Exponent) + Sign_Width;
			begin
				if Length = 0 then
					C.mpfr.mpfr_free_str (Image);
					return "0";
				elsif Exponent_P >= Length then
					return Result : String (1 .. Exponent_P) do
						Result (1 .. Length) := Ada_Image;
						Result (Length + 1 .. Result'Last) := (others => '0');
						C.mpfr.mpfr_free_str (Image);
					end return;
				elsif Exponent_P > 0 then
					return Result : String (1 .. Length + 1) do -- add '.'
						Result (Result'First .. Exponent_P) :=
							Ada_Image (Ada_Image'First .. Exponent_P);
						Result (Result'First + Exponent_P) := '.';
						Result (Result'First + Exponent_P + 1 .. Result'Last) :=
							Ada_Image (Ada_Image'First + Exponent_P .. Ada_Image'Last);
						C.mpfr.mpfr_free_str (Image);
					end return;
				else
					declare
						Zero_Width : constant Natural := -Integer (Exponent);
					begin
						return Result : String (1 .. Length + Zero_Width + 2) do -- add "0."
							if Sign_Width > 0 then
								Result (Result'First) := '-';
							end if;
							Result (Result'First + Sign_Width) := '0';
							Result (Result'First + Sign_Width + 1) := '.';
							Result (
								Result'First + Sign_Width + 2 ..
								Result'First + Sign_Width + Zero_Width + 1) := (others => '0');
							Result (Result'First + Sign_Width + Zero_Width + 2 .. Result'Last) :=
								Ada_Image (Ada_Image'First + Sign_Width .. Ada_Image'Last);
							C.mpfr.mpfr_free_str (Image);
						end return;
					end;
				end if;
			end;
		end if;
	end Image;
	
	function Value (
		Image : String;
		Base : Number_Base := 10;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float
	is
		Z_Image : constant String := Image & Character'Val (0);
		C_Image : C.char_array (0 .. Z_Image'Length);
		for C_Image'Address use Z_Image'Address;
	begin
		return Result : MP_Float (Precision) do
			if C.mpfr.mpfr_set_str (
				Reference (Result),
				C_Image (C_Image'First)'Access,
				C.signed_int (Base),
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding))) < 0
			then
				raise Constraint_Error;
			end if;
		end return;
	end Value;
	
	function "=" (Left, Right : MP_Float) return Boolean is
	begin
		return C.mpfr.mpfr_cmp (
			Constant_Reference (Left),
			Constant_Reference (Right)) = 0;
	end "=";
	
	function "<" (Left, Right : MP_Float) return Boolean is
	begin
		return C.mpfr.mpfr_cmp (
			Constant_Reference (Left),
			Constant_Reference (Right)) < 0;
	end "<";
	
	function ">" (Left, Right : MP_Float) return Boolean is
	begin
		return C.mpfr.mpfr_cmp (
			Constant_Reference (Left),
			Constant_Reference (Right)) > 0;
	end ">";
	
	function "<=" (Left, Right : MP_Float) return Boolean is
	begin
		return C.mpfr.mpfr_cmp (
			Constant_Reference (Left),
			Constant_Reference (Right)) <= 0;
	end "<=";
	
	function ">=" (Left, Right : MP_Float) return Boolean is
	begin
		return C.mpfr.mpfr_cmp (
			Constant_Reference (Left),
			Constant_Reference (Right)) >= 0;
	end ">=";
	
	function Copy (
		Right : MP_Float;
		Precision : MPFR.Precision := Default_Precision;
		Rounding : MPFR.Rounding := Default_Rounding)
		return MP_Float
	is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
	begin
		return Result : MP_Float (Precision) do
			Dummy := C.mpfr.mpfr_set4 (
				Reference (Result),
				Constant_Reference (Right),
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)),
				+1);
		end return;
	end Copy;
	
	function Negative (
		Right : MP_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float
	is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
	begin
		return Result : MP_Float (Precision) do
			Dummy := C.mpfr.mpfr_neg (
				Reference (Result),
				Constant_Reference (Right),
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)));
		end return;
	end Negative;
	
	function Add (
		Left, Right : MP_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float
	is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
	begin
		return Result : MP_Float (Precision) do
			Dummy := C.mpfr.mpfr_add (
				Reference (Result),
				Constant_Reference (Left),
				Constant_Reference (Right),
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)));
		end return;
	end Add;
	
	function Add (
		Left : MP_Float;
		Right : Long_Long_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float
	is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
		Right2 : C.mpfr.mpfr_t := (others => (others => <>));
	begin
		return Result : MP_Float (Precision) do
			C.mpfr.mpfr_init2 (Right2 (0)'Access, C.mpfr.mpfr_prec_t (Precision));
			Dummy := C.mpfr.mpfr_set_ld (
				Right2 (0)'Access,
				C.long_double (Right),
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)));
			Dummy := C.mpfr.mpfr_add (
				Reference (Result),
				Constant_Reference (Left),
				Right2 (0)'Access,
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)));
			C.mpfr.mpfr_clear (Right2 (0)'Access);
		end return;
	end Add;
	
	function Add (
		Left : Long_Long_Float;
		Right : MP_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float is
	begin
		return Add (Right, Left, Precision, Rounding);
	end Add;
	
	function Subtract (
		Left, Right : MP_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float
	is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
	begin
		return Result : MP_Float (Precision) do
			Dummy := C.mpfr.mpfr_sub (
				Reference (Result),
				Constant_Reference (Left),
				Constant_Reference (Right),
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)));
		end return;
	end Subtract;
	
	function Subtract (
		Left : MP_Float;
		Right : Long_Long_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float
	is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
		Right2 : C.mpfr.mpfr_t := (others => (others => <>));
	begin
		return Result : MP_Float (Precision) do
			C.mpfr.mpfr_init2 (Right2 (0)'Access, C.mpfr.mpfr_prec_t (Precision));
			Dummy := C.mpfr.mpfr_set_ld (
				Right2 (0)'Access,
				C.long_double (Right),
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)));
			Dummy := C.mpfr.mpfr_sub (
				Reference (Result),
				Constant_Reference (Left),
				Right2 (0)'Access,
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)));
			C.mpfr.mpfr_clear (Right2 (0)'Access);
		end return;
	end Subtract;
	
	function Subtract (
		Left : Long_Long_Float;
		Right : MP_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float
	is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
		Left2 : C.mpfr.mpfr_t := (others => (others => <>));
	begin
		return Result : MP_Float (Precision) do
			C.mpfr.mpfr_init2 (Left2 (0)'Access, C.mpfr.mpfr_prec_t (Precision));
			Dummy := C.mpfr.mpfr_set_ld (
				Left2 (0)'Access,
				C.long_double (Left),
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)));
			Dummy := C.mpfr.mpfr_sub (
				Reference (Result),
				Left2 (0)'Access,
				Constant_Reference (Right),
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)));
			C.mpfr.mpfr_clear (Left2 (0)'Access);
		end return;
	end Subtract;
	
	function Multiply (
		Left, Right : MP_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float
	is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
	begin
		return Result : MP_Float (Precision) do
			Dummy := C.mpfr.mpfr_mul (
				Reference (Result),
				Constant_Reference (Left),
				Constant_Reference (Right),
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)));
		end return;
	end Multiply;
	
	function Multiply (
		Left : MP_Float;
		Right : Long_Long_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float
	is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
		Right2 : C.mpfr.mpfr_t := (others => (others => <>));
	begin
		return Result : MP_Float (Precision) do
			C.mpfr.mpfr_init2 (Right2 (0)'Access, C.mpfr.mpfr_prec_t (Precision));
			Dummy := C.mpfr.mpfr_set_ld (
				Right2 (0)'Access,
				C.long_double (Right),
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)));
			Dummy := C.mpfr.mpfr_mul (
				Reference (Result),
				Constant_Reference (Left),
				Right2 (0)'Access,
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)));
			C.mpfr.mpfr_clear (Right2 (0)'Access);
		end return;
	end Multiply;
	
	function Multiply (
		Left : Long_Long_Float;
		Right : MP_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float is
	begin
		return Multiply (Right, Left, Precision, Rounding);
	end Multiply;
	
	function Divide (
		Left, Right : MP_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float
	is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
	begin
		return Result : MP_Float (Precision) do
			Dummy := C.mpfr.mpfr_div (
				Reference (Result),
				Constant_Reference (Left),
				Constant_Reference (Right),
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)));
		end return;
	end Divide;
	
	function Divide (
		Left : MP_Float;
		Right : Long_Long_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float
	is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
		Right2 : C.mpfr.mpfr_t := (others => (others => <>));
	begin
		return Result : MP_Float (Precision) do
			C.mpfr.mpfr_init2 (Right2 (0)'Access, C.mpfr.mpfr_prec_t (Precision));
			Dummy := C.mpfr.mpfr_set_ld (
				Right2 (0)'Access,
				C.long_double (Right),
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)));
			Dummy := C.mpfr.mpfr_div (
				Reference (Result),
				Constant_Reference (Left),
				Right2 (0)'Access,
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)));
			C.mpfr.mpfr_clear (Right2 (0)'Access);
		end return;
	end Divide;
	
	function Divide (
		Left : Long_Long_Float;
		Right : MP_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float
	is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
		Left2 : C.mpfr.mpfr_t := (others => (others => <>));
	begin
		return Result : MP_Float (Precision) do
			C.mpfr.mpfr_init2 (Left2 (0)'Access, C.mpfr.mpfr_prec_t (Precision));
			Dummy := C.mpfr.mpfr_set_ld (
				Left2 (0)'Access,
				C.long_double (Left),
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)));
			Dummy := C.mpfr.mpfr_div (
				Reference (Result),
				Left2 (0)'Access,
				Constant_Reference (Right),
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)));
			C.mpfr.mpfr_clear (Left2 (0)'Access);
		end return;
	end Divide;
	
	function Power (
		Left : MP_Float;
		Right : Integer;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float
	is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
	begin
		return Result : MP_Float (Precision) do
			Dummy := C.mpfr.mpfr_pow_si (
				Reference (Result),
				Constant_Reference (Left),
				C.signed_long (Right),
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)));
		end return;
	end Power;
	
	function Sqrt (
		X : MP_Float;
		Precision : MPFR.Precision;
		Rounding : MPFR.Rounding)
		return MP_Float
	is
		Dummy : C.signed_int;
		pragma Unreferenced (Dummy);
	begin
		return Result : MP_Float (Precision) do
			Dummy := C.mpfr.mpfr_sqrt (
				Reference (Result),
				Constant_Reference (X),
				C.mpfr.mpfr_rnd_t'Enum_Val (MPFR.Rounding'Enum_Rep (Rounding)));
		end return;
	end Sqrt;
	
	function NaN (
		Precision : MPFR.Precision)
		return MP_Float is
	begin
		return Result : MP_Float (Precision) do
			C.mpfr.mpfr_set_nan (Reference (Result));
		end return;
	end NaN;
	
	function Infinity (
		Precision : MPFR.Precision)
		return MP_Float is
	begin
		return Result : MP_Float (Precision) do
			C.mpfr.mpfr_set_inf (Reference (Result), 0);
		end return;
	end Infinity;
	
	package body Controlled is
		
		function Create (Precision : MPFR.Precision) return MP_Float is
		begin
			return Result : Controlled.MP_Float :=
				(Ada.Finalization.Controlled with Raw => <>)
			do
				C.mpfr.mpfr_init2 (Result.Raw (0)'Access, C.mpfr.mpfr_prec_t (Precision));
			end return;
		end Create;
		
		function Reference (Item : in out MP_Float)
			return not null access C.mpfr.mpfr_struct is
		begin
			return Item.Raw (0)'Unchecked_Access;
		end Reference;
		
		function Constant_Reference (Item : MP_Float)
			return not null access constant C.mpfr.mpfr_struct is
		begin
			return Item.Raw (0)'Unchecked_Access;
		end Constant_Reference;
		
		overriding procedure Initialize (Object : in out MP_Float) is
		begin
			raise Program_Error;
		end Initialize;
		
		overriding procedure Adjust (Object : in out MP_Float) is
			Source : constant C.mpfr.mpfr_t := Object.Raw; -- move
			Dummy : C.signed_int;
			pragma Unreferenced (Dummy);
		begin
			C.mpfr.mpfr_init2 (
				Object.Raw (0)'Access,
				C.mpfr.mpfr_get_prec (Source (0)'Access));
			Dummy := C.mpfr.mpfr_set4 (
				Object.Raw (0)'Access,
				Source (0)'Access,
				C.mpfr.MPFR_RNDN,
				C.mpfr.mpfr_sgn (Source (0)'Access));
		end Adjust;
		
		overriding procedure Finalize (Object : in out MP_Float) is
		begin
			C.mpfr.mpfr_clear (Object.Raw (0)'Access);
		end Finalize;
		
	end Controlled;
	
	function Reference (Item : in out MP_Float)
		return not null access C.mpfr.mpfr_struct is
	begin
		return Controlled.Reference (Item.Data);
	end Reference;
	
	function Constant_Reference (Item : MP_Float)
		return not null access constant C.mpfr.mpfr_struct is
	begin
		return Controlled.Constant_Reference (Item.Data);
	end Constant_Reference;
	
end MPFR.Root_FR;
