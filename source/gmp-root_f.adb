pragma Ada_2012;
with Ada.Unchecked_Conversion;
with System;
with C.stdlib;
with C.string;
package body GMP.Root_F is
	use type C.char;
	use type C.signed_int;
	use type C.size_t;
	use type C.gmp.mp_bitcnt_t;
	
	procedure memcpy (dst, src : System.Address; n : C.size_t)
		with Import,
			Convention => Intrinsic, External_Name => "__builtin_memcpy";
	
	-- implementation
	
	function To_MP_Float (
		X : Long_Float;
		Precision : GMP.Precision)
		return MP_Float is
	begin
		return Result : MP_Float (Precision) do
			C.gmp.mpf_set_d (
				Controlled.Reference (Result),
				C.double (X));
		end return;
	end To_MP_Float;
	
	function To_Long_Float (X : MP_Float) return Long_Float is
	begin
		return Long_Float (C.gmp.mpf_get_d (Controlled.Constant_Reference (X)));
	end To_Long_Float;
	
	function Image (
		Value : MP_Float;
		Base : Number_Base := 10)
		return String
	is
		function Cast is new Ada.Unchecked_Conversion (C.char_ptr, C.void_ptr);
		Exponent : aliased C.gmp.mp_exp_t;
		Image : constant C.char_ptr := C.gmp.mpf_get_str (
			null,
			Exponent'Access,
			C.signed_int (Base),
			0,
			Controlled.Constant_Reference (Value));
		Length : constant Natural := Integer (C.string.strlen (Image));
		Ada_Image : String (1 .. Length);
		for Ada_Image'Address use Image.all'Address;
		Sign_Width : constant Natural := Boolean'Pos (
			Length > 0 and then Ada_Image (1) = '-');
		Exponent_P : constant Natural := Integer (Exponent) + Sign_Width;
	begin
		if Length = 0 then
			C.stdlib.free (Cast (Image));
			return "0";
		elsif Exponent_P >= Length then
			return Result : String (1 .. Exponent_P) do
				Result (1 .. Length) := Ada_Image;
				Result (Length + 1 .. Result'Last) := (others => '0');
				C.stdlib.free (Cast (Image));
			end return;
		elsif Exponent_P > 0 then
			return Result : String (1 .. Length + 1) do -- add '.'
				Result (Result'First .. Exponent_P) :=
					Ada_Image (Ada_Image'First .. Exponent_P);
				Result (Result'First + Exponent_P) := '.';
				Result (Result'First + Exponent_P + 1 .. Result'Last) :=
					Ada_Image (Ada_Image'First + Exponent_P .. Ada_Image'Last);
				C.stdlib.free (Cast (Image));
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
					C.stdlib.free (Cast (Image));
				end return;
			end;
		end if;
	end Image;
	
	function Value (
		Image : String;
		Base : Number_Base := 10;
		Precision : GMP.Precision)
		return MP_Float
	is
		Image_Length : constant C.size_t := Image'Length;
		C_Image : C.char_array (0 .. Image_Length); -- NUL
		First : C.size_t := C_Image'First;
	begin
		memcpy (C_Image'Address, Image'Address, Image_Length);
		C_Image (Image_Length) := C.char'Val (0);
		if C_Image (First) = '+' then
			First := First + 1;
		end if;
		return Result : MP_Float (Precision) do
			if C.gmp.mpf_set_str (
				Controlled.Reference (Result),
				C_Image (First)'Access,
				C.signed_int (Base)) < 0
			then
				raise Constraint_Error;
			end if;
		end return;
	end Value;
	
	function "=" (Left, Right : MP_Float) return Boolean is
	begin
		return C.gmp.mpf_cmp (
			Controlled.Constant_Reference (Left),
			Controlled.Constant_Reference (Right)) = 0;
	end "=";
	
	function "<" (Left, Right : MP_Float) return Boolean is
	begin
		return C.gmp.mpf_cmp (
			Controlled.Constant_Reference (Left),
			Controlled.Constant_Reference (Right)) < 0;
	end "<";
	
	function ">" (Left, Right : MP_Float) return Boolean is
	begin
		return C.gmp.mpf_cmp (
			Controlled.Constant_Reference (Left),
			Controlled.Constant_Reference (Right)) > 0;
	end ">";
	
	function "<=" (Left, Right : MP_Float) return Boolean is
	begin
		return C.gmp.mpf_cmp (
			Controlled.Constant_Reference (Left),
			Controlled.Constant_Reference (Right)) <= 0;
	end "<=";
	
	function ">=" (Left, Right : MP_Float) return Boolean is
	begin
		return C.gmp.mpf_cmp (
			Controlled.Constant_Reference (Left),
			Controlled.Constant_Reference (Right)) >= 0;
	end ">=";
	
	function Copy (
		Right : MP_Float;
		Precision : GMP.Precision := Default_Precision)
		return MP_Float is
	begin
		return Result : MP_Float (Precision) do
			C.gmp.mpf_set (
				Controlled.Reference (Result),
				Controlled.Constant_Reference (Right));
		end return;
	end Copy;
	
	function Negative (
		Right : MP_Float;
		Precision : GMP.Precision)
		return MP_Float is
	begin
		return Result : MP_Float (Precision) do
			C.gmp.mpf_neg (
				Controlled.Reference (Result),
				Controlled.Constant_Reference (Right));
		end return;
	end Negative;
	
	function Add (
		Left, Right : MP_Float;
		Precision : GMP.Precision)
		return MP_Float is
	begin
		return Result : MP_Float (Precision) do
			C.gmp.mpf_add (
				Controlled.Reference (Result),
				Controlled.Constant_Reference (Left),
				Controlled.Constant_Reference (Right));
		end return;
	end Add;
	
	function Subtract (
		Left, Right : MP_Float;
		Precision : GMP.Precision)
		return MP_Float is
	begin
		return Result : MP_Float (Precision) do
			C.gmp.mpf_sub (
				Controlled.Reference (Result),
				Controlled.Constant_Reference (Left),
				Controlled.Constant_Reference (Right));
		end return;
	end Subtract;
	
	function Multiply (
		Left, Right : MP_Float;
		Precision : GMP.Precision)
		return MP_Float is
	begin
		return Result : MP_Float (Precision) do
			C.gmp.mpf_mul (
				Controlled.Reference (Result),
				Controlled.Constant_Reference (Left),
				Controlled.Constant_Reference (Right));
		end return;
	end Multiply;
	
	function Divide (
		Left, Right : MP_Float;
		Precision : GMP.Precision)
		return MP_Float is
	begin
		return Result : MP_Float (Precision) do
			C.gmp.mpf_div (
				Controlled.Reference (Result),
				Controlled.Constant_Reference (Left),
				Controlled.Constant_Reference (Right));
		end return;
	end Divide;
	
	function Power (
		Left : MP_Float;
		Right : Integer;
		Precision : GMP.Precision)
		return MP_Float is
	begin
		return Result : MP_Float (Precision) do
			declare
				Raw_Result : constant not null access C.gmp.mpf_struct :=
					Controlled.Reference (Result);
				Raw_Left : constant not null access constant C.gmp.mpf_struct :=
					Controlled.Constant_Reference (Left);
			begin
				if Right >= 0 then
					C.gmp.mpf_pow_ui (
						Raw_Result,
						Raw_Left,
						C.unsigned_long'Mod (Right));
				else
					declare
						Den : aliased C.gmp.mpf_t := (others => (others => <>));
					begin
						C.gmp.mpf_init2 (
							Den (0)'Access,
							C.gmp.mp_bitcnt_t (Precision));
						C.gmp.mpf_pow_ui (
							Den (0)'Access,
							Raw_Left,
							C.unsigned_long'Mod (Right));
						C.gmp.mpf_ui_div (Raw_Result, 1, Den (0)'Access);
						C.gmp.mpf_clear (Den (0)'Access);
					end;
				end if;
			end;
		end return;
	end Power;
	
	package body Controlled is
		
		function Create (Precision : GMP.Precision) return MP_Float is
		begin
			return Result : MP_Float := (Ada.Finalization.Controlled with Raw => <>) do
				C.gmp.mpf_init2 (Result.Raw (0)'Access, C.gmp.mp_bitcnt_t (Precision));
			end return;
		end Create;
		
		function Reference (Item : in out Root_F.MP_Float)
			return not null access C.gmp.mpf_struct is
		begin
			return Item.Data.Raw (0)'Unchecked_Access;
		end Reference;
		
		function Constant_Reference (Item : Root_F.MP_Float)
			return not null access constant C.gmp.mpf_struct is
		begin
			return Item.Data.Raw (0)'Unchecked_Access;
		end Constant_Reference;
		
		overriding procedure Initialize (Object : in out MP_Float) is
		begin
			raise Program_Error;
		end Initialize;
		
		overriding procedure Adjust (Object : in out MP_Float) is
			Source : constant C.gmp.mpf_t := Object.Raw; -- move
		begin
			C.gmp.mpf_init_set (
				Object.Raw (0)'Access,
				Source (0)'Access);
			pragma Assert (
				C.gmp.gmpf_get_prec (Object.Raw (0)'Access) =
				C.gmp.gmpf_get_prec (Source (0)'Access));
		end Adjust;
		
		overriding procedure Finalize (Object : in out MP_Float) is
		begin
			C.gmp.mpf_clear (Object.Raw (0)'Access);
		end Finalize;
		
	end Controlled;
	
end GMP.Root_F;
