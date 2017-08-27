pragma Ada_2012;
with System;
with GMP.Z.Inside;
with C.string;
package body GMP.Q is
	use type C.signed_int;
	use type C.size_t;
	
	procedure memcpy (dst, src : System.Address; n : C.size_t)
		with Import,
			Convention => Intrinsic, External_Name => "__builtin_memcpy";
	
	-- implementation
	
	function Num (X : MP_Rational) return Z.MP_Integer is
	begin
		return Result : Z.MP_Integer do
			C.gmp.mpz_set (
				Z.Inside.Reference (Result),
				Controlled.Constant_Reference (X).mp_num'Access);
		end return;
	end Num;
	
	function Den (X : MP_Rational) return Z.MP_Integer is
	begin
		return Result : Z.MP_Integer do
			C.gmp.mpz_set (
				Z.Inside.Reference (Result),
				Controlled.Constant_Reference (X).mp_den'Access);
		end return;
	end Den;
	
	function Image (Value : MP_Rational; Base : Number_Base := 10)
		return String
	is
		Raw_Value : constant not null access constant C.gmp.mpq_struct :=
			Controlled.Constant_Reference (Value);
		Buffer_Size : constant C.size_t :=
			C.gmp.mpz_sizeinbase (Raw_Value.mp_num'Access, C.signed_int (Base))
			+ C.gmp.mpz_sizeinbase (Raw_Value.mp_den'Access, C.signed_int (Base))
			+ 2;
		Buffer : aliased C.char_array (0 .. Buffer_Size);
		Dummy : C.char_ptr;
	begin
		Dummy := C.gmp.mpq_get_str (
			Buffer (Buffer'First)'Access,
			C.signed_int (Base),
			Raw_Value);
		declare
			Length : constant Natural :=
				Natural (C.string.strlen (Buffer (Buffer'First)'Access));
			Result : String (1 .. Length);
			for Result'Address use Buffer'Address;
		begin
			return Result;
		end;
	end Image;
	
	function Value (Image : String; Base : Number_Base := 10)
		return MP_Rational
	is
		Image_Length : constant C.size_t := Image'Length;
		C_Image : C.char_array (0 .. Image_Length); -- NUL
	begin
		memcpy (C_Image'Address, Image'Address, Image_Length);
		C_Image (Image_Length) := C.char'Val (0);
		return Result : MP_Rational do
			declare
				Raw_Result : constant not null access C.gmp.mpq_struct :=
					Controlled.Reference (Result);
			begin
				if C.gmp.mpq_set_str (
					Raw_Result,
					C_Image (C_Image'First)'Access,
					C.signed_int (Base)) < 0
				then
					raise Constraint_Error;
				end if;
				C.gmp.mpq_canonicalize (Raw_Result);
			end;
		end return;
	end Value;
	
	function "=" (Left, Right : MP_Rational) return Boolean is
	begin
		return C.gmp.mpq_cmp (
			Controlled.Constant_Reference (Left),
			Controlled.Constant_Reference (Right)) = 0;
	end "=";
	
	function "<" (Left, Right : MP_Rational) return Boolean is
	begin
		return C.gmp.mpq_cmp (
			Controlled.Constant_Reference (Left),
			Controlled.Constant_Reference (Right)) < 0;
	end "<";
	
	function ">" (Left, Right : MP_Rational) return Boolean is
	begin
		return C.gmp.mpq_cmp (
			Controlled.Constant_Reference (Left),
			Controlled.Constant_Reference (Right)) > 0;
	end ">";
	
	function "<=" (Left, Right : MP_Rational) return Boolean is
	begin
		return C.gmp.mpq_cmp (
			Controlled.Constant_Reference (Left),
			Controlled.Constant_Reference (Right)) <= 0;
	end "<=";
	
	function ">=" (Left, Right : MP_Rational) return Boolean is
	begin
		return C.gmp.mpq_cmp (
			Controlled.Constant_Reference (Left),
			Controlled.Constant_Reference (Right)) >= 0;
	end ">=";
	
	function "+" (Right : MP_Rational) return MP_Rational is
	begin
		return Right;
	end "+";
	
	function "-" (Right : MP_Rational) return MP_Rational is
	begin
		return Result : MP_Rational do
			C.gmp.mpq_neg (
				Controlled.Reference (Result),
				Controlled.Constant_Reference (Right));
		end return;
	end "-";
	
	function "+" (Left, Right : MP_Rational) return MP_Rational is
	begin
		return Result : MP_Rational do
			C.gmp.mpq_add (
				Controlled.Reference (Result),
				Controlled.Constant_Reference (Left),
				Controlled.Constant_Reference (Right));
		end return;
	end "+";
	
	function "-" (Left, Right : MP_Rational) return MP_Rational is
	begin
		return Result : MP_Rational do
			C.gmp.mpq_sub (
				Controlled.Reference (Result),
				Controlled.Constant_Reference (Left),
				Controlled.Constant_Reference (Right));
		end return;
	end "-";
	
	function "*" (Left, Right : MP_Rational) return MP_Rational is
	begin
		return Result : MP_Rational do
			C.gmp.mpq_mul (
				Controlled.Reference (Result),
				Controlled.Constant_Reference (Left),
				Controlled.Constant_Reference (Right));
		end return;
	end "*";
	
	function "/" (Left, Right : MP_Rational) return MP_Rational is
	begin
		return Result : MP_Rational do
			C.gmp.mpq_div (
				Controlled.Reference (Result),
				Controlled.Constant_Reference (Left),
				Controlled.Constant_Reference (Right));
		end return;
	end "/";
	
	function "/" (Left, Right : Long_Long_Integer) return MP_Rational is
		Numerator : Long_Long_Integer renames Left;
		Denominator : Long_Long_Integer renames Right;
	begin
		return Result : MP_Rational do
			declare
				Raw_Result : constant not null access C.gmp.mpq_struct :=
					Controlled.Reference (Result);
			begin
				mpz_set_Long_Long_Integer (Raw_Result.mp_num'Access, Numerator);
				mpz_set_Long_Long_Integer (Raw_Result.mp_den'Access, Denominator);
				C.gmp.mpq_canonicalize (Raw_Result);
			end;
		end return;
	end "/";
	
	function "/" (Left, Right : Z.MP_Integer) return MP_Rational is
		Numerator : Z.MP_Integer renames Left;
		Denominator : Z.MP_Integer renames Right;
	begin
		return Result : MP_Rational do
			declare
				Raw_Result : constant not null access C.gmp.mpq_struct :=
					Controlled.Reference (Result);
			begin
				C.gmp.mpz_set (
					Raw_Result.mp_num'Access,
					Z.Inside.Constant_Reference (Numerator));
				C.gmp.mpz_set (
					Raw_Result.mp_den'Access,
					Z.Inside.Constant_Reference (Denominator));
				C.gmp.mpq_canonicalize (Raw_Result);
			end;
		end return;
	end "/";
	
	function "**" (Left : MP_Rational; Right : Integer) return MP_Rational is
	begin
		return Result : MP_Rational do
			declare
				Raw_Result : constant not null access C.gmp.mpq_struct :=
					Controlled.Reference (Result);
				Raw_Left : constant not null access constant C.gmp.mpq_struct :=
					Controlled.Constant_Reference (Left);
			begin
				if Right >= 0 then
					declare
						E : constant C.unsigned_long := C.unsigned_long'Mod (Right);
					begin
						C.gmp.mpz_pow_ui (
							Raw_Result.mp_num'Access,
							Raw_Left.mp_num'Access,
							E);
						C.gmp.mpz_pow_ui (
							Raw_Result.mp_den'Access,
							Raw_Left.mp_den'Access,
							E);
					end;
				else
					declare
						E : constant C.unsigned_long := C.unsigned_long'Mod (-Right);
					begin
						C.gmp.mpz_pow_ui (
							Raw_Result.mp_num'Access,
							Raw_Left.mp_den'Access,
							E);
						C.gmp.mpz_pow_ui (
							Raw_Result.mp_den'Access,
							Raw_Left.mp_num'Access,
							E);
					end;
				end if;
				C.gmp.mpq_canonicalize (Raw_Result);
			end;
		end return;
	end "**";
	
	package body Controlled is
		
		function View_Reference (Item : in out MP_Rational)
			return not null access C.gmp.mpq_struct;
		pragma Inline (View_Reference);
		
		function View_Reference (Item : in out MP_Rational)
			return not null access C.gmp.mpq_struct is
		begin
			return Item.Raw (0)'Unchecked_Access;
		end View_Reference;
		
		-- implementation
		
		function Reference (Item : in out Q.MP_Rational)
			return not null access C.gmp.mpq_struct is
		begin
			return View_Reference (MP_Rational (Item)); -- view conversion
		end Reference;
		
		function Constant_Reference (Item : Q.MP_Rational)
			return not null access constant C.gmp.mpq_struct is
		begin
			return MP_Rational (Item).Raw (0)'Unchecked_Access;
		end Constant_Reference;
		
		overriding procedure Initialize (Object : in out MP_Rational) is
		begin
			C.gmp.mpq_init (Object.Raw (0)'Access);
		end Initialize;
		
		overriding procedure Adjust (Object : in out MP_Rational) is
			Source : constant C.gmp.mpq_t := Object.Raw;
		begin
			C.gmp.mpq_init (Object.Raw (0)'Access);
			C.gmp.mpq_set (Object.Raw (0)'Access, Source (0)'Access);
		end Adjust;
		
		overriding procedure Finalize (Object : in out MP_Rational) is
		begin
			C.gmp.mpq_clear (Object.Raw (0)'Access);
		end Finalize;
		
	end Controlled;
	
	procedure Read (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out MP_Rational)
	is
		Raw_Item : constant not null access C.gmp.mpq_struct :=
			Controlled.Reference (Item);
	begin
		C.gmp.mpq_clear (Raw_Item);
		Z.Inside.Read (Stream, Raw_Item.mp_num'Access);
		Z.Inside.Read (Stream, Raw_Item.mp_den'Access);
	end Read;
	
	procedure Write (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : in MP_Rational)
	is
		Raw_Item : constant not null access constant C.gmp.mpq_struct :=
			Controlled.Constant_Reference (Item);
	begin
		Z.Inside.Write (Stream, Raw_Item.mp_num'Access);
		Z.Inside.Write (Stream, Raw_Item.mp_den'Access);
	end Write;
	
end GMP.Q;
