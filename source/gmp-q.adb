pragma Ada_2012;
with GMP.Z.Inside;
with C.string;
package body GMP.Q is
	use type C.signed_int;
	use type C.size_t;
	
	function Num (X : MP_Rational) return Z.MP_Integer is
	begin
		return Result : Z.MP_Integer do
			C.gmp.mpz_set (
				Z.Inside.Reference (Result),
				Constant_Reference (X).mp_num'Access);
		end return;
	end Num;
	
	function Den (X : MP_Rational) return Z.MP_Integer is
	begin
		return Result : Z.MP_Integer do
			C.gmp.mpz_set (
				Z.Inside.Reference (Result),
				Constant_Reference (X).mp_den'Access);
		end return;
	end Den;
	
	function Image (Value : MP_Rational; Base : Number_Base := 10)
		return String
	is
		Buffer_Size : constant C.size_t :=
			C.gmp.mpz_sizeinbase (
				Constant_Reference (Value).mp_num'Access,
				C.signed_int (Base)) + 
			C.gmp.mpz_sizeinbase (
				Constant_Reference (Value).mp_den'Access,
				C.signed_int (Base)) + 2;
		Buffer : C.char_array (0 .. Buffer_Size);
		Dummy : C.char_ptr := C.gmp.mpq_get_str (
			Buffer (Buffer'First)'Access,
			C.signed_int (Base),
			Constant_Reference (Value));
		pragma Unreferenced (Dummy);
		Length : constant Natural :=
			Natural (C.string.strlen (Buffer (Buffer'First)'Access));
		Result : String (1 .. Length);
		for Result'Address use Buffer'Address;
	begin
		return Result;
	end Image;
	
	function Value (Image : String; Base : Number_Base := 10)
		return MP_Rational
	is
		Z_Image : constant String := Image & Character'Val (0);
		C_Image : C.char_array (0 .. Z_Image'Length);
		for C_Image'Address use Z_Image'Address;
	begin
		return Result : MP_Rational do
			if C.gmp.mpq_set_str (
				Reference (Result),
				C_Image (C_Image'First)'Access,
				C.signed_int (Base)) < 0
			then
				raise Constraint_Error;
			end if;
			C.gmp.mpq_canonicalize (Reference (Result));
		end return;
	end Value;
	
	function "=" (Left, Right : MP_Rational) return Boolean is
	begin
		return C.gmp.mpq_cmp (
			Constant_Reference (Left),
			Constant_Reference (Right)) = 0;
	end "=";
	
	function "<" (Left, Right : MP_Rational) return Boolean is
	begin
		return C.gmp.mpq_cmp (
			Constant_Reference (Left),
			Constant_Reference (Right)) < 0;
	end "<";
	
	function ">" (Left, Right : MP_Rational) return Boolean is
	begin
		return C.gmp.mpq_cmp (
			Constant_Reference (Left),
			Constant_Reference (Right)) > 0;
	end ">";
	
	function "<=" (Left, Right : MP_Rational) return Boolean is
	begin
		return C.gmp.mpq_cmp (
			Constant_Reference (Left),
			Constant_Reference (Right)) <= 0;
	end "<=";
	
	function ">=" (Left, Right : MP_Rational) return Boolean is
	begin
		return C.gmp.mpq_cmp (
			Constant_Reference (Left),
			Constant_Reference (Right)) >= 0;
	end ">=";
	
	function "+" (Right : MP_Rational) return MP_Rational is
	begin
		return Right;
	end "+";
	
	function "-" (Right : MP_Rational) return MP_Rational is
	begin
		return Result : MP_Rational do
			C.gmp.mpq_neg (
				Reference (Result),
				Constant_Reference (Right));
		end return;
	end "-";
	
	function "+" (Left, Right : MP_Rational) return MP_Rational is
	begin
		return Result : MP_Rational do
			C.gmp.mpq_add (
				Reference (Result),
				Constant_Reference (Left),
				Constant_Reference (Right));
		end return;
	end "+";
	
	function "-" (Left, Right : MP_Rational) return MP_Rational is
	begin
		return Result : MP_Rational do
			C.gmp.mpq_sub (
				Reference (Result),
				Constant_Reference (Left),
				Constant_Reference (Right));
		end return;
	end "-";
	
	function "*" (Left, Right : MP_Rational) return MP_Rational is
	begin
		return Result : MP_Rational do
			C.gmp.mpq_mul (
				Reference (Result),
				Constant_Reference (Left),
				Constant_Reference (Right));
		end return;
	end "*";
	
	function "/" (Left, Right : MP_Rational) return MP_Rational is
	begin
		return Result : MP_Rational do
			C.gmp.mpq_div (
				Reference (Result),
				Constant_Reference (Left),
				Constant_Reference (Right));
		end return;
	end "/";
	
	function "/" (Left, Right : Long_Long_Integer) return MP_Rational is
		Numerator : Long_Long_Integer renames Left;
		Denominator : Long_Long_Integer renames Right;
	begin
		return Result : MP_Rational do
			mpz_set_Long_Long_Integer (
				Reference (Result).mp_num'Access,
				Numerator);
			mpz_set_Long_Long_Integer (
				Reference (Result).mp_den'Access,
				Denominator);
			C.gmp.mpq_canonicalize (Reference (Result));
		end return;
	end "/";
	
	function "/" (Left, Right : Z.MP_Integer) return MP_Rational is
		Numerator : Z.MP_Integer renames Left;
		Denominator : Z.MP_Integer renames Right;
	begin
		return Result : MP_Rational do
			C.gmp.mpz_set (
				Reference (Result).mp_num'Access,
				Z.Inside.Constant_Reference (Numerator));
			C.gmp.mpz_set (
				Reference (Result).mp_den'Access,
				Z.Inside.Constant_Reference (Denominator));
			C.gmp.mpq_canonicalize (Reference (Result));
		end return;
	end "/";
	
	function "**" (Left : MP_Rational; Right : Integer) return MP_Rational is
	begin
		return Result : MP_Rational do
			if Right >= 0 then
				declare
					E : constant C.unsigned_long := C.unsigned_long'Mod (Right);
				begin
					C.gmp.mpz_pow_ui (
						Reference (Result).mp_num'Access,
						Constant_Reference (Left).mp_num'Access,
						E);
					C.gmp.mpz_pow_ui (
						Reference (Result).mp_den'Access,
						Constant_Reference (Left).mp_den'Access,
						E);
				end;
			else
				declare
					E : constant C.unsigned_long := C.unsigned_long'Mod (-Right);
				begin
					C.gmp.mpz_pow_ui (
						Reference (Result).mp_num'Access,
						Constant_Reference (Left).mp_den'Access,
						E);
					C.gmp.mpz_pow_ui (
						Reference (Result).mp_den'Access,
						Constant_Reference (Left).mp_num'Access,
						E);
				end;
			end if;
			C.gmp.mpq_canonicalize (Reference (Result));
		end return;
	end "**";
	
	package body Controlled is
		
		function Reference (Item : in out MP_Rational)
			return not null access C.gmp.mpq_struct is
		begin
			return Item.Raw (0)'Unchecked_Access;
		end Reference;
		
		function Constant_Reference (Item : MP_Rational)
			return not null access constant C.gmp.mpq_struct is
		begin
			return Item.Raw (0)'Unchecked_Access;
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
		Item : out MP_Rational) is
	begin
		C.gmp.mpq_clear (Reference (Item));
		Z.Inside.Read (Stream, Reference (Item).mp_num'Access);
		Z.Inside.Read (Stream, Reference (Item).mp_den'Access);
	end Read;
	
	procedure Write (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : in MP_Rational) is
	begin
		Z.Inside.Write (Stream, Constant_Reference (Item).mp_num'Access);
		Z.Inside.Write (Stream, Constant_Reference (Item).mp_den'Access);
	end Write;
	
end GMP.Q;
