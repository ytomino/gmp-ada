with GMP.Z.Inside;
with C.string;
package body GMP.Q is
	use type C.signed_int;
	use type C.size_t;
	
	function Num (X : MP_Rational) return Z.MP_Integer is
	begin
		return Result : aliased Z.MP_Integer do
			C.gmp.mpz_set (
				Z.Inside.Reference (Result'Access),
				X.Data.Raw (0).mp_num'Access);
		end return;
	end Num;
	
	function Den (X : MP_Rational) return Z.MP_Integer is
	begin
		return Result : aliased Z.MP_Integer do
			C.gmp.mpz_set (
				Z.Inside.Reference (Result'Access),
				X.Data.Raw (0).mp_den'Access);
		end return;
	end Den;
	
	function Image (Value : MP_Rational; Base : Number_Base := 10)
		return String
	is
		Buffer_Size : constant C.size_t :=
			C.gmp.mpz_sizeinbase (
				Value.Data.Raw (0).mp_num'Access,
				C.signed_int (Base)) + 
			C.gmp.mpz_sizeinbase (
				Value.Data.Raw (0).mp_den'Access,
				C.signed_int (Base)) + 2;
		Buffer : C.char_array (0 .. Buffer_Size);
		Dummy : C.char_ptr := C.gmp.mpq_get_str (
			Buffer (Buffer'First)'Access,
			C.signed_int (Base),
			Value.Data.Raw (0)'Access);
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
				Result.Data.Raw (0)'Access,
				C_Image (C_Image'First)'Access,
				C.signed_int (Base)) < 0
			then
				raise Constraint_Error;
			end if;
			C.gmp.mpq_canonicalize (Result.Data.Raw (0)'Access);
		end return;
	end Value;
	
	function "=" (Left, Right : MP_Rational) return Boolean is
	begin
		return C.gmp.mpq_cmp (
			Left.Data.Raw (0)'Access,
			Right.Data.Raw (0)'Access) = 0;
	end "=";
	
	function "<" (Left, Right : MP_Rational) return Boolean is
	begin
		return C.gmp.mpq_cmp (
			Left.Data.Raw (0)'Access,
			Right.Data.Raw (0)'Access) < 0;
	end "<";
	
	function ">" (Left, Right : MP_Rational) return Boolean is
	begin
		return C.gmp.mpq_cmp (
			Left.Data.Raw (0)'Access,
			Right.Data.Raw (0)'Access) > 0;
	end ">";
	
	function "<=" (Left, Right : MP_Rational) return Boolean is
	begin
		return C.gmp.mpq_cmp (
			Left.Data.Raw (0)'Access,
			Right.Data.Raw (0)'Access) <= 0;
	end "<=";
	
	function ">=" (Left, Right : MP_Rational) return Boolean is
	begin
		return C.gmp.mpq_cmp (
			Left.Data.Raw (0)'Access,
			Right.Data.Raw (0)'Access) >= 0;
	end ">=";
	
	function "+" (Right : MP_Rational) return MP_Rational is
	begin
		return Right;
	end "+";
	
	function "-" (Right : MP_Rational) return MP_Rational is
	begin
		return Result : MP_Rational do
			C.gmp.mpq_neg (
				Result.Data.Raw (0)'Access,
				Right.Data.Raw (0)'Access);
		end return;
	end "-";
	
	function "+" (Left, Right : MP_Rational) return MP_Rational is
	begin
		return Result : MP_Rational do
			C.gmp.mpq_add (
				Result.Data.Raw (0)'Access,
				Left.Data.Raw (0)'Access,
				Right.Data.Raw (0)'Access);
		end return;
	end "+";
	
	function "-" (Left, Right : MP_Rational) return MP_Rational is
	begin
		return Result : MP_Rational do
			C.gmp.mpq_sub (
				Result.Data.Raw (0)'Access,
				Left.Data.Raw (0)'Access,
				Right.Data.Raw (0)'Access);
		end return;
	end "-";
	
	function "*" (Left, Right : MP_Rational) return MP_Rational is
	begin
		return Result : MP_Rational do
			C.gmp.mpq_mul (
				Result.Data.Raw (0)'Access,
				Left.Data.Raw (0)'Access,
				Right.Data.Raw (0)'Access);
		end return;
	end "*";
	
	function "/" (Left, Right : MP_Rational) return MP_Rational is
	begin
		return Result : MP_Rational do
			C.gmp.mpq_div (
				Result.Data.Raw (0)'Access,
				Left.Data.Raw (0)'Access,
				Right.Data.Raw (0)'Access);
		end return;
	end "/";
	
	function "/" (Left, Right : Long_Long_Integer) return MP_Rational is
		Numerator : Long_Long_Integer renames Left;
		Denominator : Long_Long_Integer renames Right;
	begin
		return Result : MP_Rational :=
			(Data => (Ada.Finalization.Controlled with Raw => <>))
		do
			mpz_init_set_Long_Long_Integer (
				Result.Data.Raw (0).mp_num'Access,
				Numerator);
			mpz_init_set_Long_Long_Integer (
				Result.Data.Raw (0).mp_den'Access,
				Denominator);
			C.gmp.mpq_canonicalize (Result.Data.Raw (0)'Access);
		end return;
	end "/";
	
	function "/" (Left, Right : Z.MP_Integer) return MP_Rational is
		Numerator : Z.MP_Integer renames Left;
		Denominator : Z.MP_Integer renames Right;
	begin
		return Result : MP_Rational :=
			(Data => (Ada.Finalization.Controlled with Raw => <>))
		do
			C.gmp.mpz_init_set (
				Result.Data.Raw (0).mp_num'Access,
				Z.Inside.Constant_Reference (Numerator'Unrestricted_Access));
			C.gmp.mpz_init_set (
				Result.Data.Raw (0).mp_den'Access,
				Z.Inside.Constant_Reference (Denominator'Unrestricted_Access));
			C.gmp.mpq_canonicalize (Result.Data.Raw (0)'Access);
		end return;
	end "/";
	
	overriding procedure Initialize (Object : in out Controlled) is
	begin
		C.gmp.mpq_init (Object.Raw (0)'Access);
	end Initialize;
	
	overriding procedure Adjust (Object : in out Controlled) is
		Source : constant C.gmp.mpq_t := Object.Raw;
	begin
		C.gmp.mpq_init (Object.Raw (0)'Access);
		C.gmp.mpq_set (Object.Raw (0)'Access, Source (0)'Access);
	end Adjust;
	
	overriding procedure Finalize (Object : in out Controlled) is
	begin
		C.gmp.mpq_clear (Object.Raw (0)'Access);
	end Finalize;
	
	procedure Read (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out MP_Rational) is
	begin
		C.gmp.mpq_clear (Item.Data.Raw (0)'Access);
		Z.Inside.Read (Stream, Item.Data.Raw (0).mp_num'Access);
		Z.Inside.Read (Stream, Item.Data.Raw (0).mp_den'Access);
	end Read;
	
	procedure Write (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : in MP_Rational) is
	begin
		Z.Inside.Write (Stream, Item.Data.Raw (0).mp_num'Access);
		Z.Inside.Write (Stream, Item.Data.Raw (0).mp_den'Access);
	end Write;
	
end GMP.Q;
