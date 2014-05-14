with System;
with C.string;
package body GMP.Z is
	use type Ada.Streams.Stream_Element;
	use type Ada.Streams.Stream_Element_Offset;
	use type C.signed_int;
	use type C.signed_long;
	use type C.size_t;
	use type C.gmp.mp_bitcnt_t;
	
	function To_MP_Integer (X : Long_Long_Integer) return MP_Integer is
	begin
		return Result : MP_Integer :=
			(Data => (Ada.Finalization.Controlled with Raw => <>))
		do
			mpz_init_set_Long_Long_Integer (Result.Data.Raw (0)'Access, X);
		end return;
	end To_MP_Integer;
	
	function Image (Value : MP_Integer; Base : Number_Base := 10) return String is
		Buffer_Size : constant C.size_t :=
			C.gmp.mpz_sizeinbase (Value.Data.Raw (0)'Access, C.signed_int (Base)) + 2;
		Buffer : C.char_array (0 .. Buffer_Size);
		Dummy : C.char_ptr := C.gmp.mpz_get_str (
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
	
	function Value (Image : String; Base : Number_Base := 10) return MP_Integer is
		Z_Image : constant String := Image & Character'Val (0);
		C_Image : C.char_array (0 .. Z_Image'Length);
		for C_Image'Address use Z_Image'Address;
	begin
		return Result : MP_Integer do
			if C.gmp.mpz_set_str (
				Result.Data.Raw (0)'Access,
				C_Image (C_Image'First)'Access,
				C.signed_int (Base)) < 0
			then
				raise Constraint_Error;
			end if;
		end return;
	end Value;
	
	function "=" (Left, Right : MP_Integer) return Boolean is
	begin
		return C.gmp.mpz_cmp (
			Left.Data.Raw (0)'Access,
			Right.Data.Raw (0)'Access) = 0;
	end "=";
	
	function "<" (Left, Right : MP_Integer) return Boolean is
	begin
		return C.gmp.mpz_cmp (
			Left.Data.Raw (0)'Access,
			Right.Data.Raw (0)'Access) < 0;
	end "<";
	
	function ">" (Left, Right : MP_Integer) return Boolean is
	begin
		return C.gmp.mpz_cmp (
			Left.Data.Raw (0)'Access,
			Right.Data.Raw (0)'Access) > 0;
	end ">";
	
	function "<=" (Left, Right : MP_Integer) return Boolean is
	begin
		return C.gmp.mpz_cmp (
			Left.Data.Raw (0)'Access,
			Right.Data.Raw (0)'Access) <= 0;
	end "<=";
	
	function ">=" (Left, Right : MP_Integer) return Boolean is
	begin
		return C.gmp.mpz_cmp (
			Left.Data.Raw (0)'Access,
			Right.Data.Raw (0)'Access) >= 0;
	end ">=";
	
	function "+" (Right : MP_Integer) return MP_Integer is
	begin
		return Right;
	end "+";
	
	function "-" (Right : MP_Integer) return MP_Integer is
	begin
		return Result : MP_Integer do
			C.gmp.mpz_neg (
				Result.Data.Raw (0)'Access,
				Right.Data.Raw (0)'Access);
		end return;
	end "-";
	
	function "+" (Left, Right : MP_Integer) return MP_Integer is
	begin
		return Result : MP_Integer do
			C.gmp.mpz_add (
				Result.Data.Raw (0)'Access,
				Left.Data.Raw (0)'Access,
				Right.Data.Raw (0)'Access);
		end return;
	end "+";
	
	function "-" (Left, Right : MP_Integer) return MP_Integer is
	begin
		return Result : MP_Integer do
			C.gmp.mpz_sub (
				Result.Data.Raw (0)'Access,
				Left.Data.Raw (0)'Access,
				Right.Data.Raw (0)'Access);
		end return;
	end "-";
	
	function "*" (Left, Right : MP_Integer) return MP_Integer is
	begin
		return Result : MP_Integer do
			C.gmp.mpz_mul (
				Result.Data.Raw (0)'Access,
				Left.Data.Raw (0)'Access,
				Right.Data.Raw (0)'Access);
		end return;
	end "*";
	
	function "/" (Left, Right : MP_Integer) return MP_Integer is
	begin
		return Result : MP_Integer do
			C.gmp.mpz_div (
				Result.Data.Raw (0)'Access,
				Left.Data.Raw (0)'Access,
				Right.Data.Raw (0)'Access);
		end return;
	end "/";
	
	function "**" (Left : MP_Integer; Right : Natural) return MP_Integer is
	begin
		return Result : MP_Integer do
			C.gmp.mpz_pow_ui (
				Result.Data.Raw (0)'Access,
				Left.Data.Raw (0)'Access,
				C.unsigned_long'Mod (Right));
		end return;
	end "**";
	
	function Copy_Sign (Value, Sign : MP_Integer) return MP_Integer is
	begin
		if (C.gmp.qmpz_cmp_si (Value.Data.Raw (0)'Access, 0) < 0) =
			(C.gmp.qmpz_cmp_si (Sign.Data.Raw (0)'Access, 0) < 0)
		then
			return Value;
		else
			return -Value;
		end if;
	end Copy_Sign;
	
	function Copy_Sign (Value : MP_Integer; Sign : Integer) return MP_Integer is
	begin
		if (C.gmp.qmpz_cmp_si (Value.Data.Raw (0)'Access, 0) < 0) = (Sign < 0) then
			return Value;
		else
			return -Value;
		end if;
	end Copy_Sign;
	
	function Copy_Sign (Value : Integer; Sign : MP_Integer) return Integer is
	begin
		if (Value < 0) = (C.gmp.qmpz_cmp_si (Sign.Data.Raw (0)'Access, 0) < 0) then
			return Value;
		else
			return -Value;
		end if;
	end Copy_Sign;
	
	overriding procedure Initialize (Object : in out Controlled) is
	begin
		C.gmp.mpz_init (Object.Raw (0)'Access);
	end Initialize;
	
	overriding procedure Adjust (Object : in out Controlled) is
		Source : constant C.gmp.mpz_t := Object.Raw;
	begin
		C.gmp.mpz_init_set (Object.Raw (0)'Access, Source (0)'Access);
	end Adjust;
	
	overriding procedure Finalize (Object : in out Controlled) is
	begin
		C.gmp.mpz_clear (Object.Raw (0)'Access);
	end Finalize;
	
	procedure Read (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out MP_Integer) is
	begin
		Read (Stream, Item.Data.Raw (0)'Access);
	end Read;
	
	procedure Write (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : in MP_Integer) is
	begin
		Write (Stream, Item.Data.Raw (0)'Access);
	end Write;
	
	procedure Read (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : not null access C.gmp.mpz_struct)
	is
		Size : Ada.Streams.Stream_Element_Offset;
	begin
		Ada.Streams.Stream_Element_Offset'Read (Stream, Size);
		declare
			Data : Ada.Streams.Stream_Element_Array (1 .. Size);
		begin
			Ada.Streams.Stream_Element_Array'Read (Stream, Data);
			C.gmp.mpz_import (
				Item,
				C.size_t (Size),
				1, -- big endian in array
				1, -- word size
				1, -- big endian in word
				0, -- no padding
				C.void_const_ptr (Data'Address));
			if Size > 0 and then Data (1) >= 16#80# then
				declare
					X : C.gmp.mpz_t := (others => (others => <>));
				begin
					C.gmp.mpz_init_set_si (X (0)'Access, -1);
					C.gmp.mpz_mul_2exp (
						X (0)'Access,
						X (0)'Access,
						C.unsigned_long'Mod (Size) * Ada.Streams.Stream_Element'Size);
					C.gmp.mpz_ior (
						Item,
						Item,
						X (0)'Access);
					C.gmp.mpz_clear (X (0)'Access);
				end;
			end if;
		end;
	end Read;
	
	procedure Write (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : not null access constant C.gmp.mpz_struct)
	is
		C_Size : aliased C.size_t;
		Dummy : C.void_ptr := C.gmp.mpz_export (
			C.void_ptr (System.Null_Address),
			C_Size'Access,
			1,
			1,
			1,
			0,
			Item);
		pragma Unreferenced (Dummy);
		Size : constant Ada.Streams.Stream_Element_Count :=
			Ada.Streams.Stream_Element_Count (C_Size);
		Data : Ada.Streams.Stream_Element_Array (0 .. Size);
	begin
		if Size = 0 then
			Ada.Streams.Stream_Element_Offset'Write (Stream, 0);
		elsif C.gmp.qmpz_cmp_si (Item, 0) >= 0 then
			Dummy := C.gmp.mpz_export (
				C.void_ptr (Data (1)'Address),
				C_Size'Access,
				1, -- big endian in array
				1, -- word size
				1, -- big endian in word
				0, -- no padding
				Item);
			if Data (1) >= 16#80# then
				Ada.Streams.Stream_Element_Offset'Write (Stream, Size + 1);
				Data (0) := 0;
				Ada.Streams.Stream_Element_Array'Write (Stream, Data);
			else
				Ada.Streams.Stream_Element_Offset'Write (Stream, Size);
				Ada.Streams.Stream_Element_Array'Write (Stream, Data (1 .. Size));
			end if;
		else
			declare
				Item_1 : C.gmp.mpz_t := (others => (others => <>));
				X : C.gmp.mpz_t := (others => (others => <>));
			begin
				C.gmp.mpz_init (Item_1 (0)'Access);
				C.gmp.mpz_sub_ui (Item_1 (0)'Access, Item, 1);
				C.gmp.mpz_init_set_ui (X (0)'Access, 1);
				C.gmp.mpz_mul_2exp (
					X (0)'Access,
					X (0)'Access,
					(C.unsigned_long'Mod (C.gmp.mpz_sizeinbase (Item_1 (0)'Access, 2)) +
					Ada.Streams.Stream_Element'Size - 1)
					/ Ada.Streams.Stream_Element'Size
					* Ada.Streams.Stream_Element'Size);
				C.gmp.mpz_sub_ui (X (0)'Access, X (0)'Access, 1);
				C.gmp.mpz_and (X (0)'Access, X (0)'Access, Item);
				Dummy := C.gmp.mpz_export (
					C.void_ptr (Data (0)'Address),
					C_Size'Access,
					1, -- big endian in array
					1, -- word size
					1, -- big endian in word
					0, -- no padding
					X (0)'Access);
				Ada.Streams.Stream_Element_Offset'Write (
					Stream,
					Ada.Streams.Stream_Element_Count (C_Size));
				Ada.Streams.Stream_Element_Array'Write (
					Stream,
					Data (0 .. Ada.Streams.Stream_Element_Count (C_Size) - 1));
				C.gmp.mpz_clear (X (0)'Access);
				C.gmp.mpz_clear (Item_1 (0)'Access);
			end;
		end if;
	end Write;
	
end GMP.Z;
