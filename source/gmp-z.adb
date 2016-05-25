pragma Ada_2012;
with System.Storage_Elements;
with C.stdint;
with C.string;
package body GMP.Z is
	use type Ada.Streams.Stream_Element;
	use type Ada.Streams.Stream_Element_Offset;
	use type System.Storage_Elements.Storage_Element;
	use type System.Storage_Elements.Storage_Offset;
	use type C.signed_int;
	use type C.size_t;
	use type C.gmp.mp_size_t;
	
	function To_MP_Integer (X : Long_Long_Integer) return MP_Integer is
	begin
		return Result : MP_Integer do
			mpz_set_Long_Long_Integer (Controlled.Reference (Result), X);
		end return;
	end To_MP_Integer;
	
	function Image (Value : MP_Integer; Base : Number_Base := 10) return String is
		Raw_Value : constant not null access constant C.gmp.mpz_struct :=
			Controlled.Constant_Reference (Value);
		Buffer_Size : constant C.size_t :=
			C.gmp.mpz_sizeinbase (Raw_Value, C.signed_int (Base)) + 2;
		Buffer : aliased C.char_array (0 .. Buffer_Size);
		Dummy : C.char_ptr;
	begin
		Dummy := C.gmp.mpz_get_str (
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
	
	function Value (Image : String; Base : Number_Base := 10) return MP_Integer is
		Z_Image : aliased constant String := Image & Character'Val (0);
		C_Image : C.char_array (0 .. Z_Image'Length);
		for C_Image'Address use Z_Image'Address;
	begin
		return Result : MP_Integer do
			if C.gmp.mpz_set_str (
				Controlled.Reference (Result),
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
			Controlled.Constant_Reference (Left),
			Controlled.Constant_Reference (Right)) = 0;
	end "=";
	
	function "<" (Left, Right : MP_Integer) return Boolean is
	begin
		return C.gmp.mpz_cmp (
			Controlled.Constant_Reference (Left),
			Controlled.Constant_Reference (Right)) < 0;
	end "<";
	
	function ">" (Left, Right : MP_Integer) return Boolean is
	begin
		return C.gmp.mpz_cmp (
			Controlled.Constant_Reference (Left),
			Controlled.Constant_Reference (Right)) > 0;
	end ">";
	
	function "<=" (Left, Right : MP_Integer) return Boolean is
	begin
		return C.gmp.mpz_cmp (
			Controlled.Constant_Reference (Left),
			Controlled.Constant_Reference (Right)) <= 0;
	end "<=";
	
	function ">=" (Left, Right : MP_Integer) return Boolean is
	begin
		return C.gmp.mpz_cmp (
			Controlled.Constant_Reference (Left),
			Controlled.Constant_Reference (Right)) >= 0;
	end ">=";
	
	function "+" (Right : MP_Integer) return MP_Integer is
	begin
		return Right;
	end "+";
	
	function "-" (Right : MP_Integer) return MP_Integer is
	begin
		return Result : MP_Integer do
			C.gmp.mpz_neg (
				Controlled.Reference (Result),
				Controlled.Constant_Reference (Right));
		end return;
	end "-";
	
	function "+" (Left, Right : MP_Integer) return MP_Integer is
	begin
		return Result : MP_Integer do
			C.gmp.mpz_add (
				Controlled.Reference (Result),
				Controlled.Constant_Reference (Left),
				Controlled.Constant_Reference (Right));
		end return;
	end "+";
	
	function "-" (Left, Right : MP_Integer) return MP_Integer is
	begin
		return Result : MP_Integer do
			C.gmp.mpz_sub (
				Controlled.Reference (Result),
				Controlled.Constant_Reference (Left),
				Controlled.Constant_Reference (Right));
		end return;
	end "-";
	
	function "*" (Left, Right : MP_Integer) return MP_Integer is
	begin
		return Result : MP_Integer do
			C.gmp.mpz_mul (
				Controlled.Reference (Result),
				Controlled.Constant_Reference (Left),
				Controlled.Constant_Reference (Right));
		end return;
	end "*";
	
	function "/" (Left, Right : MP_Integer) return MP_Integer is
	begin
		return Result : MP_Integer do
			C.gmp.mpz_div (
				Controlled.Reference (Result),
				Controlled.Constant_Reference (Left),
				Controlled.Constant_Reference (Right));
		end return;
	end "/";
	
	function "**" (Left : MP_Integer; Right : Natural) return MP_Integer is
	begin
		return Result : MP_Integer do
			C.gmp.mpz_pow_ui (
				Controlled.Reference (Result),
				Controlled.Constant_Reference (Left),
				C.unsigned_long'Mod (Right));
		end return;
	end "**";
	
	function Copy_Sign (Value, Sign : MP_Integer) return MP_Integer is
	begin
		if (C.gmp.qmpz_cmp_si (Controlled.Constant_Reference (Value), 0) < 0) =
			(C.gmp.qmpz_cmp_si (Controlled.Constant_Reference (Sign), 0) < 0)
		then
			return Value;
		else
			return -Value;
		end if;
	end Copy_Sign;
	
	function Copy_Sign (Value : MP_Integer; Sign : Integer) return MP_Integer is
	begin
		if (C.gmp.qmpz_cmp_si (Controlled.Constant_Reference (Value), 0) < 0) =
			(Sign < 0)
		then
			return Value;
		else
			return -Value;
		end if;
	end Copy_Sign;
	
	function Copy_Sign (Value : Integer; Sign : MP_Integer) return Integer is
	begin
		if (Value < 0) =
			(C.gmp.qmpz_cmp_si (Controlled.Constant_Reference (Sign), 0) < 0)
		then
			return Value;
		else
			return -Value;
		end if;
	end Copy_Sign;
	
	package body Controlled is
		
		function View_Reference (Item : in out MP_Integer)
			return not null access C.gmp.mpz_struct;
		pragma Inline (View_Reference);
		
		function View_Reference (Item : in out MP_Integer)
			return not null access C.gmp.mpz_struct is
		begin
			return Item.Raw (0)'Unchecked_Access;
		end View_Reference;
		
		-- implementation
		
		function Reference (Item : in out Z.MP_Integer)
			return not null access C.gmp.mpz_struct is
		begin
			return View_Reference (MP_Integer (Item)); -- view conversion
		end Reference;
		
		function Constant_Reference (Item : Z.MP_Integer)
			return not null access constant C.gmp.mpz_struct is
		begin
			return MP_Integer (Item).Raw (0)'Unchecked_Access;
		end Constant_Reference;
		
		overriding procedure Initialize (Object : in out MP_Integer) is
		begin
			C.gmp.mpz_init (Object.Raw (0)'Access);
		end Initialize;
	
		overriding procedure Adjust (Object : in out MP_Integer) is
			Source : constant C.gmp.mpz_t := Object.Raw;
		begin
			C.gmp.mpz_init_set (Object.Raw (0)'Access, Source (0)'Access);
		end Adjust;
	
		overriding procedure Finalize (Object : in out MP_Integer) is
		begin
			C.gmp.mpz_clear (Object.Raw (0)'Access);
		end Finalize;
	
	end Controlled;
	
	procedure Read (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out MP_Integer) is
	begin
		Read (Stream, Controlled.Reference (Item));
	end Read;
	
	procedure Write (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : in MP_Integer) is
	begin
		Write (Stream, Controlled.Constant_Reference (Item));
	end Write;
	
	-- streaming
	
	function bswap32 (x : C.stdint.int32_t) return C.stdint.int32_t
		with Import, Convention => Intrinsic, External_Name => "__builtin_bswap32";
	
	function Convert_BE (X : C.stdint.int32_t) return C.stdint.int32_t is
	begin
		case System.Default_Bit_Order is
			when System.High_Order_First =>
				return X;
			when System.Low_Order_First =>
				return bswap32 (X);
		end case;
	end Convert_BE;
	
	-- Read, equivalent to mpz_in_raw
	
	procedure Read (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : not null access C.gmp.mpz_struct)
	is
		csize : System.Storage_Elements.Storage_Offset;
	begin
		declare
			csize_bytes : C.stdint.int32_t; -- 4 bytes for size
		begin
			C.stdint.int32_t'Read (Stream, csize_bytes);
			csize := System.Storage_Elements.Storage_Offset ( -- sign extending
				Convert_BE (csize_bytes));
		end;
		if csize /= 0 then
			declare
				abs_csize : constant System.Storage_Elements.Storage_Offset := abs csize;
				cp : aliased System.Storage_Elements.Storage_Array (0 .. abs_csize - 1);
			begin
				System.Storage_Elements.Storage_Array'Read (Stream, cp);
				C.gmp.mpz_import (
					Item,
					C.size_t (abs_csize), -- word count
					1, -- big endian in array
					1, -- word size
					1, -- big endian in word
					0, -- no padding
					C.void_const_ptr (cp'Address));
			end;
			if csize < 0 then
				C.gmp.mpz_neg (Item, Item);
			end if;
		else
			C.gmp.mpz_set_ui (Item, 0);
		end if;
	end Read;
	
	-- Write, equivalent to mpz_out_raw
	
	procedure Write (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : not null access constant C.gmp.mpz_struct)
	is
		xsize : constant C.gmp.mp_size_t := C.gmp.mp_size_t (Item.mp_size);
	begin
		if xsize /= 0 then
			declare
				abs_xsize : constant C.gmp.mp_size_t := abs xsize;
				tsize : constant System.Storage_Elements.Storage_Offset :=
					(System.Storage_Elements.Storage_Offset (abs_xsize)
						* C.gmp.GMP_NUMB_BITS + 7)
					/ 8;
				tp : aliased
					System.Storage_Elements.Storage_Array (0 .. tsize - 1);
				bp : System.Storage_Elements.Storage_Offset := 0;
				bytes : aliased C.size_t;
				Dummy : C.void_ptr;
			begin
				Dummy := C.gmp.mpz_export (
					C.void_ptr (tp'Address),
					bytes'Access,
					1, -- big endian in array
					1, -- word size
					1, -- big endian in word
					0, -- no padding
					Item);
				-- strip high zero bytes (without fetching from bp)
				declare
					zeros : System.Storage_Elements.Storage_Offset := 0;
				begin
					while tp (zeros) = 0 loop
						zeros := zeros + 1;
					end loop;
					bp := bp + zeros;
					bytes := bytes - C.size_t (zeros);
				end;
				declare
					-- total bytes to be written
					ssize : constant System.Storage_Elements.Storage_Offset :=
						System.Storage_Elements.Storage_Offset (bytes);
				begin
					declare
						bytes_Signed : C.stdint.int32_t := C.stdint.int32_t (bytes);
					begin
						if xsize < 0 then
							-- twos complement negative for the size value
							bytes_Signed := -bytes_Signed;
						end if;
						C.stdint.int32_t'Write (Stream, Convert_BE (bytes_Signed));
					end;
					System.Storage_Elements.Storage_Array'Write (
						Stream,
						tp (bp .. bp + ssize - 1));
				end;
			end;
		else
			C.stdint.int32_t'Write (Stream, 0);
		end if;
	end Write;
	
end GMP.Z;
