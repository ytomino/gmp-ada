pragma Ada_2012;
package body GMP.Random is
	use type C.unsigned_long;

	function Initialize return Generator is
	begin
		return (State => Initialize);
	end Initialize;
	
	function Initialize (Initiator : Long_Integer) return Generator is
	begin
		return (State => Initialize (Initiator));
	end Initialize;
	
	procedure Reset (Gen : in out Generator) is
	begin
		Gen.State := Initialize;
	end Reset;
	
	procedure Reset (Gen : in out Generator; Initiator : Long_Integer) is
	begin
		Gen.State := Initialize (Initiator);
	end Reset;
	
	function Initialize return State is
	begin
		return Result : State do
			pragma Unreferenced (Result);
			null;
		end return;
	end Initialize;
	
	function Initialize (Initiator : Long_Integer) return State is
		pragma Suppress (Overflow_Check);
		pragma Suppress (Range_Check);
	begin
		return Result : State do
			C.gmp.gmp_randseed_ui (
				Reference (Result),
				C.unsigned_long'Mod (Initiator));
		end return;
	end Initialize;
	
	function Save (Gen : Generator) return State is
	begin
		return Gen.State;
	end Save;
	
	procedure Save (Gen : Generator; To_State : out State) is
	begin
		To_State := Gen.State;
	end Save;
	
	function Reset (From_State : State) return Generator is
	begin
		return (State => From_State);
	end Reset;
	
	procedure Reset (Gen : in out Generator; From_State : State) is
	begin
		Gen.State := From_State;
	end Reset;
	
	function Random (Gen : aliased in out Generator) return Long_Integer is
		pragma Suppress (Overflow_Check);
		pragma Suppress (Range_Check);
	begin
		return Long_Integer (C.gmp.gmp_urandomb_ui (
			Reference (Gen.State),
			Long_Integer'Size));
	end Random;
	
	package body Discrete_Random is
		
		function Random (Gen : aliased in out Generator) return Result_Subtype is
		begin
			return Result_Subtype'Val (
				C.gmp.gmp_urandomm_ui (
					Reference (Gen.State),
					Result_Subtype'Range_Length)
				+ Result_Subtype'Pos (Result_Subtype'First));
		end Random;
		
	end Discrete_Random;
	
	package body Controlled is
		
		function Reference (Item : in out State)
			return not null access C.gmp.gmp_randstate_struct is
		begin
			return Item.Raw (0)'Unchecked_Access;
		end Reference;
		
		function Constant_Reference (Item : State)
			return not null access constant C.gmp.gmp_randstate_struct is
		begin
			return Item.Raw (0)'Unchecked_Access;
		end Constant_Reference;
		
		overriding procedure Initialize (Object : in out State) is
		begin
			C.gmp.gmp_randinit_default (Object.Raw (0)'Access);
		end Initialize;
		
		overriding procedure Adjust (Object : in out State) is
			Old : constant C.gmp.gmp_randstate_t := Object.Raw;
		begin
			C.gmp.gmp_randinit_set (Object.Raw (0)'Access, Old (0)'Access);
		end Adjust;
		
		overriding procedure Finalize (Object : in out State) is
		begin
			C.gmp.gmp_randclear (Object.Raw (0)'Access);
		end Finalize;
		
	end Controlled;
	
end GMP.Random;
