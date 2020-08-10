with Ada.Finalization;
private with C.gmp;
package GMP.Random is
	pragma Preelaborate;
	
	type Generator is limited private;
	
	function Initialize return Generator;
	function Initialize (Initiator : Long_Integer) return Generator;
	procedure Reset (Gen : in out Generator);
	procedure Reset (Gen : in out Generator; Initiator : in Long_Integer);
	
	type State is private;
	
	function Initialize return State;
	function Initialize (Initiator : Long_Integer) return State;
	function Save (Gen : Generator) return State;
	procedure Save (Gen : in Generator; To_State : out State);
	function Reset (From_State : State) return Generator;
	procedure Reset (Gen : in out Generator; From_State : State);
	
	function Random (Gen : aliased in out Generator) return Long_Integer;
	
	generic
		type Result_Subtype is (<>);
	package Discrete_Random is
		function Random (Gen : aliased in out Generator) return Result_Subtype;
	end Discrete_Random;
	
private
	
	package Controlled is
		
		type State is private;
		
		function Reference (Item : in out GMP.Random.State)
			return not null access C.gmp.gmp_randstate_struct;
		function Constant_Reference (Item : GMP.Random.State)
			return not null access constant C.gmp.gmp_randstate_struct;
		
		pragma Inline (Reference);
		pragma Inline (Constant_Reference);
		
	private
		
		type State is new Ada.Finalization.Controlled
			with record
				Raw : aliased C.gmp.gmp_randstate_t :=
					(others => (mp_seed => (others => (others => <>)), others => <>));
			end record;
		
		overriding procedure Initialize (Object : in out State);
		overriding procedure Adjust (Object : in out State);
		overriding procedure Finalize (Object : in out State);
		
	end Controlled;
	
	type State is new Controlled.State;
	
	type Generator is limited record
		State : GMP.Random.State;
	end record;
	
end GMP.Random;
