with GMP;
with MPFR;
private with C.mpc;
package MPC is
	pragma Preelaborate;
	pragma Linker_Options ("-lmpc");
	
	function Version return String;
	
	subtype Number_Base is GMP.Number_Base;
	
	type Rounding is private;
	
	function Compose (
		Real_Rounding : MPFR.Rounding;
		Imaginary_Rounding : MPFR.Rounding)
		return Rounding;
	
	function Default_Rounding return Rounding;
	
	pragma Inline (Compose);
	
private
	
	type Rounding is new C.mpc.mpc_rnd_t;
	
end MPC;
