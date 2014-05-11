pragma Ada_2012;
package body MPFR.Root_FR.Inside is
	
	function Reference (X : aliased in out MP_Float)
		return not null access C.mpfr.mpfr_struct is
	begin
		return X.Data.Raw (0)'Unchecked_Access;
	end Reference;
	
	function Constant_Reference (X : aliased in MP_Float)
		return not null access constant C.mpfr.mpfr_struct is
	begin
		return X.Data.Raw (0)'Unchecked_Access;
	end Constant_Reference;
	
end MPFR.Root_FR.Inside;
