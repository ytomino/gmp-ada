pragma Ada_2012;
package body GMP.Z.Inside is
	
	function Reference (X : aliased in out MP_Integer)
		return not null access C.gmp.mpz_struct is
	begin
		return X.Data.Raw (0)'Unchecked_Access;
	end Reference;
	
	function Constant_Reference (X : aliased in MP_Integer)
		return not null access constant C.gmp.mpz_struct is
	begin
		return X.Data.Raw (0)'Unchecked_Access;
	end Constant_Reference;
	
end GMP.Z.Inside;
