package body GMP.Z.Inside is
	
	function Reference (X : not null access MP_Integer)
		return not null access C.gmp.mpz_struct is
	begin
		return X.Data.Raw (0)'Access;
	end Reference;
	
	function Constant_Reference (X : not null access constant MP_Integer)
		return not null access constant C.gmp.mpz_struct is
	begin
		return X.Data.Raw (0)'Access;
	end Constant_Reference;
	
end GMP.Z.Inside;
