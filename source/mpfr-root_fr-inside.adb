package body MPFR.Root_FR.Inside is
	
	function Reference (X : not null access MP_Float)
		return not null access C.mpfr.mpfr_struct is
	begin
		return X.Data.Raw (0)'Access;
	end Reference;
	
	function Constant_Reference (X : not null access constant MP_Float)
		return not null access constant C.mpfr.mpfr_struct is
	begin
		return X.Data.Raw (0)'Access;
	end Constant_Reference;
	
end MPFR.Root_FR.Inside;
