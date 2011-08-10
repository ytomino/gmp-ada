with C.mpfr;
package MPFR.Root_FR.Inside is
	pragma Preelaborate;
	
	function Reference (X : not null access MP_Float)
		return not null access C.mpfr.mpfr_struct;
	pragma Inline (Reference);
	function Constant_Reference (X : not null access constant MP_Float)
		return not null access constant C.mpfr.mpfr_struct;
	pragma Inline (Constant_Reference);
	
end MPFR.Root_FR.Inside;
