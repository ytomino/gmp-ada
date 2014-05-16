pragma Ada_2012;
with C.mpfr;
package MPFR.Root_FR.Inside is
	pragma Preelaborate;
	
	function Reference (X : aliased in out MP_Float)
		return not null access C.mpfr.mpfr_struct;
	function Constant_Reference (X : aliased in MP_Float)
		return not null access constant C.mpfr.mpfr_struct;
	
	pragma Inline (Reference);
	pragma Inline (Constant_Reference);
	
end MPFR.Root_FR.Inside;
