with C.mpfr;
package MPFR.Root_FR.Inside is
	pragma Preelaborate;
	
	function Reference (X : in out MP_Float)
		return not null access C.mpfr.mpfr_struct;
	function Constant_Reference (X : MP_Float)
		return not null access constant C.mpfr.mpfr_struct;
	
	pragma Inline (Reference); -- renamed
	pragma Inline (Constant_Reference); -- renamed
	
private	
	
	function Reference (X : in out MP_Float)
		return not null access C.mpfr.mpfr_struct
		renames Controlled.Reference;
	function Constant_Reference (X : MP_Float)
		return not null access constant C.mpfr.mpfr_struct
		renames Controlled.Constant_Reference;
	
end MPFR.Root_FR.Inside;
