with GMP.Z;
with GMP.Q;
with GMP.F;
with GMP.FR;
with GMP.C;
procedure check_sig is
	-- numeric
	generic
		type T is private;
		with function Image (Value : T; Base : GMP.Number_Base) return String is <>;
		with function Value (Image : String; Base : GMP.Number_Base) return T is <>;
		pragma Unreferenced (Image);
		pragma Unreferenced (Value);
		with function "=" (Left, Right : T) return Boolean is <>;
		with function "<" (Left, Right : T) return Boolean is <>;
		with function ">" (Left, Right : T) return Boolean is <>;
		with function "<=" (Left, Right : T) return Boolean is <>;
		with function ">=" (Left, Right : T) return Boolean is <>;
		pragma Unreferenced ("=");
		pragma Unreferenced ("<");
		pragma Unreferenced (">");
		pragma Unreferenced ("<=");
		pragma Unreferenced (">=");
		with function "+" (Right : T) return T is <>;
		with function "-" (Right : T) return T is <>;
		pragma Unreferenced ("+");
		pragma Unreferenced ("-");
		with function "+" (Left, Right : T) return T is <>;
		with function "-" (Left, Right : T) return T is <>;
		with function "*" (Left, Right : T) return T is <>;
		with function "/" (Left, Right : T) return T is <>;
		with function "**" (Left : T; Right : Natural) return T is <>;
		pragma Unreferenced ("+");
		pragma Unreferenced ("-");
		pragma Unreferenced ("*");
		pragma Unreferenced ("/");
		pragma Unreferenced ("**");
		-- scale, root, sqrt
	package Sig_N is
	end Sig_N;
	-- scalar
	generic
		type T is private;
		pragma Unreferenced (T);
	package Sig_S is
	end Sig_S;
	-- real
	generic
		type T is private;
		pragma Unreferenced (T);
		-- truncate, ceil, floor
	package Sig_R is
	end Sig_R;
	-- float
	generic
		type T is private;
		pragma Unreferenced (T);
		-- nearly_equal, frexp, log, based_log
	package Sig_F is
	end Sig_F;
	-- instances
	package F is new GMP.F;
	package FR is new GMP.FR;
	package C is new GMP.C (FR, FR);
begin
	declare -- Z is numeric/scalar
		use GMP.Z;
		package ZN is new Sig_N (MP_Integer);
		package ZS is new Sig_S (MP_Integer);
		pragma Unreferenced (ZN);
		pragma Unreferenced (ZS);
	begin
		null;
	end;
	declare -- Q is numeric/real
		use GMP.Q;
		package QN is new Sig_N (MP_Rational);
		package QR is new Sig_R (MP_Rational);
		pragma Unreferenced (QN);
		pragma Unreferenced (QR);
	begin
		null;
	end;
	declare -- F is numeric/real/float
		use F;
		package FN is new Sig_N (MP_Float);
		package FR is new Sig_R (MP_Float);
		package FF is new Sig_F (MP_Float);
		pragma Unreferenced (FN);
		pragma Unreferenced (FR);
		pragma Unreferenced (FF);
	begin
		null;
	end;
	declare -- FR is numeric/real/float
		use FR;
		package FRN is new Sig_N (MP_Float);
		package FRR is new Sig_R (MP_Float);
		package FRF is new Sig_F (MP_Float);
		pragma Unreferenced (FRN);
		pragma Unreferenced (FRR);
		pragma Unreferenced (FRF);
	begin
		null;
	end;
	declare -- C is numeric
		use C;
		package CN is new Sig_N (MP_Complex);
		pragma Unreferenced (CN);
	begin
		null;
	end;
end check_sig;
