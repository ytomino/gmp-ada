-- original: http://homepage2.nifty.com/m_kamada/math/gmp_ja.htm
with Ada.Command_Line;
with Ada.Real_Time;
with Ada.Text_IO;
with C.gmp;
procedure test_pi is
	use type Ada.Real_Time.Time;
	use C;
	use C.gmp;
	
--    Function: void mpf_pi (mpf_t rop)
--      Set the value of pi.
--  
--    Reference:
--      http://www.kurims.kyoto-u.ac.jp/~ooura/pi_fft-j.html
--  
--    ---- a formula based on the AGM (Arithmetic-Geometric Mean) ----
--      c = sqrt(0.125);
--      a = 1 + 3 * c;
--      b = sqrt(a);
--      e = b - 0.625;
--      b = 2 * b;
--      c = e - c;
--      a = a + e;
--      npow = 4;
--      do {
--          npow = 2 * npow;
--          e = (a + b) / 2;
--          b = sqrt(a * b);
--          e = e - b;
--          b = 2 * b;
--          c = c - e;
--          a = e + b;
--      } while (e > SQRT_SQRT_EPSILON);
--      e = e * e / 4;
--      a = a + b;
--      pi = (a * a - e - e / 2) / (a * c - e) / npow;
--    ---- modification ----
--      This is a modified version of Gauss-Legendre formula
--      (by T.Ooura). It is faster than original version.
	
	procedure mpf_pi (a : in out mpf_t) is
		prec : mp_bitcnt_t;
		SQRT_SQRT_EPSILON, c, b, e : mpf_t;
		log2_npow : unsigned_long;
	begin
		prec := mpf_get_prec (a (0)'Access);
		mpf_init2 (SQRT_SQRT_EPSILON (0)'Access, prec);
		mpf_init2 (c (0)'Access, prec);
		mpf_init2 (b (0)'Access, prec);
		mpf_init2 (e (0)'Access, prec);
		
		mpf_set_ui (SQRT_SQRT_EPSILON (0)'Access, 1);
		mpf_div_2exp (
			SQRT_SQRT_EPSILON (0)'Access,
			SQRT_SQRT_EPSILON (0)'Access,
			prec - 4);
		
		-- c = sqrt(0.125);
		mpf_set_d (c (0)'Access, 0.125);
		mpf_sqrt (c (0)'Access, c (0)'Access);
		-- a = 1 + 3 * c;
		mpf_mul_ui (a (0)'Access, c (0)'Access, 3);
		mpf_add_ui (a (0)'Access, a (0)'Access, 1);
		-- b = sqrt(a);
		mpf_sqrt (b (0)'Access, a (0)'Access);
		-- e = b - 0.625;
		mpf_set_d (e (0)'Access, 0.625);
		mpf_sub (e (0)'Access, b (0)'Access, e (0)'Access);
		-- b = 2 * b;
		mpf_add (b (0)'Access, b (0)'Access, b (0)'Access);
		-- c = e - c;
		mpf_sub (c (0)'Access, e (0)'Access, c (0)'Access);
		-- a = a + e;
		mpf_add (a (0)'Access, a (0)'Access, e (0)'Access);
		-- npow = 4;
		log2_npow := 2;
		loop
			-- npow = 2 * npow;
			log2_npow := log2_npow + 1;
			-- e = (a + b) / 2;
			mpf_add (e (0)'Access, a (0)'Access, b (0)'Access);
			mpf_div_2exp (e (0)'Access, e (0)'Access, 1);
			-- b = sqrt(a * b);
			mpf_mul (b (0)'Access, a (0)'Access, b (0)'Access);
			mpf_sqrt (b (0)'Access, b (0)'Access);
			-- e = e - b;
			mpf_sub (e (0)'Access, e (0)'Access, b (0)'Access);
			-- b = 2 * b;
			mpf_add (b (0)'Access, b (0)'Access, b (0)'Access);
			-- c = c - e;
			mpf_sub (c (0)'Access, c (0)'Access, e (0)'Access);
			-- a = e + b;
			mpf_add (a (0)'Access, e (0)'Access, b (0)'Access);
			-- e > SQRT_SQRT_EPSILON
			exit when not (mpf_cmp (e (0)'Access, SQRT_SQRT_EPSILON (0)'Access) > 0);
		end loop;
		-- e = e * e / 4;
		mpf_mul (e (0)'Access, e (0)'Access, e (0)'Access);
		mpf_div_2exp (e (0)'Access, e (0)'Access, 2);
		-- a = a + b;
		mpf_add (a (0)'Access, a (0)'Access, b (0)'Access);
		-- pi = (a * a - e - e / 2) / (a * c - e) / npow;
		mpf_mul (c (0)'Access, c (0)'Access, a (0)'Access);
		mpf_sub (c (0)'Access, c (0)'Access, e (0)'Access);
		mpf_mul (a (0)'Access, a (0)'Access, a (0)'Access);
		mpf_sub (a (0)'Access, a (0)'Access, e (0)'Access);
		mpf_div_2exp (e (0)'Access, e (0)'Access, 1);
		mpf_sub (a (0)'Access, a (0)'Access, e (0)'Access);
		mpf_div (a (0)'Access, a (0)'Access, c (0)'Access);
		mpf_div_2exp (a (0)'Access, a (0)'Access, log2_npow);
		
		mpf_clear (e (0)'Access);
		mpf_clear (b (0)'Access);
		mpf_clear (c (0)'Access);
		mpf_clear (SQRT_SQRT_EPSILON (0)'Access);
	end mpf_pi;
	
	LOG_2_10 : constant := 3.32192809488736234787031942949;
	
	output : Integer := 1;
	prec10 : mp_bitcnt_t := 100;
	pi : mpf_t;
	start, stop : Ada.Real_Time.Time;
begin
	begin
		if Ada.Command_Line.Argument_Count in 1 .. 2 then
			if Ada.Command_Line.Argument_Count = 2 then
				output := Integer'Value (Ada.Command_Line.Argument (2));
			end if;
			prec10 := mp_bitcnt_t'Value (Ada.Command_Line.Argument (1));
			if prec10 < 10 then
				prec10 := 10;
			end if;
		else
			raise Constraint_Error;
		end if;
	exception
		when Constraint_Error =>
			Ada.Text_IO.Put_Line (
				"usage: " &
				Ada.Command_Line.Command_Name &
				" precision(10-) output-flag(0-1)");
			Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
			return;
	end;
	
	mpf_init2 (
		pi (0)'Access,
		mp_bitcnt_t (Long_Long_Float (2 + prec10) * LOG_2_10 + 1.0));
	
	stop := Ada.Real_Time.Clock;
	loop
		start := Ada.Real_Time.Clock;
		exit when start /= stop;
	end loop;
	mpf_pi (pi);
	stop := Ada.Real_Time.Clock;
	Ada.Text_IO.Put_Line (
		Duration'Image (Ada.Real_Time.To_Duration (stop - start)) &
		" sec.");
	if output /= 0 then
		declare
			fmt : C.char_array := "%.*Ff" & C.char'Val (10) & C.char'Val (0);
		begin
			gmp_printf (fmt (0)'Access, C.size_t (prec10), pi (0)'Access);
		end;
		stop := Ada.Real_Time.Clock;
		Ada.Text_IO.Put_Line (
			Duration'Image (Ada.Real_Time.To_Duration (stop - start)) &
			" sec.");
	end if;
	mpf_clear (pi (0)'Access);
end test_pi;
