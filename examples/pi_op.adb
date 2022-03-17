-- original: http://homepage2.nifty.com/m_kamada/math/gmp_ja.htm
with Ada.Command_Line;
with Ada.Real_Time;
with Ada.Text_IO;
with MPFR.Generic_FR;
procedure pi_op is
	use type Ada.Real_Time.Time;
	use type MPFR.Precision;
	
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
	
	generic
		with package MPF is new MPFR.Generic_FR (others => <>);
	function mpf_pi return MPF.MP_Float;
	function mpf_pi return MPF.MP_Float is
		use type MPF.MP_Float;
		SQRT_SQRT_EPSILON : constant MPF.MP_Float :=
			MPF.To_MP_Float (1.0) / MPF.To_MP_Float (2.0) ** (Integer (MPF.Precision) - 4);
		c : MPF.MP_Float := MPF.Sqrt (MPF.To_MP_Float (0.125));
		a : MPF.MP_Float := 1.0 + 3.0 * c;
		b : MPF.MP_Float := MPF.Sqrt (a);
		e : MPF.MP_Float := b - 0.625;
		npow : MPF.MP_Float := MPF.To_MP_Float (4.0);
	begin
		b := 2.0 * b;
		c := e - c;
		a := a + e;
		loop
			npow := 2.0 * npow;
			e := (a + b) / 2.0;
			b := MPF.Sqrt (a * b);
			e := e - b;
			b := 2.0 * b;
			c := c - e;
			a := e + b;
			exit when not (e > SQRT_SQRT_EPSILON);
		end loop;
		e := e * e / 4.0;
		a := a + b;
		return (a * a - e - e / 2.0) / (a * c - e) / npow;
	end mpf_pi;

	LOG_2_10 : constant := 3.32192809488736234787031942949;

	output : Integer := 1;
	prec10 : MPFR.Precision := 100;
	start, stop : Ada.Real_Time.Time;
begin
	begin
		if Ada.Command_Line.Argument_Count in 1 .. 2 then
			if Ada.Command_Line.Argument_Count = 2 then
				output := Integer'Value (Ada.Command_Line.Argument (2));
			end if;
			prec10 := MPFR.Precision'Value (Ada.Command_Line.Argument (1));
			if prec10 < 10 then
				prec10 := 10;
			end if;
		else
			raise Constraint_Error;
		end if;
	exception
		when Constraint_Error =>
			Ada.Text_IO.Put_Line (
				"usage: " & Ada.Command_Line.Command_Name
					& " precision(10-) output-flag(0-1)");
			Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
			return;
	end;
	declare
		package MPF is new MPFR.Generic_FR (
			MPFR.Precision (Long_Long_Float (2 + prec10) * LOG_2_10 + 1.0),
			MPFR.Default_Rounding);
		function mpf_pi is new pi_op.mpf_pi (MPF);
		pi : MPF.MP_Float;
	begin
		stop := Ada.Real_Time.Clock;
		loop
			start := Ada.Real_Time.Clock;
			exit when start /= stop;
		end loop;
		pi := mpf_pi;
		stop := Ada.Real_Time.Clock;
		Ada.Text_IO.Put_Line (
			Duration'Image (Ada.Real_Time.To_Duration (stop - start)) & " sec.");
		if output /= 0 then
			Ada.Text_IO.Put_Line (MPF.Image (pi));
			stop := Ada.Real_Time.Clock;
			Ada.Text_IO.Put_Line (
				Duration'Image (Ada.Real_Time.To_Duration (stop - start)) & " sec.");
		end if;
	end;
end pi_op;
