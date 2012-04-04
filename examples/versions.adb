with Ada.Text_IO;
with GMP;
with MPFR;
with MPC;
procedure versions is
begin
	Ada.Text_IO.Put_Line ("GMP: " & GMP.Version);
	Ada.Text_IO.Put_Line ("MPFR: " & MPFR.Version);
	Ada.Text_IO.Put_Line ("MPC: " & MPC.Version);
end versions;
