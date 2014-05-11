with GMP.Random;
with Ada.Text_IO;
procedure random is
	Gen : aliased GMP.Random.Generator;
begin
	Ada.Text_IO.Put_Line (Long_Integer'Image (GMP.Random.Random (Gen)));
	Ada.Text_IO.Put_Line (Long_Integer'Image (GMP.Random.Random (Gen)));
	Ada.Text_IO.Put_Line (Long_Integer'Image (GMP.Random.Random (Gen)));
end random;
