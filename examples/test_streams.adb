with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GMP.Z;
procedure test_streams is
	use Ada.Streams.Stream_IO;
	use Ada.Strings;
	use Ada.Strings.Fixed;
	use Ada.Text_IO;
	use GMP.Z;
	Verbose : Boolean := False;
	package Binary_IO is new Ada.Text_IO.Modular_IO (Ada.Streams.Stream_Element);
	use Binary_IO;
	procedure test (X : Long_Long_Integer) is
		Z1 : constant MP_Integer := To_MP_Integer (X);
		Z2 : MP_Integer;
		F : Ada.Streams.Stream_IO.File_Type;
		B : Ada.Streams.Stream_Element;
	begin
		if Verbose then
			Put (Trim (Long_Long_Integer'Image (X), Both)); New_Line;
			Put (Image (Z1)); New_Line;
		end if;
		Create (F);
		MP_Integer'Write (Stream (F), Z1);
		Reset (F, In_File);
		Set_Index (F, 1);
		MP_Integer'Read (Stream (F), Z2);
		if Verbose then
			Put (Image (Z2)); New_Line;
		end if;
		Set_Index (F, 1);
		while not End_Of_File (F) loop
			Ada.Streams.Stream_Element'Read (Stream (F), B);
			if Verbose then
				Put (B, Base => 16);
				Put (" ");
			end if;
		end loop;
		if Verbose then
			New_Line;
		end if;
		Close (F);
	end test;
begin
	for I in 1 .. Ada.Command_Line.Argument_Count loop
		if Ada.Command_Line.Argument (I) = "-v" then
			Verbose := True;
		end if;
	end loop;
	test (0);
	test (1);
	test (2);
	test (3);
	test (256);
	test (65535);
	test (-1);
	test (-2);
	test (-3);
	test (-256);
	test (-65535);
	test (Long_Long_Integer'First);
	test (Long_Long_Integer'Last);
	-- finish
	Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error.all, "ok");
end test_streams;
