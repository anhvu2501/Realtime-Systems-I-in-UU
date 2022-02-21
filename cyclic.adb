with Ada.Calendar;
with Ada.Text_IO;
use Ada.Calendar;
use Ada.Text_IO;

procedure cyclic is
	Message: constant String := "Cyclic scheduler";

	Start_Time : constant Time := Clock;
	iteration_start: Time;

	d: Duration := 1.0;
	f3_offset : Duration := 0.5;

	procedure f1 is 
		Message: constant String := "f1 executing, time is now";
	begin
		Put(Message);
		Put_Line(Duration'Image(Clock - Start_Time));
	end f1;

	procedure f2 is 
		Message: constant String := "f2 executing, time is now";
	begin
		Put(Message);
		Put_Line(Duration'Image(Clock - Start_Time));
	end f2;

	procedure f3 is 
		Message: constant String := "f3 executing, time is now";
	begin
		Put(Message);
		Put_Line(Duration'Image(Clock - Start_Time));
	end f3;

begin
	loop
		iteration_start := Clock;  
		f1;
		f2;
		delay until iteration_start + f3_offset;
		f3;
		delay until iteration_start + d;
	end loop;
end cyclic;