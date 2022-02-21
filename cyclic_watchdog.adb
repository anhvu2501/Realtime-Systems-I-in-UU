-- Cyclic scheduler with a watchdog: Ada lab part 2

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Numerics.Float_Random;

use Ada.Calendar;
use Ada.Text_IO;
use Ada.Numerics.Float_Random;


procedure cyclic_wd is
	Message: constant String := "Cyclic scheduler with watchdog";

	Start_Time : constant Time := Clock;
	iteration_start: Time;
	iteration_end: Time;

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
		Gen : Generator;
		rand_delay : Duration;
	begin
		Put(Message);
		Put_Line(Duration'Image(Clock - Start_Time));

		Reset(Gen);
		rand_delay := Duration(Random(Gen));
		delay until Clock + rand_delay;
	end f3;

	task Watchdog is
		entry Start;
		entry Stop;	
	end Watchdog;

	task body Watchdog is
		time_limit : constant Duration := 0.5; 
		local_start_time : Time;
	begin
		loop
			accept Start;
			local_start_time := Clock;
			select
				accept Stop do
					Put_Line("WD: f3 finished, execution took" & Duration'Image(Clock - local_start_time));
				end Stop;
			or 
				delay until Clock + time_limit;
				Put_Line("WD: timeout");
				accept Stop do
					Put_Line("WD: f3 finished, execution took" & Duration'Image(Clock - local_start_time));
				end Stop;
			end select;
		end loop;
	end Watchdog;

begin
	loop
		iteration_start := Clock;  
		iteration_end := Clock + d;
		f1;
		f2;

		delay until iteration_start + f3_offset;
		Watchdog.Start;
		f3;
		Watchdog.Stop;

		if Clock > iteration_end then
			loop
				iteration_end := iteration_end + d;
				exit when CLock < iteration_end;
			end loop;
		end if;

		delay until iteration_end; 
	end loop;
end cyclic_wd;