-- Protected types: Ada lab part 4

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;
use Ada.Calendar;
use Ada.Text_IO;

procedure comm2 is
  Message: constant String := "Protected Object";
  Max_Value : constant Integer := 20;
  Buffer_Size_Limit : constant Integer := 10;

  type Buffer_Array is array (0 .. (buffer_size_limit - 1)) of Integer;

  function rand_delay(min, max : Float) return Duration is
    use Ada.Numerics.Float_Random;
    gen : Generator;
  begin
    Reset(gen);
    return Duration(min + Random(gen) * (max - min));
  end rand_delay;


  protected buffer is
    entry put(n : in Integer);
    entry get(n : out Integer);
  private
    size_limit : Integer := Buffer_Size_Limit;
    storage : Buffer_Array;

    head : Integer := 0;
    size : Integer := 0; 
  end buffer;

  task producer is
    entry Start;
    entry Stop;
  end producer;

  task consumer is
    entry Start;
  end consumer;


  protected body buffer is 
    entry put(n : in Integer) when size < size_limit is 
    begin
      storage((head + size) mod size_limit) := n;
      size := size + 1;
    end put;

    entry get(n : out Integer) when size > 0 is
    begin
      n := storage(head);
      head := (head + 1) mod size_limit;
      size := size - 1;
     end get;
  end buffer;

  task body producer is 
    Message: constant String := "producer executing";
    Min_Delay : constant Float := 2.0;
    Max_Delay : constant Float := 5.0;

    n : Integer;

    function rand_value return Integer is
    type rand_range is range 0 .. 20;
    package Rand_Int is new Ada.Numerics.Discrete_Random(rand_range);
    use Rand_Int;

    gen : Generator;
    begin
    reset(gen);
    return Integer(Random(gen));
    end rand_value;
  begin
    accept Start;
    Put_Line(Message);
    loop
      select
        accept Stop;
        exit;
      or
        delay until Clock + rand_delay(Min_Delay, Max_Delay);		
        n := rand_value;
        buffer.put(n);
        Put_Line("producer put" & Integer'Image(n) & " into buffer");
      end select;
    end loop;
    Put_Line("Ending the producer");
  end producer;

  task body consumer is 
    Message: constant String := "consumer executing";
    Min_Delay : constant Float := 2.0;
    Max_Delay : constant Float := 5.0;
    Sum_Goal : constant Integer := 100;

    n : Integer;
    sum : Integer := 0; 
  begin
    accept Start;
    Put_Line(Message);
    Main_Cycle:
    loop 
      delay until Clock + rand_delay(Min_Delay, Max_Delay);		
      buffer.get(n);
      sum := sum + n;
      Put_Line("consumer took" & Integer'Image(n) & " from buffer, sum is" & Integer'Image(sum));

      exit when sum > Sum_Goal;
    end loop Main_Cycle; 

    producer.Stop; 
    Put_Line("Ending the consumer");
  end consumer;

begin
  Put_Line(Message);
  producer.Start;
  consumer.Start;
end comm2;