> https://www.tutorialspoint.com/slf4j/slf4j_profiling.htm

SLF4J Distribution provides slf4j-ext.jar this contains APIs for the functionalities such as profiling, Extended logging, Event logging and, logging with java agent.

Profiling
Sometimes the programmer wants to measure some attributes like the use of memory, time complexity or usage of particular instructions about the programs to measure the real capability of that program. Such kind of measuring about the program is called profiling. Profiling uses dynamic program analysis to do such measuring.

SLF4J provides a class named Profiler in the org.slf4j.profiler package for profiling purpose. This is known as the poor man’s profiler. Using this, the programmer can find out the time taken to carry out prolonged tasks.

Profiling Using the Profiler class
The profiler contains stopwatches and child stopwatches and we can start and stop these using the methods provided by the profiler class.

To carry on with profiling using the profiler class, follow the steps given below.

Step 1 - Instantiate the profiler class
Instantiate the Profiler class by passing a String value representing the name of the profiler. When we instantiate a Profiler class, a global stopwatch will be started.

//Creating a profiler
Profiler profiler = new Profiler("Sample");
Step 2 - Start a child stopwatch
When we invoke the start() method it will start a new child stopwatch (named) and, stops the earlier child stopwatches (or, time instruments).

Invoke the start() method of the Profiler class by passing a String value representing the name of the child stopwatch to be created.

//Starting a child stopwatch and stopping the previous one.
profiler.start("Task 1");
obj.demoMethod1();
After creating these stopwatches, you can perform your tasks or, invoke those methods, which run your tasks.

Step 3: Start another child stopwatch (if you wish to)
If you need, create another stopwatch using the start() method and perform the required tasks. If you do so, it will start a new stop watch and stops the previous one (i.e. task 1).

//Starting another child stopwatch and stopping the previous one.
profiler.start("Task 2");
obj.demoMethod2();
Step 4: Stop the watches
When we invoke the stop() method, it will stop the recent child stopwatch and the global stopwatch and returns the current Time Instrument.

// Stopping the current child stopwatch and the global stopwatch.
TimeInstrument tm = profiler.stop();
Step 5: Print the contents of the time instrument.
Print the contents of the current time instrument using the print() method.

//printing the contents of the time instrument
tm.print();
Example
The following example demonstrates the profiling using Profiler class of SLF4J. Here we have taken two sample tasks, printing the sum of squares of the numbers from 1 to 10000, printing the sum of the numbers from 1 to 10000. We are trying to get the time taken for these two tasks.

import org.slf4j.profiler.Profiler;
import org.slf4j.profiler.TimeInstrument;

public class ProfilerExample {
   public void demoMethod1(){
      double sum = 0;
      for(int i=0; i< 1000; i++){
         sum = sum+(Math.pow(i, 2));
      }
      System.out.println("Sum of squares of the numbers from 1 to 10000: "+sum);
   }
   public void demoMethod2(){
      int sum = 0;
      for(int i=0; i< 10000; i++){
         sum = sum+i;
      }
      System.out.println("Sum of the numbers from 1 to 10000: "+sum);
   }
   public static void main(String[] args) {
      ProfilerExample obj = new ProfilerExample();

      //Creating a profiler
      Profiler profiler = new Profiler("Sample");

      //Starting a child stop watch and stopping the previous one.
      profiler.start("Task 1");
      obj.demoMethod1();

      //Starting another child stop watch and stopping the previous one.
      profiler.start("Task 2");
      obj.demoMethod2();
 
      //Stopping the current child watch and the global watch.
      TimeInstrument tm = profiler.stop();

      //printing the contents of the time instrument
      tm.print();
   }
}
Output
Upon execution, the above program generates the following output −

Sum of squares of the numbers from 1 to 10000: 3.328335E8
Sum of the numbers from 1 to 10000: 49995000
+ Profiler [BASIC]
|-- elapsed time [Task 1] 2291.827 microseconds.
|-- elapsed time [Task 2] 225.802 microseconds.
|-- Total [BASIC] 3221.598 microseconds.
Logging the Profiler Information
Instead of printing the result of a profiler to log this information, you need to −

Create a logger using the LoggerFactory class.

Create a profiler by instantiating the Profiler class.

Associate the logger to profiler by passing the logger object created to the setLogger() method of the Profiler class.

Finally, instead of printing log the information of the profiler using the log() method.

Example
In the following example, unlike the previous one (instead of printing), we are trying to log the contents of the time instrument.

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.profiler.Profiler;
import org.slf4j.profiler.TimeInstrument;

public class ProfilerExample_logger {
   public void demoMethod1(){
      double sum = 0;
      for(int i=0; i< 1000; i++){
         sum = sum+(Math.pow(i, 2));
      }
      System.out.println("Sum of squares of the numbers from 1 to 10000: "+sum);
   }
   public void demoMethod2(){
      int sum = 0;
      for(int i=0; i< 10000; i++){
         sum = sum+i;
      }
      System.out.println("Sum of the numbers from 1 to 10000: "+sum);
   }
   public static void main(String[] args) {
      ProfilerExample_logger obj = new ProfilerExample_logger();

      //Creating a logger
      Logger logger = LoggerFactory.getLogger(ProfilerExample_logger.class);

      //Creating a profiler
      Profiler profiler = new Profiler("Sample");

      //Adding logger to the profiler
      profiler.setLogger(logger);

      //Starting a child stop watch and stopping the previous one.
      profiler.start("Task 1");
      obj.demoMethod1();

      //Starting another child stop watch and stopping the previous one.
      profiler.start("Task 2");
      obj.demoMethod2();

      //Stopping the current child watch and the global watch.
      TimeInstrument tm = profiler.stop();

      //Logging the contents of the time instrument
      tm.log();
   }
}
Output
Upon execution, the above program generates the following output.

Sum of squares of the numbers from 1 to 10000: 3.328335E8
Sum of the numbers from 1 to 10000: 49995000
