Qry: A lightweight experiment management system
===

Qry aims to simplify the task of starting a large number of experiments; for example,
when exploring a parameter space.
In addition, it can store basic information about the run, including runtime, Git revision, and key results.

Key features include:

-  Simple, intuitive syntax; no external dependencies
-  Managing statistics from runs
-  Process level parallelism
-  Arbitrary Scala code can be run either once, or for each run.

Basic Syntax
-------------

### First Script
We will create a Scala script with `Qry` imported.
This is unfortunately boilerplate necessary for every script -- I promise
there's not much of it (arguably, this is all of it)!

     #!/bin/bash
     exec scala -cp qry.jar "$0" "$@"
     !#
     import Qry._
     
     println("Hello World! (but, we're cheating)")
     
     >> Hello World! (but, we're cheating)

### Hello World!
On most unix systems, there is a program entitled `hello` which prints Hello World to the screen.
We will run this program, with no arguments.
Note that we can use either "strings" or 'symbols to denote a program name, and later arguments as well.
Submitting a program then comes down to simply calling the `submit()` function.

     #!/bin/bash
     exec scala -cp qry.jar "$0" "$@"
     !#
     import Qry._
     
     submit("hello")
     submit('hello)
     
     >> -- 1 job submitted
     >> Hello, world!
     >> -- 1 job submitted
     >> Hello, world!

### Hello Google!
Of course, this would not be a very useful tool if you could not pass a program command line arguments.
This is done in a way reminiscent of shell scripting: A dash (`-`) preceeds arguments which are intended to
be preceded by a dash; an arrow (`->`) preceeds arguments which are passed verbatim.
Arguments which are intended to be key/values are placed into a tuple: `-("key", "value")`.
For example, we can try to ping Google:

     #!/bin/bash
     exec scala -cp qry.jar "$0" "$@"
     !#
     import Qry._
    
     submit("ping" -("c", 3) -"a" ->"google.com")

     >> -- 1 job submitted

Note that we could have equally well used symbols for all the entries but
google.com:
     
     submit('ping -('c, 3) -'a ->"google.com")

### Hello Internet!
Let's not leave anyone out!
If we want to start many runs with multiple parameters, we can separate them with either the `&` symbol
or `|` symbol.
The difference between the two becomes apparent when multiple arguments vary -- see below.
For now, let's try to start two jobs: one to ping google, and one to ping facebook:

     #!/bin/bash
     exec scala -cp qry.jar "$0" "$@"
     !#
     import Qry._
    
     submit("ping" -("c", 3) -"a" ->("google.com" & "facebook.com"))

     >> -- 2 jobs submitted

### Multiple Free Parameters
The difference between the `&` and `|` symbol becomes apparent when multiple arguments vary.
The first case (`&`) will take the cross product of the arguments.
The second case (`|`) will run a single run with the first argument from each list, and then a number of runs
varying each of the variable parameters (but holding every other parameter fixed to its first value).
To illustrate:

     #!/bin/bash
     exec scala -cp qry.jar "$0" "$@"
     !#
     import Qry._
    
     submit("ping" -("c", "3" | "4") -"a" ->("google.com" | "facebook.com"))

     >> -- 3 job submitted
     >> // ping -c 3 -a google.com
     >> // ping -c 4 -a google.com
     >> // ping -c 3 -a facebook.com

Versus:

     #!/bin/bash
     exec scala -cp qry.jar "$0" "$@"
     !#
     import Qry._
    
     submit("ping" -("c", "3" & "4") -"a" ->("google.com" & "facebook.com"))

     >> -- 4 job submitted
     >> // ping -c 3 -a google.com
     >> // ping -c 4 -a google.com
     >> // ping -c 3 -a facebook.com
     >> // ping -c 4 -a google.com

Configuration
-------------
Many of the auxilliary features of Qry are provided with the `using()` function.
A single function call with different types of arguments will eventually be able
to handle a wide range of different tasks.
Examples are documented in the following sections:

### Managing Runs
A directory can be specified for saving key characteristics of every job started
with a script by specifying:

     using("/path/to/directory/")

If the directory does not exist, and the specification ends with a `/`, then it
will be created.

The files saved in this directory are:
-  `_qry.json`: A key-value store with relevant statistics about the run.
   This includes: (1) timing information; (2) environment information
   (host, folder, git revision, etc); and (3) key results scraped from the
   output stream (more later).
-  `_cmd.ser`: A serialized version of the process to be run. Note that this is
   buggy still.
-  `_cmd.txt`: A text file with the list of arguments to pass to the program;
   each element of the list should be surrounded by spaces.

As mentioned above, one of the key elements in `_qry.json` in this folder are
results scraped from the running program.
It will save lines matching the regexps:

     ^.*result[\s\]]*[:\-\s]\s*(.+)\s*[=:\-]>?\s*([^>].+)\s*(?iu)$
     ^\s*(.+)\s*[=:\-]>?\s*([0-9\.\-e\%\(\)\$]+)\s*(?iu)$

Informally, these are lines of the form:
`result: key = value` or `result: key: value` or `key: value_as_number`; etc.

Lastly, note that you can retrieve this run directory with the `touch` function.
For example, the following command will create a new file in the execution
directory (confusingly, using the unix touch program):
     
     using("/path/to/dir/")
     submit("touch" ->touch("filename_to_create.txt"))
     
     >> -- 1 job submitted
     >> // touch /path/to/dir/filename_to_create.txt

### Remote Execution
Eventually, the capability of running jobs on remote hosts will be added.
The syntax for this will be:

     using("user@hostname")

This functionality is not yet implemented, but is documented here as a roadmap.

### Redis Integration
Eventually, results will be storable not only in a file in the run folder,
but also in a [potentially remote] redis server.
This would guarantee better querying of relevant statistics, as well as more
robust concurrency. The syntax for this will be:

     using("redis:hostname")

This functionality is not yet implemented, but is documented here as a roadmap.

### Properties File
Often, a configuration already exists in a properties file.
This will be able to be loaded as a starting point for the program's arguments
with the command:
     
     using("/properties/file")

Note that the file must exist, and should not contain a trailing `/`.
This functionality is not yet implemented, but is documented here as a roadmap.

Parallelism
-------------
Occasionally we may want to run many single-threaded programs on a single
machine, making use of multiple cores.
Effectively, we would like to create a single machine scheduler.
Qry aims to make this as easy as possible: with the addition of a single
keyword before `submit`:

     parallel submit("hello")

More interestingly, if a specification would start multiple jobs
(usually in sequence), it now
starts each job in its own thread, using as many threads as available on the
machine.

If we would like to use a fixed number of cores, they can be specified as
follows:
     
     parallel(number_of_cores) submit("hello")

Advanced Features
-------------
Note that these are more likely to be bug-prone; but, also more likely to
be cool!

### Lazy Function
A rare, but neat feature of the program is the ability to use laxy functions
as arguments.
For example, the following program will actually print a different value after
each invocation:

     var i = 0
     val getI = () => { i += 1; i }
     submit("echo" ->(getI | getI | getI))

     >> -- 3 jobs submitted
     >> 1
     >> 2
     >> 3

### Cloning Jobs
Particularly useful when used with lazy functions, the ability to run a job
more than once is supported, by simply multiplying it by the number of times
you'd like it to be run.
For example, the example from lazy functions could be simplified to:
     
     var i = 0
     val getI = () => { i += 1; i }
     submit({"echo" ->getI} * 3)

     >> -- 3 jobs submitted
     >> 1
     >> 2
     >> 3

Pitfalls and TODOs
-------------
-  The `|` and `&` operators are overloaded for numbers; thus, entries such as `-('param, 1 | 2)` will resolve the
   numeric operation `1 | 2` and not start two runs with `param=1` and `param=2`.