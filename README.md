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

You can also start Qry as an interactive prompt, with all dependencies
bundled, using:

    java -jar qry.jar

### Hello World!
On most Unix systems, there is a program entitled `hello` which prints Hello World to the screen.
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
This is done in a way reminiscent of shell scripting: A dash (`-`) precedes arguments which are intended to
be preceded by a dash; an arrow (`->`) precedes arguments which are passed verbatim.
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
For now, let's try to start two jobs: one to ping Google, and one to ping Facebook:

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
Many of the auxiliary features of Qry are provided with the `using()` function.
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
-  `_rerun.sh`: A bash script which can be used to re-run this job, in a newly
   created nested rerun folder.

As mentioned above, one of the key elements in `_qry.json` in this folder are
results scraped from the running program.
It will save lines matching the regexps:

     ^.*result[\s\]]*[:\-\s]\s*(.+)\s*[=:\-]>?\s*([^>].+)\s*(?iu)$
     ^\s*(.+)\s*[=:\-]>?\s*([0-9\.\-e\%\(\)\$]+)\s*(?iu)$

Informally, these are lines of the form:
`result: key = value` or `result: key: value` or `key: value_as_number`; etc.

Lastly, note that you can retrieve this run directory with the `touch` function.
For example, the following command will create a new file in the execution
directory (confusingly, using the Unix touch program):
     
     using("/path/to/rundir/")
     submit("touch" ->touch("filename_to_create.txt"))
     
     >> -- 1 job submitted
     >> // touch /path/to/rundir/filename_to_create.txt

### Properties File
Often, a configuration already exists in a properties file and it is useful
to encode small differences rather than translating the entire file.
In this case, you can specify the file you'd like to import using:
     
     using("/properties/file")

Note that the file must exist, and should not contain a trailing `/`.
Also note that this requires Typesafe Config
(https://github.com/typesafehub/config)
in your classpath at runtime -- all of the heavy lifting happens in that
project.

The format of the properties file can be any of the formats that Typesafe
Config takes, including Java `.properties`, JSON, or HOCON / `.conf`.
See their GitHub page for more details

The options in the properties will be appended directly after the program
name (the first argument to `submit`), and before all other arguments.
Note that you'd like it to be appended at a specific point, the first
portion should be specified as a list of arguments -- for example, by
calling `.toList` on the first group of arguments.

### PBS Integration
Jobs can be run over PBS by specifying the option:

    using("pbs")

Options for the PBS job (arguments to qsub) can be specified by setting one
or more of:

    PBS.name:String
    PBS.queue:String
    PBS.priority:String
    pbs.memory:String
    pbs.cores:Int

Note that memory is autdetected from java processes; also, the Priority and Queue
objects have some common preset priority and queue values.

### Remote Execution
Eventually, the capability of running jobs on remote hosts will be added.
The syntax for this will be:

     using("user@hostname")

This functionality is not yet implemented, but is documented here as a roadmap.

### Redis Integration
Eventually, results will be storable not only in a file in the run folder,
but also in a [potentially remote] Redis server.
This would guarantee better querying of relevant statistics, as well as more
robust concurrency. The syntax for this will be:

     using("redis:hostname")

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

When used in conjunction with PBS integration, the number of cores denote the
maximum number of PBS jobs to run at any given time.
For example, you could start 100 PBS jobs, but ensure that rather than
getting loaded at once only 10 are on the queue at any time, with:

     using("pbs")
     parallel(10) submit( ('echo ->"This is a PBS job!") * 100 )

Advanced Features
-------------
Note that these are more likely to be bug-prone; but, also more likely to
be cool!

### Lazy Function
A rare, but neat feature of the program is the ability to use lazy functions
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

### Power Sets
In certain cases, you may want to set a parameter to the power set of a given
set of options.
For example, if you're doing feature selection, you may want to run a classifier
with every combination of feature templates.
A utility is included for this, conveniently named `powerset`:

    submit("echo" ->powerset("foo", "bar", "baz"))
    
    >> -- 7 jobs submitted

Note that the empty set is not included; and, that if your power set is larger
than 1000 elements, the program will check to confirm that this is what was intended.
Each set is denoted by a comma-separated list; e.g., "foo,bar,baz".

Also note that by default, the powerset function has the semantics of
joining all the arguments with `&`.
For joining arguments with `|`, see `powersetOr` (for convenience, there
is also a function `powersetAnd`).

Query Results
-------------
Of course, managing experiments would not be particularly useful if there
weren't an easy way to retrieve and analyze these results.
Qry provides a simple command-line interface for browsing results; this is
particularly useful when a large number of jobs are started and need to be
digested.

### Basic Usage
The command line interface can be started with:

    java -jar qry.jar path/to/exec/dir/

Note that this will start up a full-blown Scala interpreter -- any valid Scala
code is valid here!
However, by default, a small SQL-like domain specific language is loaded.
Like SQL, the fundamental operations you can perform are `projection` and
`selection`. Projection allows you to display only a subset of the keys for
inputs and results; selection allows you to select a subset of the runs, based
on certain criteria.

For example, the following will display only `key1` and `key2` where
`accuracy` is greater than 0.7:

    select ('key1, 'key2) where 'accuracy > 0.7

Wildcards are also supported for projection
    
    select (*) where 'accuracy > 0.7

or

     * where 'accuracy > 0.7

Additionally, predicates such as `max` and `min` can be used (diverging somewhat
from SQL syntax):

    select (*) where 'accuracy.maximized

This is equivalent to saying the following:

    select (*) where maximized('accuracy)

### Useful Utilities
The goal of the language is to be as forgiving as possible; thus, a number of
shortcuts and utilities are provided:

-  `maximized` can be replaced with `max`, `maximize`, or `maximum`; the same
   holds for `minimized`.

-  `keys` will show all the keys that are not constant across the runs.

-  `key(fragments*)` will return a single key that matches the provided
   string fragments. For example, the following might select the key for
   accuracy:

   `select (*) where maximized(key("acc"))`

-  `get(selection)` will return the runs matching a predicate.
   For example, the following will get the highest accuracy run:

   `get( max(key("acc")) )`

Pitfalls and TODOs
-------------
-  The `|` and `&` operators are overloaded for numbers; thus, entries such as `-('param, 1 | 2)` will resolve the
   numeric operation `1 | 2` and not start two runs with `param=1` and `param=2`.

-  The interactive mode will eat your terminal; you must `reset` the terminal
   after exiting, or start the command as;

     java -Djline.shutdownhook=true -jar ./qry.jar
