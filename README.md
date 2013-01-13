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
Create a Scala script with `Qry` imported.
This is unfortunately boilerplate necessary for every script -- I promise there's not much of it!

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
     
     >> 1 job submitted
     >> Hello, world!
     >> 1 job submitted
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
     >> 1 job submitted

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
     >> 2 jobs submitted

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
     >> 3 job submitted
     >> // ping -c 3 -a google.com
     >> // ping -c 4 -a google.com
     >> // ping -c 3 -a facebook.com

Versus:

     #!/bin/bash
     exec scala -cp qry.jar "$0" "$@"
     !#
     import Qry._
    
     submit("ping" -("c", "3" & "4") -"a" ->("google.com" & "facebook.com"))
     >> 3 job submitted
     >> // ping -c 3 -a google.com
     >> // ping -c 4 -a google.com
     >> // ping -c 3 -a facebook.com
     >> // ping -c 4 -a google.com

Pitfalls and TODOs
-------------
-  The `|` and `&` operators are overloaded for numbers; thus, entries such as `-('param, 1 | 2)` will resolve the
   numeric operation `1 | 2` and not start two runs with `param=1` and `param=2`.
