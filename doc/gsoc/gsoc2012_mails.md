Hello,

I am Uvarov Michael. 
I am studding at Southern Federal University in Russia from 2008.
My github profile is here: https://github.com/freeakk
I want to implement a binding as an Erlang driver. I already have an 
experience with NIFs (Native Implemented Functions) 
(Code is here https://github.com/freeakk/i18n).

I implemented basic operations with Unicode both in pure Erlang and as NIF.
I think, that Erlang will be a good choice for binding, because it has a nice 
inter-node communication interface. This interface can be used as a transport 
for access to a remote database.

I have few questions and ideas about Xapian:

First of all, how does Xapian handle concurrent access to shared resources?
How many connections can be established to one database for reading and writing?
Who does coordinate parallel access to same database?
Are opening and closing a database expensive operations or not?
Is a minimal example of using Xapian with a real data set? 
Are there datasets for an efficiently testing?

I think it is a good idea for implementing this binding as a linked-in driver.
A driver provides a queue and Xapian classes are not thread-safe. So, it will 
be a mapping: one driver port -- one opening connection to a base.
NIFs will require an implementing of a thread pool in C, but it is not a trivial 
task.
Calls of NIFs can block a schedule of an virtual machine. It is also not good,
because other green processes will wait. 

I will wait for answers and your questions.


Thank you for the answers :)
The idea about Erlang is that it has few interfaces with C/C++:
- driver: 
    for slow operations, it is a program which waits requests throw stdio.
    It is a safest variant, for example, for image manipulating.
    If the C process crash, Erlang is still alive.
- linked-in driver:
    The same, but a driver is a dll. If it crashed, one Erlang node crashed 
    also. But it is faster.
    Use case: RDBMS drivers.
- NIF:
    If we run "module:fun(4)" from Erlang, we will call a C function with 
    TERM[1] array as argument. Is is simple, because we don't need encoding and 
    decoding data.
    It is faster because it use the same VM thread. Erlang uses the next 
    schema: one core of CPU - one thread for a schedule. When we call a NIF,
    it cannot be stopped. It will stop the world, the same behaviour in JVM 
    with GC.
    A NIF can provide resources. Resources are objects which can be automatically
    garbage collecting. Resources must be thread safe, because they can be shared
    between OS threads.
    Use case: ICU NIFs.


