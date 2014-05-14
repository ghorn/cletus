pipeline
========

Programs
--------

The code is broken into three programs: `run_sensors`, `run_controller` and
`run_actuators`.  They do roughly what you'd expect from the names:

 - `run_sensors` gathers data from serial or other devices on the system
   and packages them into messages to send to the controller.

 - `run_controller` receives sensor input and processes it, then sends the
   actuator commands it has calculated to `actuators`.

 - `run_actuators` receives actuator commands and sends them to the
   appropriate devices.

Each program is organized as an event loop using the zmq_poll() call.
These loops are configured to poll for output as well; they only try
to send messages when the internal logic has indicated that they have
something to send.  This is done by clearing the `events` mask in the
`zmq_pollitem_t` structure corresponding to that output after a
message is sent and masking it with `ZMQ_POLLOUT` (output) when there
is data to be transmitted.

As noted in the comments, the event loop can easily be extended to
additional inputs or outputs; you probably want to put the handling
logic in a separate function anyway, so once you do that, you can
simply loop over the inputs and outputs and dispatch.  If you find
yourself digging into this a lot, ask me; I have partially prototyped
a higher-level event loop that you could use instead.

ZMQ usage
---------

A minimal but decently high-level ZMQ abstraction library is provided
in `zmq.c` and `zmq.h`.  It only handles two types of socket pair:
`PUSH`/`PULL` and `PUB`/`SUB`.  These have very different
characteristics, but the abstraction library is designed so that you
can pretty much just change the one `setup_zmq_sender()` or
`setup_zmq_receiver()` call to get different properties, without
having to change much else in your code.  The salient properties are
described below.  The ZMQ abstraction library assumes you want to do
blocking I/O for synchronization, because it's designed for event-loop
situations where you are checking beforehand whether there's any I/O
to be done.  If you need non-blocking I/O, I can add it pretty easily.

## `PUSH` and `PULL` sockets

`PUSH` and `PULL` sockets are basically synchronous.  They can be used
to give flow control, but (this took me days to figure out) _only_ if
the queues are configured to be quite short.  If you don't set both
the queue length (in messages) and the buffer size (in bytes) to be
small, ZMQ will happily let lots of messages build up before finally
blocking you, thereby screwing you over.

If you configure both the sender (`PUSH`) and the receiver (`PULL`) to
have short queues and buffers and use them to connect two programs,
then you can effectively synchronize the programs; the source will
push until the tiny queue fills (ideally only one or two messages),
then (by default) block.  Of course, rather than actually blocking, we
use an event loop so that we only transmit when there is room for our
message, allowing us to continue servicing other inputs and outputs
while we wait.

The default application uses `PUSH` and `PULL` sockets for the main
processing pipeline (`sensors` -> `controller` -> `actuators`) to
demonstrate how they can be run "synchronously" while still allowing
the sensors process to read incoming data and decide how best to
handle overflow or timing fault conditions.  I will talk more about
how you can use these in real-time applications later.

Also note that multiple processes can `PUSH` messages to a given
`PULL` socket easily.

## `PUB` and `SUB` sockets

`PUB` and `SUB` sockets are basically like UDP broadcast or LCM.  You
can't do flow control, because publishing can never block; if the
queue overflows, messages are simply dropped.  This can be handy in
real-time applications in two ways.

For logging messages, the default application is configured to use a
huge queue.  The log writer should have a huge queue as well, and then
as long as it can keep up with the throughput, it can stall for a bit
and then catch up later; the queues will hang onto all the messages
unless stuff gets really out of whack.

For the main processing pipeline, you could use `PUB`-`SUB` pairs with
tiny queues to ensure that only fresh data is being passed along.  If
the `actuators` program takes too long to get the commands out or the
`controller` program takes too long to process, then when they finally
get around to reading in the next message, they won't be reading
backlog.

Multiple processes can `SUB`scribe to messages from a single
`PUB`lisher easily.

ZMQ caveats
-----------

ZMQ is not really designed for real-time systems, in the sense of
"hard real-time", but it _is_ very performant and flexible.  Obviously
performance is important and helps meet deadlines, but the flexibility
is nice too -- if we use it right, it allows us to change
communication patterns pretty easily, which means we have a lot of
"engineering margin" to make things work before we need to abandon it
in favor of something more real-time.

ZMQ has queues on both the sender and receiver ends.  This means that
it's actually very tough to get a queue that can only store one
message, but in a real-time system, even storing two messages is a
bummer.  Even more annoyingly, you can't simply ask ZMQ how many
messages are in the queue.  The sender and receiver in a socket pair
run in different processes, and ZMQ sockets are very dynamic, so in
general, there's no guaranteed-efficient way to get this information.

The easiest thing to do is ignore it and see whether it causes
problems.  If your system is architected properly, you shouldn't be
seeing backlog anyway, and ZMQ tends to have pretty low delivery
latency.

There are a few workarounds you can try as well.  The easiest hack is
that you can "double-buffer": when the time comes to get data, read
into alternating receive buffers until you would be unable to read
further, then take the last message you got.  It's a bit inefficient,
but it can help.

If you merge several programs into threads of a single program, but
don't use _any_ shared memory (keeping them totally isolated except
for ZMQ messages), then you can use the `inproc://` transport instead
of `ipc://` or `tcp://`, which I think is a lot more deterministic and
can be used for signalling back and forth without much performance
penalty.  The nice thing about setting everything up with ZMQ and
processes is that if you are hitting performance limits due to
synchronization and context switches, you can pretty easily just merge
into one program with multiple threads without rewriting much and get
some performance and determinism benefits.

Architecture
------------

One hard real-time system I can think of that ran at full system
capacity was the Apollo LEM computer, and they got a lot of error
conditions (1202 program alarm) and spent a lot of time trying to make
the important stuff actually work reliably on the ground.  Most
systems that _need_ hard real-time performance get a big advantage by
simply over-specifying the processor by a large safety factor.  It's
fairly obvious that the less you care about latency and deadlines, the
less safety margin you'll need to achieve the same performance.  The
Gumstix we used at Joby was pretty heavily loaded, and we paid a
penalty for it at times.  If I recall correctly, Arnold made some of
his final flights with a somewhat-degraded 100Hz control loop.

If the system has a pretty decent safety margin, then we'd expect
messages in any ZMQ queue type to not build up in small queues; they
should get passed along without building up anywhere.  Most of the
decisions are about what to do when something fucks up -- do we save
the message and wait for it to come back?  Do we put the message on
the queue and let ZMQ get it there when it gets there?  Do we just
drop it and not send anything until we get another message?

If you decide you need more determinism, especially on a processor
that's doing a lot at once, you can investigate real-time process
scheduling and IO scheduling in the Linux kernel.  PREEMPT_RT is
mostly merged into mainline nowadays, so both of these can be used on
most modern standard kernels.  If you have multiple processors and
cache-sensitive numerics, perhaps you can set the `controller` process
to be bound to one core and let the other core run the I/O code.

Another architectural question that's more specific to us is whether
the estimator is completely synchronous or asynchronous.  If we
receive a sensor message, do we send it along straight away, or do we
wait to bundle it with a complete set of sensor measurements, to be
forwarded as a unit to the estimator?  What happens if we have a
partial set but run past our deadline?  Do we simply drop everything,
or is a complete sensor set only a preferred format (i.e. can we get
something out of getting partial data anyway)?  Does the attitude
filter run at 200Hz and the position filter at 50?  If we're just
going to forward individual measurements, do we even need a `sensors`
process?  Maybe it would be simpler to have the controller event loop
handle all the inputs and just kick off the estimator/controller
pipeline on a timer, with whatever data has arrived.
