# CAVA ACT-UP integration

> Reverse-engineer what you read.     (Steven Pinker, *Twitter*, 13 January 2019)

This is our current effort at hooking ACT-UP models to CAVA.

When run this starts a Lisp process that listens for UDP messages (currently on port 9017, though
that can obviously be easily changed), turns the cranks of a pair of ACT-UP models, and returns their results as
the response to this UDP message.

The messages read should contain a single JSON object, containing at least the two slots

* `id`

* and `timestamp`

Each of these messages corresponds to a click, at the specified time, on an displayed item
named by the id. For example, `{"id": "C", timestamp: 1660073624"}`.

The return value is also a JSON object, with three slots

* `past`

* `future`

* and `timestamp`

The `past` and `future` values are themselves JSON objects, mapping `id`s to highlighting levels.
The `timestamp` is the timestamp of the message being replied to.
For example, `{"past": {"D":4,"C":3}, "future": {"C":5,"B":3}, "timestamp": 1660073624}`.
Note that the same `id` can be duplicated between `past` and `future`, and it is currently
the Ghidra of of thing’s responsibility to figure out what to do in that case, typically favoring
`future` over `past`. If this is a problem, we can easily force this preference on the Lisp side,
if preferred.

The `id`s should be non-empty strings or integers, though non-empty strings are preferred. In addition,
the string `"nil"` should not be used. Since
only a string may be a key in a JSON object, if an integer `id` is supplied the return value
must use its string representation rather than the integer itself.

Highlighting levels are currently assumed to be integers in the range 0 to 5, inclusive, where
the higher the value the more the model wishes to emphasize it. For any `id`s not mentioned in the
return value it should be assumed their highlighting value is zero, and, in fact, the return
object omits all `id`s that would otherwise be mapped to zero.

The `timestamp`s are Unix times, and may included fractional seconds. It is assumed
that all clicks will be delivered in chronological order, and an error will be signaled
if out of order clicks are received.

To run

* ensure that SBCL is install as by `cava-platform_bootstrap_provision-actup.sh`

* cd to this directory

* and run `./run-cava-actup-server.sh‘

Note that there’s a minor testing and debugging kludge in this run script that checks the user name;
you don’t want to run this as a user named `dfm` or unexpected things may happen.


## Init file ##

If the file `initial-data-lisp`, in this directory, exists it is read. It should contains a single Lisp form,
and list of chunk descriptions that will be used to prime ACT-UP memory. If the file does not exist, a
warning is printed, but everything should proceed smoothly; its use is optional.

Note that an appropriate init file needs to be constructed for each specific task things are operating
upon, it depends upon the underlying graph of things to be highlighted and their IDs. Drew is currently
the doyen of init files, and should be consulted for an appropriate one for a given task.

There are a pair of init files available in the sub-directory `init-files/`. These can be copied, or probably better,
symlinked, to `initial-data-lisp`. Note that `initial-data-lisp` is included in the `.gitignorre` file so things
can be linked or copied there without worrying about clobbering other people’s choices in the repo.

The two existing init files are intended for use with the POI task 1.


## Environment variables ##

There are a couple of parameters that can be set by environment variables. The are

* `CAVA_PAST_MAX_HIGHLIGHTS`, which defaults to `5`

and

* `CAVA_FUTURE_MAX_HIGHLIGHTS`, which defaults to `3`

These are the maximum number of highlighted things shown for the two prongs of the model. If these
variables are exported from the shell from which the application is launched before launching it,
the supplied values, which should be positive integers, will override the defaults.



## Errors ##

If an error is detected when handling a message instead of the above result a different
JSON object will be returned, containing four slots, all with string values:

* `error`: the Common Lisp name of the error’s type

* `message` the JSON message that was being processed when the error was detected

* `description`: the text the Common Lisp listener would have displayed when reporting the error interactively

* `backtrace`: a backtrace from the point the error was detected; this is typically a longish string containing line feeds



## Logging ##

A log of the actions taken by the model is written. By default this is in a file
called `cava-data-<datetime>.json`, in the directory containing the server source, and where `<datetime>`
is the date and time the server was started, but this can be changed by setting the
environment variable `CAVA_ACTR_LOGFILE`.

The contents of this file are JSON objects, one per line, each corresponding to an
interaction with the mdoel. Each object contains five slots:

* `when` is the date and time the model ran, in ISO 8601 format,

* `unix-time` is the same as `when` but as a Unix time,

* `remote` is host from and to which the exchange took place,

* `message` is the JSON message received from `remote`,

* `response` is the JSON message sent back to `remote`.

* `past-activations` contains the chunk activations from running the past model

* `future-activations` contains the chunk activations from running the future model


## Test script ##

There’s a simple, *in vitro*, test script here,`test-server.sh`. This in turn calls some code
in `test-server.lisp` that reads line delimited JSON from its standard input, sends it
to the server with UDP, reads the responses, and prints them to its standard output.
The file `test-data.json` is some sample data extracted from a real run.

So, for example, to exercise this without actually integrating it into the rest of CAVA,
in one terminal run the server and then in another
run `./test-server.sh < test-data.json`, and inspect the output.
