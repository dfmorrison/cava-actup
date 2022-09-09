# Initial CAVA ACT-UP integration

> Reverse-engineer what you read.     (Steven Pinker, *Twitter*, 13 January 2019)

This is a first pass at hooking ACT-UP models to CAVA. It makes a lot of bold assumptions about
what will or will not be comfortable on the CAVA side of the communication between these
things, and undoubtedly some negotiation will be required to get to something mutually
agreeable. But this seems a good place to start.

When run this starts a Lisp process that listens for UDP messages (currently on port 9017, though
that can obviously be easily changed), turns the crank of an ACT-UP model, and returns a result as
the response to this UDP message. So far two of Drew’s models from the highlighting demo mock up have
been converted for use here.

The messages read should contain a single JSON object, containing at least the two slots

* `id`

* and `timestamp`

Each of these messages corresponds to a click, at the specified time, on an displayed item
named by the id. For example, `{"id": "C", timestamp: 1660073624"}`.

The return value is also a JSON object, with two slots

* `levels`

* and `timestamp`

The `levels` value is itself a JSON object, mapping `id`s to highlighting levels.
The `timestamp` is the timestamp of the message being replied to.
For example, `{"levels": {"D":4,"C":6,"B":3}, "timestamp": 1660073624}`.

The `id`s should be non-empty strings or integers, though non-empty strings are preferred. Since
only a string may be a key in a JSON object, if an integer `id` is supplied the return value
must use its string representation rather than the integer itself.

Highlighting levels are currently assumed to be integers in the range 0 to 5, inclusive, where
the higher the value to more the model wishes to emphasize it. For any `id`s not mentioned in the
return value it should be assumed their highlighting value is zero, and, in fact, the return
object omits all `id`s that would otherwise be mapped to zero.

The `timestamp`s are Unix times, and may included fractional seconds. It is assumed
that all clicks will be delivered in chronological order, and an error will be signaled
if out of order clicks are received.

To run

* ensure that SBCL is install as by `cava-platform_bootstrap_provision-actup.sh`

* cd to this directory

* and run `./run-cava-actup-server.sh‘

The two ACT-UP models that have been converted are named `recency-model` and `recency-frequency-model`.
By default `run-cava-actup-server.sh` runs `recency-model`. To choose a different
model pass its name as an argument to the script, for example, `./run-cava-actup-server.sh recency-frequency`.

Note that there’s a minor testing and debugging kludge in this run script that checks the user name;
you don’t want to run this as a user named `dfm` or unexpected things may happen.



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



## Test script ##

There’s a simple, *in vitro*, test script here,`test-server.sh`. This in turn call some code
in `test-server.lisp` that reads line delimited JSON from its standard input, sends it
to the server with UDP, reads the responses, and prints them to its standard output.
The file `test-data.json` is some sample data extracted from a real run.

So, for example, to exercise this without actually integrating it into the rest of CAVA,
in one terminal run the server and then in another
run `./test-server.sh < test-data.json`, and inspect the output.



## Next steps ##

Next steps include

* most importantly, working out how best to modify this to integrate comfortably with CAVA

* upgrading the models based on further information from the caller; this may also require
  some further plumbing changes to accommodate reading disparate data from two, different sockets

* there is currently no provision for restarting this, you kill it and launch it again; it
  seems likely that we will want some protocol for restarting it more gracefully

* and possibly adding a bit more flexibility to the script for launching things.



## Adding models ##

(This section is most likely primarily of interest to Drew and Christian.)

In this simple framework a model is a Lisp function of two arguments, an id and a time.
The id is a symbol and the time a real number. This function is called once for reach
message received, and should return an alist mapping ids to highlight values.
The time is the number of seconds since
this run “started,” where it is assumed to have started one second before the first
message was received.

The names of such model functions should be exported from the `cava` package.

ACT-UP memory is initialized before a model function is first called.
While not currently used, there is a provision for setting ACT-UP parameters before
the model function is called, if desired, by passing a plist as the `:model-parameters`
keyword argument to the `cava:run` function. Optimized learning is off by default.

A couple of possible useful utility functions (all in the `cava` package) are included

* `place-into-bins` takes an alist mapping items to real numbers, and an optional number of bins, defaulting to six,
   and returns an alist mapping those same items to the corresponding number integers denoting equispaced bins;
   that is, this abstracts the code that was previously copied and pasted to the end of all the models in the
   highlighting demo mock up.

* `simple-model` abstracts the simplest models which simply compute an activation function of some sort
  on the various chunks, and slot them into bins based on the results.
