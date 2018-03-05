# esofo

Easy simple_one_for_one - boilerplate for when you need to manage many simple
processes, but don't want to have to implement the same babysitting code every
time. By using esofo you don't have to implement any of these:

* simple_one_for_one supervisor
* hibernate your process after a specified period of inactivity
* terminate your process after a specified period of inactivity
* defense against spawning too many processes. If you do that, the system will start killing the processes which were not active for the longest time.


## esofo_gen_server

Esofo_gen_server is a thin wrapper around OTP gen_server.
In order to use it, just implement a regular gen_server. The only two differences are how you start it and you need to modify your init to accept a specific argument.

First start an esofo simple_one_for_one supervisor like this:
```
> {ok, _} = esofo:start_sofo_sup(your_module).
{ok,<0.787.0>}
```

Then you can start workers like this:
```
> esofo_gen_server:start({your_module, ID},
                         #{a => "A", b => "B"},
                         #{hibernate => timer:seconds(10),
                           shutdown => timer:minutes(30)}).
{ok, <0.789.0>}
```

Here the first argument is a tuple `{your_module, ID}` where ID is an arbitrary term supported by `global`, `gproc` and other registries which comply to global's API. The second argument is a map of arguments, which are provided to your module's `init/1` function when it's started. The third argument is the `esofo_gen_server` options which tell `esofo_gen_server` after what period of inactivity to hibernate or shutdown your process. Defaults are `0` for hibernation (the process will be suspended after every message) and `infinity` for shutdown which means that the framework will never shutdown your process, unless you spawn too many of them (read about `esofo_sentry` below)

While designing `esofo` I tried to make sure that it's as close to a drop-in replacement for gen_server as possible, so you can retain the `-behaviour(gen_server)` at the top of your module. There are only two differences in the implementation.

1. Your init function has to comply to this spec:
```erlang
-spec init(#{id := ID::term(), args = Args::map()}).
```
The `id` in the map is the one you provided when starting the process and the `args` are the ones you provide as a second argument

2. You can also implement a callback function `esofo_registry` so that we register the workers in it. If not implemented, `global` is chosen.
```erlang
-export([esofo_registry/0]).

esofo_registry() ->
    gproc.
```
You can choose any registry which supports the `global` API (gproc does).

After starting the gen_server, you can find it or call it in either these ways (both are equally valid):

```
> esofo_gen_server:find({your_module, 1}).
{ok,<0.789.0>}

> esofo_gen_server:call({your_module, 1}, hi).
go_away

> gen_server:call(Pid, hi).
go_away
```

The same works for `cast/2` and `info/2`

Your implementation has to comply to the `gen_server` documentation. Eg. you can return `{stop, Reason, Reply, State}` from `handle_call/3` and it will work as expected.



## esofo_sentry

Esofo_sentry is a process that constantly monitors the count of all processes spawned in the system and will start killing `esofo` processes  if the total process count is more than 90% of the system limit. You can view the limit via `erlang:system_info(process_limit)`.
It will do so gracefully, and will first kill the ones with the oldest `last_activity` timestamp (stored in ETS). This means that your `terminate/2` will be called and you can do the required cleanup.
