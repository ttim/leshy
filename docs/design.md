# Function calls, subroutines, labels
TBD

# Constants, bytes, symbols
We use symbols for jumps and function calls, symbols take only 4 bytes on stack and much easier to manipulate with because of it
See inlining section to get more insights.
TBD

# Inlining
One of guiding principles inlining should be possible and describable.
If all labels were constant then it wasn't too hard to inline function (just copy content into new one, and change labels / jumps if they clash)
But having labels to be only literals, and not const kinda feels wrong and incomplete.
tbd: how do inline stack references?! ability to have both forward and backward is clearly shitty for this reason.

# To const or not to const
Currently state of 'constantness' at any particular line means code is getting specialized to this values.

But here is a catch - since the only way to get *initial* values out of constants in fact everything in code ends up being marked as const, apart from supplied non const input data.

I've discovered that firstly when working on `fib` program - since in leaf scenario it returns `1` (*const* `1`) and later it sums consts up together it was keep returning *const* result.

This clearly causes specialization nightmare and can't be correct. So to fix that I've explicitly marked return `1` as non const (via `not_specialize` inst).

This (explicit mark of return values from function as `not_specialize`) might be good solution for most cases. Basically return values are almost never needs to be specialized.

However it breaks with `zig`s `comptime` system - in zig you can call regular `fib` from `comptime` context. With explicit `not_specialize` call above this fib function will never get specialized.

Seems like correct solution to this problem is to propogate `is_comptime` parameter in function calls which will be enabled by `comptime {}` in 'zig'.
If this parameter to function set to true we don't remark output with `not_specialize` at the end.
 