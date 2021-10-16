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
idea: maybe that's a reason why it should be more real stack machine. Stack is more composable!
idea: is it possible to make stack references to be only backward going? will it work that way?
idea: make indirect reference based from the value in cell!
