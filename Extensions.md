# Extensions to Turtle Graphics

Quite a few extensions to the turtle programming language can be thought of
(although none of them are implemented as of now).

## Multi-language support

See Todo.md for details.

## Easier parameters

Add the ability to define global variables that are automatically
filled with command-line arguments or default values.

Syntax:
```
param <identifier> = <expr>
```

Example:
```
param start_length = 5
param min_length = 0.1
```

The given `<expr>` should only be evaluated if no fitting argument is provided.
That is, each `param ...` directive is strictly equivalent to the following block
inserted at the start of the main block, where `<idx>` is determined by the order
of `param`s.
```
if @<idx> <> 0 then
  store @<idx> in @<idx>
else
  store <expr> in @<name>
endif
```

## Multithreading

Parallel turtle programs can be achieved with the introduction
of the `split` statement. The syntax is identical to the
`path` statement. However, the currently executing turtle is cloned
and the clone is executing the path, with the original turtle
continuing the program after the `split` statement. The cloned
turtle terminates once the called path finishes.

To be decided is the question of synchronizing multiple turtles. While real
threads operate independent of each other, this doesn't seem useful in an
educational language used to teach kids. What comes to mind is a
sequential execution of each turtle until the next line is drawn. Due to
the nature of `@delay`, this should be very similar to the result of using
real threads. Alternatively, a synchronization can follow after each
`walk`, `jump`, `turn` or `direction` statement (each statement that
"moves" the turtle). This however re-introduces the problem of how to
handle `@delay`, so the first approach seems more favorable. To allow
for a more clear synchronization, a statement `wait` can be introduced.
It is considered to draw a line for synchronization purposes, but has
absolutely no effect on the turtle.

## User Input

### Kinds

* Mouse clicks. They provide x- and y-coordinates in the turtle's
  coordinate system. A boolean (if present) could be used to distinguish
  between left and right mouse button.
* Key presses. This not only requires implementation of the debug protocol,
  but would also benefit from having a string type. Typed characters seem far
  more suited than actual key presses / releases.

### Eventing

Since user input usually is event-based, a fitting event system has
to be introduced to turtle. Combined with the multi-threaded approach
from above, spawning a new turtle (with initial state) for each event
should be easy to implement. To define an event handler, the new keyword
`event` can be used to define the two specific functions:

```
event key(char: string)
  " do stuff
endevent

event mouse(x: num, y: num, left: bool)
  " do stuff
endevent
```

Arguments that are not required may be omitted, but no new arguments
(identified by name) may be added. Types are given for clarification here.
If they are enabled, they have to be present.

## Data types

Currently, all Variables and expressions are of type float. To allow for
simpler programs, additional types can be introduced. A strict type system
should be enforced, meaning all types are known at compile time and do not
change. The current float type will be referenced with the keyword `num`.

### Syntax changes

Since variables are declared on use, only two syntax changes are required.
First is casting between types. This is easiest understood with a syntax
of `<type>(<value>)`, i.e. `num(some_string)` or `string(some_num * 2)`.
Second are function parameters. For input parameters, the types are to be
given after the name with a colon in between (see example for event
handlers above). For the return parameter on calculations, the type is to
be given after the closing parenthesis surrounding the argument list, like
`calculation kube_root(x: num): num`.

### Strings

The String type is called `string` and represents an immutable sequence
of valid UTF-8 codepoints. A new string is created for each modification.
String literals are enclosed in single quotes, as double quotes are already
used for comments.

Strings can be concatenated with the `+` operator or the
`append <value> to <variable>` statement. A set of methods for common
string operations should also be provided as some sort of standard library.
Finally, the statement `print <expr>` will print a string to stdout.

### Booleans

Booleans are called `bool` and are either `true` or `false`. With the
introduction of booleans, conditions are now a subset of expressions and
control statements accept boolean expressions. No new statements have to be
introduced.