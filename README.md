# StackulatorNEXT

StackulatorNEXT is a functionnal, stacked-based and minimalistic programming language.

It's the spiritual descendant of [Stackulator II](https://github.com/316k/stackulator-ii).

## Example code

```
5 10 / pp
# prints 1/2

250 -> a
a 10 / -> a
a pp
# prints 25

# functions

(dup *) -> squared
5 squared . pp
# prints 25

(dup 1 > if (dup 1 - factorial . *)) -> factorial

6 factorial . pp
# Outputs 720
```
