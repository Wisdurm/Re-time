![Retime logo](retime.svg)

# Re-time

[![Haskell CI](https://github.com/Wisdurm/Re-time/actions/workflows/haskell.yml/badge.svg)](https://github.com/Wisdurm/Re-time/actions/workflows/haskell.yml)

Re-time is a simple, purely object-oriented, Lisp inspired language which is a
redesigned and rewritten version of
[Runtime](https://github.com/Wisdurm/Runtime).

This language is made for fun and is not intended to be used for any serious
projects. It should, however, be capabable of such, although it is not the most
pleasant language to work with.

# Examples

These are functional Retime snippets which you can try out right now in the
interpreter!

```bash
# Create variable test with the value of 1
Set(test 1)

# Function which adds two args
Copy(AddArgs +(x y))

# Callbacks
Set(CallFunc Object(Nil(func)
                    Log(func(test 2))))

CallFunc(AddArgs)
# Prints 3
```

```bash
# Imperative loop
Set(i 0)
While(<(i 10) Object(Set(i +(i 1))
                     Print(i))())

# Recursive loop
Set(Loop Object(Nil(n)
                Set(x +(n 1))
                Print(x)
                If(<(x 10)
                       Loop(x)
                       Nil)))
Loop(0)
```

```bash
# Exponation function
Set(Pow Object(
    Nil(n k)
    If(=(k 1)
            n
        Series( Set(x -(k 1))
                Set(r Pow(n x))
                *(n r)))))
Print(Pow(2 4))
# Prints "16"
```
