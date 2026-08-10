# Re-time

Re-time is a simple, Lisp inspired language which is a redesigned and rewritten
version of [Runtime](https://github.com/Wisdurm/Runtime).

# TODO

This is heavily work in progress. As of writing this, I don't yet know how
monads work in Haskell as I've only started learning it a couple of weeks ago.
This means, due to the inpure nature of this language, that it can be expected
that even in a best case scenario this will not get finished very soon. I hope
to spend some of that time on also more thouroughly planning out the language,
as many oversights are what lead to Runtime ultimately starting to become a bit
too annoying to maintain.

# Snippets

Just to be clear, none of these currently work in Re-time. Some of these **may**
work in Runtime, albeit with slight modifications.


```bash
# Create variable test with the value of 1
Object(test 1)

# Function which adds two args
Object(AddArgs
	Add(arg1 arg2))

# Callbacks
Object(CallFunc
	Print(Format("Function result: $" 
		func(test 2))))
		
CallFunc(AddArgs)
# Prints "Function result: 3"
```

```bash
# Exponation function

Object(Power
  If(=(k 1)
    n
  *(n Power(n -(k 1)))))
  
Print(Power(2 4))
# Prints "16"
```
