# rlox

I'm learning Rust by implementing a bytecode vm for the clox language in the book [Crafting Interpreters](https://craftinginterpreters.com/).

WIP: just finished chapter 24 of the book.

```
$ cat fib.lox
fun fib(n) {
  if (n < 2) return n;
  return fib(n - 2) + fib(n - 1);
}

var start = clock();
print fib(35);
print clock() - start;

$ time cargo run --release --quiet fib.lox
9227465
3.0569019317626953
    real        3.12s
    user        3.10s
    sys         0.01s
```

TODO:
- 25: closure
- 26: garbage collection
- 27: classes and instances
- 28: methods and initializers
- 29: superclasses
- 30: optimization

