# Parser
A minimal test of Haskell performance for the 1 billion rows challenge.
This code doesn't try to achieve very high speed, but rather to investigate if a Haskell program can achieve decent execution performance straight out of the box.

Chris Penner's [Beating C With 80 Lines Of Haskell: Wc](https://chrispenner.ca/posts/wc) was used as the inspiration for fast code, ie: 

- removing lazy evaluation when it's not necessary,
- inlining functions, and
- unboxing structures.  

And then breaking down processing over long arrays on a per-CPU core basis.

The **wc** utility serves as the benchmark to see how the Haskell program is doing. Happy to report that when running on both MacOS M1 cpu and Linux AMD & Intel CPUs, the Haskell program is ~4-5x slower than **wc**. Importantly it runs in constant memory space. Given it's written without any special optimization tricks but for the 4 considerations mentioned before, using standard dictionary (`Data.Map`), text streaming (`BasicString.Lazy`), and structure concatenation (`Semigroup` instancing).

