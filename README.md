# slc
slc, *Switching Logic Circuit*.

Prolog interpreter easing verifying and modeling of *switching logic* and *PLC* programs.


You could get more info at https://j2e2xae.wordpress.com/slc-interprete-prolog/, a blog in spanish language.

# Operators and syntax
slc is a stateful interpreter, the internal state comprises the flags **FC**,*First Consult*, and **RLO**, *Result of Last Operation*.

**FC**, determines when logic chain starts so *RLO* should be loaded.

**RLO**, works as an accumulator.

The operators *( a, an )* are the only *load* enabled operators and should begin a logic chain if *FC* is *false*, from there on, the rest operators in the chain update *RLO* with it's results.

The *store* operators and terms, *( =, s, r, p, n )*, resets *FC*, so a new chain will start at the next *( a, an )*.

The values *1/0* mean *true/false*. 

## Basic operators
**( a, an, o, on, = )**

**a**, **an** : *conjunction* operations.
>Also works as load operation when found as the first operation in a *logic chain*.

**o**, **on** : *or-inclusive* operations.

**=** : *store* operation.


## Additional operators and terms
**( s, r, p, n )**

**s/2**, **s/1** : *set* operation.

**r/2**. **r/1** : *reset* operation.


**p/2**, **p/1** : *rising edge* evaluation.

**n/2**, **n/1** : *falling edge* evaluation.


## Parens
Parens are based on lists, so our parens are square parens.

**( a[ ], o[ ] )**

**a[ ]** : *or-before-and*.

**o[ ]** : *and-before-or*.


## Calling prolog terms
**( cal, calc, caln )**

**cal** : unconditional *call*.

**calc** : *call* if *true*.

**calln** : *call* if *false*.

# Examples
To be done.
