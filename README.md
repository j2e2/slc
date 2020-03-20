# slc
slc, *Switching Logic Circuit*.

Prolog interpreter easing verifying and modeling of *switching logic* and *PLC* programs.


You could get more info at https://j2e2xae.wordpress.com/slc-interprete-prolog/, a blog in spanish language.

# Operators and syntax
slc is a stateful interpreter, the internal state comprises the flags **FC**,*First Consult*, and **RLO**, *Result of Last Operation*.

**FC**, determines when logic chain starts so *RLO* should be loaded.

**RLO**, works as an accumulator.

The operators *( a, an )* are the only *load* enabled operators and should begin a logic chain if *FC* is *false*, from there on, the rest operators in the chain update *RLO*.

The *store* operators and terms, *( =, \\=, s, r )*, resets *FC*, so a new chain will start at the next *( a, an )*.

Flag *RLO* could be assigned with the terms *( set, clr )*.

There are support for parens evaluation on operators *( a, o, x )*.

The values *1/0* mean *true/false*. 

## Basic operators
**( a, an, o, on, =, \\= )**

**a**, **an** : *conjunction* operations.
>Also works as *load* when found at the first operation in a *logic chain*.

**o**, **on** : *or-inclusive* operations.

**=** : *store* operation.

**\\=** : *store*, negated, operation.


## Parens
Parens are based on lists, so our parens are square parens.

**( a[ ], o[ ] )**

**a[ ]** : *and-before-or*.

**o[ ]** : *or-before-and*.


## Additional operators and terms
**( x[ ], xn )**

**x[ ]** : xor operation, could use parens.

**xn** : dual of xor.

**( s, r, p, n )**

**s** : *set* operation.

**r** : *reset* operation.


**p** : *rising edge* evaluation.

**n** : *falling edge* evaluation.

**( set, clr )**

**set** : sets *RLO* flag.

**clr** : clears *RLO* flag.

## Calling prolog terms
**( cal, calc, caln )**

**cal** : unconditional *call*.

**calc** : *call* if *true*.

**caln** : *call* if *false*.

# Examples
To be done.
