# slc
slc, Switching Logic Circuit.

Prolog interpreter easing verifying and modeling of *switching logic* and *PLC* programs.


You could get more info at https://j2e2xae.wordpress.com/slc-interprete-prolog/, a blog in spanish language.

# Operators and syntax
To be done.

## Basic operators
( a, an, o, on, = )

**a**, **an** : *Logical conjunction* operations. Also works as load operation when found as the first operation in a logical chain.

**o**, **on** : *Logical or-inclusive* operations.

**=** : *store* operation.

To be done.

## Additional operators and terms
( x, xn, s, r, p, n )

**x**, **xn** : *logical or-exclusive* operations.


**s/2**, **s/1** : *set* operation.

**r/2**. **r/1** : *reset* operation.


**p/2**, **p/1** : *rising edge* evaluation.

**n/2**, **n/1** : *falling edge* evaluation.

To be done.

## Parens
Parens are based on lists, so our parens are square parens.

( a[], o[] )

**a[]** : *or-before-and*.

**o[]** : *and-before-or*.

To be done.

## Calling prolog terms
( cal, calc, caln )

**cal** : unconditional *call*.

**calc** : *call* if *true*.

**calln** : *call* if *false*.

# Examples
To be done.
