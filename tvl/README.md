# tvl
tvl, *Ternary Vector List*.

*Ternay vector list* representation of booleans, utilities and operators.

You could get more info at https://j2e2xae.wordpress.com/slc-interprete-prolog/, a blog in spanish language.

Also see the *XBOOLE* tool at http://www.informatik.tu-freiberg.de/xboole.

# Introduction

A *TVL* is a list of type *term* compounds of dicts.

Represents a conjuction/disjuction of boolean terms, is a boolean equation.

In this library a *TVL* represents a *SOP*, Sum Of Products, if no else stated.

*Don't cares* as unbounded **vars**, *true* as **1** and *false* as **0**.


# Operators and utilities

## Set operators
**( union, intersection, complement, difference, orthogonal )**



## Prolog interface
**( to_fact, from_fact )**



## Boolean equations
**( quotient, absorb, bcf, nearly_minimal )**



## Other
**( on_set, off_set, on_mark, off_mark, interval, minimal_subsets, karnaugh_map )**



# Examples
To be done.
