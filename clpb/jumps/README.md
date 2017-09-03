# slc, jump operations enabled
This is a version of [**slc**](https://github.com/j2e2/slc) that supports (un)conditional jumps to label.

This variant runs on the library [**clpb**](http://www.swi-prolog.org/pldoc/man?section=clpb).

**( :, jmp, jmpc, jmpn )**

**:** : *label:*, makes a target *label* definition.

**jmp**: *jmp label*, unconditional jump to *label*.

**jmpc**: *jmpc label*, conditional jump, if *true*.

**jmpn**: *jmpn label*, conditional jump, if *false*.

This supports all the operators and terms of the original, non constrained.

See, [slc/README.md](https://github.com/j2e2/slc/blob/master/README.md)

Notes:
   
   clpb, *Debian 9* ships with an outdated version of this library, so I'm using a local copy instead of the one shipped with the package *SWI-Prolog*.
