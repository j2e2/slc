# slc, jump operations enabled

[*** For now, jumping in forward direction is okay, jumping backward to an assignmet operation shows up as false ***]

This is a version of [**slc**](https://github.com/j2e2/slc) that supports (un)conditional jumps to label.

**( :, jmp, jmpc, jmpn )**

**:** : *label:*, makes a target *label* definition.

**jmp**: *jmp label*, unconditional jump to *label*.

**jmpc**: *jmpc label*, conditional jump, if *true*.

**jmpn**: *jmpn label*, conditional jump, if *false*.

This supports all the operators and terms of the original.

See, [slc/README.md](https://github.com/j2e2/slc/blob/master/README.md)


