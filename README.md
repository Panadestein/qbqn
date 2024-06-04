# QBQN

A minimal quantum interpreter written in [BQN](https://mlochbaum.github.io/BQN/), and tested with a circuit
implementing the quantum subroutine of Shor's algorithm for the number 15.

See the papers linked in the web page for details on the implementation.
This project was inspired by the excellent tutorial from Robert Smith,
which can be found [here](https://www.stylewarning.com/posts/quantum-interpreter/). My follow-along version (also in Common Lisp but with additional tests and some extra explanations)
can be found [here](https://github.com/Panadestein/quant_clq).

## Overview

The interpreter's idea is simple: evolve the wave function $\Psi_0^n$ in the full
Hilbert space, that means $2^n \times 2^n$ if we use $n$ qubits. Then construct
the full gates by lifting:

$L_U = I \otimes \cdots U \cdots \otimes I$

and evolve it:

$$\Psi_m^n = \Psi_0^n \prod_i^m L_{U_i}$$

The measurement is done by sampling the CDF of the squared amplitudes of the states.
