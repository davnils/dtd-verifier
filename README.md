This project contains an implementation of the following:
* DTD specification lexer and parser
* DTD to Context-Free Grammar conversion
* Simulation of a NDPDA provided the CFG and an input XML file

Basically the task is to validate an XML file, given a DTD specification.
Since DTD turns out to be very close to standard CFG syntax the conversion
can be done relatively easily. Finally a non-deterministic pushdown
automaton is used in order to "simulate" the CFG on an input file.

DTD/XML could be useful for configuration files etc when a specific
structure is needed (and validated).

See the tests folder for examples.
