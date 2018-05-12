# MIPS

MIPS(**M**icroprocessor without **i**nterlocked **p**ipeline **s**tages) processor implementation in VHDL.

## Description

### Single Cycle Implementation

In the [MIPS single cycle implementation](https://github.com/rmohashi/Mips/blob/master/mipssingle.vhd), all instructions complete in exactly one cycle(and just one instruction complete in one cycle).

### Pipeline Implementation

With [MIPS pipeline](https://github.com/rmohashi/Mips/blob/master/mipspipeline.vhd), the processor is divided in isolated pipeline stages. Each one of that stages can perform simultaneously, and doing that, multiple instructions run at the same time, in different stages.

## Authors

* Rodrigo Masaru Ohashi - [rmohashi](https://github.com/rmohashi)
* Bruno Eidi Nishimoto - [brunonishimoto](https://github.com/brunonishimoto)
* Matheus Felix Dias Lima da Silva - [matheusssf](https://github.com/matheusssf)

## References

* [Fundamentals of Computer Systems - A Pipelined MIPS Processor](http://www.cs.columbia.edu/~sedwards/classes/2012/3827-spring/mips-pipeline.pdf)
