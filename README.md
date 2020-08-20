The master branch (current) presents an example to conceive an AXI to CL to AXI verilog file which can be tested by mapping the IOs against some FPGA's appropriate pins. The ChipLink IOs can be mapped against the FMC connector.

The dev_example branch presents a few example networks like TL<->CL, TL<->AXI, AXI<->CL, etc. 

The dma branch has a TL based dma example with 2 TL ports for reading and writing and a register interface for command/control.

//Unmodified
Specifying the sbt version. Allows people with different versions of the
sbt launcher to build the same projects with consistent results. To do this,
create a file named ```project/build.properties``` that specofies the sbt version as
follows
```sbt.version = 1.3.4```


