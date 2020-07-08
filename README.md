The master branch (current) presents an example to conceive an AXI to CL to AXI verilog file which can be tested by appropriately mapping onto an FPGA's appropriate pins. The ChipLink can be mapped against the FMC connector. This experiment is ongoing.

The dev_example branch presents a bunch of example networks spanning TL-CL, TL-AXI, AXI-CL, etc. 

Specifying the sbt version. Allows people with different versions of the
sbt launcher to build the same projects with consistent results. To do this,
create a file named ```project/build.properties``` that specofies the sbt version as
follows
```sbt.version = 1.3.4```


