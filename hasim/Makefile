BSC?=bsc

FLAGS= -u -v -steps 100000 +RTS -K1000M -RTS -keep-fires -aggressive-conditions -no-opt-bool -no-opt-ATS -dschedule  

BSs=
BSVs=Datatypes.bsv DMem.bsv BypassUnit.bsv ValueVector.bsv FunctionalPartition.bsv TimingPartition.bsv Testbench.bsv
BIs=$(BSs:.bs=.bi) $(BSVs:.bsv=.bi)

## Multiple Verilog simulator support.
VERILOGLIBS = -y $(BLUESPECDIR)/Verilog -y . 
VERIMAIN = $(BLUESPECDIR)/Verilog/main.v 

## Default simulator is iverilog

## Verilog compile command line ending is file to generate
VCOMP    = iverilog -Wall $(VERILOGLIBS) 
## Verilog compile command line to specific model called by main
VCOMPTop = -DTOP=
## Prefix to run result of Verilog comiple
VSCR     = ./

ifdef vcsi
    VCOMP    = vcsi  +v2k +libext+.v $(VERILOGLIBS) $(VERIMAIN) -o 
    VCOMPTop = +define+TOP=
endif

ifdef vcs
    VCOMP    = vcs  +v2k +libext+.v $(VERILOGLIBS) +define+TOP=testBranch_V
    VCOMPTop = +define+TOP=
endif

ifdef ncverilog
    VCOMP = ncverilog -c  +access+r  +libext+.v $(VERILOGLIBS) $(VERIMAIN) +name+
    VCOMPTop = +define+TOP=
    VSCR   = ncverilog -R +name+
endif

ifdef ncsim
    VCOMP = ncsim -c  +access+r  +libext+.v $(VERILOGLIBS) $(VERIMAIN) +name+
    VCOMPTop = +define+TOP=
    VSCR   = ncsim -R +name+
endif

ifdef cver
     VCOMP = cvsr +libext+.v  $(VERILOGLIBS) -o 
     VCOMPTop = +define+TOP=
endif
.SUFFIXES: .bs .bsv .bi .v

.bs.bi:
	 $(BSC) $(FLAGS) $<
.bsv.bi:
	 $(BSC) $(FLAGS) $<

all:
	 $(BSC) -u -verilog $(FLAGS) Testbench.bsv 

Mem.bi: Datatypes.bi
BypassUnit.bi: Datatypes.bi
FunctionalPartition.bi: BypassUnit.bi Mem.bi Datatypes.bi
TimingPartition.bi: FunctionalPartition.bi Datatypes.bi
Testbench.bi: Datatypes.bi Mem.bi BypassUnit.bi FunctionalPartition.bi TimingPartition.bi
 
fp:
	 $(BSC) -u -verilog $(FLAGS)  FunctionalPartition.bsv 
tp:
	 $(BSC) -u -verilog $(FLAGS)  TimingPartition.bsv
test%: Datatypes.bi FunctionalPartition.bi TimingPartition.bi
	 $(BSC) -verilog $(FLAGS) -g $@ Testbench.bsv
	 $(VCOMP) -DTOP=$@ -o $@ $(VERIMAIN) *.v
	 ./$@ +bscvcd | tee logfile

# compile and elaborate with Verilog simulator 
mk%_v: mk%.v mk%.v 
	$(VCOMP)$@ $(VCOMPTop)mk$* 

# produce a VCD file from Verilog simulator
mk%.vcd: mk%_v
	$(VSCR)$< +bscvcd | tee logfile

.PHONY: clean
clean:
	rm -f *.bi *.bo mk*.c mk*.h mk*.o mk*.v test* *.vcd *~ *.fsdb *.log a.out
	rm -rf csrc INCA_libs simv.daidir 
