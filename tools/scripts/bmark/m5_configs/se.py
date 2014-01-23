# Simple test script
#
# "m5 test.py"

import optparse
import sys
import os

import m5
from m5.defines import buildEnv
from m5.objects import *
from m5.util import addToPath, fatal

addToPath('./common')

import Options
import Simulation
import CacheConfig
import MemConfig
from Caches import *

# Get paths we might need.
config_root = os.path.dirname(os.path.abspath(__file__))

parser = optparse.OptionParser()
Options.addCommonOptions(parser)
Options.addSEOptions(parser)

# Benchmark options
parser.add_option("--hasim-sim", action="store_true")
parser.add_option("--shared-mem", action="store_true")

(options, args) = parser.parse_args()

if args:
    print "Error: script doesn't take any positional arguments"
    sys.exit(1)

(CPUClass, test_mem_mode, FutureClass) = Simulation.setCPUClass(options)

np = options.num_cpus

CPUClass.numThreads = np

system = System(cpu = [CPUClass(cpu_id=i) for i in xrange(np)],
                mem_mode = test_mem_mode,
                mem_ranges = [AddrRange(options.mem_size)],
                cache_line_size = options.cacheline_size)


# Create a top-level voltage domain
system.voltage_domain = VoltageDomain(voltage = options.sys_voltage)

# Create a source clock for the system and set the clock period
system.clk_domain = SrcClockDomain(clock =  options.sys_clock,
                                   voltage_domain = system.voltage_domain)

# Create a CPU voltage domain
system.cpu_voltage_domain = VoltageDomain()

# Create a separate clock domain for the CPUs
system.cpu_clk_domain = SrcClockDomain(clock = options.cpu_clock,
                                       voltage_domain =
                                       system.cpu_voltage_domain)

# All cpus belong to a common cpu_clk_domain, therefore running at a common
# frequency.
for cpu in system.cpu:
    cpu.clk_domain = system.cpu_clk_domain

system.membus = CoherentBus()
system.system_port = system.membus.slave
CacheConfig.config_cache(options, system)
MemConfig.config_mem(options, system)

##
## Shared memory programs (e.g. Splash) use the clone emulated syscall to
## map memory shared.  They use a single process instance mapped everywhere.
## Normal (non-shared) programs use separate LiveProcess() instances.
##
if options.shared_mem:
    progdir = "program.0/"

    process = LiveProcess()
    process.executable = progdir + options.cmd
    process.cwd = progdir
    process.cmd = [progdir + options.cmd] + options.options.split()
    if options.input != "":
        process.input = progdir + options.input
    if options.output != "":
        process.output = progdir + options.output
    if options.errout != "":
        process.errout = progdir + options.errout

    for i in xrange(np):
        system.cpu[i].workload = process

else:
    for i in xrange(np):
        progdir = "program." + str(i) + "/"

        process = LiveProcess()
        process.executable = progdir + options.cmd
        process.cwd = progdir
        process.cmd = [progdir + options.cmd] + options.options.split()
        if options.input != "":
            process.input = progdir + options.input
        if options.output != "":
            process.output = progdir + options.output
        if options.errout != "":
            process.errout = progdir + options.errout

        system.cpu[i].workload = process

for i in xrange(np):
    if options.fastmem:
        system.cpu[i].fastmem = True


root = Root(full_system = False, system = system)
Simulation.run(options, root, system, FutureClass)
