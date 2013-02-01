# Simple test script
#
# "m5 test.py"

import os
import optparse
import sys
from os.path import join as joinpath

import m5
from m5.defines import buildEnv
from m5.objects import *
from m5.util import addToPath, fatal

addToPath('./common')

import Options
import Simulation
import CacheConfig
from Caches import *

# Get paths we might need.
config_root = os.path.dirname(os.path.abspath(__file__))

parser = optparse.OptionParser()
Options.addCommonOptions(parser)
Options.addSEOptions(parser)

# Benchmark options
parser.add_option("--hasim-sim", action="store_true")
parser.add_option("--physmem", default="512MB", help="Physical memory")
parser.add_option("--shared-mem", action="store_true")

(options, args) = parser.parse_args()

if args:
    print "Error: script doesn't take any positional arguments"
    sys.exit(1)

(CPUClass, test_mem_mode, FutureClass) = Simulation.setCPUClass(options)

np = options.num_cpus

CPUClass.clock = '2GHz'
CPUClass.numThreads = np

system = System(cpu = [CPUClass(cpu_id=i) for i in xrange(np)],
                physmem = SimpleMemory(range=AddrRange(options.physmem)),
                membus = CoherentBus(), mem_mode = test_mem_mode)

system.system_port = system.membus.slave
system.physmem.port = system.membus.master
CacheConfig.config_cache(options, system)

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
