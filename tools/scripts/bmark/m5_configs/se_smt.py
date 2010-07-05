# Simple test script
#
# "m5 test.py"

import m5
from m5.objects import *
import os, optparse, sys
m5.AddToPath('./common')
import Simulation
from Caches import *

# Get paths we might need.
config_root = os.path.dirname(os.path.abspath(__file__))

parser = optparse.OptionParser()

# Benchmark options
parser.add_option("--hasim-sim", action="store_true")
parser.add_option("--physmem", default="512MB", help="Physical memory")
parser.add_option("-c", "--cmd",
                  default="",
                  help="The binary to run in syscall emulation mode.")
parser.add_option("-o", "--options", default="",
                  help="The options to pass to the binary, use \" \" around the entire\
                        string.")
parser.add_option("-i", "--input", default="",
                  help="Read stdin from a file.")
parser.add_option("--output", default="",
                  help="Redirect stdout to a file.")
parser.add_option("--errout", default="",
                  help="Redirect stdout to a file.")
parser.add_option("-S", "--simpoint", action="store_true", default=False,
                   help="""Use workload simpoints as an instruction offset for
--checkpoint-restore or --take-checkpoint.""")


execfile(os.path.join(config_root, "common", "Options.py"))

(options, args) = parser.parse_args()

if args:
    print "Error: script doesn't take any positional arguments"
    sys.exit(1)

if options.detailed:
    #check for SMT workload
    workloads = options.cmd.split(';')
    if len(workloads) > 1:
        process = []
        smt_idx = 0
        inputs = []
        outputs = []
        errouts = []

        if options.input != "":
            inputs = options.input.split(';')
        if options.output != "":
            outputs = options.output.split(';')
        if options.errout != "":
            errouts = options.errout.split(';')

        for wrkld in workloads:
            smt_process = LiveProcess()
            smt_process.executable = wrkld
            smt_process.cmd = wrkld + " " + options.options
            if inputs and inputs[smt_idx]:
                smt_process.input = inputs[smt_idx]
            if outputs and outputs[smt_idx]:
                smt_process.output = outputs[smt_idx]
            if errouts and errouts[smt_idx]:
                smt_process.errout = errouts[smt_idx]
            process += [smt_process, ]
            smt_idx += 1

(CPUClass, test_mem_mode, FutureClass) = Simulation.setCPUClass(options)

CPUClass.clock = '2GHz'

np = options.num_cpus

system = System(cpu = [CPUClass(cpu_id=i) for i in xrange(np)],
                physmem = PhysicalMemory(range=AddrRange(options.physmem)),
                membus = Bus(), mem_mode = test_mem_mode)

system.physmem.port = system.membus.port

root = Root(system = system)

if options.fastmem:
    system.cpu[0].physmem_port = system.physmem.port

progdir = "program.0/";

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

root.workload = process

for i in xrange(np):
    system.cpu[i].connectMemPorts(system.membus)
    system.cpu[i].workload = root.workload



Simulation.run(options, root, system, FutureClass)