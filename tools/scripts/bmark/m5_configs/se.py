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
parser.add_option("-c", "--cmd",
                  default="",
                  help="The binary to run in syscall emulation mode.")
parser.add_option("-o", "--options", default="",
                  help="The options to pass to the binary, use \" \" around the entire\
                        string.")
parser.add_option("-i", "--input", default="",
                  help="A file of input to give to the binary.")
parser.add_option("--bench", action="store", type="string", default=None,
                  help="base names for --take-checkpoint and --checkpoint-restore")
parser.add_option("-S", "--simpoint", action="store_true", default=False,
                   help="""Use workload simpoints as an instruction offset for
--checkpoint-restore or --take-checkpoint.""")


execfile(os.path.join(config_root, "common", "Options.py"))

(options, args) = parser.parse_args()

if args:
    print "Error: script doesn't take any positional arguments"
    sys.exit(1)

if options.bench:
    try:
        if m5.build_env['TARGET_ISA'] != 'alpha':
            print >>sys.stderr, "Simpoints code only works for Alpha ISA at this time"
            sys.exit(1)
        exec("workload = %s('alpha', 'tru64', 'ref')" % options.bench)
        process = workload.makeLiveProcess()
    except:
        print >>sys.stderr, "Unable to find workload for %s" % options.bench
        sys.exit(1)
else:
    process = LiveProcess()
    process.executable = options.cmd
    process.cmd = [options.cmd] + options.options.split()


if options.input != "":
    process.input = options.input

if options.detailed:
    #check for SMT workload
    workloads = options.cmd.split(';')
    if len(workloads) > 1:
        process = []
        smt_idx = 0
        inputs = []

        if options.input != "":
            inputs = options.input.split(';')

        for wrkld in workloads:
            smt_process = LiveProcess()
            smt_process.executable = wrkld
            smt_process.cmd = wrkld + " " + options.options
            if inputs and inputs[smt_idx]:
                smt_process.input = inputs[smt_idx]
            process += [smt_process, ]
            smt_idx += 1

(CPUClass, test_mem_mode, FutureClass) = Simulation.setCPUClass(options)

CPUClass.clock = '2GHz'

np = options.num_cpus

system = System(cpu = [CPUClass(cpu_id=i) for i in xrange(np)],
                physmem = PhysicalMemory(range=AddrRange("512MB")),
                membus = Bus(), mem_mode = test_mem_mode)

system.physmem.port = system.membus.port

for i in xrange(np):
    system.cpu[i].connectMemPorts(system.membus)
    system.cpu[i].workload = process

    if options.fastmem:
        system.cpu[0].physmem_port = system.physmem.port

root = Root(system = system)

Simulation.run(options, root, system, FutureClass)
