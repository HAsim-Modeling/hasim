# system options
parser.add_option("-d", "--detailed", action="store_true")
parser.add_option("-t", "--timing", action="store_true")
parser.add_option("-n", "--num-cpus", type="int", default=1)
parser.add_option("--caches", action="store_true")
parser.add_option("--l2cache", action="store_true")
parser.add_option("--fastmem", action="store_true")

# Run duration options
parser.add_option("-m", "--maxtick", type="int")
parser.add_option("--maxtime", type="float")

# Checkpointing options
###Note that performing checkpointing via python script files will override
###checkpoint instructions built into binaries.
parser.add_option("--take-checkpoints", action="store", type="string",
    help="<M,N> will take checkpoint at cycle M and every N cycles thereafter")
parser.add_option("--max-checkpoints", action="store", type="int",
    help="the maximum number of checkpoints to drop", default=5)
parser.add_option("--checkpoint-dir", action="store", type="string",
    help="Place all checkpoints in this absolute directory")
parser.add_option("-r", "--checkpoint-restore", action="store", type="int",
    help="restore from checkpoint <N>")

# CPU Switching - default switch model goes from a checkpoint
# to a timing simple CPU with caches to warm up, then to detailed CPU for
# data measurement
parser.add_option("-s", "--standard-switch", action="store_true",
    help="switch from timing CPU to Detailed CPU")
parser.add_option("-w", "--warmup", action="store", type="int",
    help="if -s, then this is the warmup period.  else, this is ignored",
    default=5000000000)

# Fastforwarding and simpoint related materials
parser.add_option("-W", "--warmup-insts", action="store", type="int",
    default=None,
    help="Warmup period in total instructions (requires --standard-switch)")
parser.add_option("-I", "--max-inst", action="store", type="int", default=None,
    help="Total number of instructions to simulate (default: run forever)")
parser.add_option("-F", "--fast-forward", action="store", type="string",
    default=None,
    help="Number of instructions to fast forward before switching")
parser.add_option("--at-instruction", action="store_true", default=False,
    help="""Treate value of --checkpoint-restore or --take-checkpoint as a
number of instructions.""")
