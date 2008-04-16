import m5
from m5.objects import *

class L1Cache(BaseCache):
    assoc = 2
    block_size = 64
    latency = '1ns'
    mshrs = 10
    tgts_per_mshr = 5

class L2Cache(BaseCache):
    assoc = 8
    block_size = 64
    latency = '10ns'
    mshrs = 20
    tgts_per_mshr = 12

class IOCache(BaseCache):
    assoc = 8
    block_size = 64
    latency = '10ns'
    mshrs = 20
    size = '1kB'
    tgts_per_mshr = 12
