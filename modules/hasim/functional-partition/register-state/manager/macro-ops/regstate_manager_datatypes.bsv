
// ***** Typedefs ***** //

// UP_TO_TWO

typedef union tagged
{
    a ONE;
    Tuple2#(a, a) TWO;
}
    UP_TO_TWO#(parameter type a)
        deriving (Eq, Bits);

function a getFirst(UP_TO_TWO#(a) d);

    case (d) matches
        tagged ONE .x:         return x;
        tagged TWO {.x1, .x2}: return x1;
    endcase

endfunction

function Bool hasSecond(UP_TO_TWO#(a) d);

    case (d) matches
        tagged ONE .x:         return False;
        tagged TWO {.x1, .x2}: return True;
    endcase

endfunction

function Maybe#(a) getSecond(UP_TO_TWO#(a) d);

    case (d) matches
        tagged ONE .x:         return tagged Invalid;
        tagged TWO {.x1, .x2}: return tagged Valid x2;
    endcase

endfunction

function a getSecondOfTwo(UP_TO_TWO#(a) d);

    case (d) matches
        tagged ONE .x:         return ?;
        tagged TWO {.x1, .x2}: return x2;
    endcase

endfunction


// MEM_PATH

// A type to distinguish where load responses should be sent.

typedef enum
{
  PATH_INST,
  PATH_LOAD,
  PATH_STORE
}
  MEM_PATH
      deriving (Eq, Bits);


// REGMANAGER_STATE

// A type to indicating what we're doing on a high level.

typedef enum
{
  RSM_Initializing,
  RSM_Running,
  RSM_DrainingForRewind,
  RSM_Rewinding,
  RSM_RewindingWaitForSlowRemap,
  RSM_DrainingForEmulate,
  RSM_SyncingRegisters,
  RSM_RequestingEmulation,
  RSM_UpdatingRegisters
}
  REGMANAGER_STATE
      deriving (Eq, Bits);


// ******* PIPELINE STAGE STATE ******* //

// The following datatypes are internal states of pipeline stages that can stall.
// They are named consistently to reflect which stage uses it. The "NORMAL" state
// reflects normal unstalled operation.

// STATE_ITRANS1

typedef union tagged
{
    void        ITRANS1_NORMAL;
    ISA_ADDRESS ITRANS1_SPAN_REQ;
}
    STATE_ITRANS1
        deriving (Eq, Bits);

// STATE_ITRANS2


typedef union tagged
{
    void        ITRANS2_NORMAL;
    MEM_ADDRESS ITRANS2_SPAN_RSP;
}
    STATE_ITRANS2
        deriving (Eq, Bits);

// STATE_INST2

typedef union tagged
{
    void        INST2_NORMAL;
    INST_INFO   INST2_SPAN_REQ;
}
    STATE_INST2
        deriving (Eq, Bits);


// STATE_INST3

typedef union tagged
{
    void        INST3_NORMAL;
    ISA_VALUE   INST3_SPAN_RSP;
}
    STATE_INST3
        deriving (Eq, Bits);


// STATE_DEPS2
              
typedef union tagged
{
    void DEPS2_NORMAL;
    struct 
    { 
        Bit#(4)          numToAlloc;
        Bit#(4)          current;
        ISA_SRC_MAPPING  mapSrcs;
        ISA_DST_MAPPING  mapDstsSoFar;
        ISA_INST_DSTS    regsToFreeSoFar;
    }
        DEPS2_ALLOC_MORE;
}
    STATE_DEPS2
        deriving (Eq, Bits);


// STATE_RES4

typedef union tagged
{
    void        RES4_NORMAL;
    struct
    {
        Vector#(TSub#(ISA_MAX_DSTS, 1), Maybe#(Tuple2#(FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE))) remainingValues;
        ISA_EXECUTION_RESULT result;
        Bit#(4)    current; 
    }
        RES4_ADDITIONAL_WB;
}
    STATE_RES4
        deriving (Eq, Bits);


// STATE_DTRANS2

typedef union tagged
{
    void        DTRANS2_NORMAL;
    ISA_ADDRESS DTRANS2_SPAN_REQ;
}
    STATE_DTRANS2
        deriving (Eq, Bits);


// STATE_DTRANS3

typedef union tagged
{
    void        DTRANS3_NORMAL;
    MEM_ADDRESS DTRANS3_SPAN_RSP;
}
    STATE_DTRANS3
        deriving (Eq, Bits);

// STATE_LOADS2

typedef union tagged
{
    void       LOADS2_NORMAL;
    LOADS_INFO LOADS2_SPAN_REQ;
    
}
    STATE_LOADS2
        deriving (Eq, Bits);

// STATE_LOADS3

typedef union tagged
{
    void       LOADS3_NORMAL;
    MEM_VALUE  LOADS3_SPAN_RSP;
    
}
    STATE_LOADS3
        deriving (Eq, Bits);

// STATE_STORES2

typedef union tagged
{ 
    void        STORES2_NORMAL;
    STORES_INFO STORES2_RMW_RSP;
    STORES_INFO STORES2_SPAN_REQ;
    STORES_INFO STORES2_SPAN_RSP1;
    Tuple2#(STORES_INFO, MEM_VALUE) STORES2_SPAN_RSP2;
    Tuple2#(STORES_INFO, MEM_VALUE) STORES2_SPAN_END;
}
    STATE_STORES2
        deriving (Eq, Bits);


// ******* PIPELINE FIFO STATE ******* //

// These datatypes live in FIFOs between pipeline stages.



// ITRANS_INFO

typedef union tagged
{
    TOKEN ITRANS_NORMAL;
    TOKEN ITRANS_SPAN;
}
    ITRANS_INFO
        deriving (Eq, Bits); 


function TOKEN getITransToken(ITRANS_INFO i);

    case (i) matches
        tagged ITRANS_NORMAL .tok: return tok;
        tagged ITRANS_SPAN   .tok: return tok;
    endcase

endfunction

// INST_INFO

typedef struct
{
    TOKEN token;
    UP_TO_TWO#(MEM_ADDRESS) memAddrs;
}
    INST_INFO
        deriving (Eq, Bits);

// DTRANS_INFO

typedef union tagged
{
    TOKEN DTRANS_NORMAL;
    TOKEN DTRANS_SPAN;
}
    DTRANS_INFO
        deriving (Eq, Bits);

function TOKEN getDTransToken(DTRANS_INFO i);

    case (i) matches
        tagged DTRANS_NORMAL .tok: return tok;
        tagged DTRANS_SPAN   .tok: return tok;
    endcase

endfunction

// LOADS_INFO

typedef struct
{
    TOKEN token;
    UP_TO_TWO#(MEM_ADDRESS) memAddrs;
    ISA_MEMOP_TYPE opType;
    MEM_OFFSET offset;
}
    LOADS_INFO
        deriving (Eq, Bits);

// STORES_INFO

typedef struct
{
    TOKEN token;
    UP_TO_TWO#(MEM_ADDRESS) memAddrs;
    ISA_MEMOP_TYPE opType;
    MEM_OFFSET offset;
    ISA_VALUE storeValue;

}
    STORES_INFO
        deriving (Eq, Bits);
