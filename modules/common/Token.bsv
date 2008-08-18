///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// Token.bsv                                                                 //
//                                                                           //
// Tokens are the main way for HAsim to track data across simulator          //
// partitions. The token type includes an index for token tables, epochs,    //
// and scratchpads which partitions can use as they see fit.                 //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

typedef `TOKEN_INDEX_BITS TOKEN_INDEX_SIZE;
typedef TExp#(TOKEN_INDEX_SIZE) NUM_TOKENS;
typedef Bit#(TOKEN_INDEX_SIZE) TOKEN_INDEX;

typedef Bit#(`TOKEN_TIMEP_EPOCH_BITS)      TOKEN_TIMEP_EPOCH;
typedef Bit#(`TOKEN_TIMEP_SCRATCHPAD_BITS) TOKEN_TIMEP_SCRATCHPAD;

typedef Bit#(`TOKEN_FUNCP_EPOCH_BITS)     TOKEN_FUNCP_EPOCH;
typedef Bit#(`TOKEN_FUNCP_SCRATCHPAD_BITS)TOKEN_FUNCP_SCRATCHPAD;

typedef struct
{
    TOKEN_TIMEP_EPOCH      epoch;
    TOKEN_TIMEP_SCRATCHPAD scratchpad;
}
    TOKEN_TIMEP_INFO 
        deriving 
            (Eq, Bits);

typedef struct
{
    TOKEN_FUNCP_EPOCH      epoch;
    TOKEN_FUNCP_SCRATCHPAD scratchpad;
}
  TOKEN_FUNCP_INFO 
    deriving (Eq, Bits);

typedef struct
{
  TOKEN_INDEX       index;
  TOKEN_TIMEP_INFO  timep_info;
  TOKEN_FUNCP_INFO  funcp_info;
}
  TOKEN 
    deriving (Eq, Bits);

//isOlder: predicated on the idea that only half the tokens are in flight at once.

function Bool isOlder(TOKEN_INDEX t1, TOKEN_INDEX t2);

  return (t1 - t2) > (t2 - t1);

endfunction
