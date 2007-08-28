///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// Token.bsv                                                                 //
//                                                                           //
// Tokens are the main way for HAsim to track data across simulator          //
// partitions. The token type includes an index for token tables, epochs,    //
// and scratchpads which partitions can use as they see fit.                 //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

typedef Bit#(`HASIM_TOK_INDEX_BITS) TokIndex;

typedef Bit#(`HASIM_TIMEP_EPOCH_BITS) TIMEP_Epoch;
typedef Bit#(`HASIM_TIMEP_SCRATCHPAD_BITS) TIMEP_Scratchpad;

typedef Bit#(`HASIM_FUNCP_EPOCH_BITS) FUNCP_Epoch;
typedef Bit#(`HASIM_FUNCP_SCRATCHPAD_BITS)FUNCP_Scratchpad;

typedef struct
{
  TIMEP_Epoch   epoch;
  TIMEP_Scratchpad scratchpad;
}
  TIMEP_TokInfo 
    deriving (Eq, Bits);

typedef struct
{
  FUNCP_Epoch   epoch;
  FUNCP_Scratchpad scratchpad;
}
  FUNCP_TokInfo 
    deriving (Eq, Bits);

typedef struct
{
  TokIndex	    index;
  TIMEP_TokInfo     timep_info;
  FUNCP_TokInfo     funcp_info;
}
  Token 
    deriving (Eq, Bits);

//isOlder: predicated on the idea that only half the tokens are in flight at once.

function Bool isOlder(TokIndex t1, TokIndex t2);

  return (t1 - t2) > (t2 - t1);

endfunction
