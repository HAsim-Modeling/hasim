
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
