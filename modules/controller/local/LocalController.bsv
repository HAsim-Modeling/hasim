///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// LocalController.bsv                                                       //
//                                                                           //
// Local Controller instantiated by timing modules.                          //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

import hasim_common::*;
import hasim_modellib::*;
import soft_connections::*;

import Vector::*;
import FIFO::*;


interface LocalController;

  method Action startModelCC();
  method Bool   running();
  method Action endProgram(Bool passfail);

endinterface

typedef enum
{
  LC_Idle,               //Waiting for a command
  LC_Running,            //Running, allowing slip
  LC_Synchronizing,      //Running, attempting to synchronize
  LC_Stepping            //Run one modelCC
}
  LCState deriving (Eq, Bits);

module [HASim_Module] mkLocalController#(Vector#(n, Port_Control) inports, Vector#(m, Port_Control) outports) (LocalController);

  Reg#(LCState) state <- mkReg(LC_Idle);
  
  Reg#(Bool) balanced_since_query <- mkRegU();
  Reg#(Bool) check_balanced <- mkReg(False);
  
  Connection_Chain#(Command)  cmds  <- mkConnection_Chain(2);
  Connection_Chain#(Response) resps <- mkConnection_Chain(3);

  //Can this module read from this Port?

  function Bool canReadFrom(Port_Control inport);
  
    return case (state)
      LC_Running:        return !inport.empty();
      LC_Stepping:       return !inport.empty();
      LC_Synchronizing:  return !inport.light();
      default:           return False;
    endcase;
  
  endfunction

  function canWriteTo(Port_Control inport);
  
    return case (state)
      LC_Running:        return !inport.full();
      LC_Stepping:       return !inport.full();
      LC_Synchronizing:  return !inport.heavy();
      default:           return False;
    endcase;
    
  endfunction

  function Bool canStart();
  
    Bool canRead  = True;
    Bool canWrite = True;
    
    for (Integer x = 0; x < valueof(n); x = x + 1)
      canRead = canRead && canReadFrom(inports[x]);
    
    for (Integer x = 0; x < valueof(m); x = x + 1)
      canWrite = canWrite && canWriteTo(outports[x]);
    
    return canRead && canWrite;
  
  endfunction

  function Bool balanced();
  
    Bool res = True;
    
    for (Integer x = 0; x < valueof(n); x = x + 1)
      res = res && inports[x].balanced();
      
    for (Integer x = 0; x < valueof(m); x = x + 1)
      res = res && outports[x].balanced();
    
    return res;
  
  endfunction
  
  (* descending_urgency="checkBalance, shiftCommand, shiftResponse" *)

  rule shiftCommand (True);
  
     Command newcmd <- cmds.receive_from_prev();
     
     case (newcmd) matches
       tagged COM_RunProgram:
       begin
         state <= LC_Running;
       end
       tagged COM_Synchronize:
       begin
         state <= LC_Synchronizing;
       end
       tagged COM_StartSyncQuery:
       begin
         check_balanced <= True;
         balanced_since_query <= True;
       end
       tagged COM_SyncQuery:
       begin
         check_balanced <= False;
         if (balanced_since_query)
           resps.send_to_next(RESP_Balanced);
	 else
	   resps.send_to_next(RESP_UnBalanced);
       end
       tagged COM_Step:
       begin
         state <= LC_Stepping;
       end
     endcase
  
     //send it on
     cmds.send_to_next(newcmd);
  
  endrule
  
  rule checkBalance (check_balanced);
  
    balanced_since_query <= balanced_since_query && balanced();
    
  endrule
  
  rule shiftResponse (True);
  
    Response resp <- resps.receive_from_prev();
    
    //Just send it on
    resps.send_to_next(resp);
  
  endrule

  method Action startModelCC() if (canStart());
  
    case (state)
      LC_Idle:          noAction;
      LC_Running:       noAction;
      LC_Synchronizing: noAction;
      LC_Stepping:
      begin
        state <= LC_Idle;
      end
      
    endcase
  
  endmethod

  method Bool running();
  
    return case (state)
      LC_Idle:          return False;
      LC_Running:       return True;
      LC_Synchronizing: return True;
      LC_Stepping:      return True;
    endcase;
    
  
  endmethod
  
  method Action endProgram(Bool passfail);
  
    resps.send_to_next(tagged RESP_DoneRunning passfail);
  
  endmethod


endmodule

