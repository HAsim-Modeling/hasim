//BSV library imports
import FIFO::*;
import ModuleCollect::*;

//HASim library imports
import HASim::*;


module [Connected_Module] mkEventRecorder#(String eventname)
    //interface:
                (EventRecorder);

  FIFO#(EventData)  localQ <- mkFIFO();
  FIFO#(TimeStamp)   timeQ <- mkFIFO();
  FIFO#(EventData)  chainQ <- mkFIFO();
  Reg#(Bool)         stall <- mkReg(False);
  
  Bool isBoundary = case (chainQ.first()) matches 
                      tagged EVT_Boundary .t: return True;
		      default: return False;
		    endcase;

  rule insert (stall &&& chainQ.first() matches tagged EVT_Boundary .t &&& t == timeQ.first());
  
    chainQ.enq(localQ.first());
    localQ.deq();
    timeQ.deq();
    stall <= False;
  
  endrule
    

  rule pass (stall &&& chainQ.first() matches tagged EVT_Boundary .t &&& t < timeQ.first());
  
    stall <= False;
    chainQ.enq(EVT_NoEvent);
  
  endrule

  
  let chn = (interface FIFO;
  
	        method CON_Data first() if (!isBoundary || !stall) = marshall(chainQ.first());
		method Action deq() if (!isBoundary || !stall) = chainQ.deq();
		method Action clear() = noAction;
	        method Action enq(CON_Data x) if (!stall);
		
		  EventData evt = unmarshall(x);
		  chainQ.enq(evt);
		  
		  case (evt) matches 
		    tagged EVT_Boundary .t: stall <= True;
		    default: noAction;
		  endcase
		  
		endmethod

	     endinterface);
	     
  //Add our interface to the ModuleCollect collection
  addToCollection(LChain tuple2(0, chn));

  method Action recordEvent(TimeStamp cc, Bit#(32) data);
  
    localQ.enq(EVT_Event data);
    timeQ.enq(cc);
  
  endmethod

endmodule



module [Connected_Module] mkEventController_Software
    //interface:
                ();

  FIFO#(EventData) chainQ <- mkFIFO();
  Reg#(Bit#(32))       cc <- mkReg(0);
  Reg#(Bit#(32))      cur <- mkReg(0);
    
  rule process (True);
  
    chainQ.deq();
    
    case (chainQ.first()) matches
      tagged EVT_Boundary .t:
      begin
	cur <= 0;
	$display("EVENT BEGIN CC [%d]", t);
      end
      tagged EVT_NoEvent:
      begin
	$display("EVENT %0d: ****", cur);
	cur <= cur + 1;
      end
      tagged EVT_Event .d:
      begin
        $display("EVENT %0d: 0x%h", cur, d);
	cur <= cur + 1;
      end
    endcase
     
  endrule
  
  let chn = (interface FIFO;
  
	        method CON_Data first() = marshall(EVT_Boundary cc);
		method Action deq();
		  cc <= cc + 1;
		endmethod
	        method Action enq(CON_Data x) = chainQ.enq(unmarshall(x));
		method Action clear() = noAction;

	     endinterface);

  //Add our interface to the ModuleCollect collection
  addToCollection(LChain tuple2(0, chn));

endmodule
