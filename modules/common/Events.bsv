//BSV library imports
import FIFO::*;
import ModuleCollect::*;

//HASim library imports
import HASim::*;


module [Connected_Module] mkEventRecorder#(String eventname)
    //interface:
                (EventRecorder);

  FIFO#(EventData)  localQ <- mkFIFO();
  FIFO#(EventData)  chainQ <- mkFIFO();
  Reg#(Bool)         stall <- mkReg(False);
 
  rule insert (stall);
  
    chainQ.enq(localQ.first());
    localQ.deq();
    stall <= False;
  
  endrule
      
  let chn = (interface FIFO;
  
	        method CON_Data first() = marshall(chainQ.first());
		method Action deq() = chainQ.deq();
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

  method Action recordEvent(Maybe#(Bit#(32)) mdata);
  
    case (mdata) matches
      tagged Invalid:
      begin
        localQ.enq(EVT_NoEvent);
        //$display("EVENT %s: No Event", eventname);
      end
      tagged Valid .data:
      begin
        localQ.enq(EVT_Event data);
        //$display("EVENT %s: 0x%h", eventname, data);
      end
    endcase
  
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
