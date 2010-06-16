import FIFO::*;

import hasim_common::*;
import soft_connections::*;

/* -------------------------------------------------------------------------- */
// The StallPorts model a ping-pong buffer/double buffer.
//
// Description of a ping-pong buffer
// A ping-pong buffer is an implementation of a pipeline latch which has
// storage for two cycles worth of information. If at the beginning of a cycle,
// both entries in the buffer are occupied, the producer will not do any work,
// regardless of whether the consumer plans to consume an entry in that cycle.
// The advantage of this implementation is that there is no combinational path
// from the consumer to the producer. For those familiar with bluespec, this is
// exactly the bluespec mkFIFO.
//
// Here is an example that demonstrates the operation of a ping pong buffer as
// well as bubble squishing. A-F are instructions, FDXMW are pipeline stages.
// || is the ping-pong buffer and S represents a stall. Note that unlike
// conventional pipeline diagrams, I am showing the instructions as sitting
// inside the buffer as opposed to inside a stage.
//
// time       F  ||  D  ||  X  ||  M  ||  W
//    0
//    1           A
//    2           B      A
//    3           C      B      A  S            # S = stall due to cache-miss
//    4           D      C     BA  S
//    5           E     DC     BA
//    6          FE     DC      B      A
//    7          FE  S1  D      C      B        # S1 = stall due to dependency on B
//    8          FE  S1         D  S2  C        # S2 = stall due to cache miss
//    9          FE             D  S2
//   10           F      E      D  S2           # bubble squished
//

/* -------------------------------------------------------------------------- */
// StallPorts Interface:
//
// StallPorts guard the producer from enq-ing valid data when the modelled
// buffering is fully occupied. Only when canSend is True, the producer can
// _send_ valid data. When canSend is False, the producer must _pass_ instead.
// A pass is equivalent to a send-Invalid, but can only be called when the
// buffers are full. A producer must perform exactly one of the following
// actions every model cycle: (a) send valid data, (b) send invalid, or (c)
// pass.
//
// The consumer side can choose to either receive data for a cycle, or pass.
// Since a module may wish to base this decision on the contents of the first
// entry in the buffer, there is also a peek method which will show the data
// without removing it from the buffer. The receive method, on the other hand,
// and also deq it from the fifo.

/* -------------------------------------------------------------------------- */
// Implementation Notes:
//
// The pC & cC are state for a 4 stage FSM as shown:
//
// State pC  cC        Description                       Action
//   A    0   0  Beginning of model cycle     None.
//   B    0   1  Consumer model cycle ended   Block consumer from consuming more data.
//   C    1   0  Producer model cycle ended   Block producer from producing more data.
//   D    1   1  Both P&C model cycles ended  Determine canSend & canReceive for next cycle.
//
// Legal paths: A -> B -> D -> A and A -> C -> D -> A
//
// Note that the implementation allows the producer and consumer to "finish"
// their model cycle in any order, but prevents them from going ahead for more
// than 1 model cycle.

interface PORT_STALL_SEND#(type a);
    method Action doEnq(a x);
    method Action noEnq();
    method ActionValue#(Bool) canEnq();
    interface INSTANCE_CONTROL_IN_OUT#(1) ctrl;

endinterface

interface PORT_STALL_RECV#(type a);
    method Action noDeq();
    method Action doDeq();
    method ActionValue#(Maybe#(a)) receive();
    interface INSTANCE_CONTROL_IN_OUT#(1) ctrl;
endinterface


interface PORT_STALL_SEND_MULTIPLEXED#(type ni, type a);
    method Action doEnq(INSTANCE_ID#(ni) iid, a x);
    method Action noEnq(INSTANCE_ID#(ni) iid);
    method ActionValue#(Bool) canEnq(INSTANCE_ID#(ni) iid);
    interface INSTANCE_CONTROL_IN_OUT#(ni) ctrl;

endinterface

interface PORT_STALL_RECV_MULTIPLEXED#(type ni, type a);
    method Action noDeq(INSTANCE_ID#(ni) iid);
    method Action doDeq(INSTANCE_ID#(ni) iid);
    method ActionValue#(Maybe#(a)) receive(INSTANCE_ID#(ni) iid);
    interface INSTANCE_CONTROL_IN_OUT#(ni) ctrl;
endinterface


module [HASIM_MODULE] mkPortStallSend#(String s)
                       (PORT_STALL_SEND#(a))
            provisos (Bits#(a, sa));

    Connection_Receive#(Bool) creditFromQueue <- mkConnection_Receive(s + "__cred");

    PORT_SEND#(a) enqToQueue <- mkPortSend(s + "__portDataEnq");


    method Action doEnq (a x);

        enqToQueue.send(tagged Valid x);

    endmethod

    method Action noEnq();

        enqToQueue.send(tagged Invalid);

    endmethod

    method ActionValue#(Bool) canEnq();

        creditFromQueue.deq();
        return creditFromQueue.receive();

    endmethod

    interface INSTANCE_CONTROL_IN_OUT ctrl;

        interface INSTANCE_CONTROL_IN in;
        
            method Bool empty() = !creditFromQueue.notEmpty(); // This is that we have a credit token.
            method Bool balanced() = True;
            method Bool light() = False;
            
            method Maybe#(INSTANCE_ID#(1)) nextReadyInstance() = tagged Valid (?);
            method Action setMaxRunningInstance(INSTANCE_ID#(1) iid) = noAction;
        
        endinterface
        
        interface INSTANCE_CONTROL_OUT out;
            
            method Bool full() = enqToQueue.ctrl.full(); // This is if the output port is full.
            method Bool balanced() = True;
            method Bool heavy() = False;
            method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid) = noAction;
        
        endinterface

    endinterface

endmodule

module [HASIM_MODULE] mkPortStallRecv#(String s)
        (PORT_STALL_RECV#(a))
            provisos (Bits#(a, sa));

    Connection_Send#(Bool) creditToProducer <- mkConnection_Send(s + "__cred");

    PORT_RECV#(a) enqFromProducer <- mkPortRecv_L0(s + "__portDataEnq");

    FIFOF#(a) fifo <- mkUGSizedFIFOF(2);
    
    // We use these as self-contained Ports.
    FIFOF#(Bool)      deqFromConsumer <- mkFIFOF();
    FIFOF#(Maybe#(a)) firstToConsumer <- mkFIFOF();

    Reg#(Bool) outputCompleted <- mkReg(False); // Have we sent outputs for this cycle?

    rule sendOutputs (!outputCompleted);
    
        // First send the output to consumer.
        if (fifo.notEmpty)
        begin
            firstToConsumer.enq(tagged Valid fifo.first());
        end
        else
        begin
            firstToConsumer.enq(tagged Invalid);
        end
        
        // Now send a credit to producer if the FIFO is not full.
        creditToProducer.send(fifo.notFull());
        
        outputCompleted <= True;
    
    endrule
    
    rule handleInputs (outputCompleted);
    
        // First see if there's an enqueue.
        let m_val <- enqFromProducer.receive();

        if (m_val matches tagged Valid .val)
        begin
            fifo.enq(val);
        end
        
        // Now see if there's a dequeue.
        
        if (deqFromConsumer.first())
        begin
            fifo.deq();
        end
        
        deqFromConsumer.deq();
        
        outputCompleted <= False;
    
    endrule
    
    // NOTE: These depend on higher-level guard checking by local controller
    //       and the consumer module.
    //       !ctrl.empty -> canDeq ? 
    //                          (peek* -> (doDeq | noDeq)) :
    //                          noDeq

    method Action doDeq();
        deqFromConsumer.enq(True);
    endmethod

    method Action noDeq();
        deqFromConsumer.enq(False);
    endmethod

    method ActionValue#(Maybe#(a)) receive();
        firstToConsumer.deq();
        return firstToConsumer.first();
    endmethod

    interface INSTANCE_CONTROL_IN_OUT ctrl;

        interface INSTANCE_CONTROL_IN in;

            method Bool empty() = !firstToConsumer.notEmpty();
            method Bool balanced() = True;
            method Bool light() = False;
            
            method Maybe#(INSTANCE_ID#(ni)) nextReadyInstance() = tagged Valid (?);
            method Action setMaxRunningInstance(INSTANCE_ID#(ni) iid) = noAction;
        
        endinterface
    
        interface INSTANCE_CONTROL_OUT out;
    
            method Bool full() = !deqFromConsumer.notFull();
            method Bool balanced() = True;
            method Bool heavy() = False;
            method Action setMaxRunningInstance(INSTANCE_ID#(ni) iid) = noAction;
        
        endinterface

    endinterface

endmodule


module [HASIM_MODULE] mkPortStallSend_Multiplexed#(String s)
                       (PORT_STALL_SEND_MULTIPLEXED#(ni, a))
            provisos (Bits#(a, sa), Add#(TLog#(ni), t_TMP, 6));

    PORT_RECV_MULTIPLEXED#(ni, VOID) creditFromQueue <- mkPortRecvBuffered_Multiplexed(s + "__cred", 0);

    PORT_SEND_MULTIPLEXED#(ni, a) enqToQueue <- mkPortSend_Multiplexed(s + "__portDataEnq");

    method Action doEnq (INSTANCE_ID#(ni) iid, a x);

        enqToQueue.send(iid, tagged Valid x);

    endmethod

    method Action noEnq(INSTANCE_ID#(ni) iid);

        enqToQueue.send(iid, tagged Invalid);

    endmethod

    method ActionValue#(Bool) canEnq(INSTANCE_ID#(ni) iid);

        let m_cred <- creditFromQueue.receive(iid);
        return isValid(m_cred);

    endmethod

    interface INSTANCE_CONTROL_IN_OUT ctrl;

        interface INSTANCE_CONTROL_IN in = creditFromQueue.ctrl;

        interface INSTANCE_CONTROL_OUT out = enqToQueue.ctrl;

    endinterface

endmodule

typedef enum
{
    STALLP_idle,
    STALLP_initializing,
    STALLP_running
}
STALLP_STATE deriving (Eq, Bits);

module [HASIM_MODULE] mkPortStallRecv_Multiplexed#(String s)
        (PORT_STALL_RECV_MULTIPLEXED#(ni, a))
            provisos (Bits#(a, sa), Add#(TLog#(ni), t_TMP, 6));

    PORT_SEND_MULTIPLEXED#(ni, VOID) creditToProducer <- mkPortSend_Multiplexed(s + "__cred");

    PORT_RECV_MULTIPLEXED#(ni, a) enqFromProducer <- mkPortRecvBuffered_Multiplexed(s + "__portDataEnq", 0);

    // We use these like ports which are self-contained.
    NumTypeParam#(TAdd#(ni, 1)) buffering = ?;
    FIFOF#(Tuple2#(INSTANCE_ID#(ni), Maybe#(a))) firstToConsumer <- mkSizedLUTRAMFIFOF(buffering);
    FIFOF#(Bool) deqFromConsumer <- mkSizedFIFOF_DRAM(buffering);

    FIFO#(Tuple2#(INSTANCE_ID#(ni), FUNC_FIFO#(a, 2))) stage1Ctrl <- mkSizedLUTRAMFIFO(buffering);
    FIFO#(FUNC_FIFO#(a, 2)) stage2Ctrl <- mkSizedLUTRAMFIFO(buffering);

    Reg#(STALLP_STATE) state <- mkReg(STALLP_idle);
    Reg#(INSTANCE_ID#(ni)) maxRunningInstance <- mkRegU();
    Reg#(INSTANCE_ID#(ni)) initIID <- mkReg(0);

    rule initialize (state == STALLP_initializing);
    
        // Push every instance into stage1.
        stage1Ctrl.enq(tuple2(initIID, funcFIFO_Init));
        initIID <= initIID + 1;
        
        if (initIID == maxRunningInstance)
        begin
            state <= STALLP_running;
        end
    
    endrule

    rule stage1_creditAndFirst (state == STALLP_running);

        match {.iid, .fifo_state} = stage1Ctrl.first();
        stage1Ctrl.deq();

        // Send the first element on, if any.
        if (funcFIFO_notEmpty(fifo_state))
        begin
            firstToConsumer.enq(tuple2(iid, tagged Valid funcFIFO_UGfirst(fifo_state)));
        end
        else
        begin
            firstToConsumer.enq(tuple2(iid, tagged Invalid));
        end

        // Send a credit if the particular FIFO is not full.
        let cred = (funcFIFO_notFull(fifo_state)) ? tagged Valid (?) : tagged Invalid;
        creditToProducer.send(iid, cred);
        
        stage2Ctrl.enq(fifo_state);
        
    endrule

    rule stage2_deqAndCredit (enqFromProducer.ctrl.nextReadyInstance() matches tagged Valid .iid &&& state == STALLP_running);
 
        // First deal with enqueues.
        let m_enq <- enqFromProducer.receive(iid);
        // Could add an iid sanity check here.
        
        let fifo_state = stage2Ctrl.first();
        stage2Ctrl.deq();
        
        if (m_enq matches tagged Valid .val)
        begin
            fifo_state = funcFIFO_UGenq(fifo_state, val);
        end

        // Now deal with dequeues.
        if (deqFromConsumer.first())
        begin
            fifo_state = funcFIFO_UGdeq(fifo_state);
        end
        deqFromConsumer.deq(); // Could add an iid sanity check here, if deqFromConsumer carried iid along.
        
        stage1Ctrl.enq(tuple2(iid, fifo_state));

    endrule
    
    // NOTE: These depend on higher-level guard checking by local controller
    //       and the consumer module.
    //       !ctrl.empty -> canDeq ? 
    //                          (peek* -> (doDeq | noDeq)) :
    //                          noDeq

    method Action doDeq(INSTANCE_ID#(ni) iid);
        deqFromConsumer.enq(True);
    endmethod

    method ActionValue#(Maybe#(a)) receive(INSTANCE_ID#(ni) iid);
        match {.iid2, .m_val} = firstToConsumer.first();
        firstToConsumer.deq();
        return m_val; 
    endmethod

    method Action noDeq(INSTANCE_ID#(ni) iid);
        deqFromConsumer.enq(False);
    endmethod

    interface INSTANCE_CONTROL_IN_OUT ctrl;

        interface INSTANCE_CONTROL_IN in;

            method Bool empty() = !firstToConsumer.notEmpty();
            method Bool balanced() = True;
            method Bool light() = False;
            method Maybe#(INSTANCE_ID#(ni)) nextReadyInstance();
            
                if (firstToConsumer.notEmpty())
                begin
                    match {.iid, .*} = firstToConsumer.first();
                    return tagged Valid iid;
                end
                else
                begin
                    return tagged Invalid;
                end
            
            endmethod
            
            method Action setMaxRunningInstance(INSTANCE_ID#(ni) iid);

                maxRunningInstance <= iid;
                state <= STALLP_initializing;
                enqFromProducer.ctrl.setMaxRunningInstance(iid);

            endmethod
        
        endinterface
    
        interface INSTANCE_CONTROL_OUT out;
    
            method Bool full() = !deqFromConsumer.notFull();
            method Bool balanced() = True;
            method Bool heavy() = False;
            method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid) = noAction; // Handled by the inctrl, above.
        
        endinterface

    endinterface

endmodule
