//
// Copyright (c) 2014, Intel Corporation
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
//
// Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// Neither the name of the Intel Corporation nor the names of its contributors
// may be used to endorse or promote products derived from this software
// without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// LocalController.bsv                                                       //
//                                                                           //
// Local Controller instantiated by timing modules.                          //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

import Vector::*;
import List::*;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;

// Project imports

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/hasim_modellib.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/soft_services_deps.bsh"
`include "asim/provides/fpga_components.bsh"


//
// Global types, passed to global controller.
//

// Instructions committed this cycle.  The width here must be large enough for
// the commit bandwidth of the largest model.
typedef Bit#(4) MODEL_NUM_COMMITS;

typedef CONTEXT_ID                             CONTROL_MODEL_CYCLE_MSG;
typedef Tuple2#(CONTEXT_ID, MODEL_NUM_COMMITS) CONTROL_MODEL_COMMIT_MSG;


//
// Scan data (for debugging)
//

// CONTROLLER_SCAN_DATA is sent directly to software in response to a scan
// request, in COM_SCAN_DATA sized chunks, starting with the low bits.
typedef struct
{
    Vector#(t_NUM_OUTPORTS, Bool) outctrls;
    Vector#(t_NUM_UNPORTS, Bool) unctrls;
    Vector#(t_NUM_INPORTS, Bool) inctrls;
    Bit#(16) cycle;                    // Cycle counter to track relative
                                       // positions of controllers
    Bit#(t_NUM_IID_BITS) nextInstance;
    Bool nextIsReady;
    GLOBAL_STRING_UID name;
}
CONTROLLER_SCAN_DATA#(numeric type t_NUM_INPORTS,
                      numeric type t_NUM_UNPORTS,
                      numeric type t_NUM_OUTPORTS,
                      numeric type t_NUM_IID_BITS)
    deriving (Eq, Bits);

// Data types for scanning state out of local controllers
typedef Bit#(8) COM_SCAN_DATA;

// Inter-controller messages that travel on the controller ring
typedef union tagged
{
    // Commands from to local controllers
    void          COM_RunProgram;     // Begin running, allowed to slip
    void          COM_Synchronize;    // Start synchronizing the system
    Bool          COM_SyncQuery;      // Is the system synchronized yet?
                                      // System is synchronized if the message
                                      // makes it around the ring remaining True.
    void          COM_Step;           // Run exactly one model CC.
    Bool          COM_Pause;          // Stop running (True -> send response to host)
    Bool          COM_Resume;         // Resume running (True -> send response to host)
    CONTEXT_ID    COM_EnableContext;  // Enable context
    CONTEXT_ID    COM_DisableContext; // Disable context

    void          COM_Scan;           // Scan state for debugging
    void          COM_TestThroughput; // Stage-by-stage throughput testing

    // Messages from local controllers
    Bool          LC_DoneRunning;     // Bool is run passed

    COM_SCAN_DATA LC_ScanData;        // Response to COM_Scan request
    COM_SCAN_DATA LC_ScanDataLast;    // Last chunk of COM_Scan response

    Bool          LC_ScanRunning;     // Serial scan of instanceRunning state
                                      // (also triggered by COM_Scan request)
    Bool          LC_ScanRunningLast; // Last instance of running state scan

    Bit#(16)      LC_ThroughputData;  // Cycles for one context
    void          LC_ThroughputLast;  // Last instance of throughput test data
}
CONTROLLER_MSG
    deriving (Eq, Bits);

                
// t_NUM_INSTANCES is number of instances to control.
interface LOCAL_CONTROLLER#(type t_NUM_INSTANCES);

    method ActionValue#(INSTANCE_ID#(t_NUM_INSTANCES)) startModelCycle();
    method Action endModelCycle(INSTANCE_ID#(t_NUM_INSTANCES) iid, Bit#(8) path);
    method Action instanceDone(INSTANCE_ID#(t_NUM_INSTANCES) iid, Bool passfail);

    method INSTANCE_ID#(t_NUM_INSTANCES) getMaxActiveInstance();
    method Action setMaxActiveInstance(INSTANCE_ID#(t_NUM_INSTANCES) maxIID);

endinterface


typedef enum
{
    LC_Idle,               // Waiting for a command
    LC_Running,            // Running, allowing slip
    LC_Synchronizing,      // Running, attempting to synchronize
    LC_Stepping            // Run one modelCC
}
LC_STATE
    deriving (Eq, Bits);

// Throughput tester state.
typedef enum
{
    CTRL_TP_Idle,          // Waiting for a command
    CTRL_TP_Warmup0,       // Prepare to test this controller
    CTRL_TP_Warmup1,       // Hold this controller idle while others run
    CTRL_TP_Sample,
    CTRL_TP_Finish0,
    CTRL_TP_Finish1
}
CTRL_TP_STATE
    deriving (Eq, Bits);


// Wrapper around the real local controller, with no uncontrolled ports.

module [HASIM_MODULE] mkLocalController

    // parameters:
    #(
    Vector#(t_NUM_INPORTS,  INSTANCE_CONTROL_IN#(t_NUM_INSTANCES))  inctrls, 
    Vector#(t_NUM_OUTPORTS, INSTANCE_CONTROL_OUT#(t_NUM_INSTANCES)) outctrls
    )
    // interface:
        (LOCAL_CONTROLLER#(t_NUM_INSTANCES));

    Vector#(0, INSTANCE_CONTROL_IN#(t_NUM_INSTANCES)) empty_unctrls = newVector();

    let m <- mkNamedLocalControllerWithActive("[no name]", 0, True, inctrls, empty_unctrls, outctrls);
    return m;

endmodule

module [HASIM_MODULE] mkNamedLocalController

    // parameters:
    #(
    String name,
    Vector#(t_NUM_INPORTS,  INSTANCE_CONTROL_IN#(t_NUM_INSTANCES))  inctrls, 
    Vector#(t_NUM_OUTPORTS, INSTANCE_CONTROL_OUT#(t_NUM_INSTANCES)) outctrls
    )
    // interface:
        (LOCAL_CONTROLLER#(t_NUM_INSTANCES));

    Vector#(0, INSTANCE_CONTROL_IN#(t_NUM_INSTANCES)) empty_unctrls = newVector();

    let m <- mkNamedLocalControllerWithActive(name, 0, True, inctrls, empty_unctrls, outctrls);
    return m;

endmodule

module [HASIM_MODULE] mkLocalControllerWithUncontrolled

    // parameters:
    #(
    Vector#(t_NUM_INPORTS,  INSTANCE_CONTROL_IN#(t_NUM_INSTANCES))  inctrls, 
    Vector#(t_NUM_UNPORTS,  INSTANCE_CONTROL_IN#(t_NUM_INSTANCES))  uncontrolled_ctrls,
    Vector#(t_NUM_OUTPORTS, INSTANCE_CONTROL_OUT#(t_NUM_INSTANCES)) outctrls
    )
    // interface:
        (LOCAL_CONTROLLER#(t_NUM_INSTANCES));

    let m <- mkNamedLocalControllerWithActive("[no name]", 0, True, inctrls, uncontrolled_ctrls, outctrls);
    return m;

endmodule


module [HASIM_MODULE] mkNamedLocalControllerWithUncontrolled

    // parameters:
    #(
    String name,
    Vector#(t_NUM_INPORTS,  INSTANCE_CONTROL_IN#(t_NUM_INSTANCES))  inctrls, 
    Vector#(t_NUM_UNPORTS,  INSTANCE_CONTROL_IN#(t_NUM_INSTANCES))  uncontrolled_ctrls,
    Vector#(t_NUM_OUTPORTS, INSTANCE_CONTROL_OUT#(t_NUM_INSTANCES)) outctrls
    )
    // interface:
        (LOCAL_CONTROLLER#(t_NUM_INSTANCES));

    let m <- mkNamedLocalControllerWithActive(name, 0, True, inctrls, uncontrolled_ctrls, outctrls);
    return m;

endmodule


//
// Actual Local Controller handles inports, outports, and "uncontrolled" in ports,
// which are not used as the basis for deciding whether or not to simulate the
// next model cycle. However, they ARE told how many instances are running.
//
// The "startingActive" parameter defines the number of active instances
// outside the usual space controlled by the functional model.  For example,
// in some interconnect models the memory controller may be in the instance
// space along with CPU ports and the memory would always be active.
//
// The "dynamicActive" parameter determines whether the usual functional
// model contexts are managed by this controller.  When true (the usual
// case) contexts becoming active update the count of instances managed
// by each port.  When false, the number of active contexts is fixed to
// the value of startingActive.  This might be used by a memory controller.
//
module [HASIM_MODULE] mkNamedLocalControllerWithActive

    // parameters:
    #(
    String name,
    function Integer startingActive(),
    Bool dynamicActive,
    Vector#(t_NUM_INPORTS,  INSTANCE_CONTROL_IN#(t_NUM_INSTANCES))  inctrls, 
    Vector#(t_NUM_UNPORTS,  INSTANCE_CONTROL_IN#(t_NUM_INSTANCES))  uncontrolled_ctrls,
    Vector#(t_NUM_OUTPORTS, INSTANCE_CONTROL_OUT#(t_NUM_INSTANCES)) outctrls
    )
    // interface:
        (LOCAL_CONTROLLER#(t_NUM_INSTANCES))
    provisos (Alias#(t_IID, INSTANCE_ID#(t_NUM_INSTANCES)),
              // Counter is large enough for 256 cycle quiesce per context.
              Alias#(t_TP_COUNTER, Bit#(TAdd#(8, TLog#(t_NUM_INSTANCES)))),
              Alias#(CONTROLLER_SCAN_DATA#(t_NUM_INPORTS,
                                           t_NUM_UNPORTS,
                                           t_NUM_OUTPORTS,
                                           INSTANCE_ID_BITS#(t_NUM_INSTANCES)), t_SCAN_DATA));

    // Emit file with data for generating a connection graph
    emitPortGraphFile(name, inctrls, uncontrolled_ctrls, outctrls);


    // ====================================================================
    //
    //   Controller state.
    //
    // ====================================================================

    Reg#(LC_STATE) state <- mkReg(LC_Idle);
    Reg#(Bool) scanning <- mkReg(False);
  
    // Small cycle counter counts wrapping of the IID and is useful when debugging
    // to compare the relative cycles of local controllers.
    Reg#(Bit#(16)) cycle <- mkReg(0);

    // Counter of active instances. 
    // We start at -1, so we assume at least one instance is active.
    COUNTER#(INSTANCE_ID_BITS#(t_NUM_INSTANCES))
        maxActiveInstance <- mkLCounter(~0 + fromInteger(startingActive));

    // Vector of running instances.  In order to provide two write ports
    // (one for start one for end) the vector is broken into an "S" and an
    // "E" vector.  A running instance has different values in the two
    // vectors -- namely the start and end are out of balance.
    MULTIPLEXED_REG#(t_NUM_INSTANCES, Bit#(1)) instanceRunningS <- mkMultiplexedReg(0);
    MULTIPLEXED_REG#(t_NUM_INSTANCES, Bit#(1)) instanceRunningE <- mkMultiplexedReg(0);

    // Signalled DONE to the software?
    Reg#(Bool) signalDone <- mkReg(False);

    Wire#(Bit#(8)) pathDoneW <- mkWire();
    
    // Encode the scan data size in the string to avoid having to store it
    // in the hardware-generated data.
    String encodedName = integerToString(valueOf(t_NUM_INPORTS)) + "," +
                         integerToString(valueOf(t_NUM_UNPORTS)) + "," +
                         integerToString(valueOf(t_NUM_OUTPORTS)) + "," +
                         integerToString(valueOf(INSTANCE_ID_BITS#(t_NUM_INSTANCES))) + "," +
                         name;
    GLOBAL_STRING_UID nameUID <- getGlobalStringUID(encodedName);
    MARSHALLER#(COM_SCAN_DATA, t_SCAN_DATA) scanStream <- mkSimpleMarshaller();


    //
    // State for managing throughput tests.
    //
    Reg#(CTRL_TP_STATE) tpState <- mkReg(CTRL_TP_Idle);
    FIFOF#(t_TP_COUNTER) tpSampleQ <- mkSizedFIFOF(valueOf(TMin#(NUM_CONTEXTS, 64)));
    PulseWire tpCounterResetW <- mkPulseWireOR();
    PulseWire tpInstance0W <- mkPulseWire();
    Reg#(t_TP_COUNTER) tpCounter <- mkRegU();


    // For now this local controller just goes round-robin over the instances.
    // This is guaranteed to be correct accross multiple modules.
    // The performance of this could be improved, but the interaction with time-multiplexed
    // ports needs to be worked out.
    
    COUNTER#(INSTANCE_ID_BITS#(t_NUM_INSTANCES)) nextInstance <- mkLCounter(0);
    
    CONNECTION_CHAIN#(CONTROLLER_MSG) link_controllers <- mkConnectionChain("CONTROLLER_MESSAGES");

    // Can this module read from this Port?
    function Bool canReadFrom(INSTANCE_CONTROL_IN#(t_NUM_INSTANCES) ctrl_in);
        return case (state)
                   LC_Running:        return !ctrl_in.empty();
                   LC_Stepping:       return !ctrl_in.empty();
                   LC_Synchronizing:  return !ctrl_in.light();
                   default:           return False;
               endcase;
    endfunction

    function Bool canWriteTo(INSTANCE_CONTROL_OUT#(t_NUM_INSTANCES) ctrl_out);
        return case (state)
                   LC_Running:        return !ctrl_out.full();
                   LC_Stepping:       return !ctrl_out.full();
                   LC_Synchronizing:  return !ctrl_out.heavy();
                   default:           return False;
               endcase;
    endfunction

    //
    // isRunning --
    //     Convert split instanceRunning vector into a single isRunning state.
    //     The "S" vector is updated by starting a cyle.  The "E" by ending
    //     a cycle.  A cycle is running if the two are different.
    //
    function Bool isRunning(t_IID iid);
        Reg#(Bit#(1)) runningS = instanceRunningS.getReg(iid);
        Reg#(Bit#(1)) runningE = instanceRunningE.getReg(iid);

        return runningS != runningE;
    endfunction


    function Bool balanced();
        Bool res = True;
        
        // Are the ports all balanced?
        for (Integer x = 0; x < valueOf(t_NUM_INPORTS); x = x + 1)
        begin
            res = res && inctrls[x].balanced();
        end

        for (Integer x = 0; x < valueOf(t_NUM_OUTPORTS); x = x + 1)
        begin
            res = res && outctrls[x].balanced();
        end

        return res;
    endfunction


    //
    // instanceReady --
    //     Determine the next instance in a non-round-robin manner when
    //     we're ready to go that route.
    //
    PulseWire instanceReadyW <- mkPulseWire();
    // Wires with no consumers -- useful for waveform debugging
    RWire#(Vector#(t_NUM_INPORTS, Bool)) dbgInctrls <- mkRWire();
    RWire#(Vector#(t_NUM_OUTPORTS, Bool)) dbgOutctrls <- mkRWire();

    (* fire_when_enabled *)
    rule instanceReady (True);

        let iid = nextInstance.value();

        Bool can_read = Vector::all(canReadFrom, inctrls);
        Bool can_write = Vector::all(canWriteTo, outctrls);

        // Write input/output port state to wires for waveform debugging.
        // Without this it is necessary to find the name and signals of each
        // port individually.
        dbgInctrls.wset(map(canReadFrom, inctrls));
        dbgOutctrls.wset(map(canWriteTo, outctrls));

        let holdForThroughputTest = (tpState == CTRL_TP_Warmup1) ||
                                    (tpState == CTRL_TP_Finish0);

        if (! isRunning(iid) && can_read && can_write && ! holdForThroughputTest)
        begin
            instanceReadyW.send();
        end

    endrule


    // ====================================================================
    //
    // Process controller commands and send responses.
    //
    // ====================================================================

    FIFO#(Bool) checkBalanceQ <- mkFIFO();
    FIFO#(CONTROLLER_MSG) newCtrlMsgQ <- mkFIFO1();
    
    rule checkBalance (! scanning);
        checkBalanceQ.deq();
        link_controllers.sendToNext(tagged COM_SyncQuery balanced());
    endrule

    rule newControlMsg (! scanning);
        let cmd = newCtrlMsgQ.first();
        newCtrlMsgQ.deq();
        
        link_controllers.sendToNext(cmd);
    endrule

    rule nextCommand (! scanning &&
                      (state != LC_Stepping) &&
                      (tpState == CTRL_TP_Idle));
        let newcmd <- link_controllers.recvFromPrev();
        Maybe#(CONTROLLER_MSG) outcmd = tagged Valid newcmd;

        case (newcmd) matches
            tagged COM_RunProgram:
            begin
    
                for (Integer x = 0; x < valueof(t_NUM_INPORTS); x = x + 1)
                begin
                    inctrls[x].setMaxRunningInstance(maxActiveInstance.value());
                end

                for (Integer x = 0; x < valueof(t_NUM_UNPORTS); x = x + 1)
                begin
                    uncontrolled_ctrls[x].setMaxRunningInstance(maxActiveInstance.value());
                end

                for (Integer x = 0; x < valueof(t_NUM_OUTPORTS); x = x + 1)
                begin
                    outctrls[x].setMaxRunningInstance(maxActiveInstance.value());
                end

                state <= LC_Running;

            end

            tagged COM_Synchronize:
            begin
                state <= LC_Synchronizing;
            end

            tagged COM_SyncQuery .all_balanced:
            begin
                // The COM_SyncQuery state will remain True if all controllers
                // are balanced.  If a previous controller is unbalanced then
                // just forward the unbalanced state.  If we need to check
                // the state of this controller then queue the request.
                //
                // The Bluespec scheduler throws an error about being unable
                // to break a cycle if we attempt to check balance here
                // because it can't break the connection between setting
                // state, startModelCycle and changes to balance() while
                // the model runs.
                if (all_balanced)
                begin
                    outcmd = tagged Invalid;
                    checkBalanceQ.enq(?);
                end
            end

            tagged COM_Step:
            begin
                state <= LC_Stepping;
            end

            tagged COM_Pause .send_response:
            begin
                state <= LC_Idle;
            end

            tagged COM_Resume .send_response:
            begin
                state <= LC_Running;
            end

            // TODO: should this be COM_EnableInstance??
            tagged COM_EnableContext .iid:
            begin
                if (dynamicActive)
                begin
                    maxActiveInstance.up();
                end
            end

            // TODO: should this be COM_DisableInstance??
            tagged COM_DisableContext .iid:
            begin
                if (dynamicActive)
                begin
                    maxActiveInstance.down();
                end
            end

            tagged COM_Scan:
            begin
                scanning <= True;
                outcmd = tagged Invalid;

                let scan_data = CONTROLLER_SCAN_DATA {
                   outctrls: map(canWriteTo, outctrls),
                   unctrls: map(canReadFrom, uncontrolled_ctrls),
                   inctrls: map(canReadFrom, inctrls),
                   cycle: cycle,
                   nextInstance: nextInstance.value(),
                   nextIsReady: instanceReadyW,
                   name: nameUID };

`ifdef ENABLE_FOR_TESTING
                $display("Dump " + encodedName);
                $display("  next instance: %0d", scan_data.nextInstance);
                $display("  next is ready: %0d", pack(scan_data.nextIsReady));
                $display("  cycle:         %0d", cycle);
                $display("  inctrls:  0x%h", pack(scan_data.inctrls));
                $display("  unctrls:  0x%h", pack(scan_data.unctrls));
                $display("  outctrls: 0x%h", pack(scan_data.outctrls));
`endif

                scanStream.enq(scan_data);
            end

            tagged COM_TestThroughput:
            begin
                // Begin a throughput test.
                tpState <= CTRL_TP_Warmup0;
                tpCounterResetW.send();
                outcmd = tagged Invalid;
            end
        endcase

        // Forward command around the ring
        if (outcmd matches tagged Valid .cmd)
        begin
            link_controllers.sendToNext(cmd);
        end
    endrule

    //
    // updateStateForStepping --
    //     State update associated with startModelCycle, encoded in a rule
    //     and controlled by a wire in order to set scheduling priority.
    //
    RWire#(t_IID) newModelCycleStartedW <- mkRWire();

    (* descending_urgency = "updateStateForStepping, nextCommand" *)
    rule updateStateForStepping (state == LC_Stepping &&&
                                 newModelCycleStartedW.wget() matches tagged Valid .iid);
        if (iid == maxActiveInstance.value())
            state <= LC_Idle;
    endrule


    // ====================================================================
    //
    //   Scan state for debugging.
    //
    // ====================================================================

    Reg#(Maybe#(Bit#(TLog#(TAdd#(t_NUM_INSTANCES, 1))))) scanRunning <- mkReg(tagged Invalid);

    //
    // emitScan --
    //     First stage of scan:  marshall the CONTROLLER_SCAN_DATA through
    //     the channel.
    //
    rule emitScan (scanning && ! isValid(scanRunning));
        if (scanStream.notEmpty)
        begin
            if (! scanStream.isLast)
                link_controllers.sendToNext(tagged LC_ScanData scanStream.first());
            else
                link_controllers.sendToNext(tagged LC_ScanDataLast scanStream.first());

            scanStream.deq();
        end
        else
        begin
            // Done with CONTROLLER_SCAN_DATA.  Now send a serialized vector
            // of running contexts.  These are stored in LUTRAM, so must use
            // only a single read port.
            scanRunning <= tagged Valid 0;
        end
    endrule


    //
    // emitScanRunning --
    //     Second stage of scan:  emit a serial stream of flags indicating which
    //     instances are currently running.  The running flags are in LUTRAM
    //     and can not be retrieved in parallel.
    //
    rule emitScanRunning (scanning &&& scanRunning matches tagged Valid .iid);
        let is_running = isRunning(truncateNP(iid));

        if (iid < fromInteger(valueOf(TSub#(t_NUM_INSTANCES, 1))))
        begin
            // Emit is_running for one instance
            link_controllers.sendToNext(tagged LC_ScanRunning is_running);
            scanRunning <= tagged Valid (iid + 1);
        end
        else if (iid == fromInteger(valueOf(TSub#(t_NUM_INSTANCES, 1))))
        begin
            // Highest instance ID
            link_controllers.sendToNext(tagged LC_ScanRunningLast is_running);
            scanRunning <= tagged Valid (iid + 1);
        end
        else
        begin
            // Scan done
            link_controllers.sendToNext(tagged COM_Scan);
            scanning <= False;
            scanRunning <= tagged Invalid;
        end
    endrule


    // ====================================================================
    //
    //   Throughput testing
    //
    // ====================================================================

    (* no_implicit_conditions, fire_when_enabled *)
    rule tpCounterUpdate (True);
        if (tpCounterResetW)
            tpCounter <= 0;
        else if (tpCounter != maxBound)
            tpCounter <= tpCounter + 1;
    endrule

    //
    // tpFwdSamples --
    //     Send samples to host.
    //
    rule tpFwdSamples (! scanning);
        let smpl = tpSampleQ.first();
        tpSampleQ.deq();
        
        link_controllers.sendToNext(tagged LC_ThroughputData resize(smpl));
    endrule

    //
    // tpBeginWarmup --
    //     Warmup0 lasts only until the instance ID wraps to 0.
    //     Enter Warmup1, holding this controller idle while other
    //     controllers run.  The goal is to fill up this controller's
    //     input ports and drain the output ports.
    //
    (* no_implicit_conditions, fire_when_enabled *)
    rule tpStateWarmup ((tpState == CTRL_TP_Warmup0) && tpInstance0W &&
                        (tpCounter != maxBound));
        tpState <= CTRL_TP_Warmup1;
        tpCounterResetW.send();
    endrule

    //
    // tpAbort --
    //     A controller may be inactive and never fire.  Give up.
    //
    (* fire_when_enabled *)
    rule tpAbort ((tpState == CTRL_TP_Warmup0) && (tpCounter == maxBound)
                  && ! scanning);
        tpState <= CTRL_TP_Finish1;
        link_controllers.sendToNext(tagged LC_ThroughputLast);
    endrule

    //
    // tpStartSampling --
    //     A state machine transition that doesn't fit the predicate for most
    //     transitions.  The switch from warmup to sampling begins when the
    //     tpCounter saturates.
    //
    (* no_implicit_conditions, fire_when_enabled *)
    rule tpStartSampling ((tpState == CTRL_TP_Warmup1) && (tpCounter == maxBound));
        tpState <= CTRL_TP_Sample;
        tpCounterResetW.send();
    endrule

    //
    // tpEndSampling --
    //     Finish sampling after one complete iteration through all
    //     active contexts.
    //
    (* no_implicit_conditions, fire_when_enabled *)
    rule tpEndSampling ((tpState == CTRL_TP_Sample) && tpInstance0W);
        tpState <= CTRL_TP_Finish0;
    endrule

    //
    // tpLastSample --
    //     Send a final sample to the host indicating the end of testing
    //     for this controller.
    //
    rule tpLastSample ((tpState == CTRL_TP_Finish0) && ! scanning &&
                       ! tpSampleQ.notEmpty);
        tpState <= CTRL_TP_Finish1;
        link_controllers.sendToNext(tagged LC_ThroughputLast);
    endrule

    //
    // tpNextControll --
    //     This controller has finished sampling.  Forward the request to the
    //     next controller.
    //
    rule tpNextController ((tpState == CTRL_TP_Finish1) && ! scanning);
        tpState <= CTRL_TP_Idle;
        link_controllers.sendToNext(tagged COM_TestThroughput);
    endrule


    // ====================================================================
    //
    //   Next instance management.  Implemented in rules instead of
    //   startModelCycle() method for better scheduling control.
    //
    // ====================================================================

    RWire#(Tuple4#(t_IID, LC_STATE, CTRL_TP_STATE, t_IID)) startW <- mkRWire();

    (* descending_urgency = "tpAbort, tpFwdSamples, tpNextController, tpLastSample, checkBalance, newControlMsg, nextCommand, nextStart" *)
    rule nextStart ((state != LC_Idle) && instanceReadyW);

        let next_iid = nextInstance.value();
        // Most of the state is passed here to allow scheduling of read/write
        // order wrt startModelCycle().  Reading the state in the method
        // triggers a large set of scheduling order warnings.
        startW.wset(tuple4(next_iid, state, tpState, maxActiveInstance.value()));

    endrule


    // ====================================================================
    //
    //   Methods
    //
    // ====================================================================

    method ActionValue#(t_IID) startModelCycle() if (startW.wget() matches tagged Valid {.next_iid, .w_state, .w_tpState, .max_iid});

        if (w_state == LC_Stepping)
        begin
            newModelCycleStartedW.wset(next_iid);
        end
        
        Reg#(Bit#(1)) running_s = instanceRunningS.getReg(next_iid);
        running_s <= running_s ^ 1;
        
        if (next_iid >= max_iid)
        begin
            nextInstance.setC(0);
            cycle <= cycle + 1;
            tpInstance0W.send();
        end
        else
        begin
            nextInstance.up();
        end

        // Throughput sampling
        tpCounterResetW.send();
        if (w_tpState == CTRL_TP_Sample)
        begin
            tpSampleQ.enq(tpCounter);
        end

        return next_iid;

    endmethod

    method Action endModelCycle(t_IID iid, Bit#(8) path);
    
        Reg#(Bit#(1)) running_e = instanceRunningE.getReg(iid);
        running_e <= running_e ^ 1;

        // Put the path into the waveform (for debugging).
        pathDoneW <= path;
    
    endmethod

    method Action instanceDone(t_IID iid, Bool pf);
        // XXX this should be per-instance.  For now only allowed to fire once.
        if (! signalDone)
        begin
            newCtrlMsgQ.enq(tagged LC_DoneRunning pf);
            signalDone <= True;
        end
    endmethod
    
    method INSTANCE_ID#(t_NUM_INSTANCES) getMaxActiveInstance();
        return maxActiveInstance.value();
    endmethod

    method Action setMaxActiveInstance(INSTANCE_ID#(t_NUM_INSTANCES) maxIID);
        maxActiveInstance.setC(maxIID);
    endmethod

endmodule


// ========================================================================
//
//   Emit details of the controller and ports to a file that can be
//   used to generate a dot graph of the system.
//
// ========================================================================

module [HASIM_MODULE] emitPortGraphFile#(
    String name,
    Vector#(t_NUM_INPORTS,  INSTANCE_CONTROL_IN#(t_NUM_INSTANCES))  inctrls, 
    Vector#(t_NUM_UNPORTS,  INSTANCE_CONTROL_IN#(t_NUM_INSTANCES))  uncontrolled_ctrls,
    Vector#(t_NUM_OUTPORTS, INSTANCE_CONTROL_OUT#(t_NUM_INSTANCES)) outctrls)
    // Interface:
    ()
    provisos (Add#(t_NUM_INPORTS, t_NUM_UNPORTS, n_ALL_INPORTS));

    //
    // Need to decide whether to write a new file or append to an existing
    // one.  The first time this module is invoked during a compilation it
    // should write.  Define a global string to flag the mode.
    //
    String tag = "__emitPortGraphFile_" + genPackageName + "__";
    let tag_uid <- lookupGlobalString(tag);
    Bool first_call = ! isValid(tag_uid);
    if (first_call)
    begin
        let dummy <- getGlobalStringUID(tag);
    end

    Handle hdl <- openFile(genPackageName + ".ctrl",
                           first_call ? WriteMode : AppendMode);
    hPutStrLn(hdl, "#");
    hPutStrLn(hdl, "# Controller " + name + ": " +
                   integerToString(valueOf(t_NUM_INPORTS)) + " in, " +
                   integerToString(valueOf(t_NUM_UNPORTS)) + " uncontrolled, " +
                   integerToString(valueOf(t_NUM_OUTPORTS)) + " out.");
    hPutStrLn(hdl, "#");

    Vector#(n_ALL_INPORTS, INSTANCE_CONTROL_IN#(t_NUM_INSTANCES)) all_inports =
        Vector::append(inctrls, uncontrolled_ctrls);
    if (valueOf(n_ALL_INPORTS) != 0)
    begin
        hPutStrLn(hdl, "");
        for (Integer i = 0; i < valueOf(n_ALL_INPORTS); i = i + 1)
        begin
            List#(PORT_INFO) port_info = all_inports[i].portInfo();
            while (port_info != List::nil)
            begin
                let p = List::head(port_info);

                if (p.name != "")
                begin
                    hPutStrLn(hdl, "I," + name + "," + p.name + "," +
                              integerToString(p.latency));
                end

                port_info = List::tail(port_info);
            end
        end
    end

    if (valueOf(t_NUM_OUTPORTS) != 0)
    begin
        hPutStrLn(hdl, "");
        for (Integer i = 0; i < valueOf(t_NUM_OUTPORTS); i = i + 1)
        begin
            List#(String) names = outctrls[i].portName();
            while (names != List::nil)
            begin
                let p = List::head(names);

                if (p != "")
                begin
                    hPutStrLn(hdl, "O," + name + "," + p);
                end

                names = List::tail(names);
            end
        end
    end

    hPutStrLn(hdl, "");
    hClose(hdl);
endmodule
