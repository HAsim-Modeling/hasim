//
// Copyright (C) 2008 Intel Corporation
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
//

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// LocalController.bsv                                                       //
//                                                                           //
// Local Controller instantiated by timing modules.                          //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

import Vector::*;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;

// Project imports

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/hasim_modellib.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/soft_services_deps.bsh"
`include "asim/provides/fpga_components.bsh"

`include "asim/dict/RINGID.bsh"

// Project imports

`include "asim/provides/hasim_common.bsh"

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
    Bit#(4) cycle;                     // Small cycle counter to track relative
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
    CONTEXT_ID    COM_EnableContext;  // Enable context
    CONTEXT_ID    COM_DisableContext; // Disable context

    void          COM_Scan;           // Scan state for debugging

    // Messages from local controllers
    Bool          LC_DoneRunning;     // Bool is run passed

    COM_SCAN_DATA LC_ScanData;        // Response to COM_Scan request
    COM_SCAN_DATA LC_ScanDataLast;    // Last chunk of COM_Scan response

    Bool          LC_ScanRunning;     // Serial scan of instanceRunning state
                                      // (also triggered by COM_Scan request)
    Bool          LC_ScanRunningLast; // Last instance of running state scan
}
CONTROLLER_MSG
    deriving (Eq, Bits);

                
// t_NUM_INSTANCES is number of instances to control.
interface LOCAL_CONTROLLER#(type t_NUM_INSTANCES);

    method ActionValue#(INSTANCE_ID#(t_NUM_INSTANCES)) startModelCycle();
    method Action endModelCycle(INSTANCE_ID#(t_NUM_INSTANCES) iid, Bit#(8) path);
    method Action instanceDone(INSTANCE_ID#(t_NUM_INSTANCES) iid, Bool passfail);

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

    let m <- mkNamedLocalControllerWithActive("[no name]", 0, inctrls, empty_unctrls, outctrls);
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

    let m <- mkNamedLocalControllerWithActive(name, 0, inctrls, empty_unctrls, outctrls);
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

    let m <- mkNamedLocalControllerWithActive("[no name]", 0, inctrls, uncontrolled_ctrls, outctrls);
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

    let m <- mkNamedLocalControllerWithActive(name, 0, inctrls, uncontrolled_ctrls, outctrls);
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
module [HASIM_MODULE] mkNamedLocalControllerWithActive

    // parameters:
    #(
    String name,
    Integer startingActive,
    Vector#(t_NUM_INPORTS,  INSTANCE_CONTROL_IN#(t_NUM_INSTANCES))  inctrls, 
    Vector#(t_NUM_UNPORTS,  INSTANCE_CONTROL_IN#(t_NUM_INSTANCES))  uncontrolled_ctrls,
    Vector#(t_NUM_OUTPORTS, INSTANCE_CONTROL_OUT#(t_NUM_INSTANCES)) outctrls
    )
    // interface:
        (LOCAL_CONTROLLER#(t_NUM_INSTANCES))
    provisos (Alias#(CONTROLLER_SCAN_DATA#(t_NUM_INPORTS,
                                           t_NUM_UNPORTS,
                                           t_NUM_OUTPORTS,
                                           INSTANCE_ID_BITS#(t_NUM_INSTANCES)), t_SCAN_DATA));

    Reg#(LC_STATE) state <- mkReg(LC_Idle);
    Reg#(Bool) scanning <- mkReg(False);
  
    // Small cycle counter counts wrapping of the IID and is useful when debugging
    // to compare the relative cycles of local controllers.
    Reg#(Bit#(4)) cycle <- mkReg(0);

    // Counter of active instances. 
    // We start at -1, so we assume at least one instance is active.
    COUNTER#(INSTANCE_ID_BITS#(t_NUM_INSTANCES))
        maxActiveInstance <- mkLCounter(~0 + fromInteger(startingActive));

    // Vector of running instances
    MULTIPLEXED_REG#(t_NUM_INSTANCES, Bool) instanceRunning <- mkMultiplexedReg(False);

    // Signalled DONE to the software?
    Reg#(Bool) signalDone <- mkReg(False);

    FIFOF#(INSTANCE_ID#(t_NUM_INSTANCES)) startCycleQ <- mkBypassFIFOF();
    FIFOF#(INSTANCE_ID#(t_NUM_INSTANCES)) endCycleQ <- mkBypassFIFOF();
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

    
    // For now this local controller just goes round-robin over the instances.
    // This is guaranteed to be correct accross multiple modules.
    // The performance of this could be improved, but the interaction with time-multiplexed
    // ports needs to be worked out.
    
    COUNTER#(INSTANCE_ID_BITS#(t_NUM_INSTANCES)) nextInstance <- mkLCounter(0);
    
    Connection_Chain#(CONTROLLER_MSG) link_controllers <- mkConnection_Chain(`RINGID_CONTROLLER_MESSAGES);

    // Can this module read from this Port?
    function Bool canReadFrom(INSTANCE_CONTROL_IN#(t_NUM_INSTANCES) ctrl_in);
        return case (state)
                   LC_Running:        return !ctrl_in.empty();
                   LC_Stepping:       return !ctrl_in.empty();
                   LC_Synchronizing:  return !ctrl_in.light();
                   default:           return False;
               endcase;
    endfunction

    function canWriteTo(INSTANCE_CONTROL_OUT#(t_NUM_INSTANCES) ctrl_out);
        return case (state)
                   LC_Running:        return !ctrl_out.full();
                   LC_Stepping:       return !ctrl_out.full();
                   LC_Synchronizing:  return !ctrl_out.heavy();
                   default:           return False;
               endcase;
    endfunction

    // This function will determine the next instance in a non-round-robin manner when we're ready
    // to go that route. Currently this is unused.

    function Bool instanceReady(INSTANCE_ID#(t_NUM_INSTANCES) iid);
        
        Bool canRead  = True;
        Bool canWrite = True;

        // Can we read/write all of the ports?
        for (Integer x = 0; x < valueOf(t_NUM_INPORTS); x = x + 1)
            canRead = canRead && canReadFrom(inctrls[x]);

        for (Integer x = 0; x < valueOf(t_NUM_OUTPORTS); x = x + 1)
            canWrite = canWrite && canWriteTo(outctrls[x]);

        // An instance is ready to go only if it's been enabled.
        Reg#(Bool) running = instanceRunning.getReg(iid);
        return !running && canRead && canWrite;

    endfunction

    function Bool someInstanceReady();
        
        Bool res = False;

        for (Integer x = 0; x < valueof(t_NUM_INSTANCES); x = x + 1)
        begin
            res = instanceReady(fromInteger(x)) || res;
        end
        
        return res;
    
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

    (* descending_urgency = "checkBalance, newControlMsg, nextCommand" *)
    rule nextCommand (! scanning && (state != LC_Stepping));
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

            // TODO: should this be COM_EnableInstance??
            tagged COM_EnableContext .iid:
            begin
                maxActiveInstance.up();
            end

            // TODO: should this be COM_DisableInstance??
            tagged COM_DisableContext .iid:
            begin
                maxActiveInstance.down();
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
                   nextIsReady: instanceReady(nextInstance.value()),
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
    Wire#(Maybe#(INSTANCE_ID#(t_NUM_INSTANCES))) newModelCycleStarted <- mkDWire(tagged Invalid);

    (* descending_urgency = "updateStateForStepping, nextCommand" *)
    rule updateStateForStepping (state == LC_Stepping &&&
                                 newModelCycleStarted matches tagged Valid .iid);
        if (iid == maxActiveInstance.value())
            state <= LC_Idle;
    endrule


    rule updateRunning (True);
    
        if (startCycleQ.notEmpty())
        begin
            instanceRunning.getReg(startCycleQ.first()) <= True;
            startCycleQ.deq();
        end
        else if (endCycleQ.notEmpty())
        begin
            instanceRunning.getReg(endCycleQ.first()) <= False;
            endCycleQ.deq();
        end
    
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
        Reg#(Bool) is_running = instanceRunning.getReg(truncateNP(iid));

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
    //   Methods
    //
    // ====================================================================

    method ActionValue#(INSTANCE_ID#(t_NUM_INSTANCES)) startModelCycle() if ((state != LC_Idle) && instanceReady(nextInstance.value()));

        let next_iid = nextInstance.value();

        if (state == LC_Stepping)
        begin
            newModelCycleStarted <= tagged Valid next_iid;
        end
        
        startCycleQ.enq(next_iid);
        
        if (next_iid >= maxActiveInstance.value())
        begin
            nextInstance.setC(0);
            cycle <= cycle + 1;
        end
        else
        begin
            nextInstance.up();
        end

        return next_iid;

    endmethod

    method Action endModelCycle(INSTANCE_ID#(t_NUM_INSTANCES) iid, Bit#(8) path);
    
        endCycleQ.enq(iid);
        pathDoneW <= path; // Put the path into the waveform.
    
    endmethod

    method Action instanceDone(INSTANCE_ID#(t_NUM_INSTANCES) iid, Bool pf);
        // XXX this should be per-instance.  For now only allowed to fire once.
        if (! signalDone)
        begin
            newCtrlMsgQ.enq(tagged LC_DoneRunning pf);
            signalDone <= True;
        end
    endmethod
    
endmodule
