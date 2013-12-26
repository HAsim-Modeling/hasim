//
// Copyright (C) 2010 Massachusetts Institute of Technology
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
// Tokens are the main way for HAsim to track data across simulator      
// partitions. The token type includes an index for token tables, epochs,
// and scratchpads which partitions can use as they see fit.             

import FIFOF::*;
import Vector::*;
import ConfigReg::*;


`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/fpga_components.bsh"
`include "asim/provides/hasim_modellib.bsh"

//
// mkPortRecv_Multiplexed_ReorderSideBuffer --
//     Rotate the multiplexing of ports to be offset by the parameters.
//     This makes it possible for a network model to be implemented with
//     a single multiplexed instance.  The permutation here allows a
//     multiplexed, ordered traversal of network stations while reordering
//     messages so that they are delivered to the proper instances.
//
//     The period parameter for a 2D torus/mesh is the width of the mesh.
//     (Assuming multiplexed nodes are ordered width then height.)
//
//     numPeriods is typically the size of the dimension opposite the
//     one traversed.  (I.e. numPeriods is height for width-wise networks
//     and numPeriods is width for hight-wise traversals.)  In practice,
//     numPeriods could be computed as (run-time IIDs / period).
//     FPGAs aren't good at division by arbitrary numbers, so we pass
//     in numPeriods instead.
//
module [CONNECTED_MODULE] mkPortRecv_Multiplexed_ReorderSideBuffer
    #(
        String portname, 
        Integer latency, 
        INSTANCE_ID#(t_NUM_INSTANCES) period,
        INSTANCE_ID#(t_NUM_INSTANCES) numPeriods,
        function Bool enqToSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid),
        function Bool resetEnq(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid),
        function Bool deqFromSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid),
        function Bool resetDeq(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid)
    )
    // Interface:
    (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         NumAlias#(TMul#(t_NUM_INSTANCES, PORT_MAX_LATENCY), n_SLOTS),
         Alias#(Bit#(TLog#(n_SLOTS)), t_SLOT_IDX),
         Bits#(t_SLOT_IDX, t_SLOT_IDX_SZ),
         Alias#(t_ENTRY, PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG)),
         Bits#(t_ENTRY, t_ENTRY_SZ));

    CONNECTION_RECV#(t_ENTRY) con <- mkPortRecv_MaybeCompressed(portname);

    Reg#(INSTANCE_ID#(t_NUM_INSTANCES)) maxInstance <- mkReg(fromInteger(valueof(t_NUM_INSTANCES) - 1));

    if (latency > valueOf(PORT_MAX_LATENCY))
    begin
        error("Latency exceeds current maximum. Port: " + portname);
    end

    //
    // FIFOs hold main and side buffers.  Like A-Ports, we pick either distributed
    // memory or block RAM depending on the sizes.
    //
    FIFOF#(t_ENTRY) mainQ;
    FIFOF#(t_ENTRY) sideQ;

    if ((valueOf(n_SLOTS) >= 256) &&
        (valueOf(n_SLOTS) * valueOf(t_ENTRY_SZ) > 14000))
    begin
        mainQ <- mkSizedBRAMFIFOF(valueOf(n_SLOTS));
        sideQ <- mkSizedBRAMFIFOF(valueOf(n_SLOTS));
    end
    else
    begin
        mainQ <- mkSizedFIFOF(valueOf(n_SLOTS));
        sideQ <- mkSizedFIFOF(valueOf(n_SLOTS));
    end

    Reg#(INSTANCE_ID#(t_NUM_INSTANCES)) curEnq <- mkReg(0);
    Reg#(INSTANCE_ID#(t_NUM_INSTANCES)) curDeq <- mkReg(0);

    Bool canEnq = enqToSide(curEnq, maxInstance) ? sideQ.notFull  : mainQ.notFull;


    //
    // Initialization -- the FIFOs must be filled with no messages for the first
    // simulated cycle.
    //

    Reg#(Bool) mainInitDone <- mkReg(False);
    Reg#(t_SLOT_IDX) mainInitIdx <- mkRegU();
    Reg#(Maybe#(t_SLOT_IDX)) mainInitMax <- mkReg(tagged Invalid);

    Reg#(Bool) sideInitDone <- mkReg(False);
    Reg#(t_SLOT_IDX) sideInitIdx <- mkRegU();
    Reg#(Maybe#(t_SLOT_IDX)) sideInitMax <- mkReg(tagged Invalid);

    function Bool initialized = (mainInitDone && sideInitDone);

    rule initMain (mainInitMax matches tagged Valid .init_max);
        INSTANCE_ID#(t_NUM_INSTANCES) iid = truncateNP(mainInitIdx);
        mainQ.enq(PORT_MULTIPLEXED_MSG { iid: iid, msg: tagged Invalid });

        // Done?
        if (mainInitIdx == init_max)
        begin
            mainInitMax <= tagged Invalid;
            mainInitDone <= True;
        end

        mainInitIdx <= mainInitIdx + 1;
    endrule

    rule initSide (sideInitMax matches tagged Valid .init_max);
        INSTANCE_ID#(t_NUM_INSTANCES) iid = truncateNP(sideInitIdx);
        sideQ.enq(PORT_MULTIPLEXED_MSG { iid: iid, msg: tagged Invalid });

        // Done?
        if (sideInitIdx == init_max)
        begin
            sideInitMax <= tagged Invalid;
            sideInitDone <= True;
        end

        sideInitIdx <= sideInitIdx + 1;
    endrule


    FIFO#(INSTANCE_ID#(t_NUM_INSTANCES)) triggerInitQ <- mkFIFO1();

    (* descending_urgency = "triggerInit, initMain" *)
    (* descending_urgency = "triggerInit, initSide" *)
    rule triggerInit (True);
        let max_iid = triggerInitQ.first();
        triggerInitQ.deq();

        t_SLOT_IDX lat = fromInteger(latency);
        t_SLOT_IDX k = zeroExtendNP(max_iid) + 1;
        t_SLOT_IDX p = zeroExtendNP(period);
        t_SLOT_IDX np = zeroExtendNP(numPeriods);

        mainQ.clear();
        sideQ.clear();

        // Initialize incoming ports with no-messages to fill the latency.
        mainInitIdx <= 0;
        mainInitMax <= tagged Valid ((k - np) * lat - 1);
        mainInitDone <= False;

        sideInitIdx <= 0;
        sideInitMax <= tagged Valid (p * lat - 1);
        sideInitDone <= False;

        maxInstance <= max_iid;
    endrule



    //
    // shift --
    //   Forward data from incoming ports into local buffers.
    //
    rule shift (initialized && canEnq && con.notEmpty());
        let m = con.receive();
        con.deq();

        if (enqToSide(curEnq, maxInstance))
        begin
            sideQ.enq(m);
        end
        else
        begin
            mainQ.enq(m);
        end
        
        if (resetEnq(curEnq, maxInstance))
        begin
            curEnq <= 0;
        end
        else
        begin
            curEnq <= curEnq + 1;
        end
    endrule

    
    //
    // fetchNext --
    //   Pick the next entry to fetch and forward to output queue.
    //
    FIFOF#(t_ENTRY) outputQ <- mkFIFOF();

    rule fetchNext (initialized);
        t_ENTRY m;

        if (deqFromSide(curDeq, maxInstance))
        begin
            // Return the side buffer.
            m = sideQ.first();
            sideQ.deq();
        end
        else
        begin
            // Return the main buffer.
            m = mainQ.first();
            mainQ.deq();
        end

        if (resetDeq(curDeq, maxInstance))
        begin
            curDeq <= 0;
        end
        else
        begin
            curDeq <= curDeq + 1;
        end

        outputQ.enq(m);
    endrule


    interface INSTANCE_CONTROL_IN ctrl;
        method Bool empty() = ! outputQ.notEmpty;
        method Bool balanced() = True;
        method Bool light() = False;
        
        method Maybe#(INSTANCE_ID#(t_NUM_INSTANCES)) nextReadyInstance();
            return (outputQ.notEmpty ? tagged Valid outputQ.first().iid :
                                       tagged Invalid);
        endmethod
        
        method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) maxIID);
            triggerInitQ.enq(maxIID);
        endmethod
        
        method List#(PORT_INFO) portInfo() =
            list(PORT_INFO {name: portname, latency: latency});
    endinterface

    method ActionValue#(Maybe#(t_MSG)) receive(INSTANCE_ID#(t_NUM_INSTANCES) dummy);
        let m = outputQ.first();
        outputQ.deq();

        return m.msg;
    endmethod
endmodule


module [CONNECTED_MODULE] mkPortRecv_Multiplexed_ReorderFirstToLast#(String portname, Integer latency)
    // interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ));
         
    function Bool enqToSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq == 0;
    endfunction

    function Bool deqFromSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq == max_iid;
    endfunction
    
    function Bool resetEnq(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq == max_iid;
    endfunction
    
    function Bool resetDeq(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq == max_iid;
    endfunction
    
    let p <- mkPortRecv_Multiplexed_ReorderSideBuffer(portname, latency, 1, 1, enqToSide, resetEnq, deqFromSide, resetDeq);
    return p;

endmodule


module [CONNECTED_MODULE] mkPortRecv_Multiplexed_ReorderLastToFirst#(String portname, Integer latency)
    // interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ));
         
    function Bool enqToSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq == max_iid;
    endfunction

    function Bool deqFromSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq == 0;
    endfunction
    
    function Bool resetEnq(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq == max_iid;
    endfunction
    
    function Bool resetDeq(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq == max_iid;
    endfunction
    
    let p <- mkPortRecv_Multiplexed_ReorderSideBuffer(portname, latency, 1, 1, enqToSide, resetEnq, deqFromSide, resetDeq);
    return p;

endmodule

module [CONNECTED_MODULE] mkPortRecv_Multiplexed_ReorderFirstToLastEveryN#(
    String portname,
    Integer latency,
    INSTANCE_ID#(t_NUM_INSTANCES) period,
    INSTANCE_ID#(t_NUM_INSTANCES) numPeriods)
    // interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ));
         
    function Bool enqToSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq == 0;
    endfunction

    function Bool deqFromSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq == period - 1;
    endfunction
    
    function Bool resetEnq(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq == (period - 1);
    endfunction
    
    function Bool resetDeq(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq == (period - 1);
    endfunction
    
    let p <- mkPortRecv_Multiplexed_ReorderSideBuffer(portname, latency, period, numPeriods, enqToSide, resetEnq, deqFromSide, resetDeq);
    return p;

endmodule


module [CONNECTED_MODULE] mkPortRecv_Multiplexed_ReorderLastToFirstEveryN#(
    String portname,
    Integer latency,
    INSTANCE_ID#(t_NUM_INSTANCES) period,
    INSTANCE_ID#(t_NUM_INSTANCES) numPeriods)
    // interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ));
         
    function Bool enqToSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq == period - 1;
    endfunction

    function Bool deqFromSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq == 0;
    endfunction
    
    function Bool resetEnq(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq == (period - 1);
    endfunction
    
    function Bool resetDeq(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq == (period - 1);
    endfunction
    
    let p <- mkPortRecv_Multiplexed_ReorderSideBuffer(portname, latency, period, numPeriods, enqToSide, resetEnq, deqFromSide, resetDeq);
    return p;

endmodule


module [CONNECTED_MODULE] mkPortRecv_Multiplexed_ReorderFirstNToLastN#(
    String portname,
    Integer latency,
    INSTANCE_ID#(t_NUM_INSTANCES) period)
    // interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ));
         
    function Bool enqToSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq < period;
    endfunction

    function Bool deqFromSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq > (max_iid - period);
    endfunction
    
    function Bool resetEnq(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq == max_iid;
    endfunction
    
    function Bool resetDeq(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq == max_iid;
    endfunction
    
    let p <- mkPortRecv_Multiplexed_ReorderSideBuffer(portname, latency, period, period, enqToSide, resetEnq, deqFromSide, resetDeq);
    return p;

endmodule


module [CONNECTED_MODULE] mkPortRecv_Multiplexed_ReorderLastNToFirstN#(
    String portname,
    Integer latency,
    INSTANCE_ID#(t_NUM_INSTANCES) period)
    // interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ));
         
    function Bool enqToSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq > (max_iid - period);
    endfunction

    function Bool deqFromSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq < period;
    endfunction
    
    function Bool resetEnq(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq == max_iid;
    endfunction
    
    function Bool resetDeq(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq == max_iid;
    endfunction
    
    let p <- mkPortRecv_Multiplexed_ReorderSideBuffer(portname, latency, period, period, enqToSide, resetEnq, deqFromSide, resetDeq);
    return p;

endmodule


// ========================================================================
//
//   Mapping functions to convert one or more port interfaces into another.
//
// ========================================================================

//
// mkPortRecv_Multiplexed_Substr --
//     Export a region of an existing multiplexed receive port as an
//     interface with a smaller number of multiplexed ports.  Any
//     unaddressable instances are assumed to be unused.
//
module [CONNECTED_MODULE] mkPortRecv_Multiplexed_Substr#(
        PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES_IN, t_MSG) p)
    // interface:
    (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES_OUT, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         Add#(t_NUM_INSTANCES_OUT, t_NUM_UNUSED, t_NUM_INSTANCES_IN));

    interface INSTANCE_CONTROL_IN ctrl;
        method Bool empty() = p.ctrl.empty;
        method Bool balanced() = p.ctrl.balanced;
        method Bool light() = p.ctrl.light;
        
        method Maybe#(INSTANCE_ID#(t_NUM_INSTANCES_OUT)) nextReadyInstance();
            if (p.ctrl.nextReadyInstance() matches tagged Valid .iid)
                return tagged Valid truncateNP(iid);
            else
                return tagged Invalid;
        endmethod
        
        method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES_OUT) iid) =
            p.ctrl.setMaxRunningInstance(zeroExtendNP(iid));
        
        method List#(PORT_INFO) portInfo() = p.ctrl.portInfo;
    endinterface

    method ActionValue#(Maybe#(t_MSG)) receive(INSTANCE_ID#(t_NUM_INSTANCES_OUT) dummy) =
        p.receive(zeroExtendNP(dummy));
endmodule


//
// mkPortSend_Multiplexed_Substr --
//     Export a region of an existing multiplexed receive port as an
//     interface with a smaller number of multiplexed ports.  Any
//     unaddressable instances are assumed to be unused.
//
module [CONNECTED_MODULE] mkPortSend_Multiplexed_Substr#(
        PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES_IN, t_MSG) p)
    // interface:
    (PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES_OUT, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         Add#(t_NUM_INSTANCES_OUT, t_NUM_UNUSED, t_NUM_INSTANCES_IN));
    
    interface INSTANCE_CONTROL_OUT ctrl;
        method Bool full() = p.ctrl.full;
        method Bool balanced() = p.ctrl.balanced;
        method Bool heavy() = p.ctrl.heavy;

        method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES_OUT) iid) =
            p.ctrl.setMaxRunningInstance(zeroExtendNP(iid));
        
        method List#(String) portName() = p.ctrl.portName;
    endinterface

    method Action send(INSTANCE_ID#(t_NUM_INSTANCES_OUT) dummy,
                       Maybe#(t_MSG) msg) =
        p.send(zeroExtendNP(dummy), msg);
endmodule



//
// mkPortRecv_Multiplexed_Join2 --
//     Join a pair of receive ports into a single logical receive port.
//     The portMap parameter indicates the true source port, indexed
//     by instance ID.
//
module [CONNECTED_MODULE] mkPortRecv_Multiplexed_Join2#(
        PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES0, t_MSG) p0,
        PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES1, t_MSG) p1,
        LUTRAM#(t_JOIN_ID, Bit#(1)) sourceMap)
    // interface:
    (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         NumAlias#(t_NUM_INSTANCES, TAdd#(t_NUM_INSTANCES0, t_NUM_INSTANCES1)),
         NumAlias#(n_JOIN_ID_BITS, INSTANCE_ID_BITS#(t_NUM_INSTANCES)),
         Alias#(t_JOIN_ID, INSTANCE_ID#(t_NUM_INSTANCES)),
         // Tautology to keep Bluespec happy
         Log#(TMax#(TAdd#(TAdd#(t_NUM_INSTANCES0, t_NUM_INSTANCES1), 0), 2),
              TLog#(TMax#(TAdd#(t_NUM_INSTANCES0, t_NUM_INSTANCES1), 2))));

    PORT_RECV_MULTIPLEXED#(0, t_MSG) dummy2 <- mkPortRecv_Multiplexed_NULL();
    PORT_RECV_MULTIPLEXED#(0, t_MSG) dummy3 <- mkPortRecv_Multiplexed_NULL();

    let s <- mkPortRecv_Multiplexed_Join4_Impl(p0, p1, dummy2, dummy3, sourceMap);
    return s;
endmodule


//
// mkPortRecv_Multiplexed_Join3 --
//     Same as mkPortRecv_Multiplexed_Join2 but with 3 input ports.
//
module [CONNECTED_MODULE] mkPortRecv_Multiplexed_Join3#(
        PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES0, t_MSG) p0,
        PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES1, t_MSG) p1,
        PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES2, t_MSG) p2,
        LUTRAM#(t_JOIN_ID, Bit#(2)) sourceMap)
    // interface:
    (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         NumAlias#(t_NUM_INSTANCES, TAdd#(TAdd#(t_NUM_INSTANCES0,
                                                t_NUM_INSTANCES1),
                                          t_NUM_INSTANCES2)),
         NumAlias#(n_JOIN_ID_BITS, INSTANCE_ID_BITS#(t_NUM_INSTANCES)),
         Alias#(t_JOIN_ID, INSTANCE_ID#(t_NUM_INSTANCES)),
         // Tautology to keep Bluespec happy
         Log#(TMax#(TAdd#(TAdd#(t_NUM_INSTANCES0, t_NUM_INSTANCES1),
                          TAdd#(t_NUM_INSTANCES2, 0)), 2),
              TLog#(TMax#(TAdd#(TAdd#(t_NUM_INSTANCES0, t_NUM_INSTANCES1),
                                t_NUM_INSTANCES2), 2))));

    PORT_RECV_MULTIPLEXED#(0, t_MSG) dummy3 <- mkPortRecv_Multiplexed_NULL();

    let s <- mkPortRecv_Multiplexed_Join4_Impl(p0, p1, p2, dummy3, sourceMap);
    return s;
endmodule


//
// mkPortRecv_Multiplexed_Join4 --
//     Same as mkPortRecv_Multiplexed_Join2 but with 4 input ports.
//
module [CONNECTED_MODULE] mkPortRecv_Multiplexed_Join4#(
        PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES0, t_MSG) p0,
        PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES1, t_MSG) p1,
        PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES2, t_MSG) p2,
        PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES3, t_MSG) p3,
        LUTRAM#(t_JOIN_ID, Bit#(2)) sourceMap)
    // interface:
    (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         NumAlias#(t_NUM_INSTANCES, TAdd#(TAdd#(t_NUM_INSTANCES0,
                                                t_NUM_INSTANCES1),
                                          TAdd#(t_NUM_INSTANCES2,
                                                t_NUM_INSTANCES3))),
         NumAlias#(n_JOIN_ID_BITS, INSTANCE_ID_BITS#(t_NUM_INSTANCES)),
         Alias#(t_JOIN_ID, INSTANCE_ID#(t_NUM_INSTANCES)));

    let s <- mkPortRecv_Multiplexed_Join4_Impl(p0, p1, p2, p3, sourceMap);
    return s;
endmodule


//
// mkPortRecv_Multiplexed_Join4_Impl --
//     Base implementation of multiplexed joiner.
//
module [CONNECTED_MODULE] mkPortRecv_Multiplexed_Join4_Impl#(
        PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES0, t_MSG) p0,
        PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES1, t_MSG) p1,
        PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES2, t_MSG) p2,
        PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES3, t_MSG) p3,
        LUTRAM#(t_JOIN_ID, Bit#(t_MAP_IDX_SZ)) sourceMap)
    // interface:
    (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         NumAlias#(t_NUM_INSTANCES, TAdd#(TAdd#(t_NUM_INSTANCES0,
                                                t_NUM_INSTANCES1),
                                          TAdd#(t_NUM_INSTANCES2,
                                                t_NUM_INSTANCES3))),
         // Map index may be smaller for joins with dummy ports.
         Add#(t_MAP_IDX_SZ, t_MAP_IDX_UNUSED, 2),
         Alias#(t_JOIN_ID, INSTANCE_ID#(t_NUM_INSTANCES)));

    Reg#(t_JOIN_ID) curIdx <- mkConfigReg(0);
    Reg#(t_JOIN_ID) maxRunningInstance <- mkReg(0);

    Reg#(Bool) initialized <- mkReg(False);


    //
    // At initialization count the number in use of each port and pass
    // that number down to each setMaxRunningInstance.
    //
    Reg#(Bool) doInit <- mkReg(False);
    Reg#(t_JOIN_ID) initIdx <- mkReg(0);
    // All counters start at -1 since the value passed in is the max. ID
    Reg#(Vector#(4, t_JOIN_ID)) nRunningPerPort <- mkReg(replicate(~0));

    rule initRunningPerPort (doInit && ! initialized);
        // Update count based on port used by initIdx
        let n_running = nRunningPerPort;
        Bit#(2) p = zeroExtend(sourceMap.sub(initIdx));
        n_running[p] = n_running[p] + 1;

        // Done with all indices in use?
        if (initIdx == maxRunningInstance)
        begin
            // Yes.  Initialize all subordinate ports.
            p0.ctrl.setMaxRunningInstance(truncateNP(n_running[0]));
            p1.ctrl.setMaxRunningInstance(truncateNP(n_running[1]));
            p2.ctrl.setMaxRunningInstance(truncateNP(n_running[2]));
            p3.ctrl.setMaxRunningInstance(truncateNP(n_running[3]));

            // In case initialization triggered again.
            nRunningPerPort <= replicate(~0);
            initIdx <= 0;

            // Ready to go.
            doInit <= False;
            initialized <= True;
        end
        else
        begin
            // No
            nRunningPerPort <= n_running;
            initIdx <= initIdx + 1;
        end
    endrule


    //
    // If the next source has data send the source port ID on deqFrom.
    //
    Wire#(Maybe#(Bit#(2))) deqFrom <- mkDWire(tagged Invalid);

    rule computeDeqSrc (initialized);
        Bit#(2) cur_port = zeroExtend(sourceMap.sub(curIdx));
        Bool empty = case (cur_port)
                         0: p0.ctrl.empty;
                         1: p1.ctrl.empty;
                         2: p2.ctrl.empty;
                         3: p3.ctrl.empty;
                     endcase;

        deqFrom <= (empty ? tagged Invalid : tagged Valid cur_port);
    endrule


    interface INSTANCE_CONTROL_IN ctrl;
        method Bool empty() = ! isValid(deqFrom);
        method Bool balanced() = p0.ctrl.balanced && p1.ctrl.balanced;
        method Bool light() = p0.ctrl.light || p1.ctrl.light;
        
        method Maybe#(t_JOIN_ID) nextReadyInstance();
            return (isValid(deqFrom)) ? tagged Invalid :
                                        tagged Valid curIdx;
        endmethod
        
        method Action setMaxRunningInstance(t_JOIN_ID iid) if (! doInit);
            maxRunningInstance <= iid;
            doInit <= True;
        endmethod
        
        method List#(PORT_INFO) portInfo() =
            List::append(p0.ctrl.portInfo, p1.ctrl.portInfo);
    endinterface

    method ActionValue#(Maybe#(t_MSG)) receive(t_JOIN_ID dummy) if (deqFrom matches tagged Valid .cur_port);
        Maybe#(t_MSG) msg = tagged Invalid;    

        case (cur_port)
            0: msg <- p0.receive(?);
            1: msg <- p1.receive(?);
            2: msg <- p2.receive(?);
            3: msg <- p3.receive(?);
        endcase

        curIdx <= (curIdx == maxRunningInstance) ? 0 : curIdx + 1;

        return msg;
    endmethod
endmodule


//
// mkPortSend_Multiplexed_Split2 --
//     The opposite of join:  split a logically linear multiplexed send into
//     a pair of multiplexed ports.
//
module [CONNECTED_MODULE] mkPortSend_Multiplexed_Split2#(
        PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES0, t_MSG) p0,
        PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES1, t_MSG) p1,
        LUTRAM#(t_SPLIT_ID, Bit#(1)) destMap)
    // interface:
    (PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         NumAlias#(t_NUM_INSTANCES, TAdd#(t_NUM_INSTANCES0, t_NUM_INSTANCES1)),
         NumAlias#(n_SPLIT_ID_BITS, INSTANCE_ID_BITS#(t_NUM_INSTANCES)),
         Alias#(t_SPLIT_ID, INSTANCE_ID#(t_NUM_INSTANCES)),
         // Tautology to keep Bluespec happy
         Log#(TMax#(TAdd#(TAdd#(t_NUM_INSTANCES0, t_NUM_INSTANCES1), 0), 2),
              TLog#(TMax#(TAdd#(t_NUM_INSTANCES0, t_NUM_INSTANCES1), 2))));

    PORT_SEND_MULTIPLEXED#(0, t_MSG) dummy2 <- mkPortSend_Multiplexed_NULL();
    PORT_SEND_MULTIPLEXED#(0, t_MSG) dummy3 <- mkPortSend_Multiplexed_NULL();

    let s <- mkPortSend_Multiplexed_Split4_Impl(p0, p1, dummy2, dummy3, destMap);
    return s;
endmodule

//
// mkPortSend_Multiplexed_Split3 --
//     Same as mkPortSend_Multiplexed_Split2 but with three output ports.
//
module [CONNECTED_MODULE] mkPortSend_Multiplexed_Split3#(
        PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES0, t_MSG) p0,
        PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES1, t_MSG) p1,
        PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES2, t_MSG) p2,
        LUTRAM#(t_SPLIT_ID, Bit#(2)) destMap)
    // interface:
    (PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         NumAlias#(t_NUM_INSTANCES, TAdd#(TAdd#(t_NUM_INSTANCES0,
                                                t_NUM_INSTANCES1),
                                          t_NUM_INSTANCES2)),
         NumAlias#(n_SPLIT_ID_BITS, INSTANCE_ID_BITS#(t_NUM_INSTANCES)),
         Alias#(t_SPLIT_ID, INSTANCE_ID#(t_NUM_INSTANCES)),
         // Tautology to keep Bluespec happy
         Log#(TMax#(TAdd#(TAdd#(t_NUM_INSTANCES0, t_NUM_INSTANCES1),
                          TAdd#(t_NUM_INSTANCES2, 0)), 2),
              TLog#(TMax#(TAdd#(TAdd#(t_NUM_INSTANCES0, t_NUM_INSTANCES1),
                                t_NUM_INSTANCES2), 2))));

    PORT_SEND_MULTIPLEXED#(0, t_MSG) dummy3 <- mkPortSend_Multiplexed_NULL();

    let s <- mkPortSend_Multiplexed_Split4_Impl(p0, p1, p2, dummy3, destMap);
    return s;
endmodule

//
// mkPortSend_Multiplexed_Split4 --
//     Same as mkPortSend_Multiplexed_Split2 but with four output ports.
//
module [CONNECTED_MODULE] mkPortSend_Multiplexed_Split4#(
        PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES0, t_MSG) p0,
        PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES1, t_MSG) p1,
        PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES2, t_MSG) p2,
        PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES3, t_MSG) p3,
        LUTRAM#(t_SPLIT_ID, Bit#(2)) destMap)
    // interface:
    (PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         NumAlias#(t_NUM_INSTANCES, TAdd#(TAdd#(t_NUM_INSTANCES0,
                                                t_NUM_INSTANCES1),
                                          TAdd#(t_NUM_INSTANCES2,
                                                t_NUM_INSTANCES3))),
         NumAlias#(n_SPLIT_ID_BITS, INSTANCE_ID_BITS#(t_NUM_INSTANCES)),
         Alias#(t_SPLIT_ID, INSTANCE_ID#(t_NUM_INSTANCES)));

    let s <- mkPortSend_Multiplexed_Split4_Impl(p0, p1, p2, p3, destMap);
    return s;
endmodule


//
// mkPortSend_Multiplexed_Split4_Impl --
//     Base implementation of multiplexed splitter.
//
module [CONNECTED_MODULE] mkPortSend_Multiplexed_Split4_Impl#(
        PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES0, t_MSG) p0,
        PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES1, t_MSG) p1,
        PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES2, t_MSG) p2,
        PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES3, t_MSG) p3,
        LUTRAM#(t_SPLIT_ID, Bit#(t_MAP_IDX_SZ)) destMap)
    // interface:
    (PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         NumAlias#(t_NUM_INSTANCES, TAdd#(TAdd#(t_NUM_INSTANCES0,
                                                t_NUM_INSTANCES1),
                                          TAdd#(t_NUM_INSTANCES2,
                                                t_NUM_INSTANCES3))),
         // Map index may be smaller for joins with dummy ports.
         Add#(t_MAP_IDX_SZ, t_MAP_IDX_UNUSED, 2),
         Alias#(t_SPLIT_ID, INSTANCE_ID#(t_NUM_INSTANCES)));

    Reg#(t_SPLIT_ID) curIdx <- mkConfigReg(0);
    Reg#(t_SPLIT_ID) maxRunningInstance <- mkReg(0);

    Reg#(Bool) initialized <- mkReg(False);
    
    //
    // At initialization count the number in use of each port and pass
    // that number down to each setMaxRunningInstance.
    //
    Reg#(Bool) doInit <- mkReg(False);
    Reg#(t_SPLIT_ID) initIdx <- mkReg(0);
    // All counters start at -1 since the value passed in is the max. ID
    Reg#(Vector#(4, t_SPLIT_ID)) nRunningPerPort <- mkReg(replicate(~0));

`ifdef FOOBAR
    rule initRunningPerPort (doInit && ! initialized);
        // Update count based on port used by initIdx
        let n_running = nRunningPerPort;
        Bit#(2) p = zeroExtend(destMap.sub(initIdx));
        n_running[p] = n_running[p] + 1;

        // Done with all indices in use?
        if (initIdx == maxRunningInstance)
        begin
            // Yes.  Initialize all subordinate ports.
            p0.ctrl.setMaxRunningInstance(truncateNP(n_running[0]));
            p1.ctrl.setMaxRunningInstance(truncateNP(n_running[1]));
            p2.ctrl.setMaxRunningInstance(truncateNP(n_running[2]));
            p3.ctrl.setMaxRunningInstance(truncateNP(n_running[3]));

            // In case initialization triggered again.
            nRunningPerPort <= replicate(~0);
            initIdx <= 0;

            // Ready to go.
            doInit <= False;
            initialized <= True;
        end
        else
        begin
            // No
            nRunningPerPort <= n_running;
            initIdx <= initIdx + 1;
        end
    endrule
`endif


    //
    // If the next destination is not full send the dest port ID on enqTo.
    //
    Wire#(Maybe#(Bit#(2))) enqTo <- mkDWire(tagged Invalid);

    rule computeEnqDst (initialized);
        Bit#(2) cur_port = zeroExtend(destMap.sub(curIdx));
        Bool full = case (cur_port)
                         0: p0.ctrl.full;
                         1: p1.ctrl.full;
                         2: p2.ctrl.full;
                         3: p3.ctrl.full;
                     endcase;

        enqTo <= (full ? tagged Invalid : tagged Valid cur_port);
    endrule

    
    interface INSTANCE_CONTROL_OUT ctrl;
        method Bool full() = ! isValid(enqTo);
        method Bool balanced() = p0.ctrl.balanced && p1.ctrl.balanced;
        method Bool heavy() = p0.ctrl.heavy || p1.ctrl.heavy;

        method Action setMaxRunningInstance(t_SPLIT_ID iid) if (! doInit);
            maxRunningInstance <= iid;
            initialized <= True;
//            doInit <= True;
        endmethod

        method List#(String) portName() =
            List::append(p0.ctrl.portName, p1.ctrl.portName);
    endinterface

    method Action send(t_SPLIT_ID dummy, Maybe#(t_MSG) msg) if (enqTo matches tagged Valid .cur_port);
        case (cur_port)
            0: p0.send(?, msg);
            1: p1.send(?, msg);
            2: p2.send(?, msg);
            3: p3.send(?, msg);
        endcase

        curIdx <= (curIdx == maxRunningInstance) ? 0 : curIdx + 1;
    endmethod
endmodule



//
// mkPortRecv_Multiplexed_Join --
//     Similar to mkPortRecv_Multiplexed_Join2 above, but merges a multiplexed
//     port with a singleton port.  The insertionPoint indicates the position
//     at which to insert the singleton port.
//
module [CONNECTED_MODULE] mkPortRecv_Multiplexed_Join#(
        PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG) p0,
        PORT_RECV#(t_MSG) p1,
        t_JOIN_ID insertionPoint)
    // interface:
    (PORT_RECV_MULTIPLEXED#(TAdd#(t_NUM_INSTANCES, 1), t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         NumAlias#(INSTANCE_ID_BITS#(TAdd#(t_NUM_INSTANCES, 1)), n_JOIN_ID_BITS),
         Alias#(INSTANCE_ID#(TAdd#(t_NUM_INSTANCES, 1)), t_JOIN_ID));

    COUNTER#(n_JOIN_ID_BITS) cur <- mkLCounter(0);
    Reg#(t_JOIN_ID) maxRunningInstance <- mkReg(0);
    Reg#(Bool) initialized <- mkReg(False);
    
    Bool canDeq = (cur.value() == insertionPoint) ? ! p1.ctrl.empty() :
                                                    ! p0.ctrl.empty();
    
    interface INSTANCE_CONTROL_IN ctrl;
        method Bool empty() = !canDeq;
        method Bool balanced() = True;
        method Bool light() = False;
        
        method Maybe#(t_JOIN_ID) nextReadyInstance();
            return (!canDeq || !initialized) ? tagged Invalid :
                                               tagged Valid cur.value();
        endmethod
        
        method Action setMaxRunningInstance(t_JOIN_ID iid);
            // Local Controller has already added 1 to this number.
            maxRunningInstance <= iid;
            initialized <= True;
        endmethod
        
        method List#(PORT_INFO) portInfo() =
            List::append(p0.ctrl.portInfo, p1.ctrl.portInfo);
    endinterface

    method ActionValue#(Maybe#(t_MSG)) receive(t_JOIN_ID dummy) if (canDeq);
        if (cur.value() == maxRunningInstance)
        begin
            cur.setC(0);
        end
        else
        begin
            cur.up();
        end
        
        if (cur.value() == insertionPoint)
        begin
            let msg <- p1.receive();
            return msg;
        end
        else
        begin
            let msg <- p0.receive(truncateNP(dummy));
            return msg;
        end
    endmethod
endmodule


//
// mkPortSend_Multiplexed_Split --
//     The opposite of join:  split a logically linear multiplexed send into
//     a multiplexed port and a singleton port, with the singleton port slotted
//     at splitPoint.
//
module [CONNECTED_MODULE] mkPortSend_Multiplexed_Split#(
        PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG) p0,
        PORT_SEND#(t_MSG) p1,
        t_SPLIT_ID splitPoint)
    // interface:
        (PORT_SEND_MULTIPLEXED#(TAdd#(t_NUM_INSTANCES, 1), t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         NumAlias#(INSTANCE_ID_BITS#(TAdd#(t_NUM_INSTANCES, 1)), n_SPLIT_ID_BITS),
         Alias#(INSTANCE_ID#(TAdd#(t_NUM_INSTANCES, 1)), t_SPLIT_ID));


    COUNTER#(n_SPLIT_ID_BITS) cur <- mkLCounter(0);
    Reg#(t_SPLIT_ID) maxRunningInstance <- mkReg(0);
    Reg#(Bool) initialized <- mkReg(False);
    
    Bool canEnq = (cur.value() == splitPoint) ? ! p1.ctrl.full() :
                                                ! p0.ctrl.full();
    
    interface INSTANCE_CONTROL_OUT ctrl;
        method Bool full() = !canEnq;
        method Bool balanced() = True;
        method Bool heavy() = False;
        method Action setMaxRunningInstance(t_SPLIT_ID iid);
            // Local controller has already added one to this number
            maxRunningInstance <= iid;
            initialized <= True;
        endmethod

        method List#(String) portName() =
            List::append(p0.ctrl.portName, p1.ctrl.portName);
    endinterface

    method Action send(t_SPLIT_ID dummy, Maybe#(t_MSG) msg) if (initialized && canEnq);
        if (cur.value() == maxRunningInstance)
        begin
            cur.setC(0);
        end
        else
        begin
            cur.up();
        end
        
        if (cur.value() == splitPoint)
        begin
            p1.send(msg);
        end
        else
        begin
            p0.send(truncateNP(dummy), msg);
        end
    endmethod
endmodule
