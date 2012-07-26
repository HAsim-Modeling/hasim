//
// Copyright (C) 2008 Massachusetts Institute of Technology
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

`include "awb/provides/hasim_common.bsh"
`include "awb/provides/soft_connections.bsh"
`include "awb/provides/fpga_components.bsh"
`include "awb/provides/common_services.bsh"
`include "awb/provides/hasim_modellib.bsh"

`include "awb/dict/PARAMS_HASIM_MODELLIB.bsh"

import ConfigReg::*;
import FIFOF::*;
import SpecialFIFOs::*;
import Vector::*;
import HList::*;

typedef `PORT_MAX_LATENCY PORT_MAX_LATENCY;

interface INSTANCE_CONTROL_OUT#(numeric type t_NUM_INSTANCES);

  method Bool full();
  method Bool balanced();
  method Bool heavy();
  method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid);

endinterface

interface INSTANCE_CONTROL_IN#(numeric type t_NUM_INSTANCES);

    method Bool empty();
    method Bool balanced();
    method Bool light();

    method Maybe#(INSTANCE_ID#(t_NUM_INSTANCES)) nextReadyInstance;
    method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid);

endinterface

interface INSTANCE_CONTROL_IN_OUT#(numeric type t_NUM_INSTANCES);

    interface INSTANCE_CONTROL_IN#(t_NUM_INSTANCES) in;
    interface INSTANCE_CONTROL_OUT#(t_NUM_INSTANCES) out;

endinterface


interface PORT_SEND#(type t_MSG);
  
  method Action send(Maybe#(t_MSG) m);
  interface INSTANCE_CONTROL_OUT#(1) ctrl;
  
endinterface

interface PORT_RECV#(type t_MSG);

  method ActionValue#(Maybe#(t_MSG)) receive();
  interface INSTANCE_CONTROL_IN#(1) ctrl;

endinterface

interface PORT_SEND_MULTIPLEXED#(type t_NUM_INSTANCES, type t_MSG);
  
  method Action send(INSTANCE_ID#(t_NUM_INSTANCES) iid, Maybe#(t_MSG) m);
  interface INSTANCE_CONTROL_OUT#(t_NUM_INSTANCES) ctrl;
  
endinterface

interface PORT_RECV_MULTIPLEXED#(type t_NUM_INSTANCES, type t_MSG);

  method ActionValue#(Maybe#(t_MSG)) receive(INSTANCE_ID#(t_NUM_INSTANCES) iid);
  interface INSTANCE_CONTROL_IN#(t_NUM_INSTANCES) ctrl;

endinterface

module [HASIM_MODULE] mkPortSend#(String portname)
    //interface:
        (PORT_SEND#(t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ));
        
    CONNECTION_SEND#(Maybe#(t_MSG)) con <- mkConnectionSend(portname);

    //A temporary set of control info
    interface INSTANCE_CONTROL_OUT ctrl;

        method Bool full() = !con.notFull;
        method Bool balanced() = True;
        method Bool heavy() = False;
        method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid) = noAction;

    endinterface

    method Action send(Maybe#(t_MSG) m);

        con.send(m);

    endmethod
  
endmodule

module [HASIM_MODULE] mkPortRecv#(String portname, Integer latency)
  //interface:
              (PORT_RECV#(t_MSG))
      provisos
                (Bits#(t_MSG, t_MSG_SZ));
  
  let p <- case (latency)
             0: mkPortRecv_L0(portname);
             1: mkPortRecv_L1(portname, tagged Invalid);
             default: mkPortRecv_Buffered(portname, latency, 0, tagged Invalid);
           endcase;
 
  return p;

endmodule

module [HASIM_MODULE] mkPortRecv_Buffered#(String portname, Integer latency, Integer extra_buffering, Maybe#(t_MSG) init_value)
    //interface:
                (PORT_RECV#(t_MSG))
      provisos
                (Bits#(t_MSG, t_MSG_SZ));

  CONNECTION_RECV#(Maybe#(t_MSG)) con <- mkConnectionRecv(portname);
   
  Integer rMax = latency + extra_buffering + 1;
  
  if (rMax > 255)
    error("Total Port buffering cannot currently exceed 255.");
  
  Reg#(Maybe#(t_MSG)) rs[rMax];
  
  for (Integer x = 0; x < rMax; x = x + 1)
    rs[x] <- mkReg(init_value);

  Reg#(Bit#(8)) head <- mkReg(fromInteger(latency));
  Reg#(Bit#(8)) tail <- mkReg(0);
  Bit#(8) numElems = head - tail;
  
  function Bit#(n) overflow_incr(Bit#(n) x);
    
    let tmp = x + 1;
    return (tmp == fromInteger(rMax)) ? 0 : tmp;
  endfunction

  Bool fullQ  = overflow_incr(head) == tail;
  Bool emptyQ = head == tail;
  
  
  rule shift (!fullQ);
  
    let d = con.receive();
    con.deq();
    
    (rs[head._read()]) <= d;
    head <= overflow_incr(head);
   
  endrule
  
  //A temporary set of control info
  interface INSTANCE_CONTROL_IN ctrl;

        method Bool empty() = emptyQ;
        method Bool balanced() = True;
        method Bool light() = False;
        method Maybe#(INSTANCE_ID#(1)) nextReadyInstance = tagged Valid 0;
        method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid);
            noAction;
        endmethod

  endinterface

  method ActionValue#(Maybe#(t_MSG)) receive() if (!emptyQ);
    
    tail <= overflow_incr(tail);
    return rs[tail._read()]._read();
    
  endmethod

endmodule


//Port optimized for latency 0

module [HASIM_MODULE] mkPortRecv_L0#(String portname)
    //interface:
                (PORT_RECV#(t_MSG))
      provisos
                (Bits#(t_MSG, t_MSG_SZ));

  CONNECTION_RECV#(Maybe#(t_MSG)) con <- mkConnectionRecv(portname);
     
  //A temporary set of control info
  interface INSTANCE_CONTROL_IN ctrl;

    method Bool empty() = !con.notEmpty();
    method Bool balanced() = True;
    method Bool light() = False;
    method Maybe#(INSTANCE_ID#(1)) nextReadyInstance() = tagged Valid 0;
    method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid);
        noAction;
    endmethod

  endinterface

  method ActionValue#(Maybe#(t_MSG)) receive();
  
    con.deq();
    return con.receive();
    
  endmethod
  
endmodule

//Port optimized for latency 1

module [HASIM_MODULE] mkPortRecv_L1#(String portname, Maybe#(t_MSG) init_value)
    //interface:
                (PORT_RECV#(t_MSG))
      provisos
                (Bits#(t_MSG, t_MSG_SZ));

  CONNECTION_RECV#(Maybe#(t_MSG)) con <- mkConnectionRecv(portname);
  Reg#(Bool) initializing <- mkReg(True);
     

  // A temporary set of control info
  interface INSTANCE_CONTROL_IN ctrl;

    method Bool empty() = !con.notEmpty;
    method Bool balanced() = True;
    method Bool light() = False;
    method Maybe#(INSTANCE_ID#(1)) nextReadyInstance = tagged Valid 0;
    method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid) = noAction;

  endinterface

  
  method ActionValue#(Maybe#(t_MSG)) receive();
    if (initializing)
    begin
      initializing <= False;
      return init_value;
    end
    else
    begin
      let m = con.receive();
      con.deq();
      return m;
    end
  endmethod

endmodule


module [Connected_Module] mkPortRecvDependent#(String portname)
    // interface:
                (PORT_RECV#(t_MSG))
      provisos
                (Bits#(t_MSG, t_MSG_SZ));
    
    let m <- mkPortRecv_L0(portname);
    return m;
    
endmodule


// ========================================================================
//
//   Multiplexed ports
//
// ========================================================================

typedef Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Maybe#(t_MSG))
    PORT_MULTIPLEXED_MSG#(numeric type t_NUM_INSTANCES, type t_MSG);

module [HASIM_MODULE] mkPortSend_Multiplexed#(String portname)
    //interface:
        (PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ));

    CONNECTION_SEND#(PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG)) con <-
        mkPortSend_MaybeCompressed(portname);

    interface INSTANCE_CONTROL_OUT ctrl;

        method Bool full() = !con.notFull();
        method Bool balanced() = True;
        method Bool heavy() = False;

        // Handled on the receive side only.
        method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid) = noAction;

    endinterface

    method Action send(INSTANCE_ID#(t_NUM_INSTANCES) iid, Maybe#(t_MSG) m);
        con.send(tuple2(iid, m));
    endmethod

endmodule


module [HASIM_MODULE] mkPortRecv_Multiplexed#(String portname, Integer latency)
    //interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ));

    let p <- case (latency)
               //0: mkPortRecvL0_Multiplexed(portname);
               0: mkPortRecvBuffered_Multiplexed(portname, latency);
               default: mkPortRecvBuffered_Multiplexed(portname, latency);
             endcase;

    return p;

endmodule


module [HASIM_MODULE] mkPortRecvBuffered_Multiplexed#(String portname, Integer latency)
    //interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ));

    let p <- mkPortRecv_Multiplexed_Impl(portname, latency, True);
    return p;
endmodule


module [HASIM_MODULE] mkPortRecvL0_Multiplexed#(String portname)
    // interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ));

    let p <- mkPortRecv_Multiplexed_Impl(portname, 0, False);
    return p;
endmodule


module [HASIM_MODULE] mkPortRecvDependent_Multiplexed#(String portname)
    // interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ));
    
    let m <- mkPortRecvL0_Multiplexed(portname);
    return m;
endmodule


//
// Internal implementation of a multiplexed receive port.  Latency has meaning
// only when buffering is enabled.  When disabled, latency must be 0.
//
module [HASIM_MODULE] mkPortRecv_Multiplexed_Impl#(String portname,
                                                   Integer latency,
                                                   Bool buffered)
    //interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         Alias#(PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG), t_BUFFERED_MSG),
         Bits#(t_BUFFERED_MSG, t_BUFFERED_MSG_SZ),
         NumAlias#(TMul#(TMax#(t_NUM_INSTANCES, 1), PORT_MAX_LATENCY), n_SLOTS),
         Alias#(Bit#(TLog#(n_SLOTS)), t_SLOT_IDX));

    CONNECTION_RECV#(PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG)) con <-
        mkPortRecv_MaybeCompressed(portname);

    if (latency > valueOf(PORT_MAX_LATENCY))
    begin
        error("Latency exceeds current maximum. Port: " + portname);
    end

    //
    // The internal buffer is a FIFO.  Here we attempt to pick a reasonable
    // FIFO implementation depending on the size of the buffer.  Large buffers
    // are stored in block RAM.
    //
    // Requiring the number of slots to be 512 or greater would map optimally
    // to block RAM.  We accept inefficiencies in block RAM usage and accept
    // buffers with only 256 entries because it has been important
    // for some of our models.  In addition to the number of entries, the
    // total amount of buffered data must be close to the size of an 18Kb RAM.
    //
    // The size of the FIFO (doubling the number of slots relative to the
    // minimimum requirement of one slot per instance) allows extra buffering
    // for greater cycle variation among pipeline stages.  This flexibility
    // results in significant simulator performance gains, especially for
    // multi-FPGA designs.
    //
    FIFOF#(t_BUFFERED_MSG) rs;
    Integer total_slots = max(1, latency) * max(1, valueOf(TMul#(2, t_NUM_INSTANCES)));
    if (! buffered)
    begin
        rs <- mkBypassFIFOF();
    end
    else if ((total_slots >= 256) &&
        (total_slots * valueOf(t_BUFFERED_MSG_SZ) > 14000))
    begin
        // Large buffer.  Use block RAM.
        rs <- mkSizedBRAMFIFOF(total_slots);
    end
    else
    begin
        // Small buffer.  Use distributed memory.
        rs <- mkSizedFIFOF(total_slots);
    end

    Reg#(Bool) initialized <- mkReg(! buffered);
    Reg#(t_SLOT_IDX) initIdx <- mkReg(0);
    Reg#(Maybe#(t_SLOT_IDX)) initMax <- mkReg(tagged Invalid);
    
    //
    // doInit --
    //   Fill the FIFO with no message corresponding to the initial buffering.
    //
    rule doInit (! initialized &&& initMax matches tagged Valid .max_idx);
        INSTANCE_ID#(t_NUM_INSTANCES) iid = truncateNP(initIdx);
        rs.enq(tuple2(iid, tagged Invalid));

        initialized <= (initIdx == max_idx);
        initIdx <= initIdx + 1;
    endrule

    //
    // shift --
    //   Pass received messages to the FIFO buffer.
    //
    rule shift (initialized && con.notEmpty());
        rs.enq(con.receive());
        con.deq();
    endrule


    interface INSTANCE_CONTROL_IN ctrl;

        method Bool empty() = ! rs.notEmpty();
        method Bool balanced() = True;
        method Bool light() = False;
        
        method Maybe#(INSTANCE_ID#(t_NUM_INSTANCES)) nextReadyInstance();

            if (rs.notEmpty())
            begin
                match {.iid, .m} = rs.first();
                return tagged Valid iid;
            end
            else
            begin
                return tagged Invalid;
            end
        
        endmethod
        
        method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid);
        
            if (buffered)
            begin
                t_SLOT_IDX l = fromInteger(latency);
                t_SLOT_IDX k = zeroExtendNP(iid) + 1;

                if (l == 0)
                begin    
                    // 0-latency port has no initial fill.
                    initialized <= True;
                end
                else
                begin    
                    // Write k * l no-messages to initialize the channel.
                    initMax <= tagged Valid (k * l - 1);
                end
            end

        endmethod

    endinterface

    method ActionValue#(Maybe#(t_MSG)) receive(INSTANCE_ID#(t_NUM_INSTANCES) dummy);

        match {.iid, .m} = rs.first();
        rs.deq();

        return m;

    endmethod

endmodule


// ========================================================================
//
//   Compressor for multiplexed A-Ports
//
// ========================================================================

//
// portIsCompressed --
//    Compressing an intra-FPGA connection is of no value since the link
//    is on direct, dedicated wires that can be written every cycle.
//    For now we use a hack and just enumerate all the inter-FPGA connections
//    that should be compressed.
//

//
// Found cross-FPGA links with:
//   grep -h '//.*via_idx:' ACP*_bitfile/pm/hw/model/multifpga_routing.bsh | sed -e 'sx// xx' -e 's/ via_idx:.*//' -e 's/_chunk_[0-9]*//' | grep -v OldSchoolChain | grep -v ^rrr_ | grep -v ^funcp_ | grep -v ^ScratchpadGlobal | grep -v ^DebugScanRing | grep -v ^stdio_ | grep -v ^Stats | sort
//
List#(String) multiFPGAconnections = list(
    "CPU_to_ICache_load",
    "CPU_to_ICache_load",
    "CPU_to_ITLB_req",
    "CPU_to_ITLB_req",
    "Com_to_Dec_fault",
    "Com_to_Dec_writeback",
    "Com_to_Fet_fault",
    "DMem_to_Dec_hit_writeback",
    "DMem_to_Dec_miss_writeback",
    "Dec_to_SB_alloc",
    "Exe_to_BP_training",
    "Exe_to_BP_training",
    "Exe_to_Dec_mispredict",
    "Exe_to_Dec_writeback",
    "Exe_to_Fet_rewind",
    "ICache_to_CPU_load_delayed",
    "ICache_to_CPU_load_immediate",
    "ICache_to_CPU_load_immediate",
    "ITLB_to_CPU_rsp",
    "ITLB_to_CPU_rsp",
    "IssueQ__cred",
    "IssueQ__portDataEnq",
    "IssueQ__portDataEnq",
    "IssueQ__portDataEnq",
    "SB_to_Dec_credit",
    "model_cycle");

function Bool portIsCompressed(String portname, Integer msgSize);
    function Bool portNameMatches(String n) = (n == portname);

    return (`APORT_COMPRESS_ENABLE != 0) &&
`ifdef APORT_COMPRESS_TIMEOUT_DYNDEFAULT_Z
           (msgSize > 63) &&
`endif
           List::any(portNameMatches, multiFPGAconnections);
endfunction


//
// mkPortSend_MaybeCompressed --
//     Instantiate a soft connection for a multiplexed A-Port.  The connection
//     may be compressed, if requested by portIsCompressed() above.
//
module [HASIM_MODULE] mkPortSend_MaybeCompressed#(String portname)
    // Interface:
    (CONNECTION_SEND#(PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG)))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         Alias#(PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG), t_MUX_MSG),
         Bits#(t_MUX_MSG, t_MUX_MSG_SZ));

    CONNECTION_SEND#(t_MUX_MSG) con;

    if (portIsCompressed(portname, valueOf(t_MUX_MSG_SZ)))
    begin
        con <- mkCompressedConnectionSend(portname);
    end
    else
    begin
        // Uncompressed, basic connection
        con <- mkConnectionSend(portname);
    end

    return con;
endmodule
    

//
// mkPortRecv_MaybeCompressed --
//     Instantiate a soft connection for a multiplexed A-Port.  The connection
//     may be compressed, if requested by portIsCompressed() above.
//
module [HASIM_MODULE] mkPortRecv_MaybeCompressed#(String portname)
    // Interface:
    (CONNECTION_RECV#(PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG)))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         Alias#(PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG), t_MUX_MSG),
         Bits#(t_MUX_MSG, t_MUX_MSG_SZ));

    CONNECTION_RECV#(t_MUX_MSG) con;

    if (portIsCompressed(portname, valueOf(t_MUX_MSG_SZ)))
    begin
        con <- mkCompressedConnectionRecv(portname);
    end
    else
    begin
        // Uncompressed, basic connection
        con <- mkConnectionRecv(portname);
    end

    return con;
endmodule


// ========================================================================
//
//   Two compression schemes are implemented below for testing.  One
//   merely takes advantage of the Maybe#() no-message.  The other
//   merges contiguous no-messages to reduce traffic.
//
// ========================================================================

`ifdef APORT_COMPRESS_TIMEOUT_DYNDEFAULT_Z

//
// Compress the multiplexed port internal message by taking advantage
// of the Maybe#() in the no-message case, sending only one bit.
//

instance CompressMC#(PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG),
                     // The encoded message (just moves fields around)
                     HList2#(t_MSG, Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Bool)),
                     // The "decoder" needed to figure out the message size
                     Bool,
                     CONNECTED_MODULE)
    provisos (Alias#(PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG), t_DATA),
              Bits#(t_DATA, t_DATA_SZ),
              Bits#(t_MSG, t_MSG_SZ),
              NumAlias#(INSTANCE_ID_BITS#(t_NUM_INSTANCES), t_IID_SZ),
              Alias#(t_ENC_DATA, HList2#(t_MSG, Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Bool))),
              Bits#(t_ENC_DATA, t_ENC_DATA_SZ));

    module [CONNECTED_MODULE] mkCompressorMC
        // Interface:
        (COMPRESSION_ENCODER#(t_DATA, t_ENC_DATA));

        FIFOF#(t_DATA) inQ <- mkBypassFIFOF();

        method enq(t_DATA val) = inQ.enq(val);
        method notFull() = inQ.notFull();

        method first();
            let val = inQ.first();

            // Extract fields
            match {.iid, .msg} = val;

            // Compute the compressed message length (in bits).  Add 1 for the
            // tag bit.
            Integer data_len = 1 + valueOf(t_IID_SZ) +
                               (isValid(msg) ? valueOf(t_MSG_SZ) : 0);

            // The message is compressed by moving the tag to the low bit so it
            // will be next to the useful data.  The 2nd element in the returned
            // tuple is the compressed length.
            return tuple2(hList2(validValue(msg), tuple2(iid, isValid(msg))),
                          data_len);
        endmethod

        method deq() = inQ.deq();
        method notEmpty() = inQ.notEmpty();
    endmodule

    module [CONNECTED_MODULE] mkDecompressorMC
        // Interface:
        (COMPRESSION_DECODER#(t_DATA, t_ENC_DATA, Bool));

        FIFOF#(t_ENC_DATA) inQ <- mkBypassFIFOF();

        method Action enq(cval) = inQ.enq(cval);
        method Bool notFull() = inQ.notFull();

        method t_DATA first();
            let cval = inQ.first();

            let msg_data = hHead(cval);
            let iid = tpl_1(hLast(cval));
            let maybe = tpl_2(hLast(cval));

            if (maybe)
                return tuple2(iid, tagged Valid msg_data);
            else
                return tuple2(iid, tagged Invalid);
        endmethod

        method Action deq() = inQ.deq();
        method Bool notEmpty() = inQ.notEmpty();

        method Integer numInBits(maybe);
            return (maybe) ? valueOf(t_ENC_DATA_SZ) : 1 + valueOf(t_IID_SZ);
        endmethod
    endmodule
endinstance

`else

//
// Compress the multiplexed port internal message by merging contiguous
// no-messages and also taking advantage of the Maybe#() in the no-message
// case.
//

// Size of the merged no-message counter
typedef 6 NOMSG_CNT_SZ;

typedef struct
{
    // "payload" is either the message, when "maybe" is true or a
    // NOMSG_CNT_SZ bit no-message count when "maybe" is false.
    Bit#(TMax#(t_MSG_SZ, NOMSG_CNT_SZ)) payload;
    INSTANCE_ID#(t_NUM_INSTANCES) iid;
    Bool maybe;
}
PORT_MULTIPLEXED_COMPRESSED_MSG#(numeric type t_NUM_INSTANCES,
                                 numeric type t_MSG_SZ)
    deriving (Eq, Bits);

instance CompressMC#(PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG),
                     // The encoded message:
                     // We don't know which is larger: the message or the
                     // no-message count.  The low portion of the two connections
                     // is the maybe bit, the iid and the smaller of the
                     // message and the no-message count.  The high portion
                     // is the remainder of either the message or the no-message
                     // count, depending on which is larger.
                     HList2#(Bit#(TSub#(t_LARGER_PAYLOAD_SZ, t_SMALLER_PAYLOAD_SZ)),
                             Tuple3#(Bit#(t_SMALLER_PAYLOAD_SZ),
                                     INSTANCE_ID#(t_NUM_INSTANCES),
                                     Bool)),
                     // The "decoder" needed to figure out the message size
                     Bool,
                     CONNECTED_MODULE)
    provisos (Alias#(PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG), t_DATA),
              Bits#(t_DATA, t_DATA_SZ),
              Bits#(t_MSG, t_MSG_SZ),
              NumAlias#(INSTANCE_ID_BITS#(t_NUM_INSTANCES), t_IID_SZ),
              Alias#(t_ENC_DATA,
                     HList2#(Bit#(TSub#(t_LARGER_PAYLOAD_SZ, t_SMALLER_PAYLOAD_SZ)),
                             Tuple3#(Bit#(t_SMALLER_PAYLOAD_SZ),
                                     INSTANCE_ID#(t_NUM_INSTANCES),
                                     Bool))),
              Bits#(t_ENC_DATA, t_ENC_DATA_SZ),
              // A more convenient internal representation of the compressed
              // type.  It can be converted to the HList using pack/unpack.
              Alias#(PORT_MULTIPLEXED_COMPRESSED_MSG#(t_NUM_INSTANCES, t_MSG_SZ), t_ENC),
              // Assert that the internal type is the same size as the exported
              // compressed type.
              Bits#(t_ENC, t_ENC_DATA_SZ),
              // Compute larger & smaller of message and no-message counter
              Max#(t_MSG_SZ, NOMSG_CNT_SZ, t_LARGER_PAYLOAD_SZ),
              Min#(t_MSG_SZ, NOMSG_CNT_SZ, t_SMALLER_PAYLOAD_SZ));

    module [CONNECTED_MODULE] mkCompressorMC
        // Interface:
        (COMPRESSION_ENCODER#(t_DATA, t_ENC_DATA));

        FIFOF#(t_DATA) inQ <- mkBypassFIFOF();
        FIFOF#(t_ENC_DATA) outQ <- mkFIFOF();

        // Compressor state
        Reg#(Maybe#(INSTANCE_ID#(t_NUM_INSTANCES))) compStartIID <- mkReg(tagged Invalid);
        Reg#(INSTANCE_ID#(t_NUM_INSTANCES)) compLastIID <- mkRegU();
        Reg#(UInt#(NOMSG_CNT_SZ)) compNoMsgCount <- mkRegU();

        // Permit run-time limit on timeout so we can determine optimum value
        PARAMETER_NODE paramNode <- mkDynamicParameterNode();
        Param#(10) paramMaxWaitCycles <-
            mkDynamicParameter(`PARAMS_HASIM_MODELLIB_APORT_COMPRESS_TIMEOUT, paramNode);
        Reg#(Bit#(10)) compTimeOut <- mkConfigReg(0);


        function t_ENC_DATA encodeNoMessages(INSTANCE_ID#(t_NUM_INSTANCES) iid,
                                             UInt#(NOMSG_CNT_SZ) cnt);
            PORT_MULTIPLEXED_COMPRESSED_MSG#(t_NUM_INSTANCES, t_MSG_SZ) cmp;
            cmp.payload = zeroExtendNP(pack(cnt));
            cmp.iid = iid;
            cmp.maybe = False;

            return unpack(pack(cmp));
        endfunction

        function t_ENC_DATA encodeMessage(INSTANCE_ID#(t_NUM_INSTANCES) iid,
                                          t_MSG msg);
            PORT_MULTIPLEXED_COMPRESSED_MSG#(t_NUM_INSTANCES, t_MSG_SZ) cmp;
            cmp.payload = zeroExtendNP(pack(msg));
            cmp.iid = iid;
            cmp.maybe = True;

            return unpack(pack(cmp));
        endfunction


        //
        // Timeout logic:
        //     Increment saturating compTimeOut counter.  When saturated a decision
        //     is forced in timeOut below to flush the current compressed NoMessage
        //     state.
        //
        let noMsgCompressorTimeout = (compTimeOut == paramMaxWaitCycles);

        (* fire_when_enabled *)
        rule timeOutIncr (compTimeOut != paramMaxWaitCycles);
            compTimeOut <= compTimeOut + 1;
        endrule

        rule timeOut (outQ.notFull && noMsgCompressorTimeout);
            compTimeOut <= 0;

            if (compStartIID matches tagged Valid .nomsg_iid)
            begin
                outQ.enq(encodeNoMessages(nomsg_iid, compNoMsgCount));
                compStartIID <= tagged Invalid;
            end
        endrule

        //
        // compressInQ --
        //     Convert the incoming message stream into a compressed stream by
        //     merging groups of no-message.
        //
        (* conservative_implicit_conditions *)
        rule compressInQ (outQ.notFull && ! noMsgCompressorTimeout);
            match {.iid, .m} = inQ.first();

            if (m matches tagged Valid .msg)
            begin
                // Incoming message is valid.  Are there buffered no-messages?
                if (compStartIID matches tagged Valid .nomsg_iid)
                begin
                    // Yes.  First send the buffered set.
                    outQ.enq(encodeNoMessages(nomsg_iid, compNoMsgCount));
                    compStartIID <= tagged Invalid;
                end
                else
                begin
                    // No.  Send the incoming message.
                    outQ.enq(encodeMessage(iid, msg));
                    inQ.deq();
                end
            end
            else
            begin
                // Incoming no-message.  Try to compress it.

                // Will definitely consume inQ this cycle
                inQ.deq();

                // Are there already buffered no-messages?
                if (compStartIID matches tagged Valid .nomsg_iid)
                begin
                    // To be merged, IIDs must be contiguous and the count can't
                    // overflow.
                    if ((compNoMsgCount != maxBound) && (iid == compLastIID + 1))
                    begin
                        compNoMsgCount <= compNoMsgCount + 1;
                        compLastIID <= iid;
                    end
                    else
                    begin
                        // Can't merge.  Send the previous compressor state and
                        // start a new region.
                        outQ.enq(encodeNoMessages(nomsg_iid, compNoMsgCount));

                        compStartIID <= tagged Valid iid;
                        compLastIID <= iid;
                        compNoMsgCount <= 0;
                    end
                end
                else
                begin
                    // This is the first no-message.
                    compStartIID <= tagged Valid iid;
                    compLastIID <= iid;
                    compNoMsgCount <= 0;
                end
            end
        endrule


        method enq(t_DATA val) = inQ.enq(val);
        method notFull() = inQ.notFull();

        method first();
            let val = outQ.first();
            Bool maybe = tpl_3(hLast(val));

            // Compute the compressed message length (in bits).  Add 1 for the
            // tag bit.
            Integer data_len = 1 + valueOf(t_IID_SZ) +
                               (maybe ? valueOf(t_MSG_SZ) :
                                        valueOf(NOMSG_CNT_SZ));

            return tuple2(val, data_len);
        endmethod

        method deq() = outQ.deq();
        method notEmpty() = outQ.notEmpty();
    endmodule


    module [CONNECTED_MODULE] mkDecompressorMC
        // Interface:
        (COMPRESSION_DECODER#(t_DATA, t_ENC_DATA, Bool));

        FIFOF#(t_ENC_DATA) inQ <- mkBypassFIFOF();
        FIFOF#(t_DATA) outQ <- mkBypassFIFOF();

        Reg#(Maybe#(INSTANCE_ID#(t_NUM_INSTANCES))) compIID <- mkReg(tagged Invalid);
        Reg#(UInt#(NOMSG_CNT_SZ)) compNoMsgCount <- mkRegU();

        //
        // decompress --
        //   Decompress runs of no-message.
        //
        rule decompress (compIID matches tagged Valid .iid);
            outQ.enq(tuple2(iid, tagged Invalid));

            let cnt = compNoMsgCount - 1;
            compNoMsgCount <= cnt;

            // Done if cnt is 0, otherwise prepare for next instance.
            compIID <= (cnt == 0) ? tagged Invalid : tagged Valid (iid + 1);
        endrule

        //
        // shift --
        //   Pass received messages to the FIFO buffer.
        //
        rule shift (inQ.notEmpty() && ! isValid(compIID));
            t_ENC val = unpack(pack(inQ.first()));
            inQ.deq();

            if (val.maybe)
            begin
                outQ.enq(tuple2(val.iid,
                                tagged Valid unpack(truncateNP(val.payload))));
            end
            else
            begin
                // One more more no-messages
                outQ.enq(tuple2(val.iid, tagged Invalid));

                // Counter is 0-based
                UInt#(NOMSG_CNT_SZ) cnt = unpack(truncateNP(val.payload));
                if (cnt != 0)
                begin
                    compIID <= tagged Valid (val.iid + 1);
                    compNoMsgCount <= cnt;
                end
            end
        endrule


        method Action enq(cval) = inQ.enq(cval);
        method Bool notFull() = inQ.notFull();

        method t_DATA first() = outQ.first();
        method Action deq() = outQ.deq();
        method Bool notEmpty() = outQ.notEmpty();

        method Integer numInBits(maybe);
            return 1 + valueOf(t_IID_SZ) +
                   (maybe ? valueOf(t_MSG_SZ) : valueOf(NOMSG_CNT_SZ));
        endmethod
    endmodule
endinstance

`endif
