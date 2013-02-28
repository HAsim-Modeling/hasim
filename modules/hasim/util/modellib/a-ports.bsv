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
import List::*;
import HList::*;

typedef `PORT_MAX_LATENCY PORT_MAX_LATENCY;

typedef struct
{
    String name;
    Integer latency;
}
PORT_INFO
    deriving (Eq);

interface INSTANCE_CONTROL_OUT#(numeric type t_NUM_INSTANCES);
    method Bool full();
    method Bool balanced();
    method Bool heavy();
    method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid);

    // Most controllers will have only a single entry for the name.
    // A few controllers are the combination of individual ports.
    method List#(String) portName();
endinterface

interface INSTANCE_CONTROL_IN#(numeric type t_NUM_INSTANCES);
    method Bool empty();
    method Bool balanced();
    method Bool light();

    method Maybe#(INSTANCE_ID#(t_NUM_INSTANCES)) nextReadyInstance;
    method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid);

    // List the OUT version, support complex controllers managing
    // multiple ports.
    method List#(PORT_INFO) portInfo();
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

module [CONNECTED_MODULE] mkPortSend#(String portname)
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

        method List#(String) portName() = list(portname);
    endinterface

    method Action send(Maybe#(t_MSG) m);

        con.send(m);

    endmethod
  
endmodule

module [CONNECTED_MODULE] mkPortRecv#(String portname, Integer latency)
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

module [CONNECTED_MODULE] mkPortRecv_Buffered#(String portname, Integer latency, Integer extra_buffering, Maybe#(t_MSG) init_value)
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

        method List#(PORT_INFO) portInfo() =
            list(PORT_INFO {name: portname, latency: latency});
  endinterface

  method ActionValue#(Maybe#(t_MSG)) receive() if (!emptyQ);
    
    tail <= overflow_incr(tail);
    return rs[tail._read()]._read();
    
  endmethod

endmodule


//Port optimized for latency 0

module [CONNECTED_MODULE] mkPortRecv_L0#(String portname)
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

    method List#(PORT_INFO) portInfo() =
        list(PORT_INFO {name: portname, latency: 0});
  endinterface

  method ActionValue#(Maybe#(t_MSG)) receive();
  
    con.deq();
    return con.receive();
    
  endmethod
  
endmodule

//Port optimized for latency 1

module [CONNECTED_MODULE] mkPortRecv_L1#(String portname, Maybe#(t_MSG) init_value)
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

    method List#(PORT_INFO) portInfo() =
        list(PORT_INFO {name: portname, latency: 1});
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


module [CONNECTED_MODULE] mkPortRecvDependent#(String portname)
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

typedef struct
{
    INSTANCE_ID#(t_NUM_INSTANCES) iid;
    Maybe#(t_MSG) msg;
}
PORT_MULTIPLEXED_MSG#(numeric type t_NUM_INSTANCES, type t_MSG)
    deriving (Eq, Bits);

module [CONNECTED_MODULE] mkPortSend_Multiplexed#(String portname)
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

        method List#(String) portName() = list(portname);
    endinterface

    method Action send(INSTANCE_ID#(t_NUM_INSTANCES) iid, Maybe#(t_MSG) m);
        con.send(PORT_MULTIPLEXED_MSG { iid: iid, msg: m });
    endmethod

endmodule


module [CONNECTED_MODULE] mkPortRecv_Multiplexed#(String portname, Integer latency)
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


module [CONNECTED_MODULE] mkPortRecvBuffered_Multiplexed#(String portname, Integer latency)
    //interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ));

    let p <- mkPortRecv_Multiplexed_Impl(portname, latency, True);
    return p;
endmodule


module [CONNECTED_MODULE] mkPortRecvL0_Multiplexed#(String portname)
    // interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ));

    let p <- mkPortRecv_Multiplexed_Impl(portname, 0, False);
    return p;
endmodule


module [CONNECTED_MODULE] mkPortRecvDependent_Multiplexed#(String portname)
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
module [CONNECTED_MODULE] mkPortRecv_Multiplexed_Impl#(String portname,
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
        rs.enq(PORT_MULTIPLEXED_MSG { iid: iid, msg: tagged Invalid });

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
                let m = rs.first();
                return tagged Valid m.iid;
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

        method List#(PORT_INFO) portInfo() =
            list(PORT_INFO {name: portname, latency: latency});
    endinterface

    method ActionValue#(Maybe#(t_MSG)) receive(INSTANCE_ID#(t_NUM_INSTANCES) dummy);

        let m = rs.first();
        rs.deq();

        return m.msg;

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
module [CONNECTED_MODULE] mkPortSend_MaybeCompressed#(String portname)
    // Interface:
    (CONNECTION_SEND#(PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG)))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         Alias#(PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG), t_MUX_MSG),
         Bits#(t_MUX_MSG, t_MUX_MSG_SZ),
         Compress#(t_MUX_MSG, t_ENC_DATA, CONNECTED_MODULE));

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
module [CONNECTED_MODULE] mkPortRecv_MaybeCompressed#(String portname)
    // Interface:
    (CONNECTION_RECV#(PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG)))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         Alias#(PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG), t_MUX_MSG),
         Bits#(t_MUX_MSG, t_MUX_MSG_SZ),
         Compress#(t_MUX_MSG, t_ENC_DATA, CONNECTED_MODULE));

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


// Messages will be compressed to this tagged union.  The multi-FPGA compiler
// is capable of mapping tagged unions to multiple channels and sending
// only the data required.  A CompressionChunks instance is provided below
// for accomplishing the same, using multiple soft connections, within
// Bluespec.
typedef union tagged
{
    PORT_MULTIPLEXED_CMP_NOMSG#(t_NUM_INSTANCES) NoMSG;
    PORT_MULTIPLEXED_CMP_MSG#(t_NUM_INSTANCES, t_MSG) MSG;
}
PORT_MULTIPLEXED_MSG_COMPRESSED#(numeric type t_NUM_INSTANCES, type t_MSG)
    deriving (Eq);


//
// The compiler fails to derive Bits for PORT_MULTIPLEXED_MSG_COMPRESSED
// because of the complexity of an instance ID.  Give the poor compiler some
// help.
//
instance Bits#(PORT_MULTIPLEXED_MSG_COMPRESSED#(t_NUM_INSTANCES, t_MSG), t_SZ)
    provisos (// The packed size is the tag size plus the max field size
              Add#(t_TAG_SZ, t_MAX_FIELD_SZ, t_SZ),
              NumAlias#(t_TAG_SZ, 1),
              // Sizes of the fields
              Bits#(PORT_MULTIPLEXED_CMP_NOMSG#(t_NUM_INSTANCES), t_NOMSG_SZ),
              Bits#(PORT_MULTIPLEXED_CMP_MSG#(t_NUM_INSTANCES, t_MSG), t_MSG_SZ),
              Max#(t_NOMSG_SZ, t_MSG_SZ, t_MAX_FIELD_SZ));

   function pack(data);
       case (data) matches
           tagged NoMSG .d:
           begin
               Bit#(TAdd#(t_NOMSG_SZ, t_MAX_FIELD_SZ)) b = ?;
               b[valueOf(t_NOMSG_SZ)-1:0] = pack(d);
               return { 1'b0, truncate(b) };
           end

           tagged MSG .d:
           begin
               Bit#(TAdd#(t_MSG_SZ, t_MAX_FIELD_SZ)) b = ?;
               b[valueOf(t_MSG_SZ)-1:0] = pack(d);
               return { 1'b1, truncate(b) };
           end
       endcase
    endfunction

    function unpack(b);
        if (msb(b) == 0)
            return tagged NoMSG unpack(b[valueOf(t_NOMSG_SZ)-1:0]);
        else
            return tagged MSG unpack(b[valueOf(t_MSG_SZ)-1:0]);
    endfunction
endinstance

// Payload sent for actual messages.
typedef struct
{
    t_MSG msg;
    INSTANCE_ID#(t_NUM_INSTANCES) iid;
}
PORT_MULTIPLEXED_CMP_MSG#(numeric type t_NUM_INSTANCES, type t_MSG)
    deriving (Eq, Bits);


`ifdef APORT_COMPRESS_TIMEOUT_DYNDEFAULT_Z

//
// Compress the multiplexed port internal message by taking advantage
// of the Maybe#() in the no-message case, sending only one bit.
//

// Payload sent for no-message.
typedef struct
{
    INSTANCE_ID#(t_NUM_INSTANCES) iid;
}
PORT_MULTIPLEXED_CMP_NOMSG#(numeric type t_NUM_INSTANCES)
    deriving (Eq, Bits);


instance Compress#(PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG),
                   // The encoded message (just moves fields around)
                   PORT_MULTIPLEXED_MSG_COMPRESSED#(t_NUM_INSTANCES, t_MSG),
                   t_MODULE)
    provisos (Alias#(PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG), t_DATA),
              Alias#(PORT_MULTIPLEXED_MSG_COMPRESSED#(t_NUM_INSTANCES, t_MSG), t_ENC_DATA),
              Bits#(t_DATA, t_DATA_SZ),
              Bits#(t_ENC_DATA, t_ENC_DATA_SZ),
              Bits#(t_MSG, t_MSG_SZ),
              NumAlias#(INSTANCE_ID_BITS#(t_NUM_INSTANCES), t_IID_SZ),
              IsModule#(t_MODULE, m__));

    module [t_MODULE] mkCompressor
        // Interface:
        (COMPRESSION_ENCODER#(t_DATA, t_ENC_DATA));

        FIFOF#(t_DATA) inQ <- mkBypassFIFOF();

        method enq(t_DATA val) = inQ.enq(val);
        method notFull() = inQ.notFull();

        method first();
            let m = inQ.first();

            let cmp_msg = isValid(m.msg) ?
                    tagged MSG PORT_MULTIPLEXED_CMP_MSG { msg: validValue(m.msg), iid: m.iid } :
                    tagged NoMSG PORT_MULTIPLEXED_CMP_NOMSG { iid: m.iid };

            return cmp_msg;
        endmethod

        method deq() = inQ.deq();
        method notEmpty() = inQ.notEmpty();
    endmodule

    module [t_MODULE] mkDecompressor
        // Interface:
        (COMPRESSION_DECODER#(t_DATA, t_ENC_DATA));

        FIFOF#(t_ENC_DATA) inQ <- mkBypassFIFOF();

        method Action enq(cval) = inQ.enq(cval);
        method Bool notFull() = inQ.notFull();

        method t_DATA first();
            let cval = inQ.first();

            let val =  case (cval) matches
                           tagged NoMSG .d: PORT_MULTIPLEXED_MSG { iid: d.iid, msg: tagged Invalid };
                           tagged MSG .d: PORT_MULTIPLEXED_MSG { iid: d.iid, msg: tagged Valid d.msg };
                       endcase;

            return val;
        endmethod

        method Action deq() = inQ.deq();
        method Bool notEmpty() = inQ.notEmpty();
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

// No-message details, including the count of contiguous no-messages.
// Instance IDs are assumed to be monotonically incrementing on the receive
// side.  (That this is true is checked on the send side.)
typedef struct
{
    UInt#(NOMSG_CNT_SZ) noMsgCnt;
    INSTANCE_ID#(t_NUM_INSTANCES) iid;
}
PORT_MULTIPLEXED_CMP_NOMSG#(numeric type t_NUM_INSTANCES)
    deriving (Eq, Bits);


instance Compress#(PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG),
                   // The encoded message (just moves fields around)
                   PORT_MULTIPLEXED_MSG_COMPRESSED#(t_NUM_INSTANCES, t_MSG),
                   CONNECTED_MODULE)
    provisos (Alias#(PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG), t_DATA),
              Alias#(PORT_MULTIPLEXED_MSG_COMPRESSED#(t_NUM_INSTANCES, t_MSG), t_ENC_DATA),
              Bits#(t_DATA, t_DATA_SZ),
              Bits#(t_ENC_DATA, t_ENC_DATA_SZ),
              Bits#(t_MSG, t_MSG_SZ),
              NumAlias#(INSTANCE_ID_BITS#(t_NUM_INSTANCES), t_IID_SZ));

    module [CONNECTED_MODULE] mkCompressor
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
            return tagged NoMSG PORT_MULTIPLEXED_CMP_NOMSG { noMsgCnt: cnt,
                                                             iid: iid };
        endfunction

        function t_ENC_DATA encodeMessage(INSTANCE_ID#(t_NUM_INSTANCES) iid,
                                          t_MSG msg);
            return tagged MSG PORT_MULTIPLEXED_CMP_MSG { msg: msg,
                                                         iid: iid };
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
            let m = inQ.first();

            if (m.msg matches tagged Valid .msg)
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
                    outQ.enq(encodeMessage(m.iid, msg));
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
                    if ((compNoMsgCount != maxBound) && (m.iid == compLastIID + 1))
                    begin
                        compNoMsgCount <= compNoMsgCount + 1;
                        compLastIID <= m.iid;
                    end
                    else
                    begin
                        // Can't merge.  Send the previous compressor state and
                        // start a new region.
                        outQ.enq(encodeNoMessages(nomsg_iid, compNoMsgCount));

                        compStartIID <= tagged Valid m.iid;
                        compLastIID <= m.iid;
                        compNoMsgCount <= 0;
                    end
                end
                else
                begin
                    // This is the first no-message.
                    compStartIID <= tagged Valid m.iid;
                    compLastIID <= m.iid;
                    compNoMsgCount <= 0;
                end
            end
        endrule


        method enq(t_DATA val) = inQ.enq(val);
        method notFull() = inQ.notFull();

        method first();
            let val = outQ.first();
            return val;
        endmethod

        method deq() = outQ.deq();
        method notEmpty() = outQ.notEmpty();
    endmodule


    module [CONNECTED_MODULE] mkDecompressor
        // Interface:
        (COMPRESSION_DECODER#(t_DATA, t_ENC_DATA));

        FIFOF#(t_ENC_DATA) inQ <- mkBypassFIFOF();
        FIFOF#(t_DATA) outQ <- mkBypassFIFOF();

        Reg#(Maybe#(INSTANCE_ID#(t_NUM_INSTANCES))) compIID <- mkReg(tagged Invalid);
        Reg#(UInt#(NOMSG_CNT_SZ)) compNoMsgCount <- mkRegU();

        //
        // decompress --
        //   Decompress runs of no-message.
        //
        rule decompress (compIID matches tagged Valid .iid);
            outQ.enq(PORT_MULTIPLEXED_MSG { iid: iid, msg: tagged Invalid });

            let cnt = compNoMsgCount - 1;
            compNoMsgCount <= cnt;

            // Done if cnt is 0, otherwise prepare for next instance.
            compIID <= (cnt == 0) ? tagged Invalid : tagged Valid (iid + 1);
        endrule

        //
        // shift --
        //   Pass received messages to the FIFO buffer.
        //
        rule shift (! isValid(compIID));
            let enc = inQ.first();
            inQ.deq();

            case (enc) matches
                tagged MSG .d:
                begin
                    outQ.enq(PORT_MULTIPLEXED_MSG { iid: d.iid,
                                                    msg: tagged Valid d.msg });
                end

                tagged NoMSG .d:
                begin
                    // One more more no-messages
                    outQ.enq(PORT_MULTIPLEXED_MSG { iid: d.iid, msg: tagged Invalid });

                    // Counter is 0-based
                    if (d.noMsgCnt != 0)
                    begin
                        compIID <= tagged Valid (d.iid + 1);
                        compNoMsgCount <= d.noMsgCnt;
                    end
                end
            endcase
        endrule


        method Action enq(cval) = inQ.enq(cval);
        method Bool notFull() = inQ.notFull();

        method t_DATA first() = outQ.first();
        method Action deq() = outQ.deq();
        method Bool notEmpty() = outQ.notEmpty();
    endmodule
endinstance

`endif


//
// Map compressed port to chunks so they can be sent on multiple soft
// connections.  The multi-FPGA router can do this automatically.  This
// class exists to enable the chunk mapping within Bluespec without
// the aid of an external program.  It is not needed for the multi-FPGA
// router's compression.
//
instance CompressionChunks#(PORT_MULTIPLEXED_MSG_COMPRESSED#(t_NUM_INSTANCES, t_MSG),
                            // The chunked message:
                            // We don't know which is larger: the message or the
                            // no-message payload.  The low portion of the two
                            // chunks is the maybe bit, the iid and the smaller
                            // of the message and the no-message payloads.
                            // Both NoMSG and MSG payloads must have the IID
                            // in their least significant bits!  The high portion
                            // is the remainder of whichever type is larger.
                            HList2#(Bit#(TSub#(t_LARGER_PAYLOAD_SZ, t_SMALLER_PAYLOAD_SZ)),
                                    Tuple3#(Bit#(t_SMALLER_PAYLOAD_SZ),
                                            INSTANCE_ID#(t_NUM_INSTANCES),
                                            Bool)))
    provisos (Bits#(t_MSG, t_MSG_SZ),
              Alias#(t_ENC_DATA, PORT_MULTIPLEXED_MSG_COMPRESSED#(t_NUM_INSTANCES, t_MSG)),
              Bits#(t_ENC_DATA, t_ENC_DATA_SZ),
              Alias#(t_ENC_CHUNKS, HList2#(Bit#(TSub#(t_LARGER_PAYLOAD_SZ, t_SMALLER_PAYLOAD_SZ)),
                                           Tuple3#(Bit#(t_SMALLER_PAYLOAD_SZ),
                                                   INSTANCE_ID#(t_NUM_INSTANCES),
                                                   Bool))),
              HList#(t_ENC_CHUNKS),
              Bits#(t_ENC_CHUNKS, t_ENC_CHUNKS_SZ),
              Add#(a__, 1, t_ENC_CHUNKS_SZ),
              Bits#(PORT_MULTIPLEXED_CMP_NOMSG#(t_NUM_INSTANCES), t_PORT_NOMSG_SZ),
              Bits#(PORT_MULTIPLEXED_CMP_MSG#(t_NUM_INSTANCES, t_MSG), t_PORT_MSG_SZ),
              NumAlias#(INSTANCE_ID_BITS#(t_NUM_INSTANCES), t_IID_SZ),
              // "Payload" is the size without the IID
              Add#(t_PORT_NOMSG_PAYLOAD_SZ, t_IID_SZ, t_PORT_NOMSG_SZ),
              Add#(t_PORT_MSG_PAYLOAD_SZ, t_IID_SZ, t_PORT_MSG_SZ),
              // Compute larger & smaller of message and no-message payloads
              Max#(t_PORT_NOMSG_PAYLOAD_SZ, t_PORT_MSG_PAYLOAD_SZ, t_LARGER_PAYLOAD_SZ),
              Min#(t_PORT_NOMSG_PAYLOAD_SZ, t_PORT_MSG_PAYLOAD_SZ, t_SMALLER_PAYLOAD_SZ));

    function encDataToChunks(data);
        // Encoding assumes that both the MSG and NoMSG payloads have an IID
        // in their lowest bit positions.  Given that, the mapping to the chunks
        // is a simple rotation of the tag bit from the MSB to the LSB.
        Bit#(t_ENC_CHUNKS_SZ) p = sameSizeNP(pack(data));
        return unpack({ p[valueOf(t_ENC_CHUNKS_SZ)-2:0], msb(p) });
    endfunction

    // Which chunks are valid in an encoded message?
    function encDataToChunksMask(data);
        Bool msgIsSmaller = valueOf(t_PORT_MSG_SZ) < valueOf(t_PORT_NOMSG_SZ);
        Bool isMsg = case (data) matches
                         tagged NoMSG .d: False;
                         tagged MSG .d: True;
                     endcase;

        return list(unpack(pack(isMsg) ^ pack(msgIsSmaller)), True);
    endfunction


    // Map chunks to encoded data.  See encDataToChunks for description.
    function chunksToEncData(chunks);
        Bit#(t_ENC_DATA_SZ) b = sameSizeNP(pack(chunks));
        return unpack({ lsb(b), b[valueOf(t_ENC_DATA_SZ)-1:1] });
    endfunction

    // The tag chunk must be read to decode a chunk.
    function decodeRequiredChunksMask() = list(False, True);

    // Which chunks are valid?  The Bool chunk is always valid and the overflow
    // chunk is valid when the message is large.
    function chunksToEncDataMask(key);
        Bool msgIsSmaller = valueOf(t_PORT_MSG_SZ) < valueOf(t_PORT_NOMSG_SZ);
        Bool isMsg = tpl_3(hLast(key));

        return list(unpack(pack(isMsg) ^ pack(msgIsSmaller)), True);
    endfunction
endinstance
