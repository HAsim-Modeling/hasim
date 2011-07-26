import FIFOF::*;
import Counter::*;

`include "hasim_common.bsh"
`include "soft_connections.bsh"

interface PORT_CREDIT_SEND#(type t, numeric type bandwidth, numeric type logCredit);
    method Bool canSend();
    method Action reserve();
    method Action enq(t data);
    method Action enqReserve(t data);
    method Action done();
    method Action send(t data);
endinterface

interface PORT_CREDIT_RECEIVE#(type t, numeric type bandwidth, numeric type logCredit);
    method ActionValue#(t) pop();
    method Action done(Bit#(logCredit) consumerCredit);
    method Bool canReceive();
endinterface

module [HASIM_MODULE] mkPortCreditSend#(String str)
    (PORT_CREDIT_SEND#(dataT, bandwidth, logCredit))
    provisos(Add#(TLog#(TAdd#(1, bandwidth)), 0, logBandwidth),
             Bits#(dataT, dataSz));

    Connection_Send#(dataT)   dataConnection <- mkConnection_Send(str + "_data");
    Connection_Send#(Bit#(logBandwidth)) dataCountFifo <- mkConnection_Send(str + "_dataCount");
    Connection_Receive#(Bit#(logCredit)) creditsConnection <- mkConnection_Receive(str + "_credits");

    FIFOF#(dataT)                   dataFifo <- mkSizedFIFOF(valueOf(bandwidth));
    Reg#(Bit#(logCredit))            credits <- mkRegU;
    Reg#(Bit#(logBandwidth))       dataCount <- mkReg(0);
    PulseWire                         doneEn <- mkPulseWire;
    PulseWire                      reserveEn <- mkPulseWire;

    Reg#(Bool)                   initialized <- mkReg(False);

    Reg#(Bool)                         start <- mkReg(False);

    let _canSend = dataCount != fromInteger(valueOf(bandwidth)) && credits != 0;

    rule sendData(initialized);
        dataFifo.deq;
        dataConnection.send(dataFifo.first);
    endrule

    rule receiveCredits(initialized && !start);
        credits <= creditsConnection.receive;
        creditsConnection.deq;
        start <= True;
    endrule

    rule cycle(initialized);
        if(doneEn)
        begin
            dataCountFifo.send(reserveEn? dataCount + 1: dataCount);
            dataCount <= 0;
        end
        else if(reserveEn)
            dataCount <= dataCount + 1;
    endrule

    // A hack to allow consumer's done to be called in the beginning of the cycle
    rule initialize(!initialized);
        initialized <= True;
        dataCountFifo.send(0);
    endrule

    method Bool canSend() if(initialized && start);
        return _canSend;
    endmethod

    method Action reserve() if(initialized && start && _canSend);
        credits <= credits - 1;
        reserveEn.send;
    endmethod

    method Action enq(dataT data) if(initialized && start && _canSend);
        credits <= credits - 1;
        reserveEn.send;
        dataFifo.enq(data);
    endmethod

    // Unwritten guard: dataCount < enqCount
    method Action enqReserve(dataT data) if(initialized && start);
        dataFifo.enq(data);
    endmethod

    // Unwritten guard: currDataCount = currEnqCount
    method Action done() if(initialized && start);
        start <= False;
        doneEn.send;
    endmethod

    method Action send(dataT data) if(initialized && start && _canSend);
        credits <= credits - 1;
        reserveEn.send;
        dataFifo.enq(data);
        start <= False;
        doneEn.send;
    endmethod
endmodule

module [HASIM_MODULE] mkPortCreditReceive#(String str)
    (PORT_CREDIT_RECEIVE#(dataT, bandwidth, logCredit))
    provisos(Add#(TLog#(TAdd#(1, bandwidth)), 0, logBandwidth),
             Bits#(dataT, dataSz));

    Connection_Receive#(dataT) dataFifo <- mkConnection_Receive(str + "_data");
    Connection_Receive#(Bit#(logBandwidth)) dataCountFifo <- mkConnection_Receive(str + "_dataCount");
    Connection_Send#(Bit#(logCredit)) creditsFifo <- mkConnection_Send(str + "_credits");

    Reg#(Bit#(logBandwidth))       dataCount <- mkReg(0);
    PulseWire                          popEn <- mkPulseWire;
    PulseWire                         doneEn <- mkPulseWire;

    rule cycle;
        if(doneEn)
            dataCount <= 0;
        else if(popEn)
            dataCount <= dataCount + 1;
    endrule

    method Bool canReceive() if(dataFifo.notEmpty || dataCountFifo.notEmpty);
        if(dataCountFifo.notEmpty)
            return dataCount != dataCountFifo.receive;
        else
            return True;
    endmethod

    method ActionValue#(dataT) pop();
        dataFifo.deq;
        popEn.send;
        return dataFifo.receive;
    endmethod

    // Unwritten guard: dataCountFifo.first == currDataCount
    method Action done(Bit#(logCredit) _credits);
        doneEn.send;
        dataCountFifo.deq;
        creditsFifo.send(_credits);
    endmethod
endmodule

interface PORT_NO_STALL_RECEIVE#(type dataT, numeric type bandwidth);
    method Bool canReceive();
    method ActionValue#(dataT) pop();
    method ActionValue#(dataT) receive();
    method Action done();
endinterface

module [HASIM_MODULE] mkPortNoStallReceive#(String str)
    (PORT_NO_STALL_RECEIVE#(dataT, bandwidth))
    provisos(Add#(TLog#(TAdd#(1, bandwidth)), 0, logBandwidth),
             Add#(logBandwidth, 0, logCredit),
             Bits#(dataT, dataSz));

    PORT_CREDIT_RECEIVE#(dataT, bandwidth, logCredit) conn <- mkPortCreditReceive(str);

    Reg#(Bool) initialized <- mkReg(False);

    rule initialize(!initialized);
        conn.done(fromInteger(valueOf(bandwidth)));
        initialized <= True;
    endrule

    method Bool canReceive() if(initialized);
        return conn.canReceive;
    endmethod

    method ActionValue#(dataT) pop() if(initialized);
        let data <- conn.pop;
        return data;
    endmethod

    method ActionValue#(dataT) receive() if(initialized);
        let data <- conn.pop;
        conn.done(fromInteger(valueOf(bandwidth)));
        return data;
    endmethod

    method Action done() if(initialized);
        conn.done(fromInteger(valueOf(bandwidth)));
    endmethod
endmodule

interface PORT_FIFO_RECEIVE#(type dataT, numeric type bandwidth, numeric type logCredit);
    method Bool canReceive();
    method dataT first();
    method Action deq();
    method Action done();
endinterface

module [HASIM_MODULE] mkPortFifoReceive#(String str, Bool bufferedCredit, Integer credit)
    (PORT_FIFO_RECEIVE#(dataT, bandwidth, logCredit))
    provisos(Add#(TLog#(TAdd#(1, bandwidth)), 0, logBandwidth),
             Bits#(dataT, dataSz));

    PORT_CREDIT_RECEIVE#(dataT, bandwidth, logCredit) conn <- mkPortCreditReceive(str);

    FIFOF#(dataT)                             fifo <- mkSizedFIFOF(credit);
    COUNTER#(logCredit)                    credits <- mkLCounter(fromInteger(credit));
    Reg#(Bit#(logBandwidth))             dataCount <- mkReg(0);
    PulseWire                               doneEn <- mkPulseWire;
    PulseWire                                deqEn <- mkPulseWire;

    Reg#(Bool)                               start <- mkReg(False);

    let _canReceive = (fifo.notEmpty && dataCount != fromInteger(valueOf(bandwidth)));

    rule fill(!start);
        (* split *)
        if(conn.canReceive)
        begin
            let data <- conn.pop;
            fifo.enq(data);
            credits.down;
        end
        else
        begin
            (* nosplit *)
            start <= True;
            if(bufferedCredit)
                conn.done(credits.value);
        end
    endrule

    rule cycle;
        if(doneEn)
            dataCount <= 0;
        else if(deqEn)
            dataCount <= dataCount + 1;
    endrule

    method Bool canReceive();
        return _canReceive;
    endmethod

    method dataT first() if((start || fifo.notEmpty) && _canReceive);
        return fifo.first;
    endmethod

    method Action deq() if((start || fifo.notEmpty) && _canReceive);
        credits.up;
        fifo.deq;
        deqEn.send;
    endmethod

    method Action done() if(start);
        start <= False;
        if(!bufferedCredit)
            conn.done(credits.value);
        doneEn.send;
    endmethod
endmodule
