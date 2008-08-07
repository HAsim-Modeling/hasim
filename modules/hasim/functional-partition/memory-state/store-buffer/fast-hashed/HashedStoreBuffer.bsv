import FpgaMemory::*;
import Vector::*;
import Utils::*;
import FShow::*;

typedef struct {
    Bit#(TAdd#(tokenWidth, 1)) token;
    Bit#(addrWidth) addr;
    May#(Bit#(dataWidth)) value;
} LookupResp#(type tokenWidth, type addrWidth, type dataWidth) deriving (Bits, Eq);

function LookupResp#(tokenWidth, addrWidth, dataWidth) makeLookupResp(Bit#(TAdd#(tokenWidth, 1)) token, Bit#(addrWidth) addr, Bool valid, Bit#(dataWidth) data) =
         LookupResp{token: token, addr: addr, value: valid? makeValid(data): makeInvalid};

typedef struct {
    Bit#(TAdd#(tokenWidth, 1)) token;
    Bit#(addrWidth) addr;
    Bit#(dataWidth) value;
    Bool hasMore;
} CommitResp#(type tokenWidth, type addrWidth, type dataWidth) deriving (Bits, Eq);

typedef struct {
    Bit#(tokenWidth) token;
    Bit#(TLog#(numStores)) index;
} Pointer#(type tokenWidth, type numStores) deriving (Bits, Eq);

function Pointer#(tokenWidth, numStores) makePointer(Bit#(tokenWidth) token, Bit#(TLog#(numStores)) index) = Pointer{token: token, index: index};

instance FShow#(Pointer#(tokenWidth, numStores));
    function Fmt fshow(Pointer#(tokenWidth, numStores) ptr);
        return $format("index: %d token: %d", ptr.index, ptr.token);
    endfunction
endinstance

function CommitResp#(tokenWidth, addrWidth, dataWidth) makeCommitResp(Bit#(TAdd#(tokenWidth, 1)) token, Bit#(addrWidth) addr, Bit#(dataWidth) value, Bool hasMore) =
         CommitResp{token: token, addr: addr, value: value, hasMore: hasMore};

typedef enum {Ready, Allocate, Insert, Commit, Lookup1, Lookup2, Lookup3, Lookup4} State deriving (Bits, Eq);

interface HashedStoreBuffer#(type tokenWidth, type addrWidth, type dataWidth, numeric type hashWidth, type numStores);
    method Bool isReady();
    method Action allocate(Bit#(TAdd#(tokenWidth, 1)) token);
    method Action insert(Bit#(TAdd#(tokenWidth, 1)) token, Bit#(addrWidth) addr, Bit#(dataWidth) value, Bit#(TLog#(numStores)) storeNum);
    method Action lookupReq(Bit#(TAdd#(tokenWidth, 1)) token, Bit#(addrWidth) addr);
    method ActionValue#(LookupResp#(tokenWidth, addrWidth, dataWidth)) lookupResp();
    method Action commitReq(Bit#(TAdd#(tokenWidth, 1)) token);
    method ActionValue#(CommitResp#(tokenWidth, addrWidth, dataWidth)) commitResp();
    method Action rewind(Bit#(TAdd#(tokenWidth, 1)) prevGood, Bit#(TAdd#(tokenWidth, 1)) nextGood);
endinterface

// remove(i, x, addrX, prevX, nextX)
//
// if(addrX != null)
//     addr[i][x] = null
//     if(nextX != nil)
//         prev[nextX.index][nextX.token] = prevX
//     if(prevX != nil)
//         next[prevX.index][prevX.token] = nextX
//     else
//         head(hash(addrX)) = nextX
// 
// // allocate/commit (x)
// 
// for(i = 0; i < numStores; i++)
//     addrX = addr[i][x]
//     prevX = prev[i][x]
//     nextX = next[i][x]
//     remove(i, x, addrX, prevX, nextX)
// 
// // insert (i, x, address)
// 
// if(valid[x])
//     y = head(hash(address))
//     addr[i][x] = address
//     next[i][x] = y
//     prev[i][x] = nil
//     head(hash(address)) = (token: x, index: i)
//     if(y != nil)
//         prev[y.index][y.token] = (token: x, index: i)
// 
// // lookup (x, address)
// 
// y = head(hash(address))
// z = nil
// best = nil
// if(y == nil)
//     return nil
// else
//     loopBegin:
//     ;; At this point y != nil, else something is screwed up
//     w = next[y.index][y.token]
//     addrY = addr[y.index][y.token]
//     if(valid[y.token])
//         if(isBetter(y.token, addrY))
//             best = y.token
//     else
//         remove(y.index, y.token, addrY, z, w)
//     z = y
//     y = w
//     
//     if(w != nil)
//         goto loopBegin
//     else
//         return best

module mkHashedStoreBuffer(HashedStoreBuffer#(tokenWidth, addrWidth, dataWidth, hashWidth, numStores))
    provisos(Add#(tokenWidth, 1, TAdd#(tokenWidth, 1)),
             Add#(tokenWidth, 1, tokenWidth1),
             Div#(dataWidth, 8, dataByteWidth),
             Log#(dataByteWidth, shift),
             Add#(positive0, 1, numStores),
             Add#(positive1, hashWidth, addrWidth));

    Vector#(numStores, Bram#(tokenWidth, Bit#(dataWidth)))                       data <- replicateM(mkBram);
    Reg#(Vector#(TExp#(tokenWidth), Bool))                                 tokenValid <- mkReg(replicate(False));

    Reg#(Bit#(tokenWidth1))                                                    deqNum <- mkReg(0);

    Bram#(hashWidth, May#(Pointer#(tokenWidth, numStores)))                      head <- mkBramInitialized(makeInvalid);
    Vector#(numStores, Bram#(tokenWidth, May#(Pointer#(tokenWidth, numStores)))) prev <- replicateM(mkBramInitialized(makeInvalid));
    Vector#(numStores, Bram#(tokenWidth, May#(Pointer#(tokenWidth, numStores)))) next <- replicateM(mkBramInitialized(makeInvalid));
    Vector#(numStores, Bram#(tokenWidth, May#(Bit#(addrWidth))))                 addr <- replicateM(mkBramInitialized(makeInvalid));

    Reg#(Bit#(tokenWidth))                                                       xReg <- mkRegU;
    Reg#(May#(Pointer#(tokenWidth, numStores)))                                  yReg <- mkRegU;
    Reg#(Bit#(TLog#(numStores)))                                                 iReg <- mkRegU;

    Reg#(State)                                                                 state <- mkReg(Ready);

    Reg#(Bit#(TAdd#(tokenWidth, 1)))                                         tokenReg <- mkRegU;
    Reg#(Bit#(addrWidth))                                                  addressReg <- mkRegU;
    Reg#(Bit#(dataWidth))                                                    valueReg <- mkRegU;

    Reg#(May#(Bit#(tokenWidth)))                                                 best <- mkRegU;

    DebugFile                                                                   debug <- mkDebugFile("HashedStoreBuffer.out");

    function Bit#(hashWidth) hash(Bit#(addrWidth) address) = truncate(address >> valueOf(shift));

    function Action remove(Bit#(TLog#(numStores)) i, Bit#(tokenWidth) x, May#(Bit#(addrWidth)) addrX, May#(Pointer#(tokenWidth, numStores)) prevX, May#(Pointer#(tokenWidth, numStores)) nextX);
    action
        if(addrX.valid)
        begin
            addr[i].write(x, makeInvalid);
            let curr = makePointer(x, i);
            if(nextX.valid)
            begin
                prev[nextX.value.index].write(nextX.value.token, prevX);
                debug <= $format("remove: prev: hash: %x curr: ", hash(addrX.value)) + fshow(curr) + $format(" next: ") + fshow(nextX) + $format(" prev: ") + fshow(prevX);
            end
            if(prevX.valid)
            begin
                next[prevX.value.index].write(prevX.value.token, nextX);
                debug <= $format("remove: next: hash: %x curr: ", hash(addrX.value)) + fshow(curr) + $format(" prev: ") + fshow(nextX) + $format(" next: ") + fshow(nextX);
            end
            else
            begin
                head.write(hash(addrX.value), nextX);
                debug <= $format("remove: head: hash: %x curr: ", hash(addrX.value)) + fshow(curr) + $format(" next: ") + fshow(nextX);
            end
        end
    endaction
    endfunction

    function Bool isBetter(Bit#(tokenWidth) y, Bit#(addrWidth) address);
        function Bool isBetweenCommitAndX(Bit#(tokenWidth) x);
            Bit#(tokenWidth) commit = truncate(deqNum);
            if(x >= commit && y >= commit && y < x)
                return True;
            else if(x < commit && (y >= commit || y < x))
                return True;
            else
                return False;
        endfunction

        if(address != addressReg)
            return False;
        else if(isBetweenCommitAndX(truncate(tokenReg)))
        begin
            if(!best.valid)
                return True;
            else if(isBetweenCommitAndX(truncate(best.value)))
                return False;
            else
                return True;
        end
        else
            return False;
    endfunction

    rule allocateRule(state == Allocate);
        let addrX <- addr[iReg].readResp();
        let prevX <- prev[iReg].readResp();
        let nextX <- next[iReg].readResp();
        remove(iReg, xReg, addrX, prevX, nextX);
        if(iReg != fromInteger(valueOf(numStores)-1))
        begin
            addr[iReg + 1].readReq(xReg);
            prev[iReg + 1].readReq(xReg);
            next[iReg + 1].readReq(xReg);
            debug <= $format("allocateRule: req: ") + fshow(makePointer(xReg, iReg));
        end
        else
            state <= Ready;
        iReg <= iReg + 1;
    endrule

    rule insertRule(state == Insert);
        let y <- head.readResp();
        let curr = makePointer(xReg, iReg);
        next[iReg].write(xReg, y);
        debug <= $format("insertRule: next: hash: %x curr: ", hash(addressReg)) + fshow(curr) + $format(" next: ") + fshow(y);
        head.write(hash(addressReg), makeValid(makePointer(xReg, iReg)));
        debug <= $format("insertRule: head: hash: %x curr: ", hash(addressReg)) + fshow(curr);
        if(y.valid)
        begin
            prev[y.value.index].write(y.value.token, makeValid(makePointer(xReg, iReg)));
            debug <= $format("insertRule: prev: hash: %x curr: ", hash(addressReg)) + fshow(curr) + $format(" next: ") + fshow(y);
        end
        state <= Ready;
    endrule

    rule lookup1Rule(state == Lookup1);
        let y <- head.readResp();
        if(y.valid)
        begin
            prev[y.value.index].readReq(y.value.token);
            next[y.value.index].readReq(y.value.token);
            addr[y.value.index].readReq(y.value.token);
            data[y.value.index].readReq(y.value.token);
            debug <= $format("lookup1Rule: ") + fshow(y);
            yReg <= y;
            state <= Lookup2;
        end
        else
            state <= Lookup4;
    endrule

    rule lookup2Rule(state == Lookup2);
        let addrY <- addr[yReg.value.index].readResp();
        let value <- data[yReg.value.index].readResp();
        let t <- prev[yReg.value.index].readResp();
        let w <- next[yReg.value.index].readResp();

        if(tokenValid[yReg.value.token])
        begin
            if(isBetter(yReg.value.token, addrY.value))
            begin
                best <= makeValid(yReg.value.token);
                valueReg <= value;
            end
        end
        else
            remove(yReg.value.index, yReg.value.token, addrY, t, w);

        yReg <= w;

        if(w.valid)
            state <= Lookup3;
        else
            state <= Lookup4;
    endrule

    rule lookup3Rule(state == Lookup3);
        prev[yReg.value.index].readReq(yReg.value.token);
        next[yReg.value.index].readReq(yReg.value.token);
        addr[yReg.value.index].readReq(yReg.value.token);
        data[yReg.value.index].readReq(yReg.value.token);
        debug <= $format("lookup3Rule: ") + fshow(yReg);
        state <= Lookup2;
    endrule

    method Bool isReady();
        return state == Ready;
    endmethod

    method Action allocate(Bit#(TAdd#(tokenWidth, 1)) token) if(state == Ready);
        Bit#(tokenWidth) x = truncate(token);
        tokenValid[x] <= True;
        addr[0].readReq(x);
        prev[0].readReq(x);
        next[0].readReq(x);
        Bit#(TLog#(numStores)) zero = 0;
        iReg <= zero;
        xReg <= x;
        debug <= $format("allocate: req: ") + fshow(makePointer(x, zero));
        state <= Allocate;
    endmethod

    method Action insert(Bit#(TAdd#(tokenWidth, 1)) token, Bit#(addrWidth) address, Bit#(dataWidth) value, Bit#(TLog#(numStores)) i) if(state == Ready);
        Bit#(tokenWidth) x = truncate(token);
        if(tokenValid[x])
        begin
            head.readReq(hash(address));
            data[i].write(x, value);
            addr[i].write(x, makeValid(address));
            prev[i].write(x, makeInvalid);
            debug <= $format("insert: invalid: token: %d", x);
            xReg <= x;
            iReg <= i;
            addressReg <= address;
            state <= Insert;
        end
    endmethod

    method Action lookupReq(Bit#(TAdd#(tokenWidth, 1)) token, Bit#(addrWidth) address) if(state == Ready);
        head.readReq(hash(address));
        best <= makeInvalid;
        xReg <= truncate(token);
        tokenReg <= token;
        addressReg <= address;
        state <= Lookup1;
    endmethod

    method ActionValue#(LookupResp#(tokenWidth, addrWidth, dataWidth)) lookupResp() if(state == Lookup4);
        state <= Ready;
        return makeLookupResp(tokenReg, addressReg, best.valid, valueReg);
    endmethod

    method Action commitReq(Bit#(TAdd#(tokenWidth, 1)) token) if(state == Ready);
        deqNum <= token + 1;
        Bit#(tokenWidth) x = truncate(token);
        tokenValid[x] <= False;
        data[0].readReq(x);
        addr[0].readReq(x);
        prev[0].readReq(x);
        next[0].readReq(x);
        Bit#(TLog#(numStores)) zero = 0;
        debug <= $format("commitReq: ") + fshow(makePointer(x, zero));
        if(0 != fromInteger(valueOf(numStores) - 1))
            addr[1].readReq(x);
        iReg <= zero;
        xReg <= x;
        tokenReg <= token;
        state <= Commit;
    endmethod

    method ActionValue#(CommitResp#(tokenWidth, addrWidth, dataWidth)) commitResp() if(state == Commit);
        let dataX <- data[iReg].readResp();
        let addrX <- addr[iReg].readResp();
        let prevX <- prev[iReg].readResp();
        let nextX <- next[iReg].readResp();

        May#(Bit#(addrWidth)) addrNext = makeInvalid;
        if(iReg != fromInteger(valueOf(numStores) - 1))
            addrNext <- addr[iReg + 1].readResp();

        // addrX is valid

        remove(iReg, xReg, addrX, prevX, nextX);

        if(addrNext.valid)
        begin
            data[iReg + 1].readReq(xReg);
            addr[iReg + 1].readReq(xReg);
            prev[iReg + 1].readReq(xReg);
            next[iReg + 1].readReq(xReg);

            debug <= $format("commitResp: ") + fshow(makePointer(xReg, iReg));

            if(valueOf(numStores) > 2)
                if(iReg != fromInteger(valueOf(numStores) - 2))
                    addr[iReg + 2].readReq(xReg);
        end
        else
            state <= Ready;

        iReg <= iReg + 1;
        return makeCommitResp(tokenReg, addrX.value, dataX, addrNext.valid);
    endmethod

    method Action rewind(Bit#(TAdd#(tokenWidth, 1)) prevGood, Bit#(TAdd#(tokenWidth, 1)) nextGood) if(state == Ready);
        let newInstValid = updateRange(False, truncate(prevGood), truncate(nextGood), tokenValid);
        tokenValid <= newInstValid;
    endmethod
endmodule

(* synthesize *)
module mkHashedStoreBufferTest(HashedStoreBuffer#(8, 32, 64, 8, 1));
    let sbuffer <- mkHashedStoreBuffer();
    return sbuffer;
endmodule
