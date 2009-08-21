

module [CONNECTED_MODULE] mkDebugScanIOService#(DEBUG_SCAN_IO debugScan)
    // interface:
        ();

    // ****** State Elements ******

    // Communication link to the Stats themselves
    Connection_Chain#(DEBUG_SCAN_DATA) chain <- mkConnection_Chain(`RINGID_DEBUG_SCAN);

    // Our internal state
    Reg#(DEBUG_SCAN_STATE) state <- mkReg(DS_IDLE);
    

    // ****** Rules ******
  
    //
    // processResp --
    //
    // Process a response from an individual scan node.
    //  
    rule processResp (state == DS_DUMPING);
        let ds <- chain.receive_from_prev();

        case (ds) matches
            // A value to dump
            tagged DS_VAL .info:
            begin
                debugScan.scanValue(info.id, info.value);
            end

            // Command came all the way around the loop.  Done dumping.
            tagged DS_DUMP:
            begin
                debugScan.finishScan();
                state <= DS_IDLE;
            end
        endcase
    endrule
  
    //
    // scanStart --
    //    
    // Begin a debug scan out.
    //
    rule scanStart (state == DS_IDLE && debugScan.scanning());
    
        chain.send_to_next(tagged DS_DUMP);

        state <= DS_DUMPING;

    endrule

endmodule
