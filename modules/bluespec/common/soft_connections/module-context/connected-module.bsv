import ModuleContext::*;
import Connectable::*;
import Vector::*;
import Clocks::*;

//------------------ Connection Information ----------------------//
//                                                                //
// We gather information about each module's connections using the//
// ModuleContext library. The connections are then hooked together//
// using this info with the algorithms in connections.bsv         //
//                                                                //
//----------------------------------------------------------------//

// The data type that is sent in connections
typedef Bit#(`CON_CWIDTH) PHYSICAL_CONNECTION_DATA;
typedef Bit#(TSub#(`CON_CWIDTH, 32)) PHYSICAL_CONNECTION_PAYLOAD;

// A physical incoming connection
interface PHYSICAL_CONNECTION_IN;

  method Action try(PHYSICAL_CONNECTION_DATA d);
  method Bool   success();
  interface Clock clock;
  interface Reset reset;

endinterface

// A physical outgoing connection
interface PHYSICAL_CONNECTION_OUT;

  method Bool notEmpty();
  method PHYSICAL_CONNECTION_DATA first();
  method Action deq();
  interface Clock clock;
  interface Reset reset;

endinterface


// A bi-directional connection.
interface PHYSICAL_CONNECTION_INOUT;

  interface PHYSICAL_CONNECTION_IN  incoming;
  interface PHYSICAL_CONNECTION_OUT outgoing;

endinterface

// A logical station is just a name.
interface STATION;

    method String name();

endinterface


// A physical station just looks like two FIFOFs.
interface PHYSICAL_STATION;

  method Bool notEmpty();
  method PHYSICAL_CONNECTION_DATA first();
  method Action deq();
  
  method Action enq(PHYSICAL_CONNECTION_DATA d);

endinterface

// Data about unmatched logical send/broadcast connections
typedef struct 
{
    String logicalName;
    String logicalType;
    Bool oneToMany;
    Bool optional;
    PHYSICAL_CONNECTION_OUT outgoing;
} 
    LOGICAL_SEND_INFO
        deriving (Eq);

// Data about unmatched logical receive connections
typedef struct 
{
    String logicalName;
    String logicalType;
    Bool manyToOne;
    Bool optional;
    PHYSICAL_CONNECTION_IN incoming;
} 
    LOGICAL_RECV_INFO
        deriving (Eq);

// Data about stations.
typedef struct
{
    String stationName;
    String networkName;
    String stationType;
    List#(String) childrenNames;
    List#(LOGICAL_SEND_INFO) registeredSends;
    List#(LOGICAL_RECV_INFO) registeredRecvs;
}
    STATION_INFO
        deriving (Eq);
    
// BACKWARDS COMPATABILITY: Data about connection chains
typedef `CON_NUMCHAINS CON_NUM_CHAINS;

typedef struct 
{
    Integer logicalIdx;
    String logicalType;
    PHYSICAL_CONNECTION_IN  incoming;
    PHYSICAL_CONNECTION_OUT outgoing;
} 
    LOGICAL_CHAIN_INFO
        deriving (Eq);


// The context our connected modules operate on.
typedef struct
{
    List#(LOGICAL_SEND_INFO) unmatchedSends;
    List#(LOGICAL_RECV_INFO) unmatchedRecvs;
    Vector#(CON_NUM_CHAINS, List#(LOGICAL_CHAIN_INFO)) chains; // BACKWARDS COMPATABILITY: connection chains
    List#(STATION_INFO) stations;
    List#(STATION) stationStack;
    String rootStationName;
    Reset softReset;
}
    LOGICAL_CONNECTION_INFO
        deriving (Eq);

// A connected Module is a Bluespec module which uses Soft Connections
typedef ModuleContext#(LOGICAL_CONNECTION_INFO) ConnectedModule;

