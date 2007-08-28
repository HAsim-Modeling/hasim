import ModuleCollect::*;

//------------------ Connection Information ----------------------//
//                                                                //
// We gather information about each module's connections using the//
// ModuleCollect library. The connections are then hooked together//
// using this info with the algorithms in Connections.bsv         //
//                                                                //
//----------------------------------------------------------------//

//The data type that is sent in connections
typedef Bit#(`CON_CWIDTH) CON_Data;

typedef `CON_NUMCHAINS CON_NumChains;

//An incoming connection
interface CON_In;

  method Action get_TRY(CON_Data x);
  method Bool   get_SUCCESS();

endinterface

//An outgoing connection
interface CON_Out;

  method CON_Data try();
  method Action success();

endinterface

//A scanchain
typedef FIFO#(CON_Data) CON_Chain;

//Data about soft connections
typedef struct {String cname; String ctype; CON_Out conn;} CSend_Info;
typedef struct {String cname; String ctype; CON_In conn;} CRecv_Info;
typedef struct {Integer cnum; String ctype; CON_Chain conn;} CChain_Info;

//Data we collect with ModuleCollect
typedef union tagged
{
  CSend_Info  LSend;
  CRecv_Info  LRecv;
  CChain_Info LChain;
}
  ConnectionData;

//A connected Module is a Bluespec module which uses Soft Connections
typedef ModuleCollect#(ConnectionData) Connected_Module;

