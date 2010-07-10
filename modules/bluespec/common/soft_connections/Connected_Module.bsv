import ModuleCollect::*;

//------------------ Connection Information ----------------------//
//                                                                //
// We gather information about each module's connections using the//
// ModuleCollect library. The connections are then hooked together//
// using this info with the algorithms in Connections.bsv         //
//                                                                //
//----------------------------------------------------------------//

//The data type that is sent in connections
typedef `CON_CWIDTH PHYSICAL_CONNECTION_SIZE;
typedef Bit#(PHYSICAL_CONNECTION_SIZE) CON_Data;

typedef `CON_CHAIN_CWIDTH CON_CHAIN_DATA_SZ;
typedef Bit#(CON_CHAIN_DATA_SZ) CON_CHAIN_Data;

typedef `CON_NUMCHAINS CON_NumChains;

//An incoming connection
interface PHYSICAL_CON_In#(parameter type t_MSG);

  method Action get_TRY(t_MSG x);
  method Bool   get_SUCCESS();
  interface Clock clk;
  interface Reset rst;

endinterface

//An outgoing connection
interface PHYSICAL_CON_Out#(parameter type t_MSG);

  method t_MSG try();
  method Action success();
  interface Clock clk;
  interface Reset rst;

endinterface

typedef PHYSICAL_CON_In#(CON_Data) CON_In;
typedef PHYSICAL_CON_Out#(CON_Data) CON_Out;

typedef PHYSICAL_CON_In#(CON_CHAIN_Data) CON_CHAIN_In;
typedef PHYSICAL_CON_Out#(CON_CHAIN_Data) CON_CHAIN_Out;

//A scanchain has incoming and outgoing connections
interface CON_Chain;

  interface CON_CHAIN_In incoming;
  interface CON_CHAIN_Out outgoing;

endinterface


//Data about soft connections
typedef struct {String cname; String ctype; Bool optional; CON_Out conn;} CSend_Info;
typedef struct {String cname; String ctype; Bool optional; CON_In conn;} CRecv_Info;
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

// New type convention:
typedef Connected_Module CONNECTED_MODULE;


//
// Bluespec doesn't define Ord for String, making it impossible to use sort
// functions.  The following definition of Ord doesn't guarantee a lexical
// order, but it does guarantee a consistent order within a compilation.
// That is enough for our purposes.
//
// When Bluespec finally defines Ord for String this can be removed.
//
instance Ord#(String);
    function Bool \< (String x, String y);
        return primStringToInteger(x) < primStringToInteger(y);
    endfunction

    function Bool \> (String x, String y);
        return primStringToInteger(x) > primStringToInteger(y);
    endfunction

    function Bool \<= (String x, String y);
        return primStringToInteger(x) <= primStringToInteger(y);
    endfunction

    function Bool \>= (String x, String y);
        return primStringToInteger(x) >= primStringToInteger(y);
    endfunction
endinstance


//
// Comparison of CSend_Info and CRecv_Info for sorting.
//
instance Eq#(CSend_Info);
    function Bool \== (CSend_Info x, CSend_Info y) = x.cname == y.cname;
    function Bool \/= (CSend_Info x, CSend_Info y) = x.cname != y.cname;
endinstance

instance Ord#(CSend_Info);
    function Bool \< (CSend_Info x, CSend_Info y) = x.cname < y.cname;
    function Bool \> (CSend_Info x, CSend_Info y) = x.cname > y.cname;
    function Bool \<= (CSend_Info x, CSend_Info y) = x.cname <= y.cname;
    function Bool \>= (CSend_Info x, CSend_Info y) = x.cname >= y.cname;
endinstance

instance Eq#(CRecv_Info);
    function Bool \== (CRecv_Info x, CRecv_Info y) = x.cname == y.cname;
    function Bool \/= (CRecv_Info x, CRecv_Info y) = x.cname != y.cname;
endinstance

instance Ord#(CRecv_Info);
    function Bool \< (CRecv_Info x, CRecv_Info y) = x.cname < y.cname;
    function Bool \> (CRecv_Info x, CRecv_Info y) = x.cname > y.cname;
    function Bool \<= (CRecv_Info x, CRecv_Info y) = x.cname <= y.cname;
    function Bool \>= (CRecv_Info x, CRecv_Info y) = x.cname >= y.cname;
endinstance
