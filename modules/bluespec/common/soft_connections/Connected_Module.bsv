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

//A scanchain has incoming and outgoing connections
interface CON_Chain;

  interface CON_In incoming;
  interface CON_Out outgoing;

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
