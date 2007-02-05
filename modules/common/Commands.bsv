///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// ISA.bsv                                                               //
//                                                                           //
// Non-ISA-specific datatypes required by Controller model                   //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

//************ Basic Datatypes ************//

typedef Bit#(64)  CMD_Addr;
typedef Bit#(64)  CMD_Value;


typedef union tagged
{
  void         COM_LoadState;
  void         COM_RunProgram;  //Perhaps pass in global tick here
  void         COM_CheckResult;
  ModelCommand COM_Other;
}
  Command 
                deriving 
		        (Eq, Bits);

		
typedef union tagged
{
  void                                            RESP_DoneLoading;
  void                                            RESP_DoneRunning; //Perhaps return local tick here	 
  struct {CMD_Addr addr; CMD_Value exp_v; CMD_Value found_v;} RESP_Failure;
  void                                            RESP_CheckPassed;
  void                                            RESP_CheckFailed;
  ModelResponse                                   RESP_Other;
}
  Response
                 deriving
		         (Eq, Bits);

typedef void ModelCommand;
typedef void ModelResponse;
