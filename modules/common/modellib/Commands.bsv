///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// Commands.bsv                                                              //
//                                                                           //
// Non-ISA-specific datatypes required by Controller model                   //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////


typedef union tagged
{
  void         COM_RunProgram;     //Begin running, allowed to slip
  void         COM_Synchronize;    //Start synchronizing the system
  void         COM_StartSyncQuery; //Start checking if you're synchronized
  void         COM_SyncQuery;      //Is the system synchronized yet?
  void         COM_Step;           //Run exactly one model CC.
}
  Command 
                deriving 
		        (Eq, Bits);

		
typedef union tagged
{
  Bool RESP_DoneRunning; //Bool is run passed
  void RESP_Balanced;    //Response to query
  void RESP_UnBalanced;  //Response to query
  
}
  Response
                 deriving
		         (Eq, Bits);

