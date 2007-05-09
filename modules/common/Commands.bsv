///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// ISA.bsv                                                               //
//                                                                           //
// Non-ISA-specific datatypes required by Controller model                   //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

typedef union tagged
{
  void         COM_RunProgram;
}
  Command 
                deriving 
		        (Eq, Bits);

		
typedef union tagged
{
  Bool RESP_DoneRunning; //Bool is run passed
}
  Response
                 deriving
		         (Eq, Bits);
