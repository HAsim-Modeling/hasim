
// The transmittable typeclass allows us to turn all 
// the soft connections to the same type, and thus store them
// in a list until their match is found.

typeclass Transmittable#(type t_MSG);

  function PHYSICAL_CONNECTION_DATA marshall(t_MSG data);
  
  function t_MSG unmarshall(PHYSICAL_CONNECTION_DATA data);
  
endtypeclass

instance Transmittable#(t_MSG)
      provisos
              (Bits#(t_MSG, t_MSG_SIZE),
               Bits#(PHYSICAL_CONNECTION_DATA, t_CON_DATA_SIZE),
	       Add#(t_MSG_SIZE, t_TMP, t_CON_DATA_SIZE));

  function PHYSICAL_CONNECTION_DATA marshall(t_MSG data);
    return zeroExtend(pack(data));
  endfunction
  
  function t_MSG unmarshall(PHYSICAL_CONNECTION_DATA data);
    return unpack(truncate(data));
  endfunction
  
endinstance
