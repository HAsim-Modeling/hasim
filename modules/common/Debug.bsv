//Debugging facilities for software simulation.
//Needs to be expanded to include some way to debug in hardware, using
//Controller.
//Debug
//Level 0: No messages except when absolutely necessary
//Level 1: High-Level execution trace
//Level 2: Detailed execution trace
//Level 3+: Every rule and method traced, with conditionals

`ifndef DEBUG_LEVEL
`define DEBUG_LEVEL 3
`endif

`ifndef PARTITION_NAME
`define PARTITION_NAME "Unknown"
`endif

`ifndef MODULE_NAME
`define MODULE_NAME "Unknown"
`endif

function Action debug (Integer level, Action a);
action

  if (`DEBUG_LEVEL >= level)
   a;

endaction
endfunction

function Action debug_enter (String thing, String name);
action


  debug(3, $display("Entering %s: %s", thing, name));

endaction
endfunction

function Action debug_enter_val (String thing, String name, String val);
action


  debug(3, $display("Entering %s: %s == %s", thing, name, val));

endaction
endfunction

function Action debug_rule (String rname);
action

  
  debug_enter("rule", rname);

endaction
endfunction

function Action debug_method (String mname);
action

  
  debug_enter("method", mname);

endaction
endfunction

function Action debug_case (String sw, String val);
action

  
  debug_enter_val("case branch", sw, val);

endaction
endfunction

function Action debug_case_default (String sw);
action

  
  debug_enter("case default branch for", sw);

endaction
endfunction

function Action debug_then (String cond);
action

  
  debug_enter_val("then branch", cond, "True");

endaction
endfunction

function Action debug_else (String cond);
action

  
  debug_enter_val("else branch", cond, "False");

endaction
endfunction
