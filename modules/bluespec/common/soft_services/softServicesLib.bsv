import HList::*;
import ModuleContext::*;


// Generally useful definitions

// this is for individual contexts i.e. SoftClock, SoftConnections
typeclass FinalizableContext#(type t);
  module [Module] finalizeContext#(t contextTop) (Empty);
endtypeclass

typeclass ExposableContext#(type t, type t_e);
  module [Module] exposeContext#(t contextTop) (t_e);
endtypeclass

typeclass BuriableContext#(type m, type t_e);
  module [m] buryContext#(t_e contextIfc) (Empty);
endtypeclass

typeclass InitializableContext#(type t);
  module initializeContext(t);
endtypeclass


// Generic function for stamping out the module target of the module context

module [Module] instantiateWithConnections#(ModuleContext#(mc,t_IFC) m) (t_IFC)
  provisos(InitializableContext#(mc),
           FinalizableContext#(mc));

  // Instantiate the module and get the resulting context.
  mc ctext <- initializeContext();
  match {.finalContext, .mFinal} <- runWithContext(ctext, m);
  finalizeContext(finalContext);
  return mFinal;
endmodule
                
module [Module] instantiateSmartBoundary#(ModuleContext#(mc,t_IFC) m) (ctxtIfc)
  provisos(InitializableContext#(mc),
           ExposableContext#(mc,ctxtIfc));

  // Instantiate the module and get the resulting context.
  mc ctxt <- initializeContext();
  // drop mFinal.  By convention it's empty.
  match {.finalContext, .mFinal} <- runWithContext(ctxt, m);
  let ctxtInterface <- exposeContext(finalContext);
  return ctxtInterface;
endmodule

module [ModuleContext#(mc)] burySmartBoundary#(mc initCtxt, ctxtIfc m) ()
  provisos(InitializableContext#(mc),
           BuriableContext#(ModuleContext#(mc),ctxtIfc));
  // trust the caller to give us a useful context...
  putContext(initCtxt); // Start things off fresh...
  buryContext(m);
endmodule

// HLists are SoftServiceContext if their children are
instance InitializableContext#(HList1#(t))
  provisos (InitializableContext#(t));

  module initializeContext (HList1#(t));
    t context1 <- initializeContext();
    return hList1(context1);
  endmodule
endinstance

instance FinalizableContext#(HList1#(t))
  provisos (FinalizableContext#(t));
  // ugh manually spilt the contexts
  module [Module] finalizeContext#(HList1#(t) m) (Empty);
    finalizeContext(hHead(m));
  endmodule

endinstance

instance ExposableContext#(HList1#(t),t_e)
  provisos (ExposableContext#(t,t_e));
  // ugh manually spilt the contexts
  module [Module] exposeContext#(HList1#(t) m) (t_e);
    let exposedIfc <- exposeContext(hHead(m));
    return exposedIfc;
  endmodule

endinstance

//instance BuriableContext#(ModuleContext#(HList1#(t)),t_e)
//  provisos (BuriableContext#(ModuleContext#(HList1#(t)),t_e));
//  module [ModuleContext#(HList1#(t))] buryContext#(t_e m) (Empty);
//    buryContext(m);
//  endmodule
//endinstance


instance InitializableContext#(HList2#(t1,t2))
  provisos (InitializableContext#(t1),
            InitializableContext#(t2));

  module initializeContext (HList2#(t1,t2));
    t1 context1 <- initializeContext();
    t2 context2 <- initializeContext();
    return hList2(context1,context2);
  endmodule

endinstance


instance FinalizableContext#(HList2#(t1,t2))
  provisos (FinalizableContext#(t1),
            FinalizableContext#(t2));

  // ugh manually spilt the contexts
  module [Module] finalizeContext#(HList2#(t1,t2) c) (Empty);
    finalizeContext(hHead(c));
    finalizeContext(hHead(hTail(c)));
  endmodule

endinstance

instance ExposableContext#(HList2#(t1,t2),Tuple2#(t1_e, t2_e))
  provisos (ExposableContext#(t1,t1_e),
            ExposableContext#(t2,t2_e));
  // ugh manually spilt the contexts
  module [Module] exposeContext#(HList2#(t1,t2) m) (Tuple2#(t1_e,t2_e));
    let expIfc1 <- exposeContext(hHead(m));
    let expIfc2 <- exposeContext(hHead(hTail(m)));
    return tuple2(expIfc1,expIfc2);
  endmodule

endinstance

instance BuriableContext#(ModuleContext#(HList2#(t1,t2)),Tuple2#(t1_e,t2_e))
  provisos (BuriableContext#(ModuleContext#(HList2#(t1,t2)),t1_e),
            BuriableContext#(ModuleContext#(HList2#(t1,t2)),t2_e));
  module [ModuleContext#(HList2#(t1,t2))] buryContext#(Tuple2#(t1_e,t2_e) m) (Empty);
    buryContext(tpl_1(m));
    buryContext(tpl_2(m));
  endmodule
endinstance