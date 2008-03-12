import hasim_modellib::*;
`include "asim/dict/STREAMS.bsh"

// EventsController

// Abstracts communication from the main controller to the Event trackers
// which are distributed throughout the hardware model.

// When Events are enabled the main controller can use the getNextEvent() method
// to get the next event for recording, until noMoreEvents() is asserted.

interface EventsController;

  method Action doCommand(EventsCommand com);
  method Bool   noMoreEvents();

endinterface

// EventCommand

// The datatype of commands the EventsController accepts

typedef enum
{
  Events_Enable,
  Events_Disable
}
  EventsCommand
                deriving (Eq, Bits);

// EventInfo

// The datatype of Events that the Events Controller returns

typedef struct
{
        Bit#(1)      eventBoundary; // Is it a model cycle boundary?
        STREAMS_DICT_TYPE  eventStringID; // Which Event is it?
        Bit#(32)     modelTick;    // Model tick of the event
        EventParam   eventData;     // User-given Event data.

}
  EventInfo 
            deriving 
                     (Eq, Bits);

