#include "asim/provides/events_service.h"
#include "asim/provides/commands_service.h"


typedef class MODEL_SERVICES_CLASS* MODEL_SERVICES;

class MODEL_SERVICES_CLASS
{
  private:
  // Events and controller are RRR servers so we don't need to instantiate them.

  public:
    MODEL_SERVICES_CLASS() {}
    ~MODEL_SERVICES_CLASS() {}

    void Init() {}
};

