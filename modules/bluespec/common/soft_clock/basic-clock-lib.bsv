// Need the lib seperation to prevent death by AWB

typedef struct {
  Clock clock;
  Reset reset;
  Integer frequency;
} LOGICAL_CLOCK_INFO deriving (Eq);


