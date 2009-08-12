
#include <iostream>
#include <math.h>
#include <fstream>

#include "asim/provides/hybrid_application.h"
#include "fixed.h"

using namespace std;

Fixed fir(Fixed* coeffs, Fixed* regs, Fixed new_input)
{

    regs[0] = new_input;
    for (int i = 0; i < (N_TAPS-1); i++)
    {
        regs[i+1] = regs[i] + (regs[i] * coeffs[i]);
    }

    return regs[N_TAPS-1] + (regs[N_TAPS-1] * coeffs[N_TAPS-1]);
}

// constructor
HYBRID_APPLICATION_CLASS::HYBRID_APPLICATION_CLASS(VIRTUAL_PLATFORM vp)
{
}

// destructor
HYBRID_APPLICATION_CLASS::~HYBRID_APPLICATION_CLASS()
{
}

void
HYBRID_APPLICATION_CLASS::Init()
{
}

// main
void
HYBRID_APPLICATION_CLASS::Main()
{

    Fixed coeffs[N_TAPS];
    Fixed regs[N_TAPS];
    Fixed input[INPUT_SIZE];

    // open output file
    ofstream outfile(OUTPUT_FILE, ios::out);

    if (!outfile.is_open())
    {
        std::cerr << "Could not open output file: " << OUTPUT_FILE << std::endl;
        exit(1);
    }

    // Initialize input.
    for (int i = 0; i < INPUT_SIZE; i++)
    {
        input[i] = Fixed(sin(i*2*3.14159*7/INPUT_SIZE));
    }
    for (int i = 0; i < N_TAPS; i++)
    {
        coeffs[i] = Fixed(1 / ((double) (i + 1)));
        regs[i] = Fixed(0);
    }

    // Calculate result.
    for (int i = 0; i < INPUT_SIZE; i++)
    {
        Fixed output = fir(coeffs, regs, input[i]);
        if (i > N_TAPS)
        {
            outfile << "[" << i << "]: " << (double) output << std::endl;
        }
    }

}

