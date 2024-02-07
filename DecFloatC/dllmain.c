#include <windows.h>
#include <stdint.h>

typedef struct {
	int32_t sign;
	uint32_t exponent;
	uint32_t* mantissa;
}
DecFloat_struct;

extern DecFloat_struct pi_df;
extern DecFloat_struct pi2_df;
extern DecFloat_struct pi_half_df;
extern DecFloat_struct ln2_df;
extern DecFloat_struct ln10_df;
extern DecFloat_struct Log10e_df;
extern DecFloat_struct exp1_df;

void DecFloat_clear(DecFloat_struct* df);

BOOL WINAPI
DllMain (HANDLE hDll, DWORD dwReason, LPVOID lpReserved)
{
    switch (dwReason)
    {
        case DLL_PROCESS_ATTACH:
            // M_init_trig_globals();
            break;

        case DLL_PROCESS_DETACH:
            // M_free_all_cnst();
            if (pi_df.mantissa != 0) DecFloat_clear(&pi_df);
			if (pi2_df.mantissa != 0) DecFloat_clear(&pi2_df);
			if (pi_half_df.mantissa != 0) DecFloat_clear(&pi_half_df);
			if (ln2_df.mantissa != 0) DecFloat_clear(&ln2_df);
			if (ln10_df.mantissa != 0) DecFloat_clear(&ln10_df);
			if (Log10e_df.mantissa != 0) DecFloat_clear(&Log10e_df);
			if (exp1_df.mantissa != 0) DecFloat_clear(&exp1_df);
            break;

        case DLL_THREAD_ATTACH:
            // Code to run when a thread is created during the DLL's lifetime
            break;

        case DLL_THREAD_DETACH:
            // Code to run when a thread ends normally.
            break;
    }
    return TRUE;
}
