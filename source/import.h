#if defined(__APPLE__)
/* avoiding circular dependency about pid_t */
#include <sys/types.h>
#endif
#include <stdlib.h>
#include <string.h>
#include <gmp.h>
#include <mpfr.h>
/* block re-define mpfr_exp_t in mpc.h */
#define mpfr_exp_t mp_exp_t
#include <mpc.h>
#undef mpfr_exp_t

#pragma for Ada overload void gmp_printf (char const *, size_t, mpf_srcptr)
