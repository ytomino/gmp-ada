#include <stdint.h>
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

#if defined(__APPLE__)
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 1090
#pragma for Ada "stdint.h" include "sys/_types/_int32_t.h" /* int32_t */
#endif
#elif defined(__FreeBSD__)
#if __FreeBSD__ >= 9
#pragma for Ada "stdint.h" include "sys/_stdint.h"
#endif
#endif
