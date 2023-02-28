#ifndef CODIN_CTHREADS_H
#define CODIN_CTHREADS_H

#if !defined(__STDC_NO_THREADS__)
#include <threads.h>
#elif defined(_WIN32)
#include "cthreads_win32.h"
#else
#include "cthreads_pthreads.h"
#endif // #ifndef __STDC_NO_THREADS__

#endif // #ifndef CODIN_CTHREADS_H
