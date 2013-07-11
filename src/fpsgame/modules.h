#ifndef WIN32
#include <dlfcn.h>
#endif

#ifdef WIN32
#define Z_OPENLIB(s) LoadLibrary(s)
#define Z_GETSYM GetProcAdress
#define Z_FREELIB FreeLibrary
#else
#define Z_OPENLIB(s) dlopen(s, RTLD_NOW)
#define Z_GETSYM dlsym
#define Z_FREELIB dlclose
#endif
