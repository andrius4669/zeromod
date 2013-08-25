#ifdef WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#ifdef WIN32
#define Z_OPENLIB(s) LoadLibrary(s)
#define Z_GETSYM GetProcAddress
#define Z_FREELIB FreeLibrary
#define Z_LIBFUNC FARPROC
#else
#define Z_OPENLIB(s) dlopen(s, RTLD_NOW)
#define Z_GETSYM dlsym
#define Z_FREELIB dlclose
#define Z_LIBFUNC void *
#endif

char *z_liberror();
