#include "modules.h"

#ifdef WIN32
char *z_liberror()
{
    DWORD errnum = GetLastError();
    static char msgbuf[4096];
    if(!FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, 0, errnum, 0, (LPTSTR)msgbuf, sizeof(msgbuf), 0)) strcpy(msgbuf, "??? - FormatMessage failed");
    return msgbuf;
}
#else
char *z_liberror() { return dlerror(); }
#endif
