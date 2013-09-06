
enum
{
    AC_UNKNOWN = 0,
    AC_GUNHACK,
    AC_POSHACK,
    AC_FLAGHACK,
    
    AC_MAX  //always last one
};

static const struct { const char *name; } cheats[AC_MAX] =
{
    "unknown",
    "gunhack",
    "poshack",
    "flaghack"
};
