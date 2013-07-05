#include <string.h>

void *(* z_getext)(char *);
void (* z_setext)(char *, void *);

int _argsep(char *str, int c, char **argv)
{
    char *s;
    int argc;
    int i;
    
    for(i = 1; i < c; i++) argv[i] = 0; //zero out all pointers
    argv[0] = str;
    if(!str || !*str) return 0;
    argc = 1;
    for(i = 1; i < c; i++)
    {
        s = strchr(argv[i - 1], ' ');
        if(!s) break;   //no delimiter found - prevous argument is last argument or string end
        *s = 0;         //replace delimiter with null
        s++;            //thing after delimiter
        while(*s == ' ') s++;   //skip other delimiters if any
        argv[i] = s;    //thing after all delimiters
        argc++;
    }
    return argc;
}


char *z_init(void *getext, void *setext, char *args)
{
    int argc;
    char *argv[16];
    
    *(void **)(&z_getext) = getext;
    *(void **)(&z_setext) = setext;
    
    argc = _argsep(args, 16, argv);
    
    void (*_test)();
    *(void **)(&_test) = z_getext("test");
    
    _test();
    
	return 0;
}
