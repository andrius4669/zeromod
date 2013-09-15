#include <stdio.h>
#include <string.h>
#include "GeoIP.h"

enum { PRIV_NONE = 0, PRIV_MASTER, PRIV_AUTH, PRIV_ADMIN, PRIV_ROOT };

void *(* z_getext)(char *);
void (* z_setext)(char *, void *);

char dbfile[260] = "GeoIP.dat";

struct hookparam
{
    void *args[16];
} __attribute__((packed));

//void *(* sendf)(int, int, char *, ...);
void (*debug)(char *);
void (*notifypriv)(char *, int, int);
void (*addhook)(char *, int (*hookfunc)(struct hookparam *));
void (*delhook)(char *, int (*hookfunc)(struct hookparam *));

GeoIP *gi = 0;

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


//0 - (uint32) ip
//1 - (char *)name

char connmsg[1024];
char ipaddr[128];

int on_connect(struct hookparam *hp)
{
    char *name = (char *)hp->args[1];
    
    if(!name || !*name || !gi || !hp->args[2]) return 0;
    
    unsigned int ip = *(unsigned int *)hp->args[2];
    
    if(!ip) return 0;
    
    sprintf(ipaddr, "%i.%i.%i.%i", ip&0xFF, (ip>>8)&0xFF, (ip>>16)&0xFF, (ip>>24)&0xFF);
    
    const char *country = GeoIP_country_name_by_addr(gi, ipaddr);
    
    if(country && country[0]) sprintf(connmsg, "%s \f2is connected from \f0%s \f5[%s]", name, country, ipaddr);
    else sprintf(connmsg, "%s \f2is connected from \f4Unknown \f5[%s]", name, ipaddr);
    
    notifypriv(connmsg, PRIV_ADMIN, PRIV_ROOT);
    
    if(!country || !country[0]) return 0;
    
    sprintf(connmsg, "%s \f2is connected from \f0%s", name, country);
    notifypriv(connmsg, PRIV_NONE, PRIV_AUTH);
    
    return 0;
}

char *z_init(void *getext, void *setext, char *args)
{
    int argc;
    char *argv[16];
    int i;
    int nomem = 0;
    
    *(void **)(&z_getext) = getext;
    *(void **)(&z_setext) = setext;
    
    argc = _argsep(args, 16, argv);
    i = 0;
    while(i<argc)
    {
        switch(argv[i][0])
        {
            case 'f':
                strncpy(dbfile, argv[++i], 260);
                break;
            
            case 'n':
                nomem = 1;
                break;
                
            default:
                return "Unknown switch";
        }
        i++;
    }
    
//  *(void **)(&sendf) = z_getext("sendf");
    *(void **)(&notifypriv) = z_getext("notifypriv");
    if(!notifypriv) return "Cannot find notifypriv";
    
    *(void **)(&addhook) = z_getext("addhook");
    if(!addhook) return "Cannot find addhook";
    
    *(void **)(&delhook) = z_getext("delhook");
    
    *(void **)(&debug) = z_getext("debug");
    
    if(!gi) gi = GeoIP_open(dbfile, GEOIP_STANDARD | (nomem?0:GEOIP_MEMORY_CACHE));
    if(!gi) return "Failed loading geoip database";
    
    addhook("connected", on_connect);
    
    return 0;
}

char *z_uninit()
{
    if(delhook) delhook("connected", on_connect);
    if(gi)
    {
        GeoIP_delete(gi);
        gi = 0;
    }
    return 0;
}