// server.cpp: little more than enhanced multicaster
// runs dedicated or as client coroutine

#include "engine.h"

#define LOGSTRLEN 512

static FILE *logfile = NULL;
volatile bool reloadcfg = false;
volatile bool quitserver = false;
SVAR(configfile, "server-init.cfg");

void closelogfile()
{
    if(logfile)
    {
        fclose(logfile);
        logfile = NULL;
    }
}

FILE *getlogfile()
{
#ifdef WIN32
    return logfile;
#else
    return logfile ? logfile : stdout;
#endif
}

void setlogfile(const char *fname)
{
    closelogfile();
    if(fname && fname[0])
    {
        fname = findfile(fname, "a");
        if(fname) logfile = fopen(fname, "a");
    }
    FILE *f = getlogfile();
    if(f) setvbuf(f, NULL, _IOLBF, BUFSIZ);
}

void logoutf(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    logoutfv(fmt, args);
    va_end(args);
}

static void writelog(FILE *file, const char *buf)
{
    static uchar ubuf[512];
    int len = strlen(buf), carry = 0;
    while(carry < len)
    {
        int numu = encodeutf8(ubuf, sizeof(ubuf)-1, &((const uchar *)buf)[carry], len - carry, &carry);
        if(carry >= len) ubuf[numu++] = '\n';
        fwrite(ubuf, 1, numu, file);
    }
}

static void writelogv(FILE *file, const char *fmt, va_list args)
{
    static char buf[LOGSTRLEN];
    vformatstring(buf, fmt, args, sizeof(buf));
    writelog(file, buf);
}

void fatal(const char *fmt, ...)
{
    void cleanupserver();
    cleanupserver();
	defvformatstring(msg,fmt,fmt);
	if(logfile) logoutf("%s", msg);
#ifdef WIN32
	MessageBox(NULL, msg, "Cube 2: Sauerbraten fatal error", MB_OK|MB_SYSTEMMODAL);
#else
    fprintf(stderr, "server error: %s\n", msg);
#endif
    closelogfile();
    exit(EXIT_FAILURE);
}

void conoutfv(int type, const char *fmt, va_list args)
{
    logoutfv(fmt, args);
}

void conoutf(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    conoutfv(CON_INFO, fmt, args);
    va_end(args);
}

void conoutf(int type, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    conoutfv(type, fmt, args);
    va_end(args);
}

#define DEFAULTCLIENTS 8

enum { ST_EMPTY, ST_LOCAL, ST_TCPIP };

struct client                   // server side version of "dynent" type
{
    int type;
    int num;
    ENetPeer *peer;
    string hostname;
    void *info;
};

vector<client *> clients;

ENetHost *serverhost = NULL;
int laststatus = 0;
ENetSocket pongsock = ENET_SOCKET_NULL, lansock = ENET_SOCKET_NULL;

int localclients = 0, nonlocalclients = 0;

bool hasnonlocalclients() { return nonlocalclients!=0; }
bool haslocalclients() { return /*localclients!=0;*/ false; }

client &addclient(int type)
{
    client *c = NULL;
    loopv(clients) if(clients[i]->type==ST_EMPTY)
    {
        c = clients[i];
        break;
    }
    if(!c)
    {
        c = new client;
        c->num = clients.length();
        clients.add(c);
    }
    c->info = server::newclientinfo();
    c->type = type;
    switch(type)
    {
        case ST_TCPIP: nonlocalclients++; break;
//      case ST_LOCAL: localclients++; break;
    }
    return *c;
}

void delclient(client *c)
{
    if(!c) return;
    switch(c->type)
    {
        case ST_TCPIP: nonlocalclients--; if(c->peer) c->peer->data = NULL; break;
//      case ST_LOCAL: localclients--; break;
        case ST_EMPTY: return;
    }
    c->type = ST_EMPTY;
    c->peer = NULL;
    if(c->info)
    {
        server::deleteclientinfo(c->info);
        c->info = NULL;
    }
}

void cleanupserver()
{
    if(serverhost) enet_host_destroy(serverhost);
    serverhost = NULL;

    if(pongsock != ENET_SOCKET_NULL) enet_socket_destroy(pongsock);
    if(lansock != ENET_SOCKET_NULL) enet_socket_destroy(lansock);
    pongsock = lansock = ENET_SOCKET_NULL;
}

void updaterates();

VARF(serveruprate, 0, 0, INT_MAX, updaterates());
VARF(serverdownrate, 0, 0, INT_MAX, updaterates());
SVAR(serverip, "");
VARF(serverport, 0, server::serverport(), 0xFFFF, { if(!serverport) serverport = server::serverport(); });

void updaterates()
{
    if(serverhost) enet_host_bandwidth_limit(serverhost, serverdownrate, serveruprate);
}

VARF(maxclients, 0, DEFAULTCLIENTS, MAXCLIENTS, {
    if(!maxclients) maxclients = DEFAULTCLIENTS;
});
VARF(maxdupclients, 0, 0, MAXCLIENTS, { if(serverhost) serverhost->duplicatePeers = maxdupclients ? maxdupclients : MAXCLIENTS; });
VAR(maxslots, 0, 0, MAXCLIENTS);

void process(ENetPacket *packet, int sender, int chan);

int getservermtu() { return serverhost ? serverhost->mtu : -1; }
void *getclientinfo(int i) { return !clients.inrange(i) || clients[i]->type==ST_EMPTY ? NULL : clients[i]->info; }
ENetPeer *getclientpeer(int i) { return clients.inrange(i) && clients[i]->type==ST_TCPIP ? clients[i]->peer : NULL; }
int getnumclients()        { return clients.length(); }
uint getclientip(int n)    { return clients.inrange(n) && clients[n]->type==ST_TCPIP ? clients[n]->peer->address.host : 0; }
const char *getclienthostname(int n) { return clients.inrange(n) && clients[n]->type==ST_TCPIP ? clients[n]->hostname : 0; }

void sendpacket(int n, int chan, ENetPacket *packet, int exclude)
{
    if(n<0)
    {
        server::recordpacket(chan, packet->data, packet->dataLength);
        loopv(clients) if(i!=exclude && server::allowbroadcast(i)) sendpacket(i, chan, packet);
        return;
    }
    if(!clients.inrange(n))
    {
        logoutf("BUG: Incorrect cn in sendpacket: %i", n);
        return;
    }
    switch(clients[n]->type)
    {
        case ST_TCPIP:
        {
            enet_peer_send(clients[n]->peer, chan, packet);
            break;
        }
    }
}

ENetPacket *sendf(int cn, int chan, const char *format, ...)
{
    int exclude = -1;
    bool reliable = false;
    if(*format=='r') { reliable = true; ++format; }
    packetbuf p(MAXTRANS, reliable ? ENET_PACKET_FLAG_RELIABLE : 0);
    va_list args;
    va_start(args, format);
    while(*format) switch(*format++)
    {
        case 'x':
            exclude = va_arg(args, int);
            break;

        case 'v':
        {
            int n = va_arg(args, int);
            int *v = va_arg(args, int *);
            loopi(n) putint(p, v[i]);
            break;
        }

        case 'i':
        {
            int n = isdigit(*format) ? *format++-'0' : 1;
            loopi(n) putint(p, va_arg(args, int));
            break;
        }
        case 'f':
        {
            int n = isdigit(*format) ? *format++-'0' : 1;
            loopi(n) putfloat(p, (float)va_arg(args, double));
            break;
        }
        case 's': sendstring(va_arg(args, const char *), p); break;
        case 'm':
        {
            int n = va_arg(args, int);
            p.put(va_arg(args, uchar *), n);
            break;
        }
    }
    va_end(args);
    ENetPacket *packet = p.finalize();
    sendpacket(cn, chan, packet, exclude);
    return packet->referenceCount > 0 ? packet : NULL;
}

ENetPacket *sendfile(int cn, int chan, stream *file, const char *format, ...)
{
    if(cn < 0) return NULL;
    else if(!clients.inrange(cn)) return NULL;

    int len = (int)min(file->size(), stream::offset(INT_MAX));
    if(len <= 0 || len > 16<<20) return NULL;

    packetbuf p(MAXTRANS+len, ENET_PACKET_FLAG_RELIABLE);
    va_list args;
    va_start(args, format);
    while(*format) switch(*format++)
    {
        case 'i':
        {
            int n = isdigit(*format) ? *format++-'0' : 1;
            loopi(n) putint(p, va_arg(args, int));
            break;
        }
        case 's': sendstring(va_arg(args, const char *), p); break;
        case 'l': putint(p, len); break;
    }
    va_end(args);

    file->seek(0, SEEK_SET);
    file->read(p.subbuf(len).buf, len);

    ENetPacket *packet = p.finalize();
    sendpacket(cn, chan, packet, -1);
    return packet->referenceCount > 0 ? packet : NULL;
}

const char *disconnectreason(int reason)
{
    switch(reason)
    {
        case DISC_EOP: return "end of packet";
        case DISC_LOCAL: return "server is in local mode";
        case DISC_KICK: return "kicked/banned";
        case DISC_MSGERR: return "message error";
        case DISC_IPBAN: return "ip is banned";
        case DISC_PRIVATE: return "server is in private mode";
        case DISC_MAXCLIENTS: return "server FULL";
        case DISC_TIMEOUT: return "connection timed out";
        case DISC_OVERFLOW: return "overflow";
        case DISC_PASSWORD: return "invalid password";
        default: return NULL;
    }
}

void disconnect_client(int n, int reason)
{
    if(!clients.inrange(n) || clients[n]->type!=ST_TCPIP) return;
    enet_peer_disconnect(clients[n]->peer, reason);
    server::clientdisconnect(n, reason);
    delclient(clients[n]);
    const char *msg = disconnectreason(reason);
    string s;
    if(msg) formatstring(s)("client (%s) disconnected because: %s", clients[n]->hostname, msg);
    else formatstring(s)("client (%s) disconnected", clients[n]->hostname);
    logoutf("%s", s);
}

#if 0
void kicknonlocalclients(int reason)
{
    loopv(clients) if(clients[i]->type==ST_TCPIP) disconnect_client(i, reason);
}
#endif

void process(ENetPacket *packet, int sender, int chan)   // sender may be -1
{
    packetbuf p(packet);
    server::parsepacket(sender, chan, p);
    if(p.overread()) { disconnect_client(sender, DISC_EOP); return; }
}

#if 0
void localclienttoserver(int chan, ENetPacket *packet)
{
    client *c = NULL;
    loopv(clients) if(clients[i]->type==ST_LOCAL) { c = clients[i]; break; }
    if(c) process(packet, c->num, chan);
}
#endif

bool resolverwait(const char *name, ENetAddress *address)
{
    return enet_address_set_host(address, name) >= 0;
}

int connectwithtimeout(ENetSocket sock, const char *hostname, const ENetAddress &remoteaddress)
{
    return enet_socket_connect(sock, &remoteaddress);
}


//ENetSocket mastersock = ENET_SOCKET_NULL;
//ENetAddress masteraddress = { ENET_HOST_ANY, ENET_PORT_ANY }, serveraddress = { ENET_HOST_ANY, ENET_PORT_ANY };
//int lastupdatemaster = 0, lastconnectmaster = 0, masterconnecting = 0, masterconnected = 0;
//vector<char> masterout, masterin;
//int masteroutpos = 0, masterinpos = 0;
ENetAddress serveraddress = { ENET_HOST_ANY, ENET_PORT_ANY };

VARN(updatemaster, allowupdatemaster, 0, 1, 1);

struct masterinfo
{
    ENetSocket mastersock;
    ENetAddress masteraddress;
    int lastupdatemaster;
    int lastconnectmaster;
    int masterconnecting;
    int masterconnected;
    vector<char> masterout, masterin;
    int masteroutpos, masterinpos;

    string mastername;
    int masterport;

    int masterauthprivilege;
    int activeprivilege;
    int masterauth;
    string masterauthdesc;
    bool masterban;
    bool masterreg;
    bool allowsetprivilege;

    masterinfo():
        mastersock(ENET_SOCKET_NULL),
        lastupdatemaster(0), lastconnectmaster(0),
        masterconnecting(0), masterconnected(0),
        masteroutpos(0), masterinpos(0),
        masterport(server::masterport()),
        masterauthprivilege(PRIV_AUTH), activeprivilege(PRIV_AUTH),
        masterauth(1), masterban(true), masterreg(true),
        allowsetprivilege(false)
    {
        copystring(mastername, server::defaultmaster());
        masterauthdesc[0] = '\0';
        masteraddress.host = ENET_HOST_ANY;
        masteraddress.port = ENET_PORT_ANY;
    }
};

vector<masterinfo> masters;

// finds suitable masterserver for selected auth description higher than prevous search (or -1 on failure)
int findauthmaster(const char *desc, int prev)
{
    if(!desc) desc = "";
    for(int i = prev >= 0 ? prev + 1 : 0; i < masters.length(); i++)
        if(masters[i].masterauth && (!strcmp(desc, masters[i].masterauthdesc) || masters[i].masterauth >= 2)) return i;
    return -1;
}

bool usemastergbans(int master)
{
    return masters.inrange(master) ? masters[master].masterban : false;
}

int masterauthprivilege(int master)
{
    return masters.inrange(master) ? masters[master].activeprivilege : PRIV_NONE;
}

void setmasterprivilege(int master, int priv)
{
    if(!masters.inrange(master)) return;
    masters[master].activeprivilege = masters[master].allowsetprivilege ? priv : masters[master].masterauthprivilege;
}

void resetmasterprivilege(int master)
{
    if(!masters.inrange(master)) return;
    masters[master].activeprivilege = masters[master].masterauthprivilege;
}

int currentmaster = -1;

void disconnectmaster(int i = -1)
{
    if(i < 0)
    {
        loopv(masters) disconnectmaster(i);
        return;
    }

    if(i >= masters.length()) return;

    if(masters[i].mastersock != ENET_SOCKET_NULL)
    {
        server::masterdisconnected(i);
        enet_socket_destroy(masters[i].mastersock);
        masters[i].mastersock = ENET_SOCKET_NULL;
    }

    masters[i].masterout.setsize(0);
    masters[i].masterin.setsize(0);
    masters[i].masteroutpos = masters[i].masterinpos = 0;

    masters[i].masteraddress.host = ENET_HOST_ANY;
    masters[i].masteraddress.port = ENET_PORT_ANY;

    masters[i].lastupdatemaster = masters[i].masterconnecting = masters[i].masterconnected = 0;
    masters[i].activeprivilege = masters[i].masterauthprivilege;
}

ICOMMAND(resetmasters, "", (),
{
    disconnectmaster();
    masters.shrink(0);
    currentmaster = -1;
    server::cleargbans();
});

void addmaster(const char *s = NULL)
{
    masters.add();
    currentmaster = masters.length() - 1;
    if(s && s[0]) copystring(masters[currentmaster].mastername, s);
}
ICOMMAND(addmaster, "s", (const char *s), addmaster(s));

ICOMMAND(mastername, "s", (const char *s),
{
    if(!masters.inrange(currentmaster)) addmaster();
    disconnectmaster(currentmaster);
    server::cleargbans(currentmaster);
    copystring(masters[currentmaster].mastername, s);
});

ICOMMAND(masterport, "i", (int *i),
{
    if(!masters.inrange(currentmaster)) addmaster();
    disconnectmaster(currentmaster);
    server::cleargbans(currentmaster);
    int masterport = clamp(*i, 0, 0xFFFF);
    masters[currentmaster].masterport = masterport ? masterport : server::masterport();
});

// use this masterserver for auth? (0 - no, 1 - yes, 2 - use for all auth desc)
// note: your server may be banned if it regsiers but not use auth
ICOMMAND(masterauth, "i", (int *i),
{
    if(!masters.inrange(currentmaster)) addmaster();
    masters[currentmaster].masterauth = clamp(*i, 0, 2);
});

// desc of auth server
ICOMMAND(masterauthdesc, "s", (const char *s),
{
    if(!masters.inrange(currentmaster)) addmaster();
    copystring(masters[currentmaster].masterauthdesc, s ? s : "");
});

// privileges of auth claims for this masterserver
ICOMMAND(masterauthprivilege, "s", (char *s),
{
    if(!masters.inrange(currentmaster)) addmaster();
    int priv;
    switch(s[0])
    {
        case 'r': case 'R': case '4': priv = PRIV_ROOT; break;
        case 'a': case 'A': case '3': priv = PRIV_ADMIN; break;
        case 'c': case 'C': case '1': priv = PRIV_MASTER; break;
        case 'n': case 'N': case '0': priv = PRIV_NONE; break;
        case 'm': case 'M': case '2': default: priv = PRIV_AUTH; break;
    }
    bool diff = masters[currentmaster].masterauthprivilege != masters[currentmaster].activeprivilege;
    masters[currentmaster].masterauthprivilege = priv;
    if(!diff) masters[currentmaster].activeprivilege = priv;
});

ICOMMAND(masterextauthpriv, "i", (int *i),
{
    if(!masters.inrange(currentmaster)) addmaster();
    masters[currentmaster].allowsetprivilege = *i > 0;
});

// register to this masterserver?
ICOMMAND(masterreg, "i", (int *i),
{
    if(!masters.inrange(currentmaster)) addmaster();
    masters[currentmaster].masterreg = clamp(*i, 0, 1);
});

// use gbans from this masterserver?
// note: must be registered to server to use gbans from it
ICOMMAND(masterban, "i", (int *i),
{
    if(!masters.inrange(currentmaster)) addmaster();
    int usebans = clamp(*i, 0, 1);
    masters[currentmaster].masterban = usebans;
    if(!usebans) server::cleargbans(currentmaster);
});

ENetSocket connectmaster(int i, bool wait)
{
    if(!masters.inrange(i) || !masters[i].mastername[0]) return ENET_SOCKET_NULL;

    if(masters[i].masteraddress.host == ENET_HOST_ANY)
    {
        logoutf("looking up %s...", masters[i].mastername);
        masters[i].masteraddress.port = masters[i].masterport;
        if(!resolverwait(masters[i].mastername, &masters[i].masteraddress)) return ENET_SOCKET_NULL;
    }
    ENetSocket sock = enet_socket_create(ENET_SOCKET_TYPE_STREAM);
    if(sock == ENET_SOCKET_NULL)
    {
        logoutf("could not open master server socket");
        return ENET_SOCKET_NULL;
    }
    if(wait || serveraddress.host == ENET_HOST_ANY || !enet_socket_bind(sock, &serveraddress))
    {
        enet_socket_set_option(sock, ENET_SOCKOPT_NONBLOCK, 1);
        if(wait)
        {
            if(!connectwithtimeout(sock, masters[i].mastername, masters[i].masteraddress)) return sock;
        }
        else if(!enet_socket_connect(sock, &masters[i].masteraddress)) return sock;
    }
    enet_socket_destroy(sock);
    logoutf("could not connect to master server (%s)", masters[i].mastername);
    return ENET_SOCKET_NULL;
}

bool requestmaster(int i, const char *req)
{
    if(!masters.inrange(i)) return false;

    if(masters[i].mastersock == ENET_SOCKET_NULL)
    {
        masters[i].mastersock = connectmaster(i, false);
        if(masters[i].mastersock == ENET_SOCKET_NULL) return false;
        masters[i].lastconnectmaster = masters[i].masterconnecting = totalmillis ? totalmillis : 1;
    }

    if(masters[i].masterout.length() >= 4096) return false;

    masters[i].masterout.put(req, strlen(req));
    return true;
}

bool requestmasterf(int i, const char *fmt, ...)
{
    defvformatstring(req, fmt, fmt);
    return requestmaster(i, req);
}

void processmasterinput(int i)
{
    if(masters[i].masterinpos >= masters[i].masterin.length()) return;

    char *input = &masters[i].masterin[masters[i].masterinpos],
    *end = (char *)memchr(input, '\n', masters[i].masterin.length() - masters[i].masterinpos);
    while(end)
    {
        *end++ = '\0';

        const char *args = input;
        while(args < end && !iscubespace(*args)) args++;
        int cmdlen = args - input;
        while(args < end && iscubespace(*args)) args++;

        if(!strncmp(input, "failreg", cmdlen))
            conoutf(CON_ERROR, "master server (%s) registration failed: %s", masters[i].mastername, args);
        else if(!strncmp(input, "succreg", cmdlen))
            conoutf("master server (%s) registration succeeded", masters[i].mastername);
        else server::processmasterinput(i, input, cmdlen, args);

        masters[i].masterinpos = end - masters[i].masterin.getbuf();
        input = end;
        end = (char *)memchr(input, '\n', masters[i].masterin.length() - masters[i].masterinpos);
    }

    if(masters[i].masterinpos >= masters[i].masterin.length())
    {
        masters[i].masterin.setsize(0);
        masters[i].masterinpos = 0;
    }
}

void flushmasteroutput()
{
    loopv(masters)
    {
        if(masters[i].masterconnecting && totalmillis - masters[i].masterconnecting >= 60000)
        {
            logoutf("could not connect to master server (%s)", masters[i].mastername);
            disconnectmaster(i);
        }

        if(masters[i].masterout.empty() || !masters[i].masterconnected) continue;

        ENetBuffer buf;
        buf.data = &masters[i].masterout[masters[i].masteroutpos];
        buf.dataLength = masters[i].masterout.length() - masters[i].masteroutpos;
        int sent = enet_socket_send(masters[i].mastersock, NULL, &buf, 1);
        if(sent >= 0)
        {
            masters[i].masteroutpos += sent;
            if(masters[i].masteroutpos >= masters[i].masterout.length())
            {
                masters[i].masterout.setsize(0);
                masters[i].masteroutpos = 0;
            }
        }
        else disconnectmaster(i);
    }
}

void flushmasterinput(int i)
{
    if(masters[i].masterin.length() >= masters[i].masterin.capacity())
        masters[i].masterin.reserve(4096);

    ENetBuffer buf;
    buf.data = masters[i].masterin.getbuf() + masters[i].masterin.length();
    buf.dataLength = masters[i].masterin.capacity() - masters[i].masterin.length();
    int recv = enet_socket_receive(masters[i].mastersock, NULL, &buf, 1);
    if(recv > 0)
    {
        masters[i].masterin.advance(recv);
        processmasterinput(i);
    }
    else disconnectmaster(i);
}

static ENetAddress pongaddr;

void sendserverinforeply(ucharbuf &p)
{
    ENetBuffer buf;
    buf.data = p.buf;
    buf.dataLength = p.length();
    enet_socket_send(pongsock, &pongaddr, &buf, 1);
}

#define MAXPINGDATA 32

bool checkserversockets()        // reply all server info requests
{
    static ENetSocketSet readset, writeset;
    ENET_SOCKETSET_EMPTY(readset);
    ENET_SOCKETSET_EMPTY(writeset);
    ENetSocket maxsock = pongsock;
    ENET_SOCKETSET_ADD(readset, pongsock);
    loopv(masters)
    {
        if(masters[i].mastersock != ENET_SOCKET_NULL)
        {
            maxsock = max(maxsock, masters[i].mastersock);
            ENET_SOCKETSET_ADD(readset, masters[i].mastersock);
            if(!masters[i].masterconnected) ENET_SOCKETSET_ADD(writeset, masters[i].mastersock);
        }
    }
    if(lansock != ENET_SOCKET_NULL)
    {
        maxsock = max(maxsock, lansock);
        ENET_SOCKETSET_ADD(readset, lansock);
    }
    if(enet_socketset_select(maxsock, &readset, &writeset, 0) <= 0) return false;

    ENetBuffer buf;
    uchar pong[MAXTRANS];
    loopi(2)
    {
        ENetSocket sock = i ? lansock : pongsock;
        if(sock == ENET_SOCKET_NULL || !ENET_SOCKETSET_CHECK(readset, sock)) continue;

        buf.data = pong;
        buf.dataLength = sizeof(pong);
        int len = enet_socket_receive(sock, &pongaddr, &buf, 1);
        if(len < 0 || len > MAXPINGDATA) continue;
        ucharbuf req(pong, len), p(pong, sizeof(pong));
        p.len += len;
        server::serverinforeply(req, p);
    }

    loopv(masters)
    {
        if(masters[i].mastersock != ENET_SOCKET_NULL)
        {
            if(!masters[i].masterconnected)
            {
                if(ENET_SOCKETSET_CHECK(readset, masters[i].mastersock) || ENET_SOCKETSET_CHECK(writeset, masters[i].mastersock))
                {
                    int error = 0;
                    if(enet_socket_get_option(masters[i].mastersock, ENET_SOCKOPT_ERROR, &error) < 0 || error)
                    {
                        logoutf("could not connect to master server (%s)", masters[i].mastername);
                        disconnectmaster(i);
                    }
                    else
                    {
                        masters[i].masterconnecting = 0;
                        masters[i].masterconnected = totalmillis ? totalmillis : 1;
                        masters[i].activeprivilege = masters[i].masterauthprivilege;
                        server::masterconnected(i);
                    }
                }
            }
            if(masters[i].mastersock != ENET_SOCKET_NULL && ENET_SOCKETSET_CHECK(readset, masters[i].mastersock)) flushmasterinput(i);
        }
    }
    return true;
}

int curtime = 0, lastmillis = 0, totalmillis = 0;

void updatemasterserver(int i)
{
    if(!masters[i].masterconnected && masters[i].lastconnectmaster && totalmillis-masters[i].lastconnectmaster <= 5*60*1000) return;
    if(allowupdatemaster && masters[i].mastername[0] && masters[i].masterreg)
        requestmasterf(i, "regserv %d\n", serverport); //warning: if not registered, master gbans won't work
    masters[i].lastupdatemaster = totalmillis ? totalmillis : 1;
}

uint totalsecs = 0;

void updatetime()
{
    static int lastsec = 0;
    if(totalmillis - lastsec >= 1000)
    {
        int cursecs = (totalmillis - lastsec) / 1000;
        totalsecs += cursecs;
        lastsec += cursecs * 1000;
    }
}

VAR(serverpingsockpriority, 0, 0, 128);

void serverslice(bool dedicated, uint timeout)   // main server update, called from main loop in sp, or from below in dedicated server
{
    if(!serverhost)
    {
        server::serverupdate();
        server::sendpackets();
        return;
    }

    // below is network only

    int millis = (int)enet_time_get(), elapsed = millis - totalmillis;
    static int timeerr = 0;
    int scaledtime = server::scaletime(elapsed) + timeerr;
    curtime = scaledtime/100;
    timeerr = scaledtime%100;
    if(server::ispaused()) curtime = 0;
    lastmillis += curtime;
    totalmillis = millis;
    updatetime();
    checksleep(lastmillis);

    server::serverupdate();

    flushmasteroutput();
    int c = 0;
    while(checkserversockets() && ++c < serverpingsockpriority);

    if(allowupdatemaster && masters.empty()) addmaster();

    loopv(masters)
    {
        if(!masters[i].lastupdatemaster || totalmillis-masters[i].lastupdatemaster>60*60*1000)  // send alive signal to masterserver every hour of uptime
            updatemasterserver(i);
    }

    if(totalmillis-laststatus>60*1000)   // display bandwidth stats, useful for server ops
    {
        laststatus = totalmillis;
        if(nonlocalclients || serverhost->totalSentData || serverhost->totalReceivedData) logoutf("status: %d remote clients, %.1f send, %.1f rec (K/sec)", nonlocalclients, serverhost->totalSentData/60.0f/1024, serverhost->totalReceivedData/60.0f/1024);
        serverhost->totalSentData = serverhost->totalReceivedData = 0;
    }

    ENetEvent event;
    bool serviced = false;
    while(!serviced)
    {
        if(enet_host_check_events(serverhost, &event) <= 0)
        {
            if(enet_host_service(serverhost, &event, timeout) <= 0) break;
            serviced = true;
        }
        switch(event.type)
        {
            case ENET_EVENT_TYPE_CONNECT:
            {
                client &c = addclient(ST_TCPIP);
                c.peer = event.peer;
                c.peer->data = &c;
                char hn[1024];
                copystring(c.hostname, (enet_address_get_host_ip(&c.peer->address, hn, sizeof(hn))==0) ? hn : "unknown");
                logoutf("client connected (%s)", c.hostname);
                int reason = server::clientconnect(c.num, c.peer->address.host);
                if(reason) disconnect_client(c.num, reason);
                break;
            }
            case ENET_EVENT_TYPE_RECEIVE:
            {
                client *c = (client *)event.peer->data;
                if(c) process(event.packet, c->num, event.channelID);
                if(event.packet->referenceCount==0) enet_packet_destroy(event.packet);
                break;
            }
            case ENET_EVENT_TYPE_DISCONNECT:
            {
                client *c = (client *)event.peer->data;
                if(!c) break;
                server::clientdisconnect(c->num, DISC_NONE);
                logoutf("disconnected client (%s)", c->hostname);
                delclient(c);
                break;
            }
            default:
                break;
        }
    }
    if(server::sendpackets()) enet_host_flush(serverhost);
}

void flushserver(bool force)
{
    if(server::sendpackets(force) && serverhost) enet_host_flush(serverhost);
}

#ifdef WIN32
#include "shellapi.h"

#define IDI_ICON1 1

static string apptip = "";
static HINSTANCE appinstance = NULL;
static ATOM wndclass = 0;
static HWND appwindow = NULL, conwindow = NULL;
static HICON appicon = NULL;
static HMENU appmenu = NULL;
static HANDLE outhandle = NULL;
static const int MAXLOGLINES = 200;
struct logline { int len; char buf[LOGSTRLEN]; };
static queue<logline, MAXLOGLINES> loglines;

static void cleanupsystemtray()
{
    NOTIFYICONDATA nid;
    memset(&nid, 0, sizeof(nid));
    nid.cbSize = sizeof(nid);
    nid.hWnd = appwindow;
    nid.uID = IDI_ICON1;
    Shell_NotifyIcon(NIM_DELETE, &nid);
}

static bool setupsystemtray(UINT uCallbackMessage)
{
	NOTIFYICONDATA nid;
	memset(&nid, 0, sizeof(nid));
	nid.cbSize = sizeof(nid);
	nid.hWnd = appwindow;
	nid.uID = IDI_ICON1;
	nid.uCallbackMessage = uCallbackMessage;
	nid.uFlags = NIF_MESSAGE | NIF_ICON | NIF_TIP;
	nid.hIcon = appicon;
	strcpy(nid.szTip, apptip);
	if(Shell_NotifyIcon(NIM_ADD, &nid) != TRUE)
        return false;
    atexit(cleanupsystemtray);
    return true;
}

#if 0
static bool modifysystemtray()
{
	NOTIFYICONDATA nid;
	memset(&nid, 0, sizeof(nid));
	nid.cbSize = sizeof(nid);
	nid.hWnd = appwindow;
	nid.uID = IDI_ICON1;
	nid.uFlags = NIF_TIP;
	strcpy(nid.szTip, apptip);
	return Shell_NotifyIcon(NIM_MODIFY, &nid) == TRUE;
}
#endif

static void cleanupwindow()
{
	if(!appinstance) return;
	if(appmenu)
	{
		DestroyMenu(appmenu);
		appmenu = NULL;
	}
	if(wndclass)
	{
		UnregisterClass(MAKEINTATOM(wndclass), appinstance);
		wndclass = 0;
	}
}

static BOOL WINAPI consolehandler(DWORD dwCtrlType)
{
    switch(dwCtrlType)
    {
        case CTRL_C_EVENT:
        case CTRL_BREAK_EVENT:
        case CTRL_CLOSE_EVENT:
            exit(EXIT_SUCCESS);
            return TRUE;
    }
    return FALSE;
}

static void writeline(logline &line)
{
    static uchar ubuf[512];
    int len = strlen(line.buf), carry = 0;
    while(carry < len)
    {
        int numu = encodeutf8(ubuf, sizeof(ubuf), &((uchar *)line.buf)[carry], len - carry, &carry);
        DWORD written = 0;
        WriteConsole(outhandle, ubuf, numu, &written, NULL);
    }
}

static void setupconsole()
{
	if(conwindow) return;
    if(!AllocConsole()) return;
	SetConsoleCtrlHandler(consolehandler, TRUE);
	conwindow = GetConsoleWindow();
    SetConsoleTitle(apptip);
	//SendMessage(conwindow, WM_SETICON, ICON_SMALL, (LPARAM)appicon);
	SendMessage(conwindow, WM_SETICON, ICON_BIG, (LPARAM)appicon);
    outhandle = GetStdHandle(STD_OUTPUT_HANDLE);
    CONSOLE_SCREEN_BUFFER_INFO coninfo;
    GetConsoleScreenBufferInfo(outhandle, &coninfo);
    coninfo.dwSize.Y = MAXLOGLINES;
    SetConsoleScreenBufferSize(outhandle, coninfo.dwSize);
    SetConsoleCP(CP_UTF8);
    SetConsoleOutputCP(CP_UTF8);
    loopv(loglines) writeline(loglines[i]);
}

enum
{
	MENU_OPENCONSOLE = 0,
	MENU_SHOWCONSOLE,
	MENU_HIDECONSOLE,
	MENU_EXIT,
    MENU_RELOADCFG
};

static LRESULT CALLBACK handlemessages(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch(uMsg)
	{
		case WM_APP:
			SetForegroundWindow(hWnd);
			switch(lParam)
			{
				//case WM_MOUSEMOVE:
				//	break;
				case WM_LBUTTONUP:
				case WM_RBUTTONUP:
				{
					POINT pos;
					GetCursorPos(&pos);
					TrackPopupMenu(appmenu, TPM_CENTERALIGN|TPM_BOTTOMALIGN|TPM_RIGHTBUTTON, pos.x, pos.y, 0, hWnd, NULL);
					PostMessage(hWnd, WM_NULL, 0, 0);
					break;
				}
			}
			return 0;
		case WM_COMMAND:
			switch(LOWORD(wParam))
			{
                case MENU_OPENCONSOLE:
					setupconsole();
					if(conwindow) ModifyMenu(appmenu, 0, MF_BYPOSITION|MF_STRING, MENU_HIDECONSOLE, "Hide Console");
                    break;
				case MENU_SHOWCONSOLE:
					ShowWindow(conwindow, SW_SHOWNORMAL);
					ModifyMenu(appmenu, 0, MF_BYPOSITION|MF_STRING, MENU_HIDECONSOLE, "Hide Console");
					break;
				case MENU_HIDECONSOLE:
					ShowWindow(conwindow, SW_HIDE);
					ModifyMenu(appmenu, 0, MF_BYPOSITION|MF_STRING, MENU_SHOWCONSOLE, "Show Console");
					break;
				case MENU_EXIT:
					PostMessage(hWnd, WM_CLOSE, 0, 0);
					break;
                case MENU_RELOADCFG:
                    reloadcfg = true;
                    break;
			}
			return 0;
		case WM_CLOSE:
			PostQuitMessage(0);
			return 0;
	}
	return DefWindowProc(hWnd, uMsg, wParam, lParam);
}

static void setupwindow(const char *title)
{
	copystring(apptip, title);
	//appinstance = GetModuleHandle(NULL);
	if(!appinstance) fatal("failed getting application instance");
	appicon = LoadIcon(appinstance, MAKEINTRESOURCE(IDI_ICON1));//(HICON)LoadImage(appinstance, MAKEINTRESOURCE(IDI_ICON1), IMAGE_ICON, 0, 0, LR_DEFAULTSIZE);
	if(!appicon) fatal("failed loading icon");

	appmenu = CreatePopupMenu();
	if(!appmenu) fatal("failed creating popup menu");
    AppendMenu(appmenu, MF_STRING, MENU_OPENCONSOLE, "Open Console");
    AppendMenu(appmenu, MF_STRING, MENU_RELOADCFG, "Reload configuration");
    AppendMenu(appmenu, MF_SEPARATOR, 0, NULL);
	AppendMenu(appmenu, MF_STRING, MENU_EXIT, "Exit");
	//SetMenuDefaultItem(appmenu, 0, FALSE);

	WNDCLASS wc;
	memset(&wc, 0, sizeof(wc));
	wc.hCursor = NULL; //LoadCursor(NULL, IDC_ARROW);
	wc.hIcon = appicon;
	wc.lpszMenuName = NULL;
	wc.lpszClassName = title;
	wc.style = 0;
	wc.hInstance = appinstance;
	wc.lpfnWndProc = handlemessages;
	wc.cbWndExtra = 0;
	wc.cbClsExtra = 0;
	wndclass = RegisterClass(&wc);
	if(!wndclass) fatal("failed registering window class");

	appwindow = CreateWindow(MAKEINTATOM(wndclass), title, 0, CW_USEDEFAULT, CW_USEDEFAULT, 0, 0, HWND_MESSAGE, NULL, appinstance, NULL);
	if(!appwindow) fatal("failed creating window");

	atexit(cleanupwindow);

    if(!setupsystemtray(WM_APP)) fatal("failed adding to system tray");
}

static char *parsecommandline(const char *src, vector<char *> &args)
{
    char *buf = new char[strlen(src) + 1], *dst = buf;
    for(;;)
    {
        while(isspace(*src)) src++;
        if(!*src) break;
        args.add(dst);
		for(bool quoted = false; *src && (quoted || !isspace(*src)); src++)
        {
            if(*src != '"') *dst++ = *src;
			else if(dst > buf && src[-1] == '\\') dst[-1] = '"';
			else quoted = !quoted;
		}
		*dst++ = '\0';
    }
    args.add(NULL);
    return buf;
}


int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrev, LPSTR szCmdLine, int sw)
{
    vector<char *> args;
    char *buf = parsecommandline(GetCommandLine(), args);
	appinstance = hInst;
    int standalonemain(int argc, char **argv);
    int status = standalonemain(args.length()-1, args.getbuf());
    #define main standalonemain
    delete[] buf;
    exit(status);
    return 0;
}

void logoutfv(const char *fmt, va_list args)
{
    if(appwindow)
    {
        logline &line = loglines.add();
        vformatstring(line.buf, fmt, args, sizeof(line.buf));
        if(logfile) writelog(logfile, line.buf);
        line.len = min(strlen(line.buf), sizeof(line.buf)-2);
        line.buf[line.len++] = '\n';
        line.buf[line.len] = '\0';
        if(outhandle) writeline(line);
    }
    else if(logfile) writelogv(logfile, fmt, args);
}

#else

#include <signal.h>

void logoutfv(const char *fmt, va_list args)
{
    FILE *f = getlogfile();
    if(f) writelogv(f, fmt, args);
}

void reloadsignal(int signum)
{
    reloadcfg = true;
}

void termsignal(int signum)
{
    quitserver = true;
}

#endif

bool isdedicatedserver() { return true; }

void rundedicatedserver()
{
    logoutf("dedicated server started, waiting for clients...");
#ifdef WIN32
    SetPriorityClass(GetCurrentProcess(), HIGH_PRIORITY_CLASS);
#else
    signal(SIGUSR1, reloadsignal);
    signal(SIGTERM, termsignal);
    signal(SIGINT, termsignal);
#endif
    for(;;)
    {
#ifdef WIN32
        MSG msg;
        while(PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))
        {
            if(msg.message == WM_QUIT) { quitserver = true; break; }
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
#endif
        if(quitserver) break;
        serverslice(true, 5);
        if(reloadcfg)
        {
            logoutf("reloading server configuration");
            execfile(configfile, false);
            reloadcfg = false;
        }
    }

    //quit server
    logoutf("server is shutting down...");
    server::serverclose();
    //notify clients we stopped working
    loopv(clients) if(clients[i]->type == ST_TCPIP)
    {
        enet_peer_disconnect_now(clients[i]->peer, DISC_NONE);
        delclient(clients[i]);
    }
    //cleanup
    cleanupserver();
    closelogfile();
    return;
}

bool servererror(const char *desc)
{
    fatal("%s", desc);
    return false;
}

VAR(uselansock, 0, 1, 1);

bool setuplistenserver(void)
{
    ENetAddress address = { ENET_HOST_ANY, enet_uint16(serverport <= 0 ? server::serverport() : serverport) };
    if(*serverip)
    {
        if(enet_address_set_host(&address, serverip)<0) conoutf(CON_WARN, "WARNING: server ip not resolved");
        else serveraddress.host = address.host;
    }
    serverhost = enet_host_create(&address, min(max(maxclients + server::reserveclients(), maxslots), MAXCLIENTS), server::numchannels(), serverdownrate, serveruprate);
    if(!serverhost) return servererror("could not create server host");
    serverhost->duplicatePeers = maxdupclients ? maxdupclients : MAXCLIENTS;
    address.port = server::serverinfoport(serverport > 0 ? serverport : -1);
    pongsock = enet_socket_create(ENET_SOCKET_TYPE_DATAGRAM);
    if(pongsock != ENET_SOCKET_NULL && enet_socket_bind(pongsock, &address) < 0)
    {
        enet_socket_destroy(pongsock);
        pongsock = ENET_SOCKET_NULL;
    }
    if(pongsock == ENET_SOCKET_NULL) return servererror("could not create server info socket");
    else enet_socket_set_option(pongsock, ENET_SOCKOPT_NONBLOCK, 1);
    address.port = server::laninfoport();
    lansock = uselansock ? enet_socket_create(ENET_SOCKET_TYPE_DATAGRAM) : ENET_SOCKET_NULL;
    if(lansock != ENET_SOCKET_NULL && (enet_socket_set_option(lansock, ENET_SOCKOPT_REUSEADDR, 1) < 0 || enet_socket_bind(lansock, &address) < 0))
    {
        enet_socket_destroy(lansock);
        lansock = ENET_SOCKET_NULL;
    }
    if(lansock == ENET_SOCKET_NULL) { if(uselansock) conoutf(CON_WARN, "WARNING: could not create LAN server info socket"); }
    else enet_socket_set_option(lansock, ENET_SOCKOPT_NONBLOCK, 1);
    return true;
}

void initserver(void)
{
#ifdef WIN32
    setupwindow("Cube 2: Sauerbraten server (zeromod)");
#endif

    execfile(configfile, false);

    setuplistenserver();

    server::serverinit();

    if(allowupdatemaster && masters.empty()) addmaster();

    loopv(masters) updatemasterserver(i);
    rundedicatedserver();
}

bool serveroption(char *opt)
{
    switch(opt[1])
    {
        case 'u': setvar("serveruprate", atoi(opt+2)); return true;
        case 'd': setvar("serverdownrate", atoi(opt+2)); return true;
        case 'c': setvar("maxclients", atoi(opt+2)); return true;
        case 'i': setsvar("serverip", opt+2); return true;
        case 'j': setvar("serverport", atoi(opt+2)); return true;
        case 'm':
        {
            if(!masters.inrange(currentmaster)) addmaster();
            copystring(masters[currentmaster].mastername, opt+2);
            allowupdatemaster = masters[currentmaster].mastername[0] ? 1 : 0;
            return true;
        }
        case 'q': logoutf("Using home directory: %s", opt); sethomedir(opt+2); return true;
        case 'k': logoutf("Adding package directory: %s", opt); addpackagedir(opt+2); return true;
        case 'g': logoutf("Setting log file: %s", opt); setlogfile(opt+2); return true;
        case 's': logoutf("Setting configuration file: %s", opt+2); setsvar("configfile", opt+2); return true;
        default: return false;
    }
}

vector<const char *> gameargs;

int main(int argc, char **argv)
{
    setlogfile(NULL);
    if(enet_initialize()<0) fatal("Unable to initialise network module");
    atexit(enet_deinitialize);
    enet_time_set(0);
    for(int i = 1; i<argc; i++) if(argv[i][0]!='-' || !serveroption(argv[i])) gameargs.add(argv[i]);
    game::parseoptions(gameargs);
    initserver();
    return EXIT_SUCCESS;
}
