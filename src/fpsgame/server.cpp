#include "game.h"
#include "modules.h"
#include "anticheat.h"

// TODO:
/*
 * time btw multikills: 2 secs
 * kills without deaths:
 * 5: killing sphere
 * 10: rampage
 * 15: dominating
 * 20: unstoppable
 * 30: godlike
 */

namespace game
{
    void parseoptions(vector<const char *> &args)
    {
        loopv(args)
            if(!server::serveroption(args[i]))
                conoutf(CON_ERROR, "unknown command-line option: %s", args[i]);
    }

    const char *gameident() { return "fps"; }
}

extern ENetAddress masteraddress;

namespace server
{
    struct _hookparam
    {
        void *args[16];
    } __attribute__((packed));

    _hookparam _hp;

    int _exechook(const char *name);
    static void _enablefunc(const char *s, bool val);
    void _initfuncs();
    void _initman();

    struct server_entity            // server side version of "entity" type
    {
        int type;
        int spawntime;
        char spawned;
    };

    static const int DEATHMILLIS = 300;

    struct clientinfo;

    struct gameevent
    {
        virtual ~gameevent() {}

        virtual bool flush(clientinfo *ci, int fmillis);
        virtual void process(clientinfo *ci) {}

        virtual bool keepable() const { return false; }
    };

    struct timedevent : gameevent
    {
        int millis;

        bool flush(clientinfo *ci, int fmillis);
    };

    struct hitinfo
    {
        int target;
        int lifesequence;
        int rays;
        float dist;
        vec dir;
    };

    struct shotevent : timedevent
    {
        int id, gun;
        vec from, to;
        vector<hitinfo> hits;

        void process(clientinfo *ci);
    };

    struct explodeevent : timedevent
    {
        int id, gun;
        vector<hitinfo> hits;

        bool keepable() const { return true; }

        void process(clientinfo *ci);
    };

    struct suicideevent : gameevent
    {
        void process(clientinfo *ci);
    };

    struct pickupevent : gameevent
    {
        int ent;

        void process(clientinfo *ci);
    };

    template <int N>
    struct projectilestate
    {
        int projs[N];
        int numprojs;

        projectilestate() : numprojs(0) {}

        void reset() { numprojs = 0; }

        void add(int val)
        {
            if(numprojs>=N) numprojs = 0;
            projs[numprojs++] = val;
        }

        bool remove(int val)
        {
            loopi(numprojs) if(projs[i]==val)
            {
                projs[i] = projs[--numprojs];
                return true;
            }
            return false;
        }
    };

    struct gamestate : fpsstate
    {
        vec o;
        int state, editstate;
        int lastdeath, deadflush, lastspawn, lifesequence;
        int lastshot;
        projectilestate<8> rockets, grenades;
        int frags, flags, deaths, teamkills, shotdamage, damage, tokens;
        //zeromod
        int _suicides, _stolen, _returned;
        float yaw, pitch;
        int lastkill, multikills, rampage;
        ////
        int lasttimeplayed, timeplayed;
        float effectiveness;

        gamestate() : state(CS_DEAD), editstate(CS_DEAD), lifesequence(0) {}

        bool isalive(int gamemillis)
        {
            return state==CS_ALIVE || (state==CS_DEAD && gamemillis - lastdeath <= DEATHMILLIS);
        }

        bool waitexpired(int gamemillis)
        {
            return gamemillis - lastshot >= gunwait;
        }

        void reset()
        {
            if(state!=CS_SPECTATOR) state = editstate = CS_DEAD;
            maxhealth = 100;
            rockets.reset();
            grenades.reset();

            timeplayed = 0;
            effectiveness = 0;
            frags = flags = deaths = teamkills = shotdamage = damage = tokens = 0;
            //zeromod
            _suicides = _stolen = _returned = 0;
            yaw = pitch = 0;
            ////

            lastdeath = 0;

            respawn();
        }

        void respawn()
        {
            fpsstate::respawn();
            o = vec(-1e10f, -1e10f, -1e10f);
            deadflush = 0;
            lastspawn = -1;
            lastshot = 0;
            tokens = 0;
            //zeromod
            lastkill = 0;
            multikills = 0;
            rampage = 0;
            ////
        }

        void reassign()
        {
            respawn();
            rockets.reset();
            grenades.reset();
        }
    };

    struct savedscore
    {
        uint ip;
        string name;
        int maxhealth, frags, flags, deaths, teamkills, shotdamage, damage;
        int timeplayed;
        float effectiveness;

        void save(gamestate &gs)
        {
            maxhealth = gs.maxhealth;
            frags = gs.frags;
            flags = gs.flags;
            deaths = gs.deaths;
            teamkills = gs.teamkills;
            shotdamage = gs.shotdamage;
            damage = gs.damage;
            timeplayed = gs.timeplayed;
            effectiveness = gs.effectiveness;
        }

        void restore(gamestate &gs)
        {
            if(gs.health==gs.maxhealth) gs.health = maxhealth;
            gs.maxhealth = maxhealth;
            gs.frags = frags;
            gs.flags = flags;
            gs.deaths = deaths;
            gs.teamkills = teamkills;
            gs.shotdamage = shotdamage;
            gs.damage = damage;
            gs.timeplayed = timeplayed;
            gs.effectiveness = effectiveness;
        }
    };

    extern int gamemillis, nextexceeded;

    struct _extrainfo
    {
        int mute;
        int editmute;
        int editmutewarn;
        int forcedspectator;
        int namemute;
        //int teammute;
        //bool fakeprivon;
        //int fakepriv;
        bool spy;
        int failpass;
        clientinfo *tkiller;
        int votekickvictim;
        char acinfo[AC_MAX];
        int lastmsg, msgnum;
        int lastremip, remipnum;
        int teamkillmessageindex;
        int lasttakeflag;
    };

    struct clientinfo
    {
        int clientnum, ownernum, connectmillis, sessionid, overflow;
        string name, team, mapvote;
        int playermodel;
        int modevote;
        int privilege;
        bool connected, local, timesync;
        int gameoffset, lastevent, pushed, exceeded;
        gamestate state;
        vector<gameevent *> events;
        vector<uchar> position, messages;
        uchar *wsdata;
        int wslen;
        vector<clientinfo *> bots;
        int ping, aireinit;
        string clientmap;
        int mapcrc;
        bool warned, gameclip;
        ENetPacket *getdemo, *getmap, *clipboard;
        int lastclipboard, needclipboard;
        int connectauth;
        uint authreq;
        string authname, authdesc;
        void *authchallenge;
        int authkickvictim;
        char *authkickreason;
        int authmaster;
        _extrainfo _xi;
        char *disconnectreason;

        clientinfo() : getdemo(NULL), getmap(NULL), clipboard(NULL), authchallenge(NULL), authkickreason(NULL) { reset(); }
        ~clientinfo() { events.deletecontents(); cleanclipboard(); cleanauth(); }

        void addevent(gameevent *e)
        {
            if(state.state==CS_SPECTATOR || events.length()>100) delete e;
            else events.add(e);
        }

        enum
        {
            PUSHMILLIS = 3000
        };

        int calcpushrange()
        {
            ENetPeer *peer = getclientpeer(ownernum);
            return PUSHMILLIS + (peer ? peer->roundTripTime + peer->roundTripTimeVariance : ENET_PEER_DEFAULT_ROUND_TRIP_TIME);
        }

        bool checkpushed(int millis, int range)
        {
            return millis >= pushed - range && millis <= pushed + range;
        }

        void scheduleexceeded()
        {
            if(state.state!=CS_ALIVE || !exceeded) return;
            int range = calcpushrange();
            if(!nextexceeded || exceeded + range < nextexceeded) nextexceeded = exceeded + range;
        }

        void setexceeded()
        {
            if(state.state==CS_ALIVE && !exceeded && !checkpushed(gamemillis, calcpushrange())) exceeded = gamemillis;
            scheduleexceeded();
        }

        void setpushed()
        {
            pushed = max(pushed, gamemillis);
            if(exceeded && checkpushed(exceeded, calcpushrange())) exceeded = 0;
        }

        bool checkexceeded()
        {
            return state.state==CS_ALIVE && exceeded && gamemillis > exceeded + calcpushrange();
        }

        void mapchange()
        {
            mapvote[0] = 0;
            modevote = INT_MAX;
            state.reset();
            events.deletecontents();
            overflow = 0;
            timesync = false;
            lastevent = 0;
            exceeded = 0;
            pushed = 0;
            clientmap[0] = '\0';
            mapcrc = 0;
            warned = false;
            gameclip = false;
            _xi.tkiller = 0;
            _xi.lasttakeflag = 0;
            if(_xi.mute && _xi.mute < 2) _xi.mute = 0;
            if(_xi.editmute && _xi.editmute < 2) _xi.editmute = 0;
            if(_xi.forcedspectator && _xi.forcedspectator < 2) _xi.forcedspectator = 0;
        }

        void reassign()
        {
            state.reassign();
            events.deletecontents();
            timesync = false;
            lastevent = 0;
        }

        void cleanclipboard(bool fullclean = true)
        {
            if(clipboard) { if(--clipboard->referenceCount <= 0) enet_packet_destroy(clipboard); clipboard = NULL; }
            if(fullclean) lastclipboard = 0;
        }

        void cleanauthkick()
        {
            authkickvictim = -1;
            DELETEA(authkickreason);
        }

        void cleanauth(bool full = true)
        {
            authreq = 0;
            authmaster = -1;
            if(authchallenge) { freechallenge(authchallenge); authchallenge = NULL; }
            if(full) cleanauthkick();
        }

        void reset()
        {
            name[0] = team[0] = 0;
            playermodel = -1;
            privilege = PRIV_NONE;
            connected = local = false;
            connectauth = 0;
            position.setsize(0);
            messages.setsize(0);
            ping = 0;
            aireinit = 0;
            needclipboard = 0;
            cleanclipboard();
            cleanauth();
            memset(&_xi, 0, sizeof(_extrainfo));
            _xi.votekickvictim = -1;
            mapchange();
        }

        int geteventmillis(int servmillis, int clientmillis)
        {
            if(!timesync || (events.empty() && state.waitexpired(servmillis)))
            {
                timesync = true;
                gameoffset = servmillis - clientmillis;
                return servmillis;
            }
            else return gameoffset + clientmillis;
        }
    };

    struct ban
    {
        int time, expire;
        uint ip;
    };

    namespace aiman
    {
        extern void removeai(clientinfo *ci);
        extern void clearai();
        extern void checkai();
        extern void reqadd(clientinfo *ci, int skill);
        extern void reqdel(clientinfo *ci);
        extern void setbotlimit(clientinfo *ci, int limit);
        extern void setbotbalance(clientinfo *ci, bool balance);
        extern void changemap();
        extern void addclient(clientinfo *ci);
        extern void changeteam(clientinfo *ci);
    }

    #define MM_MODE 0xF
    #define MM_AUTOAPPROVE 0x1000
    #define MM_PRIVSERV (MM_MODE | MM_AUTOAPPROVE)
    #define MM_PUBSERV ((1<<MM_OPEN) | (1<<MM_VETO))
    #define MM_COOPSERV (MM_AUTOAPPROVE | MM_PUBSERV | (1<<MM_LOCKED))

    bool notgotitems = true;        // true when map has changed and waiting for clients to send item
    int gamemode = 0;
    int gamemillis = 0, gamelimit = 0, nextexceeded = 0, gamespeed = 100;
    bool gamepaused = false, shouldstep = true;

    void checkmaps(int req = -1);
    const char *colorname(clientinfo *ci, char *name = NULL);
    int _getpriv(clientinfo *ci);
	void _forcespectator(clientinfo *ci, int spec);

    string smapname = "";
    int interm = 0;
    enet_uint32 lastsend = 0;
    int mastermode = MM_OPEN, mastermask = MM_PRIVSERV;
    stream *mapdata = NULL;

    vector<uint> allowedips;
    vector<ban> bannedips;

    void addban(uint ip, int expire)
    {
        allowedips.removeobj(ip);
        ban b;
        b.time = totalmillis;
        b.expire = totalmillis + expire;
        b.ip = ip;
        loopv(bannedips) if(b.expire < bannedips[i].expire) { bannedips.insert(i, b); return; }
        bannedips.add(b);
    }

    vector<clientinfo *> connects, clients, bots;

    void kickclients(uint ip, clientinfo *actor = NULL, int priv = PRIV_NONE)
    {
        loopvrev(clients)
        {
            clientinfo &c = *clients[i];
            if(c.state.aitype != AI_NONE || c.privilege >= PRIV_ADMIN) continue;
            if(actor && (c.privilege > priv || c.clientnum == actor->clientnum)) continue;
            if(getclientip(c.clientnum) == ip) disconnect_client(c.clientnum, DISC_KICK);
        }
    }

    struct _scheduled_disconnect
    {
        int n, reason;
    };
    vector<_scheduled_disconnect> _scheduled_disconnects;

    void _schedule_disconnect(int n, int reason)
    {
        loopvrev(_scheduled_disconnects) if(_scheduled_disconnects[i].n == n) return;
         _scheduled_disconnect d;
         d.n = n;
         d.reason = reason;
         _scheduled_disconnects.add(d);
    }

    VAR(anticheat, 0, 3, 4);
/*
 *  0=disable, 1=warn on cheat,
 *  2=warn&disc on cheat, 3=warn&disc normal cheats, warn beta,
 *  4=warn&disc on all cheats (not recomended)
 */
    VAR(anticheatmessages, 0, 2, 2);    // 0=everyone, 1=masters, 2=admins

    VAR(anticheat_bantime, -1, 0, 14*24*60);  // ban time, in minutes

    void addpban(const char *name, const char *reason);

    void _cheater(clientinfo *ci, const char *s, int type, int n)
    {
        clientinfo *owner;
        string msg;

        if(!ci || anticheat <= 0) return;

        if(ci->state.aitype == AI_NONE) owner = ci;
        else
        {
            owner = (clientinfo *)getclientinfo(ci->ownernum);
            if(!owner) return;
        }

        if(type < 0 || type >= AC_MAX) return;

        if(!s) s = cheats[type].name;

        if(n > 0)
        {
            owner->_xi.acinfo[type] = char(min(int(owner->_xi.acinfo[type]) + min(n, 100), 100));
            n = int(owner->_xi.acinfo[type]);

            if(ci->state.aitype == AI_NONE)
            {
                formatstring(msg)("\f3[AC] \f2Cheater: \f7%s \f5(%i) \f2Type: \f7%s \f1[\f0%03i\f1]",
                    ci->name, ci->clientnum, s, n);
            }
            else
            {
                formatstring(msg)("\f3[AC] \f2Cheater: \f7%s \f5[%i] \f2(Owner: \f7%s \f5(%i)\f2) Type: \f7%s \f1[\f0%03i\f1]",
                    ci->name, ci->clientnum, owner->name, owner->clientnum, s, n);
            }
        }
        else
        {
            if(anticheat < 3) return;
            if(ci->state.aitype == AI_NONE)
            {
                formatstring(msg)("\f3[AC] \f2Cheater: \f7%s \f5(%i) \f2Type: \f7%s \f1[BETA]",
                    ci->name, ci->clientnum, s);
            }
            else
            {
                formatstring(msg)("\f3[AC] \f2Cheater: \f7%s \f5[%i] \f2(Owner: \f7%s \f5(%i)\f2) Type: \f7%s \f1[BETA]",
                    ci->name, ci->clientnum, owner->name, owner->clientnum, s);
            }
        }

        logoutf("%s", msg);

        if(anticheatmessages)
        {
            loopv(clients)
            {
                if(clients[i]->state.aitype == AI_NONE && clients[i]->privilege >= (anticheatmessages == 2 ? PRIV_ADMIN : PRIV_MASTER))
                    sendf(clients[i]->clientnum, 1, "ris", N_SERVMSG, msg);
            }
        }
        else sendf(-1, 1, "ris", N_SERVMSG, msg);

        if(anticheat <= 1 || ci->privilege >= PRIV_ADMIN || owner->privilege >= PRIV_ADMIN) return;

        if(n >= 100 || (anticheat >= 4 && n == 0))
        {
            if(anticheat_bantime == 0 || n == 0) _schedule_disconnect(ci->ownernum, DISC_MSGERR);
            else
            {
                uint ip = getclientip(ci->ownernum);
                if(anticheat_bantime > 0) addban(ip, anticheat_bantime*60000);
                else addpban(getclienthostname(ci->ownernum), NULL);
                loopvrev(clients)
                {
                    clientinfo &c = *clients[i];
                    if(c.state.aitype != AI_NONE || c.privilege >= PRIV_ADMIN) continue;
                    if(getclientip(c.clientnum) == ip) _schedule_disconnect(c.clientnum, DISC_KICK);
                }
            }
        }
    }

    struct _flagrun
    {
        char *map;
        int gamemode;
        char *name;
        int timeused;
    };
    vector<_flagrun> _flagruns;

    VAR(serverflagruns, 0, 0, 1);

    int _newflagrun = 0;
    void _doflagrun(clientinfo *ci, int timeused)
    {
        if(timeused <= 500)
        {
            _cheater(ci, "flaghack", AC_FLAGHACK, 50);
            return;
        }
        if(serverflagruns)
        {
            _flagrun *fr = 0;
            loopv(_flagruns) if(_flagruns[i].gamemode == gamemode && !strcmp(_flagruns[i].map, smapname))
            { fr = &_flagruns[i]; break; }
            bool isbest = false;
            if(!fr)
            {
                isbest = true;
                int lastfr = _flagruns.length();
                if(lastfr >= 1024) return;
                _flagruns.add();
                _flagruns[lastfr].map = newstring(smapname);
                _flagruns[lastfr].gamemode = gamemode;
                _flagruns[lastfr].name = newstring(ci->name);
                _flagruns[lastfr].timeused = timeused;
                fr = &_flagruns[lastfr];
            }
            isbest = isbest || timeused <= fr->timeused;
            if(isbest)
            {
                _newflagrun = 1;
                if(strcmp(ci->name, fr->name))
                {
                    DELETEA(fr->name);
                    fr->name = newstring(ci->name);
                }
                fr->timeused = timeused;
            }
            string msg;
            if(isbest) formatstring(msg)("\f0[flagrun] \f7%s \f2did flagrun in \f0%i.%02i seconds \f1(\f0best\f1)",
                colorname(ci), timeused/1000, (timeused%1000)/10);
            else formatstring(msg)("\f0[flagrun] \f7%s \f2did flagrun in \f0%i.%02i seconds \f1(\f0best: \f7%s \f6%i.%02i\f1)",
                colorname(ci), timeused/1000, (timeused%1000)/10, fr->name, fr->timeused/1000, (fr->timeused%1000)/10);
            sendservmsg(msg);
        }
    }

    void addflagrun(int mode, const char *map, int timeused, const char *name)
    {
        _flagrun *fr = 0;
        loopv(_flagruns) if(_flagruns[i].gamemode == mode && !strcmp(_flagruns[i].map, map))
        {
            fr = &_flagruns[i];
            break;
        }
        if(!fr)
        {
            int lastfr = _flagruns.length();
            if(lastfr >= 1024) return;
            _flagruns.add();
            _flagruns[lastfr].map = newstring(map);
            _flagruns[lastfr].gamemode = mode;
            _flagruns[lastfr].name = newstring(name);
            _flagruns[lastfr].timeused = timeused;
            fr = &_flagruns[lastfr];
        }

        if(strcmp(name, fr->name))
        {
            DELETEA(fr->name);
            fr->name = newstring(name);
        }
        fr->timeused = timeused;
    }
    ICOMMAND(flagrun, "isis", (int *i, const char *s, int *j, const char *z), addflagrun(*i, s, *j, z));

    struct maprotation
    {
        static int exclude;
        int modes;
        string map;

        int calcmodemask() const { return modes&(1<<NUMGAMEMODES) ? modes & ~exclude : modes; }
        bool hasmode(int mode, int offset = STARTGAMEMODE) const { return (calcmodemask() & (1 << (mode-offset))) != 0; }

        int findmode(int mode) const
        {
            if(!hasmode(mode)) loopi(NUMGAMEMODES) if(hasmode(i, 0)) return i+STARTGAMEMODE;
            return mode;
        }

        bool match(int reqmode, const char *reqmap) const
        {
            return hasmode(reqmode) && (!map[0] || !reqmap[0] || !strcmp(map, reqmap));
        }

        bool includes(const maprotation &rot) const
        {
            return rot.modes == modes ? rot.map[0] && !map[0] : (rot.modes & modes) == rot.modes;
        }
    };
    int maprotation::exclude = 0;
    vector<maprotation> maprotations;
    int curmaprotation = 0;

    VAR(lockmaprotation, 0, 0, 2);

    void maprotationreset()
    {
        maprotations.setsize(0);
        curmaprotation = 0;
        maprotation::exclude = 0;
    }

    void nextmaprotation()
    {
        curmaprotation++;
        if(maprotations.inrange(curmaprotation) && maprotations[curmaprotation].modes) return;
        do curmaprotation--;
        while(maprotations.inrange(curmaprotation) && maprotations[curmaprotation].modes);
        curmaprotation++;
    }

    int findmaprotation(int mode, const char *map)
    {
        for(int i = max(curmaprotation, 0); i < maprotations.length(); i++)
        {
            maprotation &rot = maprotations[i];
            if(!rot.modes) break;
            if(rot.match(mode, map)) return i;
        }
        int start;
        for(start = max(curmaprotation, 0) - 1; start >= 0; start--) if(!maprotations[start].modes) break;
        start++;
        for(int i = start; i < curmaprotation; i++)
        {
            maprotation &rot = maprotations[i];
            if(!rot.modes) break;
            if(rot.match(mode, map)) return i;
        }
        int best = -1;
        loopv(maprotations)
        {
            maprotation &rot = maprotations[i];
            if(rot.match(mode, map) && (best < 0 || maprotations[best].includes(rot))) best = i;
        }
        return best;
    }

    bool searchmodename(const char *haystack, const char *needle)
    {
        if(!needle[0]) return true;
        do
        {
            if(needle[0] != '.')
            {
                haystack = strchr(haystack, needle[0]);
                if(!haystack) break;
                haystack++;
            }
            const char *h = haystack, *n = needle+1;
            for(; *h && *n; h++)
            {
                if(*h == *n) n++;
                else if(*h != ' ') break;
            }
            if(!*n) return true;
            if(*n == '.') return !*h;
        } while(needle[0] != '.');
        return false;
    }

    int genmodemask(vector<char *> &modes)
    {
        int modemask = 0;
        loopv(modes)
        {
            const char *mode = modes[i];
            int op = mode[0];
            switch(mode[0])
            {
                case '*':
                    modemask |= 1<<NUMGAMEMODES;
                    loopk(NUMGAMEMODES) if(m_checknot(k+STARTGAMEMODE, M_DEMO|M_EDIT|M_LOCAL)) modemask |= 1<<k;
                    continue;
                case '!':
                    mode++;
                    if(mode[0] != '?') break;
                case '?':
                    mode++;
                    loopk(NUMGAMEMODES) if(searchmodename(gamemodes[k].name, mode))
                    {
                        if(op == '!') modemask &= ~(1<<k);
                        else modemask |= 1<<k;
                    }
                    continue;
            }
            int modenum = INT_MAX;
            if(isdigit(mode[0])) modenum = atoi(mode);
            else loopk(NUMGAMEMODES) if(searchmodename(gamemodes[k].name, mode)) { modenum = k+STARTGAMEMODE; break; }
            if(!m_valid(modenum)) continue;
            switch(op)
            {
                case '!': modemask &= ~(1 << (modenum - STARTGAMEMODE)); break;
                default: modemask |= 1 << (modenum - STARTGAMEMODE); break;
            }
        }
        return modemask;
    }

    bool addmaprotation(int modemask, const char *map)
    {
        if(!map[0]) loopk(NUMGAMEMODES) if(modemask&(1<<k) && !m_check(k+STARTGAMEMODE, M_EDIT)) modemask &= ~(1<<k);
        if(!modemask) return false;
        if(!(modemask&(1<<NUMGAMEMODES))) maprotation::exclude |= modemask;
        maprotation &rot = maprotations.add();
        rot.modes = modemask;
        copystring(rot.map, map);
        return true;
    }

    void addmaprotations(tagval *args, int numargs)
    {
        vector<char *> modes, maps;
        for(int i = 0; i + 1 < numargs; i += 2)
        {
            explodelist(args[i].getstr(), modes);
            explodelist(args[i+1].getstr(), maps);
            int modemask = genmodemask(modes);
            if(maps.length()) loopvj(maps) addmaprotation(modemask, maps[j]);
            else addmaprotation(modemask, "");
            modes.deletearrays();
            maps.deletearrays();
        }
        if(maprotations.length() && maprotations.last().modes)
        {
            maprotation &rot = maprotations.add();
            rot.modes = 0;
            rot.map[0] = '\0';
        }
    }

    COMMAND(maprotationreset, "");
    COMMANDN(maprotation, addmaprotations, "ss2V");

    struct demofile
    {
        string info;
        uchar *data;
        int len;
    };

    vector<demofile> demos;

    VAR(recorddemo, 0, 0, 1);
    int demonextmatch = 0;
    stream *demotmp = NULL, *demorecord = NULL, *demoplayback = NULL;
    int nextplayback = 0, demomillis = 0;

    VAR(maxdemos, 0, 5, 25);
    VAR(maxdemosize, 0, 16, 64);
    VAR(restrictdemos, 0, 1, 1);

    VAR(restrictpausegame, 0, 1, 1);
    VAR(restrictgamespeed, 0, 1, 1);

    //zeromod variables
    VAR(clearbots, 0, 1, 1);                        //decides if server clears bots upon changing map
    VAR(servergamespeed, 10, 100, 1000);            //default gamespeed
    VAR(servergamelimit, 1, 10, 1440);              //max is 24 hours
    VAR(serverovertime, 0, 0, 1);                   //decides if server use 15 minutes instead of 10 in some modes
    VAR(servercheckgbans, 0, 1, 2);                 //decides if server checks gbans (0=no;1=yes;2=checkifnoadminexists)
    VAR(serverhideip, 0, 0, 1);                     //protects users privacy; disables showing ip in cube server listener
    VAR(beststats, 0, 1, 1);                        //show best stats
    VAR(serverintermission, 1, 10, 3600);           //intermission interval (in seconds)
    VAR(serversuggestnp, 0, 1, 1);                  //decides if server suggest players to say #np
    SVAR(commandchars, "#");                        //defines characters which are interepted as command starting characters
    VARF(defaultgamemode, 0, 0, NUMGAMEMODES+STARTGAMEMODE, { if(!m_mp(defaultgamemode)) defaultgamemode = 0; });
    int _nodamage = 0;
    VARF(disabledamage, 0, 0, 2,
    {
        _enablefunc("nodamage", disabledamage>0);
        if(disabledamage <= 0)
        {
            if(_nodamage > 0) sendservmsg("nodamage \f4disabled");
            _nodamage = 0;
        }
    });
    VAR(defaultmastermode, MM_AUTH, MM_OPEN, MM_PASSWORD);
    VAR(serverhidepriv, 0, 0, 2);   //0 - no, 1 - >=admin, 2 - >=master
    VARF(votekick, 0, 0, 1, { _enablefunc("votekick", votekick); });
    SVAR(serveradmin, "");
    VAR(persistteams, 0, 0, 2);
    int persist = 0;
    int autosendmap = 0;
    VAR(protectteamscores, 0, 0, 1);
    SVAR(serverdesc, "");
    SVAR(serverpass, "");
    SVAR(adminpass, "");
    SVAR(masterpass, "");
    VARF(publicserver, 0, 0, 2, {
        switch(publicserver)
        {
            case 0: default: mastermask = MM_PRIVSERV; break;
            case 1: mastermask = MM_PUBSERV; break;
            case 2: mastermask = MM_COOPSERV; break;
        }
    });
    SVAR(servermotd, "");

    VAR(autolockmaster, 0, 0, MAXCLIENTS);

    struct teamkillkick
    {
        int modes, limit, ban;

        bool match(int mode) const
        {
            return (modes&(1<<(mode-STARTGAMEMODE)))!=0;
        }

        bool includes(const teamkillkick &tk) const
        {
            return tk.modes != modes && (tk.modes & modes) == tk.modes;
        }
    };
    vector<teamkillkick> teamkillkicks;

    void teamkillkickreset()
    {
        teamkillkicks.setsize(0);
    }

    void addteamkillkick(char *modestr, int *limit, int *ban)
    {
        vector<char *> modes;
        explodelist(modestr, modes);
        teamkillkick &kick = teamkillkicks.add();
        kick.modes = genmodemask(modes);
        kick.limit = *limit;
        kick.ban = *ban > 0 ? *ban*60000 : (*ban < 0 ? 0 : 30*60000);
        modes.deletearrays();
    }

    COMMAND(teamkillkickreset, "");
    COMMANDN(teamkillkick, addteamkillkick, "sii");

    struct teamkillmessage
    {
        char *message;
        int modes;

        bool match(int mode) const
        {
            return (modes&(1<<(mode-STARTGAMEMODE)))!=0;
        }
    };
    vector<teamkillmessage> teamkillmessages;

    void addteamkillmessage(char *modestr, char *message)
    {
        vector<char *> modes;
        if(!modestr || !modestr[0] || !message || !message[0]) return;
        explodelist(modestr, modes);
        teamkillmessage &tkm = teamkillmessages.add();
        tkm.message = newstring(message);
        tkm.modes = genmodemask(modes);
        modes.deletearrays();
    }
    ICOMMAND(teamkillmessage, "ss", (char *a, char *b), addteamkillmessage(a, b));

    void teamkillmessagereset()
    {
        loopv(teamkillmessages) DELETEA(teamkillmessages[i].message);
        teamkillmessages.shrink(0);
    }
    COMMAND(teamkillmessagereset, "");

    void sendteamkillmessage(clientinfo *ci)
    {
        if(teamkillmessages.empty() || !ci || ci->state.aitype != AI_NONE) return;
        if(!teamkillmessages.inrange(ci->_xi.teamkillmessageindex)) ci->_xi.teamkillmessageindex = 0;
        bool found = false;
        for(int t = 0; t < teamkillmessages.length(); t++)
        {
            if(teamkillmessages[(ci->_xi.teamkillmessageindex + t) % teamkillmessages.length()].match(gamemode))
            {
                found = true;
                ci->_xi.teamkillmessageindex = (ci->_xi.teamkillmessageindex + t) % teamkillmessages.length();
                break;
            }
        }
        if(found) sendf(ci->clientnum, 1, "ris", N_SERVMSG, teamkillmessages[ci->_xi.teamkillmessageindex++]);
    }

    struct teamkillinfo
    {
        uint ip;
        int teamkills;
    };
    vector<teamkillinfo> teamkills;
    bool shouldcheckteamkills = false;

    void addteamkill(clientinfo *actor, clientinfo *victim, int n)
    {
        if(!m_timed || actor->state.aitype != AI_NONE || actor->privilege || (victim && victim->state.aitype != AI_NONE)) return;
        shouldcheckteamkills = true;
        uint ip = getclientip(actor->clientnum);
        loopv(teamkills) if(teamkills[i].ip == ip)
        {
            teamkills[i].teamkills += n;
            return;
        }
        if(n > 0)
        {
            teamkillinfo &tk = teamkills.add();
            tk.ip = ip;
            tk.teamkills = n;
        }
    }

    void checkteamkills()
    {
        teamkillkick *kick = NULL;
        if(m_timed) loopv(teamkillkicks) if(teamkillkicks[i].match(gamemode) && (!kick || kick->includes(teamkillkicks[i])))
            kick = &teamkillkicks[i];
        if(kick) loopvrev(teamkills)
        {
            teamkillinfo &tk = teamkills[i];
            if(tk.teamkills >= kick->limit)
            {
                if(kick->ban > 0) addban(tk.ip, kick->ban);
                kickclients(tk.ip);
                teamkills.removeunordered(i);
                continue;
            }
            if(tk.teamkills <= 0) teamkills.removeunordered(i);
        }
        shouldcheckteamkills = false;
    }

    void *newclientinfo() { return new clientinfo; }
    void deleteclientinfo(void *ci) { delete (clientinfo *)ci; }

    clientinfo *getinfo(int n)
    {
        if(n < MAXCLIENTS) return (clientinfo *)getclientinfo(n);
        n -= MAXCLIENTS;
        return bots.inrange(n) ? bots[n] : NULL;
    }

    uint mcrc = 0;
    vector<entity> ments;
    vector<server_entity> sents;
    vector<savedscore> scores;

    int msgsizelookup(int msg)
    {
        static int sizetable[NUMMSG] = { -1 };
        if(sizetable[0] < 0)
        {
            memset(sizetable, -1, sizeof(sizetable));
            for(const int *p = msgsizes; *p >= 0; p += 2) sizetable[p[0]] = p[1];
        }
        return msg >= 0 && msg < NUMMSG ? sizetable[msg] : -1;
    }

    const char *modename(int n, const char *unknown)
    {
        if(m_valid(n)) return gamemodes[n - STARTGAMEMODE].name;
        return unknown;
    }

    const char *mastermodename(int n, const char *unknown)
    {
        return (n>=MM_START && size_t(n-MM_START)<sizeof(mastermodenames)/sizeof(mastermodenames[0])) ? mastermodenames[n-MM_START] : unknown;
    }

    const char *privname(int type)
    {
        switch(type)
        {
            case PRIV_ADMIN: return "admin";
            case PRIV_AUTH: return "auth";
            case PRIV_MASTER: return "master";
            case PRIV_ROOT: return "root";
            case PRIV_NONE: return "none";
            default: return "unknown";
        }
    }

    void sendservmsg(const char *s) { sendf(-1, 1, "ris", N_SERVMSG, s); }
    void sendservmsgf(const char *fmt, ...)
    {
         defvformatstring(s, fmt, fmt);
         sendf(-1, 1, "ris", N_SERVMSG, s);
    }

    void resetitems()
    {
        mcrc = 0;
        ments.setsize(0);
        sents.setsize(0);
    }

    bool serveroption(const char *arg)
    {
        if(arg[0]=='-') switch(arg[1])
        {
            case 'n': setsvar("serverdesc", &arg[2]); return true;
            case 'y': setsvar("serverpass", &arg[2]); return true;
            case 'p': setsvar("adminpass", &arg[2]); return true;
            case 'o': setvar("publicserver", atoi(&arg[2])); return true;
        }
        return false;
    }

    void serverinit()
    {
        smapname[0] = '\0';
        gamemode = defaultgamemode;
        mastermode = defaultmastermode;
        persist = persistteams;
        if(recorddemo) demonextmatch = 2;
        resetitems();
        _initfuncs();
        _initman();
        _enablefunc("nodamage", disabledamage>0);
        _enablefunc("votekick", votekick);
        if(serverflagruns) execfile("flagruns.cfg", false);
    }

    void _storeflagruns()
    {
        if(serverflagruns)
        {
            stream *f = openutf8file(path("flagruns.cfg", true), "w");
            if(f)
            {
                f->printf("// Automatically generated by zeromod at exit; lists flagruns\n\n");
                loopv(_flagruns)
                    f->printf("flagrun %i \"%s\" %i \"%s\"\n",
                              _flagruns[i].gamemode, _flagruns[i].map, _flagruns[i].timeused, _flagruns[i].name);
                delete f;
            }
        }
    }

    void serverclose()
    {
        //store flagruns
        _storeflagruns();
    }

    int numclients(int exclude = -1, bool nospec = true, bool noai = true, bool priv = false)
    {
        int n = 0;
        loopv(clients)
        {
            clientinfo *ci = clients[i];
            if(ci->_xi.spy) continue;
            if(ci->clientnum!=exclude && (!nospec || ci->state.state!=CS_SPECTATOR || (priv && ci->privilege)) && (!noai || ci->state.aitype == AI_NONE)) n++;
        }
        return n;
    }

    bool duplicatename(clientinfo *ci, char *name)
    {
        if(!name) name = ci->name;
        loopv(clients) if(clients[i]!=ci && !clients[i]->_xi.spy && !strcmp(name, clients[i]->name)) return true;
        return false;
    }

    const char *colorname(clientinfo *ci, char *name)
    {
        if(!name) name = ci->name;
        if(name[0] && !duplicatename(ci, name) && ci->state.aitype == AI_NONE) return name;
        static string cname[3];
        static int cidx = 0;
        cidx = (cidx+1)%3;
        formatstring(cname[cidx])(ci->state.aitype == AI_NONE ? "%s \fs\f5(%d)\fr" : "%s \fs\f5[%d]\fr", name, ci->clientnum);
        return cname[cidx];
    }

    struct servmode
    {
        virtual ~servmode() {}

        virtual void entergame(clientinfo *ci) {}
        virtual void leavegame(clientinfo *ci, bool disconnecting = false) {}

        virtual void moved(clientinfo *ci, const vec &oldpos, bool oldclip, const vec &newpos, bool newclip) {}
        virtual bool canspawn(clientinfo *ci, bool connecting = false) { return true; }
        virtual void spawned(clientinfo *ci) {}
        virtual int fragvalue(clientinfo *victim, clientinfo *actor)
        {
            if(victim==actor || isteam(victim->team, actor->team)) return -1;
            return 1;
        }
        virtual void died(clientinfo *victim, clientinfo *actor) {}
        virtual bool canchangeteam(clientinfo *ci, const char *oldteam, const char *newteam) { return true; }
        virtual void changeteam(clientinfo *ci, const char *oldteam, const char *newteam) {}
        virtual void initclient(clientinfo *ci, packetbuf &p, bool connecting) {}
        virtual void update() {}
        virtual void cleanup() {}
        virtual void setup() {}
        virtual void newmap() {}
        virtual void intermission() {}
        virtual bool hidefrags() { return false; }
        virtual int getteamscore(const char *team) { return 0; }
        virtual void getteamscores(vector<teamscore> &scores) {}
        virtual bool extinfoteam(const char *team, ucharbuf &p) { return false; }
    };

    #define SERVMODE 1
    #include "capture.h"
    #include "ctf.h"
    #include "collect.h"

    captureservmode capturemode;
    ctfservmode ctfmode;
    collectservmode collectmode;
    servmode *smode = NULL;

    bool canspawnitem(int type) { return !m_noitems && (type>=I_SHELLS && type<=I_QUAD && (!m_noammo || type<I_SHELLS || type>I_CARTRIDGES)); }

    int spawntime(int type)
    {
        if(m_classicsp) return INT_MAX;
        int np = numclients(-1, true, false);
        np = np<3 ? 4 : (np>4 ? 2 : 3);         // spawn times are dependent on number of players
        int sec = 0;
        switch(type)
        {
            case I_SHELLS:
            case I_BULLETS:
            case I_ROCKETS:
            case I_ROUNDS:
            case I_GRENADES:
            case I_CARTRIDGES: sec = np*4; break;
            case I_HEALTH: sec = np*5; break;
            case I_GREENARMOUR: sec = 20; break;
            case I_YELLOWARMOUR: sec = 30; break;
            case I_BOOST: sec = 60; break;
            case I_QUAD: sec = 70; break;
        }
        return sec*1000;
    }

    bool delayspawn(int type)
    {
        switch(type)
        {
            case I_GREENARMOUR:
            case I_YELLOWARMOUR:
                return !m_classicsp;
            case I_BOOST:
            case I_QUAD:
                return true;
            default:
                return false;
        }
    }

    bool pickup(int i, int sender)         // server side item pickup, acknowledge first client that gets it
    {
        clientinfo *ci = getinfo(sender);
        if(!ci || (m_timed && gamemillis >= gamelimit)) return false;
        if(!sents.inrange(i))
        {
            //if(m_edit) return false;
            //ci->mapcrc = -1;
            //checkmaps();
            return false;
        }
        //if((m_timed && gamemillis>=gamelimit) || !sents.inrange(i) || !sents[i].spawned) return false;
        if(!ci->state.canpickup(sents[i].type)) return false;
        sents[i].spawned = false;
        sents[i].spawntime = spawntime(sents[i].type);
        sendf(-1, 1, "ri3", N_ITEMACC, i, sender);
        ci->state.pickup(sents[i].type);
        return true;
    }

    static hashset<teaminfo> teaminfos;

    void clearteaminfo()
    {
        teaminfos.clear();
    }

    bool teamhasplayers(const char *team) { loopv(clients) if(!strcmp(clients[i]->team, team)) return true; return false; }

    bool pruneteaminfo()
    {
        int oldteams = teaminfos.numelems;
        enumerates(teaminfos, teaminfo, old,
            if(!old.frags && !teamhasplayers(old.team)) teaminfos.remove(old.team);
        );
        return teaminfos.numelems < oldteams;
    }

    teaminfo *addteaminfo(const char *team)
    {
        teaminfo *t = teaminfos.access(team);
        if(!t)
        {
            if(teaminfos.numelems >= MAXTEAMS && !pruneteaminfo()) return NULL;
            t = &teaminfos[team];
            copystring(t->team, team, sizeof(t->team));
            t->frags = 0;
        }
        return t;
    }

    clientinfo *choosebestclient(float &bestrank)
    {
        clientinfo *best = NULL;
        bestrank = -1;
        loopv(clients)
        {
            clientinfo *ci = clients[i];
            if(ci->state.timeplayed<0) continue;
            float rank = ci->state.state!=CS_SPECTATOR ? ci->state.effectiveness/max(ci->state.timeplayed, 1) : -1;
            if(!best || rank > bestrank) { best = ci; bestrank = rank; }
        }
        return best;
    }

    void autoteam()
    {
        static const char * const teamnames[2] = {"good", "evil"};
        vector<clientinfo *> team[2];
        float teamrank[2] = {0, 0};
        for(int round = 0, remaining = clients.length(); remaining>=0; round++)
        {
            int first = round&1, second = (round+1)&1, selected = 0;
            while(teamrank[first] <= teamrank[second])
            {
                float rank;
                clientinfo *ci = choosebestclient(rank);
                if(!ci) break;
                if(smode && smode->hidefrags()) rank = 1;
                else if(selected && rank<=0) break;
                ci->state.timeplayed = -1;
                team[first].add(ci);
                if(rank>0) teamrank[first] += rank;
                selected++;
                if(rank<=0) break;
            }
            if(!selected) break;
            remaining -= selected;
        }
        loopi(sizeof(team)/sizeof(team[0]))
        {
            addteaminfo(teamnames[i]);
            loopvj(team[i])
            {
                clientinfo *ci = team[i][j];
                if(!strcmp(ci->team, teamnames[i])) continue;
                copystring(ci->team, teamnames[i], MAXTEAMLEN+1);
                sendf(!ci->_xi.spy ? -1 : ci->ownernum, 1, "riisi", N_SETTEAM, ci->clientnum, teamnames[i], -1);
            }
        }
    }

    void persistautoteam()
    {
        static char const * const teamnames[2] = {"good", "evil"};  // standard team names
        vector<clientinfo *> team[3];   // 0=good, 1=evil, 2=other nonstandard teams
        float teamrank[2] = {0, 0};     // standard team scores
        int remaining;

        // first of all, send all nonstandard teams players to thrid team group
        remaining = clients.length();
        loopv(clients)
        {
            clientinfo &ci = *clients[i];
            if(!ci.team[0]) continue;
            bool standard = false;
            loopj(sizeof(teamnames)/sizeof(teamnames[0])) if(!strcmp(ci.team, teamnames[j]))
            {
                standard = true;
                break;
            }
            if(standard) continue;

            ci.state.timeplayed = -1;
            team[2].add(&ci);
            remaining--;
        }
        // now autoteam standard teams players
        for(int round = 0; remaining>=0; round++)
        {
            int first = round&1, second = (round+1)&1, selected = 0;
            while(teamrank[first] <= teamrank[second])
            {
                float rank;
                clientinfo *ci = choosebestclient(rank);
                if(!ci) break;
                if(smode && smode->hidefrags()) rank = 1;
                else if(selected && rank<=0) break;
                ci->state.timeplayed = -1;
                team[first].add(ci);
                if(rank>0) teamrank[first] += rank;
                selected++;
                if(rank<=0) break;
            }
            if(!selected) break;
            remaining -= selected;
        }
        loopi(sizeof(team)/sizeof(team[0]))
        {
            if(uint(i) < sizeof(teamnames)/sizeof(teamnames[0]))
            {
                addteaminfo(teamnames[i]);
                loopvj(team[i])
                {
                    clientinfo *ci = team[i][j];
                    if(!strcmp(ci->team, teamnames[i])) continue;
                    copystring(ci->team, teamnames[i], MAXTEAMLEN+1);
                    sendf(!ci->_xi.spy ? -1 : ci->ownernum, 1, "riisi", N_SETTEAM, ci->clientnum, teamnames[i], -1);
                }
            }
            else
            {
                loopvj(team[i]) addteaminfo(team[i][j]->team);
            }
        }
    }

    void persistctfautoteam()
    {
        static char const * const teamnames[2] = {"good", "evil"};
        string goodteam;
        goodteam[0] = '\0';
        loopv(clients)
        {
            clientinfo &ci = *clients[i];
            if(strcmp(ci.team, teamnames[0]) && strcmp(ci.team, teamnames[1]))
            {
                if(!goodteam[0] || !strcmp(ci.team, goodteam))
                {
                    if(!goodteam[0]) copystring(goodteam, ci.team);
                    copystring(ci.team, teamnames[0], MAXTEAMLEN+1);
                }
                else copystring(ci.team, teamnames[1], MAXTEAMLEN+1);

                sendf(!ci._xi.spy ? -1 : ci.ownernum, 1, "riisi", N_SETTEAM, ci.clientnum, ci.team, -1);
            }
        }
        loopi(2) addteaminfo(teamnames[i]);
    }

    struct teamrank
    {
        const char *name;
        float rank;
        int clients;

        teamrank(const char *name) : name(name), rank(0), clients(0) {}
    };

    const char *chooseworstteam(const char *suggest = NULL, clientinfo *exclude = NULL)
    {
        teamrank teamranks[2] = { teamrank("good"), teamrank("evil") };
        const int numteams = sizeof(teamranks)/sizeof(teamranks[0]);
        loopv(clients)
        {
            clientinfo *ci = clients[i];
            if(ci==exclude || ci->state.aitype!=AI_NONE || ci->state.state==CS_SPECTATOR || !ci->team[0]) continue;
            ci->state.timeplayed += lastmillis - ci->state.lasttimeplayed;
            ci->state.lasttimeplayed = lastmillis;

            loopj(numteams) if(!strcmp(ci->team, teamranks[j].name))
            {
                teamrank &ts = teamranks[j];
                ts.rank += ci->state.effectiveness/max(ci->state.timeplayed, 1);
                ts.clients++;
                break;
            }
        }
        teamrank *worst = &teamranks[numteams-1];
        loopi(numteams-1)
        {
            teamrank &ts = teamranks[i];
            if(smode && smode->hidefrags())
            {
                if(ts.clients < worst->clients || (ts.clients == worst->clients && ts.rank < worst->rank)) worst = &ts;
            }
            else if(ts.rank < worst->rank || (ts.rank == worst->rank && ts.clients < worst->clients)) worst = &ts;
        }
        return worst->name;
    }

    void prunedemos(int extra = 0)
    {
        int n = clamp(demos.length() + extra - maxdemos, 0, demos.length());
        if(n <= 0) return;
        loopi(n) delete[] demos[i].data;
        demos.remove(0, n);
    }

    void adddemo()
    {
        if(!demotmp) return;
        int len = (int)min(demotmp->size(), stream::offset((maxdemosize<<20) + 0x10000));
        demofile &d = demos.add();
        time_t t = time(NULL);
        char *timestr = ctime(&t), *trim = timestr + strlen(timestr);
        while(trim>timestr && iscubespace(*--trim)) *trim = '\0';
        formatstring(d.info)("%s: %s, %s, %.2f%s", timestr, modename(gamemode), smapname, len > 1024*1024 ? len/(1024*1024.f) : len/1024.0f, len > 1024*1024 ? "MB" : "kB");
        sendservmsgf("demo \"%s\" recorded", d.info);
        d.data = new uchar[len];
        d.len = len;
        demotmp->seek(0, SEEK_SET);
        demotmp->read(d.data, len);
        DELETEP(demotmp);
    }

    void enddemorecord()
    {
        if(!demorecord) return;

        DELETEP(demorecord);

        if(!demotmp) return;
        if(!maxdemos || !maxdemosize) { DELETEP(demotmp); return; }

        prunedemos(1);
        adddemo();
    }

    void writedemo(int chan, void *data, int len)
    {
        if(!demorecord) return;
        int stamp[3] = { gamemillis, chan, len };
        lilswap(stamp, 3);
        demorecord->write(stamp, sizeof(stamp));
        demorecord->write(data, len);
        if(demorecord->rawtell() >= (maxdemosize<<20)) enddemorecord();
    }

    void recordpacket(int chan, void *data, int len)
    {
        writedemo(chan, data, len);
    }

    int welcomepacket(packetbuf &p, clientinfo *ci);
    void sendwelcome(clientinfo *ci);

    void setupdemorecord()
    {
        if(!m_mp(gamemode) || m_edit) return;

        demotmp = opentempfile("demorecord", "w+b");
        if(!demotmp) return;

        stream *f = opengzfile(NULL, "wb", demotmp);
        if(!f) { DELETEP(demotmp); return; }

        if(demonextmatch < 2) sendservmsg("recording demo");

        demorecord = f;

        demoheader hdr;
        memcpy(hdr.magic, DEMO_MAGIC, sizeof(hdr.magic));
        hdr.version = DEMO_VERSION;
        hdr.protocol = PROTOCOL_VERSION;
        lilswap(&hdr.version, 2);
        demorecord->write(&hdr, sizeof(demoheader));

        packetbuf p(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
        welcomepacket(p, NULL);
        writedemo(1, p.buf, p.len);
    }

    void listdemos(int cn)
    {
        packetbuf p(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
        putint(p, N_SENDDEMOLIST);
        putint(p, demos.length());
        loopv(demos) sendstring(demos[i].info, p);
        sendpacket(cn, 1, p.finalize());
    }

    void cleardemos(int n)
    {
        if(!n)
        {
            loopv(demos) delete[] demos[i].data;
            demos.shrink(0);
            sendservmsg("cleared all demos");
        }
        else if(demos.inrange(n-1))
        {
            delete[] demos[n-1].data;
            demos.remove(n-1);
            sendservmsgf("cleared demo %d", n);
        }
    }

    static void freegetmap(ENetPacket *packet)
    {
        loopv(clients)
        {
            clientinfo *ci = clients[i];
            if(ci->getmap == packet) ci->getmap = NULL;
        }
    }

    static void freegetdemo(ENetPacket *packet)
    {
        loopv(clients)
        {
            clientinfo *ci = clients[i];
            if(ci->getdemo == packet) ci->getdemo = NULL;
        }
    }

    void senddemo(clientinfo *ci, int num)
    {
        if(ci->getdemo) return;
        if(!num) num = demos.length();
        if(!demos.inrange(num-1)) return;
        demofile &d = demos[num-1];
        if((ci->getdemo = sendf(ci->clientnum, 2, "rim", N_SENDDEMO, d.len, d.data)))
            ci->getdemo->freeCallback = freegetdemo;
    }

    void enddemoplayback()
    {
        if(!demoplayback) return;
        DELETEP(demoplayback);

        loopv(clients) sendf(clients[i]->clientnum, 1, "ri3", N_DEMOPLAYBACK, 0, clients[i]->clientnum);

        sendservmsg("demo playback finished");

        loopv(clients) sendwelcome(clients[i]);
    }

    void setupdemoplayback()
    {
        if(demoplayback) return;
        demoheader hdr;
        string msg;
        msg[0] = '\0';
        defformatstring(file)("%s.dmo", smapname);
        demoplayback = opengzfile(file, "rb");
        if(!demoplayback) formatstring(msg)("could not read demo \"%s\"", file);
        else if(demoplayback->read(&hdr, sizeof(demoheader))!=sizeof(demoheader) || memcmp(hdr.magic, DEMO_MAGIC, sizeof(hdr.magic)))
            formatstring(msg)("\"%s\" is not a demo file", file);
        else
        {
            lilswap(&hdr.version, 2);
            if(hdr.version!=DEMO_VERSION) formatstring(msg)("demo \"%s\" requires an %s version of Cube 2: Sauerbraten", file, hdr.version<DEMO_VERSION ? "older" : "newer");
            else if(hdr.protocol!=PROTOCOL_VERSION) formatstring(msg)("demo \"%s\" requires an %s version of Cube 2: Sauerbraten", file, hdr.protocol<PROTOCOL_VERSION ? "older" : "newer");
        }
        if(msg[0])
        {
            DELETEP(demoplayback);
            sendservmsg(msg);
            return;
        }

        sendservmsgf("playing demo \"%s\"", file);

        demomillis = 0;
        sendf(-1, 1, "ri3", N_DEMOPLAYBACK, 1, -1);

        if(demoplayback->read(&nextplayback, sizeof(nextplayback))!=sizeof(nextplayback))
        {
            enddemoplayback();
            return;
        }
        lilswap(&nextplayback, 1);
    }

    void readdemo()
    {
        if(!demoplayback) return;
        demomillis += curtime;
        while(demomillis>=nextplayback)
        {
            int chan, len;
            if(demoplayback->read(&chan, sizeof(chan))!=sizeof(chan) ||
               demoplayback->read(&len, sizeof(len))!=sizeof(len))
            {
                enddemoplayback();
                return;
            }
            lilswap(&chan, 1);
            lilswap(&len, 1);
            ENetPacket *packet = enet_packet_create(NULL, len+1, 0);
            if(!packet || demoplayback->read(packet->data+1, len)!=len)
            {
                if(packet) enet_packet_destroy(packet);
                enddemoplayback();
                return;
            }
            packet->data[0] = N_DEMOPACKET;
            sendpacket(-1, chan, packet);
            if(!packet->referenceCount) enet_packet_destroy(packet);
            if(!demoplayback) break;
            if(demoplayback->read(&nextplayback, sizeof(nextplayback))!=sizeof(nextplayback))
            {
                enddemoplayback();
                return;
            }
            lilswap(&nextplayback, 1);
        }
    }

    void stopdemo()
    {
        if(m_demo) enddemoplayback();
        else enddemorecord();
    }

    void pausegame(bool val, clientinfo *ci = NULL)
    {
        if(gamepaused==val) return;
        gamepaused = val;
        sendf(-1, 1, "riii", N_PAUSEGAME, gamepaused ? 1 : 0, ci ? ci->clientnum : -1);
    }

    void checkpausegame()
    {
        if(!gamepaused) return;
        int admins = 0;
        loopv(clients) if(clients[i]->privilege >= (restrictpausegame ? PRIV_ADMIN : PRIV_MASTER)) admins++;
        if(!admins) pausegame(false);
    }

    void forcepaused(bool paused)
    {
        pausegame(paused);
    }

    bool ispaused() { return gamepaused; }

    void changegamespeed(int val, clientinfo *ci = NULL)
    {
        val = clamp(val, 10, 1000);
        if(val!=100 && m_ctf) loopv(clients) clients[i]->_xi.lasttakeflag = 0;
        if(gamespeed==val) return;
        gamespeed = val;
        sendf(-1, 1, "riii", N_GAMESPEED, gamespeed, ci ? ci->clientnum : -1);
    }

    void forcegamespeed(int speed)
    {
        changegamespeed(speed);
    }

    int scaletime(int t) { return t*gamespeed; }

    SVAR(serverauth, "");

    struct userkey
    {
        char *name;
        char *desc;

        userkey() : name(NULL), desc(NULL) {}
        userkey(char *name, char *desc) : name(name), desc(desc) {}
    };

    static inline uint hthash(const userkey &k) { return ::hthash(k.name); }
    static inline bool htcmp(const userkey &x, const userkey &y) { return !strcmp(x.name, y.name) && !strcmp(x.desc, y.desc); }

    struct userinfo : userkey
    {
        void *pubkey;
        int privilege;

        userinfo() : pubkey(NULL), privilege(PRIV_NONE) {}
        ~userinfo() { delete[] name; delete[] desc; if(pubkey) freepubkey(pubkey); }
    };
    hashset<userinfo> users;

    void adduser(char *name, char *desc, char *pubkey, char *priv)
    {
        userkey key(name, desc);
        userinfo &u = users[key];
        if(u.pubkey) { freepubkey(u.pubkey); u.pubkey = NULL; }
        if(!u.name) u.name = newstring(name);
        if(!u.desc) u.desc = newstring(desc);
        u.pubkey = parsepubkey(pubkey);
        switch(priv[0])
        {
            case 'r': case 'R': u.privilege = PRIV_ROOT; break;
            case 'a': case 'A': u.privilege = PRIV_ADMIN; break;
            case 'c': case 'C': u.privilege = PRIV_MASTER; break;
            // case 'n': case 'N': u.privilege = PRIV_NONE; break; // uncomment then nameprotection is done
            case 'm': case 'M': default: u.privilege = PRIV_AUTH; break;
        }
    }
    COMMAND(adduser, "ssss");

    void clearusers()
    {
        users.clear();
    }
    COMMAND(clearusers, "");

    void hashpassword(int cn, int sessionid, const char *pwd, char *result, int maxlen)
    {
        char buf[2*sizeof(string)];
        formatstring(buf)("%d %d ", cn, sessionid);
        copystring(&buf[strlen(buf)], pwd);
        if(!hashstring(buf, result, maxlen)) *result = '\0';
    }

    bool checkpassword(clientinfo *ci, const char *wanted, const char *given)
    {
        string hash;
        hashpassword(ci->clientnum, ci->sessionid, wanted, hash, sizeof(hash));
        return !strcmp(hash, given);
    }

    void revokemaster(clientinfo *ci)
    {
        ci->privilege = PRIV_NONE;
        if(ci->state.state==CS_SPECTATOR) aiman::removeai(ci);
    }

    extern bool connected(clientinfo *ci);

    void _putmaster(packetbuf &p)
    {
        putint(p, N_CURRENTMASTER);
        putint(p, mastermode);
        loopv(clients) if(clients[i]->privilege >= PRIV_MASTER &&
            !clients[i]->_xi.spy &&
            !(serverhidepriv > 0 && clients[i]->privilege >= (serverhidepriv == 1 ? PRIV_ADMIN : PRIV_MASTER) &&
                !(clients[i]->privilege == PRIV_AUTH && clients[i]->authname[0] && !clients[i]->authdesc[0])))
        {
            putint(p, clients[i]->clientnum);
            putint(p, clamp(clients[i]->privilege, int(PRIV_NONE), int(PRIV_ADMIN)));
        }
        putint(p, -1);
    }

    VAR(maxpassfail, 0, 16, 4096);  //maximum password attempts

    bool setmaster(clientinfo *ci, bool val, const char *pass = "", const char *authname = NULL, const char *authdesc = NULL, int authpriv = PRIV_MASTER, bool force = false, bool trial = false)
    {
        if(authname && !val) return false;
        const char *name = "";
        int oldpriv = ci->privilege;

        /* if authname exists and authdesc does not, this is gauth: do not hide privilege */
        bool washidden = (serverhidepriv > 0 &&
                            ci->privilege >= (serverhidepriv == 1 ? PRIV_ADMIN : PRIV_MASTER) &&
                            !(ci->privilege == PRIV_AUTH && ci->authname[0] && !ci->authdesc[0])) ||
                            ci->_xi.spy;

        if(val)
        {
            /* Skip checking of passwords if password is already correct */
            bool hasadminpass = (!maxpassfail || ci->_xi.failpass < maxpassfail) &&
                    adminpass[0] && checkpassword(ci, adminpass, pass);
            bool hasmasterpass = (!maxpassfail || ci->_xi.failpass < maxpassfail) &&
                    !hasadminpass && masterpass[0] && checkpassword(ci, masterpass, pass);
            /* If incorrect password was given, register this as failed attempt */
            if(pass && pass[0] && maxpassfail && !hasadminpass && !hasmasterpass)
                ci->_xi.failpass = max(ci->_xi.failpass + 1, ci->_xi.failpass);

            int wantpriv = (maxpassfail && ci->_xi.failpass >= maxpassfail)
                ? authpriv
                : hasadminpass
                    ? PRIV_ADMIN
                    : hasmasterpass
                        ? PRIV_MASTER
                        : authpriv;

            if(ci->privilege)
            {
                if(wantpriv <= ci->privilege) return true;
            }
            else if(wantpriv <= PRIV_MASTER && !force && !hasmasterpass)
            {
                if(ci->state.state==CS_SPECTATOR)
                {
                    sendf(ci->clientnum, 1, "ris", N_SERVMSG, "Spectators may not claim master.");
                    return false;
                }
                loopv(clients) if(ci!=clients[i] && clients[i]->privilege)
                {
                    sendf(ci->clientnum, 1, "ris", N_SERVMSG, "Master is already claimed.");
                    return false;
                }
                if(!authname && !(mastermask&MM_AUTOAPPROVE) && !ci->privilege)
                {
                    sendf(ci->clientnum, 1, "ris", N_SERVMSG, "This server requires you to use the \"\f0/auth\f7\" command to claim master.");
                    return false;
                }
            }
            if(trial) return true;
            ci->privilege = wantpriv;
            name = privname(ci->privilege);
        }
        else
        {
            if(!ci->privilege) return false;
            if(trial) return true;
            name = privname(ci->privilege);
            revokemaster(ci);
        }
        bool hasmaster = false;
        loopv(clients) if(clients[i]->privilege >= PRIV_MASTER) { hasmaster = true; break; }
        if(!hasmaster)
        {
            mastermode = defaultmastermode;
            if(mastermode < MM_PRIVATE) allowedips.shrink(0);
        }

        bool ishidden = ci->_xi.spy ||
                        (serverhidepriv > 0 &&
                        ci->privilege >= (serverhidepriv == 1 ? PRIV_ADMIN : PRIV_MASTER) &&
                        !(ci->privilege == PRIV_AUTH && ci->authname[0] && !ci->authdesc[0]));

        string msg;
        if(val && authname)
        {
            if(authdesc && authdesc[0]) formatstring(msg)("%s claimed %s as '\fs\f5%s\fr' [\fs\f0%s\fr]%s",
                colorname(ci), name, authname, authdesc, !(ishidden || (oldpriv && washidden)) ? "" : " \f1(hidden)");
            else formatstring(msg)("%s claimed %s as '\fs\f5%s\fr'%s",
                colorname(ci), name, authname, !(ishidden || (oldpriv && washidden)) ? "" : " \f1(hidden)");
        }
        else formatstring(msg)("%s %s %s%s", colorname(ci), val ? "claimed" : "relinquished", name,
            !(ishidden || (oldpriv && washidden)) ? "" : " \f1(hidden)");

        logoutf("%s", msg);

        if((ci->privilege && !ishidden && ((oldpriv && washidden) || (!oldpriv && !washidden))) || (oldpriv && !washidden))
        {
            packetbuf p(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
            putint(p, N_SERVMSG);

            if(washidden)
            {
                string tmp;
                formatstring(tmp)("%s %s %s", colorname(ci), "claimed", privname(ci->privilege));
                sendstring(tmp, p);
            }
            else if(ishidden)
            {
                defformatstring(tmp)("%s %s %s", colorname(ci), "relinquished", privname(oldpriv));
                sendstring(tmp, p);
            }
            else sendstring(msg, p);

            _putmaster(p);
            sendpacket(-1, 1, p.finalize(), !(ishidden || (oldpriv && washidden)) ? -1 : ci->ownernum);
        }

        if(ishidden || (oldpriv && washidden))
        {
            packetbuf q(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
            putint(q, N_SERVMSG);
            sendstring(msg, q);
            _putmaster(q);
            sendpacket(ci->ownernum, 1, q.finalize());
        }

        checkpausegame();
        return true;
    }

    bool trykick(clientinfo *ci, int victim, const char *reason = NULL, const char *authname = NULL, const char *authdesc = NULL, int authpriv = PRIV_NONE, bool trial = false)
    {
        int priv = ci->privilege;
        if(authname)
        {
            if(priv >= authpriv) authname = authdesc = NULL;
            else priv = authpriv;
        }
        if(priv && ci->clientnum!=victim)
        {
            clientinfo *vinfo = (clientinfo *)getclientinfo(victim);
            if(vinfo && (priv >= vinfo->privilege) && vinfo->privilege < PRIV_ADMIN)
            {
                if(trial) return true;
                string kicker, msg;
                if(authname)
                {
                    if(authdesc && authdesc[0]) formatstring(kicker)("%s as '\fs\f5%s\fr' [\fs\f0%s\fr]", colorname(ci), authname, authdesc);
                    else formatstring(kicker)("%s as '\fs\f5%s\fr'", colorname(ci), authname);
                }
                else copystring(kicker, colorname(ci));

                if(!ci->_xi.spy)
                {
                    if(reason && reason[0]) formatstring(msg)("%s kicked %s because: %s", kicker, colorname(vinfo), reason);
                    else formatstring(msg)("%s kicked %s", kicker, colorname(vinfo));
                }
                else
                {
                    if(reason && reason[0]) formatstring(msg)("%s was kicked because: %s", colorname(vinfo), reason);
                    else formatstring(msg)("%s was kicked", colorname(vinfo));
                }
                sendf(-1, 1, "ris", N_SERVMSG, msg);
                logoutf("%s", msg);

                uint ip = getclientip(victim);
                addban(ip, 4*60*60000);
                kickclients(ip, ci, priv);
            }
        }
        return false;
    }

    savedscore *findscore(clientinfo *ci, bool insert)
    {
        uint ip = getclientip(ci->clientnum);
        if(!ip) return 0;
        if(!insert)
        {
            loopv(clients)
            {
                clientinfo *oi = clients[i];
                if(oi->clientnum != ci->clientnum && getclientip(oi->clientnum) == ip && !strcmp(oi->name, ci->name))
                {
                    oi->state.timeplayed += lastmillis - oi->state.lasttimeplayed;
                    oi->state.lasttimeplayed = lastmillis;
                    static savedscore curscore;
                    curscore.save(oi->state);
                    return &curscore;
                }
            }
        }
        loopv(scores)
        {
            savedscore &sc = scores[i];
            if(sc.ip == ip && !strcmp(sc.name, ci->name)) return &sc;
        }
        if(!insert) return 0;
        savedscore &sc = scores.add();
        sc.ip = ip;
        copystring(sc.name, ci->name);
        return &sc;
    }

    void savescore(clientinfo *ci)
    {
        savedscore *sc = findscore(ci, true);
        if(sc) sc->save(ci->state);
    }

    static struct msgfilter
    {
        uchar msgmask[NUMMSG];

        msgfilter(int msg, ...)
        {
            memset(msgmask, 0, sizeof(msgmask));
            va_list msgs;
            va_start(msgs, msg);
            for(uchar val = 1; msg < NUMMSG; msg = va_arg(msgs, int))
            {
                if(msg < 0) val = uchar(-msg);
                else msgmask[msg] = val;
            }
            va_end(msgs);
        }

        uchar operator[](int msg) const { return msg >= 0 && msg < NUMMSG ? msgmask[msg] : 0; }
    } msgfilter(-1, N_CONNECT, N_SERVINFO, N_INITCLIENT, N_WELCOME, N_MAPCHANGE,
                N_SERVMSG, N_DAMAGE, N_HITPUSH, N_SHOTFX, N_EXPLODEFX, N_DIED,
                N_SPAWNSTATE, N_FORCEDEATH, N_TEAMINFO, N_ITEMACC, N_ITEMSPAWN,
                N_TIMEUP, N_CDIS, N_CURRENTMASTER, N_PONG, N_RESUME, N_BASESCORE,
                N_BASEINFO, N_BASEREGEN, N_ANNOUNCE, N_SENDDEMOLIST, N_SENDDEMO,
                N_DEMOPLAYBACK, N_SENDMAP, N_DROPFLAG, N_SCOREFLAG, N_RETURNFLAG,
                N_RESETFLAG, N_INVISFLAG, N_CLIENT, N_AUTHCHAL, N_INITAI,
                N_EXPIRETOKENS, N_DROPTOKENS, N_STEALTOKENS, N_DEMOPACKET,
                -2, N_REMIP, N_NEWMAP, N_GETMAP, N_SENDMAP, N_CLIPBOARD,
                -3, N_EDITENT, N_EDITF, N_EDITT, N_EDITM, N_FLIP, N_COPY, N_PASTE,
                N_ROTATE, N_REPLACE, N_DELCUBE, N_EDITVAR, -4, N_POS, NUMMSG),
      connectfilter(-1, N_CONNECT, -2, N_AUTHANS, -3, N_PING, NUMMSG);

    int checktype(int type, clientinfo *ci)
    {
        if(ci)
        {
            if(!ci->connected) switch(connectfilter[type])
            {
                // allow only before authconnect
                case 1: return !ci->connectauth ? type : -1;
                // allow only during authconnect
                case 2: return ci->connectauth ? type : -1;
                // always allow
                case 3: return type;
                // never allow
                default: return -1;
            }
            if(ci->local) return type;
        }
        switch(msgfilter[type])
        {
            // server-only messages
            case 1: return ci ? -1 : type;
            // only allowed in coop-edit
            case 2: if(m_edit) break; return -1;
            // only allowed in coop-edit, no overflow check
            case 3: return m_edit ? type : -1;
            // no overflow check
            case 4: return type;
        }
        if(ci && ++ci->overflow >= 200) return -2;
        return type;
    }

    struct worldstate
    {
        int uses, len;
        uchar *data;

        worldstate() : uses(0), len(0), data(NULL) {}

        void setup(int n) { len = n; data = new uchar[n]; }
        void cleanup() { DELETEA(data); len = 0; }
        bool contains(const uchar *p) const { return p >= data && p < &data[len]; }
    };
    vector<worldstate> worldstates;
    bool reliablemessages = false;

    void cleanworldstate(ENetPacket *packet)
    {
        loopv(worldstates)
        {
            worldstate &ws = worldstates[i];
            if(!ws.contains(packet->data)) continue;
            ws.uses--;
            if(ws.uses <= 0)
            {
                ws.cleanup();
                worldstates.removeunordered(i);
            }
            break;
        }
    }

    void flushclientposition(clientinfo &ci)
    {
        if(ci.position.empty() || (!hasnonlocalclients() && !demorecord)) return;
        packetbuf p(ci.position.length(), 0);
        p.put(ci.position.getbuf(), ci.position.length());
        ci.position.setsize(0);
        sendpacket(-1, 0, p.finalize(), ci.ownernum);
    }

    static void sendpositions(worldstate &ws, ucharbuf &wsbuf)
    {
        if(wsbuf.empty()) return;
        int wslen = wsbuf.length();
        recordpacket(0, wsbuf.buf, wslen);
        wsbuf.put(wsbuf.buf, wslen);
        loopv(clients)
        {
            clientinfo &ci = *clients[i];
            if(ci.state.aitype != AI_NONE) continue;
            uchar *data = wsbuf.buf;
            int size = wslen;
            if(ci.wsdata >= wsbuf.buf) { data = ci.wsdata + ci.wslen; size -= ci.wslen; }
            if(size <= 0) continue;
            ENetPacket *packet = enet_packet_create(data, size, ENET_PACKET_FLAG_NO_ALLOCATE);
            sendpacket(ci.clientnum, 0, packet);
            if(packet->referenceCount) { ws.uses++; packet->freeCallback = cleanworldstate; }
            else enet_packet_destroy(packet);
        }
        wsbuf.offset(wsbuf.length());
    }

    static inline void addposition(worldstate &ws, ucharbuf &wsbuf, int mtu, clientinfo &bi, clientinfo &ci)
    {
        if(bi.position.empty()) return;
        if(wsbuf.length() + bi.position.length() > mtu) sendpositions(ws, wsbuf);
        int offset = wsbuf.length();
        wsbuf.put(bi.position.getbuf(), bi.position.length());
        bi.position.setsize(0);
        int len = wsbuf.length() - offset;
        if(ci.wsdata < wsbuf.buf) { ci.wsdata = &wsbuf.buf[offset]; ci.wslen = len; }
        else ci.wslen += len;
    }

    static void sendmessages(worldstate &ws, ucharbuf &wsbuf)
    {
        if(wsbuf.empty()) return;
        int wslen = wsbuf.length();
        recordpacket(1, wsbuf.buf, wslen);
        wsbuf.put(wsbuf.buf, wslen);
        loopv(clients)
        {
            clientinfo &ci = *clients[i];
            if(ci.state.aitype != AI_NONE) continue;
            uchar *data = wsbuf.buf;
            int size = wslen;
            if(ci.wsdata >= wsbuf.buf) { data = ci.wsdata + ci.wslen; size -= ci.wslen; }
            if(size <= 0) continue;
            ENetPacket *packet = enet_packet_create(data, size, (reliablemessages ? ENET_PACKET_FLAG_RELIABLE : 0) | ENET_PACKET_FLAG_NO_ALLOCATE);
            sendpacket(ci.clientnum, 1, packet);
            if(packet->referenceCount) { ws.uses++; packet->freeCallback = cleanworldstate; }
            else enet_packet_destroy(packet);
        }
        wsbuf.offset(wsbuf.length());
    }

    static inline void addmessages(worldstate &ws, ucharbuf &wsbuf, int mtu, clientinfo &bi, clientinfo &ci)
    {
        if(bi.messages.empty()) return;
        if(wsbuf.length() + 10 + bi.messages.length() > mtu) sendmessages(ws, wsbuf);
        int offset = wsbuf.length();
        putint(wsbuf, N_CLIENT);
        putint(wsbuf, bi.clientnum);
        putuint(wsbuf, bi.messages.length());
        wsbuf.put(bi.messages.getbuf(), bi.messages.length());
        bi.messages.setsize(0);
        int len = wsbuf.length() - offset;
        if(ci.wsdata < wsbuf.buf) { ci.wsdata = &wsbuf.buf[offset]; ci.wslen = len; }
        else ci.wslen += len;
    }

    bool buildworldstate()
    {
        int wsmax = 0;
        loopv(clients)
        {
            clientinfo &ci = *clients[i];
            ci.overflow = 0;
            ci.wsdata = NULL;
            wsmax += ci.position.length();
            if(ci.messages.length()) wsmax += 10 + ci.messages.length();
        }
        if(wsmax <= 0)
        {
            reliablemessages = false;
            return false;
        }
        worldstate &ws = worldstates.add();
        ws.setup(2*wsmax);
        int mtu = getservermtu() - 100;
        if(mtu <= 0) mtu = ws.len;
        ucharbuf wsbuf(ws.data, ws.len);
        loopv(clients)
        {
            clientinfo &ci = *clients[i];
            if(ci.state.aitype != AI_NONE) continue;
            addposition(ws, wsbuf, mtu, ci, ci);
            loopvj(ci.bots) addposition(ws, wsbuf, mtu, *ci.bots[j], ci);
        }
        sendpositions(ws, wsbuf);
        loopv(clients)
        {
            clientinfo &ci = *clients[i];
            if(ci.state.aitype != AI_NONE) continue;
            addmessages(ws, wsbuf, mtu, ci, ci);
            loopvj(ci.bots) addmessages(ws, wsbuf, mtu, *ci.bots[j], ci);
        }
        sendmessages(ws, wsbuf);
        reliablemessages = false;
        if(ws.uses) return true;
        ws.cleanup();
        worldstates.drop();
        return false;
    }

    bool sendpackets(bool force)
    {
        if(clients.empty() || (!hasnonlocalclients() && !demorecord)) return false;
        enet_uint32 curtime = enet_time_get()-lastsend;
        if(curtime<33 && !force) return false;
        bool flush = buildworldstate();
        lastsend += curtime - (curtime%33);
        return flush;
    }

    template<class T>
    void sendstate(gamestate &gs, T &p)
    {
        putint(p, gs.lifesequence);
        putint(p, gs.health);
        putint(p, gs.maxhealth);
        putint(p, gs.armour);
        putint(p, gs.armourtype);
        putint(p, gs.gunselect);
        loopi(GUN_PISTOL-GUN_SG+1) putint(p, gs.ammo[GUN_SG+i]);
    }

    void spawnstate(clientinfo *ci)
    {
        gamestate &gs = ci->state;
        gs.spawnstate(gamemode);
        gs.lifesequence = (gs.lifesequence + 1)&0x7F;
    }

    void sendspawn(clientinfo *ci)
    {
        gamestate &gs = ci->state;
        spawnstate(ci);
        sendf(ci->ownernum, 1, "rii7v", N_SPAWNSTATE, ci->clientnum, gs.lifesequence,
            gs.health, gs.maxhealth,
            gs.armour, gs.armourtype,
            gs.gunselect, GUN_PISTOL-GUN_SG+1, &gs.ammo[GUN_SG]);
        gs.lastspawn = gamemillis;
    }

    void sendwelcome(clientinfo *ci)
    {
        packetbuf p(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
        int chan = welcomepacket(p, ci);
        sendpacket(ci->clientnum, chan, p.finalize());
    }

    void putinitclient(clientinfo *ci, packetbuf &p)
    {
        if(ci->state.aitype != AI_NONE)
        {
            putint(p, N_INITAI);
            putint(p, ci->clientnum);
            putint(p, ci->ownernum);
            putint(p, ci->state.aitype);
            putint(p, ci->state.skill);
            putint(p, ci->playermodel);
            sendstring(ci->name, p);
            sendstring(ci->team, p);
        }
        else
        {
            putint(p, N_INITCLIENT);
            putint(p, ci->clientnum);
            sendstring(ci->name, p);
            sendstring(ci->team, p);
            putint(p, ci->playermodel);
        }
    }

    void welcomeinitclient(packetbuf &p, int exclude = -1)
    {
        loopv(clients)
        {
            clientinfo *ci = clients[i];
            if(!ci->connected || ci->clientnum == exclude || ci->_xi.spy) continue;

            putinitclient(ci, p);
        }
    }

    bool hasmap(clientinfo *ci)
    {
        return (m_edit && (clients.length() > 0)) ||
               (smapname[0] && (!m_timed || gamemillis < gamelimit || (ci->state.state==CS_SPECTATOR && !ci->privilege) || numclients(ci->clientnum, true, true, true)));
    }

    int welcomepacket(packetbuf &p, clientinfo *ci)
    {
        putint(p, N_WELCOME);
        putint(p, N_MAPCHANGE);
        sendstring(smapname, p);
        putint(p, gamemode);
        putint(p, notgotitems ? 1 : 0);
        if(!ci || (m_timed && smapname[0]))
        {
            putint(p, N_TIMEUP);
            putint(p, gamemillis < gamelimit && !interm ? max((gamelimit - gamemillis)/1000, 1) : 0);
        }
        if(!notgotitems)
        {
            putint(p, N_ITEMLIST);
            loopv(sents) if(sents[i].spawned)
            {
                putint(p, i);
                putint(p, sents[i].type);
            }
            putint(p, -1);
        }
        bool hasmaster = false;
        if(mastermode != MM_OPEN)
        {
            putint(p, N_CURRENTMASTER);
            putint(p, mastermode);
            hasmaster = true;
        }
        loopv(clients) if(clients[i]->privilege >= PRIV_MASTER &&
            !clients[i]->_xi.spy &&
            !(serverhidepriv > 0 && clients[i]->privilege >= (serverhidepriv == 1 ? PRIV_ADMIN : PRIV_MASTER) &&
                !(clients[i]->privilege == PRIV_AUTH && clients[i]->authname[0] && !clients[i]->authdesc[0])))
        {
            if(!hasmaster)
            {
                putint(p, N_CURRENTMASTER);
                putint(p, mastermode);
                hasmaster = true;
            }
            putint(p, clients[i]->clientnum);
            putint(p, clamp(clients[i]->privilege, int(PRIV_NONE), int(PRIV_ADMIN)));
        }
        if(hasmaster) putint(p, -1);
        if(gamepaused)
        {
            putint(p, N_PAUSEGAME);
            putint(p, 1);
            putint(p, -1);
        }
        if(gamespeed != 100)
        {
            putint(p, N_GAMESPEED);
            putint(p, gamespeed);
            putint(p, -1);
        }
        if(m_teammode)
        {
            putint(p, N_TEAMINFO);
            enumerates(teaminfos, teaminfo, t,
                if(t.frags) { sendstring(t.team, p); putint(p, t.frags); }
            );
            sendstring("", p);
        }
        if(ci)
        {
            putint(p, N_SETTEAM);
            putint(p, ci->clientnum);
            sendstring(ci->team, p);
            putint(p, -1);
        }
        if(ci && ci->state.state!=CS_SPECTATOR)
        {
            if(smode && !smode->canspawn(ci, true))
            {
                ci->state.state = CS_DEAD;
                putint(p, N_FORCEDEATH);
                putint(p, ci->clientnum);
                sendf(-1, 1, "ri2x", N_FORCEDEATH, ci->clientnum, ci->clientnum);
            }
            else
            {
                gamestate &gs = ci->state;
                spawnstate(ci);
                putint(p, N_SPAWNSTATE);
                putint(p, ci->clientnum);
                sendstate(gs, p);
                gs.lastspawn = gamemillis;
            }
        }
        if(ci && ci->state.state==CS_SPECTATOR)
        {
            putint(p, N_SPECTATOR);
            putint(p, ci->clientnum);
            putint(p, 1);
            sendf(-1, 1, "ri3x", N_SPECTATOR, ci->clientnum, 1, ci->clientnum);
        }
        if(!ci || clients.length()>1)
        {
            putint(p, N_RESUME);
            loopv(clients)
            {
                clientinfo *oi = clients[i];
                if(ci && oi->clientnum==ci->clientnum) continue;
                if(oi->_xi.spy) continue;
                putint(p, oi->clientnum);
                putint(p, oi->state.state);
                putint(p, oi->state.frags);
                putint(p, oi->state.flags);
                putint(p, oi->state.quadmillis);
                sendstate(oi->state, p);
            }
            putint(p, -1);
            welcomeinitclient(p, ci ? ci->clientnum : -1);
        }
        if(smode) smode->initclient(ci, p, true);
        return 1;
    }

    bool restorescore(clientinfo *ci)
    {
        //if(ci->local) return false;
        savedscore *sc = findscore(ci, false);
        if(sc)
        {
            sc->restore(ci->state);
            return true;
        }
        return false;
    }

    void sendresume(clientinfo *ci)
    {
        gamestate &gs = ci->state;
        sendf(-1, 1, "ri3i9vi", N_RESUME, ci->clientnum,
            gs.state, gs.frags, gs.flags, gs.quadmillis,
            gs.lifesequence,
            gs.health, gs.maxhealth,
            gs.armour, gs.armourtype,
            gs.gunselect, GUN_PISTOL-GUN_SG+1, &gs.ammo[GUN_SG], -1);
    }

    void sendinitclient(clientinfo *ci)
    {
        packetbuf p(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
        putinitclient(ci, p);
        sendpacket(-1, 1, p.finalize(), ci->clientnum);
    }

    void loaditems()
    {
        resetitems();
        notgotitems = true;
        if(m_edit || !loadents(smapname, ments, &mcrc))
            return;
        loopv(ments) if(canspawnitem(ments[i].type))
        {
            server_entity se = { NOTUSED, 0, false };
            while(sents.length()<=i) sents.add(se);
            sents[i].type = ments[i].type;
            if(delayspawn(sents[i].type)) sents[i].spawntime = spawntime(sents[i].type);
            else sents[i].spawned = true;
        }
        notgotitems = false;
    }

    void changemap(const char *s, int mode)
    {
        stopdemo();
        pausegame(false);
        changegamespeed(servergamespeed);
        if(smode) smode->cleanup();
        if(clearbots) aiman::clearai();

        gamemode = mode;
        gamemillis = 0;
        gamelimit = ((serverovertime && m_overtime) ? servergamelimit*3/2 : servergamelimit) * 60000;
        interm = 0;
        nextexceeded = 0;
        copystring(smapname, s);
        loaditems();
        scores.setsize(0);
        shouldcheckteamkills = false;
        teamkills.setsize(0);
        loopv(clients)
        {
            clientinfo *ci = clients[i];
            ci->state.timeplayed += lastmillis - ci->state.lasttimeplayed;
        }

        sendf(-1, 1, "risii", N_MAPCHANGE, smapname, gamemode, 1);

        clearteaminfo();
        if(m_teammode) switch(persist)
        {
            case 2:
                if(m_ctf) autoteam();
                else persistautoteam();
                break;

            case 1:
                if(m_ctf) persistctfautoteam();
                else loopv(clients) if(clients[i]->team[0]) addteaminfo(clients[i]->team);
                break;

            default:
                autoteam();
                break;
        }

        if(m_capture) smode = &capturemode;
        else if(m_ctf) smode = &ctfmode;
        else if(m_collect) smode = &collectmode;
        else smode = NULL;

        if(m_timed && smapname[0]) sendf(-1, 1, "ri2", N_TIMEUP, gamemillis < gamelimit && !interm ? max((gamelimit - gamemillis)/1000, 1) : 0);
        loopv(clients)
        {
            clientinfo *ci = clients[i];
            ci->mapchange();
            ci->state.lasttimeplayed = lastmillis;
            if(ci->state.state!=CS_SPECTATOR && !ci->_xi.spy) sendspawn(ci);
        }

        if(!clearbots) loopv(bots) if(bots[i] && bots[i]->aireinit < 1) bots[i]->aireinit = 1;
        aiman::changemap();

        if(m_demo)
        {
            if(clients.length()) setupdemoplayback();
        }
        else if(demonextmatch || recorddemo)
        {
            if(demonextmatch) setupdemorecord();
            demonextmatch = recorddemo ? 2 : 0;
        }

        if(smode) smode->setup();
    }

    void rotatemap(bool next)
    {
        if(!maprotations.inrange(curmaprotation))
        {
            curmaprotation = findmaprotation(gamemode, smapname);
            if(curmaprotation < 0) { if(smapname[0]) curmaprotation = findmaprotation(gamemode, ""); }
            else if(next) nextmaprotation();
            if(maprotations.inrange(curmaprotation) && maprotations[curmaprotation].modes)
            {
                maprotation &rot = maprotations[curmaprotation];
                changemap(rot.map, rot.findmode(gamemode));
            }
            else changemap(smapname, gamemode);
            return;
        }
        curmaprotation = findmaprotation(gamemode, smapname);
        if(next)
        {
            if(curmaprotation >= 0) nextmaprotation();
            else curmaprotation = smapname[0] ? max(findmaprotation(gamemode, ""), 0) : 0;
        }
        else if(curmaprotation < 0) curmaprotation = smapname[0] ? max(findmaprotation(gamemode, ""), 0) : 0;
        maprotation &rot = maprotations[curmaprotation];
        changemap(rot.map, rot.findmode(gamemode));
    }

    struct votecount
    {
        char *map;
        int mode, count;
        votecount() {}
        votecount(char *s, int n) : map(s), mode(n), count(0) {}
    };

    void checkvotes(bool force = false)
    {
        vector<votecount> votes;
        int maxvotes = 0;
        loopv(clients)
        {
            clientinfo *oi = clients[i];
            if(oi->state.state==CS_SPECTATOR && !oi->privilege) continue;
            if(oi->state.aitype!=AI_NONE) continue;
            maxvotes++;
            if(!m_valid(oi->modevote)) continue;
            votecount *vc = NULL;
            loopvj(votes) if(!strcmp(oi->mapvote, votes[j].map) && oi->modevote==votes[j].mode)
            {
                vc = &votes[j];
                break;
            }
            if(!vc) vc = &votes.add(votecount(oi->mapvote, oi->modevote));
            vc->count++;
        }
        votecount *best = NULL;
        loopv(votes) if(!best || votes[i].count > best->count || (votes[i].count == best->count && rnd(2))) best = &votes[i];
        if(force || (best && best->count > maxvotes/2))
        {
            if(demorecord) enddemorecord();
            if(best && (best->count > (force ? 1 : maxvotes/2)))
            {
                sendservmsg(force ? "vote passed by default" : "vote passed by majority");
                changemap(best->map, best->mode);
            }
            else rotatemap(true);
        }
    }

#if 0
    void forcemap(const char *map, int mode)
    {
        stopdemo();
        if(!map[0] && !m_check(mode, M_EDIT))
        {
            int idx = findmaprotation(mode, smapname);
            if(idx < 0 && smapname[0]) idx = findmaprotation(mode, "");
            if(idx < 0) return;
            map = maprotations[idx].map;
        }
        if(hasnonlocalclients()) sendservmsgf("local player forced %s on map %s", modename(mode), map[0] ? map : "[new map]");
        changemap(map, mode);
    }
#endif

    void vote(const char *map, int reqmode, int sender)
    {
        clientinfo *ci = getinfo(sender);
        if(!ci || (ci->state.state==CS_SPECTATOR && !ci->privilege) || !m_mp(reqmode)) return;
        if(!m_valid(reqmode)) return;
        if(!map[0] && !m_check(reqmode, M_EDIT))
        {
            int idx = findmaprotation(reqmode, smapname);
            if(idx < 0 && smapname[0]) idx = findmaprotation(reqmode, "");
            if(idx < 0) return;
            map = maprotations[idx].map;
        }
        if(lockmaprotation && ci->privilege < (lockmaprotation > 1 ? PRIV_ADMIN : PRIV_MASTER) && findmaprotation(reqmode, map) < 0)
        {
            sendf(sender, 1, "ris", N_SERVMSG, "This server has locked the map rotation.");
            return;
        }
        bool changed = false;
        if(strcmp(ci->mapvote, map)) { copystring(ci->mapvote, map); changed = true; }
        if(ci->modevote != reqmode) { ci->modevote = reqmode; changed = true; }
        if(ci->privilege && mastermode>=MM_VETO)
        {
            if(demorecord) enddemorecord();
            sendservmsgf("%s forced %s on map %s", colorname(ci), modename(ci->modevote), ci->mapvote[0] ? ci->mapvote : "[new map]");
            changemap(ci->mapvote, ci->modevote);
        }
        else
        {
            if(changed)
                sendservmsgf("%s suggests %s on map %s (select map to vote)", colorname(ci), modename(reqmode), map[0] ? map : "[new map]");
            checkvotes();
        }
    }

#define _BESTSTAT(stat) \
    { \
        best.setsize(0); \
        best.add(clients[0]); \
        besti = best[0]->state.stat; \
        for(int i = 1; i < clients.length(); i++) if(!clients[i]->_xi.spy) \
        { \
            if(clients[i]->state.stat > besti) \
            { \
                best.setsize(0); \
                best.add(clients[i]); \
                besti = clients[i]->state.stat; \
            } \
            else if(clients[i]->state.stat == besti) \
            { \
                best.add(clients[i]); \
            } \
        } \
    }

    void _printbest(vector<clientinfo *> &best, int besti, char *msg)
    {
        int l = min(best.length(), 3);
        loopi(l)
        {
            concatstring(msg, colorname(best[i]), MAXTRANS);
            if(i + 1 < l) concatstring(msg, ", ", MAXTRANS);
        }
        defformatstring(buf)(" \f1(\f0%i\f1)", besti);
        concatstring(msg, buf, MAXTRANS);
    }

    void printbeststats()
    {
        vector<clientinfo *> best;
        int besti;
        char msg[MAXTRANS];

        static char const * const bestkills = "\f0Best kills:\f1";
        static char const * const worstkills = "\f0Worst kills:\f1";

        // Best kills
        msg[0] = '\0';
        // frags
        _BESTSTAT(frags);
        if(besti)
        {
            copystring(msg, bestkills, MAXTRANS);
            concatstring(msg, " frags: \f7", MAXTRANS);
            _printbest(best, besti, msg);
        }
        // damage dealt
        _BESTSTAT(damage);
        if(besti > 0)
        {
            if(!msg[0]) copystring(msg, bestkills, MAXTRANS);
            concatstring(msg, " damage dealt: \f7", MAXTRANS);
            _printbest(best, besti, msg);
        }
        // kpd
        best.setsize(0);
        best.add(clients[0]);
        {
            float bestf = float(best[0]->state.frags) / float(max(best[0]->state.deaths, 1));
            for(int i = 1; i < clients.length(); i++)
            {
                float currf = float(clients[i]->state.frags) / float(max(clients[i]->state.deaths, 1));
                if(currf > bestf)
                {
                    best.setsize(0);
                    best.add(clients[i]);
                    bestf = currf;
                }
                else if(currf == bestf)
                {
                    best.add(clients[i]);
                }
            }

            if(bestf >= 0.01f || bestf <= -0.01f)   // non 0
            {
                if(!msg[0]) copystring(msg, bestkills, MAXTRANS);
                concatstring(msg, " kpd: \f7", MAXTRANS);
                int l = min(best.length(), 3);
                loopi(l)
                {
                    concatstring(msg, colorname(best[i]), MAXTRANS);
                    if(i + 1 < l) concatstring(msg, ", ", MAXTRANS);
                }
                defformatstring(buf)(" \f1(\f0%.2f\f1)", bestf);
                concatstring(msg, buf, MAXTRANS);
            }
        }
        // accuracy
        best.setsize(0);
        best.add(clients[0]);
        besti = best[0]->state.damage * 100 / max(best[0]->state.shotdamage, 1);
        for(int i = 1; i < clients.length(); i++) if(!clients[i]->_xi.spy)
        {
            int curri = clients[i]->state.damage * 100 / max(clients[i]->state.shotdamage, 1);
            if(curri > besti)
            {
                best.setsize(0);
                best.add(clients[i]);
                besti = curri;
            }
            else if(curri == besti)
            {
                best.add(clients[i]);
            }
        }
        if(besti)
        {
            if(!msg[0]) copystring(msg, bestkills, MAXTRANS);
            concatstring(msg, " accuracy: \f7", MAXTRANS);
            int l = min(best.length(), 3);
            loopi(l)
            {
                concatstring(msg, colorname(best[i]), MAXTRANS);
                if(i + 1 < l) concatstring(msg, ", ", MAXTRANS);
            }
            defformatstring(buf)(" \f1(\f0%i%%\f1)", besti);
            concatstring(msg, buf, MAXTRANS);
        }
        // print out
        if(msg[0]) sendservmsg(msg);

        // Worst kills
        msg[0] = '\0';
        // deaths
        _BESTSTAT(deaths);
        if(besti)
        {
            copystring(msg, worstkills, MAXTRANS);
            concatstring(msg, " deaths: \f7", MAXTRANS);
            _printbest(best, besti, msg);
        }
        // suicides
        _BESTSTAT(_suicides);
        if(besti)
        {
            if(!msg[0]) copystring(msg, worstkills, MAXTRANS);
            concatstring(msg, " suicides: \f7", MAXTRANS);
            _printbest(best, besti, msg);
        }
        // teamkills
        if(m_teammode)
        {
            _BESTSTAT(teamkills);
            if(besti)
            {
                if(!msg[0]) copystring(msg, worstkills, MAXTRANS);
                concatstring(msg, " teamkills: \f7", MAXTRANS);
                _printbest(best, besti, msg);
            }
        }
        // damage wasted
        best.setsize(0);
        best.add(clients[0]);
        besti = best[0]->state.shotdamage-best[0]->state.damage;
        for(int i = 1; i < clients.length(); i++) if(!clients[i]->_xi.spy)
        {
            int curri = clients[i]->state.shotdamage-clients[i]->state.damage;
            if(curri > besti)
            {
                best.setsize(0);
                best.add(clients[i]);
                besti = curri;
            }
            else if(curri == besti)
            {
                best.add(clients[i]);
            }
        }
        if(besti > 0)
        {
            if(!msg[0]) copystring(msg, worstkills, MAXTRANS);
            concatstring(msg, " damage wasted: \f7", MAXTRANS);
            _printbest(best, besti, msg);
        }
        // print out
        if(msg[0]) sendservmsg(msg);

        // Print statuses for ctf modes
        if(m_ctf)
        {
            static char const * const bestflags = "\f0Best flags:\f1";
            _BESTSTAT(flags);
            if(besti)
            {
                copystring(msg, bestflags, MAXTRANS);
                concatstring(msg, " scored: \f7", MAXTRANS);
                _printbest(best, besti, msg);
            }
            else msg[0] = 0;

            if(m_hold)
            {
                _BESTSTAT(_stolen);
                if(besti)
                {
                    if(!msg[0]) copystring(msg, bestflags, MAXTRANS);
                    concatstring(msg, " taken: \f7", MAXTRANS);
                    _printbest(best, besti, msg);
                }
            }
            else if(!m_protect)
            {
                _BESTSTAT(_stolen);
                if(besti)
                {
                    if(!msg[0]) copystring(msg, bestflags, MAXTRANS);
                    concatstring(msg, " stolen: \f7", MAXTRANS);
                    _printbest(best, besti, msg);
                }

                _BESTSTAT(_returned);
                if(besti)
                {
                    if(!msg[0]) copystring(msg, bestflags, MAXTRANS);
                    concatstring(msg, " returned: \f7", MAXTRANS);
                    _printbest(best, besti, msg);
                }
            }
            if(msg[0]) sendservmsg(msg);
        }
        else if(m_collect)
        {
            static char const * const bestskulls = "\f0Best skulls:\f1";
            _BESTSTAT(flags);
            if(besti)
            {
                copystring(msg, bestskulls, MAXTRANS);
                concatstring(msg, " scored: \f7", MAXTRANS);
                _printbest(best, besti, msg);
            }
            else msg[0] = 0;
            _BESTSTAT(_stolen);
            if(besti)
            {
                if(!msg[0]) copystring(msg, bestskulls, MAXTRANS);
                concatstring(msg, " stolen: \f7", MAXTRANS);
                _printbest(best, besti, msg);
            }
            _BESTSTAT(_returned);
            if(besti)
            {
                if(!msg[0]) copystring(msg, bestskulls, MAXTRANS);
                concatstring(msg, " returned: \f7", MAXTRANS);
                _printbest(best, besti, msg);
            }
            if(msg[0]) sendservmsg(msg);
        }
    }

    void checkintermission()
    {
        if(gamemillis >= gamelimit && !interm)
        {
            sendf(-1, 1, "ri2", N_TIMEUP, 0);
            if(smode) smode->intermission();
            changegamespeed(100);
            interm = gamemillis + serverintermission*1000;
            if(beststats && clients.length() > 0) printbeststats();
        }
    }

    void startintermission() { gamelimit = min(gamelimit, gamemillis); checkintermission(); }

    VAR(allowmultikill, 0, 0, 1);

    void dodamage(clientinfo *target, clientinfo *actor, int damage, int gun, const vec &hitpush = vec(0, 0, 0))
    {
        gamestate &ts = target->state;
        //zeromod
        if(!m_edit || !_nodamage || (_nodamage == 1 ? 0 : (target->_xi.editmute && actor->_xi.editmute))) ts.dodamage(damage);
        if(target!=actor && !isteam(target->team, actor->team)) actor->state.damage += damage;
        sendf(-1, 1, "ri6", N_DAMAGE, target->clientnum, actor->clientnum, damage, ts.armour, ts.health);
        if(target==actor) target->setpushed();
        else if(!hitpush.iszero())
        {
            ivec v = vec(hitpush).rescale(DNF);
            sendf(ts.health<=0 ? -1 : target->ownernum, 1, "ri7", N_HITPUSH, target->clientnum, gun, damage, v.x, v.y, v.z);
            target->setpushed();
        }
        if(ts.health<=0)
        {
            target->state.deaths++;
            int fragvalue = smode ? smode->fragvalue(target, actor) : (target==actor || isteam(target->team, actor->team) ? -1 : 1);
            actor->state.frags += fragvalue;
            //zeromod
            if(target==actor) target->state._suicides++;
            ////
            if(fragvalue>0)
            {
                int friends = 0, enemies = 0; // note: friends also includes the fragger
                if(m_teammode) loopv(clients) if(strcmp(clients[i]->team, actor->team)) enemies++; else friends++;
                else { friends = 1; enemies = clients.length()-1; }
                actor->state.effectiveness += fragvalue*friends/float(max(enemies, 1));
            }
            teaminfo *t = m_teammode ? teaminfos.access(actor->team) : NULL;
            if(t)
            {
                if(!protectteamscores || target!=actor || smode || ts.frags >= 0) t->frags += fragvalue;
            }
            sendf(-1, 1, "ri5", N_DIED, target->clientnum, actor->clientnum, actor->state.frags, t ? t->frags : 0);
            //zeromod
            if(fragvalue>0)
            {
                if(!actor->state.lastkill || gamemillis-actor->state.lastkill>2000) actor->state.multikills = 0;
                actor->state.lastkill = gamemillis ? gamemillis : 1;
                actor->state.multikills++;
                actor->state.rampage++;
                if(allowmultikill && !m_edit)
                {
                    if(identexists("showmultikill"))
                    {
                        defformatstring(cmd)("showmultikill %d %d %d", actor->clientnum, actor->state.rampage, actor->state.multikills);
                        execute(cmd);
                    }
                    else
                    {
                        if(actor->state.rampage > 0 && actor->state.rampage%5 == 0) sendservmsgf("\fs%s \f2did \f6%d \f2frags\fr", colorname(actor), actor->state.rampage);
                    }
                }
            }
            ////
            target->position.setsize(0);
            if(smode) smode->died(target, actor);
            ts.state = CS_DEAD;
            ts.lastdeath = gamemillis;
            if(actor!=target && isteam(actor->team, target->team))
            {
                actor->state.teamkills++;
                sendteamkillmessage(actor);
                target->_xi.tkiller = actor;
                addteamkill(actor, target, 1);
            }
            ts.deadflush = ts.lastdeath + DEATHMILLIS;
            // don't issue respawn yet until DEATHMILLIS has elapsed
            // ts.respawn();
        }
    }

    void suicide(clientinfo *ci)
    {
        gamestate &gs = ci->state;
        if(gs.state!=CS_ALIVE) return;
        int fragvalue = smode ? smode->fragvalue(ci, ci) : -1;
        ci->state.frags += fragvalue;
        ci->state.deaths++;
        //zeromod
        ci->state._suicides++;
        ////
        teaminfo *t = m_teammode ? teaminfos.access(ci->team) : NULL;
        if(t)
        {
            //t->frags += fragvalue;
            if(!protectteamscores || smode || gs.frags >= 0) t->frags += fragvalue;
        }
        sendf(-1, 1, "ri5", N_DIED, ci->clientnum, ci->clientnum, gs.frags, t ? t->frags : 0);
        ci->position.setsize(0);
        if(smode) smode->died(ci, NULL);
        gs.state = CS_DEAD;
        gs.lastdeath = gamemillis;
        gs.respawn();
    }

    void suicideevent::process(clientinfo *ci)
    {
        suicide(ci);
    }

    void explodeevent::process(clientinfo *ci)
    {
        gamestate &gs = ci->state;
        switch(gun)
        {
            case GUN_RL:
                if(!gs.rockets.remove(id)) return;
                break;

            case GUN_GL:
                if(!gs.grenades.remove(id)) return;
                break;

            default:
                return;
        }
        sendf(-1, 1, "ri4x", N_EXPLODEFX, ci->clientnum, gun, id, ci->ownernum);
        loopv(hits)
        {
            hitinfo &h = hits[i];
            clientinfo *target = getinfo(h.target);
//          if(!target || target->state.state!=CS_ALIVE || h.lifesequence!=target->state.lifesequence || h.dist<0 || h.dist>guns[gun].exprad) continue;
            if(!target || target->state.state!=CS_ALIVE || h.lifesequence!=target->state.lifesequence) continue;
            bool dup = false;
            loopj(i) if(hits[j].target==h.target) { dup = true; break; }
            if(h.dist < 0 || h.dist > guns[gun].exprad || dup)
            {
                _cheater(ci, "gunhack::explosioin", AC_GUNHACK, 100);
                break;
            }
            int damage = guns[gun].damage;
            if(gs.quadmillis) damage *= 4;
            damage = int(damage*(1-h.dist/EXP_DISTSCALE/guns[gun].exprad));
            if(target==ci) damage /= EXP_SELFDAMDIV;
            dodamage(target, ci, damage, gun, h.dir);
        }
    }

    void shotevent::process(clientinfo *ci)
    {
        gamestate &gs = ci->state;
        int wait = millis - gs.lastshot;
/*      if(!gs.isalive(gamemillis) ||
           wait<gs.gunwait ||
           gun<GUN_FIST || gun>GUN_PISTOL ||
           gs.ammo[gun]<=0 || (guns[gun].range && from.dist(to) > guns[gun].range + 1))
            return;
*/

        if(gun < GUN_FIST || gun > GUN_PISTOL ||
            (guns[gun].range && from.dist(to) > guns[gun].range + 1))
        {
            _cheater(ci, "gunhack", AC_GUNHACK, 100);
            return;
        }
        if(!gs.isalive(gamemillis) /*|| wait < gs.gunwait*/) return; //tolerate this
        if(wait < gs.gunwait)
        {
            _cheater(ci, "gunhack::gundelay", AC_GUNHACK, 100);
            return;
        }

        if(gs.ammo[gun] <= 0)
        {
            if(m_insta && gs.state==CS_ALIVE) _cheater(ci, "gunhack::noammo", AC_GUNHACK, 25);
            return;
        }

        if(gun!=GUN_FIST) gs.ammo[gun]--;
        gs.lastshot = millis;
        gs.gunwait = guns[gun].attackdelay;
        sendf(-1, 1, "rii9x", N_SHOTFX, ci->clientnum, gun, id,
                int(from.x*DMF), int(from.y*DMF), int(from.z*DMF),
                int(to.x*DMF), int(to.y*DMF), int(to.z*DMF),
                ci->ownernum);
        gs.shotdamage += guns[gun].damage*(gs.quadmillis ? 4 : 1)*guns[gun].rays;
        switch(gun)
        {
            case GUN_RL: gs.rockets.add(id); break;
            case GUN_GL: gs.grenades.add(id); break;
            default:
            {
                int totalrays = 0, maxrays = guns[gun].rays;
                loopv(hits)
                {
                    hitinfo &h = hits[i];
                    clientinfo *target = getinfo(h.target);
//                  if(!target || target->state.state!=CS_ALIVE || h.lifesequence!=target->state.lifesequence || h.rays<1 || h.dist > guns[gun].range + 1) continue;
                    if(!target || target->state.state!=CS_ALIVE || h.lifesequence!=target->state.lifesequence) continue;
                    totalrays += h.rays;
//                  if(totalrays>maxrays) continue;
                    if(totalrays>maxrays || h.rays<1 || h.dist>guns[gun].range + 1)
                    {
                        //cheat :P
                        _cheater(ci, "gunhack", AC_GUNHACK, 100);
                        break;
                    }
                    int damage = h.rays*guns[gun].damage;
                    if(gs.quadmillis) damage *= 4;
                    //bool shouldskip = false;
                    if(identexists("onhit"))
                    {
                        defformatstring(es)("onhit %i %i %f %f %f %i %f %f %f %i %i",
                                            ci->clientnum, gun, from.x, from.y, from.z,
                                            target->clientnum, to.x, to.y, to.z,
                                            h.rays, damage);
                        /*shouldskip = */executebool(es);
                    }
                    /*if(!shouldskip) */dodamage(target, ci, damage, gun, h.dir);
                }
                break;
            }
        }
    }

#if 0
    void vectoyawpitch(const vec &v, float &yaw, float &pitch)
    {
        yaw = -atan2(v.x, v.y)/RAD;
        pitch = asin(v.z/v.magnitude())/RAD;
    }
#endif

    ICOMMAND(getclientx, "i", (int *cn), { clientinfo *ci = getinfo(*cn); floatret(ci ? ci->state.o.x : 0.0); });
    ICOMMAND(getclienty, "i", (int *cn), { clientinfo *ci = getinfo(*cn); floatret(ci ? ci->state.o.y : 0.0); });
    ICOMMAND(getclientz, "i", (int *cn), { clientinfo *ci = getinfo(*cn); floatret(ci ? ci->state.o.z : 0.0); });
    ICOMMAND(getclientyaw, "i", (int *cn), { clientinfo *ci = getinfo(*cn); floatret(ci ? ci->state.yaw : 0.0); });
    ICOMMAND(getclientpitch, "i", (int *cn), { clientinfo *ci = getinfo(*cn); floatret(ci ? ci->state.pitch : 0.0); });
    ICOMMAND(vectoyaw, "ff", (float *x, float *y), { floatret(-atan2(*x, *y)/RAD); });
    ICOMMAND(vectopitch, "fff", (float *x, float *y, float *z), { vec v(*x, *y, *z); floatret(asin(v.z/v.magnitude())/RAD); });
    ICOMMAND(getclientcolorname, "i", (int *cn), { clientinfo *ci = getinfo(*cn); result(ci ? colorname(ci) : ""); });
    ICOMMAND(toclient, "is", (int *cn, const char *msg), { clientinfo *ci = (clientinfo *)getclientinfo(*cn); if(ci) sendf(ci->clientnum, 1, "ris", N_SERVMSG, msg); });

    void pickupevent::process(clientinfo *ci)
    {
        gamestate &gs = ci->state;
        if(!gs.isalive(gamemillis)) return;
        pickup(ent, ci->clientnum);
    }

    bool gameevent::flush(clientinfo *ci, int fmillis)
    {
        process(ci);
        return true;
    }

    bool timedevent::flush(clientinfo *ci, int fmillis)
    {
        if(millis > fmillis) return false;
        else if(millis >= ci->lastevent)
        {
            ci->lastevent = millis;
            process(ci);
        }
        return true;
    }

    inline void clearevent(clientinfo *ci)
    {
        delete ci->events.remove(0);
    }

    void flushevents(clientinfo *ci, int millis)
    {
        while(ci->events.length())
        {
            gameevent *ev = ci->events[0];
            if(ev->flush(ci, millis)) clearevent(ci);
            else break;
        }
    }

    void processevents()
    {
        loopv(clients)
        {
            clientinfo *ci = clients[i];
            if(curtime>0 && ci->state.quadmillis) ci->state.quadmillis = max(ci->state.quadmillis-curtime, 0);
            flushevents(ci, gamemillis);
        }
    }

    void cleartimedevents(clientinfo *ci)
    {
        int keep = 0;
        loopv(ci->events)
        {
            if(ci->events[i]->keepable())
            {
                if(keep < i)
                {
                    for(int j = keep; j < i; j++) delete ci->events[j];
                    ci->events.remove(keep, i - keep);
                    i = keep;
                }
                keep = i+1;
                continue;
            }
        }
        while(ci->events.length() > keep) delete ci->events.pop();
        ci->timesync = false;
    }

    void serverupdate()
    {
        if(shouldstep && !gamepaused)
        {
            gamemillis += curtime;

            if(m_demo) readdemo();
            else if(!m_timed || gamemillis < gamelimit)
            {
                processevents();
                if(curtime && !m_noitems)
                {
                    loopv(sents) if(sents[i].spawntime) // spawn entities when timer reached
                    {
                        int oldtime = sents[i].spawntime;
                        sents[i].spawntime -= curtime;
                        if(sents[i].spawntime<=0)
                        {
                            sents[i].spawntime = 0;
                            sents[i].spawned = true;
                            sendf(-1, 1, "ri2", N_ITEMSPAWN, i);
                        }
                        else if(sents[i].spawntime<=10000 && oldtime>10000 && (sents[i].type==I_QUAD || sents[i].type==I_BOOST))
                        {
                            sendf(-1, 1, "ri2", N_ANNOUNCE, sents[i].type);
                        }
                    }
                }
                aiman::checkai();
                if(smode) smode->update();
            }
        }

        while(bannedips.length() && bannedips[0].expire-totalmillis <= 0) bannedips.remove(0);
        while(_scheduled_disconnects.length())
        {
            disconnect_client(_scheduled_disconnects[0].n, _scheduled_disconnects[0].reason);
            _scheduled_disconnects.remove(0);
        }
        loopv(connects) if(totalmillis-connects[i]->connectmillis>15000) disconnect_client(connects[i]->clientnum, DISC_TIMEOUT);

        if(nextexceeded && gamemillis > nextexceeded && (!m_timed || gamemillis < gamelimit))
        {
            nextexceeded = 0;
            loopvrev(clients)
            {
                clientinfo &c = *clients[i];
                if(c.state.aitype != AI_NONE) continue;
                if(c.checkexceeded())
                {
                    //_cheater(clients[i], "player velocity exceeded", AC_POSHACK, 100);
                    disconnect_client(c.clientnum, DISC_MSGERR);
                }
                else c.scheduleexceeded();
            }
        }

        if(shouldcheckteamkills) checkteamkills();

        if(shouldstep && !gamepaused)
        {
            if(m_timed && smapname[0] && gamemillis-curtime>0) checkintermission();
            if(interm > 0 && gamemillis>interm)
            {
                if(demorecord) enddemorecord();
                interm = -1;
                checkvotes(true);
            }
        }

        shouldstep = clients.length() > 0;
    }

    struct crcinfo
    {
        int crc, matches;

        crcinfo() {}
        crcinfo(int crc, int matches) : crc(crc), matches(matches) {}

        static bool compare(const crcinfo &x, const crcinfo &y) { return x.matches > y.matches; }
    };

    VAR(serverspecmod, 0, 0, 2);    // server spectates moded map owners: 0=no, 1=for this map only, 2=perm (saved in clientinfo)

    void checkmaps(int req)
    {
        if(m_edit || !smapname[0]) return;
        vector<crcinfo> crcs;
        int total = 0, unsent = 0, invalid = 0;
        if(mcrc) crcs.add(crcinfo(mcrc, clients.length() + 1));
        loopv(clients)
        {
            clientinfo *ci = clients[i];
            if(ci->state.state==CS_SPECTATOR || ci->_xi.spy || ci->state.aitype != AI_NONE) continue;
            total++;
            if(!ci->clientmap[0])
            {
                if(ci->mapcrc < 0) invalid++;
                else if(!ci->mapcrc) unsent++;
            }
            else
            {
                crcinfo *match = NULL;
                loopvj(crcs) if(crcs[j].crc == ci->mapcrc) { match = &crcs[j]; break; }
                if(!match) crcs.add(crcinfo(ci->mapcrc, 1));
                else match->matches++;
            }
        }
        if(!mcrc && total - unsent < min(total, 4)) return;
        crcs.sort(crcinfo::compare);
        string msg;
        loopv(clients)
        {
            clientinfo *ci = clients[i];
            if(ci->state.state==CS_SPECTATOR || ci->_xi.spy || ci->state.aitype != AI_NONE || ci->clientmap[0] || ci->mapcrc >= 0 || (req < 0 && ci->warned)) continue;
            formatstring(msg)("%s has modified map \"%s\"", colorname(ci), smapname);
            sendf(req, 1, "ris", N_SERVMSG, msg);
            if(req < 0)
            {
                ci->warned = true;
                if(serverspecmod) _forcespectator(ci, serverspecmod);
            }
        }
        if(crcs.empty() || crcs.length() < 2) return;
        loopv(crcs)
        {
            crcinfo &info = crcs[i];
            if(i || info.matches <= crcs[i+1].matches) loopvj(clients)
            {
                clientinfo *ci = clients[j];
                if(ci->state.state==CS_SPECTATOR || ci->_xi.spy || ci->state.aitype != AI_NONE || !ci->clientmap[0] || ci->mapcrc != info.crc || (req < 0 && ci->warned)) continue;
                formatstring(msg)("%s has modified map \"%s\"", colorname(ci), smapname);
                sendf(req, 1, "ris", N_SERVMSG, msg);
                if(req < 0)
                {
                    ci->warned = true;
                    if(serverspecmod) _forcespectator(ci, serverspecmod);
                }
            }
        }
    }

    void sendservinfo(clientinfo *ci)
    {
        sendf(ci->clientnum, 1, "ri5ss", N_SERVINFO, ci->clientnum, PROTOCOL_VERSION, ci->sessionid, serverpass[0] ? 1 : 0, serverdesc, serverauth);
    }

    void noclients()
    {
        bannedips.shrink(0);
        aiman::clearai();
        persist = persistteams;
        autosendmap = 0;
        if(_newflagrun) { _storeflagruns(); _newflagrun = 0; }
        _nodamage = 0;
    }
#if 0
    void localconnect(int n)
    {
        clientinfo *ci = getinfo(n);
        ci->clientnum = ci->ownernum = n;
        ci->connectmillis = totalmillis;
        ci->sessionid = (rnd(0x1000000)*((totalmillis%10000)+1))&0xFFFFFF;
        ci->local = true;

        connects.add(ci);
        sendservinfo(ci);
    }

    void localdisconnect(int n)
    {
        if(m_demo) enddemoplayback();
        clientdisconnect(n);
    }
#endif
    int clientconnect(int n, uint ip)
    {
        clientinfo *ci = getinfo(n);
        ci->clientnum = ci->ownernum = n;
        ci->connectmillis = totalmillis;
        ci->sessionid = (rnd(0x1000000)*((totalmillis%10000)+1))&0xFFFFFF;

        connects.add(ci);
        sendservinfo(ci);
        return DISC_NONE;
    }

    void clientdisconnect(int n, int reason)
    {
        clientinfo *ci = (clientinfo *)getclientinfo(n);
        if(!ci) return;
        loopv(clients)
        {
            if(clients[i]->authkickvictim == ci->clientnum) clients[i]->cleanauth();
            if(clients[i]->_xi.tkiller == ci) clients[i]->_xi.tkiller = 0;
            if(clients[i]->_xi.votekickvictim == ci->clientnum) clients[i]->_xi.votekickvictim = -1;
        }
        const char *msg = disconnectreason(reason);
        string s;
        if(ci->connected)
        {
            if(ci->privilege) setmaster(ci, false);
            if(smode) smode->leavegame(ci, true);
            ci->state.timeplayed += lastmillis - ci->state.lasttimeplayed;
            savescore(ci);
            logoutf("leave: %s", colorname(ci));
            if(!ci->_xi.spy) sendf(-1, 1, "ri2", N_CDIS, n);
            aiman::removeai(ci);
            clients.removeobj(ci);
            if(msg)
            {
                formatstring(s)("\f4client %s (%s) disconnected because: %s", colorname(ci), getclienthostname(n), msg);
                sendservmsg(s);
            }
            if(!numclients(-1, false, true)) noclients(); // bans clear when server empties
            //if(ci->local) checkpausegame();
        }
        else
        {
            connects.removeobj(ci);
            if(reason != DISC_IPBAN)
            {
                if(msg) formatstring(s)("\f4client (%s) disconnected because: %s", getclienthostname(n), msg);
                else formatstring(s)("\f4client (%s) disconnected", getclienthostname(n));
                sendservmsg(s);
            }
        }

        if(publicserver != 1 && autolockmaster && numclients(-1, false) < autolockmaster) mastermask |= MM_AUTOAPPROVE;
    }

    int reserveclients() { return 8; }

    struct gbaninfo
    {
        enet_uint32 ip, mask;
        int master;
    };
    vector<gbaninfo> gbans;

    void cleargbans(int master)
    {
        if(master < 0)
        {
            loopvrev(gbans) if(gbans[i].master >= 0) gbans.remove(i);
        }
        else
        {
            loopvrev(gbans) if(gbans[i].master == master) gbans.remove(i);
        }
    }

    bool checkgban(uint ip)
    {
        loopv(gbans) if((ip & gbans[i].mask) == gbans[i].ip) return true;
        return false;
    }

    void addgban(int master, const char *name)
    {
        union { uchar b[sizeof(enet_uint32)]; enet_uint32 i; } ip, mask;
        ip.i = 0;
        mask.i = 0;
        loopi(4)
        {
            char *end = NULL;
            int n = strtol(name, &end, 10);
            if(!end) break;
            if(end > name) { ip.b[i] = n; mask.b[i] = 0xFF; }
            name = end;
            while(*name && *name++ != '.');
        }
        gbaninfo &ban = gbans.add();
        ban.ip = ip.i;
        ban.mask = mask.i;
        ban.master = master;

        loopvrev(clients)
        {
            clientinfo *ci = clients[i];
            if(ci->privilege >= PRIV_ADMIN) continue;
            if(checkgban(getclientip(ci->clientnum))) disconnect_client(ci->clientnum, DISC_IPBAN);
        }
    }

    struct pbaninfo
    {
        enet_uint32 ip, mask;
        char reason[80];
    };
    vector<pbaninfo> pbans;

    void clearpbans()
    {
        pbans.shrink(0);
    }
    COMMAND(clearpbans, "");

    bool checkpban(uint ip)
    {
        loopvrev(pbans) if((ip & pbans[i].mask) == pbans[i].ip) return true;
        return false;
    }

    void addpban(const char *name, const char *reason)
    {
        pbaninfo b;
        union { uchar b[sizeof(enet_uint32)]; enet_uint32 i; } ip, mask;
        ip.i = 0;
        mask.i = 0;
        loopi(4)
        {
            char *end = NULL;
            int n = strtol(name, &end, 10);
            if(!end) break;
            if(end > name) { ip.b[i] = n; mask.b[i] = 0xFF; }
            name = end;
            while(*name && *name++ != '.');
        }
        b.ip = ip.i;
        b.mask = mask.i;
        if(reason) filtertext(b.reason, reason, true, 80);
        else b.reason[0] = 0;
        pbans.add(b);
    }
    COMMAND(addpban, "ss");
/*
    struct reservedname
    {
        bool sensitive;
        char name[MAXNAMELEN+1];
    };
    vector<reservedname> reservednames;

    bool nameprotection(const char *name)
    {
        loopvrev(reservednames)
            if(reservednames[i].sensitive ? (!strcmp(name, reservednames[i].name)) : (!strcasecmp(name, reservednames[i].name)))
                return true;
        return false;
    }
*/
    int allowconnect(clientinfo *ci/*, const char *pwd = ""*/)
    {
        //if(serverpass[0])
        //{
        //    if(!checkpassword(ci, serverpass, pwd)) return DISC_PASSWORD;
        //    return DISC_NONE;
        //}
        //if(adminpass[0] && checkpassword(ci, adminpass, pwd)) return DISC_NONE;
        if(numclients(-1, false, true)>=maxclients) return DISC_MAXCLIENTS;
        uint ip = getclientip(ci->clientnum);
        loopvrev(bannedips) if(bannedips[i].ip==ip) return DISC_IPBAN;
        if(checkgban(ip)) return DISC_IPBAN;
        if(checkpban(ip)) return DISC_IPBAN;
        if(mastermode>=MM_PRIVATE && allowedips.find(ip)<0) return DISC_PRIVATE;
        //if(nameprotection(ci->name)) return DISC_PASSWORD;
        return DISC_NONE;
    }

    bool allowbroadcast(int n)
    {
        clientinfo *ci = (clientinfo *)getclientinfo(n);
        return ci && ci->connected;
    }

    clientinfo *findauth(uint id)
    {
        loopv(clients) if(clients[i]->authreq == id) return clients[i];
        loopv(connects) if(connects[i]->authreq == id) return connects[i];
        return NULL;
    }

    // finds local authkey and sends challenge to client
    // returns true on success and false on failure
    inline bool challengeauth(clientinfo *ci)
    {
        userinfo *u = users.access(userkey(ci->authname, ci->authdesc));
        if(u)
        {
            uint seed[3] = { ::hthash(serverauth) + detrnd(size_t(ci) + size_t(ci->authname) + size_t(ci->authdesc), 0x10000), uint(totalmillis), randomMT() };
            vector<char> buf;
            ci->authchallenge = genchallenge(u->pubkey, seed, sizeof(seed), buf);
            sendf(ci->clientnum, 1, "risis", N_AUTHCHAL, ci->authdesc, ci->authreq, buf.getbuf());
            return true;
        }
        else return false;
    }

    // executed then auth server failed to authenticate user
    void authfailed(clientinfo *ci)
    {
        // we failed in this auth server, check for other posibilities
        bool found = false;
        for(int authmaster = findauthmaster(ci->authdesc, ci->authmaster); authmaster >= 0; authmaster = findauthmaster(ci->authdesc, authmaster))
        {
            if(requestmasterf(authmaster, "reqauth %u %s\n", ci->authreq, ci->authname))
            {
                ci->authmaster = authmaster;
                found = true;
                break;
            }
        }
        // no more suitable auth servers?
        if(!found)
        {
            // we are doomed
            ci->cleanauth();
            if(ci->connectauth) disconnect_client(ci->clientnum, ci->connectauth);
        }
    }

    // auth server successfully authenticated user
    void authsucceeded(clientinfo *ci)
    {
        int priv = masterauthprivilege(ci->authmaster);
        ci->cleanauth(ci->connectauth!=0);
        if(ci->connectauth) connected(ci);
        if(ci->authkickvictim >= 0)
        {
            if(setmaster(ci, true, "", ci->authname, ci->authdesc, priv, false, true))
                trykick(ci, ci->authkickvictim, ci->authkickreason, ci->authname, ci->authdesc, priv);
            ci->cleanauthkick();
        }
        else setmaster(ci, true, "", ci->authname, ci->authdesc, priv);
    }

    // auth server sent challenge to user
    void authchallenged(clientinfo *ci, const char *val, const char *desc = "")
    {
        sendf(ci->clientnum, 1, "risis", N_AUTHCHAL, desc, ci->authreq, val);
    }

    uint nextauthreq = 0;

    // executed then user tries to authenticate
    bool tryauth(clientinfo *ci, const char *user, const char *desc)
    {
        ci->cleanauth();
        if(!nextauthreq) nextauthreq = 1;
        ci->authreq = nextauthreq++;
        filtertext(ci->authname, user, false, 100);
        copystring(ci->authdesc, desc);

        // firstly check for local authkeys
        if(!ci->authdesc[0] || !challengeauth(ci))
        {
            // no local authkeys found, search for suitable auth server
            for(int authmaster = findauthmaster(ci->authdesc); authmaster >= 0; authmaster = findauthmaster(ci->authdesc, authmaster))
            {
                if(requestmasterf(authmaster, "reqauth %u %s\n", ci->authreq, ci->authname))
                {
                    // this server seems to work, use it for auth
                    ci->authmaster = authmaster;
                    break;
                }
            }
            // we found 0 suitable auth servers?
            if(ci->authmaster < 0)
            {
                // cleanup
                ci->cleanauth();
                // warn user
                if(!ci->authdesc[0])
                    sendf(ci->clientnum, 1, "ris", N_SERVMSG, "not connected to authentication server");
            }
        }

        if(ci->authreq) return true;
        if(ci->connectauth) disconnect_client(ci->clientnum, ci->connectauth);
        return false;
    }

    void masterconnected(int m)
    {
    }

    void masterdisconnected(int m)
    {
        loopvrev(clients)
        {
            clientinfo *ci = clients[i];
            if(ci->authreq && ci->authmaster == m) authfailed(ci);
        }
    }

    bool answerchallenge(clientinfo *ci, uint id, char *val, const char *desc)
    {
        if(ci->authreq != id || strcmp(ci->authdesc, desc))
        {
            ci->cleanauth();
            return !ci->connectauth;
        }
        for(char *s = val; *s; s++)
        {
            if(!isxdigit(*s)) { *s = '\0'; break; }
        }

        // masterserver auth, forward challenge to masterserver
        if(ci->authmaster >= 0)
        {
            if(!requestmasterf(ci->authmaster, "confauth %u %s\n", id, val))
            {
                // auth server seems to be disconnected, so find another one
                bool found = false;
                for(int authmaster = findauthmaster(ci->authdesc, ci->authmaster); authmaster >= 0; authmaster = findauthmaster(ci->authdesc, authmaster))
                    if(requestmasterf(authmaster, "reqauth %u %s\n", ci->authreq, ci->authname))
                    {
                        ci->authmaster = authmaster;
                        found = true;
                        break;
                    }
                // no suitable auth servers found?
                if(!found)
                {
                    ci->cleanauth();
                    if(!ci->authdesc[0]) sendf(ci->clientnum, 1, "ris", N_SERVMSG, "not connected to authentication server");
                }
            }
        }
        else
        {
            if(ci->authchallenge && checkchallenge(val, ci->authchallenge))
            {
                userinfo *u = users.access(userkey(ci->authname, ci->authdesc));
                if(u)
                {
                    if(ci->connectauth) connected(ci);
                    if(ci->authkickvictim >= 0)
                    {
                        if(u->privilege && setmaster(ci, true, "", ci->authname, ci->authdesc, u->privilege, false, true))
                            trykick(ci, ci->authkickvictim, ci->authkickreason, ci->authname, ci->authdesc, u->privilege);
                    }
                    else setmaster(ci, true, "", ci->authname, ci->authdesc, u->privilege);
                }
            }
            ci->cleanauth();
        }

        return ci->authreq || !ci->connectauth;
    }

    void processmasterinput(int i, const char *cmd, int cmdlen, const char *args)
    {
        uint id;
        string val;
        clientinfo *ci;
        if(sscanf(cmd, "failauth %u", &id) == 1)
        {
            ci = findauth(id);
            if(ci && ci->authmaster == i) authfailed(ci);
        }
        else if(sscanf(cmd, "succauth %u", &id) == 1)
        {
            ci = findauth(id);
            if(ci && ci->authmaster == i) authsucceeded(ci);
        }
        else if(sscanf(cmd, "chalauth %u %255s", &id, val) == 2)
        {
            ci = findauth(id);
            if(ci && ci->authmaster == i) authchallenged(ci, val, ci->authdesc);
        }
        else if(!strncmp(cmd, "cleargbans", cmdlen))
        {
            cleargbans(i);
        }
        else if(sscanf(cmd, "addgban %100s", val) == 1)
        {
            if(usemastergbans(i)) addgban(i, val);
        }
    }

    void _sendmap(clientinfo *ci, clientinfo *target)
    {
        if(!target) return;

        if(!mapdata)
        {
            if(ci) sendf(ci->clientnum, 1, "ris", N_SERVMSG, "no map to send");
        }
        else if(target->getmap)
        {
            if(ci) sendf(ci->clientnum, 1, "ris", N_SERVMSG, "already sending map");
        }
        else
        {
            sendservmsgf("[%s is getting the map]", colorname(target));
            if((target->getmap = sendfile(target->clientnum, 2, mapdata, "ri", N_SENDMAP))) target->getmap->freeCallback = freegetmap;
            target->needclipboard = totalmillis ? totalmillis : 1;
        }
    }

    VAR(maxsendmap, -1, 4, 1024);
    void receivefile(int sender, uchar *data, int len)
    {
        clientinfo *ci = getinfo(sender);
        if(!ci || !m_edit) return;
        if(ci->state.state==CS_SPECTATOR && !ci->privilege) return;
        if(maxsendmap >= 0 && len > maxsendmap*1024*1024 && ci->privilege < PRIV_ROOT)
        {
            sendf(sender, 1, "ris",
                N_SERVMSG,
                maxsendmap
                ?"server rejected map because of size"
                :"server has disabled /sendmap");
            return;
        }
        if(ci->_xi.editmute && !ci->privilege)
        {
            sendf(sender, 1, "ris", N_SERVMSG, "\f5[MUTE] \f3Your sendmap was muted");
            return;
        }
        if(mapdata) DELETEP(mapdata);
        if(!len) return;
        mapdata = opentempfile("mapdata", "w+b");
        if(!mapdata) { sendf(sender, 1, "ris", N_SERVMSG, "failed to open temporary file for map"); return; }
        mapdata->write(data, len);
        sendservmsgf("[%s sent a map to server, \"\f0/getmap\f7\" to receive it]", colorname(ci));
        if(autosendmap >= 2) loopv(clients)
        {
            if(clients[i]->state.aitype!=AI_NONE || clients[i]->clientnum==ci->clientnum) continue;
            _sendmap(NULL, clients[i]);
        }
    }

    void sendclipboard(clientinfo *ci)
    {
        if(!ci->lastclipboard || !ci->clipboard) return;
        if(ci->_xi.editmute) return;
        bool flushed = false;
        loopv(clients)
        {
            clientinfo &e = *clients[i];
            if(e.clientnum != ci->clientnum && e.needclipboard - ci->lastclipboard >= 0)
            {
                if(!flushed) { flushserver(true); flushed = true; }
                sendpacket(e.clientnum, 1, ci->clipboard);
            }
        }
    }

    bool connected(clientinfo *ci)
    {
        if(m_demo) enddemoplayback();

        if(!hasmap(ci)) rotatemap(false);

        shouldstep = true;

        connects.removeobj(ci);
        clients.add(ci);

        ci->connectauth = 0;
        ci->connected = true;
        ci->needclipboard = totalmillis ? totalmillis : 1;
        if(mastermode>=MM_LOCKED || ci->_xi.forcedspectator) ci->state.state = CS_SPECTATOR;
        ci->state.lasttimeplayed = lastmillis;

        const char *worst = m_teammode ? chooseworstteam(NULL, ci) : NULL;
        copystring(ci->team, worst ? worst : "good", MAXTEAMLEN+1);

        sendwelcome(ci);
        if(restorescore(ci)) sendresume(ci);
        logoutf("join: %s", colorname(ci));
        sendinitclient(ci);

        aiman::addclient(ci);

        if(m_demo) setupdemoplayback();

        {
            memset(_hp.args, 0, sizeof(_hp));
            _hp.args[0] = &ci->clientnum;
            _hp.args[1] = (void *)colorname(ci);
            uint ip = getclientip(ci->clientnum);
            _hp.args[2] = &ip;
            _hp.args[3] = (void *)getclienthostname(ci->clientnum);
            _exechook("connected");
            if(_hp.args[4])
            {
                if(((const char *)_hp.args[4])[0])
                {
                    sendservmsgf("%s was kicked because: %s", colorname(ci), (const char *)_hp.args[4]);
                    logoutf("%s was kicked by external module because: %s", colorname(ci), (const char *)_hp.args[4]);
                }
                else
                {
                    sendservmsgf("%s was kicked", colorname(ci));
                    logoutf("%s was kicked by external module", colorname(ci));
                }
                addban(ip, 4*60*60000);
                _schedule_disconnect(ci->ownernum, DISC_KICK);
                return false;
            }
        }

        if(servermotd[0]) sendf(ci->clientnum, 1, "ris", N_SERVMSG, servermotd);

        if(m_edit && autosendmap) _sendmap(NULL, ci);

        // automatically change master mode to auth if certain client count reached
        if(publicserver != 1 && autolockmaster && numclients(-1, false) >= autolockmaster) mastermask &= ~MM_AUTOAPPROVE;

        return true;
    }

// **************************    ZEROMOD     **********************************************
    void _privfail(clientinfo *ci);

    VAR(serverdebug, 0, 1, 1);
    VAR(debuglevel, 0, PRIV_ROOT, PRIV_ROOT+1);

    void _debug(const char *msg)
    {
        string buf;
        logoutf("debug::%s", msg ? msg : "");
        formatstring(buf)("\f4[DEBUG] \f7%s", msg ? msg : "");
        loopv(clients) if(clients[i] && clients[i]->privilege >= debuglevel)
        {
            sendf(clients[i]->clientnum, 1, "ris", N_SERVMSG, buf);
        }
    }

    struct _funcdeclaration
    {
        string name;    //function name
        int priv;       //privilege required to call function
        void (*func)(const char *cmd, const char *args, clientinfo *ci);    //function pointer
        bool hidden, disabled;  //some flags

        _funcdeclaration()
        {
            name[0] = 0;
            priv = 0;
            func = 0;
            hidden = disabled = false;
        }

        _funcdeclaration(const char *name, int priv, void (*func)(const char *cmd, const char *args, clientinfo *ci), bool hidden = false, bool disabled = false)
        {
            copystring(_funcdeclaration::name, name);
            _funcdeclaration::priv = priv;
            _funcdeclaration::func = func;
            _funcdeclaration::hidden = hidden;
            _funcdeclaration::disabled = disabled;
        }
    };
    vector<_funcdeclaration *> _funcs;

    void _notify(const char *msg, clientinfo *ci = 0, int priv = 0, bool self = true)
    {
        loopv(clients) if(clients[i]->state.aitype==AI_NONE && ((priv && clients[i]->privilege >= priv) || (self && clients[i]==ci)))
        {
            sendf(clients[i]->clientnum, 1, "ris", N_SERVMSG, msg);
        }
    }

    void _notifypriv(const char *msg, int min, int max)
    {
        loopv(clients) if(clients[i]->state.aitype==AI_NONE && (clients[i]->privilege>=min) && (clients[i]->privilege<=max))
            sendf(clients[i]->clientnum, 1, "ris", N_SERVMSG, msg);
    }

    int _argsep(char *str, int c, char *argv[], char sep = ' ') //separate args (str = source string; c = expected argc; argv = ptrs array; ret = argc)
    {
        char *s;
        int argc;

        for(int i = 1; i < c; i++) argv[i] = 0; //zero out all pointers
        argv[0] = str;
        if(!str || !*str) return 0;
        argc = 1;
        for(int i = 1; i < c; i++)
        {
            s = strchr(argv[i - 1], sep);
            if(!s) break;   //no delimiter found - prevous argument is last argument or string end
            *s = 0;         //replace delimiter with null
            s++;            //thing after delimiter
            while(*s == sep) s++;   //skip other delimiters if any
            argv[i] = s;    //thing after all delimiters
            argc++;
        }
        return argc;
    }

//  >>> Executable functions and extensions
    void _wall(const char *cmd, const char *args, clientinfo *ci)
    {
        if(!args || !*args) return;
        sendf(-1, 1, "ris", N_SERVMSG, args);
    }

    struct _manpage
    {
        char name[256];
        char args[64];
        char help[1024];
//      int expire;

        _manpage()
        {
//          expire = 0;
        }
        _manpage(const char *n, const char *a, const char *h)
        {
            if(n) strncpy(name, n, 256);
            if(a) strncpy(args, a, 64);
            if(h) strncpy(help, h, 1024);
//          expire = 0;
        }
    };
    vector<_manpage *> _manpages;

    bool _readmanfile(const char *cmd)
    {
        string buf;

        if(!cmd || !*cmd) return false;

        char badchars[] = "/$%^&*()\\'\"`~";
        for(const char *p = cmd; *p; p++) for(char *b = badchars; *b; b++) if(*p == *b) return false;
#ifdef WIN32
        formatstring(buf)("man\\%s", cmd);
#else
        formatstring(buf)("man/%s", cmd);
#endif
        FILE *f;
        f = fopen(buf, "r");
        if(!f) return false;

        _manpage *mp;
        mp = new _manpage;
        _manpages.add(mp);

        if(!fgets(mp->args, 64, f))
        {
            fclose(f);
            delete mp;
            _manpages.drop();
            return false;
        }
        for(char *p = mp->args; *p; p++) if(*p=='\n') *p=0;
        if(!fgets(mp->help, 1024, f))
        {
            fclose(f);
            delete mp;
            _manpages.drop();
            return false;
        }
        for(char *p = mp->help; *p; p++) if(*p=='\n') *p=0;
        fclose(f);
        return true;
    }

    static void _addmanpage(const char *cmd, const char *arg, const char *desc)
    {
        _manpages.add(new _manpage(cmd, arg, desc));
    }

    void _initman()
    {
        _addmanpage("help man", "[command]", "Shows help about command or prints avaiable commands");
        _addmanpage("info version", "", "Shows information about server");
        _addmanpage("wall announce", "<message>", "Prints message on the wall");
        _addmanpage("mute", "[cn]", "Mutes one or all players");
        _addmanpage("unmute", "[cn]", "Unmutes one or all players");
        _addmanpage("priv setpriv givepriv", "[cn] <priv>", "Gives privilege for user (privilege can be number or string, like master)");
        _addmanpage("takepriv", "[cn]", "Takes privilege from cn or you");
        _addmanpage("setmaster givemaster", "[cn]", "Gives master");
        _addmanpage("setadmin giveadmin", "[cn]", "Gives admin");
        _addmanpage("spec spectate", "[cn] [mode]", "Spectates one or all players; mode can be: 0-unspectate, 1-spectate for this map, 2-perm");
        _addmanpage("unspec unspectate", "[cn]", "Unspectates one or all players");
        _addmanpage("stats", "[cn]", "Gives stats of you or another user, use -1 to see stats of all players");
        _addmanpage("pm", "<cn>[,cn,...] <message>", "Sends message to specified clients");
        _addmanpage("editmute", "[cn] [mode]", "Mutes one or all players editing; mode can be: 0-unmute, 1-mute for this map, 2-perm");
        _addmanpage("editunmute", "[cn]", "Unmutes one or all players editing");
        _addmanpage("load", "<module>", "Loads specified module");
        _addmanpage("reload", "<module>", "Reloads specified module");
        _addmanpage("unload", "<module>", "Unloads specified module");
        _addmanpage("exec", "<cubescript>", "Executes cubescript command");
        _addmanpage("spy", "[1/0]", "Enters or leaves spy mode");
        _addmanpage("np forgive fg", "", "Forgives teamkill");
        _addmanpage("interm intermission", "", "Starts intermission");
        _addmanpage("ban", "cn [time]", "Bans client; time is in format: [num][ ][s/m/h/d]; by default, time is 4h; example: #ban 0 1d");
        _addmanpage("votekick", "cn", "Votes client kick");
        _addmanpage("namemute", "cn [newname]", "Renames client and mutes further rename attempts from him/her");
        _addmanpage("unnamemute nameunmute", "cn [newname]", "Allows client to rename and optionally sets new name");
        _addmanpage("rename name", "<cn> [name]", "Renames player");
        _addmanpage("time", "<minutes> [seconds]", "Sets remaining time");
        _addmanpage("listgbans showgbans", "", "Shows gbas list");
        _addmanpage("nodamage", "[1/0]", "Disables damage in coop edit mode");
        _addmanpage("getip", "cn", "Get client ip");
        _addmanpage("persist", "mode", "Controls persist teams behavour: 0 - disabled, 1 - persist teams, 2 - persist only nonstandard teams");
    }

    void _man(const char *cmd, const char *args, clientinfo *ci)
    {
        char msg[MAXTRANS];
        bool usage = false;
        int searchc = 0;
        bool first;
        bool found;

        if(!ci) return;

        if(!args || !*args)
        {
            if(!ci) return;
            copystring(msg, "\f0[HELP] \f1Possible commands:", MAXTRANS);
            sendf(ci->ownernum, 1, "ris", N_SERVMSG, msg);
            for(int priv = 0; priv <= min(ci->privilege, int(PRIV_ROOT)); priv++)
            {
                first  = true;
                loopv(_funcs) if(_funcs[i] && !_funcs[i]->hidden && !_funcs[i]->disabled && _funcs[i]->priv == priv)
                {
                    if(first)
                    {
                        first = false;
                        switch(priv)
                        {
                            case PRIV_NONE: copystring(msg, "\f7", MAXTRANS); break;
                            case PRIV_MASTER: case PRIV_AUTH: copystring(msg, "\f0", MAXTRANS); break;
                            case PRIV_ADMIN: copystring(msg, "\f6", MAXTRANS); break;
                            case PRIV_ROOT: copystring(msg, "\f3", MAXTRANS); break;
                            default: copystring(msg, "\f1", MAXTRANS); break;
                        }
                    }
                    else concatstring(msg, ", ", MAXTRANS);
                    concatstring(msg, _funcs[i]->name, MAXTRANS);
                }
                if(!first) sendf(ci->ownernum, 1, "ris", N_SERVMSG, msg);
            }
            return;
        }

        if(cmd && *cmd && !strcmp(cmd, "usage")) usage = true;

    _search:
        found = false;
        loopv(_manpages) if(_manpages[i])
        {
            char name[256];
            char *names[16];
            int c;

            copystring(name, _manpages[i]->name, 256);
            c = _argsep(name, 16, names);
            loopj(c)
            {
                if(!strcmp(args, names[j]))
                {
                    if(usage && _manpages[i]->args[0] != 0)
                    {
                        formatstring(msg)("\f1[HELP] Usage: \f0%s \f2%s", args, _manpages[i]->args);
                    }
                    else if(_manpages[i]->args[0] == 0 && _manpages[i]->help[0] != 0)
                    {
                        formatstring(msg)("\f1[HELP] \f2%s", _manpages[i]->help);
                    }
                    else if(_manpages[i]->args[0] != 0 && _manpages[i]->help[0] != 0)
                    {
                        formatstring(msg)("\f1[HELP] Usage: \f0%s \f2%s\n\f1[HELP] Description: \f2%s",
                            args, _manpages[i]->args, _manpages[i]->help);
                    }
                    else formatstring(msg)("\f1[HELP] \f3Internal system error");

                    found = true;
                    break;
                }
            }
            if(found) break;
        }

        if(!found)
        {
            if(++searchc < 3 && _readmanfile(args)) goto _search;
            else formatstring(msg)("\f1[HELP] \f2Help page for command \f0%s \f2was not found.", args);
        }

		sendf(ci->ownernum, 1, "ris" , N_SERVMSG, msg);
    }

    void _showgbans(const char *cmd, const char *args, clientinfo *ci)
    {
        union { uchar b[sizeof(enet_uint32)]; enet_uint32 i; } ip, mask;
        string msg;
        if(!ci) return;
        int sender = ci->ownernum;
        sendf(sender, 1, "ris", N_SERVMSG, "\f3gbans list:");
        loopv(gbans)
        {
            int x = 0;
            ip.i = gbans[i].ip;
            mask.i = gbans[i].mask;
            while(x < 4 && mask.b[x]) x++;
            formatstring(msg)("%i.%i.%i.%i/%i",
                int(ip.b[0]), int(ip.b[1]), int(ip.b[2]), int(ip.b[3]), x << 3);
            sendf(sender, 1, "ris", N_SERVMSG, msg);
        }
    }

    void _spectate(clientinfo *ci, bool val)
    {
        if(!ci || (ci->state.state==CS_SPECTATOR ? val : !val)) return;
        if(ci->state.state!=CS_SPECTATOR && val)
        {
            if(ci->state.state==CS_ALIVE) suicide(ci);
            if(smode) smode->leavegame(ci);
            ci->state.state = CS_SPECTATOR;
            ci->state.timeplayed += lastmillis - ci->state.lasttimeplayed;
            if(!ci->privilege) aiman::removeai(ci);
        }
        else if(ci->state.state==CS_SPECTATOR && !val)
        {
            ci->state.state = CS_DEAD;
            ci->state.respawn();
            ci->state.lasttimeplayed = lastmillis;
            aiman::addclient(ci);
            if(ci->clientmap[0] || ci->mapcrc) checkmaps();
        }
        sendf(-1, 1, "ri3", N_SPECTATOR, ci->clientnum, val);
        if(!val && !hasmap(ci)) rotatemap(true);
    }

    void _forcespectator(clientinfo *ci, int spec)
    {
        string msg;
        if(ci)
        {
            if(ci->state.aitype == AI_NONE)
            {
                _spectate(ci, (spec));
                ci->_xi.forcedspectator = spec;
                formatstring(msg)("You are %sspectated%s", spec ? "\f3" : "\f0un", (spec == 1) ? " \f7for this match" : "");
                sendf(ci->ownernum, 1, "ris", N_SERVMSG, msg);
            }
        }
        else
        {
            loopv(clients) if(clients[i]->state.aitype == AI_NONE)  //skip bots
            {
                _spectate(clients[i], (spec));
                clients[i]->_xi.forcedspectator = spec;
            }
            formatstring(msg)("All players are %sspectated%s", spec ? "\f3" : "\f0un", (spec == 1) ? " \f7for this match" : "");
            sendf(-1, 1, "ris", N_SERVMSG, msg);
        }
    }

    void _spectfunc(const char *cmd, const char *args, clientinfo *ci)
    {
        int spec = (!cmd || !*cmd || !strcmp(cmd, "spectate") || !strcmp(cmd, "spec")) ? 1 : 0;

        if(!args || !*args) _forcespectator(0, spec);
        else
        {
            int cn = atoi(args);
            if(!cn && strcmp(args, "0"))
            {
                _man("usage", cmd, ci);
                return;
            }
            clientinfo *cx = (clientinfo *)getclientinfo(cn);
            if(!cx)
            {
                defformatstring(msg)("\f3Unknown client number \f0%i", cn);
                _notify(msg, ci);
                return;
            }
            _forcespectator(cx, spec);
        }
    }

    void _editmute(clientinfo *ci, int val)
    {
        string msg;
        if(ci)
        {
            ci->_xi.editmute = val;
            formatstring(msg)("Your editing is %smuted%s", val ? "\f3" : "\f0un", (val == 1) ? " \f7for this map" : "");
        }
        else
        {
            loopv(clients) clients[i]->_xi.editmute = val;
            formatstring(msg)("All players editing is %smuted%s", val ? "\f3" : "\f0un", (val == 1) ? " \f7for this map" : "");
        }
        if(!ci || ci->state.aitype == AI_NONE)
            sendf( ci ? ci->clientnum : -1, 1, "ris", N_SERVMSG, msg);
    }

    void _editmutefunc(const char *cmd, const char *args, clientinfo *ci)
    {
        int val = (!cmd || !*cmd || !strcmp(cmd, "editmute")) ? 1 : 0;
        if(!args || !*args) _editmute(0, val);
        else
        {
            string buf;
            char *argv[2];
            copystring(buf, args);
            _argsep(buf, 2, argv);
            int cn = atoi(argv[0]);
            if(!cn && strcmp(argv[0], "0"))
            {
                if(ci && !strcmp(argv[0], "me")) cn = ci->clientnum;
                else
                {
                    _man("usage", cmd, ci);
                    return;
                }
            }
            clientinfo *cx = getinfo(cn);
            if(!cx)
            {
                defformatstring(msg)("\f3Unknown client number \f0%i", cn);
                _notify(msg, ci);
                return;
            }
            if(argv[1] && *argv[1])
            {
                val = atoi(argv[1]);
                if(!val && strcmp(argv[1], "0")) val = 1;
            }
            _editmute(cx, val);
        }
    }

    void _mute(clientinfo *ci, int val)
    {
        string msg;
        if(ci)
        {
            ci->_xi.mute = val;
            formatstring(msg)("You are %smuted%s", val?"\f3":"\f0un", (val == 1)?" \f7for this match":"");
        }
        else
        {
            loopv(clients) if(clients[i]) clients[i]->_xi.mute = val;
            formatstring(msg)("All players are %smuted%s", val?"\f3":"\f0un", (val == 1)?" \f7for this match":"");
        }
        if(!ci || ci->state.aitype == AI_NONE)
            sendf(ci ? ci->clientnum : -1, 1, "ris", N_SERVMSG, msg);
    }

    void _mutefunc(const char *cmd, const char *args, clientinfo *ci)
    {
        int val = (!cmd || !*cmd || !strcmp(cmd, "mute")) ? 2 : 0;
        if(!args || !*args) _mute(0, val);
        else
        {
            string buf;
            char *argv[2];
            copystring(buf, args);
            _argsep(buf, 2, argv);
            int cn = atoi(argv[0]);
            if(!cn && strcmp(argv[0], "0"))
            {
                if(ci && !strcmp(argv[0], "me")) cn = ci->clientnum;
                else
                {
                    _man("usage", cmd, ci);
                    return;
                }
            }
            clientinfo *cx = getinfo(cn);
            if(!cx)
            {
                defformatstring(msg)("\f3Unknown client number \f0%i", cn);
                _notify(msg, ci);
                return;
            }
            if(argv[1] && *argv[1])
			{
				val = atoi(argv[1]);
				if(!val && strcmp(argv[1], "0")) val = 2;
			}
            _mute(cx, val);
        }
    }

    void _spy(clientinfo *ci, bool val)
    {

        if(!ci || (ci->_xi.spy ? val : !val) || ci->state.aitype != AI_NONE) return;
        ci->_xi.spy = val;
        if(val)
        {
            if(ci->state.state!=CS_SPECTATOR)
            {
                if(ci->state.state==CS_ALIVE) suicide(ci);
                if(smode) smode->leavegame(ci);
                ci->state.timeplayed += lastmillis - ci->state.lasttimeplayed;
            }
            aiman::removeai(ci);
            if(ci->privilege && !(serverhidepriv > 0 && ci->privilege >= (serverhidepriv == 1 ? PRIV_ADMIN : PRIV_MASTER) &&
                !(ci->privilege == PRIV_AUTH && ci->authname[0] && !ci->authdesc[0])))
            {
                //send out privileges
                packetbuf p(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
                defformatstring(tmp)("%s relinquished %s", colorname(ci), privname(ci->privilege));
                putint(p, N_SERVMSG);
                sendstring(tmp, p);
                _putmaster(p);
                sendpacket(-1, 1, p.finalize(), ci->clientnum);
                //send out same privileges but without message to spyer
                packetbuf q(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
                _putmaster(q);
                sendpacket(ci->clientnum, 1, q.finalize());
            }

            sendf(-1, 1, "rxi2", ci->clientnum, N_CDIS, ci->clientnum);

            sendf(ci->clientnum, 1, "ri3is", N_SPECTATOR, ci->clientnum, 1, N_SERVMSG, "\f1[SPY] \f0You entered spy mode");
        }
        else
        {
            if(ci->state.state!=CS_SPECTATOR)
            {
                ci->state.state = CS_DEAD;
                ci->state.respawn();
                ci->state.lasttimeplayed = lastmillis;
                aiman::addclient(ci);
                if(ci->clientmap[0] || ci->mapcrc) checkmaps();
                if(!hasmap(ci)) rotatemap(true);
            }
            //sendf(-1, 1, "rxi2ssi", ci->clientnum, N_INITCLIENT, ci->clientnum, ci->name, ci->team, ci->playermodel);
            sendinitclient(ci);
            sendresume(ci);
            sendf(-1, 1, "ri3", N_SPECTATOR, ci->clientnum, ci->state.state==CS_SPECTATOR ? 1 : 0);

            //send out privileges
            packetbuf p(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
            _putmaster(p);
            sendpacket(-1, 1, p.finalize());

            sendf(ci->clientnum, 1, "ris", N_SERVMSG, "\f1[SPY] \f0You left spy mode");
        }
    }

    void _spyfunc(const char *cmd, const char *args, clientinfo *ci)
    {
        string buf;
        char *argv[2];

        if(!args || !*args)
        {
            if(!ci) return;
            _spy(ci, !ci->_xi.spy);
        }
        else
        {
            copystring(buf, args);
            _argsep(buf, 2, argv);

            bool val = atoi(argv[0])!=0;

            if(argv[1] && *argv[1])
            {
                int cn = atoi(argv[1]);
                if(!cn && strcmp(argv[1], "0"))
                {
                    _man("usage", cmd, ci);
                    return;
                }
                clientinfo *cx = (clientinfo *)getclientinfo(cn);
                if(!cx)
                {
                    defformatstring(msg)("\f3Unknown client number \f0%i", cn);
                    _notify(msg, ci);
                    return;
                }
                _spy(cx, val);
            }
            else
            {
                if(!ci) return;
                _spy(ci, val);
            }
        }
    }

    struct _pluginfunc
    {
        char name[64];
        void *ptr;
    };

    vector<_pluginfunc *> _plfuncs; //plugin functions - intermodule communication

    struct _hookfunc
    {
        int (*func)(_hookparam *);
//      int priority;
    };

    struct _hookstruct
    {
        char name[16];
//      int num;
        vector<_hookfunc> funcs;
    };

    vector<_hookstruct *> _hookfuncs;

    int _exechook(const char *name)
    {
        bool found=false;
        int ret;

        if(!name || !name[0]) return -1;  //<0==fail
        //for(int i = 0; i < _hookfuncs.length(); i++)
        loopvrev(_hookfuncs)
        {
            if(_hookfuncs[i] && !strcmp(_hookfuncs[i]->name, name))
            {
                found = true;
                ret = 0;    //==0 - continue, ==1 - dont continue, ==-1 - error (dont continue)
                for(int j = 0; j < _hookfuncs[i]->funcs.length(); j++)
                {
                    ret = _hookfuncs[i]->funcs[j].func(&_hp);
                    if(ret) break;
                }
                break;
            }
        }
        return found ? ret : -1;
    }

    void _addhook(const char *name, int (*hookfunc)(_hookparam *))
    {
        _hookstruct *hs = 0;
        loopv(_hookfuncs) if(_hookfuncs[i] && !strcmp(_hookfuncs[i]->name, name))
        {
            hs = _hookfuncs[i];
            break;
        }
        if(!hs)
        {
            hs = new _hookstruct;
            if(!hs) return;
            strncpy(hs->name, name, 16);
            hs->funcs.add();
            hs->funcs[0].func = hookfunc;
            _hookfuncs.add(hs);
        }
        else
        {
            loopv(hs->funcs) if(hs->funcs[i].func == hookfunc) return;
            _hookfunc hf;
            hf.func = hookfunc;
            hs->funcs.add(hf);
        }
    }

    void _delhook(const char *name, int (*hookfunc)(_hookparam *))
    {
        loopv(_hookfuncs) if(_hookfuncs[i] && !strcmp(_hookfuncs[i]->name, name))
        {
            if(hookfunc)
            {
                loopj(_hookfuncs[i]->funcs.length()) if(_hookfuncs[i]->funcs[j].func == hookfunc)
                {
                    _hookfuncs[i]->funcs.remove(j);
                    break;
                }
            }
            else
            {
                delete _hookfuncs[i];
                _hookfuncs.removeunordered(i);
            }
            break;
        }
    }

    void _setext(char *s, void *ptr)
    {
        _pluginfunc *p = 0;
        for(int i = 0; i < _plfuncs.length(); i++)
        {
            if(_plfuncs[i] && !strcmp(s, _plfuncs[i]->name))
            {
                p = _plfuncs[i];
                break;
            }
        }
        if(!p)
        {
            p = new _pluginfunc;
            if(!p) return;  //fatal 0_o
            _plfuncs.add(p);
            strncpy(p->name, s, 64);
        }

        p->ptr = ptr;
    }

    void _testfunc()
    {
        sendf(-1, 1, "ris", N_SERVMSG, "[DEBUG] Plugin test function");
    }

    void * _getext(char *s)
    {
        if(!strcmp(s, "test")) return (void *)_testfunc;
        else if(!strcmp(s, "addhook")) return (void *)_addhook;
        else if(!strcmp(s, "delhook")) return (void *)_delhook;
        else if(!strcmp(s, "sendf")) return (void *)sendf;
        else if(!strcmp(s, "debug")) return (void *)_debug;
        else if(!strcmp(s, "notifypriv")) return (void *)_notifypriv;
        else if(!strcmp(s, "logoutf")) return (void *)logoutf;
        else
        {
            loopv(_plfuncs)
            {
                if(_plfuncs[i] && !strcmp(s, _plfuncs[i]->name) && _plfuncs[i]->ptr) return _plfuncs[i]->ptr;
            }
        }
        return 0;
    }

    struct _module
    {
#ifdef WIN32
        HINSTANCE h;
#else
        void *h;
#endif
        char name[64];
    };
    vector<_module> _modules;

    void _load(const char *cmd, const char *args, clientinfo *ci)
    {
        char *argv[2];
        string buf;
        bool needload;
        bool needunload;
        string fname;

        if(!args || !*args) return;

        needload = (!cmd || !*cmd || !strcmp(cmd, "load") || !strcmp(cmd, "reload"));
        needunload = (cmd && *cmd && (!strcmp(cmd, "reload") || !strcmp(cmd, "unload")));

        copystring(buf, args);

        _argsep(buf, 2, argv);

        char badchars[] = "/$%^&*()\\'\"`~";
        for(char *s = argv[0]; *s; s++)
            for(char *p = badchars; *p; p++)
                if(*s == *p)
                {
                    _notify("\f3[FAIL] Invalid module name", ci);
                    if(!ci) logoutf("\f3[FAIL] Invalid module name");
                    return;
                }

#ifdef WIN32
        formatstring(fname)("modules\\%s.dll", argv[0]);
#else
        formatstring(fname)("modules/lib%s.so", argv[0]);
#endif

        _module *m = 0;
        int mi;
        loopv(_modules) if(!strcmp(argv[0], _modules[i].name))
        {
            m = &_modules[i];
            mi = i;
            break;
        }
        if(!m)
        {
            if(!needload) return;
            mi = _modules.length();
            _modules.add();
            m = &_modules[mi];
            strncpy(m->name, argv[0], 64);
            m->h = 0;
        }

        if(m->h)
        {
            if(needload)
            {
                if(!needunload) return;
                typedef char *(* retinitfunctype)();
                char *(*reinitfunc)();
                //*(Z_LIBFUNC *)(&reinitfunc) = Z_GETSYM(m->h, "z_reinit");
                reinitfunc = (retinitfunctype)Z_GETSYM(m->h, "z_reinit");
                if(reinitfunc)
                {
                    char *ret;
                    //_debug("z_reinit found");
                    ret = reinitfunc();
                    if(ret)
                    {
                        defformatstring(msg)("\f1[WARN] \f3Plugin \f0%s \f3reinitialization function failed \f2(%s)", m->name, ret);
                        _notify(msg, ci, PRIV_ROOT);
                        logoutf("%s", msg);
                    }
                    else
                    {
                        needload = false;
                        needunload = false;
                    }
                }
            }
            if(needunload)
            {
                //_debug("unloading");
                char *(*uninitfunc)();
                typedef char *(* uninitfunctype)();
                //*(Z_LIBFUNC *)(&uninitfunc) = Z_GETSYM(m->h, "z_uninit");
                uninitfunc = (uninitfunctype)Z_GETSYM(m->h, "z_uninit");
                if(uninitfunc)
                {
                    char *ret;
                    //_debug("z_uninit found");
                    ret = uninitfunc();
                    if(ret)
                    {
                        defformatstring(msg)("\f1[WARN] \f3Plugin \f0%s \f3uninitialization function failed \f2(%s)", m->name, ret);
                        _notify(msg, ci, PRIV_ROOT);
                        logoutf("%s", msg);
                    }
                }
                Z_FREELIB(m->h);
                m->h = 0;
            }
        }

        if(needload)
        {
            m->h = Z_OPENLIB(fname);
            if(!m->h)
            {
                defformatstring(msg)("\f3[WARN] Plugin \f0%s \f3loading failed \f2(%s)", argv[0], z_liberror());
                _notify(msg, ci, PRIV_ROOT);
                logoutf("%s", msg);
                _modules.remove(mi);
                return;
            }

            char *(*initfunc)(void *, void *, char *);
            // *(Z_LIBFUNC *)(&initfunc) = Z_GETSYM(m->h, "z_init");
            typedef char *(* initfunctype)(void *, void *, char *);
            initfunc = (initfunctype)Z_GETSYM(m->h, "z_init");
            if(!initfunc)
            {
                defformatstring(msg)("\f3[FAIL] Plugin \f0%s \f3symbol \f0z_init \f3lookup failed \f2(%s)", argv[0], z_liberror());
                _notify(msg, ci, PRIV_ROOT);
                logoutf("%s", msg);
                Z_FREELIB(m->h);
                _modules.remove(mi);
                return;
            }

            //_debug("executing z_init");

            char *ret;
            ret = initfunc((void *)_getext, (void *)_setext, argv[1]);
            if(ret)
            {
                defformatstring(msg)("\f3[WARN] Plugin \f0%s \f3initialization function failed \f2(%s)", argv[0], ret);
                _notify(msg, ci, PRIV_ROOT);
                logoutf("%s", msg);
                Z_FREELIB(m->h);
                _modules.remove(mi);
                return;
            }
        }
        else if(!m->h)
        {
            _modules.remove(mi);
        }
    }

    void _setvar(const char *cmd, const char *args, clientinfo *ci)
	{
		if(!cmd || !cmd[0]) return;
        if(args && args[0]) setvar(cmd, atoi(args));
        else
		{
			defformatstring(msg)("%s = %i", cmd, getvar(cmd));
			if(ci) sendf(ci->clientnum, 1, "ris", N_SERVMSG, msg);
            else logoutf("%s", msg);
		}
	}

	void _setsvar(const char *cmd, const char *args, clientinfo *ci)
	{
		if(!cmd || !cmd[0]) return;
		setsvar(cmd, args ? args : "");
	}

    /*
    void _setvarfunc(const char *cmd, const char *args, clientinfo *ci)
    {
        string buf;
        char *argv[2];

    }
    */

    void _pm(const char *cmd, const char *args, clientinfo *ci)
    {
        char *argv[2];
        char *cns[32];
        int cnc;
        string buf;
        string msg;

        if(!ci) return;

        if(!args || !*args)
        {
            _man("usage", cmd, ci);
            return;
        }

        if(ci->_xi.mute)
        {
            sendf(ci->ownernum, 1, "ris", N_SERVMSG, "\f5[MUTE] \f3You are muted");
            return;
        }

        copystring(buf, args);

        _argsep(buf, 2, argv);

        if(!argv[1] || !*argv[1]) return;

        cnc = _argsep(argv[0], 32, cns, ',');

        vector<int> clientnums;
        loopi(cnc)
        {
            int j = atoi(cns[i]);
            if((j == 0 && *cns[i] != '0') || !getclientinfo(j))
            {
                if(ci)
                {
                    formatstring(msg)("\f3Unknown client number \"%s\"", cns[i]);
                    sendf(ci->clientnum, 1, "ris", N_SERVMSG, msg);
                    return;
                }
                else return;
            }
            bool exists = false;
            for(int k = clientnums.length() - 1; k >= 0; k--)
                if(j == clientnums[k]) { exists = true; break; }
            if(!exists) clientnums.add(j);
        }

        formatstring(msg)("\f1[PM:\f0%i\f1:\f7%s\f1] \f0%s", ci->clientnum, colorname(ci), argv[1]);

        loopvrev(clientnums) sendf(clientnums[i], 1, "ris", N_SERVMSG, msg);
    }

    void _setpriv(const char *cmd, const char *args, clientinfo *ci)
    {
        string buf;
        char *argv[2];
        int cn, privilege;
        clientinfo *cx;

        if(!args || !*args)
        {
            _man("usage", cmd, ci);
            return;
        }

        copystring(buf, args);
        _argsep(buf, 2, argv);

        if(!cmd || !*cmd || !strcmp(cmd, "setpriv") || !strcmp(cmd, "givepriv") || !strcmp(cmd, "priv"))
        {
            if(!*argv[0])
            {
                _man("usage", cmd, ci);
                return;
            }

            if(!argv[1] || !*argv[1])
            {
                if(!ci) return;
                cn = ci->clientnum;
                argv[1] = argv[0];
            }
            else
            {
                cn = atoi(argv[0]);
                if(!cn && strcmp(argv[0], "0"))
                {
                    _man("usage", cmd, ci);
                    return;
                }
            }

            privilege = atoi(argv[1]);
            if(!privilege && strcmp(argv[1], "0"))
            {
                if(!strcmp(argv[1], "none")) privilege = PRIV_NONE;
                else if(!strcmp(argv[1], "master")) privilege = PRIV_MASTER;
                else if(!strcmp(argv[1], "auth")) privilege = PRIV_AUTH;
                else if(!strcmp(argv[1], "admin")) privilege = PRIV_ADMIN;
                else if(!strcmp(argv[1], "root")) privilege = PRIV_ROOT;
                else
                {
                    _man("usage", cmd, ci);
                    return;
                }
            }
        }
        else if(!strcmp(cmd, "setadmin") || !strcmp(cmd, "giveadmin"))
        {
            privilege = PRIV_ADMIN;
            if(!*argv[0])
            {
                _man("usage", cmd, ci);
                return;
            }
            cn = atoi(argv[0]);
            if(!cn && strcmp(argv[0], "0"))
            {
                _man("usage", cmd, ci);
                return;
            }
        }
        else if(!strcmp(cmd, "setmaster") || !strcmp(cmd, "givemaster"))
        {
            privilege = PRIV_MASTER;
            if(!*argv[0])
            {
                _man("usage", cmd, ci);
                return;
            }
            cn = atoi(argv[0]);
            if(!cn && strcmp(argv[0], "0"))
            {
                _man("usage", cmd, ci);
                return;
            }
        }
        else if(!strcmp(cmd, "takepriv"))
        {
            privilege = PRIV_NONE;
            if(*argv[0])
            {
                if(!ci) return;
                cn = ci->clientnum;
            }
            else
            {
                cn = atoi(argv[0]);
                if(!cn && strcmp(argv[0], "0"))
                {
                    _man("usage", cmd, ci);
                    return;
                }
            }
        }
        else
        {
            _notify("\f2[DEBUG] This function isn't implemented yet", ci);
            return;
        }

        cx = (clientinfo *)getclientinfo(cn);
        if(!cx)
        {
            defformatstring(msg)("\f3Unknown client number \f0%i", cn);
            _notify(msg, ci);
            return;
        }

        if(_getpriv(ci)>=PRIV_ROOT ||
            ((_getpriv(ci)>_getpriv(cx) || cx==ci) &&
            (privilege>=0) &&
            (cx->privilege!=privilege) &&
            (_getpriv(ci)>=privilege)))
        {
            bool washidden = cx->_xi.spy || (serverhidepriv > 0 && cx->privilege >= (serverhidepriv == 1 ? PRIV_ADMIN : PRIV_MASTER) &&
                !(cx->privilege == PRIV_AUTH && cx->authname[0] && !cx->authdesc[0]));
            bool ishidden = cx->_xi.spy || (serverhidepriv && privilege >= (serverhidepriv == 1 ? PRIV_ADMIN : PRIV_MASTER));
            int oldpriv = cx->privilege;

            defformatstring(msg)("%s %s %s%s", colorname(cx), privilege?"claimed":"relinquished",
                                 privname(privilege ? privilege : cx->privilege), !(ishidden || (oldpriv && washidden && !privilege)) ? "" : " \f1(hidden)");
            cx->privilege = privilege;

            //if was gauther, unset authname, so it will be no longer identified as claimed as gauther
            if(cx->authname[0] && !cx->authdesc[0]) cx->authname[0] = '\0';

            logoutf("%s", msg);

            bool hasmaster = false;
            loopv(clients) if(clients[i]->privilege >= PRIV_MASTER) { hasmaster = true; break; }
            if(!hasmaster)
            {
                mastermode = defaultmastermode;
                if(mastermode < MM_PRIVATE) allowedips.shrink(0);
            }

            //if((!ishidden || oldpriv) && !cx->_xi.spy)
            if((privilege && !ishidden && ((oldpriv && washidden) || (!oldpriv && !washidden))) || (oldpriv && !washidden))
            {
                packetbuf p(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
                putint(p, N_SERVMSG);
                if(washidden)
                {
                    defformatstring(tmp)("%s %s %s", colorname(cx), "claimed", privname(privilege));
                    sendstring(tmp, p);
                }
                else if(ishidden)
                {
                    defformatstring(tmp)("%s %s %s", colorname(cx), "relinquished", privname(oldpriv));
                    sendstring(tmp, p);
                }
                else sendstring(msg, p);

                _putmaster(p);
                ENetPacket *_p = p.finalize();
                if(!(ishidden || (oldpriv && washidden))) sendpacket(-1, 1, _p);
                else
                {
                    loopv(clients) if(clients[i]->state.aitype == AI_NONE && clients[i] != ci && clients[i] != cx)
                        sendpacket(clients[i]->ownernum, 1, _p);
                }
            }

            if(ishidden || (oldpriv && washidden))
            {
                packetbuf q(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
                putint(q, N_SERVMSG);
                sendstring(msg, q);
                _putmaster(q);
                ENetPacket *_q = q.finalize();
                sendpacket(cx->ownernum, 1, _q);
                if(ci && ci != cx) sendpacket(ci->ownernum, 1, _q);
            }

            checkpausegame();
        }
        else _privfail(ci);
    }

    void _exec(const char *cmd, const char *args, clientinfo *ci)
    {
        if(!args || !*args)
        {
            _man("usage", cmd, ci);
            return;
        }
        defformatstring(msg)("\f1[EXEC] \f0%s", args);
        _notify(msg, ci, PRIV_ADMIN);
        execute(args);
    }

    void _np(const char *cmd, const char *args, clientinfo *ci)
    {
        string msg;
        if(!ci) return;
        if(!m_teammode || !ci->_xi.tkiller)
        {
            sendf(ci->clientnum, 1, "ris", N_SERVMSG, "no teamkills to forgive");
            return;
        }
        ci->_xi.tkiller->state.teamkills--;
        addteamkill(ci->_xi.tkiller, ci, -1);
        if(ci->_xi.tkiller->state.aitype == AI_NONE)
        {
            formatstring(msg)("\f3[teamkill] \f7%s \f0forgave your teamkill", colorname(ci));
            sendf(ci->_xi.tkiller->clientnum, 1, "ris", N_SERVMSG, msg);
        }
        formatstring(msg)("\f3[teamkill] \f7%s \f0teamkill forgiven", colorname(ci->_xi.tkiller));
        sendf(ci->clientnum, 1, "ris", N_SERVMSG, msg);
        ci->_xi.tkiller = 0;
    }

    void _interm(const char *cmd, const char *args, clientinfo *ci)
    {
        startintermission();
    }

    void _timefunc(const char *cmd, const char *args, clientinfo *ci)
    {
        string buf;
        char *argv[2];
        copystring(buf, args);
        _argsep(buf, 2, argv);

        if((!argv[0] || !argv[0][0]) && (!argv[1] || !argv[1][0]))
        {
            _man("usage", cmd, ci);
            return;
        }

        if(!m_timed || !smapname[0])
        {
            if(ci) sendf(ci->ownernum, 1, "ris", N_SERVMSG, "game mode is not timed");
            return;
        }

        int gl = 0;

        if(argv[0] && argv[0][0])
            gl += atoi(argv[0]) * 60000;

        if(argv[1] && argv[1][0])
            gl += atoi(argv[1]) * 1000;

        if(gl <= 1000 || gl > 48*60*60000 || gl + gamemillis <= 0 || gl + gamemillis + serverintermission*1000 <= 0)
        {
            if(!interm && gamemillis < gamelimit) startintermission();
        }
        else if(!interm && gamemillis < gamelimit)
        {
            gamelimit = gamemillis + gl;
            int mins = gl / 60000;
            int secs = (gl % 60000) / 1000;

            string msg;
            copystring(msg, "time remaining:");
            if(mins > 0)
            {
                defformatstring(buf)(" %i minute%s", mins, mins!=1 ? "s" : "");
                concatstring(msg, buf);
            }
            if(secs > 0)
            {
                defformatstring(buf)(" %i second%s", secs, secs!=1 ? "s" : "");
                concatstring(msg, buf);
            }

            sendf(-1, 1, "ri2is", N_TIMEUP, max((gamelimit - gamemillis)/1000, 1), N_SERVMSG, msg);
        }
    }

    void _rename(clientinfo *ci, const char *name, bool broadcast = true)
    {
        uchar buf[MAXSTRLEN];
        //prepare packet
        ucharbuf b(buf, MAXSTRLEN);
        putint(b, N_SWITCHNAME);
        sendstring(name, b);
        //broadcast to other clients
        if(broadcast) ci->messages.put(buf, b.len);
        //prepare packet for client itself
        packetbuf p(MAXSTRLEN, ENET_PACKET_FLAG_RELIABLE);
        putint(p, N_CLIENT);
        putint(p, ci->clientnum);
        //put length of packet
        putint(p, b.len);
        //put packet itself
        p.put(buf, b.len);
        //send to owner, because ci->messages doesn't do it
        sendpacket(ci->ownernum, 1, p.finalize());
    }

    void _renamefunc(const char *cmd, const char *args, clientinfo *ci)
    {
        string buf;
        char *argv[2];
        copystring(buf, args);
        _argsep(buf, 2, argv);
        int cn = atoi(argv[0]);
        if(!cn && strcmp(argv[0], "0"))
        {
            if(ci) _man("usage", cmd, ci);
            return;
        }
        clientinfo *cx = getinfo(cn);
        if(!cx)
        {
            _notify("client with such client number not found", ci);
            return;
        }

        if(argv[1])
        {
            filtertext(cx->name, argv[1], false, MAXNAMELEN);
            if(!cx->name[0]) copystring(cx->name, "unnamed");
        }
        else copystring(cx->name, "unnamed");

        _rename(cx, cx->name, true);
    }

    void _namemutefunc(const char *cmd, const char *args, clientinfo *ci)
    {
        string buf;
        char *argv[2];
        bool val;

        copystring(buf, args);
        _argsep(buf, 2, argv);

        val = strcmp(cmd, "unnamemute") && strcmp(cmd, "nameunmute");

        int cn = atoi(argv[0]);
        if(!cn && strcmp(argv[0], "0"))
        {
            if(ci && !strcmp(argv[0], "me")) cn = ci->clientnum;
            else
            {
                if(ci) _man("usage", cmd, ci);
                return;
            }
        }
        clientinfo *cx = getinfo(cn);
        if(!cx)
        {
            _notify("client with such client number not found", ci);
            return;
        }

        if((argv[1] && argv[1][0]) || val)
        {
            if(argv[1] && argv[1][0]) filtertext(cx->name, argv[1], false, MAXNAMELEN);
            if(!argv[1] || !argv[1][0] || !cx->name[0]) copystring(cx->name, "unnamed");
            _rename(cx, cx->name, true);
        }

        defformatstring(msg)("your rename was %smuted", val ? "" : "un");
        if(cx->state.aitype == AI_NONE) sendf(cx->clientnum, 1, "ris", N_SERVMSG, msg);
        cx->_xi.namemute = val ? 1 : 0;
    }

    void _sendto(const char *cmd, const char *args, clientinfo *ci)
    {
        int cn;
        if(!m_edit)
        {
            if(ci) sendf(ci->clientnum, 1, "ris", N_SERVMSG, "\f3This is not edit mode");
            return;
        }
        cn = atoi(args);
        if(!cn && strcmp(args, "0"))
        {
            if(ci)
            {
                defformatstring(msg)("\f3Unknown client number \"%s\"", args);
                sendf(ci->clientnum, 1, "ris", N_SERVMSG, msg);
            }
            return;
        }
        if(cn < 0)
        {
            if(!mapdata)
            {
                if(ci) sendf(ci->clientnum, 1, "ris", N_SERVMSG, "no map to send");
                return;
            }
            loopv(clients) if(clients[i]->state.aitype==AI_NONE && (!ci || clients[i]->clientnum!=ci->clientnum)) _sendmap(NULL, clients[i]);
        }
        clientinfo *cx = (clientinfo *)getclientinfo(cn);
        if(!cx)
        {
            if(ci)
            {
                defformatstring(msg)("\f3Unknown client number \"%s\"", args);
                sendf(ci->clientnum, 1, "ris", N_SERVMSG, msg);
            }
            return;
        }
        _sendmap(ci, cx);
    }

    void _ban(const char *cmd, const char *args, clientinfo *ci)
    {
        string buf;
        char *argv[2];
        if(!args || !args[0]) return;
        copystring(buf, args);
        _argsep(buf, 2, argv);
        int cn = atoi(argv[0]);
        if(!cn && strcmp(argv[0], "0"))
        {
            if(ci) sendf(ci->ownernum, 1, "ris", N_SERVMSG, "\f3Such client number not found");
            else logoutf("_ban:%s isnt cn", argv[0]);
            return;
        }
        clientinfo *cx = (clientinfo *)getclientinfo(cn);
        uint ip = getclientip(cn);
        if(!cx || !ip)
        {
            if(ci) sendf(ci->clientnum, 1, "ris", N_SERVMSG, "\f3Such client number not found");
            else logoutf("_ban:no such cn");
            return;
        }

        if(_getpriv(ci) < PRIV_ROOT && _getpriv(ci) < _getpriv(cx))
        {
            _privfail(ci);
            return;
        }

        int t;

        const char *address = getclienthostname(cn);

        defformatstring(msg)("\f3[BAN] \f7%s \f1is banned for \f7%s",
                             address ? address : "(null)",
                             (argv[1]&&argv[1][0]) ? argv[1] : "4h");

        if(argv[1] && argv[1][0])
        {
            filtertext(argv[1], argv[1], false);
            char *z = argv[1];
            while(*z && *z>='0' && *z<='9') z++;
            int m;
            switch(*z)
            {
                case 's': m = 1000; break;                          //seconds
                case 'm': case 'M': m = 60000; break;               //minutes
                case 'h': case 'H': case 0: m = 60*60000; break;    //hours
                case 'd': case 'D': m = 24*60*60000; break;         //days
                default:
                    if(ci) sendf(ci->clientnum, 1, "ris", N_SERVMSG, "\f3Unknown time specification");
                    else logoutf("_ban:unknown time %s", argv[1]);
                    return;
            }
            if(z == argv[1]) t = 1;
            else
            {
                *z = 0;
                t = atoi(argv[1]);
                if(!t)
                {
                    if(ci) sendf(ci->clientnum, 1, "ris", N_SERVMSG, "\f3Unknown time specification");
                    else logoutf("_ban:unknown time %s", argv[1]);
                    return;
                }
            }
            t *= m;
        }
        else t = 4*60*60000;

        sendservmsg(msg);

        addban(ip, t);
        kickclients(ip);
    }

    struct _kickvote
    {
        int cn, n;
        bool kicked;
    };
    void _checkvotekick(clientinfo *actor)
    {
        string msg, buf;
        vector <_kickvote> votes;
        votes.setsize(0);
        int nc = numclients(-1, false);
        static int timestat = 0;    //stats timeout
        static int timeout = 0;     //voting timeout
        static int timesugg = 0;    //suggestions timeout
        if(!timeout || totalmillis - timeout >= 2*60*1000)
        {
            timeout = totalmillis ? totalmillis : 1;    //timed out: clear all votekicks except current voter
            loopv(clients) if(clients[i] != actor) clients[i]->_xi.votekickvictim = -1;
        }
        if(nc < 5) return;  //dont check if less than 5 players
        loopv(clients) if(clients[i]->state.aitype == AI_NONE && clients[i]->_xi.votekickvictim >= 0)
        {
            int cn = clients[i]->_xi.votekickvictim;
            clientinfo *ci = (clientinfo *)getclientinfo(cn);
            if(!ci) //shouldnt happen, but lets be flexible xD
            {
                clients[i]->_xi.votekickvictim = -1;
                continue;
            }
            if(ci->privilege > PRIV_MASTER && ci->privilege > clients[i]->privilege) continue;
            bool found = false;
            loopj(votes.length()) if(votes[j].cn == cn)
            {
                votes[j].n++;
                found = true;
                break;
            }
            if(found) continue;
            _kickvote &v = votes.add();
            v.cn = cn;
            v.n = 1;
            v.kicked = false;
        }
        loopv(votes) if(votes[i].n >= nc/2)
        {
            uint ip = getclientip(votes[i].cn);
            clientinfo *ci = (clientinfo *)getclientinfo(votes[i].cn);
            if(!ip || !ci) continue;
            formatstring(msg)("\f3[votekick] \f6votekick succeded for \f7%s \f5(%i)", ci->name, votes[i].cn);
            logoutf("%s", msg);
            sendf(-1, 1, "ris", N_SERVMSG, msg);
            addban(ip, 4*60*60000);
            kickclients(ip);
            votes[i].kicked = true;
        }

        if(!timestat || totalmillis - timestat >= 2000)  //display stats each 2 voting seconds
        {
            timestat = totalmillis ? totalmillis : 1;
            formatstring(msg)("\f3[votekick]");
            loopv(votes) if(!votes[i].kicked)
            {
                clientinfo *ci = (clientinfo *)getclientinfo(votes[i].cn);  // null isnt possible
                formatstring(buf)(" \f7%s \f5(%i) \f1(\f0%i\f1)", ci->name, votes[i].cn, votes[i].n);
                concatstring(msg, buf);
            }
            formatstring(buf)(" \f3(\f0%i \f2needed\f3)", nc/2);
            concatstring(msg, buf);
            sendf(-1, 1, "ris", N_SERVMSG, msg);

            if(!timesugg || totalmillis - timesugg >= 5000)  //display suggestions each 5 voting seconds
            {
                timesugg = totalmillis ? totalmillis : 1;
                sendf(-1, 1, "ris", N_SERVMSG, "\f3[votekick] \f2use \f0#votekick \f2to vote for kicking");
            }
        }
    }

    bool _votekick(clientinfo *ci, int victim)
    {
        string msg;
        if(!ci || ci->clientnum==victim) return false;
        int priv = _getpriv(ci);
        clientinfo *vinfo = (clientinfo *)getclientinfo(victim);
        if(!vinfo) return false;
        if(vinfo->privilege > PRIV_MASTER && vinfo->privilege > priv) return false;     //allow to votekick masters
        if(ci->_xi.votekickvictim != victim)
        {
            ci->_xi.votekickvictim = victim;
            formatstring(msg)("\f3[votekick] \f7%s \f2voted for kicking \f7%s \f5(%i)", colorname(ci), vinfo->name, victim);
            sendf(-1, 1, "rxis", victim, N_SERVMSG, msg);
        }
        _checkvotekick(ci);
        return true;
    }

    void _votekickfunc(const char *cmd, const char *args, clientinfo *ci)
    {
        int cn;
        if(!ci) return;
        if(!votekick)
        {
            sendf(ci->clientnum, 1, "ris", N_SERVMSG, "Votekick is disabled in this server");
            return;
        }
        if(!args || !*args || (!(cn = atoi(args)) && strcmp(args, "0")))
        {
            _man("usage", cmd, ci);
            return;
        }
        if(!_votekick(ci, cn)) sendf(ci->clientnum, 1, "ris", N_SERVMSG, "\f3[FAIL] Votekick failed");
    }

    void _stats(const char *cmd, const char *args, clientinfo *ci)
    {
        vector<clientinfo *> cns;
        char *argv[32];
        int cnc;
        string buf;
        char msg[MAXTRANS];

        if(args) copystring(buf, args);
        else buf[0]=0;

        if(args && strchr(args,',')) cnc = _argsep(buf, 32, argv, ',');
        else cnc = _argsep(buf, 32, argv);

        //parse clientnums
        loopi(cnc)
        {
            int cn = atoi(argv[i]);
            if(!cn && argv[i][0]!='0') continue;

            if(cn < 0)
            {
                cns.setsize(0);
                loopj(clients.length()) if(!clients[j]->_xi.spy) cns.add(clients[j]);
                break;
            }

            bool exists=false;
            loopvj(cns) if(cn == cns[j]->clientnum) { exists=true; break; }
            if(exists) continue;

            clientinfo *cx = getinfo(cn);
            if(cx && !cx->_xi.spy) cns.add(cx);
            else
            {
                formatstring(msg)("\f3Unknown client number \f0%i", cn);
                _notify(msg, ci);
                return;
            }
        }
        //no cns found?
        if(cns.empty())
        {
            if(ci) cns.add(ci);
            else if(cnc == 0)
            {
                loopv(clients) cns.add(clients[i]);
            }
        }

        //buf is no longer used

        int sendcn = ci ? ci->clientnum : -1;
        loopv(cns)
        {
            clientinfo *cx = cns[i];

            formatstring(msg)("\f0[stats:\f7%s\f0] \f1frags: \f0%i \f1deaths: \f0%i \f1suicides: \f0%i \f1kpd: \f0%.2f \f1acc: \f0%i%%",
                colorname(cx), cx->state.frags, cx->state.deaths, cx->state._suicides,
                (float(cx->state.frags)/float(max(cx->state.deaths, 1))), cx->state.damage*100/max(cx->state.shotdamage,1));

            if(m_teammode)
            {
                formatstring(buf)(" \f1tk: \f0%i", cx->state.teamkills);
                concatstring(msg, buf, MAXTRANS);

                if(m_ctf)
                {
                    formatstring(buf)(" \f1flags scored: \f0%i", cx->state.flags);
                    concatstring(msg, buf, MAXTRANS);

                    if(m_hold)
                    {
                        formatstring(buf)(" \f1taken: \f0%i", cx->state._stolen);
                        concatstring(msg, buf, MAXTRANS);
                    }
                    else if(!m_protect)
                    {
                        formatstring(buf)(" \f1stolen: \f0%i \f1returned: \f0%i", cx->state._stolen, cx->state._returned);
                        concatstring(msg, buf, MAXTRANS);
                    }
                }
                else if(m_collect)
                {
                    formatstring(buf)(" \f1skulls scored: \f0%i \f1stolen: \f0%i \f1returned: \f0%i",
                        cx->state.flags, cx->state._stolen, cx->state._returned);
                    concatstring(msg, buf, MAXTRANS);
                }
            }

            sendf(sendcn, 1, "ris", N_SERVMSG, msg);
        }
    }

    void _getip(const char *cmd, const char *args, clientinfo *ci)
    {
        string msg;
        if(!args || !*args)
        {
            _man("usage", cmd, ci);
            return;
        }
        int cn = atoi(args);
        if(!cn && strcmp(args, "0"))
        {
            _man("usage", cmd, ci);
            return;
        }
        clientinfo *cx = (clientinfo *)getclientinfo(cn);
        if(!cx)
        {
            formatstring(msg)("\f3Unknown client number \f0%i", cn);
            _notify(msg, ci);
            return;
        }
        //uint ip = getclientip(cx->clientnum);
        formatstring(msg)("\fs\f1[IP:\f0%i\f1:\f7%s\f1] \f0%s\fr", cn, colorname(cx), getclienthostname(cn));
        sendf(ci ? ci->clientnum : -1, 1, "ris", N_SERVMSG, msg);
    }

    void _info(const char *cmd, const char *args, clientinfo *ci)
    {
        string msg, buf;
        uint t, months, weeks, days, hours, minutes, seconds;

        copystring(msg,
            "\f5[INFO] \f7Cube 2: Sauerbraten \f2server modification \f7zeromod \f2(based on original server)"
            /*"\f5[INFO] \f7Contributors: \f0/dev/zero, ~Haytham"*/);

        sendf(ci ? ci->clientnum : -1, 1, "ris", N_SERVMSG, msg);

        if(serveradmin[0])
        {
            copystring(msg, "\f5[INFO] \f7Administrator(s): \f0");
            concatstring(msg, serveradmin);
            sendf(ci ? ci->clientnum : -1, 1, "ris", N_SERVMSG, msg);
        }

        copystring(msg, "\f5[INFO] \f7Architecture: \f0"
        /* Firstly determine OS */
#if !(defined(_WIN32) || defined(WIN32) || defined(WIN64) || defined(_WIN64))
        /* unix/posix compilant os */
#   if defined(__linux__) || defined(__linux) || defined(linux) || defined(__gnu_linux__)
            "GNU/Linux"
#   elif defined(__GNU__) || defined(__gnu_hurd__)
            "GNU/Hurd"
#   elif defined(__FreeBSD_kernel__) && defined(__GLIBC__)
            "GNU/FreeBSD"
#   elif defined(__FreeBSD__) || defined(__FreeBSD_kernel__)
            "FreeBSD"
#   elif defined(__OpenBSD__)
            "OpenBSD"
#   elif defined(__NetBSD__)
            "NetBSD"
#   elif defined(__sun) || defined(sun)
            "Solaris"
#   elif defined(__DragonFly__)
            "DragonFlyBSD"
#   elif defined(__MACH__)
#       if defined(__APPLE__)
            "Apple"
#       else
            "Mach"
#       endif
#   elif defined(__CYGWIN__)
            "Cygwin"
#   elif defined(__unix__) || defined(__unix) || defined(unix) || defined(_POSIX_VERSION)
            "UNIX"
#   else
            "unknown"
#   endif
#else
        /* Windows */
            "Windows"
#endif
            " "
        );

        concatstring(msg, (sizeof(void *) == 8) ? "x86_64" : "i386");

        concatstring(msg, "\n\f5[INFO] \f7Uptime:");

        t = totalsecs;
        months = t / (30*24*60*60);
        t = t % (30*24*60*60);
        weeks = t / (7*24*60*60);
        t = t % (7*24*60*60);
        days = t / (24*60*60);
        t = t % (24*60*60);
        hours = t / (60*60);
        t = t % (60*60);
        minutes = t / 60;
        t = t % 60;
        seconds = t;

        if(months)
        {
            formatstring(buf)(" %u month%s", months, months > 1 ? "s" : "");
            concatstring(msg, buf);
        }

        if(weeks)
        {
            formatstring(buf)(" %u week%s", weeks, weeks > 1 ? "s" : "");
            concatstring(msg, buf);
        }

        if(days)
        {
            formatstring(buf)(" %u day%s", days, days > 1 ? "s" : "");
            concatstring(msg, buf);
        }

        if(hours)
        {
            formatstring(buf)(" %u hour%s", hours, hours > 1 ? "s" : "");
            concatstring(msg, buf);
        }

        if(minutes)
        {
            formatstring(buf)(" %u minute%s", minutes, minutes > 1 ? "s" : "");
            concatstring(msg, buf);
        }

        if(seconds)
        {
            formatstring(buf)(" %u second%s", seconds, seconds > 1 ? "s" : "");
            concatstring(msg, buf);
        }

        sendf(ci ? ci->clientnum : -1, 1, "ris", N_SERVMSG, msg);
    }

    void _nodamagefunc(const char *cmd, const char *args, clientinfo *ci)
    {
        bool onlyask;
        string msg;

        if(!m_edit)
        {
            if(ci) sendf(ci->clientnum, 1, "ris", N_SERVMSG, "nodamage is only avaiable in coop edit mode (1)");
            else logoutf("nodamage is only avaiable in coop edit mode (1)");
            return;
        }

        onlyask = (!args || !args[0]);

        if(!onlyask)
        {
            int i = atoi(args);
            int j = clamp(i, 0, 2);
            if(j > disabledamage)
            {
                formatstring(msg)("nodamage %i is disabled in server configuration", i);
                if(ci) sendf(ci->clientnum, 1, "ris", N_SERVMSG, msg);
                else logoutf(msg);
                return;
            }
            _nodamage = j;
        }

        formatstring(msg)("nodamage %sabled%s", _nodamage ? "\f0en" : "\f4dis", _nodamage>1 ? " \f7for non-editmuted players" : "");
        if(!onlyask || ci) sendf((!onlyask || !ci) ? -1 : ci->clientnum, 1, "ris", N_SERVMSG, msg);
        else logoutf(msg);
    }

    void _persistfunc(const char *cmd, const char *args, clientinfo *ci)
    {
        string msg;
        if(args && args[0]) persist = clamp(atoi(args), 0, 2);
        formatstring(msg)("persistent teams %sabled%s", persist ? "\f0en" : "\f4dis", persist>1 ? " \f7for non-standard teams" : "");
        sendf((!args || !args[0]) && ci ? ci->clientnum : -1, 1, "ris", N_SERVMSG, msg);
    }

    void _autosendmapfunc(const char *cmd, const char *args, clientinfo *ci)
    {
        string msg;
        if(args && args[0]) autosendmap = clamp(atoi(args), 0, 2);
        if(autosendmap) formatstring(msg)("autosendmap set to %d mode", autosendmap);
        else copystring(msg, "autosendmap disabled");
        sendf((!args || !args[0]) && ci ? ci->clientnum : -1, 1, "ris", N_SERVMSG, msg);
    }

    void _haltfunc(const char *cmd, const char *args, clientinfo *ci)
    {
        quitserver = true;
    }

//  >>> Server internals

    static void _addfunc(const char *s, int priv, void (*_func)(const char *cmd, const char *args, clientinfo *ci))
    {
        char buf[MAXTRANS];
        int argc;
        char *argv[260];
        copystring(buf, s, MAXTRANS);
        argc = _argsep(buf, 260, argv);
        loopi(argc) if(argv[i]) _funcs.add(new _funcdeclaration(argv[i], priv, _func));
    }

    static void _enablefunc(const char *s, bool val)
    {
        char buf[MAXTRANS];
        int argc;
        char *argv[260];
        copystring(buf, s, MAXTRANS);
        argc = _argsep(buf, 260, argv);
        loopk(argc) if(argv[k])
        {
            loopv(_funcs) if(_funcs[i] && !strcmp(argv[k], _funcs[i]->name))
            {
                _funcs[i]->disabled = !val;
                break;
            }
        }
    }

    static void _addhiddenfunc(const char *s, int priv, void (*_func)(const char *cmd, const char *args, clientinfo *ci))
    {
        char buf[MAXTRANS];
        int argc;
        char *argv[260];
        copystring(buf, s, MAXTRANS);
        argc = _argsep(buf, 260, argv);
        loopi(argc) if(argv[i]) _funcs.add(new _funcdeclaration(argv[i], priv, _func, true));
    }

    void _initfuncs()
    {
        _addfunc("wall announce", PRIV_AUTH, _wall);
        _addfunc("help", 0, _man);
        _addhiddenfunc("man", 0, _man);
        _addfunc("info", 0, _info);
        _addhiddenfunc("version", 0, _info);
        _addfunc("pm", 0, _pm);
        _addfunc("exec", PRIV_ROOT, _exec);
        _addfunc("stats", 0, _stats);
        _addfunc("load reload unload", PRIV_ROOT, _load);
        _addfunc("getip", PRIV_ADMIN, _getip);
        _addfunc("priv setpriv setmaster givemaster", PRIV_MASTER, _setpriv);
        _addfunc("setadmin giveadmin", PRIV_ADMIN, _setpriv);
        _addfunc("spec spectate unspec unspectate", PRIV_MASTER, _spectfunc);
        _addfunc("mute unmute", PRIV_AUTH, _mutefunc);
        _addfunc("editmute editunmute", PRIV_MASTER, _editmutefunc);
        _addhiddenfunc("uneditmute", PRIV_MASTER, _editmutefunc);
        _addfunc("namemute nameunmute", PRIV_AUTH, _namemutefunc);
        _addhiddenfunc("unnamemute", PRIV_AUTH, _namemutefunc);
        _addfunc("spy", PRIV_ADMIN, _spyfunc);
        _addfunc("np", PRIV_NONE, _np);
        _addhiddenfunc("forgive fg", PRIV_NONE, _np);
        _addfunc("interm intermission", PRIV_MASTER, _interm);
        _addfunc("time", PRIV_MASTER, _timefunc);
        _addfunc("ban", PRIV_ADMIN, _ban);
        _addfunc("votekick", PRIV_NONE, _votekickfunc);
        _addfunc("sendto", PRIV_MASTER, _sendto);
        _addfunc("rename", PRIV_AUTH, _renamefunc);
        _addhiddenfunc("name", PRIV_AUTH, _renamefunc);
        _addfunc("listgbans", PRIV_MASTER, _showgbans);
        _addhiddenfunc("showgbans", PRIV_MASTER, _showgbans);
        _addfunc("nodamage", PRIV_MASTER, _nodamagefunc);
        _addfunc("persist", PRIV_MASTER, _persistfunc);
        _addfunc("autosendmap", PRIV_MASTER, _autosendmapfunc);
        _addfunc("halt", PRIV_ROOT, _haltfunc);
    }

    void _privfail(clientinfo *ci)
    {
        if(!ci) return;
        sendf(ci->ownernum, 1, "ris", N_SERVMSG, "\f3You do not have enough privileges to execute this command");
    }

    void _nocommand(const char *cmd, clientinfo *ci)
    {
        string msg;

        if(!ci || !cmd || !cmd[0]) return;

        if(commandchars[0]) formatstring(msg)("\f3Unknown command \"\f0%s\f3\". For a list of avaiable commands type \"\f0%chelp\f3\"", cmd, commandchars[0]);
        else formatstring(msg)("\f3Unknown command \"\f0%s\f3\". For a list of avaiable commands type \"\f3/servcmd help\f3\"", cmd);
        sendf(ci->ownernum, 1, "ris", N_SERVMSG, msg);
    }

    inline int _getpriv(clientinfo *ci)
    {
        return ci ? ci->privilege : PRIV_ROOT;
    }

    void _servcmd(const char *cmd, clientinfo *ci, char cmdchar = 0)
    {
        char *argv[2];
        string str;
        bool executed=false;

        if(!cmd || !cmd[0]) return;
        copystring(str, cmd);
        _argsep(str, 2, argv);
        filtertext(argv[0], argv[0]);

        loopv(_funcs) if(_funcs[i])
        {
            if(!strcmp(argv[0], _funcs[i]->name))
            {
                if(_funcs[i]->disabled && _getpriv(ci) < PRIV_ROOT)
                {
                    if(ci) sendf(ci->ownernum, 1, "ris", N_SERVMSG, "\f6This command is disabled");
                }
                else if(_getpriv(ci) >= _funcs[i]->priv)
                {
                    //execute function
                    _funcs[i]->func(argv[0], argv[1] ? argv[1] : "", ci);
                }
                else _privfail(ci);
                executed=true;
                break;
            }
        }
        if(!executed) _nocommand(argv[0], ci);
    }

    void _checktext(char *text, clientinfo *ci)
    {
        char buf[16];
        char *argv[2];
        int argc;

        if(!serversuggestnp || !ci || !ci->_xi.tkiller || !text[0]) return;
        copystring(buf, text, 16);
        argc = _argsep(buf, 2, argv);
        if(!argc) return;
        if((argv[0][0]=='n' || argv[0][0]=='N') && (argv[0][1]=='p' || argv[0][1]=='P')) goto _np;
        if(argc >= 2 && (argv[0][0]=='n' || argv[0][0]=='N') && (argv[1][0]=='p' || argv[1][0]=='P') && (argv[1][1]=='r' || argv[1][1]=='R')) goto _np;
        return;
    _np:
        string msg;
        if(commandchars[0])
        {
            formatstring(msg)("\f3[teamkill] \f7type \"\f0%cnp\f7\" to forgive %s teamkill",
                  commandchars[0], colorname(ci->_xi.tkiller));
        }
        else
        {
            formatstring(msg)("\f3[teamkill] \f7type \"\f0/servcmd np\f7\" to forgive %s teamkill", colorname(ci->_xi.tkiller));
        }
        sendf(ci->ownernum, 1, "ris", N_SERVMSG, msg);

    }

    ICOMMAND(zexec, "C", (char *cmd), _servcmd(cmd, 0));
    ICOMMAND(zload, "C", (char *modulename), _load("load", modulename, 0));
    ICOMMAND(zwall, "C", (char *message), _wall(0, message, 0));
    ICOMMAND(announce, "C", (char *message), _wall(0, message, 0));

// ****************************************************************************************

    void parsepacket(int sender, int chan, packetbuf &p)     // has to parse exactly each byte of the packet
    {
        if(sender<0 || p.packet->flags&ENET_PACKET_FLAG_UNSEQUENCED || chan > 2) return;
        char text[MAXTRANS];
        int type;
        clientinfo *ci = getinfo(sender), *cq = ci, *cm = ci;
        if(!ci) return; //zeromod: should never happen
        if(!ci->connected)
        {
            if(chan==0) return;
            else if(chan!=1) { disconnect_client(sender, DISC_MSGERR); return; }
            else while(p.length() < p.maxlen) switch(checktype(getint(p), ci))
            {
                case N_CONNECT:
                {
                    getstring(text, p);
                    filtertext(text, text, false, MAXNAMELEN);
                    if(!text[0]) copystring(text, "unnamed");
                    copystring(ci->name, text, MAXNAMELEN+1);
                    ci->playermodel = getint(p);

                    string password, authdesc, authname;
                    getstring(password, p, sizeof(password));
                    getstring(authdesc, p, sizeof(authdesc));
                    getstring(authname, p, sizeof(authname));

                    int disc = DISC_NUM;
                    int priv = PRIV_NONE;
                    if(serverpass[0])
                    {
                        if(!checkpassword(ci, serverpass, password)) disc = DISC_PASSWORD;
                        else disc = DISC_NONE;
                    }
                    else if(adminpass[0] && checkpassword(ci, adminpass, password))
                    {
                        disc = DISC_NONE;
                        priv = PRIV_ADMIN;
                    }

                    if(disc >= DISC_NUM) disc = allowconnect(ci/*, password*/);
                    if(disc)
                    {
                        if(disc == DISC_LOCAL || !serverauth[0] || strcmp(serverauth, authdesc) || !tryauth(ci, authname, authdesc))
                        {
                            disconnect_client(sender, disc);
                            return;
                        }
                        ci->connectauth = disc;
                    }
                    else
                    {
                        if(connected(ci) && priv > PRIV_NONE) setmaster(ci, true, "", NULL, NULL, priv, true);
                    }
                    break;
                }

                case N_AUTHANS:
                {
                    string desc, ans;
                    getstring(desc, p, sizeof(desc));
                    uint id = (uint)getint(p);
                    getstring(ans, p, sizeof(ans));
                    if(!answerchallenge(ci, id, ans, desc))
                    {
                        disconnect_client(sender, ci->connectauth);
                        return;
                    }
                    break;
                }

                case N_PING:
                    getint(p);
                    break;

                default:
                    disconnect_client(sender, DISC_MSGERR);
                    return;
            }
            return;
        }
        else if(chan==2)
        {
            receivefile(sender, p.buf, p.maxlen);
            return;
        }
        if(p.packet->flags&ENET_PACKET_FLAG_RELIABLE) reliablemessages = true;
        #define QUEUE_AI clientinfo *cm = cq;
        #define QUEUE_MSG { if(cm) while(curmsg<p.length()) cm->messages.add(p.buf[curmsg++]); }
        #define QUEUE_BUF(body) { \
            if(cm) \
            { \
                curmsg = p.length(); \
                { body; } \
            } \
        }
        #define QUEUE_INT(n) QUEUE_BUF(putint(cm->messages, n))
        #define QUEUE_UINT(n) QUEUE_BUF(putuint(cm->messages, n))
        #define QUEUE_STR(text) QUEUE_BUF(sendstring(text, cm->messages))
        int curmsg;
        while((curmsg = p.length()) < p.maxlen) switch(type = checktype(getint(p), ci))
        {
            case N_POS:     //TODO: anticheat
            {
                int pcn = getuint(p);
                p.get();
                uint flags = getuint(p);
                clientinfo *cp = getinfo(pcn);
                if(cp && pcn != sender && cp->ownernum != sender) cp = NULL;
                vec pos;
                loopk(3)
                {
                    int n = p.get(); n |= p.get()<<8; if(flags&(1<<k)) { n |= p.get()<<16; if(n&0x800000) n |= -1<<24; }
                    pos[k] = n/DMF;
                }
                loopk(3) p.get();
                int mag = p.get(); if(flags&(1<<3)) mag |= p.get()<<8;
                int dir = p.get(); dir |= p.get()<<8;
                float yaw = dir%360;
                float pitch = clamp(dir/360, 0, 180)-90;
                vec vel = vec(yaw*RAD, pitch*RAD).mul(mag/DVELF);
                if(flags&(1<<4))
                {
                    p.get(); if(flags&(1<<5)) p.get();
                    if(flags&(1<<6)) loopk(2) p.get();
                }
                if(cp)
                {
                    if(cp->state.state==CS_ALIVE || cp->state.state==CS_EDITING)
                    {
                        if(!m_edit && max(vel.magnitude2(), (float)fabs(vel.z)) >= 180)
                            cp->setexceeded();
                        cp->position.setsize(0);
                        while(curmsg<p.length()) cp->position.add(p.buf[curmsg++]);
                    }
                    if(smode && cp->state.state==CS_ALIVE) smode->moved(cp, cp->state.o, cp->gameclip, pos, (flags&0x80)!=0);
                    cp->state.o = pos;
                    cp->state.yaw = yaw;
                    cp->state.pitch = pitch;
                    cp->gameclip = (flags&0x80)!=0;
                }
                break;
            }

            case N_TELEPORT:
            {
                int pcn = getint(p), teleport = getint(p), teledest = getint(p);
                clientinfo *cp = getinfo(pcn);
                if(cp && pcn != sender && cp->ownernum != sender) cp = NULL;
                if(cp && (cp->state.state==CS_ALIVE || cp->state.state==CS_EDITING))
                {
                    flushclientposition(*cp);
                    sendf(-1, 0, "ri4x", N_TELEPORT, pcn, teleport, teledest, cp->ownernum);
                }
                break;
            }

            case N_JUMPPAD:
            {
                int pcn = getint(p), jumppad = getint(p);
                clientinfo *cp = getinfo(pcn);
                if(cp && pcn != sender && cp->ownernum != sender) cp = NULL;
                if(cp && (cp->state.state==CS_ALIVE || cp->state.state==CS_EDITING))
                {
                    cp->setpushed();
                    flushclientposition(*cp);
                    sendf(-1, 0, "ri3x", N_JUMPPAD, pcn, jumppad, cp->ownernum);
                }
                break;
            }

            case N_FROMAI:
            {
                int qcn = getint(p);
                if(qcn < 0) cq = ci;
                else
                {
                    cq = getinfo(qcn);
                    if(cq && qcn != sender && cq->ownernum != sender) cq = NULL;
                }
                break;
            }

            case N_EDITMODE:
            {
                int val = getint(p);
                if(!m_edit)
                {
                    if(val && gamemillis >= 10000)
                    {
                        disconnect_client(sender, DISC_MSGERR);
                        return;
                    }
                    break;
                }
                if(val ? ci->state.state!=CS_ALIVE && ci->state.state!=CS_DEAD : ci->state.state!=CS_EDITING) break;
                if(smode)
                {
                    if(val) smode->leavegame(ci);
                    else smode->entergame(ci);
                }
                if(val)
                {
                    ci->state.editstate = ci->state.state;
                    ci->state.state = CS_EDITING;
                    ci->events.setsize(0);
                    ci->state.rockets.reset();
                    ci->state.grenades.reset();
                }
                else ci->state.state = ci->state.editstate;
                QUEUE_MSG;
                break;
            }

            case N_MAPCRC:
            {
                getstring(text, p);
                int crc = getint(p);
                if(strcmp(text, smapname))
                {
                    if(ci->clientmap[0])
                    {
                        ci->clientmap[0] = '\0';
                        ci->mapcrc = 0;
                    }
                    else if(ci->mapcrc > 0) ci->mapcrc = 0;
                    break;
                }
                copystring(ci->clientmap, text);
                ci->mapcrc = text[0] ? crc : 1;
                checkmaps();
                break;
            }

            case N_CHECKMAPS:
                checkmaps(sender);
                break;

            case N_TRYSPAWN:
                if(/*!ci || */!cq || cq->state.state!=CS_DEAD || ci->_xi.spy || cq->state.lastspawn>=0 || (smode && !smode->canspawn(cq))) break;
                if(!ci->clientmap[0] && !ci->mapcrc)
                {
                    ci->mapcrc = -1;
                    checkmaps();
                }
                if(cq->state.deadflush)
                {
                    flushevents(cq, cq->state.deadflush);
                    cq->state.respawn();
                }
                cleartimedevents(cq);
                sendspawn(cq);
                break;

            case N_GUNSELECT:
            {
                int gunselect = getint(p);
                if(!cq || cq->state.state!=CS_ALIVE) break;

                if(gunselect < GUN_FIST || gunselect > GUN_PISTOL)
                {
                    _cheater(cq, "gunhack::gunselect", AC_GUNHACK, 100);
                    break;
                }
                cq->state.gunselect = !m_insta || gunselect == GUN_FIST || gunselect == GUN_RIFLE ? gunselect : GUN_FIST;
                QUEUE_AI;
                QUEUE_INT(N_GUNSELECT);
                QUEUE_INT(cq->state.gunselect);
                break;
            }

            case N_SPAWN:
            {
                int ls = getint(p), gunselect = getint(p);
                if(!cq || (cq->state.state!=CS_ALIVE && cq->state.state!=CS_DEAD) || ls!=cq->state.lifesequence || cq->state.lastspawn<0 || cq->_xi.spy) break;
                cq->state.lastspawn = -1;
                cq->state.state = CS_ALIVE;
                cq->state.gunselect = gunselect >= GUN_FIST && gunselect <= GUN_PISTOL ? gunselect : GUN_FIST;
                cq->exceeded = 0;
                if(smode) smode->spawned(cq);
                QUEUE_AI;
                QUEUE_BUF({
                    putint(cm->messages, N_SPAWN);
                    sendstate(cq->state, cm->messages);
                });
                break;
            }

            case N_SUICIDE:
            {
                if(cq) cq->addevent(new suicideevent);
                break;
            }

            case N_SHOOT:
            {
                shotevent *shot = new shotevent;
                shot->id = getint(p);
                shot->millis = cq ? cq->geteventmillis(gamemillis, shot->id) : 0;
                shot->gun = getint(p);
                loopk(3) shot->from[k] = getint(p)/DMF;
                loopk(3) shot->to[k] = getint(p)/DMF;
                int hits = getint(p);
                loopk(hits)
                {
                    if(p.overread()) break;
                    if(k > MAXRAYS)
                    {
                        loopj(7) getint(p);
                        continue;
                    }
                    hitinfo &hit = shot->hits.add();
                    hit.target = getint(p);
                    hit.lifesequence = getint(p);
                    hit.dist = getint(p)/DMF;
                    hit.rays = getint(p);
                    loopk(3) hit.dir[k] = getint(p)/DNF;
                }
                if(cq)
                {
                    cq->addevent(shot);
                    if(cq->state.hasammo(shot->gun, GUN_FIST)) cq->setpushed(); //DANGER may be used to hide cheat
                }
                else delete shot;
                break;
            }

            case N_EXPLODE:
            {
                explodeevent *exp = new explodeevent;
                int cmillis = getint(p);
                exp->millis = cq ? cq->geteventmillis(gamemillis, cmillis) : 0;
                exp->gun = getint(p);
                exp->id = getint(p);
                int hits = getint(p);
                loopk(hits)
                {
                    if(p.overread()) break;
                    if(k > 512)
                    {
                        loopj(7) getint(p);
                        continue;
                    }
                    hitinfo &hit = exp->hits.add();
                    hit.target = getint(p);
                    hit.lifesequence = getint(p);
                    hit.dist = getint(p)/DMF;
                    hit.rays = getint(p);
                    loopk(3) hit.dir[k] = getint(p)/DNF;
                }
                if(cq) cq->addevent(exp);
                else delete exp;
                break;
            }

            case N_ITEMPICKUP:
            {
                int n = getint(p);
                if(!cq) break;
                pickupevent *pickup = new pickupevent;
                pickup->ent = n;
                cq->addevent(pickup);
                break;
            }

            case N_TEXT:
            {
                string ftext;
                //QUEUE_AI;
                //QUEUE_MSG;
                getstring(text, p);
                if(!cq) break;

                if(totalmillis - ci->_xi.lastmsg >= 500) ci->_xi.msgnum = 0;
                else ci->_xi.msgnum = max(ci->_xi.msgnum + 1, ci->_xi.msgnum);
                ci->_xi.lastmsg = totalmillis;
                if(ci->_xi.msgnum >= 80)
                {
                    if(ci->_xi.msgnum < 100) sendf(sender, 1, "ris", N_SERVMSG, "\f3[ANTIFLOOD] N_TEXT was blocked");
                    break;
                }

                filtertext(ftext, text);
                _checktext(ftext, ci);
                register char firstchar = ftext[0];
                bool iscommand = false;
                for(int z = 0; commandchars[z]; z++) if(firstchar == commandchars[z])
                {
                    iscommand = true;
                    break;
                }
                if(iscommand)
                {
                    //cq->messages.drop();
                    if(strlen(text) >= MAXSTRLEN) break;
                    logoutf("%s: %s", colorname(cq), ftext);
                    _servcmd(&text[1], ci, firstchar);
                }
                else
                {
                    if(cq->_xi.spy)
                    {
                        //cq->messages.drop();
                        logoutf("%s: %s", colorname(cq), ftext);
                        //sendservmsgf("\f3[REMOTE:\f7%s\f3] \f7%s", cq->name, cq->clientnum, ftext);
                        defformatstring(pubmsg)("\f3[REMOTE:\f7%s\f3] \f7%s", cq->name, text);
                        defformatstring(spymsg)("\f3[REMOTE:\f7%s \f5(%i)\f3] \f7%s", cq->name, cq->clientnum, text);
                        loopv(clients) if(clients[i]->state.aitype == AI_NONE)
                            sendf(clients[i]->clientnum, 1, "ris", N_SERVMSG, (!clients[i]->_xi.spy) ? pubmsg : spymsg);
                    }
                    else if(ci->_xi.mute || cq->_xi.mute)
                    {
                        //cq->messages.drop();
                        sendf(sender, 1, "ris", N_SERVMSG, "\f5[MUTE] \f3You are muted");
                    }
                    else
                    {
                        logoutf("%s: %s", colorname(cq), ftext);
                        QUEUE_AI;
                        QUEUE_INT(N_TEXT);
                        QUEUE_STR(ftext);
                    }
                }
                break;
            }

            case N_SAYTEAM:
            {
                getstring(text, p);
                if(!cq) break;

                if(totalmillis - ci->_xi.lastmsg >= 500) ci->_xi.msgnum = 0;
                else ci->_xi.msgnum = max(ci->_xi.msgnum + 1, ci->_xi.msgnum);
                ci->_xi.lastmsg = totalmillis;
                if(ci->_xi.msgnum >= 80)
                {
                    if(ci->_xi.msgnum < 100) sendf(sender, 1, "ris", N_SERVMSG, "\f3[ANTIFLOOD] N_SAYTEAM was blocked");
                    break;
                }

                logoutf("%s <%s>: %s", colorname(cq), cq->_xi.spy ? "spychat" : cq->state.state!=CS_SPECTATOR ? cq->team : "spectator", text);
                _checktext(text, ci);
                if(cq->_xi.spy)
                {
                    defformatstring(msg)("\f0[REMOTECHAT:\f7%s \f5(%i)\f0] \f1%s", cq->name, cq->clientnum, text);
                    loopv(clients) if(clients[i]->_xi.spy)
                        sendf(clients[i]->ownernum, 1, "ris", N_SERVMSG, msg);
                    break;
                }
                //if(!m_teammode || !cq->team[0] || (ci->state.state==CS_SPECTATOR && !ci->local && !ci->privilege)) break;
                if(ci->_xi.mute || cq->_xi.mute)
                {
                    sendf(sender, 1, "ris", N_SERVMSG,
                          "\f5[MUTE] \f3You are muted");
                    break;
                }
                register bool isntspectator = (cq->state.state != CS_SPECTATOR);
                loopv(clients)
                {
                    clientinfo *t = clients[i];
                    //if(t==cq || t->state.state==CS_SPECTATOR || t->state.aitype != AI_NONE || strcmp(cq->team, t->team)) continue;
                    if(t==cq || t->_xi.spy || t->state.aitype != AI_NONE) continue;
                    if(isntspectator == (t->state.state==CS_SPECTATOR)) continue;
                    if(isntspectator && m_teammode && strcmp(cq->team, t->team)) continue;
                    sendf(t->clientnum, 1, "riis", N_SAYTEAM, cq->clientnum, text);
                }
                break;
            }

            case N_SWITCHNAME:
            {
                getstring(text, p);

                if(totalmillis - ci->_xi.lastmsg >= 500) ci->_xi.msgnum = 0;
                else ci->_xi.msgnum = max(ci->_xi.msgnum + 1, ci->_xi.msgnum);
                ci->_xi.lastmsg = totalmillis;
                if(ci->_xi.msgnum >= 80)
                {
                    if(ci->_xi.msgnum < 100) sendf(sender, 1, "ris", N_SERVMSG, "\f3[ANTIFLOOD] N_SWITCHNAME was blocked");
                    break;
                }

                if(ci->_xi.namemute)
                {
                    _rename(ci, ci->name, false);
                    sendf(sender, 1, "ris", N_SERVMSG, "your rename was muted");
                    break;
                }

                filtertext(ci->name, text, false, MAXNAMELEN);
                if(!ci->name[0]) copystring(ci->name, "unnamed");
                if(!ci->_xi.spy)
                {
                    QUEUE_INT(N_SWITCHNAME);
                    QUEUE_STR(ci->name);
                }
                break;
            }

            case N_SWITCHMODEL:
            {
                ci->playermodel = getint(p);

                if(totalmillis - ci->_xi.lastmsg >= 500) ci->_xi.msgnum = 0;
                else ci->_xi.msgnum = max(ci->_xi.msgnum + 1, ci->_xi.msgnum);
                ci->_xi.lastmsg = totalmillis;
                if(ci->_xi.msgnum >= 64)
                {
                    if(ci->_xi.msgnum < 80) sendf(sender, 1, "ris", N_SERVMSG, "\f3[ANTIFLOOD] N_SWITCHMODEL was blocked");
                    break;
                }

                if(!ci->_xi.spy) QUEUE_MSG;
                break;
            }

            case N_SWITCHTEAM:
            {
                getstring(text, p);

                if(totalmillis - ci->_xi.lastmsg >= 500) ci->_xi.msgnum = 0;
                else ci->_xi.msgnum = max(ci->_xi.msgnum + 1, ci->_xi.msgnum);
                ci->_xi.lastmsg = totalmillis;
                if(ci->_xi.msgnum >= 80)
                {
                    if(ci->_xi.msgnum < 160) sendf(sender, 1, "ris", N_SERVMSG, "\f3[ANTIFLOOD] N_SWITCHTEAM was blocked");
                    break;
                }

                filtertext(text, text, false, MAXTEAMLEN);
                if(m_teammode && text[0] && strcmp(ci->team, text) && (!smode || smode->canchangeteam(ci, ci->team, text)) && addteaminfo(text))
                {
                    if(ci->state.state==CS_ALIVE) suicide(ci);
                    copystring(ci->team, text);
                    aiman::changeteam(ci);
                    if(!ci->_xi.spy) sendf(-1, 1, "riisi", N_SETTEAM, sender, ci->team, ci->state.state==CS_SPECTATOR ? -1 : 0);
                    else sendf(sender, 1, "riisi", N_SETTEAM, sender, ci->team, -1);
                }
                break;
            }

            case N_MAPVOTE:
            {
                getstring(text, p);
                filtertext(text, text, false);
                int reqmode = getint(p);
                vote(text, reqmode, sender);
                break;
            }

            case N_ITEMLIST:
            {
                if(!notgotitems || (ci->state.state==CS_SPECTATOR && !ci->privilege) ||
                     strcmp(ci->clientmap, smapname)) { while(getint(p)>=0 && !p.overread()) getint(p); break; }
                int n;
                while((n = getint(p))>=0 && n<MAXENTS && !p.overread())
                {
                    server_entity se = { NOTUSED, 0, false };
                    while(sents.length()<=n) sents.add(se);
                    sents[n].type = getint(p);
                    if(canspawnitem(sents[n].type))
                    {
                        if(delayspawn(sents[n].type)) sents[n].spawntime = spawntime(sents[n].type);
                        else sents[n].spawned = true;
                    }
                }
                notgotitems = false;
                break;
            }

            case N_EDITF:              // coop editing messages
            case N_EDITT:
            case N_EDITM:
            case N_FLIP:
            case N_ROTATE:
            case N_REPLACE:
            case N_DELCUBE:
            {
                int size = server::msgsizelookup(type);
                if(size<=0) { disconnect_client(sender, DISC_MSGERR); return; }
                loopi(size-1) getint(p);
                if(cq && (ci->_xi.editmute || cq->_xi.editmute))
                {
                    if(!ci->_xi.editmutewarn || totalmillis - ci->_xi.editmutewarn >= 10000)
                    {
                        sendf(sender, 1, "ris", N_SERVMSG,
                              "\f5[MUTE] \f3Your editing is muted");
                        ci->_xi.editmutewarn = totalmillis ? totalmillis : 1;
                    }
                    break;
                }
                if(cq && !ci->_xi.spy && (ci != cq || ci->state.state!=CS_SPECTATOR)) { QUEUE_AI; QUEUE_MSG; }
                break;
            }

            case N_REMIP:
                if(/*!ci || */ci->_xi.editmute || ci->state.state==CS_SPECTATOR || ci->_xi.spy) break;
                if(totalmillis - ci->_xi.lastremip >= 1000) ci->_xi.remipnum = 0;
                else ci->_xi.remipnum = max(ci->_xi.remipnum + 1, ci->_xi.remipnum);
                ci->_xi.lastremip = totalmillis;
                if(ci->_xi.remipnum >= 10)
                {
                    if(ci->_xi.remipnum < 20) sendf(sender, 1, "ris", N_SERVMSG, "\f3[ANTIFLOOD] N_REMIP was blocked");
                    break;
                }
                QUEUE_MSG;
                break;

            case N_EDITENT:
            {
                int i = getint(p);
                loopk(3) getint(p);
                int type = getint(p);
                loopk(5) getint(p);
                if(/*!ci || */ci->state.state==CS_SPECTATOR || ci->_xi.spy) break;
                if(ci->_xi.editmute)
                {
                    if(!ci->_xi.editmutewarn || totalmillis - ci->_xi.editmutewarn >= 10000)
                    {
                        sendf(sender, 1, "ris", N_SERVMSG,
                              "\f5[MUTE] \f3Your editing is muted");
                        ci->_xi.editmutewarn = totalmillis ? totalmillis : 1;
                    }
                    break;
                }
                QUEUE_MSG;
                bool canspawn = canspawnitem(type);
                if(i<MAXENTS && (sents.inrange(i) || canspawnitem(type)))
                {
                    server_entity se = { NOTUSED, 0, false };
                    while(sents.length()<=i) sents.add(se);
                    sents[i].type = type;
                    if(canspawn ? !sents[i].spawned : (sents[i].spawned || sents[i].spawntime))
                    {
                        sents[i].spawntime = canspawn ? 1 : 0;
                        sents[i].spawned = false;
                    }
                }
                break;
            }

            case N_EDITVAR:
            {
                int type = getint(p);
                getstring(text, p);
                switch(type)
                {
                    case ID_VAR: getint(p); break;
                    case ID_FVAR: getfloat(p); break;
                    case ID_SVAR: getstring(text, p);
                }
                if(ci && ci->_xi.editmute)
                {
                    if(!ci->_xi.editmutewarn || totalmillis - ci->_xi.editmutewarn >= 10000)
                    {
                        sendf(sender, 1, "ris", N_SERVMSG,
                              "\f5[MUTE] \f3Your editing is muted");
                        ci->_xi.editmutewarn = totalmillis ? totalmillis : 1;
                    }
                    break;
                }
                if(ci->state.state!=CS_SPECTATOR) QUEUE_MSG;
                break;
            }

            case N_PING:
                sendf(sender, 1, "i2", N_PONG, getint(p));
                break;

            case N_CLIENTPING:
            {
                int ping = getint(p);
                if(ci)
                {
                    ci->ping = ping;
                    loopv(ci->bots) ci->bots[i]->ping = ping;
                }
                if(!ci->_xi.spy) QUEUE_MSG;
                break;
            }

            case N_MASTERMODE:
            {
                int mm = getint(p);
                if(ci->privilege && mm>=MM_OPEN && mm<=MM_PRIVATE)
                {
                    if(ci->privilege>=PRIV_ADMIN || (mastermask&(1<<mm)))
                    {
                        mastermode = mm;
                        allowedips.shrink(0);
                        if(mm>=MM_PRIVATE)
                        {
                            loopv(clients) if(clients[i]->state.aitype==AI_NONE)
                                allowedips.add(getclientip(clients[i]->clientnum));
                        }
                        sendf(-1, 1, "rii", N_MASTERMODE, mastermode);
                        //sendservmsgf("mastermode is now %s (%d)", mastermodename(mastermode), mastermode);
                    }
                    else
                    {
                        defformatstring(s)("mastermode %d is disabled on this server", mm);
                        sendf(sender, 1, "ris", N_SERVMSG, s);
                    }
                }
                break;
            }

            case N_CLEARBANS:
            {
                if(ci->privilege)
                {
                    bannedips.shrink(0);
                    sendservmsg("cleared all bans");
                }
                break;
            }

            case N_KICK:
            {
                int victim = getint(p);
                getstring(text, p);
                filtertext(text, text);
                trykick(ci, victim, text);
                break;
            }

            case N_SPECTATOR:
            {
                int spectator = getint(p), val = getint(p);
                if(!ci->privilege && (spectator!=sender || (ci->state.state==CS_SPECTATOR && (mastermode>=MM_LOCKED || ci->_xi.forcedspectator)))) break;
                if(ci->_xi.spy && spectator == ci->clientnum)   //unspy spyer
                {
                    ci->state.state = (!val) ? CS_DEAD : CS_SPECTATOR;
                    _spy(ci, false);
                    break;
                }
                clientinfo *spinfo = (clientinfo *)getclientinfo(spectator); // no bots
                if(!spinfo || (spinfo->state.state==CS_SPECTATOR ? val : !val)) break;

                if(spinfo->state.state!=CS_SPECTATOR && val)
                {
                    if(spinfo->state.state==CS_ALIVE) suicide(spinfo);
                    if(smode) smode->leavegame(spinfo);
                    spinfo->state.state = CS_SPECTATOR;
                    spinfo->state.timeplayed += lastmillis - spinfo->state.lasttimeplayed;
                    if(!spinfo->privilege) aiman::removeai(spinfo);
                }
                else if(spinfo->state.state==CS_SPECTATOR && !val)
                {
                    spinfo->state.state = CS_DEAD;
                    spinfo->state.respawn();
                    spinfo->state.lasttimeplayed = lastmillis;
                    aiman::addclient(spinfo);
                    if(spinfo->clientmap[0] || spinfo->mapcrc) checkmaps();
                }
                sendf(-1, 1, "ri3", N_SPECTATOR, spectator, val);
                if(!val && !hasmap(spinfo)) rotatemap(true);
                break;
            }

            case N_SETTEAM:
            {
                int who = getint(p);
                getstring(text, p);

                filtertext(text, text, false, MAXTEAMLEN);
                if(!ci->privilege) break;
                clientinfo *wi = getinfo(who);
                if(!m_teammode || !text[0] || !wi || !strcmp(wi->team, text)) break;

                if(totalmillis - ci->_xi.lastmsg >= 500) ci->_xi.msgnum = 0;
                else ci->_xi.msgnum = max(ci->_xi.msgnum + 1, ci->_xi.msgnum);
                ci->_xi.lastmsg = totalmillis;
                if(ci->_xi.msgnum >= 160)
                {
                    if(ci->_xi.msgnum < 320) sendf(sender, 1, "ris", N_SERVMSG, "\f3[ANTIFLOOD] N_SETTEAM was blocked");
                    break;
                }

                if((!smode || smode->canchangeteam(wi, wi->team, text)) && addteaminfo(text))
                {
                    if(wi->state.state==CS_ALIVE) suicide(wi);
                    copystring(wi->team, text, MAXTEAMLEN+1);
                }
                aiman::changeteam(wi);
                sendf(-1, 1, "riisi", N_SETTEAM, who, wi->team, 1);
                break;
            }
#if 0
            case N_FORCEINTERMISSION:
                if(ci->local && !hasnonlocalclients()) startintermission();
                break;
#endif
            case N_RECORDDEMO:
            {
                int val = getint(p);
                if(ci->privilege < (restrictdemos ? PRIV_ADMIN : PRIV_MASTER)) break;
                if(!maxdemos || !maxdemosize)
                {
                    sendf(ci->clientnum, 1, "ris", N_SERVMSG, "the server has disabled demo recording");
                    break;
                }
                demonextmatch = clamp(val, 0, 1);
                sendservmsgf("demo recording is %s for next match", demonextmatch ? "enabled" : "disabled");
                break;
            }

            case N_STOPDEMO:
            {
                if(ci->privilege < (restrictdemos ? PRIV_ADMIN : PRIV_MASTER)) break;
                stopdemo();
                break;
            }

            case N_CLEARDEMOS:
            {
                int demo = getint(p);
                if(ci->privilege < (restrictdemos ? PRIV_ADMIN : PRIV_MASTER)) break;
                cleardemos(demo);
                break;
            }

            case N_LISTDEMOS:
                if(!ci->privilege && ci->state.state==CS_SPECTATOR) break;
                listdemos(sender);
                break;

            case N_GETDEMO:
            {
                int n = getint(p);
                if(!ci->privilege && ci->state.state==CS_SPECTATOR) break;
                senddemo(ci, n);
                break;
            }

            case N_GETMAP:
                if(!mapdata) sendf(sender, 1, "ris", N_SERVMSG, "no map to send");
                else if(ci->getmap) sendf(sender, 1, "ris", N_SERVMSG, "already sending map");
                else
                {
                    sendservmsgf("[%s is getting the map]", colorname(ci));
                    if((ci->getmap = sendfile(sender, 2, mapdata, "ri", N_SENDMAP)))
                        ci->getmap->freeCallback = freegetmap;
                    ci->needclipboard = totalmillis ? totalmillis : 1;
                }
                break;

            case N_NEWMAP:
            {
                int size = getint(p);
                if(totalmillis - ci->_xi.lastremip >= 1000) ci->_xi.remipnum = 0;
                else ci->_xi.remipnum = max(ci->_xi.remipnum + 1, ci->_xi.remipnum);
                ci->_xi.lastremip = totalmillis;
                if(ci->_xi.remipnum >= 10)
                {
                    if(ci->_xi.remipnum < 20) sendf(sender, 1, "ris", N_SERVMSG, "\f3[ANTIFLOOD] N_NEWMAP was blocked");
                    break;
                }
                if(ci->_xi.editmute)
                {
                    if(!ci->_xi.editmutewarn || totalmillis - ci->_xi.editmutewarn >= 10000)
                    {
                        sendf(sender, 1, "ris", N_SERVMSG,
                              "\f5[MUTE] \f3Your newmap was muted");
                        ci->_xi.editmutewarn = totalmillis ? totalmillis : 1;
                    }
                    break;
                }

                if(!ci->privilege && ci->state.state==CS_SPECTATOR) break;
                if(size>=0)
                {
                    smapname[0] = '\0';
                    resetitems();
                    notgotitems = false;
                    if(smode) smode->newmap();
                    loopv(clients)
                    {
                        clientinfo &cx = *clients[i];
                        if(cx._xi.mute && cx._xi.mute < 2) cx._xi.mute = 0;
                        if(cx._xi.editmute && cx._xi.editmute < 2) cx._xi.editmute = 0;
                        if(cx._xi.forcedspectator && cx._xi.forcedspectator < 2) cx._xi.forcedspectator = 0;
                    }
                }
                QUEUE_MSG;
                break;
            }

            case N_SETMASTER:
            {
                int mn = getint(p), val = getint(p);
                getstring(text, p);
                if(mn != ci->clientnum)
                {
                    if(!ci->privilege) break;
                    clientinfo *minfo = (clientinfo *)getclientinfo(mn);
                    if(!minfo || (minfo->privilege >= ci->privilege) || (val && minfo->privilege)) break;
                    setmaster(minfo, val!=0, "", NULL, NULL, PRIV_MASTER, true);
                }
                else setmaster(ci, val!=0, text);
                // don't broadcast the master password
                break;
            }

            case N_ADDBOT:
            {
                aiman::reqadd(ci, getint(p));
                break;
            }

            case N_DELBOT:
            {
                aiman::reqdel(ci);
                break;
            }

            case N_BOTLIMIT:
            {
                int limit = getint(p);
                if(ci) aiman::setbotlimit(ci, limit);
                break;
            }

            case N_BOTBALANCE:
            {
                int balance = getint(p);
                if(ci) aiman::setbotbalance(ci, balance!=0);
                break;
            }

            case N_AUTHTRY:
            {
                string desc, name;
                getstring(desc, p, sizeof(desc));
                getstring(name, p, sizeof(name));
                tryauth(ci, name, desc);
                break;
            }

            case N_AUTHKICK:
            {
                string desc, name;
                getstring(desc, p, sizeof(desc));
                getstring(name, p, sizeof(name));
                int victim = getint(p);
                getstring(text, p);
                filtertext(text, text);
                int authpriv = PRIV_AUTH;
                userinfo *u = NULL;
                if(desc[0]) u = users.access(userkey(name, desc));
                if(u) authpriv = u->privilege;
                else
                {
                    authpriv = masterauthprivilege(findauthmaster(desc));
                    if(!authpriv) break;
                }
                if(trykick(ci, victim, text, name, desc, authpriv, true) && tryauth(ci, name, desc))
                {
                    ci->authkickvictim = victim;
                    ci->authkickreason = newstring(text);
                }
                break;
            }

            case N_AUTHANS:
            {
                string desc, ans;
                getstring(desc, p, sizeof(desc));
                uint id = (uint)getint(p);
                getstring(ans, p, sizeof(ans));
                answerchallenge(ci, id, ans, desc);
                break;
            }

            case N_PAUSEGAME:
            {
                int val = getint(p);
                if(ci->privilege < (restrictpausegame ? PRIV_ADMIN : PRIV_MASTER)) break;
                pausegame(val > 0, ci);
                break;
            }

            case N_GAMESPEED:
            {
                int val = getint(p);
                if(ci->privilege < (restrictgamespeed ? PRIV_ADMIN : PRIV_MASTER)) break;
                changegamespeed(val, ci);
                break;
            }

            case N_COPY:
            {
                ci->cleanclipboard();
                ci->lastclipboard = totalmillis ? totalmillis : 1;

                int size = server::msgsizelookup(type);
                if(size<=0) { disconnect_client(sender, DISC_MSGERR); return; }
                loopi(size-1) getint(p);
                if(ci && ci->_xi.editmute)
                {
                    if(!ci->_xi.editmutewarn || totalmillis - ci->_xi.editmutewarn >= 10000)
                    {
                        sendf(sender, 1, "ris", N_SERVMSG,
                              "\f5[MUTE] \f3Your editing is muted");
                        ci->_xi.editmutewarn = totalmillis ? totalmillis : 1;
                    }
                    break;
                }
                if(ci && cq && (ci != cq || ci->state.state!=CS_SPECTATOR)) { QUEUE_AI; QUEUE_MSG; }
                break;
            }


            case N_PASTE:
            {
                if(ci->state.state!=CS_SPECTATOR) sendclipboard(ci);

                int size = server::msgsizelookup(type);
                if(size<=0) { disconnect_client(sender, DISC_MSGERR); return; }
                loopi(size-1) getint(p);
                if(ci && ci->_xi.editmute)
                {
                    if(!ci->_xi.editmutewarn || totalmillis - ci->_xi.editmutewarn >= 10000)
                    {
                        sendf(sender, 1, "ris", N_SERVMSG,
                              "\f5[MUTE] \f3Your editing is muted");
                        ci->_xi.editmutewarn = totalmillis ? totalmillis : 1;
                    }
                    break;
                }
                if(ci && cq && (ci != cq || ci->state.state!=CS_SPECTATOR)) { QUEUE_AI; QUEUE_MSG; }
                break;
            }

            case N_CLIPBOARD:
            {
                int unpacklen = getint(p), packlen = getint(p);
                ci->cleanclipboard(false);
                if(ci->state.state==CS_SPECTATOR)
                {
                    if(packlen > 0) p.subbuf(packlen);
                    break;
                }
                if(packlen <= 0 || packlen > (1<<16) || unpacklen <= 0)
                {
                    if(packlen > 0) p.subbuf(packlen);
                    packlen = unpacklen = 0;
                }
                packetbuf q(32 + packlen, ENET_PACKET_FLAG_RELIABLE);
                putint(q, N_CLIPBOARD);
                putint(q, ci->clientnum);
                putint(q, unpacklen);
                putint(q, packlen);
                if(packlen > 0) p.get(q.subbuf(packlen).buf, packlen);
                ci->clipboard = q.finalize();
                ci->clipboard->referenceCount++;
                break;
            }

            case N_SERVCMD:
            {
                string ftext;
                getstring(text, p);

                if(totalmillis - ci->_xi.lastmsg >= 500) ci->_xi.msgnum = 0;
                else ci->_xi.msgnum = max(ci->_xi.msgnum + 1, ci->_xi.msgnum);
                ci->_xi.lastmsg = totalmillis;
                if(ci->_xi.msgnum >= 160)
                {
                    if(ci->_xi.msgnum < 200) sendf(sender, 1, "ris", N_SERVMSG, "\f3[ANTIFLOOD] N_SERVCMD was blocked");
                    break;
                }

                if(/*!ci || */(strlen(text) > MAXSTRLEN)) break;
                filtertext(ftext, text);
                logoutf("N_SERVMSG:%s:%s", colorname(ci), ftext);
                _servcmd(text, ci);
                break;
            }

            #define PARSEMESSAGES 1
            #include "capture.h"
            #include "ctf.h"
            #include "collect.h"
            #undef PARSEMESSAGES

            case -1:
                logoutf("disconnect because unknown packet");
                disconnect_client(sender, DISC_MSGERR);
                return;

            case -2:
                disconnect_client(sender, DISC_OVERFLOW);
                return;

            default:
            {
                int size = server::msgsizelookup(type);
                if(size<=0) { disconnect_client(sender, DISC_MSGERR); return; }
                loopi(size-1) getint(p);
                if(ci && cq && (ci != cq || ci->state.state!=CS_SPECTATOR)) { QUEUE_AI; QUEUE_MSG; }
                break;
            }
        }
    }

    int laninfoport() { return SAUERBRATEN_LANINFO_PORT; }
    int serverinfoport(int servport) { return servport < 0 ? SAUERBRATEN_SERVINFO_PORT : servport+1; }
    int serverport(int infoport) { return infoport < 0 ? SAUERBRATEN_SERVER_PORT : infoport-1; }
    const char *defaultmaster() { return "sauerbraten.org"; }
    int masterport() { return SAUERBRATEN_MASTER_PORT; }
    int numchannels() { return 3; }

    #include "extinfo.h"

    void serverinforeply(ucharbuf &req, ucharbuf &p)
    {
        if(req.remaining() && !getint(req))
        {
            extserverinforeply(req, p);
            return;
        }

        putint(p, numclients(-1, false, true));
        putint(p, gamepaused || gamespeed != 100 ? 7 : 5);                   // number of attrs following
        putint(p, PROTOCOL_VERSION);    // generic attributes, passed back below
        putint(p, gamemode);
        putint(p, m_timed ? max((gamelimit - gamemillis)/1000, 0) : 0);
        putint(p, maxclients);
        putint(p, serverpass[0] ? MM_PASSWORD : (!m_mp(gamemode) ? MM_PRIVATE : (mastermode || mastermask&MM_AUTOAPPROVE ? mastermode : MM_AUTH)));
        if(gamepaused || gamespeed != 100)
        {
            putint(p, gamepaused ? 1 : 0);
            putint(p, gamespeed);
        }
        sendstring(smapname, p);
        sendstring(serverdesc, p);
        sendserverinforeply(p);
    }

    bool servercompatible(char *name, char *sdec, char *map, int ping, const vector<int> &attr, int np)
    {
        return attr.length() && attr[0]==PROTOCOL_VERSION;
    }

    #include "aiman.h"
}

