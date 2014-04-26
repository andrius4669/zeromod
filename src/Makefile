CXXFLAGS= -O3 -fomit-frame-pointer

override CXXFLAGS+= -Wall -fsigned-char -fno-exceptions -fno-rtti

# get platform name
PLATFORM= $(shell uname -s)
PLATFORM_PREFIX= native

INCLUDES= -Ishared -Iengine -Ifpsgame -Ienet/include

STRIP=
ifeq (,$(findstring -g,$(CXXFLAGS)))
ifeq (,$(findstring -pg,$(CXXFLAGS)))
  STRIP=strip
endif
endif

MV=mv

# we are compiling on windows?
ifneq (,$(findstring MINGW,$(PLATFORM)))
WINDRES= windres

# we are compiling on 64 bit windows?
ifneq (,$(findstring 64,$(PLATFORM)))

ifneq (,$(findstring CROSS,$(PLATFORM)))
  CXX=x86_64-w64-mingw32-g++
  WINDRES=x86_64-w64-mingw32-windres
ifneq (,$(STRIP))
  STRIP=x86_64-w64-mingw32-strip
endif
endif

# 64 bit windows compilation options
WINLIB=lib64
WINBIN=../bin64
override CXX+= -m64
override WINDRES+= -F pe-x86-64

else # ifneq (,$(findstring 64,$(PLATFORM)))

# we are running 32 bit windows
ifneq (,$(findstring CROSS,$(PLATFORM)))
  CXX=i686-w64-mingw32-g++
  WINDRES=i686-w64-mingw32-windres
ifneq (,$(STRIP))
  STRIP=i686-w64-mingw32-strip
endif
endif

# 32 bit compilation options
WINLIB=lib
WINBIN=../bin
override CXX+= -m32
override WINDRES+= -F pe-i386

endif # ifneq (,$(findstring 64,$(PLATFORM)))

ifneq (,$(findstring TDM,$(PLATFORM)))
STD_LIBS=
else
STD_LIBS= -static-libgcc -static-libstdc++
endif

endif # ifneq (,$(findstring MINGW,$(PLATFORM)))


ifneq (,$(findstring MINGW,$(PLATFORM)))
# platform is windows
SERVER_INCLUDES= -DSTANDALONE $(INCLUDES) -Iinclude
SERVER_LIBS= -mwindows $(STD_LIBS) -L$(WINBIN) -L$(WINLIB) -lzlib1 -lenet -lws2_32 -lwinmm

MASTER_INCLUDES= -DSTANDALONE -DMASTER $(INCLUDES) -Iinclude
MASTER_LIBS= $(STD_LIBS) -L$(WINBIN) -L$(WINLIB) -lzlib1 -lenet -lws2_32 -lwinmm
else
# platform is unix-like
SERVER_INCLUDES= -DSTANDALONE $(INCLUDES)
SERVER_LIBS= -Lenet -lenet -lz

MASTER_INCLUDES= -DSTANDALONE -DMASTER $(INCLUDES)
MASTER_LIBS= $(SERVER_LIBS)
endif

SERVER_OBJS= \
	shared/crypto-standalone.o \
	shared/stream-standalone.o \
	shared/tools-standalone.o \
	engine/command-standalone.o \
	engine/server-standalone.o \
	engine/worldio-standalone.o \
	fpsgame/entities-standalone.o \
	fpsgame/server-standalone.o \
	fpsgame/modules-standalone.o
# 	engine/world-standalone.o \
# 	engine/octa-standalone.o \
# 	engine/octaedit-standalone.o

MASTER_OBJS= \
	shared/crypto-master.o \
	shared/stream-master.o \
	shared/tools-master.o \
	engine/command-master.o \
	engine/master-master.o

# Operating System depended libraries
ifeq ($(PLATFORM),SunOS)
SERVER_LIBS+= -lsocket -lnsl
endif
ifeq ($(PLATFORM),Linux)
SERVER_LIBS+= -ldl
endif

default: all

all: server master

enet/libenet.a:
	$(MAKE)	-C enet
libenet: enet/libenet.a

clean-enet: enet/Makefile
	$(MAKE) -C enet clean

clean:
	-$(RM) $(SERVER_OBJS) $(MASTER_OBJS) sauer_server sauer_master

%-standalone.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $(subst -standalone.o,.cpp,$@)

%-master.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $(subst -master.o,.cpp,$@)

$(SERVER_OBJS): CXXFLAGS += $(SERVER_INCLUDES)
$(MASTER_OBJS): CXXFLAGS += $(MASTER_INCLUDES)

# compilation and instalation for windows
ifneq (,$(findstring MINGW,$(PLATFORM)))

server: $(SERVER_OBJS)
	$(WINDRES) -I vcpp -i vcpp/mingw.rc -J rc -o vcpp/mingw.res -O coff
	$(CXX) $(CXXFLAGS) -o $(WINBIN)/sauer_server.exe vcpp/mingw.res $(SERVER_OBJS) $(SERVER_LIBS)

master: $(MASTER_OBJS)
	$(CXX) $(CXXFLAGS) -o $(WINBIN)/sauer_master.exe $(MASTER_OBJS) $(MASTER_LIBS)

install: install_server install_master
install_server: server
install_master: master

# compilation and installation options for windows
else

server:	libenet $(SERVER_OBJS)
	$(CXX) $(CXXFLAGS) -o sauer_server $(SERVER_OBJS) $(SERVER_LIBS)  
	
master: libenet $(MASTER_OBJS)
	$(CXX) $(CXXFLAGS) -o sauer_master $(MASTER_OBJS) $(MASTER_LIBS)  

install: install_server install_master

install_server: server
	cp sauer_server	../bin_unix/$(PLATFORM_PREFIX)_server
ifneq (,$(STRIP))
	$(STRIP) ../bin_unix/$(PLATFORM_PREFIX)_server
endif

install_master: master
	cp sauer_master ../bin_unix/$(PLATFORM_PREFIX)_master
ifneq (,$(STRIP))
	$(STRIP) ../bin_unix/$(PLATFORM_PREFIX)_master
endif

endif


depend:
	makedepend -a -o-standalone.o -Y -DSTANDALONE -Ishared -Iengine -Ifpsgame $(subst -standalone.o,.cpp,$(SERVER_OBJS))
	makedepend -a -o-master.o -Y -DSTANDALONE -DMASTER -Ishared -Iengine -Ifpsgame $(subst -master.o,.cpp,$(MASTER_OBJS))

# DO NOT DELETE

# server objects dependencies
shared/crypto-standalone.o: shared/cube.h shared/tools.h shared/geom.h
shared/crypto-standalone.o: shared/ents.h shared/command.h shared/iengine.h
shared/crypto-standalone.o: shared/igame.h
shared/stream-standalone.o: shared/cube.h shared/tools.h shared/geom.h
shared/stream-standalone.o: shared/ents.h shared/command.h shared/iengine.h
shared/stream-standalone.o: shared/igame.h
shared/tools-standalone.o: shared/cube.h shared/tools.h shared/geom.h
shared/tools-standalone.o: shared/ents.h shared/command.h shared/iengine.h
shared/tools-standalone.o: shared/igame.h
engine/command-standalone.o: engine/engine.h shared/cube.h shared/tools.h
engine/command-standalone.o: shared/geom.h shared/ents.h shared/command.h
engine/command-standalone.o: shared/iengine.h shared/igame.h engine/world.h
engine/server-standalone.o: engine/engine.h shared/cube.h shared/tools.h
engine/server-standalone.o: shared/geom.h shared/ents.h shared/command.h
engine/server-standalone.o: shared/iengine.h shared/igame.h engine/world.h
engine/worldio-standalone.o: engine/engine.h shared/cube.h shared/tools.h
engine/worldio-standalone.o: shared/geom.h shared/ents.h shared/command.h
engine/worldio-standalone.o: shared/iengine.h shared/igame.h engine/world.h
fpsgame/entities-standalone.o: fpsgame/game.h shared/cube.h shared/tools.h
fpsgame/entities-standalone.o: shared/geom.h shared/ents.h shared/command.h
fpsgame/entities-standalone.o: shared/iengine.h shared/igame.h fpsgame/ai.h
fpsgame/server-standalone.o: fpsgame/game.h shared/cube.h shared/tools.h
fpsgame/server-standalone.o: shared/geom.h shared/ents.h shared/command.h
fpsgame/server-standalone.o: shared/iengine.h shared/igame.h fpsgame/ai.h
fpsgame/server-standalone.o: fpsgame/capture.h fpsgame/ctf.h
fpsgame/server-standalone.o: fpsgame/collect.h fpsgame/extinfo.h
fpsgame/server-standalone.o: fpsgame/aiman.h fpsgame/anticheat.h
fpsgame/modules-standalone.o: fpsgame/modules.h

# masterserver objects dependencies
engine/master-master.o: shared/cube.h shared/tools.h shared/geom.h
engine/master-master.o: shared/ents.h shared/command.h shared/iengine.h
engine/master-master.o: shared/igame.h
shared/crypto-master.o: shared/cube.h shared/tools.h shared/geom.h
shared/crypto-master.o: shared/ents.h shared/command.h shared/iengine.h
shared/crypto-master.o: shared/igame.h
shared/stream-master.o: shared/cube.h shared/tools.h shared/geom.h
shared/stream-master.o: shared/ents.h shared/command.h shared/iengine.h
shared/stream-master.o: shared/igame.h
shared/tools-master.o: shared/cube.h shared/tools.h shared/geom.h
shared/tools-master.o: shared/ents.h shared/command.h shared/iengine.h
shared/tools-master.o: shared/igame.h
engine/command-master.o: engine/engine.h shared/cube.h shared/tools.h
engine/command-master.o: shared/geom.h shared/ents.h shared/command.h
engine/command-master.o: shared/iengine.h shared/igame.h engine/world.h

# dont look at these
# engine/world-standalone.o: engine/engine.h shared/cube.h shared/tools.h shared/geom.h
# engine/world-standalone.o: shared/ents.h shared/command.h shared/iengine.h
# engine/world-standalone.o: shared/igame.h engine/world.h engine/octa.h engine/lightmap.h
# engine/world-standalone.o: engine/bih.h engine/texture.h engine/model.h engine/varray.h
# engine/octa-standalone.o: engine/engine.h shared/cube.h shared/tools.h shared/geom.h
# engine/octa-standalone.o: shared/ents.h shared/command.h shared/iengine.h shared/igame.h
# engine/octa-standalone.o: engine/world.h engine/octa.h engine/lightmap.h engine/bih.h
# engine/octa-standalone.o: engine/texture.h engine/model.h engine/varray.h
# engine/octaedit-standalone.o: engine/engine.h shared/cube.h shared/tools.h shared/geom.h
# engine/octaedit-standalone.o: shared/ents.h shared/command.h shared/iengine.h
# engine/octaedit-standalone.o: shared/igame.h engine/world.h engine/octa.h
# engine/octaedit-standalone.o: engine/lightmap.h engine/bih.h
# engine/octaedit-standalone.o: engine/model.h engine/varray.h
