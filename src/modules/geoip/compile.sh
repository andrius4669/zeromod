cc -O2 -g -Wall -fPIC -c main.c -o geoip.o
cc -O2 -g -shared geoip.o -lGeoIP -o libgeoip.so
