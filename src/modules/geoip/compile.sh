cc -O2 -Wall -fPIC -c main.c -o geoip.o
cc -O2 -shared geoip.o -lGeoIP -o libgeoip.so
