gcc -O0 -g -Wall -fPIC -c main.c -o geoip.o
gcc -O0 -g -shared geoip.o -lGeoIP -o libgeoip.so
