gcc -Wall -fPIC -c test.c -o test.o
gcc -shared test.o -o test.so
