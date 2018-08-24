#include <stdio.h>
#include <stdint.h>
#include <sodium.h>

// including cpp file, cause we will use its internal structures
// cube.h is included by it automatically
#include "crypto.cpp"

enum { AM_SECRETPASS = 0, AM_PRIVKEY, AM_RANDOMPASS };

void printhelp(const char *progname)
{
    printf("Usage: %s [-s] secretpass\n", progname);
    printf("       %s  -p  privatekey\n", progname);
    printf("       %s  -r\n", progname);
    printf("generates authkey pair for sauerbraten servers and clients\n");
    printf("  -h                   this help screen\n");
    printf("  -s secretpass        use secret password to generate key pair\n");
    printf("  -p privatekey        use private key to generate public key\n");
    printf("  -r                   generate random password and random key pair\n");
}

int main(int argc, char **argv)
{
    int mode = AM_SECRETPASS;
    int verbose = 1;
    int ignoreargs = false;
    string seed;

    memset(seed, 0, sizeof(seed));

    for(int i = 1; i < argc; i++)
    {
        if(argv[i][0]=='-' && !ignoreargs)
        {
            switch(argv[i][1])
            {
                case 's':
                    mode = AM_SECRETPASS;
                    if(argv[i][2]) copystring(seed, argv[i]+2);
                    break;

                case 'p':
                    mode = AM_PRIVKEY;
                    if(argv[i][2]) copystring(seed, argv[i]+2);
                    break;

                case 'r':
                    mode = AM_RANDOMPASS;
                    break;

                case 'q':
                    verbose--;
                    break;

                case 'v':
                    verbose++;
                    break;

                case 'h':
                    printhelp(argv[0]);
                    return 0;

                case '\0':
                    ignoreargs = true;
                    break;

                case '-':
                    if(argv[i][2] == '\0') { ignoreargs = true; break; }
                    else if(!strcmp(argv[i]+2, "help"))
                    {
                        printhelp(argv[0]);
                        return 0;
                    }
                    // fall through

                default:
                    fprintf(stderr, "%s: unrecognized option '%s'\n", argv[0], argv[i]);
                    printhelp(argv[0]);
                    return 1;
            }
        }
        else
        {
            if(!seed[0]) copystring(seed, argv[i]);
            else
            {
                if(mode!=AM_PRIVKEY) concatstring(seed, " ");
                concatstring(seed, argv[i]);
            }
        }
    }

    if(!seed[0] && mode!=AM_RANDOMPASS)
    {
        fprintf(stderr, "no %s specified!\n", mode==AM_SECRETPASS ? "secret password" : "private key");
        return 1;
    }

    if(!seed[0])
    {
        // generate random password

        if(sodium_init() < 0)
        {
            fprintf(stderr, "sodium_init() failed\n");
            return 1;
        }

        // randomly select password length
        uint32_t s = 16 + randombytes_uniform(8);
        // characters used in password
        static const char characters[] =
            "0123456789"
            "ABCDEFGHIJKLMNOPQRSTUVXYZ"
            "abcdefghijklmnopqrstuvxyz"
            "!#$%&*+,-./<=>?@_`";
        uint32_t sz = sizeof(characters)-1;
        char *p = seed;
        for(uint32_t i = 0; i < s; i++)
        {
            *p++ = characters[randombytes_uniform(sz)];
            *p++ = characters[randombytes_uniform(sz)];
        }
    }

    tiger::hashval hash;
    // private key is just a hash of password
    bigint<8*sizeof(hash.bytes)/BI_DIGIT_BITS> privkey;
    if(mode!=AM_PRIVKEY)
    {
        vector<char> privstr;
        tiger::hash((const uchar *)seed, (int)strlen(seed), hash);
        memcpy(privkey.digits, hash.bytes, sizeof(hash.bytes));
        sodium_memzero(hash.bytes, sizeof(hash.bytes));
        privkey.len = 8*sizeof(hash.bytes)/BI_DIGIT_BITS;
        privkey.shrink();
        privkey.printdigits(privstr);
        privstr.add('\0');
        if(mode==AM_RANDOMPASS || verbose>1) printf("%s%s\n", verbose>0 ? "secure pass: " : "", seed);
        sodium_memzero(seed, sizeof(seed));
        printf("%s%s\n", verbose>0 ? "private key: " : "", privstr.getbuf());
        sodium_memzero(privstr.getbuf(), privstr.length());
    }
    else
    {
        privkey.parse(seed);
        sodium_memzero(seed, sizeof(seed));
        if(verbose>1)
        {
            vector<char> privstr;
            privkey.printdigits(privstr);
            privstr.add('\0');
            printf("private key: %s\n", privstr.getbuf());
            sodium_memzero(privstr.getbuf(), privstr.length());
        }
    }

    vector<char> pubstr;
    ecjacobian c(ecjacobian::base);
    c.mul(privkey);
    sodium_memzero(privkey.digits, privkey.len);
    c.normalize();
    c.print(pubstr);
    pubstr.add('\0');
    printf("%s%s\n", verbose>0 ? "public key: " : "", pubstr.getbuf());

    return 0;
}
