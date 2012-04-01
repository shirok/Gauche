/*
 * ndbm-makedb.c - create an empty ndbm database
 */

/* There are some variations in ndbm implementations that use different
   suffixes for database files.  I think the original ndbm inherited
   *.dir / *.pag from the original dbm, while BSD-traits uses *.db (they
   might be a wrapper of old berkeley db, though I'm not sure.)
   This small program is run during build stage and creates an empty
   ndbm database with the name given to the command line.  Then the build
   process finds out what files are actually created.
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include "dbmconf.h"

#if HAVE_NDBM_H
#include <ndbm.h>
#elif HAVE_GDBM_SLASH_NDBM_H
#include <gdbm/ndbm.h>
#elif HAVE_GDBM_MINUS_NDBM_H
#include <gdbm-ndbm.h>
#endif

int main(int argc, char **argv)
{
    DBM *dbf;

    if (argc != 2) {
        printf("Usage: ndbm-makedb <dbname>\n");
        exit(1);
    }

    if ((dbf = dbm_open(argv[1], O_CREAT|O_RDWR, 0777)) == NULL) {
        printf("dbm_open failed for %s: %s\n", argv[1], strerror(errno));
        exit(1);
    }

    dbm_close(dbf);
    return 0;
}
