/*
 * axTLS/ssl/test/ssltest.c heavily uses system() to run background process
 * to communicate, but it doesn't always work on some platforms (e.g.
 * OSX doesn't like calling system() from multiple threads).
 * So we provide an alternative.
 *
 * During the build, we preprocess ssltest.c to generate ssltest.mod.c,
 * and this file is included in the latter.
 *
 * Note that our system() doesn't return a value; ssltest.c doesn't check
 * the return value anyway.
 */

#include <errno.h>

void safe_system(const char *commands)
{
#if !defined(WIN32)
    pid_t pid;

    fprintf(stdout, "system: executing {%s}\n", commands);
    if ((pid = fork()) == 0) {
        execlp("sh", "sh", "-c", commands, NULL);
        fprintf(stdout, "system: couldn't invoke sh: %s\n", strerror(errno));
        exit(1);
    } else {
        int status;
        if (waitpid(pid, &status, 0) < 0) {
            fprintf(stdout, "waitpid failed on pid %d (%s)\n", pid,
                    strerror(errno));
            return;
        }
        if (status != 0) {
            fprintf(stdout, "process exit with %d (command: %s)\n",
                    status, commands);
        }
    }
#else  /*WIN32*/
    fprintf(stdout, "system: executing (%s)\n", commands);
    /* We know system() works on MinGW.  Just pretend that we honor the
       return value of system() so that the compiler won't complain.  */
    if (system(commands)) do {} while (0);
    /* This is needed to give time for kick_openssl to invoke openssl. */
    Sleep(200);
#endif /*WIN32*/
}

