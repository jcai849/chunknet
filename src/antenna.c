#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <R.h>
#include <Rinternals.h>

#define MAX_RECV_SIZE (1024*1024*128)
#define MAX_SEND_SIZE (1024*1024*128)
#define BACKLOG 10

SEXP receive_sexp(SEXP port)
{
    struct sockaddr_storage client_addr;
    socklen_t addr_size;
    struct addrinfo hints, *res;
    int addr_error;
    int sockfd, client_fd;
    unsigned int len = 0, i = 0;
    unsigned char *payload;
    int n, need;
    SEXP out;

    memset(&hints, 0, sizeof hints);
    hints.ai_flags = AI_PASSIVE;
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    addr_error = getaddrinfo(NULL, CHAR(STRING_ELT(port, 0)), &hints, &res);
    if (addr_error)
        Rf_error("%s\n", gai_strerror(addr_error));
    sockfd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
    if (sockfd == -1)
        Rf_error("%s\n", strerror(errno));
    if (bind(sockfd, res->ai_addr, res->ai_addrlen) == -1) {
        Rf_error("%s\n", strerror(errno));
        close(sockfd);
    }
    if (sockfd == 0)
        Rf_error("%s\n", strerror(errno));
    freeaddrinfo(res);
    (void) listen(sockfd, BACKLOG);
    addr_size = sizeof client_addr;
    client_fd = accept(sockfd, (struct sockaddr *) &client_addr, &addr_size);
    if (client_fd == -1)
        Rf_error("%s\n", strerror(errno));

    n = recv(sockfd, &len, sizeof len, 0);
#ifdef LARGERSCALE_DEBUG
    Rprintf("Receiving %d bytes from descriptor %d...\n", len, sockfd);
#endif
    if (n != sizeof(len) || len == 0) {
        close(sockfd);
        sockfd = -1;
        Rf_error("Header read error on descriptor %d", sockfd);
    } else {
        out = PROTECT(allocVector(RAWSXP, len));
        payload = RAW(out);
        while (i < len) {
            need = (len - i > MAX_RECV_SIZE) ? MAX_RECV_SIZE : (len - i);
            n = recv(sockfd, payload + i, need, 0);
            if (n < 0) {
                if (errno == EAGAIN || errno == EWOULDBLOCK) {
                    R_CheckUserInterrupt();
                    continue;
                }
                close(sockfd);
                sockfd = -1;
                Rf_error("Read error on descriptor %d: %s", sockfd, strerror(errno));
            } else if (n == 0) {
                close(sockfd);
                sockfd = -1;
                Rf_error("Connection closed on descriptor %d before all data was received", sockfd);
            }
            i += n;

        }
    }
    UNPROTECT(1);
    return out;
}

SEXP send_sexp(SEXP value, SEXP where, SEXP port)
{
    struct addrinfo hints, *res;
    int addr_error, retcode;
    int sockfd;
    SEXP fd;
    int need, n;
    unsigned int len = 0, i = 0;
    unsigned char *payload;


    addr_error = getaddrinfo(CHAR(STRING_ELT(where, 0)), CHAR(STRING_ELT(port, 0)), &hints, &res);
    if (addr_error)
        Rf_error("%s\n", gai_strerror(addr_error));
    sockfd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
    if (sockfd == -1)
        Rf_error("%s\n", strerror(errno));
    retcode = connect(sockfd, res->ai_addr, res->ai_addrlen);
    if (retcode == -1)
        Rf_error("%s\n", strerror(errno));

    len = LENGTH(value);
    payload = RAW(value);
#ifdef LARGERSCALE_DEBUG
    Rprintf("Sending %d bytes to descriptor %d...\n", len, sockfd);
#endif
    if (send(sockfd, &len, sizeof len, 0) != sizeof len) {
      close(sockfd);
      sockfd = -1;
      Rf_error("Failed to write header (descriptor=%d) %s", sockfd, strerror(errno));
    }
    while (i < len) {
        need = (len - i > MAX_SEND_SIZE) ? MAX_SEND_SIZE : (len - i);
        n = send(sockfd, payload + i, need, 0);
        if (n < 1) {
          close(sockfd);
          sockfd = -1;
          Rf_error("Failed to write (n=%d of %d, descriptor=%d) %s", n, need,
                   sockfd, (n == -1 && errno) ? strerror(errno) : "");
        }
        i += n;
    }

    fd = allocVector(INTSXP, 1);
    INTEGER(fd)[0] = sockfd;
    return fd;
}
