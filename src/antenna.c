#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <pthread.h>
#include <R.h>
#include <Rinternals.h>
#include "queue.h"

#define MAX_RECV_SIZE (1024*1024*128)
#define MAX_SEND_SIZE (1024*1024*128)
#define BACKLOG 10

SEXP C_as_socket(int fd)
{
    SEXP sock, class;

    sock = PROTECT(allocVector(INTSXP, 1));
    INTEGER(sock)[0] = fd;
    class = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(class, 0, mkChar("Socket"));
    classgets(sock, class);
    UNPROTECT(2);
    return sock;
}

SEXP C_receive_sock(SEXP sock)
{
    int sockfd;
    unsigned int len = 0, i = 0;
    unsigned char *payload;
    int n, need;
    SEXP out;

    sockfd = INTEGER(sock)[0];
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

SEXP C_receive_loc(SEXP port)
{
    struct sockaddr_storage client_addr;
    socklen_t addr_size;
    struct addrinfo hints, *res;
    int addr_error;
    int sockfd, client_fd;
    SEXP fd, val, out;

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

    fd = PROTECT(C_as_socket(client_fd));
    val = PROTECT(C_receive_sock(fd));
    out = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(out, 0, fd);
    SET_VECTOR_ELT(out, 1, val);
    UNPROTECT(3);
    return out;
}

SEXP C_receive_and_enqueue(SEXP queue, SEXP port)
{
    struct sockaddr_storage client_addr;
    socklen_t addr_size;
    struct addrinfo hints, *res;
    int addr_error;
    int sockfd, client_fd;
    unsigned int len = 0, i = 0;
    unsigned char *payload;
    int n, need;
    SEXP payload_interface, fd_interface, out;

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

    n = recv(client_fd, &len, sizeof len, 0);
#ifdef LARGERSCALE_DEBUG
    Rprintf("Receiving %d bytes from descriptor %d...\n", len, client_fd);
#endif
    if (n != sizeof(len) || len == 0) {
        close(client_fd);
        client_fd = -1;
        Rf_error("Header read error on descriptor %d", client_fd);
    } else {
        payload_interface = PROTECT(allocVector(RAWSXP, len));
        payload = RAW(payload_interface);
        while (i < len) {
            need = (len - i > MAX_RECV_SIZE) ? MAX_RECV_SIZE : (len - i);
            n = recv(client_fd, payload + i, need, 0);
            if (n < 0) {
                if (errno == EAGAIN || errno == EWOULDBLOCK) {
                    R_CheckUserInterrupt();
                    continue;
                }
                close(client_fd);
                client_fd = -1;
                Rf_error("Read error on descriptor %d: %s", client_fd, strerror(errno));
            } else if (n == 0) {
                close(client_fd);
                client_fd = -1;
                Rf_error("Connection closed on descriptor %d before all data was received", client_fd);
            }
            i += n;

        }
    }

    fd_interface = PROTECT(C_as_socket(client_fd));
    out = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(out, 0, fd_interface);
    SET_VECTOR_ELT(out, 1, payload_interface);
    UNPROTECT(3);

    return C_push(queue, out);
}

void *async_listener()
{
    struct sockaddr_storage client_addr;
    socklen_t addr_size;
    struct addrinfo hints, *res;
    int addr_error;
    int sockfd, client_fd;

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

    while (1) {
        client_fd = accept(sockfd, (struct sockaddr *) &client_addr, &addr_size);
        if (client_fd == -1)
            Rf_error("%s\n", strerror(errno));
        push(client_queue, client_fd);
    }

}

void *async_receiver()
{
    int client_fd;
    unsigned int len = 0, i = 0;
    unsigned char *payload;
    int n, need;
    SEXP payload_interface, fd_interface, out;

    while (1) {
        client_fd = pop(client_queue);
        n = recv(client_fd, &len, sizeof len, 0);
        if (n != sizeof(len) || len == 0) {
            close(client_fd);
            client_fd = -1;
            Rf_error("Header read error on descriptor %d", client_fd);
        } else {
            payload_interface = PROTECT(allocVector(RAWSXP, len));
            payload = RAW(payload_interface);
            while (i < len) {
                need = (len - i > MAX_RECV_SIZE) ? MAX_RECV_SIZE : (len - i);
                n = recv(client_fd, payload + i, need, 0);
                if (n < 0) {
                    if (errno == EAGAIN || errno == EWOULDBLOCK) {
                        R_CheckUserInterrupt();
                        continue;
                    }
                    close(client_fd);
                    client_fd = -1;
                    Rf_error("Read error on descriptor %d: %s", client_fd, strerror(errno));
                } else if (n == 0) {
                    close(client_fd);
                    client_fd = -1;
                    Rf_error("Connection closed on descriptor %d before all data was received", client_fd);
                }
                i += n;

            }
        }

        fd_interface = PROTECT(C_as_socket(client_fd));
        out = PROTECT(allocVector(VECSXP, 2));
        SET_VECTOR_ELT(out, 0, fd_interface);
        SET_VECTOR_ELT(out, 1, payload_interface);
        UNPROTECT(3);

      C_push(user_queue, out);
    }
}

SEXP C_async_receive_and_enqueue(SEXP queue, SEXP port)
{
    pthread_t listener_thread

    pthread_create(&listener_thread, NULL, async_listener, arg)

}

SEXP C_send_sock(SEXP sock, SEXP value)
{
    unsigned int len = 0, i = 0;
    int need, n;
    unsigned char *payload;
    int sockfd;

    sockfd = INTEGER(sock)[0];
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
    return ScalarLogical(1);
}

SEXP C_send_loc(SEXP where, SEXP port, SEXP value)
{
    struct addrinfo hints, *res;
    int addr_error, retcode;
    int sockfd;
    SEXP fd;

    memset(&hints, 0, sizeof hints);
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_NUMERICSERV;
    addr_error = getaddrinfo(CHAR(STRING_ELT(where, 0)), CHAR(STRING_ELT(port, 0)), &hints, &res);
    if (addr_error)
        Rf_error("%s\n", gai_strerror(addr_error));
    sockfd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
    if (sockfd == -1)
        Rf_error("%s\n", strerror(errno));
    retcode = connect(sockfd, res->ai_addr, res->ai_addrlen);
    if (retcode == -1)
        Rf_error("%s\n", strerror(errno));

    fd = PROTECT(C_as_socket(sockfd));
    C_send_sock(fd, value);
    UNPROTECT(1);

    return fd;
}
