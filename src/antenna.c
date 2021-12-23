#include <sys/socket.h>
#include <err.h>
#include <Rinternals.h>

#define BACKLOG 10

SEXP receiver (SEXP port)
{
    struct sockaddr_storage client_addr;
    socklen_t addr_size;
    struct addrinfo hints, *res;
    int error;
    int sockfd, client_fd;
    const char *cause = NULL;

    memset(&hints, 0, sizeof hints);
    hints.ai_flags = AI_PASSIVE;
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCk_STREAM;

    error = getaddrinfo(NULL, CHAR(STRING_ELT(port)), &hints, &res);
    if (error)
        errx(1, "%s", gai_strerror(error));
    sockfd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
    if (sockfd == -1)
        cause = "socket";
    if (bind(sockfd, res->ai_addr, res->ai_addrlen) == -1) {
        cause = "bind";
        close(sockfd);
    }
    if (sockfd == 0)
        err(1, "%s", cause);
    freeadrinfo(res);
    (void) listen(sockfd, BACKLOG);
    addr_size = sizeof client_addr;
    client_fd = accept(sockfd, (struct sockaddr *) &client_addr, &addr_size);
    if (client_fd == -1)
        err(1, "accept");
}
