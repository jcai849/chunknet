# Run tests/initialisation-from-initator.R

library(largerscale)

ssh_line <- sapply(body(spawn), identical,
                           quote(system2("ssh",
                                shQuote(shQuote(c(host, "R", "-e", deparse1(summon)))))))
body(spawn) <- body(spawn)[!ssh_line]
synchronizer_initialisation_line <- sapply(body(spawn), identical,
                            quote(synchronizer <- Replier(Location(host(self), port = 0L))))
synchronizer_at_different_port <- quote(synchronizer <- Replier(Location(host(self), port = 12345L)))
body(spawn)[synchronizer_initialisation_line] <- list(synchronizer_at_different_port)

initiatee <- Node(ReplierLocation("127.0.0.1", 9878L), PublisherLocation("127.0.0.1", 8237L))
self <- ReplierLocation("127.0.0.1", 0L)

spawn(initiatee, self)
