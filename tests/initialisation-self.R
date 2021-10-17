library(largerscale)

(replier_location <- ReplierLocation("127.0.0.1", 0L))
(publisher_location <- PublisherLocation("127.0.0.1", 0L))
(self <- Node(replier_location, publisher_location))
service(self, live=TRUE)
