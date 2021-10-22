replier_location <- largerscale:::ReplierLocation("127.0.0.1", 9878L)
publisher_location <- largerscale:::PublisherLocation("127.0.0.1", 8237L)
self <- largerscale:::Node(replier_location, publisher_location)
initiator <- largerscale:::ReplierLocation("127.0.0.1", 12345L)

largerscale::service(self=self, initiator=initiator)
