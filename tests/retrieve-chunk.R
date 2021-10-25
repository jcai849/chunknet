# Run tests/initialisation-other.R

source("tests/send-chunk.R")

replier <- largerscale:::Replier.Location(self)
largerscale:::GET(requester, largerscale:::Identifier.Data(data),
                            "value",
                            rzmq::get.last.endpoint(replier))
rzmq::receive.socket(replier)
