# Run tests/initialisation-other.R

source("tests/spawn-other.R")

data <- largerscale:::Data(largerscale:::Chunk.Identifier(largerscale:::Identifier(), NULL), 1:100)
requester <- largerscale:::Requester()
rzmq::connect.socket(requester, largerscale:::as.character.Location(other_replier))
largerscale:::PUT(requester, payload=data)
rzmq::receive.socket(requester)
