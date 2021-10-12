IdentifierTable <- function(key, value) {
	symbol_table <- data.frame(key, value)
	class(symbol_table) <- c("IdentifierTable", class(symbol_table))
	symbol_table
}
`|.IdentifierTable` <- function(x, y) rbind(x, y)
key.IdentifierTable <- function(symbol_table) symbol_table$key
value.IdentifierTable <- function(symbol_table) symbol_table$value
