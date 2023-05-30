
bank_locations <- read.csv("Lending_Equity_-_Depository_Locations.csv")

bank_locations$Location <- substr(
  bank_locations$Location, 8,
  nchar((bank_locations$Location))
)

bank_locations$Location <- sub(".$", "", bank_locations$Location)

split_location <- strsplit(as.character(bank_locations$Location), " ",
  fixed = TRUE
)

bank_locations$Longitude <- sapply(split_location, function(x) x[1])

bank_locations$Latitude <- sapply(split_location, function(x) x[2])

bank_locations$Longitude <- as.numeric(bank_locations$Longitude)

bank_locations$Latitude <- as.numeric(bank_locations$Latitude)

bank_locations <- bank_locations %>% filter(Reporting.Year == "2021")

bank_locations <- bank_locations %>% select(c("Bank", "Latitude", "Longitude"))

write.csv(bank_locations, "Bank_Locations.csv", row.names = FALSE)
