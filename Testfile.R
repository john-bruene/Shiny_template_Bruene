


wahlkreise <- wahlkreise_react()

# Transformation der Koordinaten in das geografische Koordinatensystem (Längen- und Breitengrad)
wahlkreise <- st_transform(wahlkreise, crs = 4326)

data <- politician_data2125

# Erstelle eine neue Spalte "WKN_NR" 
data$WKR_NR <- str_extract(data$electoral_data_constituency.label, "\\d{1,3}(?= - )")

data$WKR_NR <- as.numeric(data$WKR_NR)


# Merge der Tabellen location_data und wahlkreise über die Spalte "WKR_NR"
data <- merge(data, wahlkreise21[, c("WKR_NR", "geometry")], by = "WKR_NR", all.x = TRUE)

# filter for electoral_data_mandate_won == "constituency"

data <- data %>% filter(electoral_data_mandate_won == "constituency")

str(votes_2125_df$vote)

# sort data by WKN_NR. omit NAs

data <- data[order(data$WKR_NR, na.last = NA),]

wahlkreise17_2 <-  st_transform(wahlkreise17, crs = 4326)

location_data$name <- location_data$label.x

str(wahlkreise17_2)

# Sicherstellen, dass location_data ein sf-Objekt ist
if (!inherits(data, "sf")) {
  data <- st_as_sf(data, crs = 4326)
}

if (any(st_is_empty(data$geometry))) {
  stop("Einige Geometrien sind leer oder ungültig")
}


str(data)

myColors <- c("yes" = "#a4d67a", "no" = "#c76f5e", "abstain" = "#c7b6a7", "no_show" = "#7c7b7d")
pal <- colorFactor(myColors, levels = location_data$vote)
