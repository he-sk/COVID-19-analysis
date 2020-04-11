library(reshape2)
library(ggplot2)
library(plyr)
library(data.table)
library(ggpubr)
library(TTR)
library(scales)
library(sqldf)

theme_set(theme_bw())
theme_update(panel.grid.minor = element_blank(),
             panel.grid.major.x = element_blank(),
             plot.subtitle = element_text(size = 9),
             legend.position = "none",
             panel.border = element_blank(),
             axis.line = element_line(color = "black", size = 0.5))

colors <- c("#7570b3", "#d95f02")

reshape_data <- function(ds, value) {
  ds <- melt(ds, id.vars = c("Country.Region", "Province.State", "Lat", "Long"), variable.name = "Date", value.name = "Count")
  ds$Value <- value
  ds
}

parse_dates <- function(ds) {
  ds <- within(ds, {
    Day <- gsub("^X([0-9]+)\\.([0-9]+)\\.([0-9]+)", "\\2", Date)
    Month <- gsub("^X([0-9]+)\\.([0-9]+)\\.([0-9]+)", "\\1", Date)
    Year <- gsub("^X([0-9]+)\\.([0-9]+)\\.([0-9]+)", "20\\3", Date)
    Date <- as.Date(as.POSIXct(sprintf("%s-%s-%s", Year, Month, Day), format = "%Y-%m-%d"))
  })
  ds
}

read_data <- function() {
  ds <- rbind(reshape_data(read.csv("time_series_covid19_deaths_global.csv"), "Deaths"),
              reshape_data(read.csv("time_series_covid19_confirmed_global.csv"), "Infections"))
  ds <- parse_dates(ds)
  ds <- within(ds, {
    Value <- factor(Value, levels = c("Infections", "Deaths"))
  })
  ds
}

filter_country_generic <- function(ds, country) {
  ds <- subset(ds, Country.Region == country)
}

filter_country_province <- function(ds, country) {
  ds <- subset(ds, Province.State == country)
}

filter_country_summarize <- function(ds, country) {
  ds <- filter_country_generic(ds, country)
  ddply(ds, .(Country.Region, Date, Value), summarize, Count = sum(Count))
}

filter_country_rename <- function(old_name) {
  return(function(ds, country) {
    filter_country_generic(ds, old_name)
  })
}

filter_country_region <- function(countries) {
  return(function(ds, country) {
    ds <- subset(ds, Country.Region %in% countries)
    ddply(ds, .(Date, Value), summarize, Count = sum(Count))
  })
}

plot_cumulative <- function(ds) {
  ggplot(ds, aes(x = Date, y = Count, color = Value, fill = Value)) + 
    geom_area(position = "identity", alpha = 0.5, color = NA) +
    scale_x_date(NULL, expand = c(0, 0)) +
    #scale_y_continuous(NULL, trans = "log10", expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0), labels = scales::number) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    labs(subtitle = "Cumulative progression") +
    theme(legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.title = element_blank(),
          legend.text = element_text(margin = margin(l = 3, unit = "pt")),
          legend.margin = margin(t=0, l=3, r=3, unit = "pt"),
          legend.box.margin = margin(t=0, l=1, unit = "pt"),
          legend.background = element_rect(fill = "white"),
          legend.box.background = element_blank())
}

try_sma <- function(x, n) {
  tryCatch(TTR::SMA(x, n), error = function(e) { x })
}

compute_daily_change <- function(ds) {
  ds <- ds[order(ds$Value, ds$Date), ]
  ds <- ddply(ds, .(Value), transform, Day_Before = shift(Count))
  ds <- within(ds, {
    Daily_Change <- (Count / Day_Before) - 1
    Daily_Change <- ifelse(Daily_Change == Inf, NA, Daily_Change)
    New_Cases <- Count - Day_Before
  })
  ds <- ddply(ds, .(Value), transform, Rolling_Daily_Change = try_sma(Daily_Change, 5))
  ds <- ddply(ds, .(Value), transform, Rolling_New_Cases = try_sma(New_Cases, 5))
  max_daily_change <- ceiling(max(ds$Rolling_Daily_Change, na.rm = T) * 20) / 20
  ds <- within(ds, {
    Daily_Change <- ifelse(Daily_Change < max_daily_change, Daily_Change, max_daily_change)
  })
  ds
}

plot_daily_change <- function(ds, max_value) {
  ggplot(ds, aes(x = Date, y = Daily_Change, color = Value)) +
    geom_point(size = 1) +
    geom_line(aes(x = Date, y = Rolling_Daily_Change), linetype = "solid", size = 2) +
    scale_x_date(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0), limits = c(0, max_value), labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(values = colors) +
    labs(subtitle = "Daily rate of change")
}

plot_daily_cases <- function(ds, value, color) {
  ds <- subset(ds, Value == value)
  ggplot(ds, aes(x = Date, y = New_Cases)) +
    geom_bar(stat = "identity", color = "lightgrey", fill = "lightgrey", alpha = 0.5) +
    geom_line(aes(x = Date, y = Rolling_New_Cases), linetype = "solid", color = color, size = 2) +
    scale_x_date(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0), labels = scales::number) +
    labs(subtitle = sprintf("Daily reported %s", tolower(value)))
}

plot_doubling_rate <- function(ds, first_date) {
  ds <- sqldf('SELECT ds1.Value, ds1.Date as Date, ds1.Date - max(ds2.Date) as Doubling_Rate FROM ds AS ds1, ds AS ds2 WHERE ds1.Count / 2 >= ds2.Count AND ds1.Value = ds2.Value GROUP BY ds1.Value, ds1.Date')
  ds <- ddply(ds, .(Value), transform, Rolling_Doubling_Rate = try_sma(Doubling_Rate, 3))
  ggplot(ds, aes(x = Date, y = Rolling_Doubling_Rate, color = Value)) +
    geom_line(linetype = "solid", size = 2) +
    scale_x_date(NULL, expand = c(0, 0), limits = c(first_date, NA)) +
    scale_y_continuous(NULL, expand = c(0, 0), limits = c(0, NA), labels = scales::number) +
    scale_color_manual(values = colors) +
    labs(subtitle = "Days since last doubling")
}

plot_country <- function(ds, country, filter_fun = filter_country_generic, plot_png = F) {
  ds <- filter_fun(ds, country)
  ds.daily_change <- compute_daily_change(ds)
  max_daily_change <- ceiling(max(ds.daily_change$Rolling_Daily_Change, na.rm = T) * 20) / 20
  first_date <- min(subset(ds, Value == "Deaths" & Count > 0)$Date)
  ds <- subset(ds, Date >= first_date)
  ds.daily_change <- subset(ds.daily_change, Date >= first_date)
  
  p <- ggarrange(plot_cumulative(ds), 
                 plot_doubling_rate(ds, first_date),
                 #plot_daily_change(ds.daily_change, max_daily_change),
                 plot_daily_cases(ds.daily_change, "Infections", colors[1]),
                 plot_daily_cases(ds.daily_change, "Deaths", colors[2]),
                 ncol = 1, nrow = 4, heights = c(1.5, 1, 1, 1), align = "hv")
  country <- gsub(' ', '-', country)
  ggsave(sprintf("plots/%s.pdf", country), plot = p, width = 5, height = 8)
  if (plot_png) {
    ggsave(sprintf("plots/%s.png", country), plot = p, width = 5, height = 8, dpi = 100)
  }
}

ds <- read_data()

plot_country(ds, "World", function(ds, country) {
  ddply(ds, .(Date, Value), summarize, Count = sum(Count))
}, plot_png = T)

plot_country(ds, "European-Union", filter_country_region(
  c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
    "Czechia", "Denmark", "Estonia", "Finland", "France",
    "Germany", "Greece", "Hungary", "Ireland", "Italy",
    "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
    "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
    "Spain", "Sweden")), 
  plot_png = T)

plot_country(ds, "Africa", filter_country_region(
  c("Algeria", "Angola", "Benin", "Burkina Faso", "Cabo Verde", 
    "Cameroon", "Central African Republic", "Chad", "Congo (Brazzaville)", 
    "Congo (Kinshasa)", "Cote d'Ivoire", "Djibouti", "Egypt", "Equatorial Guinea", 
    "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", 
    "Ghana", "Guinea", "Kenya", "Liberia", "Madagascar", 
    "Malaysia", "Maldives", "Mauritania", "Mauritius", "Morocco", 
    "Namibia", "Niger", "Nigeria", "Panama", "Senegal", 
    "Seychelles", "Somalia", "South Africa", "Togo", "Tunisia", 
    "Uganda", "Zambia", "Zimbabwe", "Mozambique", "Libya", 
    "Guinea-Bissau", "Mali", "Botswana", "Burundi", "Sierra Leone", 
    "Malawi", "South Sudan", "Western Sahara", "Sao Tome and Principe")))

plot_country(ds, "Americas", filter_country_region(
  c("Antigua and Barbuda", "Argentina", "Bahamas", "Barbados", "Bolivia",
    "Brazil", "Canada", "Chile", "Colombia", "Costa Rica", "Cuba", 
    "Dominican Republic", "Ecuador", "El Salvador", "Guatemala", 
    "Guyana", "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", 
    "Paraguay", "Peru", "Rwanda", "Saint Lucia", "Saint Vincent and the Grenadines", 
    "Suriname", "Trinidad and Tobago", "Uruguay", "US", 
    "Venezuela", "Dominica", "Grenada", "Belize", "Saint Kitts and Nevis")))

plot_country(ds, "Asia", filter_country_region(
  c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", 
    "Bhutan", "Brunei", "Cambodia", "China", "Fiji", "Georgia", 
    "India", "Indonesia", "Iran", "Iraq", "Israel", 
    "Japan", "Jordan", "Kazakhstan", "Korea, South", "Kuwait", 
    "Kyrgyzstan", "Lebanon", "Mongolia", "Nepal", "Oman", 
    "Pakistan", "Philippines", "Qatar", "Saudi Arabia", "Singapore", 
    "Sri Lanka", "Sudan", "Taiwan*", "Thailand", "United Arab Emirates", 
    "Uzbekistan", "Vietnam", "Syria", "Timor-Leste", "Laos", 
    "West Bank and Gaza", "Burma")))

plot_country(ds, "Europe", filter_country_region(
  c("Albania", "Andorra", "Austria", "Belarus", "Belgium", 
    "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czechia", 
    "Denmark", "Estonia", "Finland", "France", "Germany", 
    "Greece", "Holy See", "Hungary", "Iceland", "Ireland", 
    "Italy", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", 
    "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", 
    "North Macedonia", "Norway", "Poland", "Portugal", "Romania", 
    "Russia", "San Marino", "Serbia", "Slovakia", "Slovenia", 
    "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "United Kingdom")))

plot_country(ds, "Oceania", filter_country_region(
  c("Australia", "New Zealand", "Papua New Guinea", "Tanzania")))

# Countries
plot_country(ds, "Austria")
plot_country(ds, "Belgium")
plot_country(ds, "Bulgaria")
plot_country(ds, "Croatia")
plot_country(ds, "Cyprus")
plot_country(ds, "Czechia")
plot_country(ds, "Denmark", filter_country_summarize)
plot_country(ds, "Estonia")
plot_country(ds, "Finland")
plot_country(ds, "France", filter_country_summarize)
plot_country(ds, "Germany", plot_png = T)
plot_country(ds, "Greece")
plot_country(ds, "Hungary")
plot_country(ds, "Iceland")
plot_country(ds, "Ireland")
plot_country(ds, "Italy", plot_png = T)
plot_country(ds, "Latvia")
plot_country(ds, "Lithuania")
plot_country(ds, "Luxembourg")
plot_country(ds, "Malta")
plot_country(ds, "Netherlands", filter_country_summarize)
plot_country(ds, "Norway")
plot_country(ds, "Poland")
plot_country(ds, "Portugal")
plot_country(ds, "Romania")
plot_country(ds, "Slovakia")
plot_country(ds, "Slovenia")
plot_country(ds, "Spain")
plot_country(ds, "Sweden")
plot_country(ds, "Switzerland")
plot_country(ds, "Turkey")
plot_country(ds, "United Kingdom", filter_country_summarize)

# Americas
plot_country(ds, "United States", function(ds, country) { filter_country_summarize(ds, "US") })
plot_country(ds, "Brazil")
plot_country(ds, "Canada", filter_country_summarize)

# Asia
plot_country(ds, "China", filter_country_summarize)
plot_country(ds, "China Hubei", function(ds, country) { filter_country_province(ds, "Hubei") })
plot_country(ds, "China Rest", function(ds, country) {
  ds <- filter_country_generic(ds, "China")
  ds <- subset(ds, Province.State != "Hubei")
  ddply(ds, .(Country.Region, Date, Value), summarize, Count = sum(Count))
})
plot_country(ds, "Hong Kong", filter_country_province)
plot_country(ds, "Iran")
plot_country(ds, "Japan")
plot_country(ds, "South Korea", filter_country_rename("Korea, South"))
plot_country(ds, "Taiwan", filter_country_rename("Taiwan*"))
plot_country(ds, "Singapore")

# Pacific
plot_country(ds, "Australia", filter_country_summarize)
plot_country(ds, "New Zealand")