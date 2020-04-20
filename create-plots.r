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

read_data_ecdc <- function() {
  ds <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")[c(1, 11, 5, 6)]
  names(ds) <- c("Date", "Location", "Infections", "Deaths")
  ds$Date <- as.Date(strptime(ds$Date, format = "%d/%m/%Y"))
  ds <- ds[order(ds$Date), ]
  ds <- ddply(ds, .(Location), transform,
              Infections = cumsum(Infections),
              Deaths = cumsum(Deaths))
  ds <- melt(ds, id.vars = c("Date", "Location"), variable.name = "Value", value.name = "Count")
  ds
}

reshape_data_jhu <- function(ds, value) {
  ds <- melt(ds, id.vars = c("Country.Region", "Province.State", "Lat", "Long"), variable.name = "Date", value.name = "Count")
  ds$Value <- value
  ds
}

filter_jhu <- function(ds, location, filter_fun = filter_jhu_province) {
  ds <- filter_fun(ds)
  ds <- within(ds, {
    Location <- location
    Country.Region <- NULL
    Province.State <- NULL
  })
  ds
}

read_data_jhu <- function() {
  ds <- rbind(reshape_data_jhu(read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"), "Deaths"),
              reshape_data_jhu(read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"), "Infections"))
  ds <- within(ds, {
    Date <- as.Date(strptime(Date, format = 'X%m.%d.%y'))
    Value <- factor(Value, levels = c("Infections", "Deaths"))
    Lat <- NULL
    Long <- NULL
  })
  rbind(filter_jhu(ds, "Hong_Kong", function(ds) subset(ds, Province.State == "Hong Kong")),
        filter_jhu(ds, "China_Hubei", function(ds) subset(ds, Province.State == "Hubei")),
        filter_jhu(ds, "China_Rest", function(ds) {
          ds <- subset(ds, Country.Region == "China" & Province.State != "Hubei")
          ddply(ds, .(Country.Region, Date, Value), summarize, Count = sum(Count))}))
}

filter_location <- function(ds, location) subset(ds, Location == location)

filter_region <- function(locations) {
  return(function(ds, location) {
    ds <- subset(ds, Location %in% locations)
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

try_sma <- function(x, n) tryCatch(TTR::SMA(x, n), error = function(e) { x })

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

adaptive_number_scale <- function(x) {
  max_value <- max(x, na.rm = T)
  if (max_value < 1000) {
    x
  } else {
    scales::number(x)
  }
}

plot_daily_cases <- function(ds, value, color) {
  ds <- subset(ds, Value == value)
  ggplot(ds, aes(x = Date, y = New_Cases)) +
    geom_bar(stat = "identity", color = "lightgrey", fill = "lightgrey", alpha = 0.5) +
    geom_line(aes(x = Date, y = Rolling_New_Cases), linetype = "solid", color = color, size = 2) +
    scale_x_date(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0), labels = adaptive_number_scale) +
    labs(subtitle = sprintf("Daily reported %s", tolower(value)))
}

plot_doubling_rate <- function(ds, first_date) {
  ds <- sqldf('SELECT ds1.Value, ds1.Date as Date, ds1.Date - max(ds2.Date) as Doubling_Rate FROM ds AS ds1, ds AS ds2 WHERE ds1.Count / 2 >= ds2.Count AND ds1.Value = ds2.Value GROUP BY ds1.Value, ds1.Date')
  ds <- ddply(ds, .(Value), transform, Rolling_Doubling_Rate = try_sma(Doubling_Rate, 3))
  ggplot(ds, aes(x = Date, y = Rolling_Doubling_Rate, color = Value)) +
    geom_line(linetype = "solid", size = 2) +
    scale_x_date(NULL, expand = c(0, 0), limits = c(first_date, NA)) +
    scale_y_continuous(NULL, expand = c(0, 0), limits = c(0, NA), labels = adaptive_number_scale) +
    scale_color_manual(values = colors) +
    labs(subtitle = "Days since last doubling")
}

plot_location <- function(ds, location, filter_fun = filter_location, plot_png = F) {
  ds <- filter_fun(ds, location)
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
  location <- gsub(' ', '-', location)
  ggsave(sprintf("plots/%s.pdf", location), plot = p, width = 5, height = 8)
  if (plot_png) {
    ggsave(sprintf("plots/%s.png", location), plot = p, width = 5, height = 8, dpi = 100)
  }
}

ds <- read_data_ecdc()
ds.jhu <- read_data_jhu()

plot_location(ds, "World", function(ds, location) {
  ddply(ds, .(Date, Value), summarize, Count = sum(Count))
}, plot_png = T)

plot_location(ds, "European-Union", filter_region(
  c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
    "Czechia", "Denmark", "Estonia", "Finland", "France",
    "Germany", "Greece", "Hungary", "Ireland", "Italy",
    "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
    "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
    "Spain", "Sweden")), 
  plot_png = T)

plot_location(ds, "Africa", filter_region(
  c("Algeria", "Angola", "Benin", "Botswana", "Burkina_Faso", "Burundi", 
    "Cameroon", "Cape_Verde", "Central_African_Republic", "Chad", "Congo", 
    "Cote_dIvoire", "Democratic_Republic_of_the_Congo", "Djibouti", "Egypt", 
    "Equatorial_Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", 
    "Ghana", "Guinea", "Guinea_Bissau", "Kenya", "Liberia", "Libya", 
    "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", 
    "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", 
    "Sao_Tome_and_Principe", "Senegal", "Seychelles", "Sierra_Leone", 
    "Somalia", "South_Africa", "South_Sudan", "Sudan", "Togo", "Tunisia", 
    "Uganda", "United_Republic_of_Tanzania", "Zambia", "Zimbabwe")))

plot_location(ds, "Americas", filter_region(
  c("Anguilla", "Antigua_and_Barbuda", "Argentina", "Aruba", "Bahamas", 
    "Barbados", "Belize", "Bermuda", "Bolivia", 
    "Bonaire, Saint Eustatius and Saba", "Brazil", "British_Virgin_Islands", 
    "Canada", "Cayman_Islands", "Chile", "Colombia", "Costa_Rica", "Cuba", 
    "Curaçao", "Dominica", "Dominican_Republic", "Ecuador", "El_Salvador", 
    "Falkland_Islands_(Malvinas)", "Greenland", "Grenada", "Guatemala", 
    "Guyana", "Haiti", "Honduras", "Jamaica", "Mexico", "Montserrat", 
    "Nicaragua", "Panama", "Paraguay", "Peru", "Puerto_Rico", 
    "Saint_Kitts_and_Nevis", "Saint_Lucia", 
    "Saint_Vincent_and_the_Grenadines", "Sint_Maarten", "Suriname", 
    "Trinidad_and_Tobago", "Turks_and_Caicos_islands", 
    "United_States_of_America", "United_States_Virgin_Islands", "Uruguay", 
    "Venezuela")))

plot_location(ds, "Asia", filter_region(
  c("Afghanistan", "Bahrain", "Bangladesh", "Bhutan", "Brunei_Darussalam", 
    "Cambodia", "China", "India", "Indonesia", "Iran", "Iraq", "Israel", 
    "Japan", "Jordan", "Kazakhstan", "Kuwait", "Kyrgyzstan", "Laos", 
    "Lebanon", "Malaysia", "Maldives", "Mongolia", "Myanmar", "Nepal", 
    "Oman", "Pakistan", "Palestine", "Philippines", "Qatar", "Saudi_Arabia", 
    "Singapore", "South_Korea", "Sri_Lanka", "Syria", "Taiwan", "Thailand", 
    "Timor_Leste", "Turkey", "United_Arab_Emirates", "Uzbekistan", "Vietnam", 
    "Yemen")))

plot_location(ds, "Europe", filter_region(
  c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", 
    "Belgium", "Bosnia_and_Herzegovina", "Bulgaria", "Croatia", "Cyprus", 
    "Czechia", "Denmark", "Estonia", "Faroe_Islands", "Finland", "France", 
    "Georgia", "Germany", "Gibraltar", "Greece", "Guernsey", "Holy_See", 
    "Hungary", "Iceland", "Ireland", "Isle_of_Man", "Italy", "Jersey", 
    "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", 
    "Moldova", "Monaco", "Montenegro", "Netherlands", "North_Macedonia", 
    "Norway", "Poland", "Portugal", "Romania", "Russia", "San_Marino", 
    "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", 
    "Ukraine", "United_Kingdom")))

plot_location(ds, "Oceania", filter_region(
  c("Australia", "Fiji", "French_Polynesia", "Guam", "New_Caledonia", 
    "New_Zealand", "Northern_Mariana_Islands", "Papua_New_Guinea")))

# Countries
plot_location(ds, "Austria")
plot_location(ds, "Belgium")
plot_location(ds, "Bulgaria")
plot_location(ds, "Croatia")
plot_location(ds, "Cyprus")
plot_location(ds, "Czechia")
plot_location(ds, "Denmark")
plot_location(ds, "Estonia")
plot_location(ds, "Finland")
plot_location(ds, "France")
plot_location(ds, "Germany", plot_png = T)
plot_location(ds, "Greece")
plot_location(ds, "Hungary")
plot_location(ds, "Iceland")
plot_location(ds, "Ireland")
plot_location(ds, "Italy", plot_png = T)
plot_location(ds, "Latvia")
plot_location(ds, "Lithuania")
plot_location(ds, "Luxembourg")
plot_location(ds, "Malta")
plot_location(ds, "Netherlands")
plot_location(ds, "Norway")
plot_location(ds, "Poland")
plot_location(ds, "Portugal")
plot_location(ds, "Romania")
#plot_location(ds, "Slovakia")
plot_location(ds, "Slovenia")
plot_location(ds, "Spain")
plot_location(ds, "Sweden")
plot_location(ds, "Switzerland")
plot_location(ds, "Turkey")
plot_location(ds, "United_Kingdom")

# Americas
plot_location(ds, "United_States_of_America")
plot_location(ds, "Brazil")
plot_location(ds, "Canada")

# Asia
plot_location(ds, "China")
plot_location(ds.jhu, "China_Hubei")
plot_location(ds.jhu, "China_Rest")
plot_location(ds.jhu, "Hong_Kong")
plot_location(ds, "Iran")
plot_location(ds, "Japan")
plot_location(ds, "South_Korea")
plot_location(ds, "Taiwan")
plot_location(ds, "Singapore")

# Pacific
plot_location(ds, "Australia")
plot_location(ds, "New_Zealand")
