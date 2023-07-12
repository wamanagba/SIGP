setwd("~/Desktop/SIGP/")
rm(list = ls())

# Install the nasapower package if not already installed
if (!require(nasapower)) {
  install.packages("nasapower")
}

# Load the librarys
library(nasapower)
library(rio)

# Specify the coordinates of the location (latitude, longitude)
lon = - seq(5, 10, .5)
lat = rep(-37,11)

# Specify the date range
start_date <- "2023-05-23"
end_date <- "2023-05-23"

# Specify the variables to download (in this example, solar radiation)
variables <- c("RH2M", "T2M")

# Download the data from NASA POWER
Data= data.frame()


for (lont in lon) {
  #for (lati in lat) {
    
    data <- get_power(
      community = "AG",
      lonlat = c(lont, 37),
      pars = variables,
      dates = c(start_date,end_date),
      temporal_api= "DAILY",
      temporal_average = "DAILY"
    )
    data$nasapid <- paste(lont, 37, sep = "_")
    Data=rbind(Data,data)
  #}
  
  # Ajouter la colonne NasaPId avec la valeur correspondante
  
  
  # Concaténer les nouvelles données à l'objet Data
  #Data <- rbind(Data, data)
}

Data <- Data[c("nasapid","LON", "LAT")]
colnames(Data)[colnames(Data) == "LON"] <- "LonNP"
colnames(Data)[colnames(Data) == "LAT"] <- "LatNP"

rio::export(Data,file = "Data/Data.csv")





latitude <- 40.71
longitude <- -74.01

data <- get_power(
  community = "AG",
  lonlat = c(-2, 11),
  pars = c("RH2M", "T2M"),
  dates = c(start_date,end_date),
  temporal_api= "DAILY",
  temporal_average = "DAILY"
)


# Print the downloaded data
dir.create("Data",recursive = T,showWarnings = F)

rio::export(data,file = "Data/Data.xlsx")





import requests
import pandas as pd

def download_nasa_power_data(nasapid, start_date, end_date):
  url = f"https://power.larc.nasa.gov/api/temporal/daily/point?parameters=T2M&community=AG&format=JSON&start={start_date}&end={end_date}&longitude={nasapid[0]}&latitude={nasapid[1]}"
response = requests.get(url)
if response.status_code == 200:
  data = response.json()
return data
else:
  print(f"Failed to download data for nasapid: {nasapid}")

def get_nasa_power_data(nasapids, start_date, end_date):
  data = []
for nasapid in nasapids:
  result = download_nasa_power_data(nasapid, start_date, end_date)
if result is not None:
  result["nasapid"] = nasapid
data.append(result)
return data

# Exemple d'utilisation
nasapids = [
  (-74.0059, 40.7128),  # New York
  (-118.2437, 34.0522),  # Los Angeles
  (139.6917, 35.6895)  # Tokyo
]
start_date = "2023-01-01"
end_date = "2023-01-31"

nasa_power_data = get_nasa_power_data(nasapids, start_date, end_date)

# Convertir les données en DataFrame pandas
df = pd.DataFrame(nasa_power_data)
print(df)
