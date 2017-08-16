############################################################
# Zillow Prize: Zillow’s Home Value Prediction (Zestimate) #
############################################################
################          Ethan Tsao        ################
############################################################

## Import libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(caret)
library(magrittr)
## read in the data
properties <- fread(input = "https://s3.amazonaws.com/kaggle.competition.zillow.prize/properties_2016.csv", 
                    na.strings = "") 
train <- fread(input = "https://s3.amazonaws.com/kaggle.competition.zillow.prize/train_2016_v2.csv",
               na.strings = "")
## convert lat/lon
properties <- properties %>%
  mutate(latitude = latitude/1e6, longitude = longitude/1e6)
## rename the features
properties <- properties %>% rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26, 
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet, 
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_unknown = finishedsquarefeet50,
  num_unit = unitcnt, 
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,
  num_75_bath = threequarterbathnbr, 
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,  
  tax_total = taxvaluedollarcnt,
  tax_building = structuretaxvaluedollarcnt,
  tax_land = landtaxvaluedollarcnt,
  tax_property = taxamount,
  tax_year = assessmentyear,
  tax_delinquency = taxdelinquencyflag,
  tax_delinquency_year = taxdelinquencyyear,
  zoning_property = propertyzoningdesc,
  zoning_landuse = propertylandusetypeid,
  zoning_landuse_county = propertycountylandusecode,
  flag_fireplace = fireplaceflag, 
  flag_tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style= architecturalstyletypeid
)
train <- train %>% rename(
  id_parcel = parcelid,
  date = transactiondate
)
## change categorical features to numeric features
properties <- properties %>% mutate(
  tax_delinquency = ifelse(tax_delinquency == "Y",1,0),
  flag_fireplace = ifelse(flag_fireplace == "Y",1,0),
  flag_tub = ifelse(flag_tub=="Y",1,0)
)
## transaction the volumn by date
train %>% 
  mutate(year_month = make_date(year=year(date),month=month(date))) %>%
  group_by(year_month) %>%
  count() %>%
  ggplot(aes(x=year_month,y=n)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-10-15"), size = 2)))+
  ggtitle("Transaction Volumn by Date")
## distribution of logerror (99% percentile)
train %>%
  filter(logerror %between% c(quantile(train$logerror, .005),quantile(train$logerror,.995))) %>%
  ggplot(aes(x=logerror))+
  geom_density(alpha=.5, fill = "yellow")+
  geom_histogram(aes(y=..density..), bins = 50, fill = "blue",color = "black")+
  ggtitle("Distribution of Logerror (99% percentile)")
## distribution of absolute logerror (99% percentile)
train %>%
  filter(logerror %between% c(quantile(train$logerror, .005),quantile(train$logerror, .995))) %>%
  mutate(abslogerror = abs(logerror)) %>%
  ggplot(aes(x=abslogerror))+
  geom_density(alpha=.5, fill = "yellow")+
  geom_histogram(aes(y=..density..),bins = 50, fill = "blue", color = "black")+
  ggtitle("Distribution of Absolute Logerror (99% percentile)")
## mean of absolute logerror over time (99% percengtile)
train %>%
  filter(logerror %between% c(quantile(train$logerror, .005),quantile(train$logerror, .995))) %>%
  mutate(year_month = make_date(year = year(date), month = month(date))) %>%
  group_by(year_month) %>%
  summarise(meanerror = mean(abs(logerror)),
            stderror = sqrt(var(logerror)/n())) %>%
  ggplot(aes(x = year_month, y = meanerror))+
  geom_line(linetype = "dashed", color = "blue")+
  geom_errorbar(aes(ymin = meanerror - 1.96 * stderror, ymax = meanerror +1.96 * stderror),
                color = "blue", width =10)+
  geom_point(size = 3, color = "blue")+
  ggtitle("Mean of Absolute Logerror over Time (99% percengtile)")
## distribution of mean absolute logerror by month (99% percengtile)
train %>%  
  filter(logerror %between% c(quantile(train$logerror, .005),quantile(train$logerror, .995))) %>%
  mutate(year_month = as.factor(make_date(year = year(date), month = month(date)))) %>%
  mutate(abslogerror = abs(logerror)) %>%
  ggplot(aes(x = abslogerror))+
  geom_histogram(aes(y = ..density..), fill = "blue", bins = 30, alpha = .5)+
  facet_wrap(~ year_month)+
  ggtitle("Distribution of Mean Absolute Logerror by Month")
## geographic distribution of logerror
properties %>% 
  mutate(latitude = latitude/1e6, longitude = longitude/1e6) %>%
  inner_join(train, by="id_parcel") %>%
  group_by(longitude, latitude) %>%
  summarise(logerror = mean(logerror)) %>%
  leaflet() %>%
  addTiles() %>%
  addHeatmap(lng = ~longitude, lat = ~latitude, 
             intensity = .02,
             blur = 5, radius = 4, 
             group = "Property heatmap") %>%
  addHeatmap(lng = ~longitude, lat = ~latitude, 
             intensity = ~logerror,
             blur = 5, radius = 4, 
             group = "logerror heatmap") %>%
  addLayersControl(
    baseGroups = c("Property heatmap", "logerror heatmap"),
    options = layersControlOptions(collapsed = FALSE)
  )%>%
  addMiniMap()

# remove columns with the lower varience
nearZeroVar(properties)
unique(properties)
properties1 <- properties[,c(-10,-23,-28,-30,-31,-32,-42,-43,-44,-50,-53,-56)]



