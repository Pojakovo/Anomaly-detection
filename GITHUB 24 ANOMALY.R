#INSTALL REQUIRED PACKAGES
install.packages("tidyverse")  
install.packages("stats")  
install.packages("corrplot")  
install.packages("ggfortify") 
install.packages("NbClust")    
install.packages("fpc") 
install.packages("factoextra") 
#LOAD REQUIRED LIBRARIES
library(ggplot2) 
library(ggfortify) 
library(readr)   
library(dplyr)  
library(tidyr)  
library(stringr)  
library(lubridate) 
library(stats)    
library(corrplot)  
library(NbClust)  
library(cluster)    
library(fpc)        
library(factoextra)  
library(gridExtra)   
# DATASET IMPORTATION: IMPORT DATASET INTO THE DATAFRAME 
#  WE WILL NEED TO SET THE WORKING DIRECTORY 
setwd("~/Downloads/CETM24-DATA") # SETTING WORKING DIRECTORY
read.csv(file.choose(), header = T) 
my_data <- Walmart2_csv_ASSESSMENT
####################################################################
# DATA EXPLORATION PROCESS
my_data 
head(my_data)     # TO VIEW FIRST SIX ROWS
tail(my_data)     # TO VIEW LAST SIX ROWS
head(my_data,15) # TO CHECK THE FIRST 15 ROWS                                  
tail(my_data,15) # TO CHECK THE LAST 15 ROWS
sample_n(my_data,10) #THIS COMMAND RANDOMLY SELECT 10 OBSERVATIONS/ROWS  
str(my_data)  #TO CHECK THE STRUCTURE OF THE DATASET
summary(my_data) # TO CHECK THE STATISTICAL INFORMATION OF THE DATASET
###################################################################
#DATA CLEANING
is.na(my_data)    # FIND MISSING VALUES
#COUNT MISSING VALUES
sum(is.na(my_data))    # THE DATASET HAS ZERO MISSING VALUES
duplicated(my_data)    # FIND DUPLICATE VALUES
sum(duplicated(my_data))  # THE DATASET IS FREE FROM DUPLICATE VALUES
#####################################################################
# DATA PRE-PROCESSING
# TO TRANSFORM DATE FROM CHARACTER VECTOR 
my_data$Date <- dmy(my_data$Date)  # A LUBRIDATE FUNCTION  TO CONVERT DATE TO DATE TYPE
plot(my_data)                      # TO GET THE OVERVIEW OF THE DATA
boxplot(my_data)      # TO DETECT OUTLIERS IN THE FEATURES DATASET
# TO VISUALIZE ALL THE OBSERVATIONS WITH OUTLIERS 
# WEEKLY SALES BOXPLOT FOR OUTLIER DETECTION
outlierplot1 <- ggplot(data = my_data) + 
  geom_boxplot(aes(x = Weekly_Sales),width = 1.0)+
  labs( title = "Outliers in Weekly Sales")+ scale_fill_brewer(palette = "Dark2")
outlierplot1
#UNEMPLOYMENT BOXPLOT FOR OUTLIER DETECTION
  outlierplot2 <- ggplot(data = my_data) + 
  geom_boxplot(aes(x = Unemployment),width = 1.0)+
  labs( title = "Outliers in Unemployment Rate")+ scale_fill_brewer(palette = "Dark2")
outlierplot2
# TEMPERATURE BOXPLOT FOR OUTLIER DETECTION
 outlierplot3 <- ggplot(data = my_data) + 
   geom_boxplot(aes(x = Temperature),width = 0.5)+
   labs( title = "Outliers in Temperature")+ scale_fill_brewer(palette = "Dark2")
outlierplot3 
 outlierplot4 <- ggplot(data = my_data) +
   geom_boxplot(aes(x= Holiday_Flag), width = 1.0) +
   labs(title = "Outliers in Holiday") + scale_fill_brewer(palette = "Dark2")
 outlierboxplot <- grid.arrange(outlierplot1,outlierplot2,outlierplot3,outlierplot4, ncol = 2.5)
outlierplot4
######################################################################## 
#PCA PRINCIPAL COMPONENT ANALYSIS
# FIRST WE CHECK THE  PCA ELIGIBILITY OF THE DATASET
# CORRELATION BETWEEN THE FEATURES OF THE DATASET AND WEEKLY SALES 
my_data2 <- my_data[, -2] # BECAUSE DATE IS A CHARACTER VECTOR WE WILL REMOVE THE COLOUMN
cor(my_data2, method = "spearman") #TO BUILD CORRELATION MATRIX
mean(cor(my_data2))  # MEAN IS 0.1106468,THE AVERAGE CORRELATION AMONG THE VARIABLES
corrplot(corr = cor(my_data2), method = "color",    #APPLY CORRPLOT FUNCTION TO VISUALIZE THE CORRELATION BETWEEN ALL THE VARIABLES
         col = colorRampPalette(c("white", "lightpink","red", "brown" ))(10))  
corrplot(corr = cor(my_data2), method = "number",   #TO GET THE NUMERICAL REPRESENTATION
         col = colorRampPalette(c("white", "lightpink","red", "brown" ))(10)) 
PCA <- prcomp(my_data2, center = TRUE, scale = TRUE) #PCA, GETTING THE PRINCIPAL COMPONENTS USING PRCOMP() COMMAND AND SCALNG THE DATASET
PCA                                       # THIS WILL DISPLAY STANDARD DEVIATION AND LOADINGS
summary(PCA)                             # THIS WILL DISPLAY THE VARIANCE OF COMPONENTS
plot(PCA)
plot(PCA, type = "l")
biplot(PCA)# THIS LOADINGS WHICH ARE THE ARROWS AND SCORES/DATAPOINTS AND NOT JUST TWO PC'S AGAINST ONE ANOTHEER       
str(PCA)   # X IS THE PRINCIPAL COMPONENT SCORE FOR EACH OBSERVATION
###########################################################################
 # EXPLORATORY DATA ANALYSIS
# DETERMINE WHAT FACTOR INFLUENCE THIS SALES VISUALIZING WITH PLOTS
#HYPOTHESIS GENERATION 1: DOES THESE INDEPENDENT VARIABLES/PREDICTORS HAVE INFLUENCE ON SALES 
#HYPOTHESIS(1). CAN THE LOCATION OF THE STORE HAVE INFLUENCE ON SALES
selected_data <- select(my_data, Store: Holiday_Flag)
selected_data <- select(selected_data, - Date)        
store_sales <- group_by(selected_data, Store)%>%    # TO DETERMINE THE AVERAGE SALES MADE BY EACH STORES
  summarise(average_sales = mean(Weekly_Sales))
#TO CHECK FOR THE STORE WITH THE HIGHEST SALES 
store_sales <- group_by(selected_data, Store)%>%
  summarise(average_sales = mean(Weekly_Sales)) %>%
  arrange(desc(average_sales))
head(store_sales)                   # STORE 20,4,14,13,3,10 HAS THE HIGHEST SALES 
tail(store_sales)                   # STORE 33, 44,5,36,38,3 HAS THE LOWEST SALES
#A SCATTERPLOT WILL BE USED TO VISUALIZE THE OBSERVATION
ggplot(store_sales)+
  geom_point(mapping = aes(x = Store, y = average_sales),
             shape = 19, size = 3.5, colour = "blue")+
labs(x= "Store location", y = " Average Weekly Sales", 
     title = "Relationship Between Store Location and Average Sales")+
  theme_linedraw()
# A BAR CHART VISUALIZATION FOR EASY AND BETTER ANALYSIS
ggplot(store_sales, aes(x = Store, y = average_sales))+
  geom_bar(stat = "identity", fill = 'blue')+
  labs(title = "Relationship Between Store Location and Average Sales",
       x = "store location", y = "Average Weekly Sales")
# HYPOTHESIS (2). HOLIDAY AND NON- HOLIDAY INFLUENCE ON WEEKLY SALES 
hol_sales <- selected_data
 On_holiday <- hol_sales%>%     # TO CHECK DAYS ON HOLIDAYS 
  filter(Holiday_Flag == 1)  # THERE WAS HOLIDAYS FOR 450 WEEKS
  non_holiday <- hol_sales %>%   # TO CHECK DAYS WITHOUT HOLIDAYS 
  filter(Holiday_Flag == 0)  #  THERE WERE NOT HOLIDAYS FOR 5985 WEEKS
holiday_sale <- rbind(On_holiday, non_holiday) # JOINING THE TWO DATAFRAMES
holiday_sale
holiday_sales <- selected_data %>%
  group_by(Store, Holiday_Flag)%>%    # TO GET THE SUMMARY  FOR EACH STORE AND HOLIDAY PERIOD
  summarise(average_sales = mean(Weekly_Sales), sum_sales = sum(Weekly_Sales), max_sales = max(Weekly_Sales))
#A BAR CHART WILL BE USED TO VISUALIZE THE OBSERVATION 
ggplot( holiday_sales, aes(x =Store, y = average_sales))+
  geom_bar(stat = "identity", aes(fill = Holiday_Flag))+
  labs(title = "Holiday Influence on Average Sales",
     x = "Store ", y = "Average Weekly Sales") + theme_bw()
boxplot(holiday_sale$Holiday_Flag)
# DOES THE TEMPERATURE ON THE DAY OF SALES HAVE INFLUENCE ON SALES
temperature_sales <- data.frame(my_data$Temperature, my_data$Weekly_Sales, my_data$Store) 
temperature_sales
 temperature_sales <- temperature_sales%>%
  group_by(my_data.Store, my_data.Temperature) %>%
  summarise(ave_weekly_sales = mean(my_data.Weekly_Sales))%>%
    arrange(desc(ave_weekly_sales))
ggplot(data = temperature_sales)+
  geom_point(mapping = aes(x = my_data.Temperature, y = ave_weekly_sales, color = my_data.Store), shape = 19, size = 2)+
  labs(x = "Temperature Rate", y = " Average Weekly Sales", 
       title = "The Relaionship between Temperature Rate and Sales")+
       scale_colour_gradient2(low ='red', high = 'yellow', mid = 'blue')+
      theme_linedraw()
 # HOW WILL THE INCREASE/DECREASE OF UNEMPLOYMENT RATE HAVE INFLUENCE ON SALES
unemployment_sales <- data.frame(my_data$Weekly_Sales, my_data$Unemployment)
unemployment_sales
unemployment_sales <- group_by(unemployment_sales,my_data.Unemployment)%>%
  summarise(ave_weekly_sales = mean(my_data.Weekly_Sales))
ggplot(unemployment_sales)+
  geom_point(mapping = aes(x = my_data.Unemployment, y = ave_weekly_sales), shape = 19, size = 3.5, colour = "red")+
  labs(x = "Unemployment Rate", y = "Average Weekly sales", 
       title = "INFLUENCE OF UNEMPLOYMENT RATE ON AVERAGE SALES")+
  theme_linedraw()
# HOW DOES INCREASE/DECREASE IN CPI RATE AFFECT SALES
CPI_sales <- data.frame(my_data$Weekly_Sales, my_data$CPI)
ggplot(CPI_sales)+
  geom_point(mapping = aes(x = my_data.CPI, y = my_data.Weekly_Sales), shape = 19, size = 2, colour = "red")+
  labs(x = "CPI RATE", y = "WEEKLY SALES", 
       title = "INFLUENCE OF CPI RATE ON SALES")+
  theme_linedraw()
# EFFECT OF FUEL PRICE ON SALES 
fuel_sales <- data.frame(my_data$Weekly_Sales, my_data$Fuel_Price)
ggplot(fuel_sales)+
  geom_point(mapping = aes(x = my_data.Fuel_Price, y = my_data.Weekly_Sales), shape = 19, size = 2, colour = "red")+
  labs(x = "Fuel Price", y = "Weekly Sales", 
       title = "The Relationship Between Fuel Price and Rate of Sales ")+
  theme_linedraw()
#SINCE IT IS A WEEKLY DATSET IS/ARE THERE SOME WEEKS THAT TENDS TO HAVE MORE SALES "WEEK OF THE MONTH EFFECT"
# VISUALIZE THE TREND USING A LINE PLOT
plot(my_data$Weekly_Sales ~ my_date, type = "l", col = "red", axes = F)
     box() + axis(1,my_date,format(my_date,"%m-%y"))
##################################################################
#ANOMALY DETECTION ALGORITHM
#K-MEANS CLUSTERING
#WSS(WITHIN SUM SQUARES) PLOT TO CHOOSE THE MAXIMUM NUMBER OF (K)CLUSTERS
wssplot <- function(data, nc = 15, seed = 1234){
  wss <- (nrow(data)-1)* sum(apply(data,2, var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:nc,wss,type = "b", xlab = "NUMBER OF CLUSTERS", ylab = "GROUP SUM OF SQUARES")
}
wssplot(my_data2, nc = 15, seed = 1234)  # MAXIMUM NUMBER OF K IS 4 FROM THE WSS PLOT
# TO CONFIRM THE OPTIMUM NUMBER OF CLUSTERS USING NBCLUST
attach(my_data2)   #TO ACCESS THE VARIABLES IN THE DATAFRAME
my_data2
set.seed(1234)
N_clusters <- NbClust(my_data2, min.nc = 2, max.nc = 4, method = "kmeans")  # NBCLUST CONFIRMS THE BEST VALUE OF K IS 4
N_clusters
###########################################################
KM_clusters <- kmeans(my_data2, centers = 4) # TRAINING THE KMEANS CLUSTER MODEL
KM_clusters  # 4 CLUSTER OF SIZES 1693, 1526, 2260, 956 
###############################################
# EVALUATING THE CLUSTERS TO CHECK IF THEY ARE DISTINCT OR NOT
autoplot(KM_clusters, my_data2, frame = TRUE) # USING AUTOPLOT TO BUILD CLUSTER PLOT TO CHECK THE DISTINCTNESS
attributes(KM_clusters)
KM_clusters$centers  # THE CLUSTER CENTERS TO CHECK THE DISTINCTNESS THE CENTERS HAVE DIFFERENT VALUES HENCE THEY ARE DISTINCT IN NATURE
KM_clusters$cluster
################################################
# TO CHECK THE CORRESPONDING CLUSTER FOR EACH OBSERVATION IN THE DATASET
my_dataCluster <- my_data2|> mutate(KM_clusters = KM_clusters$cluster) 
 my_dataCluster$KM_clusters
 ##########################################
 #PLOTTING THE CLUSTERS
plotcluster(my_data2, KM_clusters$cluster)  # TO PLOT THE CLUSTERS
clusplot(my_data2, KM_clusters$cluster, main = "K-MEANS CLUSTERPLOT", color = TRUE,
         shade = TRUE, labels = 4, lines = 12)
#VISUALIZING THE OUTLIERS IN EACH VARIABLES IN THE DATASET
cluster1 <- my_dataCluster|> ggplot(aes(x = Unemployment, y = Weekly_Sales,
                            col = as.factor(KM_clusters)))+ geom_point()
cluster2 <- my_dataCluster|> ggplot(aes(x = Store, y = Weekly_Sales,
                            col = as.factor(KM_clusters)))+ geom_point()
cluster3 <- my_dataCluster|> ggplot(aes(x = Holiday_Flag, y = Store,
                            col = as.factor(KM_clusters)))+ geom_point()
cluster4 <- my_dataCluster|> ggplot(aes(x = CPI, y = Weekly_Sales,
                            col = as.factor(KM_clusters)))+ geom_point()
cluster5 <- my_dataCluster|> ggplot(aes(x = Temperature, y = Weekly_Sales,
                            col = as.factor(KM_clusters)))+ geom_point()
cluster6 <- my_dataCluster|> ggplot(aes(x = Store, y = Unemployment,
                                        col = as.factor(KM_clusters)))+ geom_point()
cluster7 <- my_dataCluster|> ggplot(aes(x = Weekly_Sales, y = Fuel_Price,
                                        col = as.factor(KM_clusters)))+ geom_point()
my_data_clusters <- grid.arrange(cluster1, cluster2, cluster3, cluster4, cluster5, cluster6, cluster7)
####################################################################
# CLUSTER VALIDATION: TO EVALUATE THE RESULT OF THE KMEANS ANALYSIS USING SILHOUETTEWIDTH
sil_result <- silhouette(KM_clusters$cluster, dist(my_data2))
fviz_silhouette(sil_result)
sil_result
######################################################################
#SAVE THE TRAINED K-MEANS MODEL
saveRDS(KM_clusters, "kmeans_model.AnomalyDetection")


