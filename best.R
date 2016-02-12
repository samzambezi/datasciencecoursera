best <- function(state, outcome){
## read data
my_data <- read.csv("outcome-of-care-measures.csv")
## create dataframe
my_states <- my_data [,7]
my_hospitals <- my_data [,2]
outcome1 <- my_data [,11]
outcome1 = as.numeric(as.character(outcome1))
outcome2 <- my_data [,17]
outcome2 = as.numeric(as.character(outcome2))
outcome3 <- my_data [,23]
outcome3 = as.numeric(as.character(outcome3))
dataframe <- data.frame(my_states,my_hospitals,outcome1,outcome2,outcome3)
dataframe = na.omit(dataframe)
mycolnames <- c("states","hospitals","heart.attack", "heart.failure", "pneumonia")
colnames(dataframe) <- mycolnames
## Validation
if(!any(outcome == c("heart attack","heart failure","pneumonia")))
{stop("invalid outcome")}
if(!any(state == dataframe$states))
{stop("invalid outcome")}
## filter according to state
filter <- dataframe[dataframe$states == state,]
## return minimum value of the filter based on outcome
if (outcome == "heart attack"){
best_hospital <- filter[which.min(filter$heart.attack),]}
if (outcome == "heart failure"){
best_hospital <- filter[which.min(filter$heart.failure),]}
if (outcome == "pneumonia"){
best_hospital <- filter[which.min(filter$pneumonia),]}
return(best_hospital["hospitals"])}


