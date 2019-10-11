# Pitch Classification Project
# Isabelle Pardew
# September 2019

# data work
library(dplyr)
# model clustering
library(mclust)
# regression
library(stats)
# plotting
library(ggplot2)
# KNN Imputation
library(DMwR)
# Rpart
library(rpart)
library(rpart.plot)

############################### 1. Data Import and Cleaning ###############################

# import data using na.strings field to convert the blank cells to true NA
allpitches <- read.csv("/Users/isabellehpardew/Desktop/Pitch_Data.csv", header = TRUE, 
                       sep = ",", quote = "\"", dec = ".", 
                       fill = TRUE, comment.char = "", 
                       as.is = TRUE, na.strings=c("","NA"))
allpitches <- as.data.frame(allpitches)

# list of pitcher ids
pit_ids <- unique(allpitches$pitcher_id)

# check variable type
str(allpitches)
# convert int to num
allpitches <- taRifx::japply(allpitches, which(sapply(allpitches, class)=="integer"), as.numeric)

# check relationship between a break values and b break values
ggplot(data = allpitches, mapping = aes(x = break_z_a, y = break_z_b)) + geom_point() # basic ggplot
scatter.smooth(x=allpitches$break_x_a, y=allpitches$break_x_b, main="System A ~ System B")  # scatterplot
cor(allpitches$break_x_a, allpitches$break_x_b, use = "pairwise.complete.obs") # x break correlation coefficients
cor(allpitches$break_z_a, allpitches$break_z_b, use = "pairwise.complete.obs") # z break correlation coefficients

# scale the numerical data
allpitches_scale <- data.frame(scale(allpitches[,5:11]))
# include ID columns
allpitches_scale[,8:11] <- allpitches[,1:4]
# account for the negative linear relationship by multiplying by (-1)
allpitches_scale$break_x_a <- -(allpitches_scale$break_x_a)

# try imputing the b values that's missing to ultimately put spin rate in too
# need all data to impute, because it works with all data trends, not just one column itself
knnOutput <- knnImputation(allpitches_scale[, c(1:7)])
allpitches_complete <- allpitches_scale
# replace the original dataframe spin value with imputed, complete data spin value
allpitches_complete[, 5] <- knnOutput[,5]
# clean up global environment
rm(knnOutput, allpitches_scale, allpitches)

# if b values are unknown, use a values - we prefer to use b values for prediction
allpitches_complete <- allpitches_complete %>% mutate(pitch_initial_speed = coalesce(pitch_initial_speed_b, pitch_initial_speed_a),
                                      break_x = coalesce(break_x_b, break_x_a),
                                      break_z = coalesce(break_z_b, break_z_a))

# separate data into complete vs missing pitch type values
pitches_known <- allpitches_complete %>% filter(!is.na(pitch_type))
pitches_unknown <- allpitches_complete %>% filter(is.na(pitch_type))
# clean up global environment
rm(allpitches_complete)

# convert categorical resposne variables into factor levels
pitches_known$pitch_num <- unclass(as.factor(pitches_known$pitch_type))
pitches_unknown$pitch_num <- unclass(as.factor(pitches_unknown$pitch_type))


############################### 2. Data Analysis, Classification, and Prediction ###############################

get_predictions <- function(pit_id){
  
  # known data
  pitcher_known <- pitches_known %>% filter(pitcher_id == pit_id)
  # use only non-NA rows
  pitcher_known <- na.omit(pitcher_known)
  # random sampling
  sample <- 0.70 * nrow(pitcher_known)
  set.seed(80)
  index <- sample(seq_len(nrow(pitcher_known)), size = sample)
  # create training and test set
  data_train <- pitcher_known[index,]
  data_test <- pitcher_known[-index,]
  rm(sample, index)
  
  # ctree model
  ctree_model <- party::ctree(pitch_num ~ pitch_initial_speed + break_x + break_z + spinrate_b, 
                              data=data_train)
  newPrediction <- data.frame(predict(ctree_model, data_test,
                                      interval="predict", level=.95))
  # round the predicted values
  newPrediction$rounded_pred <- round(newPrediction$pitch_num)
  # put it back into the test dataframe
  data_test$predictions <- newPrediction$rounded_pred
  table(data_test$predictions, data_test$pitch_type)
  table(data_test$pitch_type)
  
  # now do it on completely unknown data
  # unknown data to eventually use for prediction
  pitcher_unknown <- pitches_unknown %>% filter(pitcher_id == pit_id)
  # make the prediction
  unknownPred <- data.frame(predict(ctree_model, pitcher_unknown,
                                    interval="predict", level=.95))
  pitcher_unknown$rounded_pred <- round(unknownPred$pitch_num)
  
  # return predicted types with actual number of pitch types
  result <- list(table(data_test$predictions, data_test$pitch_type), table(data_test$pitch_type), pitcher_unknown)
  return(result)
}

# get results
results <- lapply(pit_ids, get_predictions)

# put predictions together
complete_predictions <- NULL

for (i in 1:10) {
  complete_predictions <- rbind(complete_predictions, results[[i]][[3]])
}
# order by pitch number
complete_predictions <- complete_predictions %>% arrange(pitch_num)
complete_predictions <- complete_predictions %>% mutate(pitch_type_classification = case_when(rounded_pred == 1 ~ "Changeup",
                                                                                       rounded_pred == 2 ~ "Curveball",
                                                                                       rounded_pred == 3 ~ "Fastball",
                                                                                       rounded_pred == 4 ~ "Slider"))
  
# reorder by column name for readability
complete_predictions <- complete_predictions[c("pitch_id", "pitcher_id", "pitcher_side", "pitch_type", "pitch_type_classification",
                                               "pitch_initial_speed_a", "break_x_a", "break_z_a", "pitch_initial_speed_b",
                                               "spinrate_b", "break_x_b", "break_z_b", "pitch_initial_speed", "break_x",
                                               "break_z", "pitch_num", "rounded_pred")]

write.csv(complete_predictions, "Pitch_Predictions.csv")


