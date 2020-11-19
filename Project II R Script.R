library(readxl) #open excel file
Project_II <- read_excel("Desktop/Intro to Business Analytics/Project I TIDY Data.xlsx")
View(Project_II)
library(ggplot2) #open ggplot2
library(plyr) #for ddply()
library(tseries) #for the J-B test

#rename and shorten variable names
Child_Mortality<-Project_II$`Child Mortality (0-5 year olds dying per 1000 born)`
Life_Expectancy<-Project_II$`Life Expectancy (years)`
Median_Age<-Project_II$`Median Age (years)`
Income<-Project_II$`Income per person (GDP/capita)`
Population<-Project_II$`Population, total`
Empl_Rate<-Project_II$`Employment Rate Aged 15+`

#INCORPORATING NONLINEAR (POLYNOMIAL) TRANSFORMATIONS OF DISPL
Income2<-Income^2
Income3<-Income^3

#fraction of sample to be used for training
p<-.7

#number of observations (rows) in the dataframe
obs_count<-dim(Project_II)[1]

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)
training_size
#set the seed to make your partition reproducible
set.seed(1234)
#create a vector with the shuffled row numbers of the original dataset
train_ind <- sample(obs_count, size = training_size)

Training <- Project_II[train_ind, ] #pulls random rows for training
Testing <- Project_II[-train_ind, ] #pulls random rows for testing

#CHECKING THE DIMENSIONS OF THE PARTITIONED DATA
dim(Training)
dim(Testing)

#rename and shorten training and testing variables
train_income<-Training$`Income per person (GDP/capita)`
train_life<-Training$`Life Expectancy (years)`
test_income<-Testing$`Income per person (GDP/capita)`
test_life<-Testing$`Life Expectancy (years)`

#PLOTTING THE TRAINING AND TESTING PARTITIONS

#PLOT THE IN-SAMPLE TRAINING PARTITION
plot(Life_Expectancy ~ Income, Training, main = 'Income vs. Life Expectancy',
  col = 'blue', ylab = 'Life Expectancy', xlim = c(0,50000), ylim = c(50,85)) +
  abline(lm(Life_Expectancy ~ Income, Training))

#PLOT THE OUT-OF-SAMPLE TESTING PARTITION
plot(Life_Expectancy ~ Income, Testing, main = 'Income vs. Life Expectancy',
  col = 'red', ylab = 'Life Expectancy', xlim = c(0,50000), ylim = c(50,85)) +
  abline(lm(Life_Expectancy ~ Income, Testing))

#PLOT THE IN-SAMPLE TRAINING PARTITION
points(train_income, train_life, col='blue')

#PLOT THE OUT-OF-SAMPLE TESTING PARTITION
points(test_income, test_life, col='red', pch=3)

#BUILDING THE MODEL FROM THE TRAINING DATA
M1 <- lm(Life_Expectancy ~ Income, Training)
summary(M1) #generates summary diagnostic output

#QUESTION:  ARE THE RESIDUALS NORMAL?
hist(M1$residuals) #PLOT THEM!
jarque.bera.test(M1$residuals) #TEST FOR NORMLAITY!

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_1_IN <- predict(M1, Training) #generate predictions on the (in-sample) training data
View(PRED_1_IN)

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_1_OUT <- predict(M1, Testing) #generate predictions on the (out-of-sample) testing data
View(PRED_1_OUT)

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_1_IN<-sqrt(mean(PRED_1_IN, na.rm = TRUE - (train_life)^2))
RMSE_1_OUT<-sqrt(mean(PRED_1_OUT, na.rm = TRUE - (test_life)^2))

RMSE_1_IN #IN-SAMPLE ERROR
RMSE_1_OUT #OUT-OF-SAMPLE ERROR

#create model 2
M2<-lm(Life_Expectancy ~ Income + Income2, Training)
summary(M2)
x_grid <- seq(0,150000,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(MODEL2, list(Income = x_grid, Income2 = x_grid^2))
plot(Life_Expectancy ~ Income, Training, main = 'Income vs. Life Expectancy',
     col = 'blue', ylab = 'Life Expectancy', xlim = c(0,50000), ylim = c(50,85)) +
  abline(lm(Life_Expectancy ~ Income, Training)) + lines(x_grid, predictions, col='green', lwd=3)
points(test_life ~ test_income, col='red', pch=3)

#QUESTION:  ARE THE RESIDUALS NORMAL?
hist(M2$residuals) #PLOT THEM!
jarque.bera.test(M2$residuals) #TEST FOR NORMLAITY!

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_2_IN <- predict(M2, Training)
PRED_2_OUT <- predict(M2, Testing) 
RMSE_2_IN<-sqrt(mean(PRED_2_IN, na.rm = TRUE - (train_life)^2))
RMSE_2_OUT<-sqrt(mean(PRED_2_OUT, na.rm = TRUE - (test_life)^2))
RMSE_2_IN
RMSE_2_OUT

predictions_2 <- predict(M2, list(ln_displ=log(x_grid)))
lines(x_grid, predictions_2, col='darkblue', lwd=3)

#create model 3
M3<-lm(Life_Expectancy ~ Income + Income2 + Income3, Training)
summary(M3)

#QUESTION:  ARE THE RESIDUALS NORMAL?
hist(M3$residuals) #PLOT THEM!
jarque.bera.test(M3$residuals) #TEST FOR NORMLAITY!

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_3_IN <- predict(M3, Training)
PRED_3_OUT <- predict(M3, Testing) 
RMSE_3_IN<-sqrt(mean(PRED_3_IN, na.rm = TRUE - (train_life)^2))
RMSE_3_OUT<-sqrt(mean(PRED_3_OUT, na.rm = TRUE - (test_life)^2))
RMSE_3_IN
RMSE_3_OUT

predictions_3 <- predict(M3, list(ln_displ=log(x_grid)))
lines(x_grid, predictions_3, col='lightgreen', lwd=3)

M4 <- lm(Life_Expectancy ~ Child_Mortality, Training)
summary(M4)

#QUESTION:  ARE THE RESIDUALS NORMAL?
hist(M4$residuals) #PLOT THEM!
jarque.bera.test(M4$residuals) #TEST FOR NORMLAITY!

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_4_IN <- predict(M4, Training)
PRED_4_OUT <- predict(M4, Testing) 
RMSE_4_IN<-sqrt(mean(PRED_4_IN, na.rm = TRUE - (train_life)^2))
RMSE_4_OUT<-sqrt(mean(PRED_4_OUT, na.rm = TRUE - (test_life)^2))
RMSE_4_IN
RMSE_4_OUT

predictions_4 <- predict(M4, list(ln_displ=log(x_grid)))
lines(x_grid, predictions_4, col='orange', lwd=3)

M5<-lm(Life_Expectancy ~ Income + Child_Mortality, Training)
summary(M5)

#QUESTION:  ARE THE RESIDUALS NORMAL?
hist(M5$residuals) #PLOT THEM!
jarque.bera.test(M5$residuals) #TEST FOR NORMLAITY!

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_5_IN <- predict(M5, Training)
PRED_5_OUT <- predict(M5, Testing) 
RMSE_5_IN<-sqrt(mean(PRED_5_IN, na.rm = TRUE - (train_life)^2))
RMSE_5_OUT<-sqrt(mean(PRED_5_OUT, na.rm = TRUE - (test_life)^2))
RMSE_5_IN
RMSE_5_OUT

predictions_5 <- predict(M5, list(ln_displ=log(x_grid)))
lines(x_grid, predictions_5, col='blue', lwd=3)
