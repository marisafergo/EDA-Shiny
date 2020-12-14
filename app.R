#packages
install.packages("shinyapps")
install.packages("shinythemes")
install.packages("rsconnect")
library(shiny)
library(tidyverse)
library(lubridate)
library(rsconnect)
library(glmnet)
library(mice)
library(randomForest)
library(caret)
library(e1071)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(base)
library(party)

#import files
gas_date <- read.csv("gas_data.csv", stringsAsFactors = F, row.names=1)
gas <- read.csv("gas_data.csv", stringsAsFactors = F, row.names=1)
data_test<- read.csv("test.csv",stringsAsFactors = F, row.names=1)
data_train <- read.csv("train.csv",stringsAsFactors = F, row.names=1)
test<- read.csv("test.csv",stringsAsFactors = F, row.names=1)
train <- read.csv("train.csv",stringsAsFactors = F, row.names=1)



#drop columns
gas_new <-  gas[,!(names(gas) %in% c("Date"))]
data_test <- test[,!(names(gas) %in% c("Date"))]
data_train <- train[,!(names(gas) %in% c("Date"))]

data_train_gas <- as.numeric(data_train[,7])
data_train_wo_gas <- as.matrix(data_train[,-7])
data_test_gas <- as.numeric(data_test[,7])
data_test_wo_gas <- as.matrix(data_test[,-7])

data_train <-  data_train[,!(names(data_train) %in% c("Year"))]
data_test <-  data_test[,!(names(data_test) %in% c("Year"))]

data_train <-  data_train[,!(names(data_train) %in% c("Date"))]
data_test <-  data_test[,!(names(data_test) %in% c("Date"))]

data_test <-  data_test[,!(names(data_test) %in% c("Price.Gas"))]




#define variables
#ridge regression
set.seed(42)

ridgereg <- cv.glmnet(data_train_wo_gas,
                      data_train_gas,
                      type.measure = "mse",
                      alpha=0,
                      family="gaussian")

ridgereg_pred <- predict(ridgereg, s=ridgereg$lambda.1se, newx=data_test_wo_gas)

gas$Date <- as.Date(gas$Date, format="%d/%m/%Y")

rr_model <- data.frame(year=gas$Date, price=gas$Price.Gas)
rr_model$plotpred <- c(gas$Price.Gas[seq(1,87)], ridgereg_pred)

year <- gas$Date
rr_real_price <- gas$Price.Gas
rr_pred_price <- rr_model$plotpred

#lasso regression
lassoreg <- cv.glmnet(data_train_wo_gas,
                      data_train_gas,
                      type.measure = "mse",
                      alpha=1,
                      family="gaussian")

lassoreg_pred <- predict(lassoreg, s=lassoreg$lambda.1se, newx=data_test_wo_gas)

lr_model <- data.frame(year=gas$Date, price=gas$Price.Gas)
lr_model$plotpred <- c(gas$Price.Gas[seq(1,87)], lassoreg_pred)

lr_real_price <- gas$Price.Gas
lr_pred_price <- lr_model$plotpred

#linear regression
lm_reg <- lm(Price.Gas~., data=data_train)
pred_lm= predict(lm_reg, newdata = data_test)

lm_model <- data.frame(year=gas$Date, price=gas$Price.Gas)
lm_model$plotpred <- c(gas$Price.Gas[seq(1,87)], pred_lm)

lm_real_price <- lm_model$price
lm_pred_price <- lm_model$plotpred

#decision tree
tree <- rpart(Price.Gas~Gbp.usd+Domestic.Production.from.UKCS+Import+Per.GDP.Growth+Average.Temperature+Price.Electricity+Gbp.usd+Gbp.eur+Price.ngl+Number.of.gas.switches+Electricity.churn.ratios+Price.lng+Price.coal+Price.gas.oil+Price.standard.grade.burning.oil+Price.diesel+Price.premium.unleaded+Price.super.unleaded+Price.wti.crude+Price.brent.crude,
              data=data_train,
              method='anova',
              parms = list(max.depth = 5,
                           min.node.size = 5,
                           mtry=auto,
                           splitrule=Gini))

prune_tree<- prune(tree, cp=0.01)

test_tree <- data.frame (data_test)
pred_dt <- predict(prune_tree, test_tree, method = "anova")

dt_model <- data.frame(year=gas$Date, price=gas$Price.Gas)
dt_model$plot_tree_pred <- c(gas$Price.Gas[seq(1,87)], pred_dt)

real_price_tree <- dt_model$price
pred_price_tree <- dt_model$plot_tree_pred

#random forest
trControl <- trainControl(method ="repeatedcv",
                          number = 10,
                          repeats=20,
                          search="grid")

rf_random <- randomForest(Price.Gas~.,
                          data=data_train,
                          method = "rf",
                          tuneLength = 5,
                          metric = "RMSE",
                          trControl = trControl,
                          parms = list(replace=TRUE,
                                       max.depth = 5,
                                       min.node.size = 5,
                                       sampsize=0.632*nrow(data_train)))

pred_rf_random= predict(rf_random, newdata = data_test)

rf_model_random <- data.frame(year=gas$Date, price=gas$Price.Gas)
rf_model_random$plotpred <- c(gas$Price.Gas[seq(1,87)], pred_rf_random)

rf_real_price <- rf_model_random$price
rf_pred_price <- rf_model_random$plotpred


#--------------------------------------------------------------------------
ui <- fluidPage(
  theme = shinythemes::shinytheme("superhero"),
  titlePanel("Time Series Forecasting of Gas Prices on Historical Data using Machine Learning"),
  
  sidebarPanel(
    radioButtons("typeInput", "Model",
                 choices = c("Linear Regression", "Ridge Regression", "Lasso Regression", "Decision Tree","Random Forest"))
  ),
  
  mainPanel(
    h4("Plot to show the predicted gas price trend from 2018 to 2020 in comparison to the actual price from 2010."),
    plotOutput("prediction_plots"),
    br(), br(),
    tableOutput("table")
  )
)



server <- function(input, output){
  
  output$table <- renderTable(gas_date)
  
  output$prediction_plots <- renderPlot({
    switch(input$typeInput,
           "Linear Regression" = ggplot(lm_model, aes(x=year, y=lm_pred_price)) +
             geom_point(color="red") +
             geom_line(aes(color="red")) +
             geom_line(aes(x=year, y=lm_real_price,color='black')) +
             labs(title="Linear Regression",
                  x="Year",
                  y="Gas Price") +
             scale_color_manual(labels = c("True Value", "Predicted Value"),
                                values=c('black','red')) +
             labs(color=' '),
           
           "Ridge Regression" = ggplot(rr_model, aes(x=year, y=rr_pred_price)) +
             geom_point(color="red") +
             geom_line(aes(color="red")) +
             geom_line(aes(x=year, y=rr_real_price,color='black')) +
             labs(title="Ridge Regression",
                  x="Year",
                  y="Gas Price") +
             scale_color_manual(labels = c("True Value", "Predicted Value"),
                                values=c('black','red')) +
             labs(color=' '),
           
           "Lasso Regression" = ggplot(lr_model, aes(x=year, y=lr_pred_price)) +
             geom_point(color="red") +
             geom_line(aes(color="red")) +
             geom_line(aes(x=year, y=lr_real_price,color='black')) +
             labs(title="Lasso Regression",
                  x="Year",
                  y="Gas Price") +
             scale_color_manual(labels = c("True Value", "Predicted Value"),
                                values=c('black','red')) +
             labs(color=' '),
           
           "Decision Tree"=ggplot(dt_model, aes(x=year, y=pred_price_tree)) +
             geom_point(color="red") +
             geom_line(aes(color="red")) +
             geom_line(aes(x=year, y=real_price_tree,color='black')) +
             labs(title="Decision Tree",
                  x="Year",
                  y="Gas Price") +
             scale_color_manual(labels = c("True Value", "Predicted Value"),
                                values=c('black','red')) +
             labs(color=' '),
           
           "Random Forest"=ggplot(rf_model_random, aes(x=year, y=rf_pred_price)) +
             geom_point(color="red") +
             geom_line(aes(color="red")) +
             geom_line(aes(x=year, y=rf_real_price, color='black')) +
             labs(title="Random Forest",
                  x="Year",
                  y="Gas Price") +
             scale_color_manual(labels = c("True Value", "Predicted Value"),
                                values=c('black','red')) +
             labs(color=' '))
    
  })
  
}


shinyApp(ui = ui, server = server)



