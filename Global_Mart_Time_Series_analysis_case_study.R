
################### Case Study: Retail-Giant Sales Forecasting  #######################

## Submitted by:

    ## Narendra Kumar
    ## Priyanka Srivastava
    ## Rajasekar Gopalakrishnan
    ## Venkatesh Singamsetty
    ## from the PG Diploma in Data Science June 2018 Cohort

## Submitted on 27-January-2019 ##

#######################################################################################

## Structure Of The Case Study Presentation Is Given Below

    # 1. Business Understanding
    # 2. Data Understanding
    # 3. Data Preparation
    # 4. Model Building 
    # 5. Model Evaluation 
    # 6. Conclusion

######################################################################################

# 1. Business Understanding: 

    # Global Mart is an online store super giant having worldwide operations. 
    # It takes orders and delivers across the globe and deals with all the 
    # major product categories - consumer, corporate & home office.

    # The objective of this assignment is to develop a model to forecast the 
    # sales and the demand for this retail store the next 6 months
    
    # The store caters to 7 different market segments and in 3 major categories.
    # Therefore,the forecast needs to be undertaken a this granular level
    
    # From the 21 market buckets, the analyst should determine the 2 most profitable 
    # (and consistent) segments and forecast the sales and demand for these segments.

######################################################################################

# 2. Data Understanding: 

    # The dataset provided captures the details of sales transactions at the point of sale
    # at Global Mart, including attributes describing the order, the shipment, customer
    # The market segment to which the product belongs, Market segment of which the customer
    # the product itself - including name, category and sub category
    # The columns "Profit", "Quantity" and "Sales" will be essential to determine
    # profitability of customer segments and to determine the forecast of sales and demand 
    # for these segments
    # Number of Observations: 51290
    # Number of Attributes: 24
    # Number of Markets:  7
    # Number of Segments: 3
    # Number of Market Segments: 21

######################################################################################

#3. Data Preparation: 

    ## Loading Neccessary libraries
    
    library(forecast)
    library(tseries)
    library(graphics)
    library(dplyr)
    library(ggplot2)
    
    ## Read The Source Data File
    
    global_mart_data <- read.csv("Global Superstore.csv", header = T, sep = ',')
    head(global_mart_data)
    str(global_mart_data)
    
    ## Check For Missing Values In The Data Set 
    sapply(global_mart_data, function(x) sum(is.na(x))) #Postal.Code Has 41296 Missing Values
    global_mart_data$Postal.Code<-NULL # Remove Postal.Code from Further Analysis

    ## Understanding Markets and Segments
    unique(global_mart_data$Segment) # Number of Segments = 3
    unique(global_mart_data$Market)  # Number of Markets  = 7 
    
    ## Create A New Column Called Market.Segment
    global_mart_data$Market.Segment <- paste(global_mart_data$Market, 
                                             global_mart_data$Segment, 
                                             sep=" - ") 
    
    ## Removing Columns Not Required For Analysis
    global_mart_data$Row.ID         <-  NULL
    global_mart_data$Order.ID       <-  NULL
    global_mart_data$Ship.Date      <-  NULL
    global_mart_data$Ship.Mode      <-  NULL
    global_mart_data$Customer.ID    <-  NULL
    global_mart_data$Customer.Name  <-  NULL
    global_mart_data$City           <-  NULL
    global_mart_data$State          <-  NULL
    global_mart_data$Country        <-  NULL
    global_mart_data$Region         <-  NULL
    global_mart_data$Product.ID     <-  NULL
    global_mart_data$Product.Name   <-  NULL
    global_mart_data$Category       <-  NULL
    global_mart_data$Sub.Category   <-  NULL
    global_mart_data$Order.Priority <-  NULL
    
    
    ## Converting Order Date Into Period (Month And Year Combination)
    global_mart_data$Order.Date   <- as.Date(global_mart_data$Order.Date, "%d-%m-%Y")
    global_mart_data$Period       <- format(global_mart_data$Order.Date, "%Y-%m")

    ## Group And Aggregate Sales, Quantity and Profit by Market.Segment
        ## for determing the segment with Highest Profits
    
    grouped_by_market_segment  <- group_by(global_mart_data, Market.Segment)
    summarised_by_mark_segment <- summarise(grouped_by_market_segment, 
                                           Sales_Total    = sum(Sales), 
                                           Sales_Average  = mean(Sales), 
                                           Profit_Total   = sum(Profit), 
                                           Profit_Average = mean(Profit),
                                           Profit_Percentage =((sum(Profit)/sum(Sales))*100), 
                                           Total_Quantity = sum(Quantity),
                                           Profit_SD      = sd(Profit),
                                           Profit_COV     = sd(Profit)/mean(Profit))
    
    # Rounding All Numeric Values to 2 Digits
    is.num <- sapply(summarised_by_mark_segment, is.numeric)
    summarised_by_mark_segment[is.num] <- lapply(summarised_by_mark_segment[is.num], round, 2)
    
    View(summarised_by_mark_segment$Market.Segment) # for presentation material purpose


    ggplot(summarised_by_mark_segment, 
           aes(Market.Segment, Profit_Total, fill = Market.Segment)) + 
           geom_bar(position = "dodge",
                    stat = "identity") #plotting total profit
    
    ggplot(summarised_by_mark_segment, 
           aes(Market.Segment, Profit_COV, fill = Market.Segment)) + 
      geom_bar(position = "dodge",
               stat = "identity")      #plotting COV of Profit
    
    ggplot(summarised_by_mark_segment, 
           aes(Market.Segment, Profit_Percentage, fill = Market.Segment)) + 
      geom_bar(position = "dodge",
               stat = "identity")      #plotting profitability or profit percentage

    
    ## Based on Highest Profits and Lowest Cofficient Of Variance,
    ## APAC - Consumer and EU - Consumer Are Chosen For Time Series Analysis
    ## All Three Segments of Canada Are High On Profitability, but are not chosen,
    ## As The Total Profit is Very Small
    
######################################################################################
# 4. Time Series Analysis - Model Building And Evaluation
######################################################################################
    
    ## Group And Aggregate Sales, Quantity and Profit by Market.Segment by Period
    ## using the global_mart_data data frame
    ## This will be used for the Time Series Analysis
    ## For APAC Consumer and EU Consumer
    
    
    grouped_by_period     <- group_by(global_mart_data, Market.Segment, Period)
    summarised_by_period  <- summarise(grouped_by_period, 
                                       TotalSalesAmount = sum(Sales), 
                                       TotalQuantity    = sum(Quantity))
                                       
    
    # Rounding All Numeric Values to 2 Digits
    is.num <- sapply(summarised_by_period, is.numeric)
    summarised_by_period[is.num] <- lapply(summarised_by_period[is.num], round, 2)
    
    APAC_Consumer <- summarised_by_period[ 
                     which(summarised_by_period$Market.Segment=='APAC - Consumer'),]
    
    EU_Consumer <- summarised_by_period[ 
                     which(summarised_by_period$Market.Segment=='EU - Consumer'),]
    
    colnames(APAC_Consumer)  <- c('MarketSegment', 'Month', 'Sales', 'Quantity')
    colnames(EU_Consumer)    <- c('MarketSegment', 'Month', 'Sales', 'Quantity')
    
## There Are Four Attributes To Be Forecast
    ## 1. APAC-Consumer Sales
    ## 2. APAC-Consumer Demand (Quantity)
    ## 3. EU-Consumer Sales
    ## 4. EU-Consumer Demand (Quantity)
    
    APAC_Consumer_Sales     <-APAC_Consumer[,c(2,3)]
    APAC_Consumer_Quantity  <-APAC_Consumer[,c(2,4)]
    
    EU_Consumer_Sales       <-EU_Consumer[,c(2,3)]
    EU_Consumer_Quantity    <-EU_Consumer[,c(2,4)]
    
    
    
    ## Dividing The Data Into Training And Test Data Sets
    ## Out of 48 obs in each data set, 42 will be Train and 6 in Test
    ## This is for all the 4 Attributes to be Forecast
    
    APAC_Consumer_Sales_Train     <- APAC_Consumer_Sales[1:42,]
    APAC_Consumer_Sales_Test      <- APAC_Consumer_Sales[43:48,]
    APAC_Consumer_Quantity_Train  <- APAC_Consumer_Quantity[1:42,]
    APAC_Consumer_Quantity_Test   <- APAC_Consumer_Quantity[43:48,]
    
    
    EU_Consumer_Sales_Train     <- EU_Consumer_Sales[1:42,]
    EU_Consumer_Sales_Test      <- EU_Consumer_Sales[43:48,]
    EU_Consumer_Quantity_Train  <- EU_Consumer_Quantity[1:42,]
    EU_Consumer_Quantity_Test   <- EU_Consumer_Quantity[43:48,]
    
    

######-----------------------APAC-Consumer Sales---------------------------#########
    
    
    
    # Converting dataframe to time series
    
    
    #Build and plot time series for Sales and Quantity
    plot(decompose(ts(APAC_Consumer_Sales_Train[,2],frequency=12)))
    
    # Decomposition shows that trend and seasonality both are sine/cosine plots for Sales 
    # and Quantity data and multiplicative in nature
    
    # Let's create the model using the first 42 rows. Then we can 
    # test the model on the remaining 6 rows later
    
    apac_sales_train_ts <- ts(APAC_Consumer_Sales_Train$Sales)
    plot(apac_sales_train_ts, main="APAC Consumer Sales Graph", xlab = "Time Period", ylab = "Total Sales")
    
    # Moving Average Smoothing
    
    w <-1
    smoothedseries_sales_APAC <- stats::filter(apac_sales_train_ts, 
                                     filter=rep(1/(2*w+1),(2*w+1)), 
                                     method='convolution', sides=2)
    
    # Smoothing left end of the time series
    
    diff <- smoothedseries_sales_APAC[w+2] - smoothedseries_sales_APAC[w+1]
    for (i in seq(w,1,-1)) {
      smoothedseries_sales_APAC[i] <- smoothedseries_sales_APAC[i+1] - diff
    }
    
    # Smoothing right end of the time series
    
      n <- length(apac_sales_train_ts)
    diff <- smoothedseries_sales_APAC[n-w] - smoothedseries_sales_APAC[n-w-1]
    for (i in seq(n-w+1, n)) {
      smoothedseries_sales_APAC[i] <- smoothedseries_sales_APAC[i-1] + diff
    }
    
    # Plot the smoothed time series
    
    plot(apac_sales_train_ts, 
         main="APAC Consumer Sales Graph", 
         xlab = "Time Period", 
         ylab = "Total Sales")
    lines(smoothedseries_sales_APAC, col="blue", lwd=2)

    
    # In both the Smoothing, It Is Evident That There Is A Seasonality Of the Data, 
    # As Well As A Trend.

    # Building a model on the smoothed time series using classical decomposition
    # First, let's convert the time series to a dataframe
    
    timevals_in                             <- c(1:42)
    smoothedseries_sales_df_APAC            <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries_sales_APAC)))
    colnames(smoothedseries_sales_df_APAC)  <- c('Month', 'Sales')
    
    # Model Fitment Using lm Function
    # Now, let's fit a multiplicative model with trend and seasonality to the data

    lmfit_sales_APAC        <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + 
                                          cos(0.5*Month) * poly(Month,3) + 
                                          Month, 
                                  data=smoothedseries_sales_df_APAC)
    accuracy(lmfit_sales_APAC)
    global_pred_sales_APAC <- predict(lmfit_sales_APAC, Month=timevals_in)
    summary(global_pred_sales_APAC)
    lines(timevals_in, global_pred_sales_APAC, col='red', lwd=2)
    
    # Now, let's look at the locally predictable series and 
    # We will model it as an ARMA series
    
    local_pred_sales_APAC <- apac_sales_train_ts - global_pred_sales_APAC
    plot(local_pred_sales_APAC, col='green', type = "l")
    acf(local_pred_sales_APAC)
    acf(local_pred_sales_APAC, type="partial")
    armafit_sales_APAC<- auto.arima(local_pred_sales_APAC)
    
    tsdiag(armafit_sales_APAC)
    armafit_sales_APAC
        # ARIMA(0,0,0) with zero mean 
        # log likelihood=-443.75
        # AIC=889.49   AICc=889.59   BIC=891.23
    
    # We'll check if the residual series is white noise
    
    residual_sales_APAC <- local_pred_sales_APAC - fitted(armafit_sales_APAC)
    plot(residual_sales_APAC, col='blue', type = "l")
    adf.test(residual_sales_APAC,alternative = "stationary") # p-value = 0.01
    kpss.test(residual_sales_APAC)                           # p-value = 0.1
    
    #Now, let's evaluate the model using MAPE
    #First, let's make a prediction for the last 6 months
    
    timevals_out               <- c(43:48)
    global_pred_out_sales_APAC <- predict(lmfit_sales_APAC, 
                                          data.frame(Month = timevals_out))
    fcast_sales_APAC           <- global_pred_out_sales_APAC
    
    #Now, let's compare our prediction with the actual values, using MAPE
   
    MAPE_class_dec_sales_APAC <- accuracy(fcast_sales_APAC, APAC_Consumer_Sales_Test$Sales)[5]
    MAPE_class_dec_sales_APAC # MAPE = 31.07429
    
    #Let's also plot the predictions along with original values, to
    #get a visual feel of the fit
    
    class_dec_pred_sales_APAC <- c(ts(global_pred_sales_APAC),ts(global_pred_out_sales_APAC))
    plot(ts(APAC_Consumer_Sales$Sales), col = "black")
    lines(class_dec_pred_sales_APAC, col = "red")
    
    # Forecasting For Next 6 Months
    
    APAC_Sales_nextSix <- predict(lmfit_sales_APAC,data.frame(Month=seq(1:54)))[49:54]
    APAC_Sales_nextSix
    # Period  49       50       51       52       53       54 
    # Sales   48894.49 55229.36 59321.81 57988.42 49177.89 33068.79 
    
    #So, that was classical decomposition, now let's do an ARIMA fit
    
    autoarima_sales_APAC <- auto.arima(apac_sales_train_ts)
    autoarima_sales_APAC
        # ARIMA(0,1,1)
        # log likelihood=-447.11
        # AIC=898.23   AICc=898.55   BIC=901.66
    
    tsdiag(autoarima_sales_APAC)
    plot(autoarima_sales_APAC$x, col="black")
    lines(fitted(autoarima_sales_APAC), col="red")
    
    #Again, let's check if the residual series is white noise
    
    resi_auto_arima_sales_APAC <- apac_sales_train_ts - fitted(autoarima_sales_APAC)
    adf.test(resi_auto_arima_sales_APAC, alternative = "stationary") # p-value = 0.01
    kpss.test(resi_auto_arima_sales_APAC)                            # p-value = 0.1
    
    #Also, let's evaluate the model using MAPE
    fcast_auto_arima_sales_APAC <- predict(autoarima_sales_APAC, n.ahead = 6)
    
    MAPE_auto_arima_sales_APAC <- accuracy(fcast_auto_arima_sales_APAC$pred, 
                                           APAC_Consumer_Sales_Test$Sales)[5]
    MAPE_auto_arima_sales_APAC # MAPE = 27.68952
    
    #Lastly, let's plot the predictions along with original values, to
    #get a visual feel of the fit
    
    auto_arima_pred_sales_APAC <- c(fitted(autoarima_sales_APAC),
                                    ts(fcast_auto_arima_sales_APAC$pred))
    plot(ts(APAC_Consumer_Sales$Sales), col = "black")
    lines(auto_arima_pred_sales_APAC, col = "red")
    
    
    ## For Forecasting The Next 6 Months Sales for APAC Consumer Using Arima
    
    APAC_Sales_nextSix_autoArima <- predict(autoarima_sales_APAC, n.ahead = 12)$pred
    APAC_Sales_nextSix_autoArima[7:12]
    # Period  49       50       51       52       53       54 
    # Sales   44898.7 44898.7 44898.7 44898.7 44898.7 44898.7
    
    # This concludes the model building using Moving Average Smoothing
    # MAPE_class_dec  = 31.07429
    # MAPE_auto_arima = 27.68952
    
    timevals_fcst         <- c(49:54)
    apac_sales_forecast   <- predict(lmfit_sales_APAC, data.frame(Month = timevals_fcst))
    model_pred            <- c(ts(global_pred_sales_APAC),ts(global_pred_out_sales_APAC))
    forcast_pred          <- c(ts(global_pred_sales_APAC),ts(global_pred_out_sales_APAC),
                               ts(apac_sales_forecast))
    plot(ts(APAC_Consumer$Sales), 
         main="APAC Consumer Sales Forecast Graph", 
         xlab = "Time Period", 
         ylab = "Total Sales",
         xlim=c(0, 55))
    lines(forcast_pred, col = "blue", lwd=2)
    lines(model_pred, col = "red", lwd=2)

######-----------------------APAC-Consumer Demand (Quantity)---------------------------#########
    
    # Converting dataframe to time series
    
    #Build and plot time series for Quantity
    plot(decompose(ts(APAC_Consumer_Quantity_Train[,2],frequency=12)))
    
    # Decomposition shows that trend and seasonality both are sine/cosine plots for 
    # Quantity data and multiplicative in nature
    
    # Let's create the model using the first 42 rows. Then we can 
    # test the model on the remaining 6 rows later
    
    apac_quantity_train_ts <- ts(APAC_Consumer_Quantity_Train$Quantity)
    plot(apac_quantity_train_ts, 
         main="APAC Consumer Quantity Graph", 
         xlab = "Time Period", 
         ylab = "Total Quantity")
    
    # Moving Average Smoothing
    
    w <-1
    smoothedseries_quantity_APAC <- stats::filter(apac_quantity_train_ts, 
                                                  filter=rep(1/(2*w+1),(2*w+1)), 
                                                  method='convolution', sides=2)
    
    # Smoothing left end of the time series
    
    diff <- smoothedseries_quantity_APAC[w+2] - smoothedseries_quantity_APAC[w+1]
    for (i in seq(w,1,-1)) {
      smoothedseries_quantity_APAC[i] <- smoothedseries_quantity_APAC[i+1] - diff
    }
    
    # Smoothing right end of the time series
    
    n <- length(apac_quantity_train_ts)
    diff <- smoothedseries_quantity_APAC[n-w] - smoothedseries_quantity_APAC[n-w-1]
    for (i in seq(n-w+1, n)) {
      smoothedseries_quantity_APAC[i] <- smoothedseries_quantity_APAC[i-1] + diff
    }
    
    # Plot the smoothed time series
    
    plot(apac_quantity_train_ts, 
         main="APAC Consumer Demand Graph", 
         xlab = "Time Period", 
         ylab = "Total Demand")
    lines(smoothedseries_quantity_APAC, col="blue", lwd=2)
    
    
    # In both the Smoothing, It Is Evident That There Is A Seasonality Of the Data, 
    # As Well As A Trend.
    
    # Building a model on the smoothed time series using classical decomposition
    # First, let's convert the time series to a dataframe
    
    timevals_in                             <- c(1:42)
    smoothedseries_quantity_df_APAC            <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries_quantity_APAC)))
    colnames(smoothedseries_quantity_df_APAC)  <- c('Month', 'Quantity')
    
    # Model Fitment Using lm Function
    # Now, let's fit a multiplicative model with trend and seasonality to the data
    
    lmfit_quantity_APAC        <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + 
                                       cos(0.5*Month) * poly(Month,3) + 
                                       Month, 
                                     data=smoothedseries_quantity_df_APAC)
    accuracy(lmfit_quantity_APAC)
    global_pred_quantity_APAC <- predict(lmfit_quantity_APAC, Month=timevals_in)
    summary(global_pred_quantity_APAC)
    lines(timevals_in, global_pred_quantity_APAC, col='red', lwd=2)
    
    # Now, let's look at the locally predictable series and 
    # We will model it as an ARMA series
    
    local_pred_quantity_APAC <- apac_quantity_train_ts - global_pred_quantity_APAC
    plot(local_pred_quantity_APAC, col='green', type = "l")
    acf(local_pred_quantity_APAC)
    acf(local_pred_quantity_APAC, type="partial")
    armafit_quantity_APAC<- auto.arima(local_pred_quantity_APAC)
    
    tsdiag(armafit_quantity_APAC)
    armafit_quantity_APAC
    # ARIMA(0,0,0) with zero mean 
    # log likelihood=-253.71
    # AIC=509.42   AICc=509.52   BIC=511.16
    
    # We'll check if the residual series is white noise
    
    residual_quantity_APAC <- local_pred_quantity_APAC - fitted(armafit_quantity_APAC)
    plot(residual_quantity_APAC, col='blue', type = "l")
    adf.test(residual_quantity_APAC,alternative = "stationary") # p-value = 0.01
    kpss.test(residual_quantity_APAC)                           # p-value = 0.1
    
    #Now, let's evaluate the model using MAPE
    #First, let's make a prediction for the last 6 months
    
    timevals_out               <- c(43:48)
    global_pred_out_quantity_APAC <- predict(lmfit_quantity_APAC, 
                                             data.frame(Month = timevals_out))
    fcast_quantity_APAC           <- global_pred_out_quantity_APAC
    
    #Now, let's compare our prediction with the actual values, using MAPE
    
    MAPE_class_dec_quantity_APAC <- accuracy(fcast_quantity_APAC, APAC_Consumer_Quantity_Test$Quantity)[5]
    MAPE_class_dec_quantity_APAC # MAPE = 62.10289
    
    #Let's also plot the predictions along with original values, to
    #get a visual feel of the fit
    
    class_dec_pred_quantity_APAC <- c(ts(global_pred_quantity_APAC),ts(global_pred_out_quantity_APAC))
    plot(ts(APAC_Consumer_Quantity$Quantity), col = "black")
    lines(class_dec_pred_quantity_APAC, col = "red")
    
    # Forecasting For Next 6 Months
    
    APAC_Quantity_nextSix <- predict(lmfit_quantity_APAC,data.frame(Month=seq(1:54)))[49:54]
    APAC_Quantity_nextSix
    # Period     49        50        51        52        53        54 
    # Demand   1008.2773  770.4048  539.7742  393.1554  404.5120  623.5851  
    
    #So, that was classical decomposition, now let's do an ARIMA fit
    
    autoarima_quantity_APAC <- auto.arima(apac_quantity_train_ts)
    autoarima_quantity_APAC
    # ARIMA(0,1,0)
    # log likelihood=-266.07
    # AIC=534.14   AICc=534.24   BIC=535.85
    
    tsdiag(autoarima_quantity_APAC)
    plot(autoarima_quantity_APAC$x, col="black")
    lines(fitted(autoarima_quantity_APAC), col="red")
    
    #Again, let's check if the residual series is white noise
    
    resi_auto_arima_quantity_APAC <- apac_quantity_train_ts - fitted(autoarima_quantity_APAC)
    adf.test(resi_auto_arima_quantity_APAC, alternative = "stationary") # p-value = 0.01
    kpss.test(resi_auto_arima_quantity_APAC)                            # p-value = 0.1
    
    #Also, let's evaluate the model using MAPE
    fcast_auto_arima_quantity_APAC <- predict(autoarima_quantity_APAC, n.ahead = 6)
    
    MAPE_auto_arima_quantity_APAC <- accuracy(fcast_auto_arima_quantity_APAC$pred, 
                                              APAC_Consumer_Quantity_Test$Quantity)[5]
    MAPE_auto_arima_quantity_APAC # MAPE = 26.24458
    
    #Lastly, let's plot the predictions along with original values, to
    #get a visual feel of the fit
    
    auto_arima_pred_quantity_APAC <- c(fitted(autoarima_quantity_APAC),
                                       ts(fcast_auto_arima_quantity_APAC$pred))
    plot(ts(APAC_Consumer_Quantity$Quantity), col = "black")
    lines(auto_arima_pred_quantity_APAC, col = "red")
    
    
    ## For Forecasting The Next 6 Months Demand for APAC Consumer Using Arima
    
    APAC_Quantity_nextSix_autoArima <- predict(autoarima_quantity_APAC, n.ahead = 12)$pred
    APAC_Quantity_nextSix_autoArima[7:12]
    # Period    49    50    51    52    53    54 
    # Demand   721   721   721   721   721   721
    
    # This concludes the model building using Moving Average Smoothing
    # MAPE_class_dec  = 62.10289
    # MAPE_auto_arima = 26.24458
    
    timevals_fcst         <- c(49:54)
    apac_quantity_forecast   <- predict(lmfit_quantity_APAC, data.frame(Month = timevals_fcst))
    model_pred            <- c(ts(global_pred_quantity_APAC),ts(global_pred_out_quantity_APAC))
    forcast_pred          <- c(ts(global_pred_quantity_APAC),ts(global_pred_out_quantity_APAC),
                               ts(apac_quantity_forecast))
    plot(ts(APAC_Consumer$Quantity), 
         main="APAC Consumer Demand Forecast Graph", 
         xlab = "Time Period", 
         ylab = "Total Demand",
         xlim=c(0, 55))
    lines(forcast_pred, col = "blue", lwd=2)
    lines(model_pred, col = "red", lwd=2)
    
    
    ######-----------------------EU-Consumer Sales---------------------------#########
    
    
    
    # Converting dataframe to time series
    
    
    #Build and plot time series for Sales and Quantity
    plot(decompose(ts(EU_Consumer_Sales_Train[,2],frequency=12)))
    
    # Decomposition shows that trend and seasonality both are sine/cosine plots for Sales 
    # and Quantity data and multiplicative in nature
    
    # Let's create the model using the first 42 rows. Then we can 
    # test the model on the remaining 6 rows later
    
    EU_sales_train_ts <- ts(EU_Consumer_Sales_Train$Sales)
    plot(EU_sales_train_ts, main="EU Consumer Sales Graph", xlab = "Time Period", ylab = "Total Sales")
    
    # Moving Average Smoothing
    
    w <-1
    smoothedseries_sales_EU <- stats::filter(EU_sales_train_ts, 
                                             filter=rep(1/(2*w+1),(2*w+1)), 
                                             method='convolution', sides=2)
    
    # Smoothing left end of the time series
    
    diff <- smoothedseries_sales_EU[w+2] - smoothedseries_sales_EU[w+1]
    for (i in seq(w,1,-1)) {
      smoothedseries_sales_EU[i] <- smoothedseries_sales_EU[i+1] - diff
    }
    
    # Smoothing right end of the time series
    
    n <- length(EU_sales_train_ts)
    diff <- smoothedseries_sales_EU[n-w] - smoothedseries_sales_EU[n-w-1]
    for (i in seq(n-w+1, n)) {
      smoothedseries_sales_EU[i] <- smoothedseries_sales_EU[i-1] + diff
    }
    
    # Plot the smoothed time series
    
    plot(EU_sales_train_ts, 
         main="EU Consumer Sales Graph", 
         xlab = "Time Period", 
         ylab = "Total Sales")
    lines(smoothedseries_sales_EU, col="blue", lwd=2)
    
    
    # In both the Smoothing, It Is Evident That There Is A Seasonality Of the Data, 
    # As Well As A Trend.
    
    # Building a model on the smoothed time series using classical decomposition
    # First, let's convert the time series to a dataframe
    
    timevals_in                             <- c(1:42)
    smoothedseries_sales_df_EU            <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries_sales_EU)))
    colnames(smoothedseries_sales_df_EU)  <- c('Month', 'Sales')
    
    # Model Fitment Using lm Function
    # Now, let's fit a multiplicative model with trend and seasonality to the data
    
    lmfit_sales_EU        <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + 
                                  cos(0.5*Month) * poly(Month,3) + 
                                  Month, 
                                data=smoothedseries_sales_df_EU)
    accuracy(lmfit_sales_EU)
    global_pred_sales_EU <- predict(lmfit_sales_EU, Month=timevals_in)
    summary(global_pred_sales_EU)
    lines(timevals_in, global_pred_sales_EU, col='red', lwd=2)
    
    # Now, let's look at the locally predictable series and 
    # We will model it as an ARMA series
    
    local_pred_sales_EU <- EU_sales_train_ts - global_pred_sales_EU
    plot(local_pred_sales_EU, col='green', type = "l")
    acf(local_pred_sales_EU)
    acf(local_pred_sales_EU, type="partial")
    armafit_sales_EU<- auto.arima(local_pred_sales_EU)
    
    tsdiag(armafit_sales_EU)
    armafit_sales_EU
    # ARIMA(0,0,0) with zero mean 
    # log likelihood=-444.8
    # AIC=891.61   AICc=891.71   BIC=893.35
    
    # We'll check if the residual series is white noise
    
    residual_sales_EU <- local_pred_sales_EU - fitted(armafit_sales_EU)
    plot(residual_sales_EU, col='blue', type = "l")
    adf.test(residual_sales_EU,alternative = "stationary") # p-value = 0.01
    kpss.test(residual_sales_EU)                           # p-value = 0.1
    
    #Now, let's evaluate the model using MAPE
    #First, let's make a prediction for the last 6 months
    
    timevals_out               <- c(43:48)
    global_pred_out_sales_EU <- predict(lmfit_sales_EU, 
                                        data.frame(Month = timevals_out))
    fcast_sales_EU           <- global_pred_out_sales_EU
    
    #Now, let's compare our prediction with the actual values, using MAPE
    
    MAPE_class_dec_sales_EU <- accuracy(fcast_sales_EU, EU_Consumer_Sales_Test$Sales)[5]
    MAPE_class_dec_sales_EU # MAPE = 92.95789
    
    #Let's also plot the predictions along with original values, to
    #get a visual feel of the fit
    
    class_dec_pred_sales_EU <- c(ts(global_pred_sales_EU),ts(global_pred_out_sales_EU))
    plot(ts(EU_Consumer_Sales$Sales), col = "black")
    lines(class_dec_pred_sales_EU, col = "red")
    
    # Forecasting For Next 6 Months
    
    EU_Sales_nextSix <- predict(lmfit_sales_EU,data.frame(Month=seq(1:54)))[49:54]
    EU_Sales_nextSix
    # Period  49       50       51       52       53       54 
    # Sales   82488.88 58997.31 39711.49 33652.92 48160.30 86304.57  
    
    #So, that was classical decomposition, now let's do an ARIMA fit
    
    autoarima_sales_EU <- auto.arima(EU_sales_train_ts)
    autoarima_sales_EU
    # ARIMA(2,1,0) 
    # log likelihood=-445.84
    # AIC=897.67   AICc=898.32   BIC=902.81
    
    tsdiag(autoarima_sales_EU)
    plot(autoarima_sales_EU$x, col="black")
    lines(fitted(autoarima_sales_EU), col="red")
    
    #Again, let's check if the residual series is white noise
    
    resi_auto_arima_sales_EU <- EU_sales_train_ts - fitted(autoarima_sales_EU)
    adf.test(resi_auto_arima_sales_EU, alternative = "stationary") # p-value = 0.01
    kpss.test(resi_auto_arima_sales_EU)                            # p-value = 0.1
    
    #Also, let's evaluate the model using MAPE
    fcast_auto_arima_sales_EU <- predict(autoarima_sales_EU, n.ahead = 6)
    
    MAPE_auto_arima_sales_EU <- accuracy(fcast_auto_arima_sales_EU$pred, 
                                         EU_Consumer_Sales_Test$Sales)[5]
    MAPE_auto_arima_sales_EU # MAPE = 28.9226
    
    #Lastly, let's plot the predictions along with original values, to
    #get a visual feel of the fit
    
    auto_arima_pred_sales_EU <- c(fitted(autoarima_sales_EU),
                                  ts(fcast_auto_arima_sales_EU$pred))
    plot(ts(EU_Consumer_Sales$Sales), col = "black")
    lines(auto_arima_pred_sales_EU, col = "red")
    
    
    ## For Forecasting The Next 6 Months Sales for EU Consumer Using Arima
    
    EU_Sales_nextSix_autoArima <- predict(autoarima_sales_EU, n.ahead = 12)$pred
    EU_Sales_nextSix_autoArima[7:12]
    # Period  49       50       51       52       53       54 
    # Sales   40288.07 39651.62 40168.29 40181.05 39920.19 40065.13
    
    # This concludes the model building using Moving Average Smoothing
    # MAPE_class_dec  = 31.07429
    # MAPE_auto_arima = 27.68952
    
    timevals_fcst         <- c(49:54)
    EU_sales_forecast   <- predict(lmfit_sales_EU, data.frame(Month = timevals_fcst))
    model_pred            <- c(ts(global_pred_sales_EU),ts(global_pred_out_sales_EU))
    forcast_pred          <- c(ts(global_pred_sales_EU),ts(global_pred_out_sales_EU),
                               ts(EU_sales_forecast))
    plot(ts(EU_Consumer$Sales), 
         main="EU Consumer Sales Forecast Graph", 
         xlab = "Time Period", 
         ylab = "Total Sales",
         xlim=c(0, 55))
    lines(forcast_pred, col = "blue", lwd=2)
    lines(model_pred, col = "red", lwd=2)
    
    ######-----------------------EU-Consumer Demand (Quantity)---------------------------#########
    
    # Converting dataframe to time series
    
    #Build and plot time series for Quantity
    plot(decompose(ts(EU_Consumer_Quantity_Train[,2],frequency=12)))
    
    # Decomposition shows that trend and seasonality both are sine/cosine plots for 
    # Quantity data and multiplicative in nature
    
    # Let's create the model using the first 42 rows. Then we can 
    # test the model on the remaining 6 rows later
    
    EU_quantity_train_ts <- ts(EU_Consumer_Quantity_Train$Quantity)
    plot(EU_quantity_train_ts, 
         main="EU Consumer Quantity Graph", 
         xlab = "Time Period", 
         ylab = "Total Quantity")
    
    # Moving Average Smoothing
    
    w <-1
    smoothedseries_quantity_EU <- stats::filter(EU_quantity_train_ts, 
                                                filter=rep(1/(2*w+1),(2*w+1)), 
                                                method='convolution', sides=2)
    
    # Smoothing left end of the time series
    
    diff <- smoothedseries_quantity_EU[w+2] - smoothedseries_quantity_EU[w+1]
    for (i in seq(w,1,-1)) {
      smoothedseries_quantity_EU[i] <- smoothedseries_quantity_EU[i+1] - diff
    }
    
    # Smoothing right end of the time series
    
    n <- length(EU_quantity_train_ts)
    diff <- smoothedseries_quantity_EU[n-w] - smoothedseries_quantity_EU[n-w-1]
    for (i in seq(n-w+1, n)) {
      smoothedseries_quantity_EU[i] <- smoothedseries_quantity_EU[i-1] + diff
    }
    
    # Plot the smoothed time series
    
    plot(EU_quantity_train_ts, 
         main="EU Consumer Demand Graph", 
         xlab = "Time Period", 
         ylab = "Total Demand")
    lines(smoothedseries_quantity_EU, col="blue", lwd=2)
    
    
    # In both the Smoothing, It Is Evident That There Is A Seasonality Of the Data, 
    # As Well As A Trend.
    
    # Building a model on the smoothed time series using classical decomposition
    # First, let's convert the time series to a dataframe
    
    timevals_in                             <- c(1:42)
    smoothedseries_quantity_df_EU            <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries_quantity_EU)))
    colnames(smoothedseries_quantity_df_EU)  <- c('Month', 'Quantity')
    
    # Model Fitment Using lm Function
    # Now, let's fit a multiplicative model with trend and seasonality to the data
    
    lmfit_quantity_EU        <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + 
                                     cos(0.5*Month) * poly(Month,3) + 
                                     Month, 
                                   data=smoothedseries_quantity_df_EU)
    accuracy(lmfit_quantity_EU)
    global_pred_quantity_EU <- predict(lmfit_quantity_EU, Month=timevals_in)
    summary(global_pred_quantity_EU)
    lines(timevals_in, global_pred_quantity_EU, col='red', lwd=2)
    
    # Now, let's look at the locally predictable series and 
    # We will model it as an ARMA series
    
    local_pred_quantity_EU <- EU_quantity_train_ts - global_pred_quantity_EU
    plot(local_pred_quantity_EU, col='green', type = "l")
    acf(local_pred_quantity_EU)
    acf(local_pred_quantity_EU, type="partial")
    armafit_quantity_EU<- auto.arima(local_pred_quantity_EU)
    
    tsdiag(armafit_quantity_EU)
    armafit_quantity_EU
    # ARIMA(2,0,0) with zero mean
    # log likelihood=-245.89
    # AIC=497.79   AICc=498.42   BIC=503
    
    # We'll check if the residual series is white noise
    
    residual_quantity_EU <- local_pred_quantity_EU - fitted(armafit_quantity_EU)
    plot(residual_quantity_EU, col='blue', type = "l")
    adf.test(residual_quantity_EU,alternative = "stationary") # p-value = 0.01
    kpss.test(residual_quantity_EU)                           # p-value = 0.1
    
    #Now, let's evaluate the model using MAPE
    #First, let's make a prediction for the last 6 months
    
    timevals_out               <- c(43:48)
    global_pred_out_quantity_EU <- predict(lmfit_quantity_EU, 
                                           data.frame(Month = timevals_out))
    fcast_quantity_EU           <- global_pred_out_quantity_EU
    
    #Now, let's compare our prediction with the actual values, using MAPE
    
    MAPE_class_dec_quantity_EU <- accuracy(fcast_quantity_EU, EU_Consumer_Quantity_Test$Quantity)[5]
    MAPE_class_dec_quantity_EU # MAPE = 30.39741
    
    #Let's also plot the predictions along with original values, to
    #get a visual feel of the fit
    
    class_dec_pred_quantity_EU <- c(ts(global_pred_quantity_EU),ts(global_pred_out_quantity_EU))
    plot(ts(EU_Consumer_Quantity$Quantity), col = "black")
    lines(class_dec_pred_quantity_EU, col = "red")
    
    # Forecasting For Next 6 Months
    
    EU_Quantity_nextSix <- predict(lmfit_quantity_EU,data.frame(Month=seq(1:54)))[49:54]
    EU_Quantity_nextSix
    # Period     49       50       51        52       53      54 
    # Demand   590.6191 540.0785 532.8207 587.1292 707.3636 880.7869   
    
    #So, that was classical decomposition, now let's do an ARIMA fit
    
    autoarima_quantity_EU <- auto.arima(EU_quantity_train_ts)
    autoarima_quantity_EU
    # ARIMA(2,1,0) 
    # log likelihood=-261.9
    # AIC=529.8   AICc=530.44   BIC=534.94
    
    tsdiag(autoarima_quantity_EU)
    plot(autoarima_quantity_EU$x, col="black")
    lines(fitted(autoarima_quantity_EU), col="red")
    
    #Again, let's check if the residual series is white noise
    
    resi_auto_arima_quantity_EU <- EU_quantity_train_ts - fitted(autoarima_quantity_EU)
    adf.test(resi_auto_arima_quantity_EU, alternative = "stationary") # p-value = 0.04521
    kpss.test(resi_auto_arima_quantity_EU)                            # p-value = 0.1
    
    #Also, let's evaluate the model using MAPE
    fcast_auto_arima_quantity_EU <- predict(autoarima_quantity_EU, n.ahead = 6)
    
    MAPE_auto_arima_quantity_EU <- accuracy(fcast_auto_arima_quantity_EU$pred, 
                                            EU_Consumer_Quantity_Test$Quantity)[5]
    MAPE_auto_arima_quantity_EU # MAPE = 30.13319
    
    #Lastly, let's plot the predictions along with original values, to
    #get a visual feel of the fit
    
    auto_arima_pred_quantity_EU <- c(fitted(autoarima_quantity_EU),
                                     ts(fcast_auto_arima_quantity_EU$pred))
    plot(ts(EU_Consumer_Quantity$Quantity), col = "black")
    lines(auto_arima_pred_quantity_EU, col = "red")
    
    
    ## For Forecasting The Next 6 Months Demand for EU Consumer Using Arima
    
    EU_Quantity_nextSix_autoArima <- predict(autoarima_quantity_EU, n.ahead = 12)$pred
    EU_Quantity_nextSix_autoArima[7:12]
    # Period    49        50        51      52        53      54 
    # Demand   466.2458 463.7401 472.9520 467.6464 466.1350 470.3663
    
    # This concludes the model building using Moving Average Smoothing
    # MAPE_class_dec  = 62.10289
    # MAPE_auto_arima = 26.24458
    
    timevals_fcst         <- c(49:54)
    EU_quantity_forecast   <- predict(lmfit_quantity_EU, data.frame(Month = timevals_fcst))
    model_pred            <- c(ts(global_pred_quantity_EU),ts(global_pred_out_quantity_EU))
    forcast_pred          <- c(ts(global_pred_quantity_EU),ts(global_pred_out_quantity_EU),
                               ts(EU_quantity_forecast))
    plot(ts(EU_Consumer$Quantity), 
         main="EU Consumer Demand Forecast Graph", 
         xlab = "Time Period", 
         ylab = "Total Demand",
         xlim=c(0, 55))
    lines(forcast_pred, col = "blue", lwd=2)
    lines(model_pred, col = "red", lwd=2)

##----------------------- Model Evaluation and Forecasting Complete ----------------##    
######################################################################################     
    
# The Key Findings And The Forecast Recommendations Are Given In The Attached
# Presentation
    
##----------------------------  Assignment Complete --------------------------------##    
###################################################################################### 