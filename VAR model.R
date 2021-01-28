# Building Vector Auto Regressive Model for predicting the Residential House prices in USA
#Installing Pacakges 

install.packages("fredr")
install.packages("vars")
install.packages("tidyselect")
install.packages('mFilter')
install.packages("tseries")
install.packages('TSstudio')
install.packages('forecast')


# Loading Libraries

library('fredr')
library ("graphics")
library("quantmod")
library('tidyr')
library('dplyr')
library('ggplot2')
library('plotly')
library('vars')
library('tidyselect')
library('mFilter')
library('tseries')
library('forecast')

getSymbols.FRED

# Connecting to FRED Restful API using "FREDR" by API key

#?fredr_set_key() 
key <- '36e28af44d38a4d8fcb52c2af6a324c9'
fredr_set_key(key)

# Calling the FRED Economic series Data

monthly_supply_homes           <- fredr(series_id = "MSACSR",          observation_start = as.Date("2000-01-01"),  observation_end = as.Date("2019-01-01")) 
unemployment_rate              <- fredr(series_id = "UNRATE",          observation_start = as.Date("2000-01-01"),  observation_end = as.Date("2019-01-01")) 
federal_funds_interest_rate    <- fredr(series_id = "FEDFUNDS",        observation_start = as.Date("2000-01-01"),  observation_end = as.Date("2019-01-01")) 
recession                      <- fredr(series_id = "RECPROUSM156N",   observation_start = as.Date("2000-01-01"),  observation_end = as.Date("2019-01-01")) 
housing_starts                 <- fredr(series_id = "HOUST",           observation_start = as.Date("2000-01-01"),  observation_end = as.Date("2019-01-01")) 
case_shiller_price_index       <- fredr(series_id = "CSUSHPINSA",      observation_start = as.Date("2000-01-01"),  observation_end = as.Date("2019-01-01"))

# Analyzing the structure of the data

head(monthly_supply_homes)
head(unemployment_rate)
head(federal_funds_interest_rate)
head(recession)
head(housing_starts)
head(case_shiller_price_index)

# Generating Real Time Graph for the Economic series

getSymbols("MSACSR", src="FRED")
chartSeries(MSACSR)

getSymbols("UNRATE", src="FRED")
chartSeries(UNRATE)

getSymbols("FEDFUNDS", src="FRED")
chartSeries(FEDFUNDS)


getSymbols("RECPROUSM156N", src="FRED")
chartSeries(RECPROUSM156N)

getSymbols("HOUST", src="FRED")
chartSeries(HOUST)


getSymbols("CSUSHPINSA", src="FRED")
chartSeries(CSUSHPINSA)


# Subsetting the Economic series to Time and Series columns 

monthly_supply_homes              <- subset(monthly_supply_homes ,         select = c(1,3))
unemployment_rate                 <- subset(unemployment_rate ,            select = c(1,3))
federal_funds_interest_rate       <- subset(federal_funds_interest_rate ,  select = c(1,3))
recession                         <- subset(recession ,                    select = c(1,3))
housing_starts                    <- subset(housing_starts ,               select = c(1,3))
case_shiller_price_index          <- subset(case_shiller_price_index ,     select = c(1,3))

# Renameing to technical names 

monthly_supply_homes              <- rename(monthly_supply_homes ,            msh  = value)
unemployment_rate                 <- rename(unemployment_rate ,               uer  = value)
federal_funds_interest_rate       <- rename(federal_funds_interest_rate ,     ffir = value)
recession                         <- rename(recession ,                       rec  = value)
housing_starts                    <- rename(housing_starts ,                  hs   = value)
case_shiller_price_index          <- rename(case_shiller_price_index ,        cpi  = value)


# Visuilizing the Data 

monthly_supply_homes_plot                  <- ggplot(monthly_supply_homes ,            aes(x = date , y = msh ))  + geom_line(color = 'red') 
monthly_supply_homes_plot

unemployment_rate_plot                     <- ggplot(unemployment_rate ,               aes(x = date , y = uer ))  + geom_line(color = 'blue') 
unemployment_rate_plot

federal_funds_interest_rate_plot           <- ggplot(federal_funds_interest_rate ,     aes(x = date , y = ffir )) + geom_line(color = 'red') 
federal_funds_interest_rate_plot

recession_plot                             <- ggplot(recession ,                       aes(x = date , y = rec ))  + geom_line(color = 'red') 
recession_plot

housing_starts_plot                        <- ggplot(housing_starts ,                  aes(x = date , y = hs ))   + geom_line(color = 'blue') 
housing_starts_plot

case_shiller_price_index_plot              <- ggplot(case_shiller_price_index ,        aes(x = date , y = cpi ))  + geom_line(color = 'blue') 
case_shiller_price_index_plot

# Visuiizing the distribution of data

ggplot(monthly_supply_homes,              aes(msh) )    +   geom_density(kernel = 'gaussian' , color = 'red')
ggplot(unemployment_rate,                 aes(uer) )    +   geom_density(kernel = 'gaussian' , color = 'blue')
ggplot(federal_funds_interest_rate,       aes(ffir) )   +   geom_density(kernel = 'gaussian' , color = 'red')
ggplot(recession,                         aes(rec) )    +   geom_density(kernel = 'gaussian' , color = 'red')
ggplot(housing_starts,                    aes(hs) )     +   geom_density(kernel = 'gaussian' , color = 'blue')
ggplot(case_shiller_price_index,          aes(cpi) )    +   geom_density(kernel = 'gaussian' , color = 'blue')

# mergeing into dataframe

df4        <- merge(monthly_supply_homes , unemployment_rate , by = 'date')
df3        <- merge(df4 , federal_funds_interest_rate ,        by = 'date')
df2        <- merge(df3 , recession ,                          by = 'date')
df1        <- merge(df2 , housing_starts ,                     by = 'date')
df         <- merge(df1 , case_shiller_price_index ,           by = 'date')

# converting into time-series

monthly_supply_homes                       <- ts(monthly_supply_homes$msh ,          start = c(2000 , 1) , frequency = 12)
unemployment_rate                          <- ts(unemployment_rate$uer ,             start = c(2000 , 1) , frequency = 12)
federal_funds_interest_rate                <- ts(federal_funds_interest_rate$ffir,   start = c(2000 , 1) , frequency = 12)
recession                                  <- ts(recession$rec ,                     start = c(2000 , 1) , frequency = 12)
housing_starts                             <- ts(housing_starts$hs ,                 start = c(2000 , 1) , frequency = 12)
case_shiller_price_index                   <- ts(case_shiller_price_index$cpi ,      start = c(2000 , 1) , frequency = 12)

# series vs Time Plot

ts.plot(monthly_supply_homes)
ts.plot(unemployment_rate)
ts.plot(federal_funds_interest_rate)
ts.plot(recession)
ts.plot(housing_starts)
ts.plot(case_shiller_price_index)


# creating a new timeseries data frame

tf  = cbind(monthly_supply_homes , unemployment_rate , federal_funds_interest_rate , recession , housing_starts  ,case_shiller_price_index )
tf


# Auto correlation Function

acf(tf , main  = 'ACF plot ')
acf(monthly_supply_homes )
acf(unemployment_rate )
acf(federal_funds_interest_rate )
acf(recession )
acf(housing_starts )
acf(case_shiller_price_index )

# Partial Auto correlation funcion

pacf(tf , main = 'PACF plot')
pacf(monthly_supply_homes )
pacf(unemployment_rate )
pacf(federal_funds_interest_rate )
pacf(recession )
pacf(housing_starts )
pacf(case_shiller_price_index )



# Augmented Dickey–Fuller Test

adf_test1 <- adf.test(monthly_supply_homes)
adf_test1

adf_test2 <- adf.test(unemployment_rate)
adf_test2

adf_test3 <- adf.test(federal_funds_interest_rate)
adf_test3


adf_test4 <- adf.test(recession)
adf_test4

adf_test5 <- adf.test(housing_starts)
adf_test5

adf_test6 <- adf.test(case_shiller_price_index)
adf_test6



# finding the optimal lags to use for this model

lagselect                  <- VARselect(tf , lag.max = 10 ,  type = 'const')
lagselect$selection

# Building VAR model 

model                      <- VAR(tf , p = 3 , type = 'trend' , season = NULL , exogen = NULL)
summary(model)

# Daignosing VAR model
# serial Correlation

serial_tet                  <- serial.test(model , lags.pt = 12 , type = 'PT.asymptotic')
serial_tet

# Hetroscedasticity

arch_test                   <- arch.test(model , lags.multi = 12 , multivariate.only = TRUE)
arch_test

# Normal Distribution test

normality_test              <- normality.test(model , multivariate.only =  TRUE)
normality_test

# structural Breaks

stability_test              <- stability(model , type = 'OLS-CUSUM')
stability_test
plot(stability_test)


# Granger causality test

msh_granger                 <- causality(model , cause = 'monthly_supply_homes')
msh_granger

uem_granger                 <- causality(model , cause = 'unemployment_rate')
uem_granger

ffir_granger                <- causality(model , cause = 'federal_funds_interest_rate')
ffir_granger

rec_granger                 <- causality(model , cause = 'recession')
rec_granger

hs_granger                  <- causality(model , cause = 'housing_starts')
hs_granger

cpi_granger                 <- causality(model , cause = 'case_shiller_price_index')
cpi_granger


# Impulse response functions

msh_irf     <- irf(model , impulse= 'monthly_supply_homes' , 
                           response = 'case_shiller_price_index' , 
                           n.ahead = 20 , 
                           boot = TRUE)
plot(msh_irf , ylab = "case shiier price index" , main = 'shock from HPI')


uer_irf <- irf(model , impulse= 'unemployment_rate' , 
                       response = 'case_shiller_price_index' , 
                       n.ahead = 20 , 
                       boot = TRUE)
plot(uer_irf , ylab = "case shiier price index" , main = 'shock from unemployment rate')


ffir_irf <- irf(model , impulse= 'federal_funds_interest_rate' , 
                        response = 'case_shiller_price_index' , 
                        n.ahead = 20 , 
                        boot = TRUE)
plot(ffir_irf , ylab = "case shiier price index" , main = 'shock from federal funds interest rate ')


rec_irf <- irf(model , impulse= 'recession' , 
                       response = 'case_shiller_price_index' , 
                       n.ahead = 20 , 
                       boot = TRUE)
plot(rec_irf , ylab = "case shiier price index" , main = 'shock from recession ')


hs_irf <- irf(model , impulse= 'housing_starts' , 
                      response = 'case_shiller_price_index' , 
                      n.ahead = 20 , 
                      boot = TRUE)
plot(hs_irf , ylab = "case shiier price index" , main = 'shock from housing_starts ')


cpi_irf <- irf(model , impulse= 'case_shiller_price_index' , 
                       response = 'case_shiller_price_index' ,
                       n.ahead = 20 , 
                       boot = TRUE)
plot(cpi_irf , ylab = "case shiier price index" , main = 'shock from case_shiller_price_index ')


# Forecast Error variance Decomposition

var_decomp <- fevd(model , n.ahead = 10)
plot(var_decomp , main = 'Forecast Error Variance Decomposition')
var_decomp


# VAR forecarsting for next 10 periods

var_forecast <- predict(model , n.ahead =  10 , ci = 0.95)
var_forecast


# ploting the fancharts for next ten periods

fanchart(var_forecast , names = 'monthly_supply_homes',             main = 'forecast monthly_supply_homes to 10 periods',          ylab = 'monthly supply homes'  , xlab = 'Time period ')
fanchart(var_forecast , names = 'unemployment_rate',                main = 'forecast unemployment_rate to 10 periods' ,            ylab = 'Unemployment Rate'     , xlab = 'Time period ')
fanchart(var_forecast , names = 'federal_funds_interest_rate',      main = 'forecast federal_funds_interest_rate to 10 periods',   ylab = 'Federa funds interest rate'  , xlab = 'Time period ')
fanchart(var_forecast , names = 'recession',                        main = 'forecast recession to 10 periods' ,                    ylab = 'Recession'             , xlab = 'Time period ')
fanchart(var_forecast , names = 'housing_starts',                   main = 'forecast housing_starts to 10 periods' ,               ylab = 'Housing starts'  , xlab = 'Time period ')
fanchart(var_forecast , names = 'case_shiller_price_index',         main = 'forecast case_shiller_price_index to 10 periods' ,     ylab = 'case shiller house price index'  , xlab = 'Time period ')



##------------------------------------------------The End--------------------------------------------------------------------------------------##









