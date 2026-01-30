#################
# Marketing Mix #
#################

## Install Packages (if needed)
install.packages("tseries")

## Load Packages and Set Seed
library(tseries)
set.seed(1)

## Read in the marketing mix data
mmix <- read.csv(file.choose())  ## Choose the file mmix_data.csv

## Look at means of variables
lapply(sapply(mmix,mean),round,2)

## Create natural log, lag, and weekday variables
mmix$ln_quantity <- log(mmix$quantity)
mmix$ln_price <- log(mmix$price)
mmix$ln_digital_ad <- log(mmix$digital_ad)
mmix$ln_digital_search <- log(mmix$digital_search)
mmix$ln_print <- log(mmix$print+1) ## Some time periods have 0 spend
mmix$ln_tv <- log(mmix$tv)
mmix$lln_quantity <- c(0, mmix$ln_quantity[1:length(mmix$ln_quantity)-1])
mmix$weekdays <- weekdays(as.Date(mmix$date))

## Check for unit root
adf.test(mmix$ln_quantity)

## Check for multicollinearity
cor_vars <- c ("ln_quantity", "lln_quantity", "ln_price", "ln_digital_ad", 
"ln_digital_search", "ln_print", "ln_tv")
cor_data <- mmix[cor_vars]
cor_table <- cor(cor_data)
round(cor_table,2)

## Combine ln_digital_ad and ln_digital_search
mmix$ln_digital <- log(mmix$digital_ad + mmix$digital_search)

## Run the regression
mmix_reg <- lm(ln_quantity ~ lln_quantity + ln_price + ln_digital + ln_print + 
ln_tv + factor(weekdays), data = mmix)
summary(mmix_reg)

## Created predicted values for actual quantity
mmix$pred_quantity <- exp(predict(mmix_reg))

## Export the data set
write.csv(mmix, file.choose(new=TRUE), row.names = FALSE) ## Name the file mmix_predicted.csv