options(scipen = 999)
#read scraped dataset (use directory in which in you saved the dataset):
craigslist = read.csv("C:/Users/Alex/Desktop/ECON 8600/craigslist_scrapper/parsed_files/craigslist_cars.csv")

#data "cleaning":

##removing ads featuring cars with no odometer reading (either broken, rolled over, or not inserted by seller)
craigslist$ODOMETER = as.numeric(craigslist$ODOMETER)
craigslist = craigslist[complete.cases(craigslist$ODOMETER),]
##removing outliers (9999999 mileage?!)
craigslist = subset(craigslist, X != 182 & X != 4 & X != 171)
##removing out of place ads (houses, trailers...)
craigslist = subset(craigslist, X != 316 & X != 430 & X != 486 & X != 234 & X != 259)
craigslist$PRICE == 19500
##removing outliers (cars with more than 60 of life span)
craigslist = craigslist[!(craigslist$CAR.AGE > 60),]

##removing cars with absurdly low asking prices
craigslist = subset(craigslist, X != 95 & X != 247)

##removing duplicates
craigslist = subset(craigslist, X != 61 & X != 131 & X != 133 & X != 213 & X != 216 & X != 263 & X != 284 & X != 297 & X != 512 & X != 515)


#summarizing and simple plots
summary(craigslist)
craigslist$LOG.PRICE = log(craigslist$PRICE)
reg1 = lm(craigslist$LOG.PRICE ~ craigslist$CAR.AGE)
summary(reg1)
plot(craigslist$CAR.AGE, craigslist$LOG.PRICE, xlab = "Car Age (Years)", ylab = "Price Log-Scale (US$)", main = "Price and Age")
abline(reg1, col = "red")
reg2 = lm(craigslist$LOG.PRICE ~ craigslist$ODOMETER)
summary(reg2)
plot(craigslist$ODOMETER, craigslist$LOG.PRICE, xlab = "Odometer Reading (Miles)", ylab = "Price Log-Scale (US$)", main = "Price and Mileage")
abline(reg2, col = "blue")

#regressions:

model1 = lm(craigslist$LOG.PRICE ~ craigslist$ODOMETER + craigslist$CAR.AGE)
summary(model1)

model2 = lm(craigslist$LOG.PRICE ~ craigslist$ODOMETER + craigslist$CAR.AGE + craigslist$QUALITY.CAR + craigslist$ODOMETER:craigslist$QUALITY.CAR + craigslist$CAR.AGE:craigslist$QUALITY.CAR)
summary(model2)

model3 = lm(craigslist$LOG.PRICE ~ craigslist$ODOMETER + craigslist$CAR.AGE + craigslist$QUALITY.CAR + craigslist$JAPAN + craigslist$ODOMETER:craigslist$QUALITY.CAR + craigslist$CAR.AGE:craigslist$QUALITY.CAR + craigslist$ODOMETER:craigslist$JAPAN + craigslist$CAR.AGE:craigslist$JAPAN + craigslist$ODOMETER:craigslist$QUALITY.CAR:craigslist$JAPAN + craigslist$CAR.AGE:craigslist$QUALITY.CAR:craigslist$JAPAN)
summary(model3)

stargazer::stargazer(model1, model2, model3, title = "Regression Results", align = TRUE)
