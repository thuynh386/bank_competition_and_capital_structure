rm(list=ls())
library(stringr)
library(reshape2)
#install.packages("plm")
#install.packages("lfe")
#install.packages("data.table")
#install.packages("stargazer")
library(plm)
library(lfe)
library(foreign)
library(data.table)
library(asbio)
library(AER)
library(car)
library(stargazer)
library(lfe)


##################################### Banking concentration indices ##################################

## Lerner Index ##
lerner <- read.csv("Lerner index.csv", stringsAsFactors = FALSE, na.strings = "n.a.")

#Transform the lerner to long form in order to merge
lerner <- melt(lerner, id="Year")
colnames(lerner) <- c("YEAR", "country_code", "lerner.index")

# List of countries for which we have the lerner index
countries <- as.character(lerner[,2])
countries <- unique(countries)

## HHI Index - for Robustness check ##
HHI <- read.csv("HHI index.csv", stringsAsFactors = FALSE, na.strings = "n.a.")

#Transform the HHI to long form in order to merge
HHI <- melt(HHI, id="Year")
colnames(HHI) <- c("YEAR", "country_code", "HHI.index")

## CR5 Index ##
CR5 <- read.csv("CR5 index.csv", stringsAsFactors = FALSE, na.strings = "n.a.")

#Transform the HHI to long form in order to merge
CR5 <- melt(CR5, id="Year")
colnames(CR5) <- c("YEAR", "country_code", "CR5.index")



#######################Additional data########################

### Instrument 1: Banking entry requirements ############

bank.entry <- read.csv("Banking entry requirements.csv", na.strings = "n.a.")
bank.entry <- melt(bank.entry, id="Year")
colnames(bank.entry) <- c("YEAR", "country_code", "bank.entry")


### Instrument 2: Banking fraction denied domestic ########

bank.deny <- read.csv("Banking fraction denied domestic.csv", na.strings = "n.a.")
bank.deny <- melt(bank.deny, id="Year")
colnames(bank.deny) <- c("YEAR", "country_code", "bank.deny")


### GDP in millions USD

gdp <- read.csv("GDP at market prices (current mil US$).csv", na.strings = "n.a.")
gdp <- melt(gdp, id ="Year")
colnames(gdp) <- c("YEAR", "country_code", "gdp")

### Private Bond Market Size for non-financial companies as percentage of GDP

private.bond <- read.csv("Non-financial private bond (% of GDP).csv", na.strings = "n.a.")
private.bond <- melt(private.bond, id="Year")
colnames(private.bond) <- c("YEAR", "country_code", "private.bond")




###############################Firm's data#############################################
amadeus <- read.table("Amadeus_final_data.txt", header = TRUE, quote = '"', sep = ";", stringsAsFactors = FALSE, fill = TRUE, na.strings = "n.a.")


for (i in 13:dim(amadeus)[2]){
        amadeus[,i]<-as.numeric(gsub(",","",amadeus[,i]))
}


amadeus2 <- amadeus[,-c(1, 4, 6, 8:23)]


##Rename variables before reshape
names(amadeus2)[25:34] <- paste(rep("tangible.fixed.assets.th", 10), c(2014:2005), sep = "_")
names(amadeus2)[95:104] <- paste(rep("total.assets.th", 10), c(2014:2005), sep = "_")
names(amadeus2)[245:254] <- paste(rep("sales.th", 10), c(2014:2005), sep = "_")
names(amadeus2)[295:304] <- paste(rep("gearing.pct", 10), c(2014:2005), sep = "_")
names(amadeus2)[305:314] <- paste(rep("profit.margin.pct", 10), c(2014:2005), sep = "_")


#names(amadeus2)[375:384] <- paste(rep("market.cap.mil", 10), c(2014:2005), sep = "_")
#names(amadeus2)[145:154] <- paste(rep("long.term.debt.th", 10), c(2014:2005), sep = "_")
#names(amadeus2)[185:194] <- paste(rep("loans.th", 10), c(2014:2005), sep = "_")
#names(amadeus2)[235:244] <- paste(rep("working.capital.th", 10), c(2014:2005), sep = "_")
#names(amadeus2)[265:274] <- paste(rep("taxation.th", 10), c(2014:2005), sep = "_")

## Keep only useful variables
amadeus2 <- amadeus2[,c(1:4, 25:34, 95:104, 245:254, 295:314)]


## Growth rate of total assets
for (i in 0:9){
        year <- 2014 - i
        var_name <- paste("sales.growth", as.character(year), sep = "_")
        if (i == 9) {amadeus2$x <- rep(NA, dim(amadeus2)[1])}
        else { amadeus2$x <- log(amadeus2[,25+i] / amadeus2[,26+i])}
        names(amadeus2)[dim(amadeus2)[2]] <- var_name
}


# Transform to long form

amadeus2 <- reshape(amadeus2, direction = "long", varying = c(5:dim(amadeus2)[2]), sep = "_")
names(amadeus2)[5] <- "YEAR"


###############################Merge data###################################################

## New variable country_code to use for merging data in the next step
# Country_code from BvD_ID_number for merging data
amadeus2$country_code <- substr(amadeus2$BvD_ID_number, 1, 2)


final_data <- merge(amadeus2, lerner, by = c("country_code", "YEAR"))
final_data <- merge(final_data, HHI, by = c("country_code", "YEAR"))
final_data <- merge(final_data, CR5, by = c("country_code", "YEAR"))
final_data <- merge(final_data, bank.entry, by = c("country_code", "YEAR"))
final_data <- merge(final_data, gdp, by = c("country_code", "YEAR"))
final_data <- merge(final_data, bank.deny, by = c("country_code", "YEAR"))
final_data <- merge(final_data, private.bond, by = c("country_code", "YEAR"))


##### Filter the data ########

## Keep only companies in countries for which we have lerner index
amadeus2 <- amadeus2[amadeus2$country_code %in% countries,]

## Remove the data of 2005 as the growth rates are not available for this year
final_data <- final_data[final_data$YEAR != 2005,]

## Sales growth
final_data <- final_data[final_data$sales.growth != -Inf & final_data$sales.growth != Inf, ]

# Keep only the compete rows, without any NAs
final_data <- final_data[complete.cases(final_data),]

## Delete companies with total.assets = 0 or tangible.fixed.assets = 0, likely be error in recording data.

final_data <- final_data[final_data$tangible.fixed.assets.th > 0 & final_data$total.assets.th > 0, ]


######## Create more variables for regression

#final_data$ln.tangible.assets <- log(final_data$tangible.fixed.assets.th)

final_data$tangibility.pct <- (final_data$tangible.fixed.assets.th * 100.0) / final_data$total.assets.th

final_data$ln.total.assets <- log(final_data$total.assets.th)

final_data$ln.gdp <- log(final_data$gdp)


# Compute the leverage (debt / total of debt and equity) from gearing (debt / equity)
final_data$leverage <- final_data$gearing.pct / (1 + final_data$gearing.pct)

# Keep only the compete rows, without any NAs
final_data <- final_data[complete.cases(final_data),]

# Remove the duplicates. The duplicates are present as we draw 2 random samples from AMADEUS database.
final_data <- final_data[!duplicated(final_data[c(2,4)]) | duplicated(final_data[c(2,4)], fromLast=TRUE),]


######################## Descriptive Statistics ###########################

# Correlation matrix for explanatory variables
cor(final_data[c("lerner.index", "ln.total.assets", "ln.tangible.assets", "profit.margin.pct", "private.bond", "ln.gdp")])



################# Fixed Effect Models ###################

final_data$YEAR <- as.factor(final_data$YEAR)

#firm.time.fixed.effect <- felm(leverage ~ lerner.index + ln.total.assets + ln.tangible.assets + profit.margin.pct + private.bond + gdp.mil | BvD_ID_number + YEAR | 0 | 0, data = final_data)
#summary(firm.time.fixed.effect, robust = TRUE)

#firm.time.fixed.effect.IV <- felm(leverage ~ lerner.index + ln.total.assets + ln.tangible.assets + profit.margin.pct + private.bond + gdp.mil | BvD_ID_number + YEAR| (lerner.index | 0 ~ bank.entry + bank.deny) | 0, data = final_data)
#summary(firm.time.fixed.effect.IV, robust = TRUE)


pooled.OLS <- lm(leverage ~ lerner.index + ln.total.assets + tangibility.pct + sales.growth + profit.margin.pct + private.bond + ln.gdp, data = final_data) 
summary(pooled.OLS)


# Firm and time fixed effects model, without the IVs 
fe.plm <- plm(leverage ~ lerner.index + ln.total.assets + tangibility.pct + sales.growth + profit.margin.pct + private.bond + ln.gdp, data = final_data, model = "within", index = c("BvD_ID_number", "YEAR"))
summary(fe.plm)

# Firm and time fixed effects model, with the IVs: bank.deny and bank.entry
fe.IV.plm <- plm(leverage ~ lerner.index + ln.total.assets + tangibility.pct + sales.growth + profit.margin.pct + private.bond + ln.gdp | . - lerner.index + bank.deny + bank.entry, data = final_data, model = "within", index = c("BvD_ID_number", "YEAR"))
summary(fe.IV.plm)

stargazer(pooled.OLS, fe.plm, fe.IV.plm)


############################################# Robustness Check ############################

### HHI index ###
fe.plm.HHI <- plm(leverage ~ HHI.index + ln.total.assets + tangibility.pct + sales.growth + profit.margin.pct + private.bond + ln.gdp, data = final_data, model = "within", index = c("BvD_ID_number", "YEAR"))
summary(fe.plm.HHI)

fe.IV.plm.HHI <- plm(leverage ~ HHI.index + ln.total.assets + tangibility.pct + sales.growth + profit.margin.pct + private.bond + ln.gdp | . - HHI.index + bank.deny + bank.entry, data = final_data, model = "within", index = c("BvD_ID_number", "YEAR"))
summary(fe.IV.plm.HHI)



### CR5 index ###
fe.plm.CR5 <- plm(leverage ~ CR5.index + ln.total.assets + tangibility.pct + sales.growth + profit.margin.pct + private.bond + ln.gdp, data = final_data, model = "within", index = c("BvD_ID_number", "YEAR"))
summary(fe.plm.CR5)

fe.IV.plm.CR5 <- plm(leverage ~ CR5.index + ln.total.assets + tangibility.pct + sales.growth + profit.margin.pct + private.bond + ln.gdp | . - CR5.index + bank.deny + bank.entry, data = final_data, model = "within", index = c("BvD_ID_number", "YEAR"))
summary(fe.IV.plm.CR5)




#### Hausman test: Should we use the IVs? ########
# H_0: lerner is exogenous vs. H_1: lerner is endogenous

pooled.OLS.IV <- ivreg(leverage ~ lerner.index + ln.total.assets + tangibility.pct + sales.growth + profit.margin.pct + private.bond + ln.gdp | . - lerner.index + bank.entry + bank.deny, data = final_data)


beta.lerner.ols <- coefficients(pooled.OLS)[2]
var.ols <- vcov(pooled.OLS)[2,2]
beta.lerner.IV <- coefficients(pooled.OLS.IV)[2]
var.IV <- vcov(pooled.OLS.IV)[2,2]


hausman <- (beta.lerner.ols - beta.lerner.IV) / sqrt(var.IV - var.ols)
pvalue.hausman <- 2*(1 - pnorm(abs(hausman)))

pvalue.hausman # 0.0001016339 --> We reject H_0, meaning that model with IVs is preferable as the coefficient is unbiased.



################ Sargan Test ################
## H_0: instruments are valid   vs   H_1: instruments are NOT valid

resIV2<-residuals(pooled.OLS.IV)

sarganReg <- lm( resIV2 ~ final_data$ln.total.assets + final_data$ln.tangible.assets + final_data$profit.margin.pct + final_data$private.bond + final_data$ln.gdp + final_data$bank.entry + final_data$bank.deny )
summary(sarganReg)

############## 3 possibilities to compute the test statistics (asymptotically equivalent)
#### n*Multiple R-squared
n <-  dim(final_data)[1]
J3 <- n*summary(sarganReg)$r.squared
J3

##### k*F when testing the joint nullity of the coefficients of the instrumental variables
linearHypothesis(sarganReg, c("final_data$bank.entry=0", "final_data$bank.deny=0"))
J1 <-  2*0.3553
J1

#### k*F when testing the joint nullity of the coefficients of all the explanatory variables
linearHypothesis(sarganReg, c("final_data$ln.total.assets=0", "final_data$ln.tangible.assets=0", "final_data$profit.margin.pct=0", "final_data$private.bond=0", "final_data$gdp.mil=0", "final_data$bank.entry=0", "final_data$bank.deny=0"))
J2 <- 7*0.1015
J2

############## Under H0 (valid instruments),
# the test statistics J1, J2 J3 follow a chi square distribution 
# with as degree of freedom the nb of instruments - the nb of endogenous var : here 2-1=1

############## Conclusion
# compute the p-value

pvalue.sargan  <-  1-pchisq(J3,1)
pvalue.sargan

#Reject H0, i.e. the instruments are NOT exogenous







######################################################################


######## OLD OLS MODELS ############


first_stage <- lm(lerner.index ~ ln.total.assets + ln.tangible.assets + profit.margin.pct + private.bond + gdp.mil + bank.deny + bank.entry, data = final_data)
summary(first_stage)

modelIV1 <- ivreg(leverage ~ lerner.index + ln.total.assets + ln.tangible.assets + profit.margin.pct + private.bond + gdp.mil | . - lerner.index + bank.entry, data = final_data)
summary(modelIV1)

modelIV <- ivreg(leverage ~ lerner.index + ln.total.assets + ln.tangible.assets + profit.margin.pct + private.bond + gdp.mil | . - lerner.index + bank.entry + bank.deny, data = final_data)
summary(modelIV)


#### CORRELATION MATRIX

cor_table <- matrix(data = NA, nrow = 2, ncol = 2)
colnames(cor_table) <- c("bank.entry", "bank.deny")
rownames(cor_table) <- c("lerner.index", "gearing")

cor_table[1,1] <- cor(final_data$lerner.index, final_data$bank.entry) #0.633
cor_table[1,2] <- cor(final_data$lerner.index, final_data$bank.deny) #0.261
cor_table[2,1] <- cor(final_data$gearing, final_data$bank.entry) #-0.032
cor_table[2,2] <- cor(final_data$gearing, final_data$bank.deny) #-0.129
