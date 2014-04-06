#### Purchases of Ben and Jerry's Ice Cream
benjer = read.csv("BenAndJerry.csv")

## explore a bit
names(benjer)

## create a new variable for price per unit
priceper1 = (benjer$price_paid_deal + benjer$price_paid_non_deal)/benjer$quantity
y <- log(1+priceper1)

## grab some covariates of interest
## we'll create a properly formatted data.frame
x <- benjer[,c("flavor_descr","size1_descr",
               "household_income","household_size")]

## relevel 'flavor' to have baseline of vanilla
x$flavor_descr <- relevel(x$flavor_descr,"VAN")
x$formula <- factor(benjer$formula_desc, labels=c("LIGHT","HALF THE FAT","REGULAR"))
x$formula[1:10]
benjer$formula_desc[1:10]

## coupon usage
x$usecoup = factor(benjer$coupon_value>0)
x$couponper1 <- benjer$coupon_value/benjer$quantity
## organize some demographics
x$region <- factor(benjer$region, 
                   levels=1:4, labels=c("East","Central","South","West"))
x$region[1:10]
benjer$region[1:10]
x$married <- factor(benjer$marital_status==1)
x$race <- factor(benjer$race,
                 levels=1:4,labels=c("white","black","asian","other"))
x$hispanic_origin <- benjer$hispanic_origin==1
x$microwave <- benjer$kitchen_appliances %in% c(1,4,5,7)
x$dishwasher <- benjer$kitchen_appliances %in% c(2,4,6,7)
x$sfh <- benjer$type_of_residence==1
x$internet <- benjer$household_internet_connection==1
x$tvcable <- benjer$tv_items>1
x$formula

## fit the regression
fit <- glm(y~., data=x) 

## grab the non-intercept p-values from a glm
## -1 to drop the intercept, 4 is 4th column
pvals <- summary(fit)$coef[-1,4] 

## source the fdr_cut function
source("fdr.R")

#household income vs type of icream (light or regular)
plot(benjer$household_income ~ benjer$formula_desc) #people with more income try to get lighter flavors--"healthier"

#comparison of flavor and region
falvor_and_region <- table( benjer$flavor, x$region) #aggregates non-numeric data into a table with count
addmargins(prop.table(falvor_and_region))
margin_table <- addmargins(falvor_and_region)
a <- prop.table(margin_table)
add_sum_col <- a[order(a[,"Sum"]),]
top_10_icecream_by_region <- add_sum_col[1:10,1:4]
top_10_icecream_by_region
boxplot(top_10_icecream_by_region) #ylab=row.names(top_10_icecream_by_region))

#things I learned
top_10_icecream_by_region(nrow)
l <- row.names(top_10_icecream_by_region)#get row names
l
a <- a[order(a[,"Sum"]),]
a[1]
a[40:50,]
library(plyr)
arrange(a,desc(Sum))
b <- a[order("Sum")]
b
a
c[1[1]]
plot(c)
c[1]
flavors_sorted <- sort(c, decreasing= TRUE) #sort the table in descending order
top_10_flavors <- flavors_sorted[1:10] #top 10 flavors
d<-summary(benjer$flavor)
d[1[0]]

plot(top_10_flavors)
hist(top_10_flavors)
top_10_flavors[1[1]]
boxplot(top_10_flavors~y)


length(unique(benjer$flavor))
ucounts <- benjer$flavor.apply(function(x): length(x.unique()))


aggregate(benjer$flavor,1,function(x) length(unique(x)))
aggregate(data = benjer$flavor, sum)
aggregate(benjer$flavor, nfrequency = 1, FUN = sum)



d
