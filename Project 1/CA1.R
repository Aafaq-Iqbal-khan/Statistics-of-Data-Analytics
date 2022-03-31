#install.packages("lme4", repos="http://cran.rstudio.com/",type = "binary", dependencies=TRUE)
#install.packages("nlme", repos="http://cran.rstudio.com/",type = "binary", dependencies=TRUE)
#packageurl <- "https://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.4-4.tar.gz" 
#install.packages(packageurl, repos=NULL, type="source")

library(ggplot2)
library(car)
library(ggcorrplot)
library(haven)
library(moments)

install.packages("moments")
install.packages(c('ggcorrplot','car'))
install.packages('gridExtra')
library(gridExtra)
setwd("D:/NCI/SDA/CA 1")
df = read.csv("HouseDetails.txt", sep = "\t")

write.csv(df,"Data.csv")

skewness(df$lotSize)
str(df)
names(df)
head(df)

#df$fireplaces <- as.factor(df$fireplaces)
#df$bedrooms <- as.factor(df$bedrooms)
#df$bathrooms <- as.factor(df$bathrooms)
#df$rooms <- as.factor(df$rooms)
#str(df[c("bedrooms", "fireplaces", "bathrooms", "rooms")])

#df$fuel <- relevel(df$fuel,"oil")
#str(df["fuel"])

sum(is.na(df))

summary(df)
str(df)
summary(df[c("price","lotSize", "age","landValue", "livingArea","pctCollege")])

############################

df= df[which(df$sewer!="none"),]  #12 observations
df= df[which(df$bathrooms>=1),]   # 1 observations
df= df[which(df$lotSize!=0),]     # 2 observations

############################
#par(mfrow = c(2, 2))
plot1= ggplot(df, aes(x = price)) + geom_histogram(binwidth = 20000, color="darkblue", fill="lightblue") + ggtitle("Histogram of House Price") + ylab("Frequency") + xlab("Price") + theme(plot.title = element_text(hjust = 0.5))
plot2= ggplot(df, aes(x = lotSize)) + geom_histogram(binwidth = 0.5,color="darkblue", fill="lightblue") + ggtitle("Histogram of Lot Size") + ylab("Frequency") + xlab("Lot Size") + theme(plot.title = element_text(hjust = 0.5))
plot3= ggplot(df, aes(x = age)) + geom_histogram(binwidth = 10,color="darkblue", fill="lightblue") + ggtitle("Histogram of Age") + ylab("Frequency") + xlab("Age") + theme(plot.title = element_text(hjust = 0.5))
plot4= ggplot(df, aes(x = landValue)) + geom_histogram(binwidth = 20000,color="darkblue", fill="lightblue") + ggtitle("Histogram of Land Value") + ylab("Frequency") + xlab("Land Value") + theme(plot.title = element_text(hjust = 0.5))
plot5= ggplot(df, aes(x = livingArea)) + geom_histogram(binwidth = 200, color="darkblue", fill="lightblue") + ggtitle("Histogram of Living Area") + ylab("Frequency") + xlab("Living Area") + theme(plot.title = element_text(hjust = 0.5))
plot6= ggplot(df, aes(x = pctCollege)) + geom_histogram(binwidth = 5, color="darkblue", fill="lightblue") + ggtitle("Histogram of percent of neighborhood that graduated college") + ylab("Frequency") + xlab("Percent of neighborhood that graduated college") + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(plot1, plot2,plot3, plot4,plot5,plot6, ncol=2)

###

#plot7= ggplot(df, aes(x = bedrooms, fill=..count..)) + geom_histogram(binwidth = 5) + ggtitle("Histogram of Bedrooms") + ylab("Frequency") + xlab("bedrooms") + theme(plot.title = element_text(hjust = 0.5))
#plot8= ggplot(df, aes(x = bathrooms, fill=..count..)) + geom_histogram(binwidth = 5) + ggtitle("Histogram of Bathrooms") + ylab("Frequency") + xlab("Bathrooms") + theme(plot.title = element_text(hjust = 0.5))
#plot9= ggplot(df, aes(x = rooms, fill=..count..)) + geom_histogram(binwidth = 5) + ggtitle("Histogram of Rooms") + ylab("Frequency") + xlab("Rooms") + theme(plot.title = element_text(hjust = 0.5))
#plot10= ggplot(df, aes(x = fireplaces, fill=..count..)) + geom_histogram(binwidth = 5) + ggtitle("Histogram of Fireplaces") + ylab("Frequency") + xlab("Fireplaces") + theme(plot.title = element_text(hjust = 0.5))
#grid.arrange(plot7, plot8,plot9,plot10, ncol=2)

plot7= ggplot(df, aes(x = bedrooms, fill = bedrooms )) + 
  geom_bar()+ ggtitle("Distribution of Bedrooms")+
  theme(plot.title = element_text(hjust = 0.5))+ geom_text(stat='count',aes(label=..count..),vjust=-0.25)

plot8= ggplot(df, aes(x = fireplaces, fill = fireplaces )) + 
  geom_bar()+ ggtitle("Distribution of fireplaces")+
  theme(plot.title = element_text(hjust = 0.5))+ geom_text(stat='count',aes(label=..count..),vjust=-0.25)

plot9= ggplot(df, aes(x = bathrooms, fill = bathrooms )) + 
  geom_bar()+ ggtitle("Distribution of Bathrooms")+
  theme(plot.title = element_text(hjust = 0.5))+ geom_text(stat='count',aes(label=..count..),vjust=-0.25)

plot10= ggplot(df, aes(x = rooms, fill = rooms )) + 
  geom_bar()+ ggtitle("Distribution of rooms")+
  theme(plot.title = element_text(hjust = 0.5))+ geom_text(stat='count',aes(label=..count..),vjust=-0.25)

grid.arrange(plot7, plot8,plot9, plot10, ncol=2)

############

#Factor plots
fplot1= ggplot(df, aes(x = waterfront, fill = waterfront )) + 
  geom_bar()+ ggtitle("Distribution of Waterfront")+
  theme(plot.title = element_text(hjust = 0.5))+ geom_text(stat='count',aes(label=..count..),vjust=-0.25)

fplot2=ggplot(df, aes(x = newConstruction, fill = newConstruction )) + 
  geom_bar()+ ggtitle("Distribution of New Construction")+
  theme(plot.title = element_text(hjust = 0.5))+ geom_text(stat='count',aes(label=..count..),vjust=-0.25)

fplot3= ggplot(df, aes(x = centralAir, fill = centralAir )) + 
  geom_bar()+ ggtitle("Distribution of Central Air")+
  theme(plot.title = element_text(hjust = 0.5))+ geom_text(stat='count',aes(label=..count..),vjust=-0.25)

fplot4= ggplot(df, aes(x = sewer, fill = sewer )) + 
  geom_bar()+ ggtitle("Distribution of different Sewer type")+
  theme(plot.title = element_text(hjust = 0.5))+ geom_text(stat='count',aes(label=..count..),vjust=-0.25)

grid.arrange(fplot1, fplot2,fplot3, fplot4, ncol=2)

##
fplot5= ggplot(df, aes(x = heating, fill = heating )) + 
  geom_bar()+ ggtitle("Distribution of different Heat type")+
  theme(plot.title = element_text(hjust = 0.5))+ geom_text(stat='count',aes(label=..count..),vjust=-0.25)

fplot6= ggplot(df, aes(x = fuel, fill = fuel )) + 
  geom_bar()+ ggtitle("Distribution of different Fuel type")+
  theme(plot.title = element_text(hjust = 0.5))+ geom_text(stat='count',aes(label=..count..),vjust=-0.25)

grid.arrange(fplot5, fplot6, ncol=2)

##############


Splot <- function(var,title) {
  ggplot(df, aes(x=var, y=price)) + 
    geom_point(color='blue', size = 1)+ ggtitle(title) + geom_smooth(se = FALSE)+
    theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank())
}

#Splot(df$pctCollege,"PCT College vs Price")
#, Splot(df$pctCollege,"PCT College vs Price"), Splot(df$bedrooms,"Bedrooms vs Price"), Splot(df$bathrooms,"Bathrooms vs Price"), Splot(df$rooms,"Rooms vs Price")
grid.arrange(Splot(df$lotSize,"Lot Size vs Price"),Splot(df$age,"Age vs Price"),Splot(df$landValue,"Land value vs Price"), Splot(df$livingArea,"Living vs Price"), ncol=2)

grid.arrange(Splot(df$pctCollege,"PCT College vs Price"), Splot(df$bedrooms,"Bedrooms vs Price"), Splot(df$bathrooms,"Bathrooms vs Price"), Splot(df$rooms,"Rooms vs Price"), ncol=2)

###################

#lotSize,Age, landValue, livingArea, pctCollege, bedrooms, fireplaces, bathrooms, rooms
#c("price", "lotSize,Age", "landValue", "livingArea", "pctCollege", "bedrooms", "fireplaces", "bathrooms", "rooms")
str(df)
df_Corr <- df[c("price", "lotSize", "age", "landValue","livingArea", "pctCollege","bedrooms", "fireplaces","bathrooms", "rooms")]
head(df_Corr)
corr <- cor(df_Corr)
round(corr, 2)
ggcorrplot(corr,lab=TRUE)

###########################
#raw model with all
lm1=lm(price~.,data = df)
summary(lm1)
#plot(lm1)

# before removal introduce the expected interactions 
lm2<-lm(price~.+landValue*livingArea*centralAir, data = df)
summary (lm2)

# remove the interaction with high p-value 
lm3<-lm(price~.+livingArea:centralAir, data = df)
summary (lm3)


#remove sewer becuase have very high p-value
lm4= update(lm3,~.-sewer)
summary(lm4)

#remove heating becuase have very high p-value
lm5= update(lm4,~.-heating)
summary(lm5)

#remove fireplace becuase have very high p-value
lm6= update(lm5,~.-fireplaces)
summary(lm6)

#remove pct College becuase have very high p-value
lm7= update(lm6,~.-pctCollege)
summary(lm7)

#remove fuel oil becuase have very high p-value
lm8= update(lm7,~.-fuel)
summary(lm8)
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lm8)

ncvTest(lm8)
durbinWatsonTest(lm8)
vif(lm8)


################################################
keep_2 = cooks.distance(lm9) <= 5 / length(resid(lm9))

#lotSize+age+landValue+newConstruction+centralAir+livingArea+bedrooms+bathrooms+rooms,livingArea:centralAir,waterfront
lm9<-lm(log(price)~.-sewer-fireplaces-pctCollege-fuel-heating-centralAir, data=df)
summary (lm9)
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lm9)

ncvTest(lm9)
durbinWatsonTest(lm9)
vif(lm9)

##########################################

fullModel<-lm(price~lotSize+age+landValue+livingArea+pctCollege+bedrooms+fireplaces+bathrooms+rooms+heating+fuel+sewer+waterfront+newConstruction+centralAir, data=df)
a=step(fullModel, direction="backward", k=2)

fullModel2<-lm(log(price)~lotSize+age+landValue+livingArea+pctCollege+bedrooms+fireplaces+bathrooms+rooms+heating+fuel+sewer+waterfront+newConstruction+centralAir, data=df)
b=step(fullModel2, direction="backward", k=2)

fullModel3<-lm(sqrt(price)~lotSize+age+landValue+livingArea+pctCollege+bedrooms+fireplaces+bathrooms+rooms+heating+fuel+sewer+waterfront+newConstruction+centralAir, data=df)
c=step(fullModel3, direction="backward", k=2)

summary (c)
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(c)

ncvTest(c)
durbinWatsonTest(c)
vif(c)


###########################################

set.seed(42)
housing_idx = sample(nrow(df), size = floor(0.80 * nrow(df)))
housing_trn = df[housing_idx,]
housing_tst = df[-housing_idx,]
dim(df)
dim(housing_trn)
dim(housing_tst)

#new_aic = step(new, direction = "backward", trace = 0)
#names(coef(new_aic))[-1]
#resid(model_simple)
#residuals(model_simple)
#cooks.distance(model_simple)
w=subset(housing_trn,keep)
dim(w)
dim(housing_trn)


new = lm(price ~ ., data = housing_trn)
n = length(resid(new))
new_bic = step(new, direction = "backward",k = log(n), trace = 0)
names(coef(new_bic))[-1]

#lm(price~lotSize+age+landValue+livingArea+bedrooms+bathrooms+rooms+heating+waterfront+newConstruction+centralAir, data=housing_trn)
model_simple<-lm(price~lotSize+age+landValue+livingArea+bathrooms+waterfront+newConstruction+centralAir, data=housing_trn)
#Remove influential observations
keep = cooks.distance(model_simple) <= 4 / length(resid(model_simple))
model_simple_final = lm(price~lotSize+age+landValue+livingArea+bathrooms+waterfront+newConstruction+centralAir, data=housing_trn, subset = keep)

summary (model_simple_final)
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(model_simple_final)
ncvTest(model_simple_final)
durbinWatsonTest(model_simple_final)
vif(model_simple_final)

################# log ##################

model_log<-lm(log(price)~lotSize+age+landValue+livingArea+bathrooms+waterfront+newConstruction+centralAir, data=housing_trn)
#Remove influential observations
keep2 = cooks.distance(model_log) <= 4 / length(resid(model_log))
model_log_final = lm(log(price)~lotSize+age+landValue+livingArea+bathrooms+waterfront+newConstruction+centralAir, data=housing_trn, subset = keep2)

summary (model_log_final)
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(model_log_final)
ncvTest(model_log_final)
durbinWatsonTest(model_log_final)
vif(model_log_final)

############### sqrt ###################

model_sqrt<-lm(sqrt(price)~lotSize+age+landValue+livingArea+bathrooms+waterfront+newConstruction+centralAir, data=housing_trn)
#Remove influential observations
keep3 = cooks.distance(model_sqrt) <= 4 / length(resid(model_sqrt))
model_sqrt_final<-lm(sqrt(price)~lotSize+age+landValue+livingArea+bathrooms+waterfront+newConstruction+centralAir, data=housing_trn, subset = keep3)

summary (model_sqrt_final)
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(model_sqrt_final)
ncvTest(model_sqrt_final)
durbinWatsonTest(model_sqrt_final)
vif(model_sqrt_final)

names(coef(model_sqrt_final))
############################################
###############pridict######################

a= predict(object = model_simple_final,newdata = housing_tst)
summary(a)


r_sq=(cor(a, housing_tst$price))^2
r_sq
rse= sd(a-housing_tst$price)
rse

#Checking for Influential Data Points
cooks.distance(lm8)
par(mfrow=c(1,1))
influencePlot(model = lm8, scale = 3, main = "Influence Plot")


