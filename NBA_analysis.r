#Load and attach data (“Large Team Data Trimmed.csv”)
data = read.csv(file.choose(), header = TRUE)
attach(data)

#Model selection
#Fit model with all explanatory variables included
lm.full = lm(Win.Pct.~MP+FG+FGA+FG.+X3P+X3PA+X3P.+X2P+X2PA+X2P.+FT+FTA+FT.+ORB+DRB+TRB+AST+STL+BLK+TOV+PF+PTS+PTS.G+Opp.MP+Opp.FG+Opp.FGA+Opp.FG.+Opp.3P+Opp.3PA+Opp.3P.+Opp.2P+Opp.2P.+Opp.FT+Opp.FTA+Opp.FT.+Opp.ORB+Opp.DRB+Opp.TRB+Opp.AST+Opp.STL+Opp.BLK+Opp.TOV+Opp.PF+Opp.PTS+Opp.PTS.G+Age+MOV+SOS+ORtg+DRtg+Pace+FTr+X3PAr+TS.+eFG.+TOV.+ORB.+FT.FGA+Opp.eFG.+Opp.TOV.+Opp.DRB.+Opp.FT.FGA)

#All subset selection using stepwise procedure and AIC value
step(lm.full, direction="both", k=2)
…
lm.best.aic=lm(formula=Win.Pct.~FG+FG.+X3P.+X2P.+FT+FTA+ORB+DRB+STL+TOV+PTS.G+Opp.FG+Opp.FGA+Opp.FG.+Opp.3P+Opp.3PA+Opp.2P.+Opp.FTA+Opp.DRB+Opp.TOV+Opp.PF+MOV+FTr+TS.+TOV.+ORB.+FT.FGA+Opp.eFG.+Opp.TOV.+Opp.DRB.+Opp.FT.FGA)

summary(lm.best.aic)

#All subset selection using stepwise procedure and BIC value
step(lm.full, direction="both", k=log(109))
lm.best.bic=lm(formula=Win.Pct.~FG+FG.+FT+FTA+ORB+DRB+STL+TOV+PTS.G+Opp.FG.+Opp.3PA+Opp.2P.+Opp.FTA+Opp.TOV+Opp.PF+MOV+FTr+TS.+TOV.+ORB.+FT.FGA+Opp.eFG.+Opp.TOV.+Opp.DRB.+Opp.FT.FGA)
summary(lm.best.bic)

 
#Check correlation between variables in each of the models and display using heat map
library(reshape2)
library(ggplot2)

variables=data.frame(MP,FG,FGA,FG.,X3P,X3PA,X3P.,X2P,X2PA,X2P.,FT,FTA,FT.,ORB,DRB,TRB,AST,STL,BLK,TOV,PF,PTS,PTS.G,Opp.MP,Opp.FG,Opp.FGA,Opp.FG.,Opp.3P,Opp.3PA,Opp.3P.,Opp.2P,Opp.2P.,Opp.FT,Opp.FTA,Opp.FT.,Opp.ORB,Opp.DRB,Opp.TRB,Opp.AST,Opp.STL,Opp.BLK,Opp.TOV,Opp.PF,Opp.PTS,Opp.PTS.G,Age,MOV,SOS,ORtg,DRtg,Pace,FTr,X3PAr,TS.,eFG.,TOV.,ORB.,FT.FGA,Opp.eFG.,Opp.TOV.,Opp.DRB.,Opp.FT.FGA)
varcor = cor(variables)    

                    
Finalvars=data.frame(FG,FG.,FT,FTA,ORB,DRB,STL,TOV,PTS.G,Opp.FG.,Opp.3PA,Opp.2P.,Opp.FTA,Opp.TOV,Opp.PF,MOV,FTr,TS.,TOV.,ORB.,FT.FGA,Opp.eFG.,Opp.TOV.,Opp.DRB.,Opp.FT.FGA)
finalvarcor =cor(Finalvars)


aic_variables=data.frame(FG,FG.,X3P.,X2P.,FT,FTA,ORB,DRB,STL,TOV,PTS.G,Opp.FG,Opp.FGA,Opp.FG.,Opp.3P,Opp.3PA,Opp.2P.,Opp.FTA,Opp.DRB,Opp.TOV,Opp.PF,MOV,FTr,TS.,TOV.,ORB.,FT.FGA,Opp.eFG.,Opp.TOV.,Opp.DRB.,Opp.FT.FGA)

aicvarcor = cor(aic_variables)


#correlation matrix of full model
regular_cor_melt = melt(varcor)
qplot(x=Var1, y=Var2, data=regular_cor_melt, fill=value,geom="tile")+ scale_fill_gradient2(limits=c(-1, 1))


#correlation matrix of best aic model
aic_cor_melt=melt(aicvarcor) 
qplot(x=Var1, y=Var2, data=aic_cor_melt, fill=value,geom="tile")+ scale_fill_gradient2(limits=c(-1, 1))


#correlation matrix of best bic model
cor_melt = melt(finalvarcor)
qplot(x=Var1, y=Var2, data=cor_melt, fill=value,geom="tile")+ scale_fill_gradient2(limits=c(-1, 1))





#We decided to go proceed with the bic model (discussion in paper)



#Multicollinearity
#Eliminating correlated variables from the best BIC model 1 at a time untill all Variance Inflation factors are below 10
Finalvarmatrix = cbind(FG,FG.,FT,FTA,ORB,DRB,STL,TOV,PTS.G,Opp.FG.,Opp.3PA,Opp.2P.,Opp.FTA,Opp.TOV,Opp.PF,MOV,FTr,TS.,TOV.,ORB.,FT.FGA,Opp.eFG.,Opp.TOV.,Opp.DRB.,Opp.FT.FGA)

r.squared = rep(NA, 25)
for (i in 1:25){
	dat =cbind(data.frame(Y=Finalvarmatrix[,i]), as.data.frame(Finalvarmatrix[,-i]))
	lm.fit = lm(Y ~ ., data=dat)
	r.squared[i] = summary(lm.fit)$r.squared
}
#Check previous Variance Inflation Ratior and reduce until all are below 10
1 / (1-r.squared)

# remove FG
Finalvarmatrix = Finalvarmatrix[,-1]
r.squared = rep(NA, 24)
for (i in 1:24){
	dat =cbind(data.frame(Y=Finalvarmatrix[,i]), as.data.frame(Finalvarmatrix[,-i]))
	lm.fit = lm(Y ~ ., data=dat)
	r.squared[i] = summary(lm.fit)$r.squared
}
# remove MOV
Finalvarmatrix = Finalvarmatrix[,-15]
r.squared = rep(NA, 23)
for (i in 1:23){
	dat =cbind(data.frame(Y=Finalvarmatrix[,i]), as.data.frame(Finalvarmatrix[,-i]))
	lm.fit = lm(Y ~ ., data=dat)
	r.squared[i] = summary(lm.fit)$r.squared
}
# remove FT
Finalvarmatrix = Finalvarmatrix[,-2]
r.squared = rep(NA, 22)
for (i in 1:22){
	dat =cbind(data.frame(Y=Finalvarmatrix[,i]), as.data.frame(Finalvarmatrix[,-i]))
	lm.fit = lm(Y ~ ., data=dat)
	r.squared[i] = summary(lm.fit)$r.squared
}
# remove Opp.FG.
Finalvarmatrix = Finalvarmatrix[,-8]
r.squared = rep(NA, 21)
for (i in 1:21){
	dat =cbind(data.frame(Y=Finalvarmatrix[,i]), as.data.frame(Finalvarmatrix[,-i]))
	lm.fit = lm(Y ~ ., data=dat)
	r.squared[i] = summary(lm.fit)$r.squared
}
# remove PTS.G
Finalvarmatrix = Finalvarmatrix[,-7]
r.squared = rep(NA, 20)
for (i in 1:20){
	dat =cbind(data.frame(Y=Finalvarmatrix[,i]), as.data.frame(Finalvarmatrix[,-i]))
	lm.fit = lm(Y ~ ., data=dat)
	r.squared[i] = summary(lm.fit)$r.squared
}
#remove FTr
Finalvarmatrix = Finalvarmatrix[,-12]
r.squared = rep(NA, 19)
for (i in 1:19){
	dat =cbind(data.frame(Y=Finalvarmatrix[,i]), as.data.frame(Finalvarmatrix[,-i]))
	lm.fit = lm(Y ~ ., data=dat)
	r.squared[i] = summary(lm.fit)$r.squared
}
# remove Opp.TOV
Finalvarmatrix = Finalvarmatrix[,-10]
r.squared = rep(NA, 18)
for (i in 1:18){
	dat =cbind(data.frame(Y=Finalvarmatrix[,i]), as.data.frame(Finalvarmatrix[,-i]))
	lm.fit = lm(Y ~ ., data=dat)
	r.squared[i] = summary(lm.fit)$r.squared
}
# get the vif to verify that all are below 10
1/(1-r.squared)


# redo our model using these uncorrelated predictors
new.best.bic.model=lm(formula=Win.Pct.~FG.+FTA+ORB+DRB+STL+TOV+Opp.3PA+Opp.2P.+Opp.FTA+Opp.PF+TS.+TOV.+ORB.+FT.FGA+Opp.eFG.+Opp.TOV.+Opp.DRB.+Opp.FT.FGA)

new.vars = cbind(FG.,FTA,ORB,DRB, STL,TOV,Opp.3PA,Opp.2P,Opp.FTA,Opp.PF,TS.,TOV.,ORB.,FT.FGA,Opp.eFG.,Opp.TOV.,Opp.DRB., Opp.FT.FGA)

#Display correlation matrix of newest model using a heat map
qplot(x=Var1, y=Var2, main='Model Correlation Matrix Heatmap', data=melt(cor(new.vars)), fill=value,geom="tile")+ scale_fill_gradient2(limits=c(-1, 1))


#Transformations
#Determine if response variable can be predicted better after transformation
library(MASS)
boxcox(new.best.bic.model)


#Outliers
# initial diagnostic plots indicate that we have some outliers
qqnorm(resid(new.best.bic.model))
qqline(resid(new.best.bic.model))

plot(rstandard(new.best.bic.model)~Win.Pct.,ylab="Studentized Residuals", xlab="Win Percent", main="Model Residual Plot")


# check for outliers using DFFITS. Our data is medium/small so a critical value of 1 was used
dfit = abs(dffits(new.best.bic.model))
check <- c()
for(i in 1:109){
ifelse(dfit[i] > 1, check<-c(check, i), NA)
}
check

#remove these outliers

to.remove = c("3", "6", "11", "13", "14","15","21","23","24", "25", "27", "30")
data.new = data[! rownames(data) %in% to.remove,]
attach(data.new)

#fit data with removed outliers
new.best.bic.model=lm(formula=Win.Pct.~FG.+FTA+ORB+DRB+STL+TOV+Opp.3PA+Opp.2P.+Opp.FTA+Opp.PF+TS.+TOV.+ORB.+FT.FGA+Opp.eFG.+Opp.TOV.+Opp.DRB.+Opp.FT.FGA, data = data.new)

#Verify that residual plot does not have any noticeable patterns/features
plot(rstandard(new.best.bic.model)~Win.Pct.,ylab="Studentized Residuals", xlab="Win Percent", main="Model Residual Plot")


#Autocorrelation
#Check for autocorrelation using acf method
library(zoo)
library(lmtest)
plot(Win.Pct., xlab = "Team", ylab="Win Percent", main="Win Percent per Team", type="b")

acf(Win.Pct.)

dwtest(new.best.bic.model)

#Global F test

anova(new.best.bic.model)

atbl = anova(new.best.bic.model)
atbl[,3]

ssr = atbl[,3]
ssr = ssr[-19]
ssr = sum(ssr)
msr = ssr/18
mse = abtl[19,3]
Fstar = msr/mse

#F-statistic: 72.79 on 18 and 78 DF,  p-value: < 2.2e-16
#Reject null hypothesis
#At least one BetaCoeff does not equal 0 


#Partial F-test for insignificant variables 
reduced.best.bic.model = lm(formula = Win.Pct. ~ FG.+ DRB  + TOV  + Opp.FTA + Opp.PF + TS. + ORB. + Opp.eFG. + Opp.TOV., data = data.new)

anova(reduced.best.bic.model, new.best.bic.model)

#Null hypothesis not rejected- drop variables from model 

#diagnosis of new model 
qqnorm(resid(reduced.best.bic.model))
qqline(resid(reduced.best.bic.model)) 
boxcox(reduced.best.bic.model)
plot(rstandard(reduced.best.bic.model)~Win.Pct.,ylab="Studentized Residuals", xlab="Win Percent", main="Model Residual Plot")

anova(new.best.bic.model)

Ftbl = anova(reduced.best.bic.model)
Ftbl[,3]

ssr = Ftbl[,3]
ssr = ssr[-10]
ssr = sum(ssr)
msr = ssr/9
mse = Ftbl[10,3]
Fstar = msr/mse

#F-statistic: 141.8 on 9 and 87 DF,  p-value: < 2.2e-16
#Reject null hypothesis
#At least one BetaCoeff does not equal 0 