# Risk-management-model
Insurance startup 



PART 1


---
title: "Applied Statistics Mini Project Q1-3"
output:
  html_document:
    df_print: paged
---

```{r}
library(tidyverse)
library(knitr)
library(actuar)
library(moments)
library(fitdistrplus)
library(plyr)
```

```{r}
loss<-read.csv("loss_data(4).csv")#loading data into R
freq<- read.csv("frequency_data.csv")#loading data into R

lossvec<-c(loss$loss)#converting loss data into a vector
freqvec<-c(freq$freq)#Converting frequency data intot vector


mean.freq<-mean(freq$freq)#mean of frequency data
var.freq<-var(freq$freq)#variance of frequency data

mean.loss<-mean(loss$loss)#mean of loss data
var.loss<-var(loss$loss)#variance of loss data
quartile1<-quantile(loss$loss,0.25)#1st quartile of loss data
quartile3<-quantile(loss$loss,0.75)#3rd quartile of loss data
median.loss<-median(loss$loss)#median of loss data
skewness.loss<-skewness(loss$loss)#skewness of loss data
sd.loss<-sd(loss$loss)#standard deviation of loss data...aka sqaure of variance

sprintf("mean of frequency:%.2f",(mean.freq))
sprintf("variance of frequency:%.2f",(var.freq))
sprintf("mean of loss severity:%.2f",(mean.loss))
sprintf("variance of loss severity:%.2f",(var.loss))
sprintf("1st quartile of loss severity:%.2f",(quartile1))
sprintf("3rd quartile of loss severity:%.2f",(quartile3))
sprintf("median of loss severity:%.2f",(median.loss))
sprintf("skewness of loss severity:%.4f",(skewness.loss))


```

Through looking at the data calculated we can observe the expected number of claims per year and the level of positive skewness of 2.8322. Also, because the mean>median, it solidifies the fact the loss severity is positively skewwed. 

```{r}
barplot(freqvec,main="Loss Frequency Data",xlab="Year",ylab="Frequency",names.arg=c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018),col="blue",)#plot of frequency

fit.pois<-fitdist(freqvec,dpois,method=c("mle"))#fitting the datas to a poisson distribution as poisson is best for claim frequency
fit.pois
```

The distribution that fits the claims frequency the best is the poisson distribution as the number of claims per year is assumed to be constant, which is the lambda value of 297.2. All claims are independent of each other and sum of two poisson variables(l1+l2) equals a new lambda where (L=l1+l2). Claims frequency follows all of these assumptions for the poisson distribution and hence it makes sense for it to be distributed in as poisson. The probability that two or more claims occur in a small space of time is negligible also. The poisson distribution is ideal for when we know how often the event we are observing occurs. In this case, we assume it to be 297.2.
```{r}

hist<-ggplot(data=loss)
hist+geom_histogram(aes(x=loss,y=..density..),binwidth=50,color="blue",fill="blue",alpha=0.5)+ggtitle("Loss Severity For Claims")+labs(y="Dessity",x="Loss Size")+theme_minimal()#plot of loss severity 


```

As can be seen from this graph for the loss severity/size of claims. There is a heavy, fat positive skew with the majority of data points located to the left of the graph. This tells us that there is a higher frequency of smaller claims made than very large claims such as "4000"", which can be seen as an outlier. This makes sense as there will intuitively not be very many huge claims made as an event that requires this magnitude is not very likely. The density (y-axis) allows me to see the probability of a claim lying within a certain interval of claim size, which is telling us more than using frequency on the y-axis. The distibutions that follow the above model are potentially:exponential,lognormal, and pareto and hence I will attempt to fit these distributions to the above dataset.

```{r}
my.box<-ggplot(data=loss,aes(y=loss,x=year))#boxplot of loss severity showing Q1/Q3/max value/min value/median
my.box+geom_boxplot()+coord_flip()#flips boxplot horizontal
```

This boxplot shows the visualisation of the summary stats calculated from above. It shows the median, IQR, max value, the min value and both quartiles for the loss data. It is easier to see the spread of the data using this graph. The data is very clumped to the left and hence we can conclude the loss severity data is postively skewed with ouliers >Â£500. 


```{r}

ex2<-var.loss+(mean.loss^2)
alpha.pareto<-((2*ex2)-(2*(mean.loss)^2))/(ex2-(2*(mean.loss)^2))
lambda.pareto<-mean.loss*(alpha.pareto-1)
sprintf("alpha pareto:%.2f",alpha.pareto)
sprintf("lambda pareto:%.2f",lambda.pareto)

#PARETO FITTING
hist<-ggplot(data=loss)
hist+geom_histogram(aes(x=loss,y=..density..),binwidth=50,color="blue",fill="blue",alpha=0.5)+ggtitle("PARETO FITTING")+labs(y="Density",x="Loss Size")+theme_minimal()+stat_function(fun=dpareto,args=list(shape=alpha.pareto,scale=lambda.pareto),color="red")#plot of fitted against emperical

#Chi-squared test

fit.pareto<-fitdist(lossvec,dpareto,method=c("mle"))
fit.pareto#fitting the data to a pareto distribution

ks.test.par<-ks.test(lossvec,ppareto,scale=1365.523,shape=4.631763)
ks.test.par#goodness of fit test using kolmogorov smirnov
#it passes the goodnessof fit test as pvalue >0.05


```
The kolmgorov smirnov test is a goodness of fit test to see whether or not the emperical data follows a chosen distribution. The kolmogorv test requires the data to be continuous and one dimensional, which our dataset follows.
 

As observed from the graph above, I have fitted the loss data to a pareto distribution to see if it will follow such a distribution. From the goodness of fit test, it appears that a pareto will be a good choice for distributing the data 
because it passes the kolmogorv smirnov test. In our data used to calculate the compound distribution we will use this fitted distribution. In order to pass the kolmogorov test the p-value must be >0.05 and in this case the pareto distribution is a reasonable choice for representing the data. By fitting the data to a pareto distribution we can caluclate the shape and scale of the distribution which will make it easier to visualise and create a compound distribution for the insurance portfolio.
```{r}
lambda.exp<-1/mean.loss
sprintf("lambda of exponential:%.10f",lambda.exp)


#EXPONENTIAL FITTING
hist<-ggplot(data=loss)
hist+geom_histogram(aes(x=loss,y=..density..),binwidth=50,color="blue",fill="blue",alpha=0.5)+ggtitle("EXPONENTIAL FITTING")+labs(y="Density",x="Loss Size")+theme_minimal()+stat_function(fun=dexp,args=list(rate=lambda.exp),color="red")#plot of exponential fitting distribution

#Chi-squared test

fit.exp<-fitdist(lossvec,dexp,method=c("mle"))#'fitting data to an exponential distribution
fit.exp

ks.test.exp<-ks.test(lossvec,pexp,rate=0.002670719)#goondess of fit test using kolmogorov smirnov
ks.test.exp
#ot does not pass the test as p-value<0.05
```
An exponential fitting is not an appropriate distribution to visualise loss severity as it does not pass the kolmogorov smirnov test because the p-value is <0.05. Hence we can disregard this distribution for the remaining report. There is a better distribution to distribute the data such as the pareto distribution.

```{r}
sigma.logn <- sqrt(log(((sd.loss/mean.loss)^2)+1))
mui.logn <- log(mean.loss) - (0.5*(sigma.logn^2))
sprintf("sigma of lognormal:%.2f",sigma.logn)
sprintf("mui of lognormal:%.2f",mui.logn)


#LOGNORMAL FITTING
hist<-ggplot(data=loss)
hist+geom_histogram(aes(x=loss,y=..density..),binwidth=50,color="blue",fill="blue",alpha=0.5)+ggtitle("LOGNORMAL FITTINGs")+labs(y="Dessity",x="Loss Size")+theme_minimal()+stat_function(fun = dlnorm,
                       args = list(meanlog = mui.logn, sdlog = sigma.logn),
                       colour = "red") #plot of lognormal fitted

#Chi-sqaured test
fit.lnor<-fitdist(lossvec,dlnorm,method=c("mle"))#fitting the data to lognormal distribution
fit.lnor

ks.test.lnor<-ks.test(lossvec,plnorm,meanlog=5.236208,sdlog=1.327718)#goodness of fit test kolmogorv smirnov
ks.test.lnor
#'does not pass the test as p-value is <0.05



```
A lognormal fitting is not an appropriate distribution to visualise loss severity as it does not pass the kolmogorov smirnov test as the p-value is <0.05. Hence we can disregard this distribution for the remaining report. The pareto distribution is the best chosen distribution to represent the dataset.
```{r}
#Extra analysis carried out to see the number of claims that fall in the loss severity intervals shown

sim.exp<-rpareto(800,shape=alpha.pareto,scale=lambda.pareto)#simulation of pareto distribution with 800 datapoints
ssim.exp<-sort(sim.exp)#sort in ascending order

breaks<-seq(from=min(lossvec),to=max(lossvec),by=100)
tab<-cut(lossvec,breaks,right=FALSE,include.lowest=TRUE)
table(tab)#creates number of observed values from emperical data

sbreaks<-seq(from=min(sim.exp),to=max(sim.exp),by=100)
stab<-cut(sim.exp,sbreaks,right=FALSE,include.lowest=TRUE)
table(stab)#creates number of observed values from simulated data
#simluated and emperical values are similar

```
This is extra analysis I have undertaken in order to test the accuracy of my assumption to fit the loss severity in pareto terms. The expected vs the empirical data for the intervals (which represent intervals for the loss severity amounts/claims size) are very similar. I have calculated the frequency of claims that fall into the various claim size intervals. N.B. This is not asked for in the question but is extra work I have included to cross check my accuracy. I can conclude from this observation that the pareto is a strong choice for distributing the loss size for claims.

```{r}
set.seed(123)
sim.pois<-rpois(10,lambda=297)#simulate poisson distribution for frequency

#forming table of expected vs emperical frequency data
actvsexp<- matrix(c(freqvec, sim.pois), ncol=10, byrow=TRUE)
rownames(actvsexp) <- c("actual", "expected")
colnames(actvsexp) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
as.table(actvsexp)#creates table

```
This is the table of expected vs actual number of claims. The expected number is observed from a random poisson simulation using the lambda of 297.2 and for 10 datapoints. The results are similar and hence it affirms the hypothesis that claims frequencies are poisson distributed. The reason I have chosen to distribute the claims frequency in poisson terms are indicated above in my report. Using a poisson distribution lowers the complexity of the model and simplifies the mathematics in order to calculate the compound distribution.

```{r}
a.pareto <- 4.631763 # from fitted distribution for pareto as above
l.pareto <- 1365.518382#from fitted distribution for pareto as above
m.poiss <- 297.2#mean of frequency vector

EXX <- (l.pareto/(a.pareto-1))
EXS <- m.poiss*(l.pareto/(a.pareto-1))#mean of compound distribution/expected aggregate losses for 10 years

VARx <-(a.pareto*l.pareto^2)/((a.pareto-1)^2*(a.pareto-2))

VARS <- (m.poiss*VARx) + (var.freq*(EXX)^2)#variance of compound distribution

m3<-((gamma(a.pareto-3)*gamma(1+3))/gamma(a.pareto))*l.pareto^3
m2<-((gamma(a.pareto-2)*gamma(1+2))/gamma(a.pareto))*l.pareto^2
COE<-m.poiss*m3/(m.poiss*m2)^1.5


sprintf("mean of compound distribution:%.2f",EXS)
sprintf("variance of compound distribution:%.2f",VARS)
sprintf("coefficint of skewness:%2f",COE)

```
The above code details the method of calculating the compound distribution using mathematical techniques indicated such as the collective risk model in order to combine the loss frequency and loss severity to form an aggregate expected loss function. The new E[S] is formed by the E[N]*m1. Which is the mean of the claims frequency multiplied by the mean of the claims severity. The variance of the compound distribution is found by using a different mathematical formula, as indicated by the above code(line190). The collective risk model is appropriate to use instead of the individual claims as the data given shows the losses in the portfolio as opposed to studying a fixed number of policies where the claim size can be potentially Â£0. In the collective risk model, the frequency and size of claims are random variables. Here the total losses are :
S=X1+X2+...+XN, where each individual claim is noted by Xi. It is assumed that the number of claims and size of claims are not affected by one another and hence are independant.

```{r}
set.seed(123)
numsim <- 10000#number of simulations
CD<- numeric(numsim)#CD=compund distribution
m.poiss <- 297.2#mean of poisson distribution
a.pareto <- 4.631763#alpha pareto from fitted distribution
l.pareto <- 1365.518382#lambda paretio from fitted distribution
for (i in seq.int(numsim)){ N <- rpois(1, m.poiss) 
CD[i]<- sum(rpareto(N, shape = a.pareto, scale= l.pareto))}



sprintf("mean of simultaed compound distribution:%.2f",mean(CD))
sprintf("variance of simulated compound distribution:%.2f",var(CD))
sprintf("skewness of simulated compound distribution:%.2f",skewness(CD))

```
After writing a simulation method of creating a compound distribution it can be seen that the values are very similar to the empirical data compound distribution and hence solidifies my assumption that the poisson and pareto distribution are adequate representations of claims frequency and claims severity. 










PART 2:

---
title: "Mini Project Applied Statistics Q4"
output:
  html_document:
    df_print: paged
---


```{r}
library(tidyverse)
library(knitr)
library(actuar)
library(moments)
library(fitdistrplus)
library(plyr)
```

```{r}
loss<-read.csv("loss_data(4).csv")#loading data into R
freq<- read.csv("frequency_data.csv")#loading data into R

lossvec<-c(loss$loss)#converting loss data into a vector
freqvec<-c(freq$freq)#Converting frequency data intot vector
```

```{r}
sc<-(freq/100)*120#scaling data to 120 stores
scale.freq.mean<-mean(sc$freq)#new expected number of claims per annumm
sprintf("mean for scaled frequency for 120 stores:%.2f",scale.freq.mean)

scale.lossvec<-lossvec*1.1#scaling 10% increase in loss amounts
mean.scale.lossvec<-mean(scale.lossvec)#new expected value of loss severity
var.scale.lossvec<-var(scale.lossvec)#new variance of loss severity
sprintf("mean of scaled loss vector:%.2f",mean.scale.lossvec)
sprintf("variance of scaled loss vector:%.2f",var.scale.lossvec)
```
What I have done is scaled the data to now represent claims frequency to 120 stores rather than per 100 stores. I have also increased the loss severity by 10% as indicated by the adjustments. This creates a new dataset for me to run the compound distribution calculation and to find a new aggregate expected loss over the next 1 year. The mean and variance of the dataset have changed and hence we will generate a different compound distribution, which will give a higher expected aggregate loss over the 1 year.

```{r}
set.seed(123)
fit.spareto<-fitdist(scale.lossvec,dpareto,method=c("mle"))#fitting the new scaled loss data to a pareto distribution
fit.spareto


snumsims<-10000#number of simulations
SCD<-numeric(snumsims)
mean.spois<-scale.freq.mean#mean of scaled frequency data
a.spareto<-4.635319#alpha pareto from fitted distribution below
l.spareto<-1503.704460#lambda pareto from fitted sitribution below

for (i in seq.int(snumsims)){ Ns <- rpois(1, mean.spois) 
SCD[i]<- sum(rpareto(Ns, shape = a.spareto, scale= l.spareto))}

sprintf("expected losses on portfolio for 10 years:%.2f",mean(SCD))#Expected total losses on the portfolio
sprintf("variance of scaled compound distribution:%.2f",var(SCD))
sprintf("standard deviation of compound distribution:%.2f",sd(SCD))
```
After calculating the new dataset I have run a simulated compound distribution which follows a compound poisson distribution with individual pareto distributions. This gives me the aggregate expected losses for the next one year. This value of the E[S]for the new dataset. The variance and standard deviation for this new distribution is also outputted. It is clear to see that the expected losses(Â£147494) are indeed higher than that previously calculated(Â£111857.35).

```{r}
#premium calculation
AggRevenue<-(1.027*mean(SCD))#2.7% is the premium loading factor giving ROIC of 15%...AggRev is the agregate revenue for 1 years for the insurance company from premiums
sprintf("aggregate revenue for 1 years:%.2f",AggRevenue)
premium<-AggRevenue/(120)#divide by 120 stores and for 1 years to get the premium for 1 store per annumm
sprintf("premium:%.2f",premium)
```
I have multiplied the expected aggregate losses by the appropriate factor of [1+0.027], where the 2.7% is my premium loading factor. This new figure of £151476 gives the aggregate revenue for the insurance company for the next one year. Upon calculating the aggregate revenue, I divided this by the number of stores the insurance company will cover(120) in order to get the annual premium paid by each store (Â£1262.30). There are a number of assumptions I have made throughout the report I am presenting.

Assumptions:
1)The only form of revenue for the insurance company will be coming from premiums
2)The only costs will be the claims made on the policies.
3)The number of claims does not affect the size of the claims
4)The premium amount will be assumed to be for a general insurance policy whereas there may indeed be fluctuations based on the risk level of each individual supermarket.

```{r}
ini.surplus<-2.33*(sd(SCD))-0.027*mean(SCD)#approximating the compound distibution to a normal distributio of random variables. 2.33 is the Z value for a likelyhood of < 1% ruin. 2.7% is the premium loading factor
sprintf("initial investmentl:%.2f",ini.surplus)
yrloss<-(mean(SCD))#aggregate loss on the portfolio for 1 year
U<-ini.surplus+(premium*120)-yrloss#using surplus process caluclated the cash in porfoilio at end of year 1
sprintf("end of year one balance in porfolio:%.2f",U)
ROIC<-100*((U-ini.surplus)/ini.surplus)#return on invested capital
sprintf("return on invested capital :%.1f",ROIC)
```
The specifications of minimum expected return of 15% and chance of ruin of no greater than 1% are taken into account in the code above to calculate the minimum initial investment of (£26,2241). We then find out the end of year 1 portfolio balance will be (Â£30,223), which is an increase in value. The ROIC is the difference between the values at the start and end of year divided by the initial investment of (Â£26,241). This outputs an ROIC of 15.2% which is just above our hurdle rate of 15%. Also we can test the sensitivity by changing the premium loading factor to experiment different associated ROIC's based on the change in premium loading. Please see below for sensitivity tests based on varying premium loading factors.

```{r}
#test1
sAggRevenue<-(1.15*mean(SCD))#15% is the premium loading factor giving ROIC of 273%...AggRev is the agregate revenue for 1 years for the insurance company from premiums
sprintf("aggregate revenue for 1 years:%.2f",sAggRevenue)
spremium<-sAggRevenue/(120)#divide by 120 stores to get the premium for 1 store per annumm
sprintf("premium:%.2f",spremium)

sini.surplus<-2.33*(sd(SCD))-0.15*mean(SCD)#approximating the compound distibution to a normal distributio of random variables. 2.33 is the Z value for a likelyhood of < 1% ruin. 15% is the premium loading factor
sprintf("initial investmentl:%.2f",sini.surplus)
syrloss<-(mean(SCD))#aggregate loss on the portfolio for 1 year
sU<-sini.surplus+(spremium*120)-syrloss#using surplus process caluclated the cash in porfoilio at end of year 1
sprintf("end of year one balance in porfolio:%.2f",sU)
sROIC<-100*((sU-sini.surplus)/sini.surplus)#return on invested capital
sprintf("return on invested capital :%.1f",sROIC)
```
```{r}
aAggRevenue<-(1.01*mean(SCD))#5% is the premium loading factor giving ROIC of 3.2%...AggRev is the agregate revenue for 10 years for the insurance company from premiums
sprintf("aggregate revenue for 1 years:%.2f",aAggRevenue)
apremium<-aAggRevenue/(120)#divide by 120 stores get the premium for 1 store per annumm
sprintf("premium:%.2f",apremium)

aini.surplus<-2.33*(sd(SCD))-0.01*mean(SCD)#approximating the compound distibution to a normal distributio of random variables. 2.33 is the Z value for a likelyhood of < 1% ruin. 5% is the premium loading factor
sprintf("initial investmentl:%.2f",aini.surplus)
ayrloss<-(mean(SCD))#aggregate loss on the portfolio for 1 year
aU<-aini.surplus+(apremium*120)-ayrloss#using surplus process caluclated the cash in porfoilio at end of year 1
sprintf("end of year one balance in porfolio:%.2f",aU)
aROIC<-100*((aU-aini.surplus)/aini.surplus)#return on invested capital
sprintf("return on invested capital :%.1f",aROIC)
```
To conclude the report it is clear to see there is a direct positive relationship between premium loading and the Return on invested capital. However, the higher the premium loading means a lower minimum initial capital required. It is wise to note that if the premium loading is too high then it means the premiums may indeed be too expensive for the clients to afford and hence it is necessary to come to a balance on charging too high a premium and returns. It is wise to keep the initial capital required lower if possible as raising funds before the functioning of the insurance company will require gearing or giving away significant equity within the company. To recommend, I would say that a premium loading of 2.7% is reasonable which gives ROIC 15%. This is healthy returns as the S&P500 returns average annual returns of this amount.


