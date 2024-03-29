install.packages("readxl")
library(readxl)
cpuInfo <- read.csv("/Users/khuenguyen/Desktop/CPU_benchmark_v4.csv")
View(cpuInfo)
cpuInfo <- cpuInfo[,c("price", "cpuMark", "threadMark", "TDP", "powerPerf","category")]
colSums(is.na(cpuInfo))
summary(cpuInfo)
ggplot(df_combined, aes(price, category, colour=category))+
  geom_boxplot(show.legend = FALSE)+
  labs(x="Platform",y="Price",title="Price for each platform")

ggplot(df_combined, aes(cpuMark, category, colour=category))+
  geom_boxplot(show.legend = FALSE)+
  labs(x="Platform",y="Score",title="Multi Thread Benchmark for each platform")

ggplot(df_combined, aes(cpuValue, category, colour=category))+
  geom_boxplot(show.legend = FALSE)+
  labs(x="Platform",y="Score/Price",title="Multi Thread score per price for each platform")

ggplot(df_combined, aes(threadMark, category, colour=category))+
  geom_boxplot(show.legend = FALSE)+
  labs(x="Platform",y="Score",title="Single Thread benchmark for each platform")

ggplot(df_combined, aes(threadValue, category, colour=category))+
  geom_boxplot(show.legend = FALSE)+
  labs(x="Platform",y="Score/Price",title="Single Thread score per price for each platform")

ggplot(df_combined, aes(TDP, category, colour=category))+
  geom_boxplot(show.legend = FALSE)+
  labs(x="Platform",y="Watt",title="Electricity Usage for each platform")

ggplot(df_combined, aes(powerPerf, category, colour=category))+
  geom_boxplot(show.legend = FALSE)+
  labs(x="Platform",y="Score/Watt",title="CPU strength over electricity usage for each platform")

ggplot(df_combined, aes(cores, category, colour=category))+
  geom_boxplot(show.legend = FALSE)+
  labs(x="Platform",y="Cores",title="CPU cores for each platform")
hist(cpuInfo$price, breaks = 20, col = 2, xlab = "Price", xlim = c(0,8000), main =
              "Histogram of CPU price", labels = T)
hist(cpuInfo$cpuMark, breaks = 20, col = 3, xlab = "CPU Mark", xlim = c(0,80000), main =
              "Histogram of CPU Mark", labels = T)
hist(cpuInfo$threadMark, breaks = 20, col = 4, xlab = "Thread Mark", xlim = c(0,4000),
            main = "Histogram of Thread Mark", labels = T)
hist(cpuInfo$TDP, breaks = 20, col = 5, xlab = "TDP",xlim = c(0,250), main = "Histogram of
TDP", labels = T)
hist(cpuInfo$powerPerf, breaks = 20, col = 6, xlab = "Power Performance",xlim = c(0,1000),
            main = "Histogram of Power Performance", labels = T)
       attach(mtcars)
       par(mfrow=c(2,2))
plot(cpuInfo$powerPerf,cpuInfo$price, main="price vs power performance",
            xlab="powerPerf (point) ", ylab="price (USD)", pch=19)
plot(cpuInfo$cpuMark,cpuInfo$price, main="price vs cpu mark",
            xlab="cpuMark (point) ", ylab="price (USD)", pch=19)
plot(cpuInfo$threadMark,cpuInfo$price, main="price vs thread mark",
            xlab="threadMark (point) ", ylab="price (USD)", pch=19)
plot(cpuInfo$TDP,cpuInfo$price, main="price vs TDP",
            xlab="TDP (W) ", ylab="price (USD)", pch=19)
laptop <- (cpuInfo[cpuInfo$category == ’Laptop’s,])
laptop = subset(laptop, select = c(price, cpuMark, threadMark, powerPerf, TDP, cpuValue,
                                   threadValue))
View(laptop)
par(mfrow=c(2,4))
boxplot(laptop$cpuMark)
boxplot(laptop$threadMark)
boxplot(laptop$threadValue)
boxplot(laptop$cpuValue)
boxplot(laptop$TDP)
boxplot(laptop$powerPerf)
boxplot(laptop$price)
quartiles <- quantile(laptop$price, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(laptop$price)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR
laptop <- subset(laptop, laptop$price > Lower & laptop$price < Upper)
library(ggplot2)
library(GGally)
ggpairs(laptop, columns = 1:7)
newData_2 <- laptop
newData_2[, c("price","cpuMark","powerPerf","threadValue","cpuValue")] <- log(newData_2[,
                                                                                        c("price","cpuMark","powerPerf","threadValue","cpuValue")]+1)
ggpairs(newData_2, columns = 1:7)
library(corrplot)
library(RColorBrewer)
par(mfrow=c(1,1))
CR <- cor(newData_2)
corrplot(CR, type="lowe", order="hclust",col=brewer.pal(n=3, name="RdYlBu"))
linearModule <-lm(price~TDP,data=train)
summary(linearModule)
sample <- sample(c(TRUE, FALSE), nrow(newData_2), replace=TRUE, prob=c(0.9,0.1))
train <- newData_2[sample , ]
test <- newData_2[!sample ,]
install.packages("BMA")
library(BMA)
search_for_models = bicreg(subset(train, select = c(cpuMark, threadMark, powerPerf,TDP)),
                           train$price, strict=FALSE, OR=20)
summary(search_for_models)
ggpairs(subset(train, select = c(price, powerPerf, TDP)), columns = 1:3)
linearModule <-lm(price~TDP+powerPerf,data=train)
summary(linearModule)
SSE <- (linearModule$residuals ^ 2) %>% sum()
SSR <- ((linearModule$fitted.values - mean(train$price)) ^ 2) %>% sum()
SST <- ((train$price - mean(train$price)) ^ 2) %>% sum()
ss <- as.data.frame(c(SSE, SSR, SST), row.names = c("SSE", "SSR", "SST"))
names(ss) <- c("Value")
ss
library(car)
vif(linearModule)
par(mfrow=c(1,1))
plot(linearModule, col = "steel blue", which = 1)
plot(linearModule, col = "steel blue", which = 2)
plot(linearModule, col = "steel blue", which = 3)
plot(linearModule, col = "steel blue", which = 5)
predicts <- predict(linearModule, newdata = test)
actuals <- test$price
evaluate <- data.frame(actuals ,predicts)
summary(evaluate)
SSE <- (m$residuals ^ 2) %>% sum()
SSE
m <- lm(actuals~predicts,data=evaluate)
summary(m)