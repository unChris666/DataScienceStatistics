
#Untuk melakukan Instalasi, Anda bisa menggunakan syntax berikut ini :
install.packages(c("broom","caret","DataExplorer","grid","ISLR","pscl","tidyverse"))

#Jangan lupa untuk load package tersebut.
library(broom)
library(caret)
library(DataExplorer)
library(grid)
library(ISLR)
library(pscl)
library(tidyverse)

#Untuk mempermudah visualisasi beberapa bagian dalam Praktikum ini, digunakan fungsi arrange yang dibuat oleh Stephen Turner
vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
arrange <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
  dots <- list(...)
  n <- length(dots)
  if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}
  if(is.null(nrow)) { nrow = ceiling(n/ncol)}
  if(is.null(ncol)) { ncol = ceiling(n/nrow)}
  ## NOTE see n2mfrow in grDevices for possible alternative
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )
  ii.p <- 1
  for(ii.row in seq(1, nrow)){
    ii.table.row <- ii.row
    if(as.table) {ii.table.row <- nrow - ii.table.row + 1}
    for(ii.col in seq(1, ncol)){
      ii.table <- ii.p
      if(ii.p > n) break
      print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))
      ii.p <- ii.p + 1
    }
  }
}

#Eksplorasi Data
data(Default)
help(Default)
glimpse(Default)
summary(Default)
plot_histogram(Default)
plot_bar(Default)
plot_boxplot(Default,by="default")
a1<-Default %>% filter(student=="Yes") %>%
  select(balance,default) %>%  
  plot_boxplot(by="default", title="Default Occurrence on Student")
a2<-Default %>% filter(student=="No") %>%
  select(balance,default) %>%  
  plot_boxplot(by="default", title="Default Occurrence on Non-Student")
arrange(a1,a2)
## $page_1
## 
## $page_1
b1<-Default %>% filter(student=="Yes") %>%
  select(income,default) %>%  
  plot_boxplot(by="default", title="Default Occurrence on Student")
b2<-Default %>% filter(student=="No") %>%
  select(income,default) %>%  
  plot_boxplot(by="default", title="Default Occurrence on Non-Student")
arrange(b1,b2)
## $page_1
## 
## $page_1

#The Model, Estimation and Inference
reglog<-glm(default~balance,data=Default,family=binomial)
summary(reglog)
Default %>%
  mutate(prob = ifelse(default == "Yes", 1, 0)) %>%
  ggplot(aes(balance, prob)) +
  geom_point(alpha = .15) +
  geom_smooth(formula=y~x, method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("Balance") +
  ylab("Probability of Default")+
  theme_minimal()

#Interpreting coefficients
reglog$coefficients
exp(reglog$coefficients)
confint(reglog)

#Prediction
predict(reglog, newdata = data.frame(balance=1000), type="response")
reglog2<-glm(default~student,data=Default,family=binomial)
summary(reglog2)

#Categorical Independent Variable
predict(reglog2, newdata = data.frame(student="Yes"), type="res")
predict(reglog2, newdata = data.frame(student="No"), type="res")

#Multiple Logistic Regression
full.mod = glm(default~., data=Default,family=binomial)
summary(full.mod)

#Model Evaluation
pscl::pR2(reglog)["McFadden"]
pscl::pR2(reglog2)["McFadden"]
pscl::pR2(full.mod)["McFadden"]

#Lending Club Datasets
loan <- read_csv("https://www.dropbox.com/s/89g1yyhwpcqwjn9/lending_club_cleaned.csv?raw=1")
loan
summary(loan)
table(loan$good)
loan$good<-as.factor(loan$good)
loan

#Membagi Data
set.seed(5815)
sample <- sample(nrow(loan),floor(nrow(loan)*0.8))
train <- loan[sample,]
test <- loan[-sample,]

#Model
logit <- glm(good ~ fico + dti+ loan_amnt + purpose, data = train, family = "binomial")
summary(logit)

#Evaluasi Model
test$pred <- predict(logit, test, type="response")
test$good_pred <- ifelse(test$pred > 0.80, "good", "bad")
test$good_pred <- as.factor(test$good_pred)
(conf.mat<-caret::confusionMatrix(test$good_pred, test$good, positive="good"))
broom::tidy(conf.mat)

