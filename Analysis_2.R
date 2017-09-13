library(data.table); library(tibble); library(dplyr)
library(boot)  # for logit transformation and its inverse
library(lubridate) # convert date to day of week
library(survival)
library(MASS)
library(bestglm)

tb <- fread("new.csv") %>% as.tibble
tb <- tb[, -1]
tb <- tb[-which(tb$FailureTime==0), ]

tb$classTag <- tb$classTag %>% as.factor
tb$Reply <- tb$Reply %>% as.factor
tb$Evening <- tb$Evening %>% as.factor
tb$weekend <- tb$weekend %>%  as.factor
tb$classTag <- as.factor(tb$classTag)
tb$Reply <- as.factor(tb$Reply)
tb$garbage <- as.factor(tb$garbage)
tb$graphId <- as.factor(tb$graphId)
tb$youtubeId <- as.factor(tb$youtubeId)
tb$httpId <- as.factor(tb$httpId)

# set the cut-off point
time_pt <- 3600
# set the right censoring data
evnt <- ifelse(tb$FailureTime <= time_pt, 1, 0)
tb$FailureTime[tb$FailureTime > time_pt] <- time_pt
# Survival notation
recsurv <- Surv(tb$FailureTime, event = evnt)
artUrl <- tb$artUrl
tb <- cbind(recsurv, tb[, -c(1, dim(tb)[2])])

# sampling from the original data
tb <- tb[sample(dim(tb)[1], 3000), ]



# Distributions' names
dist_nm <- c("exp", "weibull", "lnorm", "llogis", "gamma", "gengamma")
dist_name <- c("Exponential", "Weibull", "Log-normal", "Log-logistic", "Gamma", "Generalized Gamma")


#####################
pdf("Figure/fig_KM.pdf")
plot(survfit(recsurv~1), xaxt = "n", xlab = "time (minute)", ylab = "", main = "")
axis(side = 1, at = seq(0, 60, 10)*60, labels = seq(0, 60, 10))
dev.off()
# Fitting Distributions
library(flexsurv)

fits <- 
  sapply(1:6, function(d){
    list(flexsurvreg(formula = recsurv~1, dist = dist_nm[d]))
  })

# Plotting
pdf("Figure/fig_survfit_dist.pdf", width = 14, height = 10)
  par(mfrow = c(2, 3))
    sapply(1:6, function(d){
      plot(fits[[d]], main = dist_name[d], 
           xaxt = "n", xlab = "time (minute)", ylab = "")
      axis(side = 1, at = seq(0, 60, 10)*60, labels = seq(0, 60, 10))
    })
  par(mfrow = c(1, 1))
dev.off()

sapply(1:6, function(d){
  cbind(dist_name[d], 
        fits[[d]]$AIC    %>% round(2), 
        fits[[d]]$loglik %>% round(2))
}) %>% t %>% data.frame(row.names = 1)


#####################
### color selection
clr <- c("#0000FF", "#FF0000", "#006400", "#708090", "#FF00FF", "#9ACD32", "#D2691E")


#### Reply or not
fit_Reply <- survfit(formula = recsurv ~ Reply, data = tb, 
                    type = "kaplan-meier", conf.type="log-log")

pdf("Figure/fig_re_KM.pdf")
plot(fit_Reply, col = clr, lwd = 2,
     xlab = "time (minute)", ylab = "", main = "", xaxt = "n")
axis(side = 1, at = seq(0, 60, 10)*60, labels = seq(0, 60, 10))
legend("topright", legend = levels(tb$Reply), col = clr, lty = 1, lwd = 2)
dev.off()

#### Class
fit_class <- survfit(formula = recsurv ~ classTag, data = tb, 
                     type = "kaplan-meier", conf.type="log-log")
pdf("Figure/fig_cls_KM.pdf")
plot(fit_class, col = clr, lwd = 2,
     xlab = "time (minute)", ylab = "", main = "", xaxt = "n")
axis(side = 1, at = seq(0, 60, 10)*60, labels = seq(0, 60, 10))
legend("topright", legend = levels(tb$classTag), col = clr, lty = 1, lwd = 2)
dev.off()

#### Evening
fit_Evening <- survfit(formula = recsurv ~ Evening, data = tb, 
                     type = "kaplan-meier", conf.type="log-log")
pdf("Figure/fig_Evening_KM.pdf")
plot(fit_Evening, col = clr, lwd = 2,
     xlab = "time (minute)", ylab = "", main = "", xaxt = "n")
axis(side = 1, at = seq(0, 60, 10)*60, labels = seq(0, 60, 10))
legend("topright", legend = levels(tb$Evening), col = clr, lty = 1, lwd = 2)
dev.off()


####  Weekend
fit_weekend <- survfit(formula = recsurv ~ weekend, data = tb, 
                    type = "kaplan-meier", conf.type="log-log")
pdf("Figure/fig_weekend_KM.pdf")
plot(fit_weekend, col = clr, lwd = 2,
     xlab = "time (minute)", ylab = "", main = "", xaxt = "n")
axis(side = 1, at = seq(0, 60, 10)*60, labels = seq(0, 60, 10))
legend("topright", legend = levels(tb$weekend), col = clr, lty = 1, lwd = 2)
dev.off()

####  Garbage
fit_garbage <- survfit(formula = recsurv ~ garbage, data = tb, 
                       type = "kaplan-meier", conf.type="log-log")
pdf("Figure/fig_garbage_KM.pdf")
plot(fit_garbage, col = clr, lwd = 2,
     xlab = "time (minute)", ylab = "", main = "", xaxt = "n")
axis(side = 1, at = seq(0, 60, 10)*60, labels = seq(0, 60, 10))
legend("topright", legend = levels(tb$garbage), col = clr, lty = 1, lwd = 2)
dev.off()

####  graphId
fit_graphId <- survfit(formula = recsurv ~ graphId, data = tb, 
                       type = "kaplan-meier", conf.type="log-log")
pdf("Figure/fig_graphId_KM.pdf")
plot(fit_garbage, col = clr, lwd = 2,
     xlab = "time (minute)", ylab = "", main = "", xaxt = "n")
axis(side = 1, at = seq(0, 60, 10)*60, labels = seq(0, 60, 10))
legend("topright", legend = levels(tb$graphId), col = clr, lty = 1, lwd = 2)
dev.off()

####  youtubeId
fit_youtubeId <- survfit(formula = recsurv ~ youtubeId, data = tb, 
                       type = "kaplan-meier", conf.type="log-log")
pdf("Figure/fig_youtubeId_KM.pdf")
plot(fit_youtubeId, col = clr, lwd = 2,
     xlab = "time (minute)", ylab = "", main = "", xaxt = "n")
axis(side = 1, at = seq(0, 60, 10)*60, labels = seq(0, 60, 10))
legend("topright", legend = levels(tb$youtubeId), col = clr, lty = 1, lwd = 2)
dev.off()

####  httpId
fit_httpId <- survfit(formula = recsurv ~ httpId, data = tb, 
                         type = "kaplan-meier", conf.type="log-log")
pdf("Figure/fig_httpId_KM.pdf")
plot(fit_httpId, col = clr, lwd = 2,
     xlab = "time (minute)", ylab = "", main = "", xaxt = "n")
axis(side = 1, at = seq(0, 60, 10)*60, labels = seq(0, 60, 10))
legend("topright", legend = levels(tb$httpId), col = clr, lty = 1, lwd = 2)
dev.off()

#### ask re/orgn
#fit_ask_re <- survfit(formula = 
#                        recsurv[tb$classTag == "ask" & tb$FailureTime < 3600] ~ 
#                        tb$Reply[tb$classTag == "ask" & tb$FailureTime < 3600], 
#                      type = "kaplan-meier", conf.type="log-log")
#pdf("Figure/fig_ask_re_KM.pdf")
#plot(fit_ask_re, col = c(2, 4), lwd = 2,
#     xlab = "time (minute)", ylab = "", main = "", xaxt = "n")
#axis(side = 1, at = seq(0, 60, 10)*60, labels = seq(0, 60, 10))
#legend("topright", legend = levels(tb$Reply), col = c(2, 4), lty = 1, lwd = 2)
#dev.off()


## Cox PH #####################

## The oridinal model
fmla_0 <- 
  recsurv ~ 
  classTag + Reply + Evening + weekend + 
  graphId + httpId + youtubeId + 
  garbage + contentLength + policy_scores
fit_cox_0 <- coxph(formula = fmla_0, data = tb)
sum_0 <- summary(fit_cox_0)
sum_0$coefficients[, c(1, 2, 5)] %>% round(4)
stepAIC_0 <- stepAIC(fit_cox_0, direction = "both")
sum_0_step <- summary(stepAIC_0)
sum_0_step$coefficients[, c(1, 2, 5)] %>% round(4)

fmla_1 <- 
  recsurv ~ classTag + Reply + Evening +
  weekend + garbage + contentLength 
fit_cox_1 <- coxph(formula = fmla_1, data = tb)
cox.zph(fit_cox_1)[[1]][, 3]>.05


fmla_2 <- 
  recsurv ~ strata(classTag) + Reply + strata(Evening) +
  weekend + strata(garbage) + strata(contentLength) 
fit_cox_2 <- coxph(formula = fmla_2, data = tb)
stepAIC(fit_cox_2)
