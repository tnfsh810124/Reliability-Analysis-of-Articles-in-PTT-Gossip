library(data.table); library(tibble); library(dplyr)
library(boot)  # for logit transformation and its inverse
library(lubridate) # convert date to day of week
library(survival)

tb <- fread("new.csv") %>% as.tibble
tb <- tb[, -1]

tb$classTag <- tb$classTag %>% as.factor
tb$reId <- tb$reId %>% as.factor
tb$Dayparts <- tb$Dayparts %>% as.factor
tb$week <- tb$week %>%  as.factor
weekend <- ifelse(tb$week %in% c(1, 7) | (tb$week %in% c(2, 6) & tb$Dayparts == "evening"), 
                  1, 0) %>% as.factor %>% as.tibble
names(weekend) <- "weekend"
tb <- tb %>%  bind_cols(., weekend) %>% as.tibble


# set the cut-off point
time_pt <- 3600

# sampling from the original data
smp <- sample(dim(tb_1)[1], 500)

# set the right censoring data
evnt <- ifelse(tb$FailureTime <= time_pt, 1, 0)
tb$FailureTime[tb$FailureTime > time_pt] <- time_pt

# Survival notation
recsurv <- Surv(tb$FailureTime, event = evnt)



#### Density plot
pdf("Figure/fig_pdf.pdf")
plot(density(tb$FailureTime[tb$FailureTime < 3600]), 
     xaxt = "n", xlab = "time (minute)", main = "", ylab = "", lwd = 2)
axis(side = 1, at = seq(0, 60, 10)*60, labels = seq(0, 60, 10))
dev.off()

#### KM
fit_all <- survfit(formula = recsurv[tb$FailureTime < 3600] ~ 1, type = "kaplan-meier", conf.type="log-log")
pdf("Figure/fig_KM_all.pdf")
plot(fit_all, lwd = 2,
     xlab = "time (minute)", ylab = "", main = "", xaxt = "n")
axis(side = 1, at = seq(0, 60, 10)*60, labels = seq(0, 60, 10))
dev.off()

fit_smp <- survfit(formula = recsurv[smp] ~ 1, type = "kaplan-meier", conf.type="log-log")
pdf("Figure/fig_KM_smp.pdf")
plot(fit_smp, lwd = 2,
     xlab = "time (minute)", ylab = "", main = "", xaxt = "n")
axis(side = 1, at = seq(0, 60, 10)*60, labels = seq(0, 60, 10))
dev.off()

pdf("Figure/fig_KM.pdf", width = 9, height = 5)
par(mfrow = c(1, 2))
plot(fit_all, lwd = 1.5,
     xlab = "time (minute)", ylab = "", main = "n = 339,346", xaxt = "n")
axis(side = 1, at = seq(0, 60, 10)*60, labels = seq(0, 60, 10))
plot(fit_smp, lwd = 1.5,
     xlab = "time (minute)", ylab = "", main = "n = 500", xaxt = "n")
axis(side = 1, at = seq(0, 60, 10)*60, labels = seq(0, 60, 10))
dev.off()



### sample size 
smp <- sample(dim(tb_1)[1], dim(tb_1)[1])

### color selection
clr <- c("#0000FF", "#FF0000", "#006400", "#708090", "#FF00FF", "#9ACD32", "#D2691E")



#### Reply or not
fit_reId <- survfit(formula = recsurv[smp] ~ tb$reId[smp], 
                    type = "kaplan-meier", conf.type="log-log")

pdf("Figure/fig_re_KM.pdf")
plot(fit_reId, col = clr, lwd = 2,
     xlab = "time (minute)", ylab = "", main = "", xaxt = "n")
axis(side = 1, at = seq(0, 60, 10)*60, labels = seq(0, 60, 10))
legend("topright", legend = levels(tb$reId), col = clr, lty = 1, lwd = 2)
dev.off()

#### Class
fit_class <- survfit(formula = recsurv[smp] ~ tb$classTag[smp], 
                     type = "kaplan-meier", conf.type="log-log")
pdf("Figure/fig_cls_KM.pdf")
plot(fit_class, col = clr, lwd = 2,
     xlab = "time (minute)", ylab = "", main = "", xaxt = "n")
axis(side = 1, at = seq(0, 60, 10)*60, labels = seq(0, 60, 10))
legend("topright", legend = levels(tb$classTag), col = clr, lty = 1, lwd = 2)
dev.off()

#### daypart
fit_dpart <- survfit(formula = recsurv[smp] ~ tb$Dayparts[smp], 
                     type = "kaplan-meier", conf.type="log-log")
pdf("Figure/fig_dpart_KM.pdf")
plot(fit_dpart, col = clr, lwd = 2,
     xlab = "time (minute)", ylab = "", main = "", xaxt = "n")
axis(side = 1, at = seq(0, 60, 10)*60, labels = seq(0, 60, 10))
legend("topright", legend = levels(tb$Dayparts), col = clr, lty = 1, lwd = 2)
dev.off()


####  Week
fit_week <- survfit(formula = recsurv[smp] ~ tb$week[smp], 
                     type = "kaplan-meier", conf.type="log-log")
pdf("Figure/fig_week_KM.pdf")
plot(fit_week, col = clr, lwd = 2,
     xlab = "time (minute)", ylab = "", main = "", xaxt = "n")
axis(side = 1, at = seq(0, 60, 10)*60, labels = seq(0, 60, 10))
legend("topright", legend = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), col = clr, lty = 1, lwd = 2)
dev.off()


#### ask re/orgn

fit_ask_re <- survfit(formula = 
                        recsurv[tb$classTag == "ask" & tb$FailureTime < 3600] ~ 
                        tb$reId[tb$classTag == "ask" & tb$FailureTime < 3600], 
                      type = "kaplan-meier", conf.type="log-log")

pdf("Figure/fig_ask_re_KM.pdf")
plot(fit_ask_re, col = c(2, 4), lwd = 2,
     xlab = "time (minute)", ylab = "", main = "", xaxt = "n")
axis(side = 1, at = seq(0, 60, 10)*60, labels = seq(0, 60, 10))
legend("topright", legend = levels(tb$reId), col = c(2, 4), lty = 1, lwd = 2)
dev.off()


