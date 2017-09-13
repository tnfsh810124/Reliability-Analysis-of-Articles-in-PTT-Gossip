library(data.table); library(tibble); library(dplyr)
library(boot)  # for logit transformation and its inverse
library(lubridate) # convert date to day of week
library(survival)

############
tb <- as_tibble(fread("df_3362_with_policy_socores.csv"))
tb <- tb[, -2]
names(tb)[1] <- "ID"
names(tb)[3] <- "FailureTime"
summary(tb)
names(tb)[1] <- "ID"
tb$ID <- as.factor(tb$ID)
tb$classTag[tb$classTag %in% c("ask", "news")==F] <- "others"
tb$classTag <- as.factor(tb$classTag)
tb$reId <- as.factor(tb$reId)
tb$garbage <- as.factor(tb$garbage)
tb$graphId <- as.factor(tb$graphId)
tb$youtubeId <- as.factor(tb$youtubeId)
tb$httpId <- as.factor(tb$httpId)

Reply <- tb$reId

Dayparts <- 
  sapply(1:dim(tb)[1], FUN = function(i){
    strsplit(
      strsplit(
        tb$startTime[i], split = c(" ")
      )[[1]][2], 
      split = ":"
    )[[1]][1]
  }) %>% as.numeric  %>% 
  cut(., breaks = c(-Inf, 1, 7, 13, 19, Inf), right = T, 
      labels = c("evening", "night", "morning", "afternoon", "vening")) %>% as.character
Dayparts <- 
  sapply(1:length(Dayparts), function(k){
    ifelse(Dayparts[k] == "vening", yes = "evening", no = Dayparts[k])
  }) 

Dayparts[Dayparts!="evening"] <- "others"
Dayparts <- as.factor(Dayparts)
Evening <- ifelse(Dayparts=="evening", 1, 0)

week <- 
  sapply(1:dim(tb)[1], function(i){
    wday(strsplit(tb$startTime[i], split = " ")[[1]][1], label=TRUE, abbr = F)
  })
week <- as.factor(week)

weekend <- ifelse(week %in% c("Saturday", "Sunday") | 
                    (week %in% c("Monday", "Friday") & Dayparts == "evening"), 
                  "weekend", "weekday") %>% as.factor %>% as.tibble
names(weekend) <- "weekend"
weekend <- ifelse(weekend == "weekend", 1, 0)

#brk <- 
#  c(c(0, 1, 3, 5, 10, 15, 20, 30, 40, 60, 80, 100, 120, 150, 
#      60*c(3, 4, 5, 6, 8, 10, 12, 15, 18, 21, 24), 
#      24*60*c(1.5, 2, 2.5, 3, 4, 5, 6, 7, 9, 11, 14))*60, 
#    Inf)
#lbl <- 
#  c(0, 1, 3, 5, 10, 15, 20, 30, 40, 60, 80, 100, 120, 150, 
#    60*c(3, 4, 5, 6, 8, 10, 12, 15, 18, 21, 24), 
#    24*60*c(1.5, 2, 2.5, 3, 4, 5, 6, 7, 9, 11, 14))*60

#FailureTime <- 
#  tb$FailureTime %>% cut(., breaks = brk, labels = lbl, right = F) %>% as.character %>% as.numeric

FailureTime <- tb$FailureTime
    
#Dead <- ifelse(FailureTime == tb$FailureTime, 1, 0)
  

tmp2 <- 
  tb %>% dplyr::select(classTag) %>% cbind(FailureTime, ., Reply, Evening, weekend) %>% as.tibble

tmp2 <- tb %>% dplyr::select(graphId, httpId, youtubeId, garbage, good, bad, 
                      contentLength, policy_scores) %>% cbind(tmp2, .)
tmp2 <- tb %>% dplyr::select(artUrl) %>% cbind(tmp2, .)
write.csv(tmp2, file = "new.csv")

