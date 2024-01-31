library(foreign)
library(ggplot2)
library(MASS)
library(readxl)
library(pscl)
library(estimatr)
library(car)
library(caret)


dat <- read_excel("/Users/yijunwang/Documents/CSI_pallative_care/case_study2/weibo-search/结果文件/all_tweets&users_robot_removal_preprocessing_sentiment_topic修改.xlsx", sheet = "Sheet1")
summary(dat)


#generate factor variable

dat <- within(dat, {
  topic <- factor(topic, levels = c("1", "2", "3","4","5","6","7","11"), labels = c("topic1", "topic2", "topic3","topic4","topic5","topic6","topic7","topic8"))
  user_type <- factor(user_type, levels = c("-1", "0", "1","2","3","4","5","7"), labels = c("user1", "user2", "user3","user4","user5","user6","user7","user8"))
  gender <- factor(gender, levels = c("m", "f"), labels = c("male", "female"))
  
})

#Standardisation of some variables
dat$number_of_posts = log(dat$number_of_posts+1)
dat$number_of_followers = log(dat$number_of_followers+1)
dat$number_of_followings = log(dat$number_of_followings+1)
dat$engagement_number = log(dat$engagement_number+1)

#look at descriptive statistics of the dataset
summary(dat)
with(dat[dat$number_of_posts>10], table(number_of_posts))

#preprocess with some variables
dat$web_page_url[dat$web_page_url >0 ]<- 1
dat$picture_video = dat$picture + dat$video
dat$user_type[dat$user_type == "user7" ]<- "user5"
dat$user_type[dat$user_type == "user6" ]<- "user3"
dat <- within(dat, {
  user_type <- factor(user_type, levels = c("user1", "user2", "user3","user4","user5","user8"), labels = c("user1", "user2", "user3","user4","user5","user6"))
})
dat_new = dat[dat$topic != "topic8",]

library(moments)
skewness(dat_new$engagement_number)
kurtosis(dat_new$engagement_number)

#look at the corrlation between variables
dat1 <-subset(dat_new, select = - c(id, bid, user_id, user_name, content,number_of_forwards, number_of_comments, number_of_likes, topic, gender, user_type, keywords, user_info,sentiment,negative_prob,confidence, verified_info))
dat1_cor=cor(dat1)
corrplot(dat1_cor,method="number")

#build negative binomial models
summary(dat_new)

nb1 <- glm(engagement_number ~ topic+at_other_user+with_tag+picture_video+post_date+post_length+number_of_followers+user_type+web_page_url+positive_prob, data = dat_new, family = negative.binomial(2))
summary(nb1)
nb2 <- glm(engagement_number ~ topic+at_other_user+with_tag+picture+video+post_date+post_length+number_of_followers+user_type+web_page_url+positive_prob+topic*positive_prob, data = dat_new, family = negative.binomial(2))
summary(nb2)


#Test for multicollinearity

vif(nb1, digits = 3)
vif(nb2, digits = 3)
