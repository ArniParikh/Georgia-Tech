install.packages("survey")
library("survey")

#SUMMARY STATISTICS 1

#1
install.packages("readxl")
library(readxl)

getwd()
setwd(("D:/C tr/Desktop"))
data <- read.csv("401k.csv")

#2
data$education.level <- cut(
  data$educ,
  breaks = c(-Inf, 11, 12, 16, Inf),
  labels = c("no high school", "high school", "some college education", "college"),
  right = TRUE)

head(data)
print(data$education.level)
table(data$education.level)

#3
install.packages("dplyr")
library("dplyr")

install.packages("summarytools")
library("summarytools")

y <- data$p401
mean(y)
sd(y)

x <- data$e401
mean(x)
sd(x)

z <- data$net_tfa
mean(z)
sd(z)

a <- data$net_nifa
mean(a)
sd(a)

b <- data$tw
mean(b)
sd(b)

c <- data$inc
mean(c)
sd(c)

d <- data$age
mean(d)
sd(d)

e <- data$fsize
mean(e)
sd(e)

f <- data$marr
mean(f)
sd(f)

g <- data$ira
mean(g)
sd(g)

h <- data$db
mean(h)
sd(h)

i <- data$hown
mean(i)
sd(i)

j <- data$nohs
mean(j)
sd(j)

k <- data$hs
mean(k)
sd(k)

l <- data$smcol
mean(l)
sd(l)

m <- data$col
mean(m)
sd(m)

summary.table <- data.frame(
  Variable = c("401(k) participation", "401(k) eligibility", "Net financial assets", "Net non-401(k) financial assets",
               "Total wealth", "Income", "Age", "Family size", "Married", "IRA participation",
               "Defined benefit pension", "Home ownership", "No high school", "High school", 
               "Some college", "College"),
  Mean = c(mean(y), mean(x), mean(z), mean(a),
           mean(b), mean(c), mean(d), mean(e),
           mean(f), mean(g), mean(h), mean(i),
           mean(j), mean(k), mean(l), mean(m)),
  SD = c(sd(y), sd(x), sd(z), sd(a),
         sd(b), sd(c), sd(d), sd(e),
         sd(f), sd(g), sd(h), sd(i),
         sd(j), sd(k), sd(l), sd(m)))

print(summary.table)


#4
median(z)
median(a)
median(b)

summary.table <- data.frame(
  Variable = c("401(k) participation", "401(k) eligibility", "Net financial assets", "Net non-401(k) financial assets",
               "Total wealth", "Income", "Age", "Family size", "Married", "IRA participation",
               "Defined benefit pension", "Home ownership", "No high school", "High school", 
               "Some college", "College"),
  Mean = c(mean(y), mean(x), mean(z), mean(a),
           mean(b), mean(c), mean(d), mean(e),
           mean(f), mean(g), mean(h), mean(i),
           mean(j), mean(k), mean(l), mean(m)),
  SD = c(sd(y), sd(x), sd(z), sd(a),
         sd(b), sd(c), sd(d), sd(e),
         sd(f), sd(g), sd(h), sd(i),
         sd(j), sd(k), sd(l), sd(m)),
  Median = c(NA, NA, median(z), median(a), median(b), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
print(summary.table)


#5
participants <- data[data$p401 == 1, ]  
nonparticipants <- data[data$p401 == 0, ] 

summary_stats <- function(df) {
  y <- df$p401
  x <- df$e401
  z <- df$net_tfa
  a <- df$net_nifa
  b <- df$tw
  c <- df$inc
  d <- df$age
  e <- df$fsize
  f <- df$marr
  g <- df$ira
  h <- df$db
  i <- df$hown
  j <- df$education.level == "no high school"
  k <- df$education.level == "high school"
  l <- df$education.level == "some college education"
  m <- df$education.level == "college"
  
  summary.table <- data.frame(
    Variable = c("401(k) participation", "401(k) eligibility", "Net financial assets", "Net non-401(k) financial assets",
                 "Total wealth", "Income", "Age", "Family size", "Married", "IRA participation",
                 "Defined benefit pension", "Home ownership", "No high school", "High school", 
                 "Some college", "College"),
    Mean = c(mean(y), mean(x), mean(z), mean(a),
             mean(b), mean(c), mean(d), mean(e),
             mean(f), mean(g), mean(h), mean(i),
             mean(j), mean(k), mean(l), mean(m)),
    SD = c(sd(y), sd(x), sd(z), sd(a),
           sd(b), sd(c), sd(d), sd(e),
           sd(f), sd(g), sd(h), sd(i),
           sd(j), sd(k), sd(l), sd(m)),
    Median = c(NA, NA, median(z), median(a), median(b),
               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  )
  
  return(summary.table)
}


summary.participants <- summary_stats(participants)
print("Summary for 401(k) Participants")
print(summary.participants)

summary.nonparticipants <- summary_stats(nonparticipants)
print(summary.nonparticipants)

#6
eligible <- data[data$p401 == 1, ]  
noneligible <- data[data$p401 == 0, ] 

summary_stats <- function(df) {
  y <- df$p401
  x <- df$e401
  z <- df$net_tfa
  a <- df$net_nifa
  b <- df$tw
  c <- df$inc
  d <- df$age
  e <- df$fsize
  f <- df$marr
  g <- df$ira
  h <- df$db
  i <- df$hown
  j <- df$education.level == "no high school"
  k <- df$education.level == "high school"
  l <- df$education.level == "some college education"
  m <- df$education.level == "college"
  
  summary.table <- data.frame(
    Variable = c("401(k) participation", "401(k) eligibility", "Net financial assets", "Net non-401(k) financial assets",
                 "Total wealth", "Income", "Age", "Family size", "Married", "IRA participation",
                 "Defined benefit pension", "Home ownership", "No high school", "High school", 
                 "Some college", "College"),
    Mean = c(mean(y), mean(x), mean(z), mean(a),
             mean(b), mean(c), mean(d), mean(e),
             mean(f), mean(g), mean(h), mean(i),
             mean(j), mean(k), mean(l), mean(m)),
    SD = c(sd(y), sd(x), sd(z), sd(a),
           sd(b), sd(c), sd(d), sd(e),
           sd(f), sd(g), sd(h), sd(i),
           sd(j), sd(k), sd(l), sd(m)),
    Median = c(NA, NA, median(z), median(a), median(b),
               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  )
  
  return(summary.table)
}


summary_eligible <- summary_stats(eligible)
print(summary_eligible)

summary.noneligible <- summary_stats(noneligible)
print(summary.noneligible)

#7- answered in pdf



#SUMMARY STATISTICS 2


#1
library(dplyr)
data$income <- cut(data$inc, 
              breaks = c(-Inf, 10000, 20000, 30000, 40000, 50000, 75000, Inf), 
              labels = c("< $10k", "$10k – 20k", "$20k – 30k", "$30k – 40k", "$40k – 50k", "$50k – 75k", "≥ $75k"),
              right = FALSE)

table(data$income)


#2
install.packages("ggplot2")
library(ggplot2)
install.packages("crayon")
library("crayon")

wealth <- data %>%
  group_by(income) %>%
  summarise(avg.tw = mean(tw),
            avg.tfa = mean(tfa),
            avg.net_tfa = mean(net_tfa),
            observations = n())

#Bar plot for average total wealth
ggplot(wealth, aes(income, avg.tw))+ geom_bar(stat = "identity")

tw_summary <- data %>%
  group_by(income) %>%
  summarise(
    count = n(),
    avg_tw = mean(tw)
  )

print(tw_summary)

#Bar plot for average total financial assets
ggplot(wealth, aes(income, avg.tfa))+ geom_bar(stat = "identity")

tfa_summary <- data %>%
  group_by(income) %>%
  summarise(
    count = n(),
    avg_tfa = mean(tfa)
  )

print(tfa_summary)

#Bar plot for average net financial assets
ggplot(wealth, aes(income, avg.net_tfa))+ geom_bar(stat = "identity")

net_tfa_summary <- data %>%
  group_by(income) %>%
  summarise(
    count = n(),
    avg_net_tfa = mean(net_tfa)
  )

print(net_tfa_summary)

#Second part of the question answered in the pdf submitted.

#3
participation.by.eligibility <- data %>%
  group_by(e401) %>%
  summarise(participation_rate = mean(p401) * 100)

print(participation.by.eligibility)
#Shows how many eligible individuals participate in 401(k) compared to ineligible individuals


eligibility.by.participation <- data %>%
  group_by(p401) %>%
  summarise(eligibility_rate = mean(e401) * 100)
print(eligibility.by.participation)
#Shows how many participants are eligible for 401(k) compared to non-participants.

#4
#Answered in the pdf submitted

#5
data <- data %>%
  filter(e401 == 1)

ggplot(data) +
  geom_density(aes(x = net_tfa, fill = as.factor(p401))) +
  labs(title = "Density Plot of Net Financial Assets by 401(k) Participation Status",
       x = "Net Financial Assets",
       y = "Density",
       fill = "401(k) Participation") +
  theme_minimal()

#Second part of the question answered in the pdf submitted.
