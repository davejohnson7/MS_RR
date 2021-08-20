setwd("~/Dropbox/York/Research/Cog.Emo.Lab/PROJECTS/Memory Strength_RR/Study2_DATA")
final59 <- read.csv(file = "MS_data_02_17_20_FINAL59.csv", header = TRUE)

#ADDING CHANGE
#NOT SURE WHERE THIS IS BEING UPLOADED TO

#Change test

#read file from github - not currently working correctly
#library(RCurl)
#x <- getURL("https://raw.githubusercontent.com/nosenhodge/memory_strength_rr/main/MS_data_02_17_20_FINAL59.csv?token=AFMW5N7VQNE6LYYPG73D3ALBDZVOG")
#final59 <- read.csv(text = x, header = TRUE)

#summarize age
summary(final59$Age)
sd(final59$Age, na.rm = TRUE)

sex59 <- table(final59$Sex)
sex59

race59 <- table(final59$Race)
race59

#run one-sample t tests for late acq; CSP1
t.test(final59$D1_CSP1_L3_diff, mu = 0)

#run one-sample t tests for late acq; CSP2
t.test(final59$D1_CSP2_L3_diff, mu = 0)

#boxplot(final59$D1_CSP2_L3_diff)

#contingency awareness frequency
contingency59 <- table(final59$contingency_awareness)
contingency59

#data summary for late acquisition, CSP1
summary(final59$D1_CSP1_L3_diff)
sd(final59$D1_CSP1_L3_diff, na.rm = TRUE)

#data summary for late acquisition, CSP2
summary(final59$D1_CSP2_L3_diff)
sd(final59$D1_CSP2_L3_diff, na.rm = TRUE)

#paired sample t-test for late acquisiton
t.test(final59$D1_CSP1_L3_diff,final59$D1_CSP2_L3_diff, mu=0, alt="two.sided",
       paired = TRUE, conf.level = .95)

#data summary for early extinction, CSP1
summary(final59$D2_CSP1_first4_diff)
sd(final59$D2_CSP1_first4_diff, na.rm = TRUE)

#data summary for early extinction, CSP2
summary(final59$D2_CSP2_first4_diff)
sd(final59$D2_CSP2_first4_diff, na.rm = TRUE)

#make plot for CSP partial, acquisition by extinction
CSP1_ACQ <- final59$D1_CSP1_L3_diff
CSP1_EXT <- final59$D2_CSP1_all_diff
plot(CSP1_ACQ,CSP1_EXT,
     xlab="CSP1_acquisition", ylab="CSP1_extinction")
abline(lm(CSP1_EXT~CSP1_ACQ)) # regression line (y~x)

#make plot for CSP partial, acquisition by reinstatement
CSP1_ACQ <- final59$D1_CSP1_L3_diff
CSP1_RE <- final59$D3_RE_CSP1_F2_diff
plot(CSP1_ACQ,CSP1_RE,
     xlab="CSP1_acquisition", ylab="CSP1_reinstatement")
abline(lm(CSP1_RE~CSP1_ACQ)) # regression line (y~x)

#make plot for CSP full, acquisition by extinction
CSP2_ACQ <- final59$D1_CSP2_L3_diff
CSP2_EXT <- final59$D2_CSP2_all_diff
plot(CSP2_ACQ,CSP2_EXT,
     xlab="CSP2_acquisition", ylab="CSP2_extinction")
abline(lm(CSP2_EXT~CSP2_ACQ)) # regression line (y~x)


#make plot for CSP full, acquisition by reinstatement
CSP2_ACQ <- final59$D1_CSP2_L3_diff
CSP2_RE <- final59$D3_RE_CSP2_F2_diff
plot(CSP2_ACQ,CSP2_RE,
     xlab="CSP2_acquisition", ylab="CSP2_reinstatment")
abline(lm(CSP2_EXT~CSP2_ACQ)) # regression line (y~x)

library(tidyverse)
install.packages(c("nycflights13", "gapminder", "Lahman"))

#How to make a simple plot using ggplot
#ggplot(data = <DATA>) + geom_point(mapping = aes(MAPPINGS))
con_aware <- final59$contingency_awareness
site <- final59$Site
ggplot(data = final59) + geom_point(mapping = aes(x = CSP1_ACQ, y = CSP1_RE, 
                                                  color = con_aware))
optimism <- final59$Optimism
pessimism <- final59$Pessimism
trait_anxiety <- final59$TraitANX

#run correlation of optimism and pessimism
cor.test(optimism, pessimism, method=c("pearson", "kendall", "spearman"))

#compute variables (means and whatnot)
D1_CSP1_L3 <- ((final59$D1csp1_10 + final59$D1csp1_11 + final59$D1csp1_12)/3)
D1_CSM1_L3 <- ((final59$D1csm1_10 + final59$D1csm1_11 + final59$D1csm1_12)/3)
D1_CSP1_L3_diff <- D1_CSP1_L3 - D1_CSM1_L3
D2_CSP2_F4 <- ((final59$D2csp1_1 + final59$D2csp1_2 + final59$D2csp1_3 + final59$D2csp1_4 )/4)
D2_CSP1_F4 <- ((final59$D2csp1_1 + final59$D2csp1_2 + final59$D2csp1_3 + final59$D2csp1_4 )/4)
D2_CSP1_F4 <- ((final59$D2csp1_1 + final59$D2csp1_2 + final59$D2csp1_3 + final59$D2csp1_4 )/4)
D2_CSM_F4 <- ((final59$D2csm1 + final59$D2csm2 + final59$D2csm3 + final59$D2csm4 )/4)
D2_CSP1_F4_diff <- D2_CSP1_F4 - D2_CSM_F4
ggplot(final59, aes(x = D1_CSP1_L3_diff, y = D2_CSP1_F4_diff)) + 
  geom_point() +
  facet_wrap(~) +
  xlab("Acquisition") +
  ylab("Extinction") +l
  geom_smooth(color = "blue", method = "lm")

#make a bar chart for 
