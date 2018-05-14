
# Load Packages
library(RCurl)
library(plyr)
library(dplyr)
library(tidyr)
library(exactRankTests)
library(effsize)
library(foreach)
library(doParallel)
library(pwr)
library(paramtest)
library(ggplot2)
library(stargazer)
library(devtools)

#Load Data 
require(RCurl)

s15comments <- read.csv(text=getURL("https://raw.githubusercontent.com/Jsteur/James_Final_Paper_Files/master/Summer_2015_comments_final.csv"))
s15students <- read.csv(text=getURL("https://raw.githubusercontent.com/Jsteur/James_Final_Paper_Files/master/Summer_2015_students_final.csv"))
Table_OverallPaired <- read.csv(text=getURL("https://raw.githubusercontent.com/Jsteur/James_Final_Paper_Files/master/Table_1_Jake.csv"))
Table_OverallPerm <- read.csv(text=getURL("https://raw.githubusercontent.com/Jsteur/James_Final_Paper_Files/master/Table_2_Jake.csv"))
Table_ClassPaired <- read.csv(text=getURL("https://raw.githubusercontent.com/Jsteur/James_Final_Paper_Files/master/Table_3_Jake.csv"))
Table_ClassPerm  <- read.csv(text=getURL("https://raw.githubusercontent.com/Jsteur/James_Final_Paper_Files/master/Table_4_Jake.csv"))

#Cleanup Data
Class1 <- subset(s15students, Class ==1)
Class2 <- subset(s15students, Class ==2)

#Summarize lengths of style, purpose, and level by student. 
temp1<-ddply(s15comments,.(Class,Paper,Student),summarize,TotStyle1=length(Style==1),
             TotStyle2=sum(Style==2),TotStyle3=sum(Style==3),TotStyle4=sum(Style==4),
             TotPurp1=length(Purpose==1),TotPurp2=sum(Purpose==2),
             TotPurp3=sum(Purpose==3),TotPurp4=sum(Purpose==4),TotLevel1=length(Level==1),
             TotLevel2=sum(Level==2), TotLevel3=sum(Level==3))

##Rewriting comment variables to be factors.

s15comments2 <- s15comments
s15comments2$Class <- factor(s15comments2$Class, labels = c("Professor 1", "Professor 2"))
s15comments2$Style <- factor(s15comments2$Style, labels = c("Directive", "Nondirective", "Corrective", "Evaluative"))
s15comments2$Purpose <- factor(s15comments2$Purpose, labels = c("Problem Detecting",
                                                                "Advising", "Editing", "Praising", "Describing", "Topical Commenting"))

s15comments2$Level <- factor(s15comments2$Level, labels = c("Micro",
                                                            "Mid", "Macro"))

s15comments2$Article <- factor(s15comments2$Article, labels = c("Thesis",
                                                                "Organization", "Evidence", "Thoroughness of Discussion", "Logic and Reasoning", "Syntax", "Transitions", "Topic Sentences", "Sentence Conciseness", "Grammar", "Punctuation", "Spelling", "Word Choice", "Capitalization", "Citation/Works Cited Page"))

#Figure 1 Bar Graph with Style & Purpose. 
#Compares professor 1 and professor 2.

s15comments2 %>% filter(Style!="NA" & Purpose != "NA") %>%
  ggplot(aes(Style, fill=Purpose))+
  geom_bar(position="dodge")+
  facet_wrap(~Class, ncol=2)+
  ggtitle("Figure 1: Overall Professor Comments")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust=1), 
        plot.title = element_text(hjust=0.5))+
  scale_x_discrete(drop=FALSE)

#Means, Range, & 50% coverage intervals For Overall First Paper & Second Paper 
mean(Class1$Overall1)
mean(Class1$Overall2)
range(Class1$Overall1)
range(Class1$Overall2)
quantile(Class1$Overall1, c(0.25, 0.75))
quantile(Class1$Overall2, c(0.25, 0.75))

#Means, Range, & 50% coverage intervals For Overall Third Paper & Fourth Paper 
mean(Class2$Overall1)
mean(Class2$Overall2)
range(Class2$Overall1)
range(Class2$Overall2)
quantile(Class2$Overall1, c(0.25, 0.75))
quantile(Class2$Overall2, c(0.25, 0.75))

#Means, Range, & 50% coverage intervals For Macro First Paper & Second Paper
mean(Class1$Macro1)
mean(Class1$Macro2)
range(Class1$Macro1)
range(Class1$Macro2)
quantile(Class1$Macro1, c(0.25, 0.75))
quantile(Class1$Macro2, c(0.25, 0.75))

#Means, Range, & 50% coverage intervals For Macro Third Paper & Fourth Paper
mean(Class2$Macro1)
mean(Class2$Macro2)
range(Class2$Macro1)
range(Class2$Macro2)
quantile(Class2$Macro1, c(0.25, 0.75))
quantile(Class2$Macro2, c(0.25, 0.75))

##Make Dataframe of Overall with Means, Range, & 50% Coverage Intervals
overall_means <- c(mean(Class1$Overall1), mean(Class1$Overall2), 
                   mean(Class2$Overall1), mean(Class2$Overall2))
overall_ranges <- c("1 to 3", "2 to 4", "1 to 3", "1 to 3")
overall_coverage <- c("2 to 3", "2 to 3", "2 to 2", "2 to3")
Overall_Table <- data.frame(overall_means, overall_ranges, overall_coverage)

row.names(Overall_Table) <- c("Professor 1 First Paper", "Professor 1 Last Paper", 
                              "Professor 2 First Paper", "Professor 2 Last Paper")
colnames(Overall_Table) <-c("Means", "Ranges", "50% Coverage Intervals")

##Make Dataframe of Macro with Means, Range, & 50% Coverage Intervals
macro_means <- c(mean(Class1$Macro1), mean(Class1$Macro2), 
                 mean(Class2$Macro1), mean(Class2$Macro2))
macro_ranges <- c("1 to 4", "2 to 4", "1 to 3", "1 to 3")
macro_coverage <- c("2 to 3", "2 to 3", "2 to 2", "2 to 3")

Macro_Table <- data.frame(macro_means, macro_ranges, macro_coverage)
row.names(Macro_Table) <- c("Professor 1 First Paper", "Professor 1 Last Paper", 
                            "Professor 2 First Paper", "Professor 2 Last Paper")
colnames(Macro_Table) <-c("Means", "Ranges", "50% Coverage Intervals")

##Table 1
##Summarize Variables of Interest for overall variable. Includes means, ranges, and 50% coverage intervals.

stargazer(summary=FALSE, header=FALSE, type = "latex", title = "Variable Overall's Means, Ranges, and Coverage Intervals", notes = "First Paper n=16. Second Paper n=10.", Overall_Table)
```
```{r, echo=FALSE, results="asis"}
##Table 2
##Summarize Variables of Interest for macro variable. Includes means, ranges, and 50% coverage intervals.
stargazer(summary=FALSE, header=FALSE, type = "latex", title = "Variable Macro's Means, Ranges, and Coverage Intervals", notes = "First Paper n=16. Second Paper n=10.", Macro_Table)

##Paired Sample T-Test for all outcome variables of interest. 
t.test(s15students$Overall2, s15students$Overall1, paired = TRUE, alternative = "two.sided")
t.test(s15students$Macro2, s15students$Macro1, paired = TRUE, alternative = "two.sided")
t.test(s15students$Mid2, s15students$Mid1, paired = TRUE, alternative = "two.sided")
t.test(s15students$Micro2, s15students$Micro1, paired = TRUE, alternative = "two.sided")

##Table 3: Two-Tailed Paired Sample T-Test with 95% CI's for outcomes overall, macro, mid, and micro.
colnames(Table_OverallPaired) <- c("Level", "95% CI", "t-statistic", "p-value", "Mean Difference") 
stargazer(summary=FALSE, header=FALSE, type = "latex", title = "Pooled Classes Paired Sample               T-Test",notes = "DF=25. Two-tailed test.", rownames = F, Table_OverallPaired)

#Columns of Overall to Explain Permutation
Perm_1 <- data.frame(s15students$Student, s15students$Overall1, s15students$Overall2)
Perm_1

##Table 4: Permutation T-Test with 95% CI's for outcome and macro. Includes Cohen's d, error rate, and power. 
##Seperate by class. 
colnames(Perm_1) <- c("Student", "Overall Grade for First Papers", "Overall Grade for Second Papers") 
stargazer(summary=FALSE, header=FALSE, type = "latex", title = "Students' Paper Grades for Outcome Overall ", notes = "N=26", rownames = F, Perm_1)

#Paired Permutation Test with Code. Outcome is overall for both classes. 
set.seed(1234)
nullR <- numeric(length = 26)
nullR[1] <- cor(s15students$Overall1, s15students$Overall2) ## obsered R in [1]
N <- length(s15students$Overall1)
for(i in seq_len(25) + 1) {
  nullR[i] <- cor(s15students$Overall1[sample(N)], s15students$Overall2)
}
## two sided
sum(nullR >= abs(nullR[1])) / length(nullR)

#Paired Permutation Test with Package. Outcomes are overall and macro for both classes.
perm.test(s15students$Overall1, s15students$Overall2, paired = T, alternative = "two.sided")
perm.test(s15students$Macro1, s15students$Macro2, paired = T, alternative = "two.sided")

#Cohen's d for effect sizes. Outcomes are overall and macro for pooled students. 
#Ran Package
res = cliff.delta(s15students$Overall1,s15students$Overall2,return.dm=TRUE)
print(res)
print(res$dm)

cohen.d(s15students$Overall2,s15students$Overall1,pooled=TRUE,paired=TRUE,
        na.rm=FALSE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

cohen.d(s15students$Macro2,s15students$Macro1,pooled=TRUE,paired=TRUE,
        na.rm=FALSE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

##Functions to Calculate False Positive Rates

#Create function to re-run experiment
newexperiment<-function(z,b){
  ## A function to randomly assign treatment within pair
  ## z = treatment assignment that is completely randomly 
  ##assigned within block
  ## b = a block indicator
  unsplit(lapply(split(z,b),sample),b)
}

## doParallel will not work on all machines. 
library(foreach)
library(doParallel)
cl <- makeCluster(4) ## 4 cores on my machine
registerDoParallel(cl)

library(robustbase)
errratefn <- function(simulations,trt,outcome,block) {
  require(robustbase)
  outpaired <-  outcome - ave(outcome,block)
  output<-foreach(1:simulations,
                  .packages="robustbase",
                  .export=c("newexperiment"),
                  .combine='c') %dopar% {
                    ## First make a new experiment with no 
                    ##relationship between z and y
                    ## since we know that there is no effect, 
                    ## then we can assess the false positive rate of
                    ## lmrob
                    newz <- newexperiment(z=trt,b=block)
                    newzpaired <- newz - ave(newz,block)
                    sim_p <- summary(lmrob(outpaired~newzpaired))$coefficients[2,4]
                    return(sim_p)
                  }
  return(output)
}

#False Postive Rate For Overall & Macro: Blocked For All Students
##Make block that includes all students.
block <- c(rep.int(1, 26))

set.seed(12345)
results2a <- (errratefn(100,trt=s15students$Student,
                        outcome=s15students$Overall2,block=block))
Overall_Error_Rate_Unblocked <- mean(results2a, na.rm = T)

results2b <- (errratefn(100,trt=s15students$Student,
                        outcome=s15students$Macro2,block=block))
Macro_Error_Rate_Unblocked <- mean(results2a, na.rm = T)

Overall_Error_Rate_Unblocked
Macro_Error_Rate_Unblocked

#Function to calculate power. 
#simNum=Number of simultations
#N=Sample Size
#d=Effect Size (Cohen's d)
set.seed(12345)
t_func <- function(simNum, N, d) {
  x1 <- rnorm(N, 0, 1)
  x2 <- rnorm(N, d, 1)
  
  # run t-test on generated data
  t <- t.test(x1, x2, var.equal=TRUE)  
  stat <- t$statistic
  p <- t$p.value
  
  return(c(t=stat, p=p, sig=(p < .05)))
  # return a named vector with the results we want to keep
}

##Package paramtest
power_ttest <- run_test(t_func, n.iter=1000, output='data.frame', N=26, d=0.7188)  # simulate data
results(power_ttest) %>%
  summarise(power=mean(sig))

##Power for overall pooled comparison (n=26).
plot.power.htest(pwr.t.test(n=26, d=0.7188, sig.level = 0.05, power=NULL))

##Power for macro pooled comparison (n=26).
plot.power.htest(pwr.t.test(n=26, d=.6068, sig.level = 0.05, power=NULL))

##Table 5: Permutation T-Test with 95% CI's for outcome and macro. Includes Cohen's d, error rate, and power. 
##Includes all students. 
colnames(Table_OverallPerm) <- c("Level", "95% CI", "Permutation p-value", "Cohen's d", "Error Rate", "Power")
stargazer(summary=FALSE, header=FALSE, type = "latex", title = "Pooled Classes Permutation Test, Effect Size, and Power",notes = "DF=25. Two-tailed test.", rownames = F, Table_OverallPerm)

#Class Specific T-Tests
t.test(Class1$Overall2, Class1$Overall1, paired = TRUE, alternative = "two.sided")
t.test(Class2$Overall2, Class2$Overall1, paired = TRUE, alternative = "two.sided")
t.test(Class1$Macro2, Class1$Macro1, paired = TRUE, alternative = "two.sided")
t.test(Class2$Macro2, Class2$Macro1, paired = TRUE, alternative = "two.sided")

#Table 6. Two-Tailed Paired Sample T-Test with 95% CI's for outcomes overall and macro. 
colnames(Table_ClassPaired) <- c("Class and Level", "95% CI", "t-statistic", "p-value", "Mean Difference") 
stargazer(summary=FALSE, header=FALSE, type = "latex", title = "Separate Classes Paired Sample T-Test",notes = "Class 1 DF=15. Class 2 DF=9. Two-tailed test for both classes.", rownames = F, Table_ClassPaired)

##Permutation Test Between Classes
perm.test(Class1$Overall1,  Class1$Overall2, paired = T, alternative = "two.sided")
perm.test(Class2$Overall1,  Class2$Overall2, paired = T, alternative = "two.sided")
perm.test(Class1$Macro1,  Class1$Macro2, paired = T, alternative = "two.sided")
perm.test(Class2$Macro1,  Class2$Macro2, paired = T, alternative = "two.sided")

#Hedges' g for effect sizes. Outcomes are overall and macro for seperate classes. 
#Ran Package

res = cliff.delta(Class1$Overall1,Class1$Overall2,return.dm=TRUE)
print(res)
print(res$dm)

cohen.d(Class1$Overall2,Class1$Overall1,pooled=TRUE,paired=TRUE,
        na.rm=FALSE, hedges.correction=TRUE,
        conf.level=0.95,noncentral=FALSE)

res = cliff.delta(Class2$Overall1,Class2$Overall2,return.dm=TRUE)
print(res)
print(res$dm)

cohen.d(Class2$Overall2,Class2$Overall1,pooled=TRUE,paired=TRUE,
        na.rm=FALSE, hedges.correction=TRUE,
        conf.level=0.95,noncentral=FALSE)

cohen.d(Class1$Macro2,Class1$Macro1,pooled=TRUE,paired=TRUE,
        na.rm=FALSE, hedges.correction=TRUE,
        conf.level=0.95,noncentral=FALSE)
cohen.d(Class2$Macro2,Class2$Macro1,pooled=TRUE,paired=TRUE,
        na.rm=FALSE, hedges.correction=TRUE,
        conf.level=0.95,noncentral=FALSE)

#Power rates for classes. Outcome variables are overall and macro.
pwr.t.test(n=16, d=0.6674, sig.level = 0.05, power=NULL)
pwr.t.test(n=10, d=0.6772, sig.level = 0.05, power=NULL)
pwr.t.test(n=16, d=0.524, sig.level = 0.05, power=NULL)
pwr.t.test(n=10, d=0.6772, sig.level = 0.05, power=NULL)

#False Postive Rate For Overall & Macro: Blocked by Class
set.seed(12345)
results2c <- (errratefn(100,trt=s15students$Student,
                        outcome=s15students$Overall2,block=s15students$Class))
Overall_Error_Rate_Blocked <- mean(results2a, na.rm = T)
Overall_Error_Rate_Blocked

results2d <- (errratefn(100,trt=s15students$Student,
                        outcome=s15students$Macro2,block=s15students$Class))
Macro_Error_Rate_Blocked <- mean(results2a, na.rm = T)
Macro_Error_Rate_Blocked

##Table 7: Permutation T-Test with 95% CI's for outcome and macro. Includes Cohen's d, error rate, and power. 
##Seperate by class. 
colnames(Table_ClassPerm) <- c("Class and Level", "95% CI", "Permutation p-value", "Hedges' g", "Error Rate", "Power") 
stargazer(summary=FALSE, header=FALSE, type = "latex", title = "Separate Classes Permutation Test, Effect Size, and Power", notes = "Class 1 DF=15. Class 2 DF=9. Two-tailed test for both classes.", rownames = F, Table_ClassPerm)

##Table 8. Contingency table of purpose comments by class.  

Purpose_Comments <- data.frame(table(s15comments$Class, s15comments2$Purpose))
colnames(Purpose_Comments) <- c("Professor", "Purpose", "Frequency")
stargazer(summary=FALSE, header=FALSE, type = "latex", 
          title = "Frequency of Purpose Comments",
          rownames = F,
          notes = "Class 1 N=201. Class 2 N=178.", Purpose_Comments)

##Table 9. Contingency table of style comments by class.  
Style_Comments <- data.frame(table(s15comments$Class, s15comments2$Style))
colnames(Style_Comments) <- c("Professor", "Style", "Frequency")
stargazer(summary=FALSE, header=FALSE, type = "latex", 
          title = "Frequency of Purpose Comments", rownames = F,
          notes = "Class 1 N=201. Class 2 N=178.", Style_Comments)

#Recode Level Comments for Table 10. 
Level_Comments <- data.frame(table(s15comments$Class, s15comments2$Level))
colnames(Level_Comments) <- c("Professor", "Level", "Frequency")

#Recode Article Comments for Table 11.
Article_Comments <- data.frame(table(s15comments$Class, s15comments2$Article))
colnames(Article_Comments) <- c("Professor", "Article", "Frequency")

##Table 10. Contingency table of level comments by class. 
stargazer(summary=FALSE, header=FALSE, type = "latex", title = "Frequency of Level Comments",
          notes = "Class 1 N=201. Class 2 N=178.", rownames = F, Level_Comments)

##Table 11. Contingency table of article comments by class. 
stargazer(summary=FALSE, header=FALSE, type = "latex", title = "Frequency of Article Comments", 
          notes = "Class 1 N=201. Class 2 N=178.", rownames = F, Article_Comments)