library(tidyverse)
library(dplyr)
data <- data.frame("Gender"=c("Female","Female","Male","Male"),
                   "Meth"=c("Yes","No","Yes","No"),
                   "Scholarship"=c(1,2,15,9),
                   "NoScholarship"=c(19,30,30,13))
everyone <- data %>%
  group_by(Meth) %>%
  summarize(Scholarship=sum(Scholarship),
            NoScholarship = sum(NoScholarship)) %>%
  mutate(ShareScholarship = Scholarship/(Scholarship + NoScholarship)) %>%
  select(Meth, ShareScholarship) %>%
  spread(Meth, ShareScholarship) %>%
  mutate(NATE= Yes -No)

everyone$NATE


#Based on sex

byGender <- data %>%
  mutate(ShareScholarship = Scholarship/(Scholarship + NoScholarship)) %>%
  select(Gender, Meth, ShareScholarship) %>%
  spread(Meth, ShareScholarship)
  mutate(NATE= Yes - No) %>%
  select(Gender, NATE)  

  byGender <- data %>% 
    mutate(shareScholarship = Scholarship/(Scholarship + NoScholarship)) %>% # getting proportions
    select(Gender, Meth, shareScholarship) %>% # selecting columns containing gender, whether meth was used, and proportions
    spread(Meth, shareScholarship) %>% #transposing data
    mutate(NATE = Yes - No) %>% #creating new column containing NATE for each gender
    select(Gender, NATE) #sel
  
  
natemale <- byGender %>% 
  filter(Gender== "Male") %>%
  pull(NATE)

genderTotals <- data %>%
  group_by(Gender) %>%
  summarize(total=sum(Scholarship+sum(NoScholarship))%>%
              mutate( prop = total / sum(total)))



#Plotting 

library(ggplot2)

scatterplot <- ggplot (data = mtcars,aes(x= wt, 
                     y= mpg, 
                     col = cyl)) + geom_point()

scatterplot

set.seed(123)
 exp <- rnorm(1000, 15, 4)
educ <- rnorm(1000, 12, 2)
salary <- 20000 + 100 * (educ*exp)

dat<- data.frame(exp,educ, salary)
head(dat)

salaryPlot <- ggplot(dat, aes(x = exp, y = educ)) +
  geom_point() + 
  xlab("Experience") + 
  ylab("Education") +
  geom_smooth(method = "lm", se = FALSE)

salaryPlot


EXample#2

beauty <- rnorm(1000, 5, 1.5)
talent <- rnorm(1000, 3, 1)

celebrity <-  ifelse(beauty + talent > 8, "celeb" ,"not_celeb") 


dat <- data.frame(beauty, talent, celebrity, 
                  stringsAsFactors = FALSE)

celebPlot <- ggplot(dat, 
                    aes(x = beauty, y = talent)) +
  geom_point() +
  xlab("Beauty") + 
  ylab("Talent") +
  geom_smooth(method = "lm", se = FALSE)

celebPlot

celebData <- dat %>% 
  filter(celebrity == "celeb")

celebPlot2 <- ggplot(celebData, 
                     aes(x = beauty, y = talent)) +
  geom_point() +
  xlab("Beauty") + 
  ylab("Talent") +
  geom_smooth(method = "lm", se = FALSE)

celebPlot2

Exercise 3 

gender <- rep(c("male", "female"), each = 500) 
shoeSize <- rnorm(1000, 38, 2) + 
  4 * as.numeric(gender == "male") 

income = rnorm(1000, 25, 2) + 
  5 * as.numeric(gender == "male") 

dat3 <- data.frame(gender, shoeSize, income, 
                   stringsAsFactors = FALSE)

income = rnorm(1000, 25, 2) + 
  5 * as.numeric(gender == "male") 

dat3 <- data.frame(gender, shoeSize, income, 
                   stringsAsFactors = FALSE)

shoePlot <- ggplot(dat3, 
                   aes(x = shoeSize, y = income)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  xlab("Shoe Size") + 
  ylab("Income")

shoePlot2 <- ggplot(dat3, 
                    aes(x=shoeSize, 
                        y=income, 
                        col = gender)) + 
  geom_point() + 
  geom_smooth(method = "lm")
shoePlot2

install.packages("ggdag")
library(ggdag)

dagify(y ~ x) %>%
  ggdag()

dagify(y ~~ x) %>%
  ggdag()

dagify(y ~~ x) %>%
  ggdag_canonical()

dagify(y ~ x, 
       d~x, 
       y~d) %>%
  ggdag_canonical()

smoking_ca_dag <- dagify(cardiacarrest ~ cholesterol,
                         cholesterol ~ smoking + weight,
                         smoking ~ unhealthy,
                         weight ~ unhealthy,
                         labels = c("cardiacarrest" = "Cardiac\n Arrest", 
                                    "smoking" = "Smoking",
                                    "cholesterol" = "Cholesterol",
                                    "unhealthy" = "Unhealthy\n Lifestyle",
                                    "weight" = "Weight"),
                         latent = "unhealthy",
                         exposure = "smoking",
                         outcome = "cardiacarrest")

smoking_ca_dag

ggdag(smoking_ca_dag, # the dag object we created
      text = FALSE, # this means the original names won't be shown
      use_labels = "label") #
