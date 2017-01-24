#TOP 10 UNIVERSITIES PLOT RCODE

#Reading a csv file
shan <- read.csv("shanghaiData.csv", sep = ",", header = TRUE)
#Subset the Harverd University Row
harvard <- subset(shan, shan$university_name == "Harvard University")
Harvard <- data.frame(harvard)
#Subset the Cambridge University Row
uoc <- subset(shan, shan$university_name == "University of Cambridge")
Uoc <-data.frame(uoc)
#Subset the Stanford University Row
su <- subset(shan, shan$university_name == "Stanford University")
Su <- data.frame(su)
#Subset the California University Row
uocb <- subset(shan, shan$university_name == "University of
               California, Berkeley")
Uocb <- data.frame(uocb)
#Subset the Massachusetts University Row
mit <- subset(shan, shan$university_name == "Massachusetts Institute
              of Technology (MIT)")
Mit <- data.frame(mit)
#Subset the California University Row
cit <- subset(shan, shan$university_name == "California Institute of
              Technology")
Cit <- data.frame(cit)
#Subset the Columbia University Row
colu <- subset(shan, shan$university_name == "Columbia University")
Col <- data.frame(colu)
#Subset the Princeton University Row
prince <- subset(shan, shan$university_name == "Princeton University")
p <- data.frame(prince)
#Subset the Chicago University Row
chic <- subset(shan, shan$university_name == "University of Chicago")
c <- data.frame(chic)
#Subset the Oxford University Row
uo <- subset(shan, shan$university_name == "University of Oxford")
Uo <- data.frame(uo)
#row bind the Universities
all<-do.call("rbind", list(Harvard, Uoc, Su, Uocb, Mit, Cit, colu, p, c, Uo))
all
library(ggplot2) #package ggplot

#PLOTTING
ggplot(all, aes(x = university_name, y = year, colour = as.factor(world_rank))) + 
  geom_point() + geom_line() + ylim(2005,2010) + ggtitle("Year Based Top 10 University Ranks") + labs(x ="University Name", y = "World Rank") 
  
  # Use a larger dot
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1))



#EXPENDITURE PLOTTING RCODE

#Reading  a csv file
data <- read.csv("education_expenditure_supplementary_data.csv")
data #displaying a  data
library(reshape2) #package
#melt function is used to group the columns from the package reshape2
dat_m <- melt(data, id.vars = "direct_expenditure_type", 
              measure.vars = grep("^X", names(data), value = TRUE))
dat_m
library(ggplot2) #package ggplot2
library(scales)  #package scales
summary(dat_m)  #summary of data
#PLOTTING THE DATA USING GGPLOT
ggplot(dat_m, aes(y = value, x = variable, colour = as.factor(direct_expenditure_type))) + geom_boxplot() +   scale_y_continuous(labels = scales::percent_format()) + coord_cartesian(ylim = c(1,8)) + 
  ggtitle("Year to Expenditure_Type Plot") + labs(x = "Years",y = "Expenditure Percentage")


#INTERNATIONAL STUDENTS YEAR WISE

data1 = read.csv("cwurData.csv", sep=',')
names(data1)
dat <- head(data1,n=15)
data6 = read.csv("timesData.csv",sep = ',')
ratio <- separate(data6,col=female_male_ratio, into = c("female",
                                                        "male"), sep = "\\:")

ggplot(ratio, aes(x=country, y=international_students,fill=year)) +
  geom_bar(stat="identity") +
  # Use a larger dot
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey60",
                                          linetype="dotted"))+ggtitle(("International Students Based On Country
                                                                       Year Wise"))

#Regression on Total Score And Rank RCODE

#Reading csv file
data <- read.csv("cwurData.csv")
a <- head(unique(data, n = 2000)) 
a1 <- a$score   #score column
a1                #displaying the column
b1 <- a$world_rank #world_rank column
b1    #displaying the column
relation <- lm(b1 ~ a1) #relation b/w two above columns
c<- data.frame( a1 = 90) #sample
c
result <- predict(relation,c) #predict the value
result #displaying the result
#PLOT
plot(a1, b1, col = "blue", main = "Regression on Total Score and
Rank", abline(lm(b1 ~ a1)), cex = 1.3, pch = 16, ylab = "World Rank",
     xlab = "Total Score")

