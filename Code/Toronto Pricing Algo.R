library(dplyr)
library(ggplot2)
library(caret)


Economics <- read.csv("Economics.csv")
Economics2011 <- read.csv("Economics 2011.csv")
Civics <- read.csv("Civics and Equity 2011.csv")
Income <- read.csv("Children-Seniors-Income.csv")
Healthy_Food <- read.csv("Healthy Food Index.csv")
Housing <- read.csv("Housing.csv")
Labor_Force <- read.csv("Labor Force & Education Level.csv")
Languages <- read.csv("Languages.csv")
Linguistic <- read.csv("Linguistic Diversity Index.csv")
Mobility <- read.csv("Mobility.csv")
Population <- read.csv("Population.csv")
Recent_Immigrants <- read.csv("Recent Immigrants.csv")
Recreation <- read.csv("Recreation.csv")
Safety <- read.csv("Safety.csv")
Tenant_Info <- read.csv("Tenant Information.csv")
Total_Pop <- read.csv("Total Population.csv")
Traffic2011 <- read.csv("Traffic 2011.csv")
Visible_Minorities <- read.csv("Visible Minorities.csv")

#looking through home prices
Upper_Tier <- Economics2011 %>% filter(Home.Prices > 600000)

#clean up how you come to this
All_Indicators <- merge(Economics, Economics2011)
All_Indicators <- merge(All_Indicators, Civics) 
All_Indicators <- merge(All_Indicators, Income)
All_Indicators <- merge(All_Indicators, Healthy_Food)
All_Indicators <- merge(All_Indicators, Housing)
All_Indicators <- merge(All_Indicators, Labor_Force)
All_Indicators <- merge(All_Indicators, Languages)
All_Indicators <- merge(All_Indicators, Linguistic)
All_Indicators <- merge(All_Indicators, Mobility)
All_Indicators <- merge(All_Indicators, Population)
All_Indicators <- merge(All_Indicators, Recent_Immigrants)
All_Indicators <- merge(All_Indicators, Recreation)
All_Indicators <- merge(All_Indicators, Safety)
All_Indicators <- merge(All_Indicators, Tenant_Info)
All_Indicators <- merge(All_Indicators, Total_Pop)
All_Indicators <- merge(All_Indicators, Traffic2011)
All_Indicators <- merge(All_Indicators, Visible_Minorities)

#2006 data
Economics_2006 <- read.csv("Economics 2006.csv")
Civics_2006 <- read.csv("Civics and Equity 2006.csv")
Education_2006 <- read.csv("Education 2006.csv")
Environment_2006 <- read.csv("Environment 2006.csv")
Health_2006 <- read.csv("Health 2006.csv")
Housing_2006 <- read.csv("Housing 2006.csv")
Languages_2006 <- read.csv("Languages 2006.csv")
Mobility_2006 <- read.csv("Mobility 2006.csv")
Population_2006 <- read.csv("Population 2006.csv")
Recent_Immigrants_2006 <- read.csv("Recent Immigrants 2006.csv")
Recreation_2006 <- read.csv("Recreation 2006.csv")
Tenant_Info_2006 <- read.csv("Tenant Information 2006.csv")
Total_Population_2006 <- read.csv("Total Population 2006.csv")
Transportation_2006 <- read.csv("Transportation 2006.csv")
Visible_Minorities_2006 <- read.csv("Visible Minorities 2006.csv")

#putting together 06 data
All_Indicators_2006 <- merge(Economics_2006, Civics_2006)
All_Indicators_2006 <- merge(All_Indicators_2006, Education_2006)
All_Indicators_2006 <- merge(All_Indicators_2006, Environment_2006)
All_Indicators_2006 <- merge(All_Indicators_2006, Health_2006)
All_Indicators_2006 <- merge(All_Indicators_2006, Housing_2006)
All_Indicators_2006 <- merge(All_Indicators_2006, Languages_2006)
All_Indicators_2006 <- merge(All_Indicators_2006, Mobility_2006)
All_Indicators_2006 <- merge(All_Indicators_2006, Population_2006)
All_Indicators_2006 <- merge(All_Indicators_2006, Recent_Immigrants_2006)
All_Indicators_2006 <- merge(All_Indicators_2006, Recreation_2006)
All_Indicators_2006 <- merge(All_Indicators_2006, Tenant_Info_2006)
All_Indicators_2006 <- merge(All_Indicators_2006, Total_Population_2006)
All_Indicators_2006 <- merge(All_Indicators_2006, Transportation_2006)
All_Indicators_2006 <- merge(All_Indicators_2006, Visible_Minorities_2006)

summary(All_Indicators_2006)


#Normalizing certain features

#TTC stops per area
All_Indicators <- All_Indicators %>% mutate(TTC.Per.Area = TTC.Stops/Total.Area, TTC.Per.Pop = TTC.Stops/Total.Population)
#Rent versus Buyer
All_Indicators <- All_Indicators %>% mutate(Owned.Ratio = Owned.Dwellings/Rented.Dwellings)
#price to rent ratio
All_Indicators <- All_Indicators %>% mutate(Price_to_Rent_Ratio = Home.Prices/(Tenant.Average.Rent * 12))
#child care spaces per child
All_Indicators <- All_Indicators %>% mutate(Child_Per_Child_Care = Child.0.14/Child.Care.Spaces)
#male to female ratio
All_Indicators <- All_Indicators %>% mutate(Male_to_Female = Pop...Males/Pop...Females)

#education levels normalization
All_Indicators <- All_Indicators %>% mutate(Less_Grade_Nine = Less.than.grade.9/Total.Population, College_Cert = With.College.Certificate.Diploma/Total.Population,
                                            Bachelors = With.Bachelor.Degree.or.Higher/Total.Population)

#crime normalization
All_Indicators <- All_Indicators %>% mutate(Assaults_Per = Assaults/Total.Population, Sexual_Assaults_Per = Sexual.Assaults/Total.Population,
                                            Break_Enters_Per = Break...Enters/Total.Population, Robberies_Per = Robberies/Total.Population,
                                            Vehicle_Thefts_Per = Vehicle.Thefts/Total.Population, Thefts_Per = Thefts/Total.Population,
                                            Murders_Per = Murders/Total.Population, Arsons_Per = Arsons/Total.Population)
#language normalization
All_Indicators <- All_Indicators %>% mutate(French_Per = X...Language...French/Total.Population, Chinese_Per = X...Language...Chinese/Total.Population,
                                            Tamil_Per = X...Language...Tamil/Total.Population, Italian_Per = X...Language...Italian/Total.Population,
                                            Spanish_Per = X...Language...Spanish/Total.Population, Portugese_Per = X...Language...Portuguese/Total.Population,
                                            Tagalog_Per = X...Language...Tagalog/Total.Population, Urdu_Per = X...Language...Urdu/Total.Population,
                                            Russian_Per = X...Language...Russian/Total.Population, Persian_Per = X...Language...Persian..Farsi./Total.Population,
                                            Korean_Per = X...Language...Korean/Total.Population)
#ethnicity normalization
All_Indicators <- All_Indicators %>% mutate(Visible_Minority_Per = Visible.Minority.Category/Total.Population, Chinese_Per = X...Chinese/Total.Population,
                                            South_Asian_Per = X...South.Asian/Total.Population, Black_Per = X...Black/Total.Population,
                                            Filipino_Per = X...Filipino/Total.Population, Latin_Per = X...Latin.American/Total.Population,
                                            Southeast_Asian_Per = X...Southeast.Asian/Total.Population, Arab_Per = X...Arab/Total.Population,
                                            West_Asian_Per = X...West.Asian/Total.Population, Japanese_Per = X...Japanese/Total.Population,
                                            Korean_Per = X...Korean/Total.Population, Other_Minority_Per = X...Other.Visible.Minority/Total.Population,
                                            Mutil_Visible_Per = X...Multiple.Visible.Minority/Total.Population, Not_Visible_Minority_Per = X...Not.a.Visible.Minority/Total.Population)
#population normalization
All_Indicators <- All_Indicators %>% mutate(Zero_Four_Per = Pop.0...4.years/Total.Population, Five_Nine_Per = Pop.5...9.years/Total.Population,
                                            Ten_Fourteen_Per = Pop.10...14.years/Total.Population, Fifteen_Nineteen_Per = Pop.15..19.years/Total.Population,
                                            Twenty_Twentyfour_Per = Pop.20...24.years/Total.Population, Twentyfive_Twentynine_Per = Pop..25...29.years/Total.Population,
                                            Thirty_Thirtyfour_Per = Pop.30...34.years/Total.Population, Thirtyfive_Thirtynine_Per = Pop.35...39.years/Total.Population,
                                            Forty_Fourtyfour_Per = Pop.40...44.years/Total.Population, Fortyfive_Fortynine_Per = Pop.45...49.years/Total.Population,
                                            Fifty_Fiftyfour_Per = Pop.50...54.years/Total.Population, Fiftyfive_Fiftynine_Per = Pop.55...59.years/Total.Population,
                                            Sixty_Sixtyfour_Per = Pop.60...64.years/Total.Population, Sixtyfive_Sixtynine_Per = Pop.65...69.years/Total.Population,
                                            Seventy_Seventyfour_Per = Pop.70...74.years/Total.Population, Seventyfive_Seventynine_Per = Pop.75...79.years/Total.Population,
                                            Eighty_Eightyfour_Per = Pop.80...84.years/Total.Population, Eightyfive_Over = Pop.85.years.and.over/Total.Population)
#Recent immigration normalization
All_Indicators <- All_Indicators %>% mutate(South_Asia_Per = X...Southern.Asia/Total.Population, South_East_Asia = X...South.East.Asia/Total.Population,
                                            East_Asia_Per = X...Eastern.Asia/Total.Population, Middle_East_Per = X...West.Asia.Middle.East/Total.Population,
                                            Africa_Per = X...Africa/Total.Population, Europe_Per = X...Europe/Total.Population, Americas_Per = X...Caribbean.Central.S..America/Total.Population)

#Unemployed normalization
All_Indicators <- All_Indicators %>% mutate(Unemployed_Per = X...Unemployed/Total.Population)

#Not in Labour Force
All_Indicators <- All_Indicators %>% mutate(Not_In_Labour_Force_Per = X...Not.in.Labour.Force/Total.Population)

#in labour force
All_Indicators <- All_Indicators %>% mutate(In_Labour_Force_Per = X...In.Labour.Force/Total.Population)

#local employment per population
All_Indicators <- All_Indicators %>% mutate(Local_Employment_Per = Local.Employment/Total.Population)

#local employment of people in labour force
All_Indicators <- All_Indicators %>% mutate(Local_Employment_In_Labour_Force = Local.Employment/X...In.Labour.Force)


#local businesses
All_Indicators <- All_Indicators %>% mutate(Businesses_Per = Businesses/Total.Population)

#recent immigrants
All_Indicators <- All_Indicators %>% mutate(Recent_Immigrants_Per = Recent.Immigrants.Category/Total.Population)

#Exploratory Analysis of certain factors

#Walk score versus home prices
Walk_Score <- ggplot(aes(x=Walk.Score, y=Home.Prices), data=All_Indicators) + geom_point() + geom_smooth(method=lm, color="red")
Walk_Score

TTC_Area <- ggplot(aes(x=TTC.Per.Area, y=Home.Prices), data=All_Indicators) + geom_point() + geom_smooth(method=lm, color="red")
TTC_Area
TTC_Pop <- ggplot(aes(x=TTC.Per.Pop, y=Home.Prices), data=All_Indicators) + geom_point() + geom_smooth(method=lm, color="red")
TTC_Pop

Ownership_Ratio <- ggplot(aes(x=Owned.Ratio, y=Home.Prices), data=All_Indicators) + geom_point() + geom_smooth(method=lm, color="red")
Ownership_Ratio

Price_to_Rent_Ratio 

Children <- ggplot(aes(x=Child.0.14, y=Home.Prices), data=All_Indicators) + geom_point() + geom_smooth(method=lm, color="red")
Children

Child_Care <- ggplot(aes(x=Child_Per_Child_Care, y=Home.Prices), data=All_Indicators) + geom_point() + geom_smooth(method=lm, color="red")
Child_Care

Chinese <- ggplot(aes(x=X...Chinese, y=Home.Prices), data=All_Indicators) + geom_point() + geom_smooth(method=lm, color="red")
Chinese

#income 
Household_Income_After_Tax <- ggplot(aes(x=After.Tax.Household.Income, y=Home.Prices), data=All_Indicators) + geom_point() + geom_smooth(method=lm, color="red")
Household_Income_After_Tax
#finding weird outliers (low after tax income and high home prices)
Income_Outliers <- All_Indicators %>% filter(Home.Prices > 1000000 & After.Tax.Household.Income < 100000)
Income_Outliers

#weird outliers the other way (high after tax income and low home prices)
Low_Income_Outliers <- All_Indicators %>% filter(Home.Prices < 700000 & After.Tax.Household.Income > 80000)
Low_Income_Outliers


#education levels
Bachelors_Degree <- ggplot(aes(x=Bachelors, y=Home.Prices), data=All_Indicators) + geom_point() + geom_smooth(method=lm, color="red")
Bachelors_Degree
College_Cert <- ggplot(aes(x=College_Cert, y=Home.Prices), data=All_Indicators) + geom_point() + geom_smooth(method=lm, color="red")
College_Cert
Less_High_School <- ggplot(aes(x=Less_Grade_Nine, y=Home.Prices), data=All_Indicators) + geom_point() + geom_smooth(method=lm, color="red")
Less_High_School

#male to female
Gender_Ratio <- ggplot(aes(x=Male_to_Female, y=Home.Prices), data=All_Indicators) + geom_point() + geom_smooth(method=lm, color="red")
Gender_Ratio
#high female
High_Female <- All_Indicators %>% filter(Male_to_Female < .85)
High_Male <- All_Indicators %>% filter(Male_to_Female > 1.1)

#local businesses
Local_Businesses <- ggplot(aes(x=Businesses_Per, y=Home.Prices), data=All_Indicators) + geom_point() + geom_smooth(method=lm, color="red")
Local_Businesses

#not visible minorities
Visible_Minorities <- ggplot(aes(x=Visible_Minority_Per, y=Home.Prices), data=All_Indicators) + geom_point() + geom_smooth(method=lm, color="red")
Visible_Minorities

#recent immigrants
Recent_Immigrants <- ggplot(aes(x=Recent_Immigrants_Per, y=Home.Prices), data=All_Indicators) + geom_point() + geom_smooth(method=lm, color="red")
Recent_Immigrants

#Linguistic Diversity
Linguistic_Diversity <- ggplot(aes(x=Linguistic.Diversity.Index, y=Home.Prices), data=All_Indicators) + geom_point() + geom_smooth(method=lm, color="red")
Linguistic_Diversity

#Healthy Food Index
Healthy_Food <- ggplot(aes(x=Healthy.Food.Index, y=Home.Prices), data=All_Indicators) + geom_point() + geom_smooth(method=lm, color="red")
Healthy_Food


#using regression to predict house prices in specific census wards

#split the data and use cross validation (tough because the sample is small, only 140 wards)
set.seed(123)
trainIndex <- createDataPartition(All_Indicators$Neighbourhood.Id, p=.8,
                                  list = FALSE, times = 1)
head(trainIndex)

neighbourhoodTrain <- All_Indicators[trainIndex,]
neighbourhoodTest <- All_Indicators[-trainIndex,]

#running basic regression with a few possible factors (just an initial test run)
linReg <- lm(Home.Prices ~  Bachelors, data=All_Indicators)
