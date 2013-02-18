# Assignment 2 Data Without Borders
#PART I

snf = read.csv("http://jakeporway.com/teaching/data/snf_2.csv", as.is=TRUE)
table(snf$race)

#Pecentage of people per race who were frisked

race_one_percent = ((nrow(subset(snf, frisked=="1" & race=="-1"))) / (nrow(subset(snf, race=="-1")))) *100
race_one_percent
#48.63133

race_two_percent = ((nrow(subset(snf, frisked=="1" & race=="1"))) / (nrow(subset(snf, race=="1")))) *100
race_two_percent
#59.30879

race_three_percent = ((nrow(subset(snf, frisked=="1" & race=="2"))) / (nrow(subset(snf, race=="2")))) *100
race_thee_percent
#59.71156



race_four_percent = ((nrow(subset(snf, frisked=="1" & race=="3"))) / (nrow(subset(snf, race=="3")))) *100
race_four_percent
# 55.76884

race_five_percent = ((nrow(subset(snf, frisked=="1" & race=="4"))) / (nrow(subset(snf, race=="4")))) *100
race_five_percent
#41.90888

race_six_percent = ((nrow(subset(snf, frisked=="1" & race=="5"))) / (nrow(subset(snf, race=="5")))) *100
race_six_percent
#48.51948

race_seven_percent = ((nrow(subset(snf, frisked=="1" & race=="6"))) / (nrow(subset(snf, race=="6")))) *100
race_seven_percent
#45.93301

#length of unique crimes
length(unique(snf$crime.suspected))
#1346

#plot crime.suspected frequencies
crimecount = as.data.frame(table(crime.suspected))
sort_crimecount= crimecount[order(crimecount$Freq),]
plot(sort_crimecount)
points(sort_crimecount, pch=10, col=2, cex=.1)

#view top 30 crimes
tail(sort_crimecount, 30)

# percentage of top 30 crimes 
sum(tail(sort_crimecount$Freq, 30))
nrow(snf)
percentage_of_top30crimes= ((sum(tail(sort_crimecount$Freq, 30)))/nrow(snf))*100
# top 30 crimes are 91.3% of all suspected crimes.


#creating crime abbreviations with first three letters of each crime
crime.abbv= substr(snf$crime.suspected,1,3)

# add crime.abbv to main data frame
snf$crime.abbv = crime.abbv

#count crime suspected frequency with filtered data set
crime.abbv_count = as.data.frame(table(snf$crime.abbv))
sort_crime.abbv_count= crime.abbv_count[order(crime.abbv_count$Freq),]
tail(sort_crime.abbv_count)
sum(tail(sort_crime.abbv_count$Freq, 30))
# 57178!! wow
real_percentage_top30crimes = ((sum(tail(sort_crime.abbv_count$Freq, 30)))/nrow(snf))*100
#98.4% ! Wondering how you knew to substring 3? Browsing through data enough?

# create subset for race
subset_crime_byrace= subset(snf, snf$race=="-1")
race_one_crimecount=as.data.frame(table(subset_crime_byrace$crime.abbv))
sort_race_one_crimecount= race_one_crimecount[order(race_one_crimecount$Freq),]
tail(sort_race_one_crimecount, 3)

subset_crime_byrace= subset(snf, snf$race=="1")
race_one_crimecount=as.data.frame(table(subset_crime_byrace$crime.abbv))
sort_race_one_crimecount= race_one_crimecount[order(race_one_crimecount$Freq),]
tail(sort_race_one_crimecount, 3)

subset_crime_byrace= subset(snf, snf$race=="2")
race_one_crimecount=as.data.frame(table(subset_crime_byrace$crime.abbv))
sort_race_one_crimecount= race_one_crimecount[order(race_one_crimecount$Freq),]
tail(sort_race_one_crimecount, 3)

subset_crime_byrace= subset(snf, snf$race=="3")
race_one_crimecount=as.data.frame(table(subset_crime_byrace$crime.abbv))
sort_race_one_crimecount= race_one_crimecount[order(race_one_crimecount$Freq),]
tail(sort_race_one_crimecount, 3)

subset_crime_byrace= subset(snf, snf$race=="4")
race_one_crimecount=as.data.frame(table(subset_crime_byrace$crime.abbv))
sort_race_one_crimecount= race_one_crimecount[order(race_one_crimecount$Freq),]
tail(sort_race_one_crimecount, 3)

subset_crime_byrace= subset(snf, snf$race=="5")
race_one_crimecount=as.data.frame(table(subset_crime_byrace$crime.abbv))
sort_race_one_crimecount= race_one_crimecount[order(race_one_crimecount$Freq),]
tail(sort_race_one_crimecount, 3)

subset_crime_byrace= subset(snf, snf$race=="6")
race_one_crimecount=as.data.frame(table(subset_crime_byrace$crime.abbv))
sort_race_one_crimecount= race_one_crimecount[order(race_one_crimecount$Freq),]
tail(sort_race_one_crimecount, 3)

#PART II
# convert hour to numeric
head(snf$time)
hour= as.numeric(substr(snf$time, 12, 13))
head(hour)
snf$hour= hour
#count frequency of each hour and plot
hour_count= as.data.frame(table(snf$hour))
sort_hour_count= hour_count[order(hour_count$Freq),]
tail(sort_hour_count)
plot(tail(sort_hour_count), type="l")

#Part III
subset_realistic_props= subset(snf, weight<400 & weight>90 & height>40)
head(subset_realistic_props)
#Add BMI column
snf$bmi= (snf$weight)*703/(snf$height*snf$height)

subset_obese= subset(snf, bmi>30)
nrow(subset_obese)

percentage_obese_arrested= (nrow(subset(snf, (bmi=30 | bmi>30) & arrested=="1")))/ (nrow(subset(snf, (bmi=30 |bmi >30))))*100
#5.44% obese people who were searched were arrested

percentage_nonobese_arrested= (nrow(subset(snf, bmi<30 & arrested=="1")))/ (nrow(subset(snf, bmi<30)))*100
#5.2% people non-obese people who were searched were arrested.


