#God is good all the times

#libraries
library(RColorBrewer)
library(e1071)

#Loading the data
bank = read.csv("C:/Users/tonyk/Documents/DSP 34/Task/6 SVM/DataSet/bank-additional-full.csv",
               sep = ';', stringsAsFactors = TRUE)

#Structure of the data
dim(bank)
'41188    21'

#Checking the missing value
"Note that NULL is different from the other two. NULL means that there is no 
value, while NA and NaN mean that there is some value, although one that is 
perhaps not usable. Here's an illustration of the difference:"
sapply(bank, function(x) sum(is.na(x)))
sapply(bank, function(x) sum(is.null(x)))
'No Missing values'

#Names of the variables
colnames(bank)

#Target Variable y - has the client subscribed a term deposit? (binary: 'yes','no')
str(bank$y)
'Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...'

#Barplot
table(bank$y)
'   no   yes 
 36548  4640 '

sum(table(bank$y))
'41188'

barplot(table(bank$y),
        col = c('forestgreen','royalblue2'),
        main = 'Client subscribed a Term Deposit',
        las=1)

#Dividing the data as per Term deposit yes & no
td_yes = bank[bank$y=='yes',]
td_no = bank[bank$y=='no',]

#____________________________Age - Age of the client
str(bank$age)
'int [1:41188] 56 57 37 40 56 45 59 41 24 25 ...'

summary(bank$age)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  17.00   32.00   38.00   40.02   47.00   98.00 '

#Histogram
hist(bank$age,
     col = brewer.pal(8,'Spectral'),
     main = 'Age of the client',
     xlab = 'Age')

par(mfrow = c(1,2))
hist(td_yes$age,
     col = brewer.pal(8,'Spectral'),
     main = 'Client having TD',
     ylim = c(0,8000),
     xlab = 'Age')

hist(td_no$age,
     col = brewer.pal(8,'Spectral'),
     main = 'Client having no TD',
     xlab = 'Age')

par(mfrow = c(1,1))

#Boxplot
boxplot(bank$age,
        col = 'orchid',
        horizontal = TRUE,
        main = 'Age of the Client')

#Checking the outliers from complete data
age_ub = quantile(bank$age, 0.75)+1.5*IQR(bank$age)
length(bank$age[bank$age>age_ub])
'469'

for (i in seq(age_ub,max(bank$age),4)){
  j = length(bank$age[bank$age > i])
  print(paste('No of outliers with ub as',round(i,0), 'is',j))
}
'
[1] "No of outliers with ub as 70 is 469"
[1] "No of outliers with ub as 74 is 301"
[1] "No of outliers with ub as 78 is 191"
[1] "No of outliers with ub as 82 is 99"
[1] "No of outliers with ub as 86 is 43"
[1] "No of outliers with ub as 90 is 10"
[1] "No of outliers with ub as 94 is 4"
[1] "No of outliers with ub as 98 is 2"'

'469 is less in number compare to complete data so removing all the outliers'

#Removing all the outliers
bank = bank[bank$age<=age_ub,]

dim(bank)
'40719    21'
'41188-40719 = 469 Obs removed'

#updating the divided data
td_yes = bank[bank$y=='yes',]
td_no = bank[bank$y=='no',]

#Boxplot
boxplot(bank$age,
        col = 'seagreen',
        horizontal = TRUE,
        main = 'Age of the Client')

#____________________________Job
"Type of job (categorical: 'admin.','blue-collar', 'entrepreneur', 
'housemaid', 'management', 'retired','self-employed', 'services', 
'student', 'technician', 'unemployed', 'unknown')"

str(bank$job)
'Factor w/ 12 levels "admin.","blue-collar",..: 4 8 8 1 8 8 1 2 10 8 ...'

#Barplot
table(bank$job)
'       admin.   blue-collar  entrepreneur     housemaid    management 
        10414          9251          1456          1035          2918 
      retired self-employed      services       student    technician 
         1301          1420          3969           875          6742 
   unemployed       unknown 
         1014           324 '
sum(table(bank$job))
'40719'

#Unknown category resembles missing values, checking whether there are any 
#unknown in other variables

for (i in names(which(sapply(bank, class)=='factor'))){
  if (sum(bank[i]=='unknown') >0 ){
    print(paste('Unknown obs in',i,'are',sum(bank[i]=='unknown')))  
  }
}

'
[1] "Unknown obs in job are 324"
[1] "Unknown obs in marital are 80"
[1] "Unknown obs in education are 1683"
[1] "Unknown obs in default are 8554"
[1] "Unknown obs in housing are 981"
[1] "Unknown obs in loan are 981"'

# 8554/40719 = 0.21(21%) of data is unknown, so not removing these obs but 
# considering as a category

#Barplot
barplot(table(bank$job),
        col = brewer.pal(11,'Dark2'),
        main = 'Job of the client',
        las=2)

par(mfrow = c(1,2))
barplot(table(td_yes$job),
        col = brewer.pal(11,'Paired'),
        main = 'Job - Having TD',
        ylim = c(0,max(table(td_no$job))),
        las=2)

barplot(table(td_no$job),
        col = brewer.pal(11,'Paired'),
        main = 'Job - Having no TD',
        ylim = c(0,max(table(td_no$job))),las=2)
par(mfrow = c(1,1))

#Getting the proportion of each factors
#saving as a dataframe
job_prop = as.data.frame(round(prop.table(table(bank$job)),6), 
                         responseName = c('Proportion'))

#Order output in descending order
job_prop = job_prop[order(job_prop$Proportion, decreasing = TRUE),]
job_prop$cum_prop = cumsum(job_prop$Proportion)
row.names(job_prop) = NULL #to reset row index
job_prop

'
            Var1 Proportion cum_prop
1         admin.   0.255753 0.255753
2    blue-collar   0.227191 0.482944
3     technician   0.165574 0.648518
4       services   0.097473 0.745991
5     management   0.071662 0.817653
6   entrepreneur   0.035757 0.853410
7  self-employed   0.034873 0.888283
8        retired   0.031951 0.920234
9      housemaid   0.025418 0.945652
10    unemployed   0.024902 0.970554
11       student   0.021489 0.992043
12       unknown   0.007957 1.000000'
#top 4 categories covering the 75% of the data

#____________________________Marital
"Marital status (categorical: 'divorced','married','single','unknown'; note: 
'divorced' means divorced or widowed)"

str(bank$marital)
'Factor w/ 4 levels "divorced","married",..: 2 2 2 2 2 2 2 2 3 3 ...'

#Barplot
table(bank$marital)
'divorced  married   single  unknown 
    4476    24610    11553       80 '

sum(table(bank$marital))
'40719'

barplot(table(bank$marital),
        col = brewer.pal(9,'Set1'),
        main = 'Marital of the client',
        las=2)

par(mfrow = c(1,2))
barplot(table(td_yes$marital),
        col = brewer.pal(9,'Set1'),
        main = 'Marital - Having TD',
        ylim = c(0,max(table(td_no$marital))),
        las=2)

barplot(table(td_no$marital),
        col = brewer.pal(9,'Set1'),
        main = 'Marital - Having no TD',
        ylim = c(0,max(table(td_no$marital))),las=2)
par(mfrow = c(1,1))

#Getting the proportion of each factors
#saving as a dataframe
mar_prop = as.data.frame(round(prop.table(table(bank$marital)),4), responseName = c('Proportion'))

#Order output in descending order
mar_prop = mar_prop[order(mar_prop$Proportion, decreasing = TRUE),]
mar_prop$cum_prop = cumsum(mar_prop$Proportion)
row.names(mar_prop) = NULL #to reset row index
mar_prop
'
      Var1 Proportion cum_prop
1  married     0.6044   0.6044
2   single     0.2837   0.8881
3 divorced     0.1099   0.9980
4  unknown     0.0020   1.0000'

#____________________________Education
"(categorical: 'basic.4y', 'basic.6y' ,'basic.9y', 'high.school', 
'illiterate', 'professional.course', 'university.degree','unknown')"

str(bank$education)
'Factor w/ 8 levels "basic.4y","basic.6y",..: 1 4 4 2 4 3 6 8 6 4 ...'

#Barplot
table(bank$education)
'           basic.4y            basic.6y            basic.9y 
               3935                2279                6018 
        high.school          illiterate professional.course 
               9481                  17                5201 
  university.degree             unknown 
              12105                1683'

sum(table(bank$education))
'40719'

barplot(table(bank$education),
        col = brewer.pal(11,'RdYlGn'),
        main = 'Education of the client',
        las=2)

par(mfrow = c(1,2))
barplot(table(td_yes$education),
        col = brewer.pal(11,'RdYlGn'),
        main = 'education - Having TD',
        ylim = c(0,max(table(td_no$education))),
        las=2)

barplot(table(td_no$education),
        col = brewer.pal(11,'RdYlGn'),
        main = 'education - Having no TD',
        ylim = c(0,max(table(td_no$education))),las=2)
par(mfrow = c(1,1))

#Getting the proportion of each factors
#saving as a dataframe
edu_prop = as.data.frame(round(prop.table(table(bank$education)),5), 
                         responseName = c('Proportion'))

#Order output in descending order
edu_prop = edu_prop[order(edu_prop$Proportion, decreasing = TRUE),]
edu_prop$cum_prop = cumsum(edu_prop$Proportion)
row.names(edu_prop) = NULL #to reset row index
edu_prop
'
                 Var1 Proportion cum_prop
1   university.degree    0.29728  0.29728
2         high.school    0.23284  0.53012
3            basic.9y    0.14779  0.67791
4 professional.course    0.12773  0.80564
5            basic.4y    0.09664  0.90228
6            basic.6y    0.05597  0.95825
7             unknown    0.04133  0.99958
8          illiterate    0.00042  1.00000'
#top 4 categories covering 80% of the data

#____________________________Has credit in default? 
"(categorical: 'no','yes','unknown')"

str(bank$default)
'Factor w/ 3 levels "no","unknown",..: 1 2 1 1 1 2 1 2 1 1 ...'

#Barplot
table(bank$default)
'     no unknown     yes 
  32162    8554       3 '

sum(table(bank$default))
'40719'

barplot(table(bank$default),
        col = brewer.pal(3,'Set1'),
        main = 'Default of the client',
        las=1)

par(mfrow = c(1,2))
barplot(table(td_yes$default),
        col = brewer.pal(3,'Set1'),
        main = 'default - Having TD',
        ylim = c(0,max(table(td_no$default))),
        las=1)

barplot(table(td_no$default),
        col = brewer.pal(3,'Set1'),
        main = 'default - Having no TD',
        ylim = c(0,max(table(td_no$default))),las=1)
par(mfrow = c(1,1))


#Getting the proportion of each factors
#saving as a dataframe
def_prop = as.data.frame(round(prop.table(table(bank$default)),6), 
                         responseName = c('Proportion'))

#Order output in descending order
def_prop = def_prop[order(def_prop$Proportion, decreasing = TRUE),]
def_prop$cum_prop = cumsum(def_prop$Proportion)
row.names(def_prop) = NULL #to reset row index
def_prop
'
     Var1 Proportion cum_prop
1      no   0.789852 0.789852
2 unknown   0.210074 0.999926
3     yes   0.000074 1.000000'
#Major portion of data are not credit defaulters

#____________________________Has a housing loan? 
"(categorical: 'no','yes','unknown')"

str(bank$housing)
'Factor w/ 3 levels "no","unknown",..: 1 1 3 1 1 1 1 1 3 3 ...'

#Barplot
table(bank$housing)
'     no unknown     yes 
  18419     981   21319 '

sum(table(bank$housing))
'40719'

barplot(table(bank$housing),
        col = brewer.pal(3,'Set2'),
        main = 'Housing of the client',
        las=1)

par(mfrow = c(1,2))
barplot(table(td_yes$housing),
        col = brewer.pal(3,'Set2'),
        main = 'housing - Having TD',
        ylim = c(0,max(table(td_no$housing))),
        las=1)

barplot(table(td_no$housing),
        col = brewer.pal(3,'Set2'),
        main = 'housing - Having no TD',
        ylim = c(0,max(table(td_no$housing))),
        las=1)
par(mfrow = c(1,1))

#Getting the proportion of each factors
#saving as a dataframe
hou_prop = as.data.frame(round(prop.table(table(bank$housing)),3), 
                         responseName = c('Proportion'))

#Order output in descending order
hou_prop = hou_prop[order(hou_prop$Proportion, decreasing = TRUE),]
hou_prop$cum_prop = cumsum(hou_prop$Proportion)
row.names(hou_prop) = NULL #to reset row index
hou_prop
'
     Var1 Proportion cum_prop
1     yes      0.524    0.524
2      no      0.452    0.976
3 unknown      0.024    1.000'
#Data is balanced with client having and not having housing loan

#____________________________has a personal loan?
"(categorical: 'no','yes','unknown')"

str(bank$loan)
'Factor w/ 3 levels "no","unknown",..: 1 1 1 1 3 1 1 1 1 1 ...'

#Barplot
table(bank$loan)
'     no unknown     yes 
  33560     981    6178'

sum(table(bank$loan))
'40719'

barplot(table(bank$loan),
        col = brewer.pal(3,'Spectral'),
        main = 'loan of the client',
        las=1)

par(mfrow = c(1,2))
barplot(table(td_yes$loan),
        col = brewer.pal(3,'Spectral'),
        main = 'loan - Having TD',
        ylim = c(0,max(table(td_no$loan))),
        las=1)

barplot(table(td_no$loan),
        col = brewer.pal(3,'Spectral'),
        main = 'loan - Having no TD',
        ylim = c(0,max(table(td_no$loan))),
        las=1)
par(mfrow = c(1,1))

#Getting the proportion of each factors
#saving as a dataframe
loan_prop = as.data.frame(round(prop.table(table(bank$loan)),3), 
                         responseName = c('Proportion'))

#Order output in descending order
loan_prop = loan_prop[order(loan_prop$Proportion, decreasing = TRUE),]
loan_prop$cum_prop = cumsum(loan_prop$Proportion)
row.names(loan_prop) = NULL #to reset row index
loan_prop
'
     Var1 Proportion cum_prop
1      no      0.824    0.824
2     yes      0.152    0.976
3 unknown      0.024    1.000'
#Major proportion of data is not having a personal loan

#____________________________contact communication type 
" (categorical: 'cellular','telephone')"

str(bank$contact)
'Factor w/ 2 levels "cellular","telephone": 2 2 2 2 2 2 2 2 2 2 ...'

#Barplot
table(bank$contact)
' cellular telephone 
    25724     14995 '

sum(table(bank$contact))
'40719'

barplot(table(bank$contact),
        col = c('turquoise4', 'tan4'),
        main = 'contact of the client',
        las=1)

par(mfrow = c(1,2))
barplot(table(td_yes$contact),
        col = c('turquoise4', 'tan4'),
        main = 'contact - Having TD',
        ylim = c(0,max(table(td_no$contact))),
        las=1)

barplot(table(td_no$contact),
        col = c('turquoise4', 'tan4'),
        main = 'contact - Having no TD',
        ylim = c(0,max(table(td_no$contact))),
        las=1)
par(mfrow = c(1,1))

#Getting the proportion of each factors
#saving as a dataframe
contact_prop = as.data.frame(round(prop.table(table(bank$contact)),3), 
                          responseName = c('Proportion'))

#Order output in descending order
contact_prop = contact_prop[order(contact_prop$Proportion, decreasing = TRUE),]
contact_prop$cum_prop = cumsum(contact_prop$Proportion)
row.names(contact_prop) = NULL #to reset row index
contact_prop
'
       Var1 Proportion cum_prop
1  cellular      0.632    0.632
2 telephone      0.368    1.000'

#____________________________last contact month of year 
" (categorical: 'jan', 'feb', 'mar', ..., 'nov', 'dec')"

str(bank$month)
'Factor w/ 2 levels "cellular","telephone": 2 2 2 2 2 2 2 2 2 2 ...'

#Barplot
table(bank$month)
'  apr   aug   dec   jul   jun   mar   may   nov   oct   sep 
 2562  6091   160  7141  5301   503 13736  4064   648   513 '

sum(table(bank$month))
'40719'

barplot(table(bank$month),
        col = brewer.pal(9,'Set1'),
        main = 'Last Contact month',
        las=1)

par(mfrow = c(1,2))
barplot(table(td_yes$month),
        col = brewer.pal(9,'Set1'),
        main = 'Last Contact - Having TD',
        ylim = c(0,max(table(td_no$month))),
        las=2)

barplot(table(td_no$month),
        col = brewer.pal(9,'Set1'),
        main = 'Last Contact - Having no TD',
        ylim = c(0,max(table(td_no$month))),
        las=2)
par(mfrow = c(1,1))

#Getting the proportion of each factors
#saving as a dataframe
month_prop = as.data.frame(round(prop.table(table(bank$month)),3), 
                             responseName = c('Proportion'))

#Order output in descending order
month_prop = month_prop[order(month_prop$Proportion, decreasing = TRUE),]
month_prop$cum_prop = cumsum(month_prop$Proportion)
row.names(month_prop) = NULL #to reset row index
month_prop
'
   Var1 Proportion cum_prop
1   may      0.337    0.337
2   jul      0.175    0.512
3   aug      0.150    0.662
4   jun      0.130    0.792
5   nov      0.100    0.892
6   apr      0.063    0.955
7   oct      0.016    0.971
8   sep      0.013    0.984
9   mar      0.012    0.996
10  dec      0.004    1.000'
#Apr to aug - 5 months contacted 85.5% of clients

#____________________________last contact day of the week  
"(categorical: 'mon','tue','wed','thu','fri')"

str(bank$day_of_week)
'Factor w/ 5 levels "fri","mon","thu",..: 2 2 2 2 2 2 2 2 2 2 ...'

#Barplot
table(bank$day_of_week)
' fri  mon  thu  tue  wed 
 7739 8426 8522 7980 8052 '

sum(table(bank$day_of_week))
'40719'

barplot(table(bank$day_of_week),
        col = brewer.pal(5,'RdYlBu'),
        main = 'Last Contact day_of_week',
        las=1)

par(mfrow = c(1,2))
barplot(table(td_yes$day_of_week),
        col = brewer.pal(5,'RdYlBu'),
        main = 'Last Contact - Having TD',
        ylim = c(0,max(table(td_no$day_of_week))),
        las=2)

barplot(table(td_no$day_of_week),
        col = brewer.pal(5,'RdYlBu'),
        main = 'Last Contact - Having no TD',
        ylim = c(0,max(table(td_no$day_of_week))),
        las=2)
par(mfrow = c(1,1))

#Getting the proportion of each factors
#saving as a dataframe
day_of_week_prop = as.data.frame(round(prop.table(table(bank$day_of_week)),3), 
                           responseName = c('Proportion'))

#Order output in descending order
day_of_week_prop = day_of_week_prop[order(day_of_week_prop$Proportion, decreasing = TRUE),]
day_of_week_prop$cum_prop = cumsum(day_of_week_prop$Proportion)
row.names(day_of_week_prop) = NULL #to reset row index
day_of_week_prop
'
   Var1 Proportion cum_prop
1  thu      0.209    0.209
2  mon      0.207    0.416
3  wed      0.198    0.614
4  tue      0.196    0.810
5  fri      0.190    1.000'
#Equally contacting in all week days

#____________________________last contact duration, in seconds
str(bank$duration)
' int [1:40719] 261 149 226 151 307 198 139 217 380 50 ...'

#Histogram
hist(bank$duration,
     col = brewer.pal(5,'Accent'),
     main = 'Last Contact duration',
     xlab = 'Duration', las=1)

par(mfrow = c(1,2))
hist(td_yes$duration,
     col = brewer.pal(5,'Accent'),
     main = 'Having TD',
     ylim = c(0,33000),
     xlab = 'Last call duration in secs', las=2)

hist(td_no$duration,
     col = brewer.pal(5,'Accent'),
     main = 'Having No TD',
     xlab = 'Last call duration in secs', las=2)
par(mfrow = c(1,1))

#Boxplot
boxplot(bank$duration,
        col = c('seagreen4'),
        horizontal = TRUE,
        main = 'Last call duration in secs')

#Checking the outliers from complete data
dur_ub = quantile(bank$duration, 0.75)+1.5*IQR(bank$duration)
length(bank$duration[bank$duration>dur_ub])
'2935'

for (i in seq(dur_ub,max(bank$duration),400)){
  j = length(bank$duration[bank$duration > i])
  print(paste('No of outliers with ub as',round(i,0), 'is',j))
}
'
[1] "No of outliers with ub as 644 is 2935"
[1] "No of outliers with ub as 1044 is 832"
[1] "No of outliers with ub as 1444 is 246"
[1] "No of outliers with ub as 1844 is 88"
[1] "No of outliers with ub as 2244 is 36"
[1] "No of outliers with ub as 2644 is 21"
[1] "No of outliers with ub as 3044 is 15"
[1] "No of outliers with ub as 3444 is 6"
[1] "No of outliers with ub as 3844 is 2"
[1] "No of outliers with ub as 4244 is 1"
[1] "No of outliers with ub as 4644 is 1"'

#Considering the ub 1044 and removing the extreme outliers
"1044/60 = 17.4 Minutes"
bank = bank[bank$duration<=1044,]

dim(bank)
'39887    21'
'40719-39887 = 832 removed'

#Updating sub datasets
td_yes = bank[bank$y=='yes',]
td_no = bank[bank$y=='no',]

#Boxplot
boxplot(bank$duration,
        col = c('purple4'),
        horizontal = TRUE,
        main = 'Last call duration in secs')

#____________________________Campaign
'number of contacts performed during this campaign and for this 
client (numeric, includes last contact)'

str(bank$campaign)
'int [1:39887] 1 1 1 1 1 1 1 1 1 1 ...'

#Histogram
hist(bank$campaign,
     col = brewer.pal(11,'Paired'),
     main = 'Campaign',
     xlab = 'No of contacts performed', las=1)

par(mfrow = c(1,2))
hist(td_yes$campaign,
     col = brewer.pal(11,'Paired'),
     main = 'Campaign - Having TD',
     ylim = c(0,30000),
     xlab = 'No of contacts performed', las=2)

hist(td_no$campaign,
     col = brewer.pal(11,'Paired'),
     main = 'Campaign - Having No TD',
     xlab = 'No of contacts performed', las=2)
par(mfrow = c(1,1))

#Boxplot
boxplot(bank$campaign,
        col = c('orangered3'),
        horizontal = TRUE,
        main = 'Campaign - No of contacts performed')

#Checking the outliers from complete data
cam_ub = quantile(bank$campaign, 0.75)+1.5*IQR(bank$campaign)
length(bank$campaign[bank$campaign>cam_ub])
'2351'

for (i in seq(cam_ub,max(bank$campaign),3)){
  j = length(bank$campaign[bank$campaign > i])
  print(paste('No of outliers with ub as',round(i,0), 'is',j))
}
'
[1] "No of outliers with ub as 6 is 2351"
[1] "No of outliers with ub as 9 is 1073"
[1] "No of outliers with ub as 12 is 559"
[1] "No of outliers with ub as 15 is 350"
[1] "No of outliers with ub as 18 is 211"
[1] "No of outliers with ub as 21 is 132"
[1] "No of outliers with ub as 24 is 84"
[1] "No of outliers with ub as 27 is 58"
[1] "No of outliers with ub as 30 is 33"
[1] "No of outliers with ub as 33 is 18"
[1] "No of outliers with ub as 36 is 10"
[1] "No of outliers with ub as 39 is 8"
[1] "No of outliers with ub as 42 is 3"
[1] "No of outliers with ub as 45 is 1"
[1] "No of outliers with ub as 48 is 1"
[1] "No of outliers with ub as 51 is 1"
[1] "No of outliers with ub as 54 is 1"'

#Considering the ub 12 and removing the extreme outliers
bank = bank[bank$campaign<=12,]

dim(bank)
'39328    21'
'39887-39328 = 559 removed'

#Updating sub datasets
td_yes = bank[bank$y=='yes',]
td_no = bank[bank$y=='no',]

#Boxplot
boxplot(bank$campaign,
        col = c('firebrick3'),
        horizontal = TRUE,
        main = 'Campaign - No of contacts performed')

#____________________________Pdays'
'number of days that passed by after the client was last contacted from a 
previous campaign (numeric; 999 means client was not previously contacted)'

str(bank$pdays)
'int [1:39328] 999 999 999 999 999 999 999 999 999 999 ...'

table(bank$pdays)
'999 is very far from sequence and it covering major portion of data,
other data is saying that there are certain people contacted in last 27 days 
as per table outcome. There converting the data into categorical 0 & 1 ie
client previously not contacted & client contacted in last 30 days respectively'
bank$pdays[bank$pdays!=999] = 1
bank$pdays[bank$pdays==999] = 0

#Converting numeric data into factors with level names specified
bank$pdays = factor(bank$pdays, 
                    labels = c('Previously not contacted', 'Contacted in last 30 days'))

#updating the divided data
td_yes = bank[bank$y=='yes',]
td_no = bank[bank$y=='no',]

table(bank$pdays)
' Previously not contacted Contacted in last 30 days 
                    37938                      1390 '

sum(table(bank$pdays))
'39328'

barplot(table(bank$pdays),
        col = c('coral4', 'darkgreen'),
        main = 'Contacting the client',
        las=1)

#Following to adjust plot margins
#mar - A numeric vector of length 4, which sets the margin sizes in the 
#following order: bottom, left, top, and right. The default is c(5.1, 4.1, 4.1, 2.1).
par(mfrow = c(1,2), mar=c(3,4,2,2))
barplot(table(td_yes$pdays),
        col = c('coral4', 'darkgreen'),
        main = 'Last Contact - Having TD',
        ylim = c(0,max(table(td_no$pdays))),
        las=1)

barplot(table(td_no$pdays),
        col = c('coral4', 'darkgreen'),,
        main = 'Last Contact - Having no TD',
        ylim = c(0,max(table(td_no$pdays))),
        las=1)
par(mfrow = c(1,1))

#Getting the proportion of each factors
#saving as a dataframe
pdays_prop = as.data.frame(round(prop.table(table(bank$pdays)),3), 
                                 responseName = c('Proportion'))

#Order output in descending order
pdays_prop = pdays_prop[order(pdays_prop$Proportion, decreasing = TRUE),]
pdays_prop$cum_prop = cumsum(pdays_prop$Proportion)
row.names(pdays_prop) = NULL #to reset row index
pdays_prop

'                       Var1 Proportion cum_prop
1  Previously not contacted      0.965    0.965
2 Contacted in last 30 days      0.035    1.000'

#____________________________Previous
'number of contacts performed before this campaign and for this client'
str(bank$previous)
'int [1:39328] 0 0 0 0 0 0 0 0 0 0 ...'

table(bank$previous)
'    0     1     2     3     4     5     6     7 
33993  4361   691   199    61    18     4     1 '

'Contacts performed more than 1 is very less in number, so keeping all as 1 and
converting the data into categorical'

bank$previous[bank$previous>1] = 1

#Converting numeric data into factors with level names specified
bank$previous = factor(bank$previous, 
                    labels = c('Not contacted before campaign', 'Contacted before campaign'))

#updating the divided data
td_yes = bank[bank$y=='yes',]
td_no = bank[bank$y=='no',]

table(bank$previous)
'Not contacted before campaign     Contacted before campaign 
                        33993                          5335'

sum(table(bank$previous))
'39328'

barplot(table(bank$previous),
        names.arg = c('Not contacted \n before campaign', 'Contacted before \n campaign'),
        col = c('gold3', 'cyan4'),
        main = 'Contacting the client',
        las=1)

#Following to adjust plot margins
#mar - A numeric vector of length 4, which sets the margin sizes in the 
#following order: bottom, left, top, and right. The default is c(5.1, 4.1, 4.1, 2.1).
par(mfrow = c(1,2), mar=c(3,4,2,2))
barplot(table(td_yes$previous),
        names.arg = c('Not contacted \n before campaign', 
                      'Contacted before \n campaign'),
        col = c('coral4', 'darkgreen'),
        main = 'Last Contact - Having TD',
        ylim = c(0,max(table(td_no$previous))),
        las=1)

barplot(table(td_no$previous),
        names.arg = c('Not contacted \n before campaign', 
                      'Contacted before \n campaign'),
        col = c('coral4', 'darkgreen'),,
        main = 'Last Contact - Having no TD',
        ylim = c(0,max(table(td_no$previous))),
        las=1)
par(mfrow = c(1,1))

#Getting the proportion of each factors
#saving as a dataframe
previous_prop = as.data.frame(round(prop.table(table(bank$previous)),3), 
                           responseName = c('Proportion'))

#Order output in descending order
previous_prop = previous_prop[order(previous_prop$Proportion, decreasing = TRUE),]
previous_prop$cum_prop = cumsum(previous_prop$Proportion)
row.names(previous_prop) = NULL #to reset row index
previous_prop
'                           Var1 Proportion cum_prop
1 Not contacted before campaign      0.864    0.864
2     Contacted before campaign      0.136    1.000'

#____________________________Poutcome
"outcome of the previous marketing campaign 
(categorical: 'failure','nonexistent','success')"

str(bank$poutcome)
'Factor w/ 3 levels "failure","nonexistent",..: 2 2 2 2 2 2 2 2 2 2 ...'

#Barplot
table(bank$poutcome)
'    failure nonexistent     success 
       4078       33993        1257 '

sum(table(bank$poutcome))
'39328'

barplot(table(bank$poutcome),
        col = c('coral3','gold2', 'cyan3'),
        main = 'Last Contact poutcome',
        las=1)

par(mfrow = c(1,2), mar=c(5.5,4,2,2))
barplot(table(td_yes$poutcome),
        col = c('coral3','gold2', 'cyan3'),
        main = 'Last Contact - Having TD',
        ylim = c(0,max(table(td_no$poutcome))),
        las=2)

barplot(table(td_no$poutcome),
        col = c('coral3','gold2', 'cyan3'),
        main = 'Last Contact - Having no TD',
        ylim = c(0,max(table(td_no$poutcome))),
        las=2)
par(mfrow = c(1,1))

#Getting the proportion of each factors
#saving as a dataframe
poutcome_prop = as.data.frame(round(prop.table(table(bank$poutcome)),3), 
                                 responseName = c('Proportion'))

#Order output in descending order
poutcome_prop = poutcome_prop[order(poutcome_prop$Proportion, decreasing = TRUE),]
poutcome_prop$cum_prop = cumsum(poutcome_prop$Proportion)
row.names(poutcome_prop) = NULL #to reset row index
poutcome_prop
'         Var1 Proportion cum_prop
1 nonexistent      0.864    0.864
2     failure      0.104    0.968
3     success      0.032    1.000'

#____________________________employment variation rate - quarterly indicator (numeric)
str(bank$emp.var.rate)
'num [1:39328] 1.1 1.1 1.1 1.1 1.1 1.1 1.1 1.1 1.1 1.1 ...'

#Histogram
hist(bank$emp.var.rate,
     col = brewer.pal(11,'RdYlBu'),
     main = 'Employment variation rate',
     xlab = 'Quaterly Idicator', las=1)

par(mfrow = c(1,2), mar=c(5,4,2,2))
hist(td_yes$emp.var.rate,
     col = brewer.pal(11,'RdYlBu'),
     main = 'Emp.var.rate - Having TD',
     ylim = c(0,20000),
     xlab = 'Quaterly Idicator', las=2)

hist(td_no$emp.var.rate,
     col = brewer.pal(11,'RdYlBu'),
     main = 'Emp.var.rate - Having No TD',
     xlab = 'Quaterly Idicator', las=2)
par(mfrow = c(1,1))

#Boxplot
boxplot(bank$emp.var.rate,
        col = c('darkslateblue'),
        horizontal = TRUE,
        main = 'emp.var.rate - Quaterly Idicator')

#____________________________consumer price index - monthly indicator
str(bank$cons.price.idx)
'num [1:39328] 94 94 94 94 94 ...'

#Histogram
hist(bank$cons.price.idx,
     col = brewer.pal(8,'Set1'),
     main = 'Consumer price index',
     xlab = 'Monthly Idicator', las=1)

par(mfrow = c(1,2), mar=c(5,4,2,2))
hist(td_yes$cons.price.idx,
     col = brewer.pal(8,'Set1'),
     main = 'cons.price.idx - Having TD',
     ylim = c(0,13000),
     xlab = 'Monthly Idicator', las=2)

hist(td_no$cons.price.idx,
     col = brewer.pal(8,'Set1'),
     main = 'cons.price.idx - Having No TD',
     xlab = 'Monthly Idicator', las=2)
par(mfrow = c(1,1))

#Boxplot
boxplot(bank$cons.price.idx,
        col = c('red3'),
        horizontal = TRUE,
        main = 'Consumer price index - Monthly Idicator')

#____________________________consumer confidence index - monthly indicator
str(bank$cons.conf.idx)
'num [1:39328] -36.4 -36.4 -36.4 -36.4 -36.4 -36.4 -36.4 -36.4 -36.4 -36.4 ...'

#Histogram
hist(bank$cons.conf.idx,
     col = brewer.pal(8,'Dark2'),
     main = 'Consumer confidence index',
     xlab = 'Monthly Idicator', las=1)

par(mfrow = c(1,2), mar=c(5,4,2,2))
hist(td_yes$cons.conf.idx,
     col = brewer.pal(8,'Set1'),
     main = 'cons.conf.idx - Having TD',
     ylim = c(0,12000),
     xlab = 'Monthly Idicator', las=2)

hist(td_no$cons.conf.idx,
     col = brewer.pal(8,'Set1'),
     main = 'cons.conf.idx - Having No TD',
     xlab = 'Monthly Idicator', las=2)
par(mfrow = c(1,1))

#Boxplot
boxplot(bank$cons.conf.idx,
        col = c('seagreen3'),
        horizontal = TRUE,
        main = 'Consumer confidence index - Monthly Idicator')

#Checking the outliers from complete data
cci_ub = quantile(bank$cons.conf.idx, 0.75)+1.5*IQR(bank$cons.conf.idx)
length(bank$cons.conf.idx[bank$cons.conf.idx>cci_ub])
'381'
#Outliers are not extreme values and less in number so ignoring

#____________________________euribor 3 month rate - daily indicator
str(bank$euribor3m)
'num [1:39328] 4.86 4.86 4.86 4.86 4.86 ...'

#Histogram
hist(bank$euribor3m,
     col = brewer.pal(8,'YlOrRd'),
     main = 'Euribor 3 month rate',
     xlab = 'Daily Idicator', las=1)

par(mfrow = c(1,2), mar=c(5,4,2,2))
hist(td_yes$euribor3m,
     col = brewer.pal(8,'Set1'),
     main = 'Euribor3m - Having TD',
     ylim = c(0,21000),
     xlab = 'Daily Idicator', las=2)

hist(td_no$euribor3m,
     col = brewer.pal(8,'Set1'),
     main = 'Euribor3m - Having No TD',
     xlab = 'Daily Idicator', las=2)
par(mfrow = c(1,1))

#Boxplot
boxplot(bank$euribor3m,
        col = c('gold3'),
        horizontal = TRUE,
        main = 'Euribor 3 month rate - Daily Idicator')

#____________________________number of employees - quarterly indicator 
str(bank$nr.employed)
'num [1:39328] 5191 5191 5191 5191 5191 ...'

#Histogram
hist(bank$nr.employed,
     col = brewer.pal(8,'YlOrRd'),
     main = 'Number of Employees',
     xlab = 'Quaterly Idicator', las=1)

par(mfrow = c(1,2))
hist(td_yes$nr.employed,
     col = brewer.pal(8,'Set1'),
     main = 'No of Emp - Having TD',
     ylim = c(0,15000),
     xlab = 'Quaterly Idicator', las=2)

hist(td_no$nr.employed,
     col = brewer.pal(8,'Set1'),
     main = 'No of Emp - Having No TD',
     xlab = 'Quaterly Idicator', las=2)
par(mfrow = c(1,1))

#Boxplot
boxplot(bank$nr.employed,
        col = c('khaki3'),
        horizontal = TRUE,
        main = 'No of Employees - Quaterly Idicator')


#Splitting the data into train & test
set.seed(121)
select_rows_80 = sample(1:nrow(bank), round(0.8*nrow(bank)),replace = FALSE)         
bank_train = bank[select_rows_80,]
bank_test = bank[-select_rows_80,]

'Model coding taking few minutes to execute'
#Building the Model - Default - Radial Basis (RBF)
model1 = svm(y~.-y, data = bank_train)

svm()
#Prediction & Accuracy
pred1 = predict(model1, bank_test[,-21])
pred_tab1 = table(pred1, bank_test$y)
print(pred_tab1)
'pred1   no  yes
  no  6984  526
  yes   92  264'

sum(diag(pred_tab1))/sum(pred_tab1)
'0.921434'

#Building the Model2 - Linear
model2 = svm(y~.-y, data = bank_train, kernel='linear')
#Prediction & Accuracy
pred2 = predict(model2, bank_test[,-21])
pred_tab2 = table(pred2, bank_test$y)
print(pred_tab2)
'pred2   no  yes
  no  6962  532
  yes  114  258'

sum(diag(pred_tab2))/sum(pred_tab2)
'0.9178744'

#Building the Model3 - Polynomial
model3 = svm(y~.-y, data = bank_train, kernel='polynomial')
#Prediction & Accuracy
pred3 = predict(model3, bank_test[,-21])
pred_tab3 = table(pred3, bank_test$y)
print(pred_tab3)
'pred3   no  yes
  no  7007  578
  yes   69  212'

sum(diag(pred_tab3))/sum(pred_tab3)
'0.9177473'

#Building the Model4 - Sigmoid
model4 = svm(y~.-y, data = bank_train, kernel='sigmoid')
#Prediction & Accuracy
pred4 = predict(model4, bank_test[,-21])
pred_tab4 = table(pred4, bank_test$y)
print(pred_tab4)
'pred4   no  yes
  no  6817  477
  yes  259  313'

sum(diag(pred_tab4))/sum(pred_tab4)
'0.9064327'