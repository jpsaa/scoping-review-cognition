
rm(list=ls())
# set environment
setwd("/Users/Womps/Documents/U/SkyDrive/Australia Life/PhD year 2/Scoping Review/R-files")

#### Packages for figures ####
# install.packages('extrafont')
# install.packages('gridExtra')
# install.packages('grid')
# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("viridis")
# install.packages("RColorBrewer")

library(extrafont)
library(gridExtra)
library(grid)
library(ggplot2)
library(tidyverse)
library(viridis)
library(RColorBrewer)

#### Packages for data analysis ####
# install.packages('countrycode')
# install.packages('datasets')
# install.packages('reshape2')
# install.packages("openxlsx", dependencies = TRUE)
# install.packages('dplyr')

library(countrycode)
library(datasets)
library(reshape2)
library(openxlsx)
library(dplyr)





# CALLING DATA DIRECTLY FROM EXCEL
b=read.xlsx ("6.Table1July18th2018.xlsx", sheet = 5, skipEmptyRows = FALSE)
# str(b)
write.csv(b, "test.csv", row.names = F)
dat=read.csv ("test.csv", h=T)
# str(dat)


#### Processing study information columns ####

### Countries and continents
foo<-data.frame(do.call('rbind',strsplit(as.character(dat$Author.and.Country), "\\)|\\(")))
foo$X1<-trimws(foo$X1)
author<-foo$X1
country<-foo$X2

### Other selected columns
year<-dat$Year
sample<-dat$sample
intervention<-dat$Study.type
descriptives<-dat$descriptives
data.status<-dat$scoping.data.status
title<-dat$Title
meta<-dat$first.meta.included
meta.final<-dat$meta.final.decision
stroke.type<-dat$Stroke.type
age<-dat$age
age.disp<-dat$age.sd
m.f<-dat$Male.Female.ratio
calc.timepoints<-dat$calculated.timepoints
fu<-dat$furthest.fu

### Dataframe with all information columns #### 
df<-data.frame(author, country, year,
                title,
               intervention,
                sample, descriptives, data.status,
               meta,
               meta.final,
               stroke.type,
               age,
               age.disp,
               m.f,
               fu)
##removing vectors from working environment
rm(author, country, year, title, sample,descriptives, data.status, meta,stroke.type, age, age.disp,m.f,intervention,fu,foo,b)



#### Assigning study ID numbers####
df$study.id<-paste(paste(df$author, df$year, sep=", "), df$country, df$intervention , sep=" - ")
# unique(df$study.id) # activate to make sure the right number of unique studies have been included in table
df$study.id<-factor(df$study.id,
                  labels=1:length(dat$Year))



# organizing columns
df<-df[,c(which(names(df)=='study.id'),
            which(names(df)=='author'),
            which(names(df)=='year'),
            which(names(df)=='country'),
            which(names(df)=='intervention'),
            which(names(df)=='title'),
          which(names(df)=='meta'),
          which(names(df)=='meta.final'),
          which(names(df)=='stroke.type'),
          which(names(df)=='age'),
          which(names(df)=='age.disp'),
          which(names(df)=='m.f'),
          which(names(df)=='sample'),
            which(names(df)=='fu'),
            which(names(df)=='data.status'),
            which(names(df)=='descriptives'))]
typeof(df$intervention)
df$intervention<-factor(df$intervention,
                         labels = c("yes","no"))

###creating a study column with (author, year)
df$study<-paste(df$author, df$year, sep=", ")

###organizing columns within the dataframe
df<-df[,c(which(names(df)=='study.id'),
            which(names(df)=='author'),
            which(names(df)=='year'),
            which(names(df)=='country'),
            which(names(df)=='study'),
            which(names(df)=='intervention'),
          # which(names(df)=='title'), #activate these 6 lines when creating report tables
          # which(names(df)=='meta'),
          # which(names(df)=='meta.final'),
          # which(names(df)=='stroke.type'),
          # which(names(df)=='age'),
          # which(names(df)=='age.disp'),
          which(names(df)=='m.f'),
            which(names(df)=='sample'),
            which(names(df)=='fu'),
            which(names(df)=='data.status'),
            which(names(df)=='descriptives'))]

df$fu<-round(df$fu,1)

#### Assigning continents the country information column ####
levels(df$country)
# table(df$continent)

df$continent<-factor(countrycode(sourcevar = df[, "country"],
                                    origin = "country.name",
                                    destination = "continent"))


## Working out warnings and multinational collaborations
df$continent<-as.character(df$continent)
df$country<-as.character(df$country)
df[is.na(df$country),'country']='mockup country'## this line is necessary because the replacement lines below do not accept missing values in the index column (country)

df[df$country=='England','continent']='Europe'
df[df$country=='Scotland','continent']='Europe'
df[df$country=='Multinational','continent']='Multinational'
df[df$country=='USA','continent']='North America'
df[df$country=='Canada','continent']='North America' #9 conflicts
df[df$country=='Turkey','continent']='Europe'
df[df$country=='Chile'|df$country=='Brazil','continent']='South America'


df$sample<-as.numeric(df$sample)
summary(df$sample)
cog.eval<-dat$Cognitive.domain..cognitive.instrument...as.reported.by.authors....organize.alphabetically

df<-data.frame(df,cog.eval)

# names(df)
# cog.eval

### Separating instruments and domains ###

# the three lines below are an example
test_1<-"abc def,ghi klm"
test_2<-"abc, def ghi klm"
strsplit(test_1, "\\, |\\,| ") ## this line separates text by defined characters - in this case we are separating each time a comma, or a comma with a space next to it, or a space alone are found

df$cog.eval<-as.character(cog.eval)

###splitting column by the folowing characters "); " OR "(" OR ")"

a<-strsplit(df$cog.eval, "); |\\(|\\)")

###Matrix domains / instruments####
n.obs <- sapply(a, length) # get "length" for each element in a
seq.max <- seq(max(n.obs)) # create a sequence with the max number of observations
mat <- t(sapply(a, "[" , seq.max)) #

#saving into new dataframe
df<-data.frame(df,mat)

#naming the columns
x<-names(df)[which(names(df)=='X1'):
                length(df)]
domains<-paste("domain",seq(1:(length(mat[1,])/2)), sep = ".")
tests<-paste("test",seq(1:(length(mat[1,])/2)), sep = ".")
x[seq(1,length(mat[1,]),2)]=domains
x[seq(2,length(mat[1,]),2)]=tests
names(df)[which(names(df)=='X1'):
                length(df)]=x
rm(mat,dat,cog.eval,domains,n.obs,seq.max,tests,x,a) #removing unnecesary environment elements

# write an excel table for the instrument analysis
# write.csv(df, file="instruments.july.2018.csv", row.names = F)


#making two new datasets - one with the domans and the other with the tests
# names(df)
domains<-df[,c(which(names(df)=="study.id"):
                  which(names(df)=="continent"),
                seq(which(names(df)=='domain.1'),which(names(df)=='domain.14'),2))]

#organizing columns
# names(domains)
domains<-domains[,c(which(names(domains)=="study.id"),
                    which(names(domains)=='domain.1'):which(names(domains)=='domain.14'))]


#melting data to make a count of the domains
domains = melt(domains, id.vars = 'study.id')
# Warning message:
# "attributes are not identical across measure variables; they will be dropped"
# checkout explanation for this warning here
# https://stackoverflow.com/questions/25688897/reshape2-melt-warning-message


#adding useful variable descriptors
domains<-data.frame(df[,which(names(df)=="study.id"):
                      which(names(df)=="continent")],domains)


#deleting lines without data
domains = na.omit(domains)

#re-arranging columns
domains<-domains[,c(which(names(domains)=="study.id"),
                    which(names(domains)=="study.id.1"),
                    which(names(domains)=="author"):
                      which(names(domains)=="continent"),
                    which(names(domains)=="value"))]
names(domains)
table(domains$study.id==domains$study.id.1)

domains.all<-domains[,c(which(names(domains)=="study.id"),
                        which(names(domains)=="author"):
                          which(names(domains)=="value"))]
names(domains.all)


domains<-domains[,c(which(names(domains)=="study.id"),
                    which(names(domains)=="author"):
                      which(names(domains)=="value"))]


###trimming white spaces at the beggining and end of column
domains$value<-trimws(domains$value)

##sorting by domain
# domains<-domains[order(domains$value),]

library(tools)
##making values sentences with uppercase at the beggining
domains$value <- toTitleCase(domains$value)
##deleting domains that start with "And "
x<-grep("And ",domains$value)
domains[x,"value"]=gsub("And ", "",domains[x,"value"], fixed = TRUE)
###sorting by domain again
# domains<-domains[order(domains$value),]
domains$value<-factor(domains$value)
# levels(domains$value)
# domains<-domains[order(domains$study.id),]



###same steps as above but for cognitive instruments
tests<-df[,c(which(names(df)=="study.id"):
                  which(names(df)=="continent"),
                seq(which(names(df)=='test.1'),which(names(df)=='test.14'),2))]
tests<-tests[,c(which(names(tests)=="study.id"),
                    which(names(tests)=='test.1'):which(names(tests)=='test.14'))]
tests = melt(tests, id.vars = 'study.id')
tests<-data.frame(df[,which(names(df)=="study.id"):
                          which(names(df)=="continent")],tests)
tests = na.omit(tests)
tests<-tests[,c(which(names(tests)=="study.id"),
                    which(names(tests)=="study.id.1"),
                    which(names(tests)=="author"):
                      which(names(tests)=="continent"),
                    which(names(tests)=="value"))]
table(tests$study.id==tests$study.id.1)
tests.all<-tests[,c(which(names(tests)=="study.id"),
                        which(names(tests)=="author"):
                          which(names(tests)=="value"))]
tests<-tests[,c(which(names(tests)=="study.id"),
                    which(names(tests)=="author"):
                      which(names(tests)=="value"))]
tests$value<-trimws(tests$value)


# tests<-tests[order(tests$study.id),]
tests$value <- toTitleCase(tests$value)
tests$value<-factor(tests$value)
# levels(tests$value)

#joining the two tables with tests and domains
table(tests$study.id==domains$study.id)
test.domain<-data.frame(domains, tests['value'])
names(test.domain)
names(test.domain)[which(names(test.domain)=="value"):which(names(test.domain)=="value.1")]=c("domain", "instrument")
#making sure all good
test.domain[1:5,]



####Individual Instruments####
###splitting by the folowing characters ", " OR ", and " OR " and "
tests$value<-as.character(tests$value)
b<-tests$value
a<-strsplit(b, ", |\\, and |\\ and |\\ or ")

####Matrix with instruments only###
n.obs <- sapply(a, length)
seq.max <- seq(max(n.obs))
mat <- t(sapply(a, "[", i = seq.max))

####Migrating data from main dataset####
x<-data.frame(tests,b,mat)
x<-x[,c(1,which(names(x)=='X1'):length(x))]
x<-melt(x, id.vars = 'study.id')
x<-data.frame(tests,domains$value,domains$study.id,x)
x=na.omit(x)
###making sure id numbers are the same
table(x$study.id==x$domains.study.id)
table(x$study.id==x$study.id.1)
##re-organizing columns and cleaning blank spaces in rows
names(x)
x<-x[,c(which(names(x)=="study.id"):
          which(names(x)=="continent"),which(names(x)=="domains.value"),which(names(x)=="value.1"))]
x$value.1<-trimws(x$value.1)
names(x)[c(which(names(x)=='study.id'),
           which(names(x)=='domains.value'),which(names(x)=='value.1'))]=c('id','domain', 'instrument')


####SAVING RAW instrument names####
# a<-x[order(x$instrument),]


tests.raw<-x

## write a table for meta-analysis below --> this will be the starting table for longitudinal instrument scores data extraction
names(tests.raw)

# these lines are for filtered papers
# meta<-tests.raw[which(tests.raw$meta.final=='include'),c('id','author', 'year', 'country', 'intervention','title','instrument','sample','stroke.type','age','age.disp', 'm.f','fu')]

# all papers
# meta<-tests.raw[,c('id','author', 'year', 'country', 'intervention','title','instrument','sample','stroke.type','age','age.disp', 'm.f','fu')]
#
# meta<-meta[order(meta$id),]
# meta$m.f<-as.character(meta$m.f)
# m.f<-strsplit(meta$m.f, '/')
# n.obs <- sapply(m.f, length)
# seq.max <- seq(max(n.obs))
# mat <- t(sapply(m.f, "[", i = seq.max))
# meta<-data.frame(meta,mat)
# names(meta)[c(which(names(meta)=='X1'), which(names(meta)=='X2'))]=c('male','female')
# meta=meta[,which(names(meta)!='m.f')]
# str(meta$male)
# meta$female=as.character(meta$female)
# meta[which(is.na(meta$female)),'female']='extract'
#
# write.csv(meta, '2018-08-18.meta.all.scoping.csv',row.names = F)
# rm(meta)
#
# length(unique(meta$id))-6

# exclude papers that did not meet inclusion for scoping review but did for the meta-analysis
df[df$data.status=='exclude',"study.id"]=NA
df=df[!is.na(df$study.id),]


####Cleaning instrument names####

####code for manual instrument categories below

clean.inst=function(dirty.tests){
  
  x=dirty.tests
  b=x$instrument
  
  ###the lines below search for key words within the data matrix and writes the code for recoding it
  
  ###Example
  paste0('x[',paste0("x=='",unique(x[grep(paste(
    'Omis',
    'Commis',
    sep = '|'), x$domain),'domain']),"'", collapse="|"),']=')
  
  b[b=="1-Min Animal Naming Test" |
      b=="1-Min Semantic Fluency Tests with Animals" |
      b=="Animal Fluency Test" |
      b=="Animal Naming Fluency Test" |
      b=="Animal Naming Test" |
      b=="Animals"]=
    "Fluency Test [animals]"
  
  b[b=="15-Item BNT"|
      b=="15-Item Subset of the BNT"]=
    "BNT [15-item version]"
  
  b[b=="60-Item BNT"]=
    "BNT [60-item version]"
  
  b[b=="10 Word List Learning"|
      b=="10-Word List"|
      b=="10-Word List Learning Task"|
      b=='Learning a Series of 10 Unrelated Words'|
      b=="10-Word Memory Test"|
      b=="10-Word Test"]=
    "10-Word List Learn"
  
  b[b=="5-Word Repetition"|
      b=="5-Word Repetition with 3 Min Delay"]=
    "5-Word Rep"
  
  b[b=="ADT"]="Auditory Detection"
  
  b[b=="Aphasia Scale [from ADAS-Cog]"]=
    "ADAS-Cog [aphasia Scale]"
  
  b[b=="Orientation [from ADAS-Cog]"]=
    "ADAS-Cog [orientation]"
  
  b[b=="Arithmetic"|
      b=="Arithmetic [from WAIS-R]"|
      b=="Arithmetic Problem Solving"]=
    "Arithmetic"
  
  b[b=="Calculation"|
      b=="Calculation"|
      b=="Calculation Test"]=
    "Calculation Test"
 
   b[b=='DCT'|
      b=='Digit Cancellation']=
    'Digit Cancel Task'
  
  b[b=="Category"|
      b=="Category Fluency"]=
    "Fluency Test [categories]"
  
  b[b=="Categorical Verbal Fluency [animals-1-Min]"|
      b=="Category Fluency [animal Naming]"|
      b=="Category Fluency [animals]"|
      b=="Category of Animals"]=
    "Fluency Test [animals]"
  
  b[b=="Category Fluency [animals & Food Subtasks]"|
      b=="Category Fluency [animals & Professions]"|
      b=="Category Naming [fruits-Vegetables & Fish]"]=
    "Fluency Test [categories]"
  
  b[b=="CBS"|
      b=="CBT"]=
    "Corsi Blocks Test"
  
  b[grep('CNT ',b)]='CNT battery [vcpt + acpt+ dsf & b + vsf & b + avlt & vrt]'
  
  
  b[b=="CDT"|
      b=="Clock Drawing"|
      b=="Clock Drawing"|
      b=="Clock Task" ]=
    "Clock Drawing Test"
  
  b[b=="CNS"]=
    "CNS-Vital Signs Test"
  
  b[b=="Construction of the MMSE"]=
    "MMSE [Construction Item]"
  
  b[b=="Memorize Three Words [from MMSE]"]=
    "MMSE [Three-Word Memory Item]"
  
  b[b=="Orientation Items of the MMSE"]=
    "MMSE [Orientation Items]"
  
  b[b=="Spatial Orientation [from MMSE]"]=
    "MMSE [Spatial Orientation Item]"
  
  b[b=="BIT-Conventional Subtest"]=
    "BIT [Conventional Subtest]"
  
  b[b=="Copying Task [from BIT]"]=
    "BIT [Copying Task]"
  
  b[b=="Picture Scanning [from BIT]"]=
    "BIT [Picture Scanning]"
  
  b[b=="Star Cancellation [from BIT]"]=
    "BIT [Star Cancellation]"
  
  b[b=="Copy Cube"]=
    "Copy a Cube"
  b[b=="Construction Ability"| 
      b=="Constructional Ability"]=
    'Construction Abilities'
  
  
  b[b=="CP"]=
    "Clock Perception"
  
  b[b=="Depression Anxiety Stress Scale"]=
    "DASS"

  
  b[b=="Delayed Recall [from RAVLT]"|
      b=="Delayed Recall Test"|
      b=="DR"|
      b=="DR [from WMS-III]"]=
    "DR"
  
  b[b=="DS [from WAIS - III]"|
      b=="DS [from WAIS-II]"|
      b=="DS [from WAIS-III]"|
      b=="DS [from WAIS-IV]"|
      b=="DS [from WAIS-R]"|
      b=="DS [from WAIS]"|
      b=="DS [from WMS-R]"|
      b=="DS"]=
    "Digit sp"
  
  b[b=='DSB'|
    b=="DSB [from WAIS-III-R]"|
      b=="DSB [from WAIS-III]"]=
    "Digit sp-b"
  
  b[b=="DSB & DSF" |
      b=="DSB & DSF [from WMS-III]"|
      b=="DSF & DSB" |
      b=="DSF & DSB [from WMS]"
    ]=
    "Digit sp-f & b"
  
  b[b=="DSF"|
      b=="DSF [from WAIS-III]"|
      b=="DSF [from WAIS]"|
      b=="DSF [wais-III-R]"]=
    "Digit sp-f"
  
  b[b=="Go-No-Go Task"|
      b=="Go-No-Go Test"|
      b=="Go-No-Go Test [initiation & Response-Inhibition]"]=
    "Go-No-Go Test"
  
  b[b=="GP"|
      b=="GPT"]=
    "Grooved Peg Test"
  
  b[b=="I-Flex"|
      b=="I-Flex [a Short Form of the EXIT]"]=
    "I-Flex"
  
  b[b=="IQCODE-SF"|
      b=="IQCODE-SS"]=
    "IQ-CODE"
  
  b[b=="JLO"|
      b=="JLO [sf]"]=
    "Judg Line Orient"
  
  b[b=="LCT"|
      b=="LCT - For Right Hemisphere Stroke"|
      b=="Letter Cancellation"|
      b=="Letter Cancellation Task"]=
    "Letter Cancellation"
  
  b[b=="1-Min Phonemic Verbal Fluency [letters F-a-S]"|
      b=="1-Minute Word Naming Trial [letters C-F-L]"|
      b=="Letter Fluency"|
      b=="Letter Fluency [words with P]"|
      b=="Letters F-a-S"]=
    "Fluency Test [Letters]"
  
  b[b=="LM [from WMS-III]"|
      b=="LM [from WMT-R]"|
      b=="LM I & LM II [from WMS-R]"|
      b=="LM-I & LM-II"|
      b=='LM']=
    "Log Mem"
  
  b[b=="LM [dr]"|
      b=="LM II [from WMS-R]"]=
    "Log Mem II"
  
  b[b=="LM I [from WMS-R]"]=
    "Log Mem I"
  
  b[b=="MMSE [acute & Chronic Stages Only]"|
      b=="MMSE [bengali Version]"|
      b=="MMSE [brief Version]"|
      b=="MMSE [chinese Version]"|
      b=="MMSE [italian Telephone Version]"|
      b=="MMSE [korean Version]"|
      b=="MMSE [thai Version]"]=
    "MMSE"
  
  b[b=="Modified BVRT"]=
    "BVRT [modified Version]"
  
  b[b=="or TMT a"]=
    "TMT A"
  
  b[b=="Phonemic Fluency"|
      b=="Phonemic Fluency [letter S]"|
      b=="Phonemic Fluency [letters F-a-S]"|
      b=="Phonemic Fluency Task"|
      b=="Phonemic Test"|
      b=="Phonetic Fluency"]=
    "Fluency Test [phonemic]"
  
  b[b=="Semantic Fluency"]=
    "Fluency Test [semantic]"
  
  b[b=="Verbal Fluency"|
      b=="Verbal Fluency [animal & Food Categories]"|
      b=="Verbal Fluency [animals]"|
      b=="Verbal Fluency [animals] [from DAS]"|
      b=="Verbal Fluency [letters F-a-S & B-H-R]"|
      b=="Verbal Fluency Test"|
      b=="VF - Categorical [animals]" |
      b=="VF - Phonological [words]" |
      b=="VFT"|
      b=="VFT [generation]"|
      b=="Word Fluency"|
      b=="Word Fluency [phonemic & Semantic]"]=
    "Fluency Test [verbal]"
  
  b[b=="Picture Completion" |
      b=="Picture Completion [from WAIS-R]"]=
    "Picture Completion"
  
  b[b=="R-CAMCOG"]=
    "CAMCOG"
  
  b[b=="RAPM [sf]"]=
    "RAPM"
  
  b[b=="WCFST"|
      b=="Weigl CST"|
      b=="Weigl Sorting Test"]=
    "Weigl Color Sorting Test"

  # x$instrument<-tolower(b)
x$instrument<-b


return(x$instrument)

}

####testing function
clean.inst(tests.raw)[grep('Dig',clean.inst(tests.raw)) ]

####CLEAN instrument names####
tests.clean<-tests.raw
tests.clean$instrument<-clean.inst(tests.raw)

very.clean.inst=function(dirty.tests){
####Organizing instrument names in broader categories####
  
  x=dirty.tests
  b=x$instrument
  
  ###the lines below search for key words within the data matrix and writes the code for recoding it
  
  ###Example
  paste0('x[',paste0("x=='",unique(x[grep(paste(
    'Omis',
    'Commis',
    sep = '|'), x$domain),'domain']),"'", collapse="|"),']=')
  
  b[b=="1-Min Animal Naming Test" |
      b=="1-Min Semantic Fluency Tests with Animals" |
      b=="Animal Fluency Test" |
      b=="Animal Naming Fluency Test" |
      b=="Animal Naming Test" |
      b=="Animals"]=
    "Fluency Test [animals]"
  
  b[b=="15-Item BNT"|
      b=="15-Item Subset of the BNT"]=
    "BNT [15-item version]"
  
  b[b=="60-Item BNT"]=
    "BNT [60-item version]"
  
  b[b=="10 Word List Learning"|
      b=="10-Word List"|
      b=="10-Word List Learning Task"|
      b=='Learning a Series of 10 Unrelated Words'|
      b=="10-Word Memory Test"|
      b=="10-Word Test"]=
    "10-Word List Learn"
  
  b[b=="5-Word Repetition"|
      b=="5-Word Repetition with 3 Min Delay"]=
    "5-Word Rep"
  
  b[b=="ADT"]="Auditory Detection"
  
  b[b=="Aphasia Scale [from ADAS-Cog]"]=
    "ADAS-Cog [aphasia Scale]"
  
  b[b=="Orientation [from ADAS-Cog]"]=
    "ADAS-Cog [orientation]"
  
  b[b=="Arithmetic"|
      b=="Arithmetic [from WAIS-R]"|
      b=="Arithmetic Problem Solving"]=
    "Arithmetic"
  
  b[b=="Calculation"|
      b=="Calculation"|
      b=="Calculation Test"]=
    "Calculation Test"
  
  b[b=='DCT'|
      b=='Digit Cancellation']=
    'Digit Cancel Task'
  
  b[b=="Category"|
      b=="Category Fluency"]=
    "Fluency Test [categories]"
  
  b[b=="Categorical Verbal Fluency [animals-1-Min]"|
      b=="Category Fluency [animal Naming]"|
      b=="Category Fluency [animals]"|
      b=="Category of Animals"]=
    "Fluency Test [animals]"
  
  b[b=="Category Fluency [animals & Food Subtasks]"|
      b=="Category Fluency [animals & Professions]"|
      b=="Category Naming [fruits-Vegetables & Fish]"]=
    "Fluency Test [categories]"
  
  b[b=="CBS"|
      b=="CBT"]=
    "Corsi Blocks Test"
  
  b[grep('CNT ',b)]='CNT battery [vcpt + acpt+ dsf & b + vsf & b + avlt & vrt]'
  
  
  b[b=="CDT"|
      b=="Clock Drawing"|
      b=="Clock Drawing"|
      b=="Clock Task" ]=
    "Clock Drawing Test"
  
  b[b=="CNS"]=
    "CNS-Vital Signs Test"
  
  b[b=="Construction of the MMSE"]=
    "MMSE [Construction Item]"
  
  b[b=="Memorize Three Words [from MMSE]"]=
    "MMSE [Three-Word Memory Item]"
  
  b[b=="Orientation Items of the MMSE"]=
    "MMSE [Orientation Items]"
  
  b[b=="Spatial Orientation [from MMSE]"]=
    "MMSE [Spatial Orientation Item]"
  
  b[b=="BIT-Conventional Subtest"]=
    "BIT [Conventional Subtest]"
  
  b[b=="Copying Task [from BIT]"]=
    "BIT [Copying Task]"
  
  b[b=="Picture Scanning [from BIT]"]=
    "BIT [Picture Scanning]"
  
  b[b=="Star Cancellation [from BIT]"]=
    "BIT [Star Cancellation]"
  
  b[b=="Copy Cube"]=
    "Copy a Cube"
  b[b=="Construction Ability"| 
      b=="Constructional Ability"]=
    'Construction Abilities'
  
  
  b[b=="CP"]=
    "Clock Perception"
  
  b[b=="Depression Anxiety Stress Scale"]=
    "DASS"
  
  
  b[b=="Delayed Recall [from RAVLT]"|
      b=="Delayed Recall Test"|
      b=="DR"|
      b=="DR [from WMS-III]"]=
    "DR"
  
  b[b=="DS [from WAIS - III]"|
      b=="DS [from WAIS-II]"|
      b=="DS [from WAIS-III]"|
      b=="DS [from WAIS-IV]"|
      b=="DS [from WAIS-R]"|
      b=="DS [from WAIS]"|
      b=="DS [from WMS-R]"|
      b=="DS"]=
    "Digit sp"
  
  b[b=='DSB'|
    b=="DSB [from WAIS-III-R]"|
      b=="DSB [from WAIS-III]"]=
    "Digit sp-b"
  
  b[b=="DSB & DSF" |
      b=="DSB & DSF [from WMS-III]"|
      b=="DSF & DSB" |
      b=="DSF & DSB [from WMS]"
    ]=
    "Digit sp-f & b"
  
  b[b=="DSF"|
      b=="DSF [from WAIS-III]"|
      b=="DSF [from WAIS]"|
      b=="DSF [wais-III-R]"]=
    "Digit sp-f"
  
  b[b=="Go-No-Go Task"|
      b=="Go-No-Go Test"|
      b=="Go-No-Go Test [initiation & Response-Inhibition]"]=
    "Go-No-Go Test"
  
  b[b=="GP"|
      b=="GPT"]=
    "Grooved Peg Test"
  
  b[b=="I-Flex"|
      b=="I-Flex [a Short Form of the EXIT]"]=
    "I-Flex"
  
  b[b=="IQCODE-SF"|
      b=="IQCODE-SS"]=
    "IQ-CODE"
  
  b[b=="JLO"|
      b=="JLO [sf]"]=
    "Judg Line Orient"
  
  b[b=="LCT"|
      b=="LCT - For Right Hemisphere Stroke"|
      b=="Letter Cancellation"|
      b=="Letter Cancellation Task"]=
    "Letter Cancellation"
  
  b[b=="1-Min Phonemic Verbal Fluency [letters F-a-S]"|
      b=="1-Minute Word Naming Trial [letters C-F-L]"|
      b=="Letter Fluency"|
      b=="Letter Fluency [words with P]"|
      b=="Letters F-a-S"]=
    "Fluency Test [Letters]"
  
  b[b=="LM [from WMS-III]"|
      b=="LM [from WMT-R]"|
      b=="LM I & LM II [from WMS-R]"|
      b=="LM-I & LM-II"|
      b=='LM']=
    "Log Mem"
  
  b[b=="LM [dr]"|
      b=="LM II [from WMS-R]"]=
    "Log Mem II"
  
  b[b=="LM I [from WMS-R]"]=
    "Log Mem I"
  
  b[b=="MMSE [acute & Chronic Stages Only]"|
      b=="MMSE [bengali Version]"|
      b=="MMSE [brief Version]"|
      b=="MMSE [chinese Version]"|
      b=="MMSE [italian Telephone Version]"|
      b=="MMSE [korean Version]"|
      b=="MMSE [thai Version]"]=
    "MMSE"
  
  b[b=="Modified BVRT"]=
    "BVRT [modified Version]"
  
  b[b=="or TMT a"]=
    "TMT A"
  
  b[b=="Phonemic Fluency"|
      b=="Phonemic Fluency [letter S]"|
      b=="Phonemic Fluency [letters F-a-S]"|
      b=="Phonemic Fluency Task"|
      b=="Phonemic Test"|
      b=="Phonetic Fluency"]=
    "Fluency Test [phonemic]"
  
  b[b=="Semantic Fluency"]=
    "Fluency Test [semantic]"
  
  b[b=="Verbal Fluency"|
      b=="Verbal Fluency [animal & Food Categories]"|
      b=="Verbal Fluency [animals]"|
      b=="Verbal Fluency [animals] [from DAS]"|
      b=="Verbal Fluency [letters F-a-S & B-H-R]"|
      b=="Verbal Fluency Test"|
      b=="VF - Categorical [animals]" |
      b=="VF - Phonological [words]" |
      b=="VFT"|
      b=="VFT [generation]"|
      b=="Word Fluency"|
      b=="Word Fluency [phonemic & Semantic]"]=
    "Fluency Test [verbal]"
  
  b[b=="Picture Completion" |
      b=="Picture Completion [from WAIS-R]"]=
    "Picture Completion"
  
  b[b=="R-CAMCOG"]=
    "CAMCOG"
  
  b[b=="RAPM [sf]"]=
    "RAPM"
  
  b[b=="WCFST"|
      b=="Weigl CST"|
      b=="Weigl Sorting Test"]=
    "Weigl Color Sorting Test"
  
  
  
  ######CLEANING FURTHER (GETTING RID OF SUBTESTS)
  
b[b=='ADAS [four Pictures]'|
    b=='ADAS [recall of Four Pictures]'|
    b=='ADAS-Cog'|
    b=='ADAS-Cog [aphasia Scale]'|
    b=='ADAS-Cog [orientation]']='ADAS-Cog'


b[b=="BDT"|
    b=="BDT [from WAIS-II]"|
    b=="BDT [from WAIS-III]"|
    b=="BDT [from WAIS-R]"|
    b=="BDT [from WAIS]"]=
  "Block Design Test"

b[b=='BIT'|
    b=='BIT [Conventional Subtest]'|
    b=='BIT [Copying Task]'|
    b=='BIT [Picture Scanning]'|
    b=='BIT [Star Cancellation]']='BIT'


b[b=="BNT"|
    b=="BNT [15-item version]"|
    b=="BNT [60-item version]"|
    b=="BNT [french Version]" |
    b=="BNT [modified Version]"|
    b=="BNT [sf]"]=
  "Boston Naming Test"

b[b=='BVRT [modified Version]'|
    b=='BVRT [sf]'|
    b=='BVRT-SF']='BVRT'


b[b=='CAMCOG'|
    b=='CAMCOG-R [section B Only]']='CAMCOG'


b[b=='Copying 4 Geometric Figures'|
    b=='Copying Designs'|
    b=='Copying Simple Figures'|
    b=='Figure Copying [from CERAD]'|
    b=='Time of Copying Task']='Figure Copying'

b[b=="COWAT [total Number of Words]"]=
  "COWAT"

b[b=='CVLT'|
    b=='CVLT-II']='CVLT'


b[b=='Digit Symbol'|
    b=='Digit Symbol [coding Subtest from WAIS-III]'|
    b=='Digit Symbol [from WAIS-II]'|
    b=='Digit Symbol [from WAIS]']='Digit Symbol'


b[b=="Digit sp"|
    b=="Digit sp-b" |
    b=='Digit sp-f & b'|
    b=="Digit sp-f"]="Digit sp"

b[b=="Fluency Test [animals]"|
    b=="Fluency Test [categories]"|
    b=="Fluency Test [Letters]"|
    b=="Fluency Test [phonemic]"|
    b=="Fluency Test [semantic]"|
    b=="Fluency Test [verbal]"]=
  "Verbal Fluency"

b[b=="Geometric Figures [dr]" |                                                           
  b=="Geometric Figures [ir]" ]=
  "Geometric Figures"

b[b=='IQ-CODE'|
    b=='IQCODE']='IQ-CODE'

b[b=="KSNAP [four Letter Word Subtest]"|                                                        
    b=="KSNAP [gestalt Closure Subtest]"|                                                         
    b=="KSNAP [mental Status Subtest]"]= 
  'KSNAP'

b[b=="Log Mem" |
    b=="Log Mem I"|
    b=="Log Mem II"]=
  "Log Mem"
b[b=="Luria's Sequences"|                                                                      
  b=="Lurias Premotor Sequences" ]=
  "Luria's sequences"

b[b=='MDRS'|
    b=='MDRS [initiation-Perseveration]'|
    b=='Perseveration [from MDRS]'|
    b=='Subtests of the MDRS']="MDRS"

b[b=="Mental Control [from WMS-III]" |
    b=="Mental Control [from WMS]"]=
  "Mental Control"

b[b=='MMSE'|
    b=='MMSE [Construction Item]'|
    b=='MMSE [Orientation Items]'|
    b=='MMSE [Spatial Orientation Item]'|
    b=='MMSE [Three-Word Memory Item]']='MMSE'

b[b=='MoCA'|
    b=='MoCA [attention Subtest]'|
    b=='Rythms Subtest [from MoCA]']='MoCA'

b[b=="Naming"|
    b=="Naming [from CERAD]"|
    b=="Naming 5 Objects"|
    b=="Naming 5 Parts of Object"|
    b=="Naming of Pictures of Objects"]=
  "Naming"

b[b=='PASAT'|
    b=='PASAT [2 Slowest Trials]']=
  'PASAT'

b[b=="RAVLT"|
    b=="RAVLT [dr]"|                                                                              
    b=="RAVLT [ir-DR-DRec]"|
    b=="RAVLT Total Number of Learnt Words from A1-A5"]=
  'RAVLT'

b[b=="RCPM"|                                                                                    
  b=="RCPM [set A]"]=
  'RCPM'

b[b=="ROCF"|
    b=="ROCF [copy Score]"|
    b=="ROCF [delay Score]"]=
  'ROCF'

b[b=='RBANS'|
    b=='RBANS [naming & Coding Subtests]'|
    b=='Recognition Memory Test [from RBANS]']=
  'RBANS'


b[b=="Similarities [from WAIS-III]"|
    b=="Similarities Subtest [from WAIS-II]"|
    b=="Similarities Subtests [from WAIS-R]"|
    b=="Similarity Test"]=
  "Similarities"

b[b=="SIS"|
    b=="SIS Memory"]=
  'SIS'

b[b=="Spontaneous Speech"]=
  "Spontaneous Speech Fluency"

b[b=="Stroop  [modified] [set B Minus A]"|
    b=="Stroop [color Naming]"|
    b=="Stroop [interference Minus Naming]"|
    b=="Stroop [interference Score]"|
    b=="Stroop [interference Subtest]"|
    b=="Stroop [modified Version]" |
    b=="Stroop [scwt Parts 1 & 2]"|
    b=="Stroop [victoria Version]"|
    b=="Stroop Animal Test"|
    b=="Stroop Test 1 & 2 [victoria Version]"|
    b=="Stroop-CWIT [from the D-KEFS]"]=
  "Stroop"

b[grep('Two Computerized Attention',b)]=
  "Attention Subtests [from TAPr]"

b[b=="SR"|
  b=="Story Recall [from WMS]"|  
  b=="SR [ir & DR]"|
  b=="Story [dr]"|                                                                              
  b=="Story [ir]"|
  b=="Story Recall [from RBMT]"|
  b=="Story Recall [ir & DR]"]=
  'Story Recall'

b[b=="TMT [interference Score]"|
    b=="TMT [set B Minus A]"|
    b=="TMT [set-Shifting]"|
    b=="TMT [sets A1-A2 & B]"|
    b=="TMT a"|
    b=="TMT A"|
    b=="TMT a & B"|
    b=="TMT B"]=
  "TMT"
b[b=="Token Test [modified Version-Time to Complete]"|
    b=="Token Test [sf]"]=
  "Token Test"

b[b=="Vocabulary [from WAIS]"| 
    b=="Vocabulary Subtest [from WAIS-R]"]=
  "Vocabulary [from WAIS]"

b[b=="VR"|
    b=="VR [copy Score] [from WMS-R]"|
    b=="VR [from WMS-III]"|
    b=="VR [from WMS]"|
    b=="VR [i - II & DRec] [from WMS-III]"|
    b=="VR [ir-DR & DRec - From WMS-R]"|
    b=="VR [ir-DR & DRec] [from WMS-R]"|
    b=="VR Copy [from WMS-III]"|
    b=="VR Copy Task [from WMS-R]"|
    b=="VR I & VR II [from WMS-R]"]=
  "Visual Reproduct"
b[b=="WAB"|                                                   
  b=="WAB [finger Gnosis & Stereognosis]"]=
  'WAB'

b[b=="WCST [modified Version]"|
    b=="WCST [nelson Version]"|
    b=="WCST"]=
  "Wisconsin Card Sort"


b[b=="Writing Task"]=
  "Writing"

b[b=="Word List Memory [or Flash Memory]"]=
  "Word List Mem"

b[b=="WLR [ir-DR & DRec]"|
    b=="WLR [ir-DR & DRec]"|
    b=="WLR [ir-DR & DRec]" |
    b=="Word List Recall [or Learning Period]"|
    b=="Word List Recognition [or Recall of Knowledge]"]=
  "Word List Recall & Recog"

b[b=="WLL [from CERAD]"|
    b=="WLL [ir & DR from CERAD]"|
    b=="WLL"]=
  "Word List Learn"


# x$instrument<-tolower(b)

x$instrument<-b
return(x$instrument)


}

###Making sure the function works
very.clean.inst(tests.raw)[grep('Digit',very.clean.inst(tests.raw))]

####VERY clean instrument names####
tests.cleaner=tests.raw
tests.cleaner$instrument<-very.clean.inst(tests.raw)

sort(unique(tests.cleaner$instrument))

####RESULTS - instruments####
table1<-data.frame(table(tests.cleaner$instrument))
table1<-table1[order(table1$Freq, decreasing = T),]
table1.trim<-table1[1:20,]
tests.result<-table1
tests.result.short<-table1.trim
rm(table1, table1.trim,a,mat,domains.all,domains,tests.all,tests)


####DOMAINS####
###splitting by the folowing characters ", " OR ", and " OR " and "

x<-as.character(test.domain$domain)
x<-strsplit(x, ", |\\, and |\\ and |\\ or ")

####Creating matrix domains only####
n.obs <- sapply(x, length)
seq.max <- seq(max(n.obs))
x <- t(sapply(x, "[", i = seq.max))

####Migrating data from main dataset####
x<-data.frame(test.domain,x)
x<-x[,c(1,which(names(x)=='X1'):length(x))]
x<-melt(x, id.vars = 'study.id')
x<-data.frame(test.domain["study.id"],x['study.id'],x['value'],test.domain[2:length(test.domain)])
x=na.omit(x)

###making sure id numbers are the same
table(x$study.id==x$study.id.1)

##re-organizing columns and cleaning blank spaces in rows
names(x)
x<-x[,c(which(names(x)=="study.id"),
          which(names(x)=="study"):which(names(x)=="continent"),which(names(x)=="value"),which(names(x)=="instrument"))]

x$value<-trimws(x$value)

names(x)[c(which(names(x)=='study.id'),
           which(names(x)=='value'))]=c('id','domain')


###RAW domains####
# x<-x[order(x$domain),]
domains.raw<-x

####FUNCTION - Domain cleaning (raw categories)####
clean.domains<-function(dirty.data){

    #input
    x=dirty.data
    

   ###studies with executive functions subdivided in categories
b=x[which(with(x,study=="Rand et al., 2010" |
               study=="Mehrabian et al., 2015" |
               study=="Poulin et al., 2017" |
               study=="Rozental-Iluz et al., 2016"|
               study=="Sachdev et al., 2009")),]

# getting positions
a<-which(with(b,study=='Mehrabian et al., 2015' &
                domain=="Set Shifting"|
                study=='Poulin et al., 2017' &
                domain=="Attention"|
                study=='Poulin et al., 2017' &
                domain=="Cognitive Flexibility"|
                study=='Poulin et al., 2017' &
                domain=="Inhibition"|
                study=='Rand et al., 2010' &
                domain=="Working Memory"|
                study=='Rand et al., 2010' &
                domain=="Delayed Recall"|
                study=='Rand et al., 2010' &
                domain=="Divided Attention"|
                study=='Rand et al., 2010' &
                domain=="Long-Term Memory"|
                study=='Rand et al., 2010' &
                domain=="Cognitive Flexibility"|
                study=='Rozental-Iluz et al., 2016' &
                domain!="EF - Information Seeking" &
                domain!="EF - Initiation" &
                domain!="EF - Visuomotor Scanning"|
                study=="Sachdev et al., 2009"&
                domain=="Verbal Fluency"))

# adding prefix "EF - "
b[a,'domain']=paste0("EF - ", b[a,'domain'])

# replacing
x[which(with(x,study=="Rand et al., 2010" |
               study=="Mehrabian et al., 2015" |
               study=="Poulin et al., 2017" |
               study=="Rozental-Iluz et al., 2016"|
               study=="Sachdev et al., 2009")),]=b

x[x=="Executive"|
    x=="Executive Functioning"|
    x=="Executive Functions"|
    x=="Executive Functions"|
    x=="Executive Reasoning"]="Executive Function"


x[x=='Alternating Attention'|
    x=='Attention'|
    x=='Attention [psychomotor Speed]'|
    x=='Attention Omission'|
    x=='Attention-Phasic Alert'|
    x=='Attentional Tasks'|
    x=='Attentiveness'|
    x=='Divided Attention'|
    x=='Focused Visual Attention'|
    x=='General Visual Attention'|
    x=='Phasic Attention'|
    x=='Selective Attention'|
    x=='Sustained Attention'|
    x=='Switching of Attention'|
    x=='Tonic Attention'|
    x=='Visual Inattention'|
    x=='Sustained'|
    x=='Selective'|
    x=='Divided']="Attention"


x[x=='Behavior'|
    x=='Behavioral Change'|
    x=='Behavioral Improvement']="Behavior"

x[x=='Constructing'|
    x=='Construction'|
    x=='Construction Skills'|
    x=='Constructional'|
    x=='Constructional Function'|
    x=='Constructional Praxis'|
    x=='Constructive Praxis'|
    x=='Visuospatial Construction'|
    x=='Visuoconstruction'|
    x=='Visuoconstructive Ability'|
    x=='Visuoconstuctive Functions']="Construction skills"


x[x=='Dementia'|
    x=='Dementia Diagnosis'|
    x=='Dementia Diagnostic Instrument']="Dementia"

x[x=='EF - Attention'|
    x=='EF - Cognitive Flexibility'|
    x=='EF - Completion'|
    x=='EF - Delayed Recall'|
    x=='EF - Divided Attention'|
    x=='EF'|
    x=='EF - Attention Switching'|
    x=='EF - Information Seeking'|
    x=='EF - Inhibition'|
    x=='EF - Initiation'|
    x=='EF - Learning'|
    x=='EF - Mental Flexibility'|
    x=='EF - Psychomotor Performance'|
    x=='EF - Response Inhibition'|
    x=='EF - Speed'|
    x=='EF - Verbal Fluency'|
    x=='EF - Visuomotor Scanning'|
    x=='EF - Judgment'|
    x=='EF - Long-Term Memory'|
    x=='EF - Planning'|
    x=='EF - Problem Solving'|
    x=='EF - Set Shifting'|
    x=='EF - Working Memory']="Executive Function"


x[x=='Cognition'|
    x=='Cognitive'|
    x=='Cognitive Abilities'|
    x=='Cognitive Ability'|
    x=='Cognitive Decline'|
    x=='Cognitive Disability'|
    x=='Cognitive Disorders'|
    x=='Cognitive Dysfunction'|
    x=='Cognitive Function'|
    x=='Cognitive Functioning'|
    x=='Cognitive Functions'|
    x=='Cognitive Impact Resulting from Stroke'|
    x=='Cognitive Impairment'|
    x=='Cognitive Mental Status'|
    x=='Cognitive Performance'|
    x=='Cognitive State'|
    x=='Cognitive Status'|
    x=='Degree of Cognitive Abilities'|
    x=='General Cognition'|
    x=='General Cognitive Function'|
    x=='General Cognitive Functioning'|
    x=='General Cognitive Functions'|
    x=='Global Cognition'|
    x=='Global Cognitive Assessment'|
    x=='Global Cognitive Function'|
    x=='Global Cognitive Functioning'|
    x=='Global Cognitive Functions'|
    x=='Global Cognitive Impairment'|
    x=='Global Cognitive Performance'|
    x=='Global Cognitive Status'|
    x=='Mild Cognitive Impairment'|
    x=='Moderate Cognitive Dysfunction'|
    x=='Recovery of Cognition'|
    x=='Neuropsychological Functions'|
    x=='Neuropsychological Impairment'|
    x=='Mental Status'|
    x=='State of Higher Mental Functions']="General Cognition"


x[x=='Premorbid Ability'|
    x=='Premorbid Cognitive Function'|
    x=='Premorbid Cognitive Impairment']="Premorbid Cognitive Status"

x[x=='Language'|
    x=='Language - Object Naming from Line Drawing'|
    x=='Language [judged by Neuropsychologist]'|
    x=='Language Abilities'|
    x=='Language Abilities - Speech Fluency'|
    x=='Language Ability'|
    x=='Language-Auditory Comprehension'|
    x=='Language-Confrontation Naming'|
    x=='Language-Phonemic Verbal Fluency'|
    x=='Language-Picture Naming'|
    x=='Language-Reading Capacity'|
    x=='Language-Sentence Comprehension'|
    x=='Cognitive Communicative Skills'|
    x=='Verbal'|
    x=='Verbal Ability'|
    x=='Verbal Expression'|
    x=='Verbal Fluency'|
    x=='Verbal Function'|
    x=='Verbal Processing'|
    x=='Letter Fluency Tasks'|
    x=='Lexical Fluency'|
    x=='Phonological Fluency'|
    x=='Semantic Fluency'|
    x=='Narrative Speech'|
    x=='Naming'|
    x=='Naming Skills'|
    x=='Word Naming']="Language Skills"


x[x=='Auditory Memory'|
    x=='Delayed Memory'|
    x=='Episodic Memory'|
    x=='Episodic Verbal Memory'|
    x=='Everyday Memory Problems'|
    x=='General Long-Term Memory'|
    x=='Immediate Memory'|
    x=='Long Term Memory'|
    x=='Long-Term Memory'|
    x=='Long-Term Verbal Memory'|
    x=='Memory'|
    x=='Memory - Short Term'|
    x=='Memory Registration'|
    x=='Memory Self-Efficacy'|
    x=='Memory-Long Term'|
    x=='Memory-Long-Term Memory'|
    x=='Memory-Short Term'|
    x=='Memory-Short-Term Verbal'|
    x=='Non-Verbal Visual Memory'|
    x=='Nonverbal Memory'|
    x=='Short-Term Memory'|
    x=='Short-Term Memory Recall'|
    x=='Short-Term Verbal Memory'|
    x=='Verbal Declarative Memory'|
    x=='Verbal Memory'|
    x=='Verbal Short Term Memory'|
    x=='Verbal Working Memory'|
    x=='Visual Memory'|
    x=='Visual Memory [nonverbal]'|
    x=='Visual Memory [or Visuospatial Functions]'|
    x=='Visual Memory Functions'|
    x=='Visuospatial Memory'|
    x=='Word Memory'|
    x=='Working Memory'|
    x=='Verbal']="Memory"

x[x=='Awareness of Visuospatial Neglect'|
    x=='Neglect'|
    x=='Personal Neglect'|
    x=='Spatial Neglect Severity'|
    x=='Unilateral Neglect'|
    x=='Unilateral Visual Neglect'|
    x=='Unilateral Visuospatial Neglect'|
    x=='Visual Neglect'|
    x=='Sensory']="Neglect"

x[x=='Neurologic Function'|
    x=='Neurological Function']="Neurologic Function"



x[x=='Abstract Reasoning'|
    x=='Nonverbal Reasoning'|
    x=='Reasoning']="Reasoning Skills"

x[x=='Learning'|
    x=='New Learning'|
    x=='Verbal Learning']="Learning Skills"

x[x=='Auditory Comprehension'|
    x=='Comprehension'|
    x=='Verbal Comprehension of Complex Material'|
    x=='Written Comprehension']="Comprehension Skills"

# x[x=='Naming'|
#     x=='Naming Skills'|
#     x=='Word Naming']='Naming Skills'## merged with language skills



x[x=='Higher Visual Perception'|
    x=='Planning Visuospatial Abilities'|
    x=='Visual Fields'|
    x=='Visual Movement Organization'|
    x=='Visual Perception'|
    x=='Visual Scanning'|
    x=='Visual Scanning Patterns'|
    x=='Visual-Spatial'|
    x=='Visuo-Motor Coordination'|
    x=='Visuo-Perceptual'|
    x=='Visuomotor Coordination'|
    x=='Visuomotor Speed'|
    x=='Visuospatial'|
    x=='Visuospatial Abilities'|
    x=='Visuospatial Cognition'|
    x=='Visuospatial Function'|
    x=='Visuospatial Functions'|
    x=='Visuospatial Perception']="Visual Skills"

x[x=='Information Processing'|
    x=='Information Processing Speed'|
    x=='Processing Speed']="Processing Skills"


x[x=='Orientation'|
    x=='Orientation to Time'|
    x=='Temporal Orientation'|
    x=='Orientation to Time & Space']="Orientation Skills"


x[x=='Social Acuity'|
    x=='Social Cognition'|
    x=='Social Interaction']="Social Cognitive Skills"

x[x=='Delayed Word Recall'|
    x=='Immediate Recall'|
    x=='Recall'|
    x=='Recall over Time']="Recall"

x[x=='Higher Level Perception'|
    x=='Perception'|
    x=='Perceptual Functions'|
    x=='Spatial Perception']='Perception'

x[x=='Gestual Praxis'|
    x=='Praxia'|
    x=='Praxis'|
    x=='Praxis-Gnosis'|
    x=="Ideational"]="Praxis"

x[x=='Motor Functions'|
    x=='Motor Skills'|
    x=='Motor Speed']='Motor Skills'

x[x=='General Inteligence'|
    x=='General Intellectual Functioning'|
    x=='General Intelligence'|
    x=='Intellectual Abilities'|
    x=='Intellectual Ability']='Intellectual Abilities'

x[x=='Hit Reaction Time'|
    x=='Reaction Speed'|
    x=='Reaction Time']='Reaction Time'

x[x=='Function'|
    x=='Functional Status'|
    x=='Disability'|
    x=='Degree of Disability'|
    x=='Extent of Disability']='Functional Status'

x[x=='Cognitive Flexibility'|
    x=='Mental Flexibility']='Cognitive Flexibility'


x[x=='[no Area]']='[Not Reported]'

  x<-x[,which(names(x)=='id'):which(names(x)=='instrument')]
  

  return(x$domain)
  }

###MAKING SURE FUNCTION WORKS (printing just the first 25 obervations)
clean.domains(domains.raw)[1:25]

###SAVING ALL IN A SEPARATE DATASET
domains.clean=domains.raw
domains.clean$domain=clean.domains(domains.raw)
# x<-domains.clean
# x<-x[order(x$domain),]

  ####RESULTS - CLEAN DOMAINS####
  domains.result<-data.frame(table(domains.clean$domain))
  domains.result<-domains.result[order(domains.result$Freq, decreasing = T),]
  # domains.result.short<-domains.result[1:20,]
  domains.result.grouped<-data.frame(table(x$domain,x$intervention))
  domains.result.grouped<-domains.result.grouped[order(domains.result.grouped$Freq, decreasing = T),]
  # domains.result.grouped.short<-domains.result.grouped[1:20,]
  rm(x)


####Domain cleaning (ICF - one rater)####

clean.icf=function(dirty.data){
  ###input file
  x<-dirty.data
    ####studies with executive functions subdivided in categories
    b=x[which(with(x,study=="Rand et al., 2010" |
                     study=="Mehrabian et al., 2015" |
                     study=="Poulin et al., 2017" |
                     study=="Rozental-Iluz et al., 2016"|
                     study=="Sachdev et al., 2009")),]

    # getting positions
    a<-which(with(b,study=='Mehrabian et al., 2015' &
                    domain=="Set Shifting"|
                    study=='Poulin et al., 2017' &
                    domain=="Attention"|
                    study=='Poulin et al., 2017' &
                    domain=="Cognitive Flexibility"|
                    study=='Poulin et al., 2017' &
                    domain=="Inhibition"|
                    study=='Rand et al., 2010' &
                    domain=="Working Memory"|
                    study=='Rand et al., 2010' &
                    domain=="Delayed Recall"|
                    study=='Rand et al., 2010' &
                    domain=="Divided Attention"|
                    study=='Rand et al., 2010' &
                    domain=="Long-Term Memory"|
                    study=='Rand et al., 2010' &
                    domain=="Cognitive Flexibility"|
                    study=='Rozental-Iluz et al., 2016' &
                    domain!="EF - Information Seeking" &
                    domain!="EF - Initiation" &
                    domain!="EF - Visuomotor Scanning"|
                    study=="Sachdev et al., 2009"&
                    domain=="Verbal Fluency"))

    # adding prefix "EF - "
    b[a,'domain']=paste0("EF - ", b[a,'domain'])

    # replacing
    x[which(with(x,study=="Rand et al., 2010" |
                   study=="Mehrabian et al., 2015" |
                   study=="Poulin et al., 2017" |
                   study=="Rozental-Iluz et al., 2016"|
                   study=="Sachdev et al., 2009")),]=b



    # domains that needed previous cleaning

    # Cognitive functions that could not be allocated to any ICF category below

    x[x=='Commission Errors'|
        x=='Commissions [number of Times Person Responds to a Non-Target Item]']='Error Commission'

    x[x=='Omissions'|
        x=='Item Omissions']='Omission'

    x[x=='Behavior'|
        x=='Behavioral Change'|
        x=='Behavioral Improvement']="Behavior"

    #Construction skills (own definition) - mental functions that involve actively (manually or mentally) constructing something new
    x[x=='Constructing'|
        x=='Construction'|
        x=='Construction Skills'|
        x=='Constructional'|
        x=='Constructional Function'|
        x=='Constructional Praxis'|
        x=='Constructive Praxis'|
        x=='Visuospatial Construction'|
        x=='Visuoconstruction'|
        x=='Visuoconstructive Ability'|
        x=='Visuoconstuctive Functions']="Construction skills"

    x[x=='Neurologic Function'|
        x=='Neurological Function']="Neurologic Function"

    # Learning (own definition) – use of information to develop a new product
    x[x=='Learning'|
        x=='New Learning'|
        x=='Verbal Learning']='Learning Skills'
    x[x=='Premotor Functions'|
        x=='Premotor Abilities']='Premotor Function'

    # Function (own definition) - performance in relation with daily activities
    x[x=='Function'|
        x=='Functional Status'|
        x=='Disability'|
        x=='Degree of Disability'|
        x=='Extent of Disability']='Functional Status'


    paste0('x[',paste0("x=='",unique(x[grep(paste(
      'Consc',
      'Vigil',
      'Alertn',
      sep = '|'), x$domain),'domain']),"'", collapse="|"),']=')

    x[x=='Alertness'|
        x=='Intrinsic Alertness'|
        x=='Vigilance']='Consciousness (b110)'

    paste0('x[',paste0("x=='",unique(x[grep(paste(
      'Orient',
      sep = '|'), x$domain),'domain']),"'", collapse="|"),"]=")

    x[x=='Orientation'|
        x=='Orientation to Time & Space'|
        x=='Temporal Orientation']='Orientation (b114)'



    paste0('x[',paste0("x=='",unique(x[grep(paste(
      'Intel',
      'Cognit',
      'Neuropsyc',
      'Intel',
      'Mental',
      'Dement',
      sep = '|'), x$domain),'domain']),"'", collapse="|"),"]=")


    x[x=='Cognition'|
        x=='Cognitive'|
        x=='Cognitive Abilities'|
        x=='Cognitive Ability'|
        x=='Cognitive Decline'|
        x=='Cognitive Disability'|
        x=='Cognitive Disorders'|
        x=='Cognitive Dysfunction'|
        x=='Cognitive Function'|
        x=='Cognitive Functioning'|
        x=='Cognitive Functions'|
        x=='Cognitive Impact Resulting from Stroke'|
        x=='Cognitive Impairment'|
        x=='Cognitive Mental Status'|
        x=='Cognitive Performance'|
        x=='Cognitive State'|
        x=='Cognitive Status'|
        x=='Degree of Cognitive Abilities'|
        x=='Dementia'|
        x=='Dementia Diagnosis'|
        x=='Dementia Diagnostic Instrument'|
        x=='General Cognition'|
        x=='General Cognitive Function'|
        x=='General Cognitive Functioning'|
        x=='General Cognitive Functions'|
        x=='General Inteligence'|
        x=='General Intellectual Functioning'|
        x=='General Intelligence'|
        x=='Global Cognition'|
        x=='Global Cognitive Assessment'|
        x=='Global Cognitive Function'|
        x=='Global Cognitive Functioning'|
        x=='Global Cognitive Functions'|
        x=='Global Cognitive Impairment'|
        x=='Global Cognitive Performance'|
        x=='Global Cognitive Status'|
        x=='Intellectual Abilities'|
        x=='Intellectual Ability'|
        x=='Mental Status'|
        x=='Mild Cognitive Impairment'|
        x=='Moderate Cognitive Dysfunction'|
        x=='Neuropsychological Functions'|
        x=='Neuropsychological Impairment'|
        x=='Premorbid Cognitive Function'|
        x=='Premorbid Cognitive Impairment'|
        x=='Recovery of Cognition'|
        x=='State of Higher Mental Functions']="Intellectual (b117)"


    paste0('x[',paste0("x=='",unique(x[grep(paste(
      'Psychoso',
      'Social',
      sep = '|'), x$domain),'domain']),"'", collapse="|"),']=')

    x[x=='Social Acuity'|
        x=='Social Cognition'|
        x=='Social Interaction']='Global Psychosocial (b122)'


    paste0('x[',paste0("x=='",unique(x[grep(paste(
      'Impuls',
      'Energ',
      'Drive',
      'Craving',
      'Sleep',
      sep = '|'), x$domain),'domain']),"'", collapse="|"),']=')


    paste0('x[',paste0("x=='",unique(x[grep(paste(
      "Attent",
      "attent",
      "Sustain",
      "Shift",
      "Divid",
      "Select",
      "Shar",
      sep = '|'), x$domain),'domain']),"'", collapse="|"),']=')

    x[x=='Alternating Attention'|
        x=='Attention'|
        x=='EF - Attention'|
        # x=='Attention [psychomotor Speed]'|# moved to psychomotor functions
        x=='Attention Omission'|
        x=='Attention-Phasic Alert'|
        x=='Attentional Tasks'|
        x=='Attentiveness'|
        x=='Concentration'|
        x=='Divided'|
        x=='Divided Attention'|
        x=='EF - Divided Attention'|
        x=='EF - Attention Switching'|
        x=='Focused Visual Attention'|
        x=='General Visual Attention'|
        x=='Phasic Attention'|
        x=='Selective'|
        x=='Selective Attention'|
        x=='Sustained'|
        x=='Sustained Attention'|
        x=='Switching of Attention'|
        x=='Tonic Attention'|
        x=='Visual Inattention']='Attention (b140)'

    paste0('x[',paste0("x=='",unique(x[grep(paste(
      'Memory',
      'Recall',
      sep = '|'), x$domain),'domain']),"'", collapse="|"),']=')

    x[x=='Auditory Memory'|
        x=='Delayed Memory'|
        x=='EF - Delayed Recall'|
        x=='Delayed Word Recall'|
        x=='Episodic Memory'|
        x=='Episodic Verbal Memory'|
        x=='Everyday Memory Problems'|
        x=='General Long-Term Memory'|
        x=='Immediate Memory'|
        x=='Immediate Recall'|
        x=='Long Term Memory'|
        x=='Long-Term Memory'|
        x=='EF - Long-Term Memory'|
        x=='Long-Term Verbal Memory'|
        x=='Memory'|
        x=='Memory - Short Term'|
        x=='Memory Registration'|
        x=='Memory Self-Efficacy'|
        x=='Memory-Long Term'|
        x=='Memory-Long-Term Memory'|
        x=='Memory-Short Term'|
        x=='Memory-Short-Term Verbal'|
        x=='Non-Verbal Visual Memory'|
        x=='Nonverbal Memory'|
        x=='Recall'|
        x=='Recall over Time'|
        x=='Short-Term Memory'|
        x=='Short-Term Memory Recall'|
        x=='Short-Term Verbal Memory'|
        x=='Verbal'|
        x=='Verbal Declarative Memory'|
        x=='Verbal Memory'|
        x=='Verbal Short Term Memory'|
        x=='Verbal Working Memory'|
        x=='Visual Memory'|
        x=='Visual Memory [nonverbal]'|
        x=='Visual Memory [or Visuospatial Functions]'|
        x=='Visual Memory Functions'|
        x=='Visuospatial Memory'|
        x=='Word Memory'|
        x=='Working Memory'|
        x=='EF - Working Memory'|
        x=='Recall over Time']="Memory (b144)"

    paste0('x[',paste0("x=='",unique(x[grep(paste(
      'Psycho',
      'Speed',
      'Time',
      'Coord',
      sep = '|'), x$domain),'domain']),"'", collapse="|"),']=')

    x[x=='Adjustment to Presentation Speed'|
        x=='Attention [psychomotor Speed]'|
        x=='EF - Psychomotor Performance'|
        x=='EF - Speed'|
        x=='Gestual Praxis'|
        x=='Hit Reaction Time'|
        x=="Ideational"|
        x=='Motor Speed'|
        x=='Praxia'|
        x=='Praxis'|
        x=='Praxis-Gnosis'|
        x=='Planning Visuospatial Abilities'|
        x=='Psychomotor Speed'|
        x=='Reaction Speed'|
        x=='Reaction Time'|
        x=='Speed'|
        x=='Visual Movement Organization'|
        x=='Visuo-Motor Coordination'|
        x=='Visuomotor Coordination'|
        x=='Visuomotor Speed']='Psychomotor (b147)'


    paste0('x[',paste0("x=='",unique(x[grep(paste(
      'Percep',
      'Audit',
      'Visu',
      'Olfac',
      'Gusta',
      'Tactil',
      'spati',
      'Spati',
      'Spac',
      sep = '|'), x$domain),'domain']),"'", collapse="|"),']=')

    x[x=='Field of Vision'|
        x=='Gnosia'|
        x=='Higher Level Perception'|
        x=='Higher Visual Perception'|
        x=='Optical-Spatial Gnosis'|
        x=='Perception'|
        x=='Perceptual Functions'|
        x=='Spatial Neglect Severity'|
        x=='Spatial Perception'|
        x=='Spatial Skills'|
        x=='Unilateral Visual Neglect'|
        x=='Unilateral Visuospatial Neglect'|
        x=='Visual Fields'|
        x=='Visual Inattention'|
        x=='Visual Neglect'|
        x=='Visual Perception'|
        x=='Visual Scanning'|
        x=='Visual Scanning Patterns'|
        x=='Visual-Spatial'|
        x=='Visuo-Perceptual'|
        x=='Visuospatial'|
        x=='Visuospatial Abilities'|
        x=='Visuospatial Cognition'|
        x=='Visuospatial Function'|
        x=='Visuospatial Functions'|
        x=='Visuospatial Perception']='Perceptual (b156)'


    paste0('x[',paste0("x=='",unique(x[grep(paste(
      'Pace',
      'Speed',
      'Form',
      'Thinking',
      'Mental',
      sep = '|'), x$domain),'domain']),"'", collapse="|"),']=')

    x[x=='Conceptual Thinking'|
        x=='Mental Control'|
        x=='Thought Operation']='Thought (b160)'

    paste0('x[',paste0("x=='",unique(x[grep(paste(
      'EF',
      'Formati',
      'Categori',
      "Executive",
      "Abstrac",
      'Reason',
      'Organi',
      'Plan',
      'Manage',
      'Flexib',
      'Shift',
      'Insigh',
      'Awarene',
      'Judegem',
      'Evaluat',
      'Solving',
      sep = '|'), x$domain),'domain']),"'", collapse="|"),']=')

    x[x=='Abstract Reasoning'|
        x=='Cognitive Flexibility'|
        x=='Concept Formation'|
        x=='Conceptualization'|
        x=='Conflict Resolution'|
        x=='EF - Cognitive Flexibility'|
        x=='EF - Completion'|
        x=='EF'|
        x=='EF - Information Seeking'|
        x=='EF - Inhibition'|
        x=='EF - Initiation'|
        x=='EF - Learning'|
        x=='EF - Mental Flexibility'|
        x=='EF - Judgment'|
        x=='EF - Planning'|
        x=='EF - Problem Solving'|
        x=='EF - Response Inhibition'|
        x=='EF - Set Shifting'|
        x=='EF - Visuomotor Scanning'|
        x=='Executive'|
        x=='Executive Functioning'|
        x=='Executive Functions'|
        x=='Executive Reasoning'|
        x=='Frontal Lobe Functions'|
        x=='Information Processing'|
        x=='Information Processing Speed'|
        x=='Letter Sequencing'|
        x=='Logical Deductive Ability'|
        x=='Mental Flexibility'|
        x=='Mental Slowness in Relation to Daily Activities'|
        x=='Number-Letter Switching'|
        x=='Number Sequencing'|
        x=='Nonverbal Reasoning'|
        x=='Performance in Time Pressure Situations'|
        x=='Problem-Solving'|
        x=='Processing Speed'|
        x=='Processing Skills'|
        x=='Reasoning'|
        x=='Time to Find Increasing Numbers'|
        x=='Self-Monitoring of Non-Motor Performance'|
        x=='Set Shifting']='HLCF (b164)'


    x[x=='Aphasia'|
        x=='Auditory Comprehension'|
        x=='Category'|
        x=='Cognitive Communicative Skills'|
        x=='Communication'|
        x=='Comprehension'|
        x=='EF - Verbal Fluency'|
        x=='Expression'|
        x=='Language'|
        x=='Language - Object Naming from Line Drawing'|
        x=='Language [judged by Neuropsychologist]'|
        x=='Language Abilities'|
        x=='Language Abilities - Speech Fluency'|
        x=='Language Ability'|
        x=='Language-Auditory Comprehension'|
        x=='Language-Confrontation Naming'|
        x=='Language-Phonemic Verbal Fluency'|
        x=='Language-Picture Naming'|
        x=='Language-Reading Capacity'|
        x=='Language-Sentence Comprehension'|
        x=='Information Content in Spontaneous Speech'|
        x=='Verbal'|
        x=='Verbal Ability'|
        x=='Verbal Comprehension of Complex Material'|
        x=='Verbal Expression'|
        x=='Verbal Fluency'|
        x=='Verbal Function'|
        x=='Verbal Processing'|
        x=='Letter Fluency Tasks'|
        x=='Lexical Fluency'|
        x=='Phonological Fluency'|
        x=='Semantic Fluency'|
        x=='Narrative Speech'|
        x=='Naming'|
        x=='Naming Skills'|
        x=='Word Generation'|
        x=='Word Naming'|
        x=='Written Comprehension']="Language (b167)"

    x[x=='Awareness of Visuospatial Neglect'|
        x=='Neglect'|
        x=='Personal Neglect'|
        x=='Spatial Neglect Severity'|
        x=='Unilateral Neglect'|
        x=='Unilateral Visual Neglect'|
        x=='Unilateral Visuospatial Neglect'|
        x=='Visual Neglect'|
        x=='Sensory']="Experience of self & time (b180)"




    x[x=='Arithmetic'|
        x=='Calculation']='Calculation (b172)'


    x[x=='[no Area]']='[Not Reported]'

    # dput(paste0("x=='",unique(x[grep("Area", x$domain),'domain']),"'", collapse="|"))
    # Unclassified
    # x=='Music Cognition'|

    x<-x[,which(names(x)=='id'):which(names(x)=='instrument')]
    # x<-x[order(x$domain),]

    return(x$domain)

  }

clean.icf(domains.raw)[1:25]
domains.clean.icf=domains.raw
domains.clean.icf$domain=clean.icf(domains.raw)
# x=domains.clean.icf
# domains.clean.icf<-x[order(x$domain),]


####ICF Domains AFTER AGREEMENT####
clean.agreed=function(dirty.data){
  ###input file
  x<-dirty.data
    ####studies with executive functions subdivided in categories
    b=x[which(with(x,study=="Rand et al., 2010" |
                     study=="Mehrabian et al., 2015" |
                     study=="Poulin et al., 2017" |
                     study=="Rozental-Iluz et al., 2016"|
                     study=="Sachdev et al., 2009")),]

    # getting positions
    a<-which(with(b,study=='Mehrabian et al., 2015' &
                    domain=="Set Shifting"|
                    study=='Poulin et al., 2017' &
                    domain=="Attention"|
                    study=='Poulin et al., 2017' &
                    domain=="Cognitive Flexibility"|
                    study=='Poulin et al., 2017' &
                    domain=="Inhibition"|
                    study=='Rand et al., 2010' &
                    domain=="Working Memory"|
                    study=='Rand et al., 2010' &
                    domain=="Delayed Recall"|
                    study=='Rand et al., 2010' &
                    domain=="Divided Attention"|
                    study=='Rand et al., 2010' &
                    domain=="Long-Term Memory"|
                    study=='Rand et al., 2010' &
                    domain=="Cognitive Flexibility"|
                    study=='Rozental-Iluz et al., 2016' &
                    domain!="EF - Information Seeking" &
                    domain!="EF - Initiation" &
                    domain!="EF - Visuomotor Scanning"|
                    study=="Sachdev et al., 2009"&
                    domain=="Verbal Fluency"))

    # adding prefix "EF - "
    b[a,'domain']=paste0("EF - ", b[a,'domain'])

    # replacing
    x[which(with(x,study=="Rand et al., 2010" |
                   study=="Mehrabian et al., 2015" |
                   study=="Poulin et al., 2017" |
                   study=="Rozental-Iluz et al., 2016"|
                   study=="Sachdev et al., 2009")),]=b



    # domains that needed previous cleaning

    # Cognitive functions that could not be allocated to any ICF category below

    x[x=='Commission Errors'|
        x=='Commissions [number of Times Person Responds to a Non-Target Item]']='Error Commission'

    x[x=='Omissions'|
        x=='Item Omissions']='Omission'

    x[x=='Behavior'|
        x=='Behavioral Change'|
        x=='Behavioral Improvement']="Behavior"

    #Construction skills (own definition) - mental functions that involve actively (manually or mentally) constructing something new
    x[x=='Constructing'|
        x=='Construction'|
        x=='Construction Skills'|
        x=='Constructional'|
        x=='Constructional Function'|
        x=='Constructional Praxis'|
        x=='Constructive Praxis'|
        x=='Visuospatial Construction'|
        x=='Visuoconstruction'|
        x=='Visuoconstructive Ability'|
        x=='Visuoconstuctive Functions']="HLCF (b164)" # Reclassified into HLCF (before was Construction skills alone)

    x[x=='Neurologic Function'|
        x=='Neurological Function']="Neurologic Function"

    # Learning (own definition) – use of information to develop a new product
    x[x=='Learning'|
        x=='New Learning'|
        x=='EF - Learning'|
        x=='Verbal Learning']='Basic learning (d130-159)' # made into ICF category of learning (before it was learning alone)


     x[x=='Premotor Functions'|
        x=='Premotor Abilities']='Premotor Function'

    # Function (own definition) - performance in relation with daily activities
    x[x=='Function'|
        x=='Functional Status'|
        x=='Disability'|
        x=='Degree of Disability'|
        x=='Extent of Disability']='Functional status'


    # paste0('x[',paste0("x=='",unique(x[grep(paste(
    #   'Consc',
    #   'Vigil',
    #   'Alertn',
    #   sep = '|'), x$domain),'domain']),"'", collapse="|"),']=')

    x[x=='Alertness'|
        x=='Intrinsic Alertness'|
        x=='Vigilance']='Consciousness (b110)'

    # paste0('x[',paste0("x=='",unique(x[grep(paste(
    #   'Orient',
    #   sep = '|'), x$domain),'domain']),"'", collapse="|"),"]=")
    #
    x[x=='Orientation'|
        x=='Orientation to Time & Space'|
        x=='Temporal Orientation']='Orientation (b114)'



    # paste0('x[',paste0("x=='",unique(x[grep(paste(
    #   'Intel',
    #   'Cognit',
    #   'Neuropsyc',
    #   'Intel',
    #   'Mental',
    #   'Dement',
    #   sep = '|'), x$domain),'domain']),"'", collapse="|"),"]=")


    x[x=='Cognition'|
        x=='Cognitive'|
        x=='Cognitive Abilities'|
        x=='Cognitive Ability'|
        x=='Cognitive Decline'|
        x=='Cognitive Disability'|
        x=='Cognitive Disorders'|
        x=='Cognitive Dysfunction'|
        x=='Cognitive Function'|
        x=='Cognitive Functioning'|
        x=='Cognitive Functions'|
        x=='Cognitive Impact Resulting from Stroke'|
        x=='Cognitive Impairment'|
        x=='Cognitive Mental Status'|
        x=='Cognitive Performance'|
        x=='Cognitive State'|
        x=='Cognitive Status'|
        x=='Degree of Cognitive Abilities'|
        # x=='Dementia'| # moved to intellectual (b117)
        # x=='Dementia Diagnosis'| # moved to intellectual (b117)
        # x=='Dementia Diagnostic Instrument'| # moved to intellectual (b117)
        x=='General Cognition'|
        x=='General Cognitive Function'|
        x=='General Cognitive Functioning'|
        x=='General Cognitive Functions'|
        # x=='General Inteligence'| # moved to intellectual (b117)
        # x=='General Intellectual Functioning'| # moved to intellectual (b117)
        # x=='General Intelligence'| # moved to intellectual (b117)
        x=='Global Cognition'|
        x=='Global Cognitive Assessment'|
        x=='Global Cognitive Function'|
        x=='Global Cognitive Functioning'|
        x=='Global Cognitive Functions'|
        x=='Global Cognitive Impairment'|
        x=='Global Cognitive Performance'|
        x=='Global Cognitive Status'|
        # x=='Intellectual Abilities'| # moved to intellectual (b117)
        # x=='Intellectual Ability'| # moved to intellectual (b117)
        x=='Mental Status'|
        x=='Mild Cognitive Impairment'|
        x=='Moderate Cognitive Dysfunction'|
        x=='Neuropsychological Functions'|
        x=='Neuropsychological Impairment'|
        x=='Premorbid Cognitive Function'|
        x=='Premorbid Cognitive Impairment'|
        x=='Recovery of Cognition'|
        x=='State of Higher Mental Functions']="Global (ICF-ch1)"


    # The domains below were moved to intellectual after agreement between reviewers (JPS and TT)
    
    x[x=='Dementia'| # moved to intellectual (b117)
    x=='Dementia Diagnosis'| # moved to intellectual (b117)
    x=='Dementia Diagnostic Instrument'| # moved to intellectual (b117)
    x=='General Inteligence'| # moved to intellectual (b117)
    x=='General Intellectual Functioning'| # moved to intellectual (b117)
    x=='General Intelligence'| # moved to intellectual (b117)
    x=='Intellectual Abilities'| # moved to intellectual (b117)
    x=='Intellectual Ability'] = 'Intellectual (b117)' # moved to intellectual (b117)


    # paste0('x[',paste0("x=='",unique(x[grep(paste(
    #   'Psychoso',
    #   'Social',
    #   sep = '|'), x$domain),'domain']),"'", collapse="|"),']=')

    x[x=='Social Acuity'|
        x=='Social Cognition'|
        x=='Social Interaction']='Global Psychosocial (b122)'


    # paste0('x[',paste0("x=='",unique(x[grep(paste(
    #   'Impuls',
    #   'Energ',
    #   'Drive',
    #   'Craving',
    #   'Sleep',
    #   sep = '|'), x$domain),'domain']),"'", collapse="|"),']=')


    # paste0('x[',paste0("x=='",unique(x[grep(paste(
    #   "Attent",
    #   "attent",
    #   "Sustain",
    #   "Shift",
    #   "Divid",
    #   "Select",
    #   "Shar",
    #   sep = '|'), x$domain),'domain']),"'", collapse="|"),']=')

    x[x=='Alternating Attention'|
        x=='Attention'|
        x=='EF - Attention'|
        # x=='Attention [psychomotor Speed]'|# moved to psychomotor functions
        # x=='Attention Omission'|# moved to perceptual functions
        # x=='Visual Inattention'|# moved to perceptual functions
        x=='Attention-Phasic Alert'|
        x=='Attentional Tasks'|
        x=='Attentiveness'|
        x=='Concentration'|
        x=='Divided'|
        x=='Divided Attention'|
        x=='EF - Divided Attention'|
        x=='EF - Attention Switching'|
        x=='Focused Visual Attention'|
        x=='General Visual Attention'|
        x=='Phasic Attention'|
        x=='Selective'|
        x=='Selective Attention'|
        x=='Sustained'|
        x=='Sustained Attention'|
        x=='Switching of Attention'|
        x=='Tonic Attention'
      ]='Attention (b140)'

    # paste0('x[',paste0("x=='",unique(x[grep(paste(
    #   'Memory',
    #   'Recall',
    #   sep = '|'), x$domain),'domain']),"'", collapse="|"),']=')

    x[x=='Auditory Memory'|
        x=='Delayed Memory'|
        x=='EF - Delayed Recall'|
        x=='Delayed Word Recall'|
        x=='Episodic Memory'|
        x=='Episodic Verbal Memory'|
        x=='Everyday Memory Problems'|
        x=='General Long-Term Memory'|
        x=='Immediate Memory'|
        x=='Immediate Recall'|
        x=='Long Term Memory'|
        x=='Long-Term Memory'|
        x=='EF - Long-Term Memory'|
        x=='Long-Term Verbal Memory'|
        x=='Memory'|
        x=='Memory - Short Term'|
        x=='Memory Registration'|
        x=='Memory Self-Efficacy'|
        x=='Memory-Long Term'|
        x=='Memory-Long-Term Memory'|
        x=='Memory-Short Term'|
        x=='Memory-Short-Term Verbal'|
        x=='Non-Verbal Visual Memory'|
        x=='Nonverbal Memory'|
        x=='Recall'|
        x=='Recall over Time'|
        x=='Short-Term Memory'|
        x=='Short-Term Memory Recall'|
        x=='Short-Term Verbal Memory'|
        x=='Verbal'|
        x=='Verbal Declarative Memory'|
        x=='Verbal Memory'|
        x=='Verbal Short Term Memory'|
        x=='Verbal Working Memory'|
        x=='Visual Memory'|
        x=='Visual Memory [nonverbal]'|
        x=='Visual Memory [or Visuospatial Functions]'|
        x=='Visual Memory Functions'|
        x=='Visuospatial Memory'|
        x=='Word Memory'|
        x=='Working Memory'|
        x=='EF - Working Memory'|
        x=='Recall over Time']="Memory (b144)"

    # paste0('x[',paste0("x=='",unique(x[grep(paste(
    #   'Psycho',
    #   'Speed',
    #   'Time',
    #   'Coord',
    #   sep = '|'), x$domain),'domain']),"'", collapse="|"),']=')

    x[x=='Adjustment to Presentation Speed'|
        x=='Attention [psychomotor Speed]'|
        x=='EF - Psychomotor Performance'|
        x=='EF - Speed'|
        x=='Hit Reaction Time'|
        x=='Motor Speed'|
        x=='Praxis-Gnosis'|
        x=='Psychomotor Speed'|
        x=='Reaction Speed'|
        x=='Reaction Time'|
        x=='Speed'
        # x=='Gestual Praxis'| # moved to Complex mov. seq. (b176)
        # x=="Ideational"| # b176
        # x=='Praxia'| # b176
        # x=='Praxis'| # b176
        # x=='Planning Visuospatial Abilities'| # b176 
        # x=='Visual Movement Organization'| # b176
        # x=='Visuo-Motor Coordination'| # b176
        # x=='Visuomotor Coordination'| # b176
        # x=='Visuomotor Speed' # b176
      ]='Psychomotor (b147)'

    x[x=='Gestual Praxis'| #(b176)
      x=="Ideational"| #(b176)
      x=='Praxia'| #(b176)
      x=='Praxis'| # (b176)
      x=='Planning Visuospatial Abilities'| #(b176)
      x=='Visual Movement Organization'| #(b176)
      x=='Visuo-Motor Coordination'| #(b176)
      x=='Visuomotor Coordination'| #(b176)
      x=='EF - Visuomotor Scanning'| # (b176)
      x=='Visuomotor Speed']='Complex mov. seq. (b176)' #(b176)

    # paste0('x[',paste0("x=='",unique(x[grep(paste(
    #   'Percep',
    #   'Audit',
    #   'Visu',
    #   'Olfac',
    #   'Gusta',
    #   'Tactil',
    #   'spati',
    #   'Spati',
    #   'Spac',
    #   sep = '|'), x$domain),'domain']),"'", collapse="|"),']=')

    x[x=='Gnosia'|
        x=='Higher Level Perception'|
        x=='Higher Visual Perception'|
      # x=='Field of Vision'| # moved to visual field functions (b2101)
      # x=='Visual Fields'| # (b2101)
        x=='Optical-Spatial Gnosis'|
        x=='Perception'|
        x=='Perceptual Functions'|
        x=='Spatial Neglect Severity'|
        x=='Spatial Perception'|
        x=='Spatial Skills'|
        x=='Unilateral Visual Neglect'|
        x=='Unilateral Visuospatial Neglect'|
        x=='Visual Inattention'|
        x=='Visual Neglect'|
        x=='Visual Perception'|
        x=='Visual Scanning'|
        x=='Visual Scanning Patterns'|
        x=='Attention Omission'|
        x=='Visual-Spatial'|
        x=='Visuo-Perceptual'|
        x=='Visuospatial'|
        x=='Visuospatial Abilities'|
        x=='Visuospatial Cognition'|
        x=='Visuospatial Function'|
        x=='Visuospatial Functions'|
        x=='Visuospatial Perception']='Perceptual (b156)'

    x[x=='Field of Vision'| # (b2101)
      x=='Visual Fields']='Visual Field (b2101)' # (b2101)


    # paste0('x[',paste0("x=='",unique(x[grep(paste(
    #   'Pace',
    #   'Speed',
    #   'Form',
    #   'Thinking',
    #   'Mental',
    #   sep = '|'), x$domain),'domain']),"'", collapse="|"),']=')

    x[x=='Conceptual Thinking'|
        x=='Mental Control'|
        x=='Thought Operation']='Thought (b160)'

    # paste0('x[',paste0("x=='",unique(x[grep(paste(
    #   'EF',
    #   'Formati',
    #   'Categori',
    #   "Executive",
    #   "Abstrac",
    #   'Reason',
    #   'Organi',
    #   'Plan',
    #   'Manage',
    #   'Flexib',
    #   'Shift',
    #   'Insigh',
    #   'Awarene',
    #   'Judegem',
    #   'Evaluat',
    #   'Solving',
    #   sep = '|'), x$domain),'domain']),"'", collapse="|"),']=')

    x[x=='Abstract Reasoning'|
        x=='Cognitive Flexibility'|
        x=='Concept Formation'|
        x=='Conceptualization'|
        x=='Conflict Resolution'|
        x=='EF - Cognitive Flexibility'|
        x=='EF - Completion'|
        x=='EF'|
        x=='EF - Information Seeking'|
        x=='EF - Inhibition'|
        x=='EF - Initiation'|
        # x=='EF - Learning'| # moved to learning (d160-179)
        x=='EF - Mental Flexibility'|
        x=='EF - Judgment'|
        x=='EF - Planning'|
        x=='EF - Problem Solving'|
        x=='EF - Response Inhibition'|
        x=='EF - Set Shifting'|
        # x=='EF - Visuomotor Scanning'| # moved to perceptual (b176)
        x=='Executive'|
        x=='Executive Functioning'|
        x=='Executive Functions'|
        x=='Executive Reasoning'|
        x=='Frontal Lobe Functions'|
        x=='Information Processing'| # left as HLCF after agreement (JPS and TT)
        x=='Information Processing Speed'| # left as HLCF after agreement (JPS and TT)
        x=='Letter Sequencing'|
        x=='Logical Deductive Ability'|
        x=='Mental Flexibility'|
        x=='Mental Slowness in Relation to Daily Activities'|
        x=='Number-Letter Switching'|
        x=='Number Sequencing'|
        x=='Nonverbal Reasoning'|
        x=='Performance in Time Pressure Situations'|
        x=='Problem-Solving'|
        x=='Processing Speed'|
        x=='Processing Skills'|
        x=='Reasoning'|
        x=='Time to Find Increasing Numbers'|
        x=='Self-Monitoring of Non-Motor Performance'|
        x=='Set Shifting']='HLCF (b164)'


    x[x=='Aphasia'|
        x=='Auditory Comprehension'|
        x=='Category'|
        x=='Cognitive Communicative Skills'|
        x=='Communication'|
        x=='Comprehension'|
        x=='EF - Verbal Fluency'|
        x=='Expression'|
        x=='Language'|
        x=='Language - Object Naming from Line Drawing'|
        x=='Language [judged by Neuropsychologist]'|
        x=='Language Abilities'|
        x=='Language Abilities - Speech Fluency'|
        x=='Language Ability'|
        x=='Language-Auditory Comprehension'|
        x=='Language-Confrontation Naming'|
        x=='Language-Phonemic Verbal Fluency'|
        x=='Language-Picture Naming'|
        x=='Language-Reading Capacity'|
        x=='Language-Sentence Comprehension'|
        x=='Information Content in Spontaneous Speech'|
        x=='Verbal'|
        x=='Verbal Ability'|
        x=='Verbal Comprehension of Complex Material'|
        x=='Verbal Expression'|
        x=='Verbal Fluency'|
        x=='Verbal Function'|
        x=='Verbal Processing'|
        x=='Letter Fluency Tasks'|
        x=='Lexical Fluency'|
        x=='Phonological Fluency'|
        x=='Semantic Fluency'|
        x=='Narrative Speech'|
        x=='Naming'|
        x=='Naming Skills'|
        x=='Word Generation'|
        x=='Word Naming'|
        x=='Written Comprehension']="Language (b167)" # language functions under the ICF look at the expression and reception of language. In the literature included language tests are mostly looking at higher-level cognition because of the incorporation of different rules in addition to the language task

    x[x=='Awareness of Visuospatial Neglect'|
        x=='Neglect'|
        x=='Personal Neglect'|
        x=='Spatial Neglect Severity'|
        x=='Unilateral Neglect'|
        x=='Unilateral Visual Neglect'|
        x=='Unilateral Visuospatial Neglect'|
        x=='Visual Neglect'|
        x=='Sensory']="Exp. of self & time (b180)"




    x[x=='Arithmetic'|
        x=='Calculation']='Calculation (b172)'


    x[x=='[no Area]']='[Not Reported]'

    # dput(paste0("x=='",unique(x[grep("Area", x$domain),'domain']),"'", collapse="|"))
    # Unclassified
    # x=='Music Cognition'

    x<-x[,which(names(x)=='id'):which(names(x)=='instrument')]
    return(x[,c("domain","instrument")])

  }

clean.agreed(domains.raw)[200:225,]
domains.agreed=domains.raw
domains.agreed$domain=clean.agreed(domains.raw)[,'domain']
# x=domains.agreed
# domains.agreed<-x[order(x$domain),]


## FINAL TABLES FOR ICF DOMAINS
domains.result.icf<-data.frame(table(domains.clean.icf$domain))
domains.result.icf<-domains.result.icf[order(domains.result.icf$Freq, decreasing = T),]
# domains.result.short.icf<-domains.result.icf[1:20,]

domains.result.grouped.icf<-data.frame(table(domains.clean.icf$domain,domains.clean.icf$intervention))
domains.result.grouped.icf<-domains.result.grouped.icf[order(domains.result.grouped.icf$Freq, decreasing = T),]
# domains.result.grouped.short.icf<-domains.result.grouped.icf[1:20,]



####Circular Plots###





####DOMAINS AND INSTRUMENTS (ALL POSSIBLE PERMUTATIONS)####

###splitting by the folowing characters ", " OR ", and " OR " and "

x<-as.character(tests.cleaner$domain)
x<-strsplit(x, ", |\\, and |\\ and |\\ or ")

####Creating matrix domains only####
n.obs <- sapply(x, length)
seq.max <- seq(max(n.obs))
x <- t(sapply(x, "[", i = seq.max))

####Migrating data from main dataset####
x<-data.frame(tests.cleaner,x)
x<-x[,c(1,which(names(x)=='X1'):length(x))]
x<-melt(x, id.vars = 'id')
x<-data.frame(tests.cleaner["id"],x['id'],x['value'],tests.cleaner[2:length(tests.cleaner)])
x=na.omit(x)
###making sure id numbers are the same
table(x$id==x$id.1)


##re-organizing columns and cleaning blank spaces in rows
names(x)
x<-x[,c(which(names(x)=="id"),
        which(names(x)=="study"):which(names(x)=="continent"),which(names(x)=="value"),which(names(x)=="instrument"))]

x$value<-trimws(x$value)

names(x)[which(names(x)=='value')]='domain'



##################################################################################################################################
##################################DELETE LINES BELOW WHEN DONE####################################################################
##################################################################################################################################
####DOMAINS AND INSTRUMENTS (ALL POSSIBLE PERMUTATIONS)
q<-as.character(tests.raw$domain)
q<-strsplit(q, ", |\\, and |\\ and |\\ or ")

####Creating matrix domains only####
n.obs <- sapply(q, length)
seq.max <- seq(max(n.obs))
q <- t(sapply(q, "[", i = seq.max))

####Migrating data from main dataset####
q<-data.frame(tests.raw,q)
q<-q[,c(1,which(names(q)=='X1'):length(q))]
q<-melt(q, id.vars = 'id')
q<-data.frame(tests.raw["id"],q['id'],q['value'],tests.raw[2:length(tests.raw)])
q=na.omit(q)
###making sure id numbers are the same
table(q$id==q$id.1)
names(q)

##re-organizing columns and cleaning blank spaces in rows

q<-q[,c(which(names(q)=="id"),
        which(names(q)=="author"):which(names(q)=="continent"),which(names(q)=="value"),which(names(q)=="instrument"))]

q$value<-trimws(q$value)

names(q)[which(names(q)=='value')]='domain'

q$domain.clean=clean.agreed(q)[,'domain']
q$instrument=clean.inst(q)
q$instrument=tolower(q$instrument)

icf.table=q
rm(q)
# icf.table=icf.table[,c(1:4,12,13)]
icf.table=icf.table[,names(icf.table)!="study"]
icf.table=icf.table[,c(which(names(icf.table)=="id"):which(names(icf.table)=="intervention"),which(names(icf.table)=="domain"),which(names(icf.table)=="domain.clean"),which(names(icf.table)=="instrument"),
                       which(names(icf.table)=="sample"):which(names(icf.table)=="continent"))]



##################################################################################################################################
##################################DELETE LINES ABOVE WHEN DONE####################################################################
##################################################################################################################################


####cleaning domain names manually####


.noicf<-x
.icf<-x
.icf.agreed<-x
rm(x)

# #domain cleaning lines below
clean.domains(.noicf)[1:25]
.noicf$domain=clean.domains(.noicf)
x=.noicf #organizing data to show in ascending order
.noicf<-x[order(x$domain),]

# #cleaning lines for ICF domains
clean.icf(.icf)[1:25]
.icf$domain=clean.icf(.icf)
x=.icf #organizing data to show in ascending order
.icf<-x[order(x$domain),]

# # cleaning lines for ICF domains (after agreement)
clean.agreed(.icf.agreed)[1:25,'domain']
.icf.agreed$domain=clean.agreed(.icf.agreed)[,'domain']
x=.icf.agreed #organizing data to show in ascending order
.icf.agreed<-x[order(x$domain),]
   



#####Plot 2: tests and domains (ICF - after agreement)####
{
  ### making full names
  tests.result$Var1<-as.character(tests.result$Var1)
  
  # Create dataset only with the first 25 instruments and domains
  
  data=data.frame(
    individual=c(as.character(tests.result$Var1[1:25]),
                 as.character(data.frame(sort(table(.icf.agreed$domain),decreasing = T))[1:25,1])
                 ),
    group=c( rep('test', 25), rep('domain', 25)),
    value=c(tests.result$Freq[1:25],
            data.frame(sort(table(.icf.agreed$domain),decreasing = T))[1:25,2]
    )
  )
  
  
    ###The lines below are for mapping the 'OTHER' categories
  # data=data.frame(
  # individual=c(as.character(tests.result$Var1[1:25]),'Other',as.character(data.frame(sort(table(.icf.agreed$domain),decreasing = T))[1:25,1]),'Other'),
  #   group=c( rep('test', 26), rep('domain', 26)),
  #   value=c(tests.result$Freq[1:25],sum(tests.result[26:length(tests.result$Var1),'Freq']),
  #           data.frame(sort(table(.icf.agreed$domain),decreasing = T))[1:25,2],sum(data.frame(sort(table(.icf.agreed$domain),decreasing = T))[26:length(data.frame(sort(table(.icf.agreed$domain),decreasing = T))[,2]),2]))
  # 
  # )

  # Set a number of 'empty bar' to add at the end of each group
  empty_bar=2
  to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
  colnames(to_add) = colnames(data)
  to_add$group=rep(levels(data$group), each=empty_bar)
  data=rbind(data, to_add)
  data=data %>% arrange(group)
  data$id=seq(1, nrow(data))
  
  # Getting names position in Y for each label
  label_data=data
  number_of_bar=nrow(label_data)
  angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust<-ifelse( angle < -90, 1, 0)
  label_data$angle<-ifelse(angle < -90, angle+180, angle)
  
  # preparing a data frame for base lines
  base_data=data %>%
    group_by(group) %>%
    summarize(start=min(id), end=max(id) - empty_bar) %>%
    rowwise() %>%
    mutate(title=mean(c(start, end)))
  
  # preparing a data frame for grid (scales)
  grid_data = base_data
  grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start = grid_data$start - 1
  grid_data=grid_data[-1,]
  
  # Making the plot
p=  ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    
    geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
    
    # Adding value lines. Done at the beginning to make sure barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 70, xend = start, yend = 70), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 130, xend = start, yend = 130), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    
    # Add text showing the value of each line
    annotate("text", x = rep(max(data$id),5), y = c(10,40,70,100,130), label = c("10", "40", "70", "100", '130') , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
    
    geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
    ylim(
      -(tests.result$Freq[1]/3.5+data.frame(sort(table(.icf.agreed$domain),decreasing = T))[1,2]/3.5),
      tests.result$Freq[1]+data.frame(sort(table(.icf.agreed$domain),decreasing = T))[1,2]/1.5
         ) +
    
    # ylim(-sum(tests.result[26:length(tests.result$Var1),'Freq'])/2,
    #      sum(tests.result[26:length(tests.result$Var1),'Freq'])+
    #        sum(tests.result[26:length(tests.result$Var1),'Freq'])/15) + #this line is to expand the limits when charting all the tests and domains (INCLUDING OTHER CATEGORIES)
    theme_minimal() +
    ggtitle(paste0("Figure 2: Twenty-five most frequently reported tests and\ndomains in studies (N =",length(df$author), ') [ICF recoded]'))+
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      # plot.margin = unit(rep(-1,4), "cm"),
      plot.title = element_text(family = "Tahoma", face = "bold",hjust = 0.5, size = 10)
    ) +
    coord_polar() +
    geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
    
    # Add base line information
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
    geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
  
  p=grid.arrange(bottom = textGrob("HLCF= Higher-Level Cognitive Functioning \n \n MMSE= Mini-Mental State Examination; TMT= Trail Making Test; FIM = Functional Independence \n Measure; Digit sp = Wechsler's Digit Span; MoCA= Montreal Cognitive Assessment; MDRS= Mattis-Dementia \n Rating Scale; ROCF= Rey-Osterrieth Complex Figure; BIT= Behavioral Inattention Test; Log Mem= Wechsler's \n Logical Memory; RAVLT= Rey Auditory Verbal Learning Test; ADAS-cog= Alzheimer's Disease Assessment \n Scale; AMT= Abbreviated Mental Test; FAB= Frontal Assessment Battery", x = 0.5, y=1.5, hjust= .5, gp =  gpar (fontface = 3L, fontsize = 6)),p)
  
  
  
  ## saving plot
  ggsave(p, filename = "Figure2-TestsDomains-ICF.png", width =9, height = 8, limitsize = F)
  rm(data,base_data,grid_data,label_data,to_add,angle,empty_bar,number_of_bar)

    
  
}


####Plot 2A: tests and domains (NOT ICF)####
{
  
  data=data.frame(
    individual=c(as.character(tests.result$Var1[1:25]),
                 as.character(domains.result$Var1[1:25] ###activate this line for non ICF domains
                 # as.character(domains.result.icf$Var1[1:25]
                 )),
    group=c( rep('test', 25), rep('domain', 25)),
    value=c(tests.result$Freq[1:25],
            domains.result$Freq[1:25] ###activate this line for non ICF domains
            # domains.result.icf$Freq[1:25]
    )
  )

  # #The lines below are for mapping the 'OTHER' categories
  # data=data.frame(
  #   individual=c(as.character(tests.result$Var1[1:25]),'Other',as.character(domains.result$Var1[1:25]),'Other'),
  #   group=c( rep('test', 26), rep('domain', 26)),
  #   value=c(tests.result$Freq[1:25],sum(tests.result[26:length(tests.result$Var1),'Freq']),
  #           domains.result$Freq[1:25],sum(domains.result[26:length(domains.result$Var1),'Freq']))
  # )

  # Set a number of 'empty bar' to add at the end of each group
  empty_bar=2
  to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
  colnames(to_add) = colnames(data)
  to_add$group=rep(levels(data$group), each=empty_bar)
  data=rbind(data, to_add)
  data=data %>% arrange(group)
  data$id=seq(1, nrow(data))

  # Getting names position in Y for each label
  label_data=data
  number_of_bar=nrow(label_data)
  angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust<-ifelse( angle < -90, 1, 0)
  label_data$angle<-ifelse(angle < -90, angle+180, angle)

  # preparing a data frame for base lines
  base_data=data %>%
    group_by(group) %>%
    summarize(start=min(id), end=max(id) - empty_bar) %>%
    rowwise() %>%
    mutate(title=mean(c(start, end)))

  # preparing a data frame for grid (scales)
  grid_data = base_data
  grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start = grid_data$start - 1
  grid_data=grid_data[-1,]

  # Making the plot
  p=ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar

    geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +

    # Adding value lines. Done at the beginning to make sure barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 70, xend = start, yend = 70), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 130, xend = start, yend = 130), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +

    # Add text showing the value of each line
    annotate("text", x = rep(max(data$id),5), y = c(10,40,70,100,130), label = c("10", "40", "70", "100", '130') , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +

    geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
    ylim(-100,180) +
    # ylim(-230,430) + #this line is to expand the limits when charting all the tests and domains (INCLUDING OTHER CATEGORIES)
    theme_minimal() +
    ggtitle(paste0("Figure 2A: Twenty-five most frequently reported tests and\ndomains in studies (N =",length(df$author), ') [not ICF recoded]'))+
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      # plot.margin = unit(rep(-1,4), "cm"),
      plot.title = element_text(family = "Tahoma", face = "bold",hjust = 0.5, size = 10)
    ) +
    coord_polar() +
    geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +

    # Add base line information
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
    geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

  p=grid.arrange(bottom = textGrob("HLCF= Higher-Level Cognitive Functioning \n \n MMSE= Mini-Mental State Examination; TMT= Trail Making Test; FIM = Functional Independence \n Measure; Digit sp = Wechsler's Digit Span; MoCA= Montreal Cognitive Assessment; MDRS= Mattis-Dementia \n Rating Scale; ROCF= Rey-Osterrieth Complex Figure; BIT= Behavioral Inattention Test; Log Mem= Wechsler's \n Logical Memory; RAVLT= Rey Auditory Verbal Learning Test; ADAS-cog= Alzheimer's Disease Assessment \n Scale; AMT= Abbreviated Mental Test; FAB= Frontal Assessment Battery", x = 0.5, y=1.5, hjust= .5, gp =  gpar (fontface = 3L, fontsize = 6)),p)
  
  #saving plot
  ggsave(p, filename = "Figure2A-TestsDomains-NO-ICF.png", width =9, height = 8, limitsize = F)
  rm(data,base_data,grid_data,label_data,to_add,angle,empty_bar,number_of_bar)

}


####Plot 2B: tests and domains (ICF - one reviewer)####
{
### making full names
  tests.result$Var1<-as.character(tests.result$Var1)

# Create dataset only with the first 25 instruments and domains

data=data.frame(
  individual=c(as.character(tests.result$Var1[1:25]),
               as.character(domains.result.icf$Var1[1:25]
                            )),
  group=c( rep('test', 25), rep('domain', 25)),
  value=c(tests.result$Freq[1:25],
          domains.result.icf$Freq[1:25]
          )
)

data$individual=as.character(data$individual)
data[data$individual=='Cognitive Screening [executive + Attentional Tasks]','individual']='Cogn screening [EF + attention]'

# #The lines below are for mapping the 'OTHER' categories
# data=data.frame(
#   individual=c(as.character(tests.result$Var1[1:25]),'Other',as.character(domains.result$Var1[1:25]),'Other'),
#   group=c( rep('test', 26), rep('domain', 26)),
#   value=c(tests.result$Freq[1:25],sum(tests.result[26:length(tests.result$Var1),'Freq']),
#           domains.result$Freq[1:25],sum(domains.result[26:length(domains.result$Var1),'Freq']))
# )

# Set a number of 'empty bar' to add at the end of each group
empty_bar=2
to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) = colnames(data)
to_add$group=rep(levels(data$group), each=empty_bar)
data=rbind(data, to_add)
data=data %>% arrange(group)
data$id=seq(1, nrow(data))

# Getting names position in Y for each label
label_data=data
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# preparing a data frame for base lines
base_data=data %>%
  group_by(group) %>%
  summarize(start=min(id), end=max(id) - empty_bar) %>%
  rowwise() %>%
  mutate(title=mean(c(start, end)))

# preparing a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

# Making the plot
p=ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar

  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +

  # Adding value lines. Done at the beginning to make sure barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 70, xend = start, yend = 70), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 130, xend = start, yend = 130), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +

  # Add text showing the value of each line
  annotate("text", x = rep(max(data$id),5), y = c(10,40,70,100,130), label = c("10", "40", "70", "100", '130') , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +

  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  ylim(-100,180) +
  # ylim(-230,430) + #this line is to expand the limits when charting all the tests and domains (INCLUDING OTHER CATEGORIES)
  theme_minimal() +
  ggtitle(paste0("Figure 2B: Twenty-five most frequently reported tests and\ndomains in studies (N =",length(df$author), ') [ICF recoded - one reviewer]'))+
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    # plot.margin = unit(rep(-1,4), "cm"),
    plot.title = element_text(family = "Tahoma", face = "bold",hjust = 0.5, size = 10)
  ) +
  coord_polar() +
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +

  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

p=grid.arrange(bottom = textGrob("HLCF= Higher-Level Cognitive Functioning \n \n MMSE= Mini-Mental State Examination; TMT= Trail Making Test; FIM = Functional Independence \n Measure; Digit sp = Wechsler's Digit Span; MoCA= Montreal Cognitive Assessment; MDRS= Mattis-Dementia \n Rating Scale; ROCF= Rey-Osterrieth Complex Figure; BIT= Behavioral Inattention Test; Log Mem= Wechsler's \n Logical Memory; RAVLT= Rey Auditory Verbal Learning Test; ADAS-cog= Alzheimer's Disease Assessment \n Scale; AMT= Abbreviated Mental Test; FAB= Frontal Assessment Battery", x = 0.5, y=1.5, hjust= .5, gp =  gpar (fontface = 3L, fontsize = 6)),p)

#saving plot
ggsave(p, filename = "Figure2B-TestsDomains-ICF-OneReviewer.png", width =9, height = 8, limitsize = F)
rm(data,base_data,grid_data,label_data,to_add,angle,empty_bar,number_of_bar)


}

#####Plot 3: studies #############
{
  
  # source
  # http://t-redactyl.io/blog/2016/02/creating-plots-in-r-using-ggplot2-part-6-weighted-scatterplots.html
  ###Bubble Colors (different theme)
  # fill=c("#7fc97f","#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17")
  
  df=df[!is.na(df$author),]
  
  fill=c(
         "violetred2",#africa
         "darkgoldenrod1",#asia
         "skyblue",#europe
         "darkcyan",#multinational
         "navy",#north america
    "lightgoldenrodyellow",#oceania
         "olivedrab1"#south america
         )
  
  breaks=c(8,300,3000)
  # fill1 = c("#fbb4ae",
  #           "#b3cde3",
  #           "#ccebc5",
  #           "snow3",
  #           "#fed9a6",
  #           "#ffffcc",
  #           "#e5d8bd")
  # fill2=c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69')

  df$continent=factor(df$continent)
  df$continent.calc<-df$continent
  levels(df$continent.calc)=paste0(levels(df$continent),' (',round(table(df$continent)*100/length(df$author)),'%)')
  

  ###Basic Plot without any bubbles at all#
  p6<-ggplot(df, aes(x = year, y = study, size= sample, fill=continent.calc))
  # p6



  ##FULL BUBBLE PLOT##

  p7=p6+
    theme_bw() +
    theme() +
    geom_point(shape=21,alpha=.5)+
    ggtitle(paste0("   Figure 3: Sample Size and Origin of Included Studies (N =",length(df$author), ')')) +
    labs(x = "Publication year", y = "Study (Author, year)",
         size = "Sample size", fill = "Continent") +
    scale_x_continuous(breaks = seq(min(df$year)-2, max(df$year)+3, 3),limits = c(min(df$year),max(df$year)+1)) +
    scale_fill_manual(values = fill) +
    scale_size(range = c(1, 230), breaks = breaks) +
    coord_cartesian(ylim=c(-3, length(df$author)+length(df$author)/8))+
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.box = "horizontal",
          # legend.key.size = unit(.5, "cm"),
          legend.key.height = unit(.6,'cm'),
          legend.key.width = unit(1,'cm'),
          legend.box.just = 2,
          # legend.title.align = .5,
          legend.text=element_text(size=10),
          axis.line = element_line(size=0.1, colour = "gray40"),
          panel.grid.major = element_line(colour = "gray90", size = 0.1),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(), panel.background = element_blank(),
          plot.title = element_text(family = "Tahoma", face = "bold",hjust = 0.5, size = length(df$author)/4.5),
          text=element_text(family="Tahoma"),
          axis.text.x=element_text(colour="gray25", size = length(df$author)/7),
          axis.text.y=element_text(colour="gray25", size = length(df$author)/15.5),
          axis.title.x = element_text(colour="black", size = length(df$author)/5),
          axis.title.y = element_text(colour="black", size = length(df$author)/5))
  
  # font_import(prompt = F)  
  # loadfonts()
  # fonts()
  p=grid.arrange(bottom = textGrob("Note: please refer to our supplementary table for more details about each included study", x = .55, y=90, hjust= .5, gp =  gpar (fontface = 3L, fontsize = 35)),p7)
  ggsave(p,filename = "Figure3-Studies.png",dpi='retina',width =length(df$author)/5, height = length(df$author)/5, limitsize = F)
    rm(p6,p7,breaks,fill)
  
}

# #structuring names with count in parenthesis (ICF - after agreement)
{
  ##input data (table with raw data)
  x<-.icf.agreed
  
  x<-x[,which(names(x)=='id'):which(names(x)=='instrument')]
  
  ####shortening names of the most widely used instruments
  ####this can't go in a loop because each instrument name needs to be shortened in a a different way
  
  x[which(with(x,instrument=="Verbal Fluency")),'instrument']='VFT'
  x[which(with(x,instrument=="Digit sp")),'instrument']='DS'
  
  
  b=data.frame(table(x$instrument))
  b=b[order(b$Freq, decreasing = T),]
  b$Var1<-as.character(b$Var1)
  
  ###creating another dataframe to plot the 7 instruments and the domains they evaluate with the count of papers in parenthesis
  
  tools=b$Var1[1:7]
  .output=data.frame()
  for (i in seq_along(tools)) {
    
    #filter by instrument and table the domains and type of study
    t=x%>%filter(instrument==tools[i]) 
    t<-as.data.frame(table(t$domain, t$intervention))
    t<-t[order(t$Freq,decreasing = T),]
    t<-t[t$Freq!=0,]
    t$Var1<-as.character(t$Var1)
    t[t$Freq==1,'Var1']='other'
    a<-data.frame(t(table(t$Var1=='other', t$Var2)))
    a<-a[a$Var2==T,]
    names(a)=c('Var2','Var1','Freq')
    t<-rbind(t[t$Freq!=1,],a)
    t$Var1<-as.character(t$Var1)
    t[t=='TRUE','Var1']="other"
    t$instrument<-rep(paste0(b[i,1],' (',b[i,2],')'),length(t$Freq))
    
    a<-data.frame(Var1=c('',NA),
                  Var2=rep(NA,2),
                  Freq=rep(NA,2),
                  instrument=rep(paste0(b[i,1],' (',b[i,2],')'),2))
    
    .output=rbind(.output,t,a)
    
  }
  
  x=.output
  
  
  names(x)=c('individual','observation','value', 'group')
  x<-x[,c('individual', 'group','observation', 'value')]
  
  x<-x[order(x$group),]
  
  x$id<-factor(paste(x$individual,x$group))
  
  x$id<-factor(x$id,
               labels=c(1:length(levels(x$id))))
  id<-as.character(unique(x$id))
  x$id<-factor(x$id,
               levels=id,
               labels=c(1:length(levels(x$id))))
  
  x[which(x$individual==''),'individual']=NA
  
}

length(unique(x$individual))

#####Plot 4: domains by test (ICF - after agreement)####
{
  # preparing the data
  str(x)
  x$individual=as.character(x$individual)
  x[!is.na(x$individual) & x$individual=='other','individual']='Other'
  x[!is.na(x$individual) & x$individual=='[Not Reported]','individual']='Not reported'
  x$individual<-factor(x$individual)
  x$group<-factor(x$group)
  x$observation<-as.character(x$observation)
  x$id<-as.integer(x$id)
  data<-x
  names(data)[which(names(data)=='observation')]='Intervention'
  
  # Get the name and the y position of each label
  label_data= data %>% group_by(id, individual) %>% summarize(tot=sum(value))
  number_of_bar=nrow(label_data)
  angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust<-ifelse( angle < -90, 1, 0)
  label_data$angle<-ifelse(angle < -90, angle+180, angle)
  
  # prepare a data frame for base lines
  empty_bar=2
  base_data=data %>%
    group_by(group) %>%
    summarize(start=min(id), end=max(id) - empty_bar) %>%
    rowwise() %>%
    mutate(title=mean(c(start, end)))
  
  
  # prepare a data frame for grid (scales)
  grid_data = base_data
  grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start = grid_data$start - 1
  grid_data=grid_data[-1,]
  
  
  # Make the plot
  
  # The following lines separate out the "Other" category and makes it into different colors
  # data[which(with(data,individual=='other'&
  #                   intervention=="yes")),'intervention']='yes '
  # data[which(with(data,individual=='other'&
  #                   intervention=="no")),'intervention']='no '
  # 
  
  cols=c("lightskyblue","brown1", ### the next two colors are to highlight the 'Other' category when the lines immediately above are activated
         "palegreen3","seagreen")
  
  ## creating a dataset with the top 7 instruments
  .top7names=tests.result[1:7,1] #grabbing the names of the top 7 instrumnets
  .top7=data.frame(matrix(ncol = length(names(tests.cleaner)))) #empty matrix with the same number of columns as the tests.cleaner dataset
  names(.top7)=names(tests.cleaner)#assign column names
  ## populating matrix
  for (i in seq_along(.top7names)) {
  .top7=rbind(.top7,tests.cleaner[which(with(tests.cleaner,instrument==.top7names[i])),])
  
  }
  .top7=.top7[-1,]
  
  
  p=ggplot(data) +
    
    # Add the stacked bar
    geom_bar(aes(x=as.factor(id), y=value, fill=Intervention), stat="identity", alpha=0.5) +
    scale_fill_viridis(discrete=TRUE) +
    scale_fill_manual(values = cols) +
    
    # Add a val=100/75/50/25 lines. I do it at the beginning to make sure barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    
    
    # Add text showing the value of each 100/75/50/25 lines
    annotate("text", x = rep(max(data$id),5), y = c(0,20,40,60,80), label = c("0", "20","40", "60",'80') , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
    
    ylim(-100,110) +
    theme_minimal() +
    ggtitle(paste0(
      "Figure 4: Cognitive Domains Evaluated by the Top 7 Instruments \nN =" ,
      length(unique(.top7$id)),' studies, ',
      round(sum(tests.result$Freq[1:7])/length(sort(tests.cleaner$instrument)),digits = 2)*100 ,'% of the assessment events recorded \n[ICF domains]' )
      )+
    theme(
      legend.position = "bottom",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(family = "Tahoma", face = "bold",hjust = 0.5, size = 10)
      # plot.margin = unit(rep(-1,4), "cm")
    )+
    coord_polar() +
    
    # Add labels on top of each bar
    geom_text(data=label_data, aes(x=id, y=tot+3, label=individual, hjust=hjust), color="black", fontface="bold",alpha=1, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
    
    # Add base line information
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=.7 , inherit.aes = FALSE )  +
    geom_text(data=base_data, aes(x = title, y = -15, label=group), hjust=c(.5,.7,.8,.5,.2,.2,.2), colour = "black", alpha=0.8, size=2.5, fontface="bold", inherit.aes = FALSE)
  
  p=grid.arrange(bottom = textGrob("Note:  FIM = Functional Independence; HLCF= Higher-Level Cognitive Functioning; ICF-ch1= International Classification of Functioning \n and Disability, chapter 1; MMSE= Mini-Mental State Examination; Measure; DS = Wechsler's Digit Span; MoCA= Montreal Cognitive \n Assessment; TMT= Trail Making Test; VFT= Verbal Fluency Test", x = 0.5, y=3, hjust= .5, gp =  gpar (fontface = 3L, fontsize = 6)),p)
  
  ggsave(p,filename = "Figure4-DomainsByTest-ICF.png", width =9, height = 8, limitsize = F)
  # rm(x)
}

# #structuring names with count in parenthesis (ICF - one reviewer)
{
  ##input data (table with raw data)
  x<-.icf
  
  x<-x[,which(names(x)=='id'):which(names(x)=='instrument')]
  
  ####shortening names of the most widely used instruments
  ####this can't go in a loop because each instrument name needs to be shortened in a a different way
    
    x[which(with(x,instrument=="Verbal Fluency")),'instrument']='VFT'
    x[which(with(x,instrument=="Digit sp")),'instrument']='DS'
    
  
  b=data.frame(table(x$instrument))
  b=b[order(b$Freq, decreasing = T),]
  b$Var1<-as.character(b$Var1)
  
  ###creating another dataframe to plot the 7 instruments and the domains they evaluate with the count of papers in parenthesis

  tools=b$Var1[1:7]
  .output=data.frame()
  for (i in seq_along(tools)) {
    
    #filter by instrument and table the domains and type of study
    t=x%>%filter(instrument==tools[i]) 
    t<-as.data.frame(table(t$domain, t$intervention))
    t<-t[order(t$Freq,decreasing = T),]
    t<-t[t$Freq!=0,]
    t$Var1<-as.character(t$Var1)
    t[t$Freq==1,'Var1']='other'
    a<-data.frame(t(table(t$Var1=='other', t$Var2)))
    a<-a[a$Var2==T,]
    names(a)=c('Var2','Var1','Freq')
    t<-rbind(t[t$Freq!=1,],a)
    t$Var1<-as.character(t$Var1)
    t[t=='TRUE','Var1']="other"
    t$instrument<-rep(paste0(b[i,1],' (',b[i,2],')'),length(t$Freq))
    
    a<-data.frame(Var1=c('',NA),
                  Var2=rep(NA,2),
                  Freq=rep(NA,2),
                  instrument=rep(paste0(b[i,1],' (',b[i,2],')'),2))
    
    .output=rbind(.output,t,a)
    
  }
  
  x=.output


names(x)=c('individual','observation','value', 'group')
x<-x[,c('individual', 'group','observation', 'value')]

x<-x[order(x$group),]

x$id<-factor(paste(x$individual,x$group))

x$id<-factor(x$id,
             labels=c(1:length(levels(x$id))))
id<-as.character(unique(x$id))
x$id<-factor(x$id,
             levels=id,
             labels=c(1:length(levels(x$id))))

x[which(x$individual==''),'individual']=NA

}

#####Plot 4A: domains by test (ICF - one reviewer)####
{

# preparing the data
  str(x)
  x$individual=as.character(x$individual)
  x[!is.na(x$individual) & x$individual=='other','individual']='Other'
  x[!is.na(x$individual) & x$individual=='[Not Reported]','individual']='Not reported'
  x$individual<-factor(x$individual)
x$group<-factor(x$group)
x$observation<-as.character(x$observation)
x$id<-as.integer(x$id)
data<-x
names(data)[which(names(data)=='observation')]='Intervention'
# Get the name and the y position of each label
label_data= data %>% group_by(id, individual) %>% summarize(tot=sum(value))
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
empty_bar=2
base_data=data %>%
  group_by(group) %>%
  summarize(start=min(id), end=max(id) - empty_bar) %>%
  rowwise() %>%
  mutate(title=mean(c(start, end)))


# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]


# Make the plot
# The following lines separate out the "Other" category and makes it into different colors
# data[which(with(data,individual=='other'&
#                   intervention=="yes")),'intervention']='yes '
# data[which(with(data,individual=='other'&
#                   intervention=="no")),'intervention']='no '

cols=c("lightskyblue","brown1",
       "palegreen3","seagreen")


sum(tests.result$Freq[1:7])


p=ggplot(data) +

  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=Intervention), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_fill_manual(values = cols) +

  # Add a val=100/75/50/25 lines. I do it at the beginning to make sure barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +


  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),5), y = c(0,20,40,60,80), label = c("0", "20","40", "60",'80') , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +

  ylim(-100,110) +
  theme_minimal() +
  ggtitle( paste0(
    "Figure 4A: Cognitive Domains Evaluated by the Top 7 Instruments \nN =" ,
    length(unique(.top7$id)),' studies, ',
    round(sum(tests.result$Freq[1:7])/length(sort(tests.cleaner$instrument)),digits = 2)*100 ,'% of the assessment events recorded \n[ICF recoded by one reviewer]' )
    
  )+
 
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(family = "Tahoma", face = "bold",hjust = 0.5, size = 10)
    # plot.margin = unit(rep(-1,4), "cm")
  )+
  coord_polar() +

  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+3, label=individual, hjust=hjust), color="black", fontface="bold",alpha=1, size=3, angle= label_data$angle, inherit.aes = FALSE ) +

  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=.7 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -15, label=group), hjust=c(.5,.7,.8,.5,.2,.2,.2), colour = "black", alpha=0.8, size=2.5, fontface="bold", inherit.aes = FALSE)

p=grid.arrange(bottom = textGrob("Note:  FIM = Functional Independence; HLCF= Higher-Level Cognitive Functioning; ICF-ch1= International Classification of Functioning \n and Disability, chapter 1; MMSE= Mini-Mental State Examination; Measure; DS = Wechsler's Digit Span; MoCA= Montreal Cognitive \n Assessment; TMT= Trail Making Test; VFT= Verbal Fluency Test", x = 0.5, y=3, hjust= .5, gp =  gpar (fontface = 3L, fontsize = 6)),p)


ggsave(p, filename = "Figure4A-DomainsByTest-ICF.png", width =9, height = 8, limitsize = F)
rm(x)
}

# #structuring names with count in parenthesis (NO ICF)
{
    ##input data (table with raw data)
    x<-.noicf
    
    x<-x[,which(names(x)=='id'):which(names(x)=='instrument')]
    
    ####shortening names of the most widely used instruments
    ####this can't go in a loop because each instrument name needs to be shortened in a a different way
    
    x[which(with(x,instrument=="Verbal Fluency")),'instrument']='VFT'
    x[which(with(x,instrument=="Digit sp")),'instrument']='DS'
    
    
    b=data.frame(table(x$instrument))
    b=b[order(b$Freq, decreasing = T),]
    b$Var1<-as.character(b$Var1)
    
    ###creating another dataframe to plot the 7 instruments and the domains they evaluate with the count of papers in parenthesis
    
    tools=b$Var1[1:7]
    .output=data.frame()
    for (i in seq_along(tools)) {
      
      #filter by instrument and table the domains and type of study
      t=x%>%filter(instrument==tools[i]) 
      t<-as.data.frame(table(t$domain, t$intervention))
      t<-t[order(t$Freq,decreasing = T),]
      t<-t[t$Freq!=0,]
      t$Var1<-as.character(t$Var1)
      t[t$Freq==1,'Var1']='other'
      a<-data.frame(t(table(t$Var1=='other', t$Var2)))
      a<-a[a$Var2==T,]
      names(a)=c('Var2','Var1','Freq')
      t<-rbind(t[t$Freq!=1,],a)
      t$Var1<-as.character(t$Var1)
      t[t=='TRUE','Var1']="other"
      t$instrument<-rep(paste0(b[i,1],' (',b[i,2],')'),length(t$Freq))
      
      a<-data.frame(Var1=c('',NA),
                    Var2=rep(NA,2),
                    Freq=rep(NA,2),
                    instrument=rep(paste0(b[i,1],' (',b[i,2],')'),2))
      
      .output=rbind(.output,t,a)
      
    }
    
    x=.output
    
    
    names(x)=c('individual','observation','value', 'group')
    x<-x[,c('individual', 'group','observation', 'value')]
    
    x<-x[order(x$group),]
    
    x$id<-factor(paste(x$individual,x$group))
    
    x$id<-factor(x$id,
                 labels=c(1:length(levels(x$id))))
    id<-as.character(unique(x$id))
    x$id<-factor(x$id,
                 levels=id,
                 labels=c(1:length(levels(x$id))))
    
    x[which(x$individual==''),'individual']=NA
    
  
}
#####Plot 4B: domains by test (NO ICF)####
{


# preparing the data

str(x)
x$individual=as.character(x$individual)
x[!is.na(x$individual) & x$individual=='other','individual']='Other'
x[!is.na(x$individual) & x$individual=='[Not Reported]','individual']='Not reported'
x$individual<-factor(x$individual)
x$observation<-as.character(x$observation)
x$id<-as.integer(x$id)
data<-x
names(data)[which(names(data)=='observation')]='Intervention'

# Get the name and the y position of each label
label_data= data %>% group_by(id, individual) %>% summarize(tot=sum(value))
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
empty_bar=2
base_data=data %>%
  group_by(group) %>%
  summarize(start=min(id), end=max(id) - empty_bar) %>%
  rowwise() %>%
  mutate(title=mean(c(start, end)))


# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]


# Make the plot
# The following lines separate out the "Other" category and makes it into different colors
# data[which(with(data,individual=='other'&
#                   intervention=="yes")),'intervention']='yes '
# data[which(with(data,individual=='other'&
#                   intervention=="no")),'intervention']='no '

cols=c("lightskyblue","brown1",
       "palegreen3","seagreen")

p=ggplot(data) +

  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=Intervention), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_fill_manual(values = cols) +

  # Add a val=100/75/50/25 lines. I do it at the beginning to make sure barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +


  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),5), y = c(0,20,40,60,80), label = c("0", "20","40", "60",'80') , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +

  ylim(-100,110) +
  theme_minimal() +
  ggtitle(paste0(
    "Figure 4B: Cognitive Domains Evaluated by the Top 7 Instruments \nN =" ,
    length(unique(.top7$id)),' studies, ',
    round(sum(tests.result$Freq[1:7])/length(sort(tests.cleaner$instrument)),digits = 2)*100 ,'% of the assessment events recorded \n[raw domains]' ))+
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(family = "Tahoma", face = "bold",hjust = 0.5, size = 10)
    # plot.margin = unit(rep(-1,4), "cm")
  )+
  coord_polar() +

  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+3, label=individual, hjust=hjust), color="black", fontface="bold",alpha=1, size=3, angle= label_data$angle, inherit.aes = FALSE ) +

  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=.7 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -15, label=group), hjust=c(.5,.7,.8,.5,.3,.2,.2), colour = "black", alpha=0.8, size=2.5, fontface="bold", inherit.aes = FALSE)

p=grid.arrange(bottom = textGrob("Note:  FIM = Functional Independence; HLCF= Higher-Level Cognitive Functioning; ICF-ch1= International Classification of Functioning \n and Disability, chapter 1; MMSE= Mini-Mental State Examination; Measure; DS = Wechsler's Digit Span; MoCA= Montreal Cognitive \n Assessment; TMT= Trail Making Test; VFT= Verbal Fluency Test", x = 0.5, y=3, hjust= .5, gp =  gpar (fontface = 3L, fontsize = 6)),p)


ggsave(p, filename = "Figure4B-DomainsByTest-NO-ICF.png", width =9, height = 8, limitsize = F)
# rm(data,base_data,grid_data,label_data,to_add,angle,breaks,empty_bar,fill,id,n.obs,number_of_bar,seq.max,x)

}

#####FINAL PLOT - MOST TESTS AND DOMAINS#####
# #structuring names with count in parenthesis (ICF)
{
  ##input data (table with raw data)
  x<-.icf.agreed
  
  x<-x[,which(names(x)=='id'):which(names(x)=='instrument')]
  
  # Limiting the plot to the tests used more than 3 times
  tests.result[which(tests.result$Freq>3),1]
  
  
  ###creating a dataframe with the 40 most commonly used instruments
  names=tests.result[which(tests.result$Freq>3),1]
  .icf.plot=data.frame()
    for (i in seq_along(names)) {
    
    inst=x[x$instrument==names[i],]
    .icf.plot=rbind(.icf.plot,inst)
    
  }
  
  table(.icf.plot$instrument)
  x=.icf.plot
  
  ####shortening names of the most widely used instruments
  ####this can't go in a loop because each instrument name needs to be shortened in a a different way
  
  {
    x$instrument=tolower(x$instrument)  
    x[which(with(x,instrument=="verbal fluency")),'instrument']='vft'
    x[which(with(x,instrument=="digit sp")),'instrument']='ds'
    x[which(with(x,instrument=="boston naming test")),'instrument']='bnt'
    x[which(with(x,instrument=="block design test")),'instrument']='bdt'
    x[which(with(x,instrument=="clock drawing test")),'instrument']='cdt'
    x[which(with(x,instrument=="visual reproduct")),'instrument']='vrt'
    x[which(with(x,instrument=="token test")),'instrument']='token'
    x[which(with(x,instrument=="wisconsin card sort")),'instrument']='wcst'
    x[which(with(x,instrument=="log mem")),'instrument']='lm'
    x[which(with(x,instrument=="similarities")),'instrument']='sim'
    x[which(with(x,instrument=="adas-cog")),'instrument']='adas'
    x[which(with(x,instrument=="letter cancellation")),'instrument']='lct'
    x[which(with(x,instrument=="naming")),'instrument']='nam'
    x[which(with(x,instrument=="story recall")),'instrument']='sr'
    x[which(with(x,instrument=="10-word list learn")),'instrument']='wll'
    x[which(with(x,instrument=="corsi blocks test")),'instrument']='cbt'
    x[which(with(x,instrument=="figure copying")),'instrument']='fct'
    x[which(with(x,instrument=="word list recall & recog")),'instrument']='wlr'
    x[which(with(x,instrument=="digit symbol")),'instrument']='dsym'
    x[which(with(x,instrument=="grooved peg test")),'instrument']='gpt'
    x[which(with(x,instrument=="rocf [copy score]")),'instrument']='rocf'
    x[which(with(x,instrument=="rocf [delay score]")),'instrument']='rocf'
    x[which(with(x,instrument=="cns-vital signs test")),'instrument']='cns'
    x[which(with(x,instrument=="bells test")),'instrument']='bells'
    x[which(with(x,instrument=="mental control")),'instrument']='mc'
    x[which(with(x,instrument=="arithmetic")),'instrument']='arithm'
  }
  
  
  
  
   ###creating another dataframe to plot the instruments and the domains they evaluate with the count of papers in parenthesis
  names<-data.frame(table(x$instrument))[,1]
  b<-data.frame(table(x$instrument))
  output=data.frame()
  for (i in seq_along(names)) {
    
    #filter by instrument and table the domains and type of study
  t=x%>%filter(instrument==names[i]) 
  t<-as.data.frame(table(t$domain, t$intervention))
  t<-t[order(t$Freq,decreasing = T),]
  t<-t[t$Freq!=0,]
  t$Var1<-as.character(t$Var1)
  t[t$Freq==1,'Var1']='other'
  a<-data.frame(t(table(t$Var1=='other', t$Var2)))
  a<-a[a$Var2==T,]
  names(a)=c('Var2','Var1','Freq')
  t<-rbind(t[t$Freq!=1,],a)
  t$Var1<-as.character(t$Var1)
  t[t=='TRUE','Var1']="other"
  t$instrument<-rep(paste0(b[i,1],' (',b[i,2],')'),length(t$Freq))
  
  a<-data.frame(Var1=c('',NA),
                Var2=rep(NA,2),
                Freq=rep(NA,2),
                instrument=rep(paste0(b[i,1],' (',b[i,2],')'),2))
  
  output=rbind(output,t,a)
 
  }
  x=output
  
  
  
  names(x)=c('individual','observation','value', 'group')
  x<-x[,c('individual', 'group','observation', 'value')]
  
  
  x<-x[order(x$group),]
  
  x$id<-factor(paste(x$individual,x$group))
  
  x$id<-factor(x$id,
               labels=c(1:length(levels(x$id))))
  id<-as.character(unique(x$id))
  
  x$id<-factor(x$id,
               levels=id,
               labels=c(1:length(levels(x$id))))
  
  x[which(x$individual==''),'individual']=NA
}

#####FINAL PLOT 5: domains by test (ICF)####
{
  # preparing the data
  
  
  str(x)
  x$id=as.integer(as.character(x$id))
  
  str.last=max(x[grep('stroo',x$group),'id'])
  last=max(x$id)
  delta=last-str.last
  
  x$id=x$id + delta
  x$id=ifelse(x$id>last, x$id - last,x$id )
  x=x[order(x$id),]
  
  
  x$individual=as.character(x$individual)
  x[!is.na(x$individual) & x$individual=='other','individual']='Other'
  x[!is.na(x$individual) & x$individual=='[Not Reported]','individual']='Not reported'
  x$individual<-factor(x$individual)
  x$group<-factor(x$group)
  x$observation<-as.character(x$observation)
  x$id<-as.integer(x$id)
  
  
  data=x
  
  
  names(data)[which(names(data)=='observation')]='Intervention'
  
  # Get the name and the y position of each label
  label_data= data %>% group_by(id, individual) %>% summarize(tot=sum(value))
  number_of_bar=nrow(label_data)
  angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust<-ifelse( angle < -90, 1, 0)
  label_data$angle<-ifelse(angle < -90, angle+180, angle)
  
  # prepare a data frame for base lines
  empty_bar=2
  base_data=data %>%
    group_by(group) %>%
    summarize(start=min(id), end=max(id) - empty_bar) %>%
    rowwise() %>%
    mutate(title=mean(c(start, end)))
  base_data=base_data[order(base_data$start),]
  
  # prepare a data frame for grid (scales)
  
  grid_data = base_data
  grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start = grid_data$start - 1
  grid_data=grid_data[-1,]
  
  
  # Make the plot
  # The following lines separate out the "Other" category and makes it into different colors
  # data[which(with(data,individual=='other'&
  #                   intervention=="yes")),'intervention']='yes '
  # data[which(with(data,individual=='other'&
  #                   intervention=="no")),'intervention']='no '
  
  cols=c("lightskyblue","brown1","palegreen3","seagreen")
  
  half.1=trunc(length(base_data$group)/2)-1
  half.2=round(length(base_data$group)/2)-2
  
p= ggplot(data) +

    # Add the stacked bar
    geom_bar(aes(x=as.factor(id), y=value, fill=Intervention), stat="identity", alpha=0.5) +
    scale_fill_viridis(discrete=TRUE) +
    scale_fill_manual(values = cols) +
      # Add a value lines. I do it at the beginning to make sure barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +

    # Add text showing the value of each 100/75/50/25 lines
    annotate("text", x = rep(max(data$id),5), y = c(0,20,40,60,80), label = c("0", "20","40", "60",'80') , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +

    ylim(-330,150) +
    theme_minimal() +
    ggtitle( paste0(
      "Figure 5: Cognitive domains evaluated by each instrument \nN = " ,
      length(unique(.icf.plot$id)),' studies, ',
round((length(.icf.plot$instrument)/length(sort(tests.cleaner$instrument))),digits = 2)*100 ,'% of the assessment events recorded \n[ICF domains]' )
      
      )+
    theme(
      legend.position = "bottom",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(family = "Tahoma", face = "bold",hjust = 0.5, size = 20)
      # plot.margin = unit(rep(-1,4), "cm")
    )+
    coord_polar() +
    
    # Add labels on top of each bar
    geom_text(data=label_data, aes(x=id, y=tot+3, label=individual, hjust=hjust), color="black", fontface="bold",alpha=1, size=4, angle= label_data$angle, inherit.aes = FALSE ) +
    
    # Add base line information
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=2 , inherit.aes = FALSE )  +
    geom_text(data=base_data, aes(x = title, y = -15, label=group), hjust=c(.5,rep(1,half.1),.8,rep(.5,2),rep(0,half.2-2),.5), colour = "black", alpha=0.8, size=4.5, fontface="bold", inherit.aes = FALSE)
 
 # unique(data$group)
 
 p=grid.arrange(bottom = 'tmt= trail making test; token= token test; vft= verbal fluency test; vrt= visual reproduction test; wcst= wisconsin card sort test; wll= word list learning; wlr= word list recall & recognition; adas= alzheimers disease assessment scale; amt= abbreviated mental test;\n avlt= auditory verbal learning test; bdt= block design test; bit= behavioral inattention test; camcog= cambridge cognition examination; = cbtcorsi blocks test; cdt= clock drawing test; cowat= controlled oral word association test; dr= delayed recall test;\n ds= digit span test; dsym= digit symbol test; fab= frontal assessment battery; fct= figure copying test; fim= functional independence measure; gpt= grooved peg test; iq-code= informant questionnaire on cognitive decline in the elderly; lct= letter cancellation\n test; lm= logical memory test; mdrs= mattis-dementia rating scale; mmse= mini mental examination; moca= montreal cognitive assessment; nam= naming test; ravlt= rey auditory verbal learning test; rbans= repeatable battery for the assessment of neuropsychological\n status; rocf= rey-osterrieth complex figure test; sdmt= symbol digit modalities test; sim= similarities test; sis= stroke impact scale; sr= story recal test; stroop= stroop test (cwit)',p)
  
  ggsave(p,filename="Figure5-DomainsByTest-ICF.png", width =20, height = 18, limitsize = F, dpi = 'retina')
  
}

####plot 6: studies reporting data####
{
  fill=c(
    "violetred2",#africa
    "darkgoldenrod1",#asia
    "skyblue",#europe
    # "darkcyan",#multinational
    "navy",#north america
    "lightgoldenrodyellow",#oceania
    "olivedrab1"#south america
  )
breaks=c(8,300,3000)

descrip.yes<-df[which(df$descriptives=='yes'),]
descrip.yes$continent=factor(descrip.yes$continent)
descrip.yes$continent.calc<-descrip.yes$continent
levels(descrip.yes$continent.calc)=paste0(levels(descrip.yes$continent),' (',round(table(descrip.yes$continent)*100/length(descrip.yes$author)),'%)')



###Basic Plot without bubbles#
p6<-ggplot(descrip.yes, aes(x = year, y = study, size= sample, fill=continent.calc))



##FULL BUBBLE PLOT##

p7=p6+
  theme_bw() +
  theme() +
  geom_point(shape=21,alpha=.5)+
  ggtitle(paste0("Figure 6: Included Studies Reporting Cognitive Outcomes (N =",length(descrip.yes$author), ')')) +
  labs(x = "Publication year", y = "Study (Author, year)",
       size = "Sample size", fill = "Continent") +
  scale_x_continuous(breaks = seq(1999, 2020, 3),limits = c(2001,2018)) +
  scale_fill_manual(values = fill) +
  scale_size(range = c(1, 230), breaks = breaks) +
  coord_cartesian(ylim=c(-1, 110))+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        # legend.key.size = unit(.5, "cm"),
        legend.key.height = unit(.6,'cm'),
        legend.key.width = unit(1,'cm'),
        legend.box.just = 2,
        # legend.title.align = .5,
        legend.text=element_text(size=10),
        axis.line = element_line(size=0.2, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3", size = 0.2),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(family = "Tahoma", face = "bold",hjust = 0.5, size = 45),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 30),
        axis.text.y=element_text(colour="black", size = 22),
        axis.title.x = element_text(colour="black", size = 40),
        axis.title.y = element_text(colour="black", size = 40))
# p7

ggsave("Figure6-StudiesWithOutcomes.png", width =40, height = 40, limitsize = F,dpi = 'retina')
rm(p6,p7,breaks,fill,descrip.yes)
}




####Final calculations####
###ABSTRACT###
#included studies
length(df$author)
#intervention studies
table(df$intervention)
round(table(df$intervention)[1]/length(df$author),digits=2)
#participants 150 or less
round(table(df$sample<=150)[2]/length(df$author),digits=2)
#more than one assessment
round(length(which(table(tests.cleaner$id)>=2))/length(df$author), digits =2)
#descriptive data reported
table(df$descriptives)
levels(df$descriptives)=c('no','no','yes')
round(table(df$descriptives)[2]/length(df$author),digits = 2) # percentage
table(df$intervention, df$descriptives)
#instrument count
length(tests.result$Var1)
#domain count
length(domains.result$Var1)
#most frequent instruments
tests.result[1:7,]
length(unique(tests.cleaner[which(tests.cleaner$instrument=='MMSE'),'id']))*100/length(df$author)
length(unique(tests.cleaner[which(tests.cleaner$instrument=='TMT'),'id']))*100/length(df$author)
length(unique(tests.cleaner[which(tests.cleaner$instrument=='Verbal Fluency'),'id']))*100/length(df$author)
length(unique(tests.cleaner[which(tests.cleaner$instrument=='FIM'),'id']))*100/length(df$author)
length(unique(tests.cleaner[which(tests.cleaner$instrument=='Digit sp'),'id']))*100/length(df$author)
length(unique(tests.cleaner[which(tests.cleaner$instrument=='Stroop'),'id']))*100/length(df$author)
length(unique(tests.cleaner[which(tests.cleaner$instrument=='MoCA'),'id']))*100/length(df$author)
#longest follow-up within the first 12 months post stroke
table(df$fu<=12)*100/length(df$author)


###Results section####
#type of study
round(table(df$intervention)[1]/length(df$study.id), digits = 2)

round(table(df$intervention,df$descriptives)*100/length(df$author))

#studies by intervetion or not reporting cognitive outcomes
table(df$sample<25,df$descriptives)[2,]
table(df$sample>=25 & df$sample<50,df$descriptives)[2,]
table(df$sample>=50 & df$sample<100,df$descriptives)[2,]
table(df$sample>=100 & df$sample<150,df$descriptives)[2,]
table(df$sample>=150 & df$sample<500,df$descriptives)[2,]
table(df$sample>=500 & df$sample<1500,df$descriptives)[2,]
table(df$sample>=1500 & df$sample<4000,df$descriptives)[2,]
table(df$sample>=4000,df$descriptives)


# studies with less than 1 year FU
table(df$fu<=12)*100/length(df$author)
table(df$fu<=1,df$descriptives)[2,]
table(df$fu>1 & df$fu<=3,df$descriptives)[2,]
table(df$fu>3 & df$fu<=6,df$descriptives)[2,]
table(df$fu>6 & df$fu<=12,df$descriptives)[2,]
table(df$fu>12 & df$fu<=24,df$descriptives)[2,]
table(df$fu>24 & df$fu<=36,df$descriptives)[2,]
table(df$fu>36 & df$fu<=60,df$descriptives)[2,]
table(df$fu>60,df$descriptives)[2,]

# Counting instruments (results in percentage)
#Only one instrument used
length(which(table(tests.cleaner$id)==1))*100/length(df$author)
#Two or more instrument used
length(which(table(tests.cleaner$id)>=2))*100/length(df$author)
#study IDs with more than one assessment
tests.cleaner[which(table(tests.cleaner$id)>1),c('id')]


#screening tests
screening.tests<-tests.cleaner[which(with(tests.cleaner, instrument=="MMSE"|
                                            instrument=='FIM'|
                                            instrument=='MoCA'|
                                            instrument=='CAMCOG'|
                                            instrument=='TICS'|
                                            instrument=='AMT'|
                                            instrument=='WTAR'|
                                            instrument=='SBT'|
                                            instrument=='MMT'|
                                            instrument=='GDS'|
                                            instrument=='MDRS'|
                                            instrument=='IQ-CODE')),]


### percentage of studies using cognitive screenings
screening.tests=screening.tests[order(screening.tests$id),]
length(unique(screening.tests[,c('id')]))*100/length(df$author)
length(screening.tests$author)/length(tests.cleaner$author)

### studies using top 7 instruments
length(unique(tests.cleaner[which(with(tests.cleaner, 
                         instrument=="MMSE"|
                           instrument=='TMT'|
                           instrument=='Verbal Fluency'|
                           instrument=='FIM'|
                           instrument=='Digit sp'|
                           instrument=='Stroop'|
                           instrument=='MoCA')),'id']))
### percentage of studies using top 7 instruments
length(unique(sort(tests.cleaner[which(with(tests.cleaner, 
                        instrument=="MMSE"|
                        instrument=='TMT'|
                        instrument=='Verbal Fluency'|
                        instrument=='FIM'|
                        instrument=='Digit sp'|
                        instrument=='Stroop'|
                        instrument=='MoCA')),'id'])))*100/length(df$author)

### percentage of times top 7 instruments were used 
length(tests.cleaner[which(with(tests.cleaner, 
                                       instrument=="MMSE"|
                                         instrument=='TMT'|
                                         instrument=='Verbal Fluency'|
                                         instrument=='FIM'|
                                         instrument=='Digit sp'|
                                         instrument=='Stroop'|
                                         instrument=='MoCA')),'id'])/length(tests.cleaner$author)



### percentage use for top 7 instruments
length(unique(tests.cleaner[which(tests.cleaner$instrument=='MMSE'),'id']))*100/length(df$author)
length(unique(tests.cleaner[which(tests.cleaner$instrument=='TMT'),'id']))*100/length(df$author)
length(unique(tests.cleaner[which(tests.cleaner$instrument=='Verbal Fluency'),'id']))*100/length(df$author)
length(unique(tests.cleaner[which(tests.cleaner$instrument=='FIM'),'id']))*100/length(df$author)
length(unique(tests.cleaner[which(tests.cleaner$instrument=='Digit sp'),'id']))*100/length(df$author)
length(unique(tests.cleaner[which(tests.cleaner$instrument=='Stroop'),'id']))*100/length(df$author)
length(unique(tests.cleaner[which(tests.cleaner$instrument=='MoCA'),'id']))*100/length(df$author)



#Counting domains
length(table(domains.raw$domain))## raw domains
length(table(domains.clean$domain)) ## semantic categories
length(table(domains.clean.icf$domain)) ## ICF domains
table(domains.clean.icf[grep('\\(',domains.clean.icf$domain,invert = T),'domain'])

length(unique(domains.clean[which(domains.clean$domain=='General Cognition'),'id']))*100/length(df$author)
length(unique(domains.clean[which(domains.clean$domain=='Memory'),'id']))*100/length(df$author)
length(unique(domains.clean[which(domains.clean$domain=='Executive Function'),'id']))*100/length(df$author)
length(unique(domains.clean[which(domains.clean$domain=='Attention'),'id']))*100/length(df$author)
length(unique(domains.clean[which(domains.clean$domain=='Language Skills'),'id']))*100/length(df$author)
length(unique(domains.clean[which(domains.clean$domain=='Visual Skills'),'id']))*100/length(df$author)


#Domains evaluated by each instrument
dom.inst.icf=data.frame(table(.icf.agreed$instrument,.icf.agreed$domain))
dom.inst.icf$Var1=as.character(dom.inst.icf$Var1)
dom.inst.icf[dom.inst.icf$Var1=='Verbal Fluency','Var1']='VFT'
dom.inst.icf[dom.inst.icf$Var1=='Digit sp','Var1']='DS'
dom.inst.icf[which(with(dom.inst.icf,Var1=='MMSE')),c('Var2','Freq')]
dom.inst.icf[which(with(dom.inst.icf,Var1=='TMT')),c('Var2','Freq')]
dom.inst.icf[which(with(dom.inst.icf,Var1=='VFT')),c('Var2','Freq')]
dom.inst.icf[which(with(dom.inst.icf,Var1=='FIM')),c('Var2','Freq')]
dom.inst.icf[which(with(dom.inst.icf,Var1=='DS')),c('Var2','Freq')]
dom.inst.icf[which(with(dom.inst.icf,Var1=='Stroop')),c('Var2','Freq')]
dom.inst.icf[which(with(dom.inst.icf,Var1=='MoCA')),c('Var2','Freq')]


# number of cognitive domains (ICF coded) evaluated by top 7 instruments
length(unique(dom.inst.icf[which(with(dom.inst.icf,Var1=='MMSE'|
                          Var1=='TMT'|
                          Var1=='VFT'|
                          Var1=='FIM'|
                          Var1=='DS'|
                          Var1=='Stroop'|
                          Var1=='MoCA')),'Var2']))

# number of studies using the top 7 instruments
length(unique(tests.cleaner[which(with(tests.cleaner,instrument=='MMSE'|
                                        instrument=='TMT'|
                                        instrument=='VFT'|
                                        instrument=='FIM'|
                                        instrument=='DS'|
                                        instrument=='Stroop'|
                                        instrument=='MoCA')),'id']))

# observed agreement = 1 - (disagreements / total)
# sorting by count before calculating agreement

dom.inst.icf=dom.inst.icf[order(dom.inst.icf$Freq, decreasing = T),]

round(1-(sum(dom.inst.icf[which(with(dom.inst.icf,Var1=='MMSE')),c('Var2','Freq')][2])-
  dom.inst.icf[which(with(dom.inst.icf,Var1=='MMSE')),c('Var2','Freq')][1,2])/
  sum(dom.inst.icf[which(with(dom.inst.icf,Var1=='MMSE')),c('Var2','Freq')][2]),2)

round(1-(sum(dom.inst.icf[which(with(dom.inst.icf,Var1=='TMT')),c('Var2','Freq')][2])-
    dom.inst.icf[which(with(dom.inst.icf,Var1=='TMT')),c('Var2','Freq')][1,2])/
  sum(dom.inst.icf[which(with(dom.inst.icf,Var1=='TMT')),c('Var2','Freq')][2]),2)

round(1-(sum(dom.inst.icf[which(with(dom.inst.icf,Var1=='VFT')),c('Var2','Freq')][2])-
    dom.inst.icf[which(with(dom.inst.icf,Var1=='VFT')),c('Var2','Freq')][1,2])/
  sum(dom.inst.icf[which(with(dom.inst.icf,Var1=='VFT')),c('Var2','Freq')][2]),2)

round(1-(sum(dom.inst.icf[which(with(dom.inst.icf,Var1=='FIM')),c('Var2','Freq')][2])-
    dom.inst.icf[which(with(dom.inst.icf,Var1=='FIM')),c('Var2','Freq')][1,2])/
  sum(dom.inst.icf[which(with(dom.inst.icf,Var1=='FIM')),c('Var2','Freq')][2]),2)

round(1-(sum(dom.inst.icf[which(with(dom.inst.icf,Var1=='DS')),c('Var2','Freq')][2])-
    dom.inst.icf[which(with(dom.inst.icf,Var1=='DS')),c('Var2','Freq')][1,2])/
  sum(dom.inst.icf[which(with(dom.inst.icf,Var1=='DS')),c('Var2','Freq')][2]),2)

round(1-(sum(dom.inst.icf[which(with(dom.inst.icf,Var1=='Stroop')),c('Var2','Freq')][2])-
    dom.inst.icf[which(with(dom.inst.icf,Var1=='Stroop')),c('Var2','Freq')][1,2])/
  sum(dom.inst.icf[which(with(dom.inst.icf,Var1=='Stroop')),c('Var2','Freq')][2]),2)

round(1-(sum(dom.inst.icf[which(with(dom.inst.icf,Var1=='MoCA')),c('Var2','Freq')][2])-
    dom.inst.icf[which(with(dom.inst.icf,Var1=='MoCA')),c('Var2','Freq')][1,2])/
  sum(dom.inst.icf[which(with(dom.inst.icf,Var1=='MoCA')),c('Var2','Freq')][2]),2)


####table with what type of data available (table 4 in paper)
as.table(sort(round(table(df$data.status)*100/length(df$author),1)))

# saving in separate table
data.reported<-as.table(sort(table(df$data.status)))[2:6]

# is there the difference significant
chisq.test(data.reported[1:2])[1:10]
chisq.test(data.reported[1:3])[1:10]
chisq.test(data.reported[1:4])[1:10]
chisq.test(data.reported)[1:10]

#####instruments with data over time (mean and SD for the top 10 instruments)
sort(table((tests.cleaner[which(tests.cleaner$descriptives=='yes'),'instrument'])),decreasing = T)[1:10]


tests.cleaner[which(with(tests.cleaner, instrument=='MMSE' &
                            descriptives=='yes'|
                            instrument=='FIM' &
                            descriptives=='yes'|
                            instrument=='TMT' &
                            descriptives=='yes'|
                            instrument=='Verbal Fluency Test' &
                            descriptives=='yes'|
                            instrument=='Stroop' &
                            descriptives=='yes'|
                            instrument=='Digit Span' &
                            descriptives=='yes'|
                            instrument=='MoCA' &
                            descriptives=='yes'|
                            instrument=='Block Design Test' &
                            descriptives=='yes'|
                            instrument=='CAMCOG' &
                            descriptives=='yes'|
                            instrument=='IQ-CODE' &
                            descriptives=='yes')),c('instrument','author','year')]

unique(tests.cleaner[which(with(tests.cleaner,instrument=='MoCA' &
                           descriptives=='yes')),c('instrument','author','year')])


#####table with recoding of domains (extracted directly from code - supplementary table)

###final table with everything (extracted from final excel - supplementary table)

