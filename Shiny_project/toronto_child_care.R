library(zoo)
library(tidyr)
library(dplyr)

toronto_data<-read.csv("./NYC_Data_Science_Academy/Toronto_child_care/child-care.csv", stringsAsFactors = FALSE)

#####################################################
##############Data cleaning##########################
#####################################################

##First, I will change the child group space names to clearly defined names. There are other columns, whose names are also changed.
names(toronto_data)[c(2:3,7, 10, 12:17)]=c('Name', 'Type', 'postal_code', 'Building.Type', 'Infant', 'Toddler', 'Preschooler', 'Kindergarten', 'Gradelevel', 'Total')

##I will see whether there is any missing value, and if so, at which column and row.
sum(is.na(toronto_data))  #there is no missing value in the dataset. 


##I will change the explanations in "Type" column for shorter ones. 
toronto_data %>% group_by(Type) %>% summarise(Total=n())

type_fnc=function(x){
    if(x=='Commercial Agency'){
      return('Commercial')
    }else if(x=='Non Profit Agency'){
      return('Non-Profit')
    }else{
      return('City-Operated')
    }
}
toronto_data=toronto_data %>% mutate(Type=sapply(Type, type_fnc))

##To see how many different building types in this dataset for further research questions.

toronto_data %>% group_by(Building.Type) %>% summarise(Total=n())

##Adding a new column to list district name.

district_list=function(x){
  if(x %in% c(1:7, 11:13, 17)){
    return('Etobicoke York')
  }else if(x %in% c(8:10, 15,16, 23:26, 33, 34)){
    return('North York')
  }else if(x %in% c(14, 18:22, 27:32)){
    return('Toronto East York')
  }else if (x %in% c(35:44)){
    return('Scarborough')
  }
}
toronto_data<- toronto_data %>% mutate(District=sapply(ward, district_list))

toronto_data <- setnames(toronto_data, tolower(names(toronto_data)))

toronto_data <- rename(toronto_data, 
                      Infant=infant, 
                      Toddler=toddler, 
                      Preschooler=preschooler, 
                      Kindergarten=kindergarten,
                      Gradelevel=gradelevel,
                      Total=total)

##Street names will be changed to lowercase. 

toronto_data$street=tolower(toronto_data$street)

##subsidy entries will be changed to 'Yes' and 'No'.
toronto_data$subsidy=ifelse(toronto_data$subsidy=='Y','Yes','No')

##Drop 'unit' column as there is very few entries filled. 

toronto_data<-toronto_data[,-6]


##Save this dataset to a new data frame name.
write.csv(toronto_data, file='./NYC_Data_Science_Academy/Toronto_child_care/toronto_cc.csv')



#######################################################################################
#####Monthly data for number of child care spaces in Toronto for the years 2013-2017.##
#######################################################################################

num_child <- read.csv("./NYC_Data_Science_Academy/Toronto_child_care/Toronto__numberof_child_care_spaces.csv", stringsAsFactors = FALSE)


###Monthly data for number in the waitlist for child care fee subsidy in Toronto for the years 2007-2017.
wait_child <- read.csv("./NYC_Data_Science_Academy/Toronto_child_care/Toronto_waitlist_for_fee_subsidy.csv", stringsAsFactors = FALSE)



###Joining two tables for number of spaces.
wait_same_period <- wait_child %>% filter(Year %in% c(2013:2017))

num_wait_child <- inner_join(num_child, wait_same_period, by=c('Year', 'Period'))

num_wait_child <- num_wait_child %>% select(Year, Period, Value.x, Value.y)

##change the column names.

names(num_wait_child)[c(3,4)]=c('Child_Care_Spaces', 'Wait_List_for_Fee_Subsidy')

#######We have to create a new column for dates. 

##First, I will convert months to numeric sequences. 

month_num=function(x){
    if(x=='Jan'){
      return('01')
    }else if(x=='Feb'){
      return('02')
    }else if(x=='Mar'){
      return('03')
    }else if(x=='Apr'){
      return('04')
    }else if(x=='May'){
      return('05')
    }else if(x=='Jun'){
      return('06')
    }else if(x=='Jul'){
      return('07')
    }else if(x=='Aug'){
      return('08')
    }else if(x=='Sep'){
      return('09')
    }else if(x=='Oct'){
      return('10')
    }else if(x=='Nov'){
      return('11')
    }else if(x=='Dec'){
      return('12')
    }
}
num_wait_child <- num_wait_child %>% mutate(Period=sapply(Period, month_num))

##Now, we will create a new column that includes both year and month.

num_wait_child <- unite(num_wait_child, 'Date', Year, Period, sep='-', remove = FALSE)

##if you check the class of Date column, you will see that it is character. 
##Now,we will add day in each row for Date column and convert that column to date format.

num_wait_child <- num_wait_child %>% mutate(Date=paste(Date, '01', sep='-'))
num_wait_child$Date<-as.Date(num_wait_child$Date)

write.csv(num_wait_child, file='num_wait_child.csv')


#####

num_wait_child2 <- gather(num_wait_child, key='Space_and_Wait', value='Number',4:5)
num_wait_child2$Date<-as.Date(num_wait_child$Date)

write.csv(num_wait_child2, file='num_wait_child2.csv')



#########################################################
################Data for Key Insights########################
#########################################################

numSpace_toronto <- toronto_cc %>% group_by(district, type, subsidy) %>% summarise(Total=sum(Total), tot.infant=sum(Infant), tot.toddler=sum(Toddler), tot.presch=sum(Preschooler), tot.kgarten=sum(Kindergarten),tot.grade=sum(Gradelevel))

city_space <- numSpace_toronto %>% group_by(type, subsidy) %>% summarise(Total=sum(Total), tot.infant=sum(tot.infant), tot.toddler=sum(tot.toddler), tot.presch=sum(tot.presch), tot.kgarten=sum(tot.kgarten),tot.grade=sum(tot.grade))

city_space <- cbind('district'=rep('City of Toronto', 5), city_space)

l=list(numSpace_toronto, city_space)
numSpace_toronto <- rbindlist(l, use.names = TRUE)
names(numSpace_toronto)[5:9]=c('Infant', 'Toddler', 'Preschooler', 'Kindergarten', 'Gradelevel')

write.csv(numSpace_toronto, file='numSpace_toronto.csv')


