library(tidyr)
library(data.table)
library(dplyr)

####################
###Dataset 1-Main###
####################

toronto_cc <- fread(input = "./toronto_cc.csv")

toronto_cc <- toronto_cc[,-1]




###############
###Dataset 2###
###############

num_wait_child <- fread(input='./num_wait_child.csv')

num_wait_child <- num_wait_child[,-1]

###############
###Dataset 3###
###############

num_wait_child$Date <- as.Date(num_wait_child$Date)

num_wait_child2 <- fread(input='./num_wait_child2.csv')

num_wait_child2 <- num_wait_child2[,-1]

num_wait_child2$Date <- as.Date(num_wait_child2$Date)

###############
###Dataset 4###
###############

numSpace_toronto <- fread(input = './numSpace_toronto.csv')

numSpace_toronto <- numSpace_toronto[,-1]

numSpace_toronto2 <- filter(numSpace_toronto, 
                           district %in% c('Etobicoke York',
                                       'North York',
                                        'Scarborough',
                                        'Toronto East York'))

numSpace_toronto_only <- filter(numSpace_toronto, district=='City of Toronto')


###################################
####Google chart specific options## 
###################################

my_options <- list(width="600px", height="400px",
                   title="Child Spaces vs Number of Children on Wait List",
                   titleTextStyle="{fontSize: 14, italic: true}",
                   hAxis="{title: 'Year'}",
                   vAxis="{title: 'Number of Children and Child Spaces', maxValue: 75000,  minValue: 0}",
                   legend="{position: 'bottom'}",
                   selectionMode='multiple',
                   explorer="{ actions: ['dragToZoom', 'rightClickToReset'],
                              keepInBounds: true}" 
                  )






