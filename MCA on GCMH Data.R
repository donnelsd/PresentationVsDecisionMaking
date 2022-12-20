#Install the required packages
#install.packages('ca') #Only install if you haven't done so yet

library("xlsx")
library('ca') #Load the package

rm(list = ls())

for (i in 1:4){

#Load data from GCMH example
  
#2x2
if (i == 1){  
data = read.xlsx('C:\\Users\\file Path Here', sheet = 1)
} 

  
  
#3x3
if (i == 2){  
data = read.xlsx('C:\\Users\\file Path Here', sheet = 1)
}
  
#4x4  
if (i == 3){ 
data = read.xlsx('C:\\Users\\file Path Here', sheet = 1)
}
  
#5x5  
if (i == 4){  
data = read.xlsx('C:\\Users\\file Path Here', sheet = 1)
}

data=data[1:219,]
  
###Meta data -- Change bigK to num of players depending on example used
bigI = 3 #number of presentations
#bigJ = 4 #Number of actionsn in the game
bigJ = ncol(data)-2 #determine number of actions automatically

#bigK = 5 #For 1st example
#bigK = 12 #Number of strata for 2nd/3rd example (i.e., number of people who played)
bigK = data[nrow(data),1] #find me the number of people

#Transform data into something readable by the ca package
row = 0 #Initialize variable @ 0
mca_table = matrix( rep(0,bigI*bigK), nrow=bigK)
for (datarow in 1:nrow(data)){
  if (datarow %% bigI == 1){  #Take modulo of datarow with num of presentations... if no remainder then increment row
    row = row + 1 #Found a new player, so make a new row
    col =1  #Go back to first column for 1st presentation
  }
  responsebinary = data[datarow,3:ncol(data)] != 0 # Binary indicators for the actions, see which is selected
  mca_table[row,col] = which(responsebinary == TRUE) #Provide action index that was selected
  col = col+1
}

#To see what the data looks like for entry into the mca function -- Look at the mca_table
mca_table #Just to display it for referece; columns are presentations, rows are people, cells is num of action taken

#Perform MCA calculation
mca_df = as.data.frame(mca_table) #Turn into a dataframe variable to use the mjca function -- same values just different structure of variable


#these two ifs are a holdover from old code... laziness is why this is why it is
if (i == 2| i==3|i==4){
mca_calc = mjca(mca_df, lambda = "Burt")
}

if (i==1){
mca_calc = mjca(mca_df,lambda = "Burt") # Uses lamabda = adjusted as default -- Uses Burt matrix but w/ formula to adjust inertia explained in Abdi
}

c1 <-rgb(255,0,0,max = 255)

#if(i==1){
#plot.mjca(mca_calc, xlab= "2x2 V1:A V2:A V3:A") 
#plot.mjca(mca_calc)
#points(-0.5, -1.0,pch=8, col=c1)

#}
if(i==1){
  #plot.mjca(mca_calc, main= "2x2") #
  plot.mjca(mca_calc, xlim=c(-1,1), collabels=c("both"), pch=15,cex.lab=1.5, cex.axis=1.5)
}


if(i==2){
  #plot.mjca(mca_calc, main= "3x3") #
  plot.mjca(mca_calc, collabels=c("both"), pch=15,cex.lab=1.5, cex.axis=1.5)
}

if(i==3){
  #plot.mjca(mca_calc, main= "4x4") #
  plot.mjca(mca_calc, collabels=c("both"), pch=15,cex.lab=1.5, cex.axis=1.5)
}

#par(cex=1.25)
if(i==4){
  #plot.mjca(mca_calc, main= "5x5") #
  plot.mjca(mca_calc, collabels=c("both"), pch=15,cex.lab=1.5, cex.axis=1.5)
}



}

