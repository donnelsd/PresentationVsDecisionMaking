#install.packages("xlsx") 
#install.packages("openxlsx", dependencies = TRUE) #WNC add-- xlsc not working on my machine
library(xlsx) 
#install.packages("nonpar")
library(nonpar)
#library(openxlsx) #WNC-add

rm(list = ls())

#2x2 -- Replace with your file path
data = read.xlsx('C:\\Users\\file Path Here', sheet = 1)
#Form data into a table for Cochran's Q test
data=data[1:219,]
data = matrix(data$Option.1,ncol=3, byrow=TRUE)
cochrans.q(data, alpha =0.05)

data = read.xlsx('C:\\Users\\file Path Here', sheet = 1)
#Form data into a table for Cochran's Q test
data=data[1:219,]
data<-data[!(data$Presentation=="Visual"),]
data = matrix(data$Option.1,ncol=2, byrow=TRUE)
cochrans.q(data, alpha =0.05)



for(l in 2:4){

#3x3
if (l == 2){  
  data = read.xlsx('C:\\Users\\file Path Here', sheet = 1)
#  data<-data[!(data$Presentation=="A&V"),]
 # data<-data[!(data$Presentation=="Visual"),]
  #data<-data[!(data$Presentation=="Audio"),]
  
  
}

#4x4  
if (l == 3){ 
  data = read.xlsx('C:\\Users\\file Path Here', sheet = 1)
#    data<-data[!(data$Presentation=="A&V"),]
  # data<-data[!(data$Presentation=="Visual"),]
 #data<-data[!(data$Presentation=="Audio"),]
}

#5x5  
if (l == 4){  
  data = read.xlsx('C:\\Users\\file Path Here', sheet = 1)
  # data<-data[!(data$Presentation=="A&V"),]
  # data<-data[!(data$Presentation=="Visual"),]
#  data<-data[!(data$Presentation=="Audio"),]
}

#data=data[1:146,]
#data=data[1:152,]
data=data[1:219,]
  


  
###Meta data -- Change bigK to num of players depending on example used
#str(data)  #Shows the "type" of variable "data" is (i.e., datafram with integer columns)
bigI = 3 #number of presentations
#bigJ = 4 #Number of actionsn in the game
bigJ = ncol(data)-2
#bigK = 5 #For 1st example
#bigK = 100 #Number of strata for 2nd/3rd/4th example (i.e., number of people who played)
bigK = data[nrow(data),1]
nplusplusk = bigI #Number of times games played by a person
niplusk = 1 #For all presentations one action is taken

#ID mu value vectors
mu_k = array(rep( 0, len=(bigJ-1)*(bigI-1)*bigK), dim=c(bigI-1, bigJ-1,bigK)) #Initialize 3D with all zero entries
nvec = mu_k #Initialize to zero as well


for (k in 1:bigK){  #Loop thru all ijk values
 for (i in 1:(bigI-1)){
  for (j in 1:(bigJ-1)){
    PlayerRow = data$Person==k  #ID rows having strata of interest
    RelevantStrat = data[data$Person==k,] #Make new data for strata for calculation
    nvec[i,j,k] = RelevantStrat[i,j+2] 
    nplusjk = sum(RelevantStrat[,j+2])  #Find number of times players played action j; added j by 2 b/c of my tables form
    mu_k[i,j,k]= niplusk*nplusjk /nplusplusk
  
  }
 }
}

#Find covariance matrices
V_k = array(rep( 0, len=((bigJ-1)^2)*((bigI-1)^2)*bigK), dim=c((bigI-1)*(bigJ-1), (bigI-1)*(bigJ-1),bigK)) #Initialize set of covariance matrices to zero
for (k in 1:bigK){  #Loop thru all ijk values
  row=0 #Initialize row to zero for each person
  PlayerRow = data$Person==k #Find strata dealing with specified person
  RelevantStrat = data[data$Person==k,] #Make new data for strata for calculation
  denom = nplusplusk*nplusplusk*(nplusplusk-1)
  for (i in 1:(bigI-1)){
    for (j in 1:(bigJ-1)){
      row= row+1
      col= 0
      for (iprime in 1:(bigI-1)){
        for (jprime in 1:(bigJ-1)){
          deltajj=0  #Set both indicators to zero for defauly
          deltaii=0
          col = col +1 #Increment col of covariance matrix
          nplusjk = sum(RelevantStrat[,j+2]) #ID this
          nplusjprimek = sum(RelevantStrat[,jprime+2]) #ID this
          if (j == jprime){
            deltajj=1 
          } 
          if (i == iprime){
            deltajj=1 
          } 
          V_k[row,col,k]= (deltaii*nplusplusk - niplusk)*nplusjk*(deltajj*nplusplusk - nplusjprimek)/denom
        }
      }
    }
  }
}

#Sum over the mu and V values to find their vectors and matrices
mu = rowSums(mu_k, dims =2) #Sum over the third dimension
mu = c(t(mu))  #Layout into a vector vs a matrix
V = as.matrix(rowSums(V_k, dims=2))  #Ensure data type is a matrix
n = rowSums(nvec,dims=2)  #Sum over strata (i.e., 3rd dimension)
n = c(t(n))   #Layout into a vector vs. a matrix


if (l==2){
#Calculate the GCMH test statistic
GCMHstat3x3 = t((n-mu))%*% solve(V)%*% (n-mu) #solve gives the inverse
#Find the p-value
pvalue3x3 = 1-pchisq(GCMHstat3x3,(bigI-1)*(bigJ-1))
}

if (l==3){
#Calculate the GCMH test statistic
GCMHstat4x4 = t((n-mu))%*% solve(V)%*% (n-mu) #solve gives the inverse
#Find the p-value
pvalue4x4 = 1-pchisq(GCMHstat4x4,(bigI-1)*(bigJ-1))
}


if (l==4){
#Calculate the GCMH test statistic
GCMHstat5x5 = t((n-mu))%*% solve(V)%*% (n-mu) #solve gives the inverse
#Find the p-value
pvalue5x5 = 1-pchisq(GCMHstat5x5,(bigI-1)*(bigJ-1))
}
#Calculate the GCMH test statistic
GCMHstat = t((n-mu))%*% solve(V)%*% (n-mu) #solve gives the inverse

#Find the p-value
pvalue = 1-pchisq(GCMHstat,(bigI-1)*(bigJ-1))

#To clear variables if you like when done
#rm(list=ls())

}
