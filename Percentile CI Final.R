#library(xlsx) 
library(ggplot2)
#library(kdensity)

library(plotrix)
library("readxl")

rm(list = ls())


boot_strapped_data = read_excel('C:\\Users\\file Path Here', sheet = 1)


A22=boot_strapped_data$A22
A33=boot_strapped_data$A33
A44=boot_strapped_data$A44
A55=boot_strapped_data$A55

V22=boot_strapped_data$V22
V33=boot_strapped_data$V33
V44=boot_strapped_data$V44
V55=boot_strapped_data$V55

AV22=boot_strapped_data$AV22
AV33=boot_strapped_data$AV33
AV44=boot_strapped_data$AV44
AV55=boot_strapped_data$AV55


A22t=0.27
A33t=0.52
A44t=0.48
A55t=0.74

V22t=1.65
V33t=2.09
V44t=1.92
V55t=1.29

AV22t=0.00
AV33t=2.50
AV44t=2.02
AV55t=1.18



#PCI to compare modality across scenarios
alpha=.10

m=6
adjalpha=alpha/m

UP=10000*(1-(adjalpha/2))
LP=10000*(adjalpha/2)


A22L=sort(A22)[LP]
A22U=sort(A22)[UP]
A33L=sort(A33)[LP]
A33U=sort(A33)[UP]
A44L=sort(A44)[LP]
A44U=sort(A44)[UP]
A55L=sort(A55)[LP]
A55U=sort(A55)[UP]

V22L=sort(V22)[LP]
V22U=sort(V22)[UP]
V33L=sort(V33)[LP]
V33U=sort(V33)[UP]
V44L=sort(V44)[LP]
V44U=sort(V44)[UP]
V55L=sort(V55)[LP]
V55U=sort(V55)[UP]

AV22L=sort(AV22)[LP]
AV22U=sort(AV22)[UP]
AV33L=sort(AV33)[LP]
AV33U=sort(AV33)[UP]
AV44L=sort(AV44)[LP]
AV44U=sort(AV44)[UP]
AV55L=sort(AV55)[LP]
AV55U=sort(AV55)[UP]

#Audio only

x <- 1:4
F <- c(A22t,A33t,A44t,A55t)
L <- c(A22L, A33L, A44L, A55L)
U <- c(A22U, A33U, A44U, A55U)



require(plotrix)
plotCI(x, F, ui=U, li=L, main="",xaxt="n",xlab="Scenario Size", ylab=~tau,ylim=c(0,5),
       pch=16,cex.lab=1.5, cex.axis=1.5)
axis(1,                         # Define x-axis manually
     at = 1:4,
     labels = c("2x2",
                "3x3",
                "4x4",
                "5x5"), cex.axis=1.5)
axis(2, cex.axis=1.5
)

#verbal only
x <- 1:4
F <- c(V22t,V33t,V44t,V55t)
L <- c(V22L, V33L, V44L, V55L )
U <- c(V22U, V33U, V44U, V55U)


require(plotrix)
plotCI(x, F, ui=U, li=L, main="",xaxt="n",xlab="Scenario Size", ylab=~tau,ylim=c(0,5),
       pch=16,cex.lab=1.5, cex.axis=1.5)
axis(1,                         # Define x-axis manually
     at = 1:4,
     labels = c("2x2",
                "3x3",
                "4x4",
                "5x5"), cex.axis=1.5)
axis(2, cex.axis=1.5
)



#A&V
x <- 1:4
F <- c(AV22t,AV33t,AV44t,AV55t)
L <- c(AV22L, AV33L, AV44L, AV55L)
U <- c(AV22U, AV33U, AV44U, AV55U)


require(plotrix)
plotCI(x, F, ui=U, li=L, main="",xaxt="n",xlab="Scenario Size", ylab=~tau,ylim=c(0,5),
       pch=16,cex.lab=1.5, cex.axis=1.5)
axis(1,                         # Define x-axis manually
     at = 1:4,
     labels = c("2x2",
                "3x3",
                "4x4",
                "5x5"), cex.axis=1.5)
axis(2, cex.axis=1.5
)

#PCI to compare modality across Modalities
alpha=.10

m=3
adjalpha=alpha/m

UP=10000*(1-(adjalpha/2))
LP=10000*(adjalpha/2)


A22L=sort(A22)[LP]
A22U=sort(A22)[UP]
A33L=sort(A33)[LP]
A33U=sort(A33)[UP]
A44L=sort(A44)[LP]
A44U=sort(A44)[UP]
A55L=sort(A55)[LP]
A55U=sort(A55)[UP]

V22L=sort(V22)[LP]
V22U=sort(V22)[UP]
V33L=sort(V33)[LP]
V33U=sort(V33)[UP]
V44L=sort(V44)[LP]
V44U=sort(V44)[UP]
V55L=sort(V55)[LP]
V55U=sort(V55)[UP]

AV22L=sort(AV22)[LP]
AV22U=sort(AV22)[UP]
AV33L=sort(AV33)[LP]
AV33U=sort(AV33)[UP]
AV44L=sort(AV44)[LP]
AV44U=sort(AV44)[UP]
AV55L=sort(AV55)[LP]
AV55U=sort(AV55)[UP]


#2x2
x <- 1:3
F <- c(A22t,V22t,AV22t)
L <- c(A22L, V22L, AV22L)
U <- c(A22U, V22U, AV22U)

require(plotrix)
plotCI(x, F, ui=U, li=L, main="", xaxt="n", xlab="Modality", ylab=~tau,ylim=c(0,5),
       pch=16,cex.lab=1.5, cex.axis=1.5)
axis(1,                         # Define x-axis manually
     at = 1:3,
     labels = c("   Audio-only",
                "Visual-only",
                "Audio & Visual       "), cex.axis=1.5)
axis(2, cex.axis=1.5
)

#3x3
x <- 1:3
F <- c(A33t,V33t,AV33t)
L <- c(A33L, V33L, AV33L)
U <- c(A33U, V33U, AV33U)

require(plotrix)
plotCI(x, F, ui=U, li=L,main="", xaxt="n", xlab="Modality", ylab=~tau,ylim=c(0,5),
       pch=16,cex.lab=1.5, cex.axis=1.5)
axis(1,                         # Define x-axis manually
     at = 1:3,
     labels = c("   Audio-only",
                "Visual-only",
                "Audio & Visual       "), cex.axis=1.5)
axis(2, cex.axis=1.5
)

#4x4

x <- 1:3
F <- c(A44t,V44t,AV44t)
L <- c(A44L, V44L, AV44L)
U <- c(A44U, V44U, AV44U)

require(plotrix)
plotCI(x, F, ui=U, li=L,main="", xaxt="n", xlab="Modality", ylab=~tau,ylim=c(0,5),
       pch=16,cex.lab=1.5, cex.axis=1.5)
axis(1,                         # Define x-axis manually
     at = 1:3,
     labels = c("   Audio-only",
                "Visual-only",
                "Audio & Visual       "), cex.axis=1.5)
axis(2, cex.axis=1.5
)

#5x5

x <- 1:3
F <- c(A55t,V55t,AV55t)
L <- c(A55L, V55L, AV55L)
U <- c(A55U, V55U, AV55U)

require(plotrix)
plotCI(x, F, ui=U, li=L, main="", xaxt="n", xlab="Modality", ylab=~tau,ylim=c(0,5),
       pch=16,cex.lab=1.5, cex.axis=1.5)
axis(1,                         # Define x-axis manually
     at = 1:3,
     labels = c("   Audio-only",
                "Visual-only",
                "Audio & Visual       "), cex.axis=1.5)
axis(2, cex.axis=1.5
     )


