############################################################################################
library(openxlsx)
library(drc)
library(lmtest)
library(sandwich)
library(drcSeedGerm)
library(tidyr)
library(maditr)
library(multcomp)
library(xtable)
library(texreg)
############################################################################################

setwd("")

####################################################################################################################################################
# Chenopodium read data

data_chenopodium.petri=read.xlsx("germinazioni.analisi.purged.xlsx",1)[,1:7]
data_chenopodium.petri$Thesis=gsub("%OE","%",data_chenopodium.petri$Thesis)
data_chenopodium.substrato=read.xlsx("germinazioni.analisi.purged.xlsx",2)[,1:7]
data_chenopodium.substrato$Thesis=gsub("%OE","%",data_chenopodium.substrato$Thesis)


data_chenopodium.petri
data_chenopodium.substrato

####################################################################################################################################################

model.chenopodium.petri<- try(drm(propCum~timeAf , 
                                  data=data_chenopodium.petri, 
                                  curveid = Thesis,
                                  fct=L.4(names = c("Slope","Lower Limit", "Upper Limit","T50"))))




model.chenopodium.substrato<- try(drm(propCum~timeAf , 
                                      data=data_chenopodium.substrato, 
                                      curveid = Thesis,
                                      fct=L.4(names = c("Slope","Lower Limit", "Upper Limit","T50"))))




pdf(file = paste0("model.chenopodium.substrato.pastazzo.pdf"))
plot(model.chenopodium.substrato,log="",col = TRUE,legendPos=c(6,0.8),cex.legend = 0.9,ylim=c(0,1),xlab="Days",
     main="Chenopodium - Substrato's germination curves")
dev.off()

pdf(file = paste0("model.chenopodium.petri.pastazzo.pdf"))
plot(model.chenopodium.petri,log="",col = TRUE,legendPos=c(6,0.8),cex.legend = 0.9,ylim=c(0,1),xlab="Days",
     main="Chenopodium -  Petri's germination curves")
dev.off()

####################################################################################################################################################
# Lactuca


data_lactuca.petri=read.xlsx("germinazione_lactuca_petri_purged.xlsx",1)[,1:7]
data_lactuca.petri$Thesis=gsub("%O","%",data_lactuca.petri$Thesis)
data_lactuca.substrato=read.xlsx("germinazioni.analisi.purged.xlsx",3)[,1:7]
data_lactuca.substrato$Thesis=gsub("%OE","%",data_lactuca.substrato$Thesis)

data_lactuca.petri
data_lactuca.substrato

model.lactuca.substrato<- try(drm(propCum~timeAf , data=data_lactuca.substrato, 
                                  curveid = Thesis,
                                  fct=L.4(names = c("Slope","Lower Limit", "Upper Limit","T50"))))
model.lactuca.petri<- try(drm(propCum~timeAf , 
                              data=data_lactuca.petri, 
                              curveid = Thesis,
                              fct=L.4(names = c("Slope","Lower Limit", "Upper Limit","T50"))))


pdf(file = paste0("model.lactuca.substrato.pastazzo.pdf"))
plot(model.lactuca.substrato,log="",col = TRUE,legendPos=c(4,0.8),cex.legend = 0.9,
     ylim=c(0,1),xlab="Days",main="Lactuca - Substrato's germination curves")
dev.off()



pdf(file = paste0("model.lactuca.petri.pastazzo.pdf"))
plot(model.lactuca.petri,log="",col = TRUE,legendPos=c(4,0.8),cex.legend = 0.9,ylim=c(0,1),xlab="Days",
     main="Lactuca - Petri's germination curves")
dev.off()

########################################################################
sink("ED_chenopodium.txt")
# xtable(ED(model.chenopodium.petri, c(10,50,90), interval = "delta"))
# print("\n")
# xtable(ED(model.chenopodium.substrato, c(10,50,90), interval = "delta"))
print("\n")
summary(model.chenopodium.petri)
print("\n")
summary(model.chenopodium.substrato)
sink()




########################################################################
sink("ED_lactuca.txt")
# xtable(ED(model.lactuca.petri, c(10,50,90), interval = "delta"))
# print("\n")
# xtable(ED(model.lactuca.substrato, c(10,50,90), interval = "delta"))
print("\n")
summary(model.lactuca.petri)
print("\n")
summary(model.lactuca.substrato)
sink()
########################################################################

# texreg(model.lactuca.petri,file="model.lactuca.petri.tex")
# texreg(model.lactuca.substrato,file="model.lactuca.substrato.tex")
# htmlreg(model.lactuca.petri,file="model.lactuca.petri.html")
# htmlreg(model.lactuca.substrato,file="model.lactuca.substrato.html")

##################################################################################
# tests
sink("comparison_lactuca_petri.txt")

compParm(model.lactuca.petri, "Slope", "-")
print("\n")
compParm(model.lactuca.petri, "Upper Limit", "-")
print("\n")
compParm(model.lactuca.petri, "Lower Limit", "-")
print("\n")
compParm(model.lactuca.petri, "T50", "-")

EDmodel.lactuca.petri<- ED(model.lactuca.petri, c(50), interval = "delta",multcomp = TRUE, display = FALSE)

confint(glht(EDmodel.lactuca.petri[["EDmultcomp"]]))

sink()

sink("comparison_lactuca_substrato.txt")
compParm(model.lactuca.substrato, "Slope", "-")
print("\n")
compParm(model.lactuca.substrato, "Upper Limit", "-")
print("\n")
compParm(model.lactuca.substrato, "Lower Limit", "-")
print("\n")
compParm(model.lactuca.substrato, "T50", "-")

EDmodel.lactuca.substrato<- ED(model.lactuca.substrato, c(50), interval = "delta",multcomp = TRUE, display = FALSE)
confint(glht(EDmodel.lactuca.substrato[["EDmultcomp"]]))
print("\n")
sink()

sink("comparison_chenopodium_substrato.txt")

compParm(model.chenopodium.substrato, "Slope", "-")
print("\n")
compParm(model.chenopodium.substrato, "Upper Limit", "-")
print("\n")
compParm(model.chenopodium.substrato, "Lower Limit", "-")
print("\n")
compParm(model.chenopodium.substrato, "T50", "-")
print("\n")
EDmodel.chenopodium.substrato<- ED(model.chenopodium.substrato, c(50), interval = "delta",multcomp = TRUE, display = FALSE)
confint(glht(EDmodel.chenopodium.substrato[["EDmultcomp"]]))
sink()

sink("comparison_chenopodium_petri.txt")


compParm(model.chenopodium.petri, "Slope", "-")
print("\n")
compParm(model.chenopodium.petri, "Upper Limit", "-")
print("\n")
compParm(model.chenopodium.petri, "Lower Limit", "-")
print("\n")
compParm(model.chenopodium.petri, "T50", "-")
print("\n")
EDmodel.chenopodium.petri<- ED(model.chenopodium.petri, c(50), interval = "delta",multcomp = TRUE, display = FALSE)
confint(glht(EDmodel.chenopodium.petri[["EDmultcomp"]]))

sink()


######################################################################################################################################
# References

