library(readxl)
setwd("C:/Users/carme/OneDrive/Desktop/materiale tesina")
myxz_nrunoff <- read_excel("myxz_nrunoff.xls")
#Questo è il nostro dataset di partenza. 
#Visualizziamo le variabili del dataset
str(myxz_nrunoff)
ncol(myxz_nrunoff)
nrow(myxz_nrunoff)
myxz_nrunoff[myxz_nrunoff$PopTot>=15000 & myxz_nrunoff$anno==2003, ]   #214
myxz_nrunoff[myxz_nrunoff$PopTot>=15000 & myxz_nrunoff$anno==1994, ]   #205


#Sono presenti 22 variabili e 17328 unità. 



#######INIZIO DELLA PULIZIA DELLE OSSERVAZIONI CON IL METODO DI TUKEY FENCES



#######QUINTALI DEI RIFIUTI RACCOLTI###########
summary(myxz_nrunoff$QliRifiuti)
#dalle misure riassuntive visualizziamo che media e mediana sono molto diverse
#tra di loro, e che, cosa più importante, il 3 quartile che raccoglie
#il 75 per cento delle osservazioni ha una differenza abbissale con il restante 25 
#per cento dei dati il cui massimo è 1 milione e mezzo 
hist(myxz_nrunoff$QliRifiuti,col="grey",lwd=3,main="Istogramma dei Rifiuti non pulito ",xlab = "RIFIUTI")
boxplot(myxz_nrunoff$QliRifiuti)
#la rappresentazione grafica conferma i nostri sospetti potrebbero esserci outliers che
#influenzano notevolmente la nostra analisi. 
#Applichiamo il metodo di Tukey Fences per individuare gli outliers
k<-1.5
q<-quantile(myxz_nrunoff$QliRifiuti, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(myxz_nrunoff$QliRifiuti)
hsup<-q[3]+k*IQR(myxz_nrunoff$QliRifiuti)
indici_out<-myxz_nrunoff$QliRifiuti<=hinf | myxz_nrunoff$QliRifiuti>=hsup
dat_nuovo<-myxz_nrunoff[-which(indici_out),]
summary(dat_nuovo$QliRifiuti)
hist(log(dat_nuovo$QliRifiuti),col="grey",lwd=3,main="Istogramma dei Rifiuti pulito",xlab = "RIFIUTI")
summary(log(dat_nuovo$QliRifiuti))


#Possiamo considerare di aumentare il k a 3 per poter individuare i gross-outliers. 
k<-3
q<-quantile(myxz_nrunoff$QliRifiuti, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(myxz_nrunoff$QliRifiuti)
hsup<-q[3]+k*IQR(myxz_nrunoff$QliRifiuti)
indici_out<-myxz_nrunoff$QliRifiuti<=hinf | myxz_nrunoff$QliRifiuti>=hsup
dat_nuovo_2<-myxz_nrunoff[-which(indici_out),]
summary(dat_nuovo_2$QliRifiuti)
hist(dat_nuovo_2$QliRifiuti,col="grey",lwd=3,main="Istogramma dei Rifiuti pulito GO",xlab = "RIFIUTI")
nrow(dat_nuovo_2)
summary(log(dat_nuovo_2$QliRifiuti))
hist(dat_nuovo_2$QliRifiuti, main="Quintali di rifiuti pulito G0", xlab="Quintali di Rifiuti")
#considerando K=3 e rimpicciolendo la nostra ricerca nell'individuazione dei 
#gross outliers otteniamo che la differenza tra il primo e il secondo dataset è di 
#circa 1000 osservazioni. 

dat_nuovo_2[dat_nuovo_2$PopTot>=15000, ]   #409
dat_nuovo_2[dat_nuovo_2$PopTot>=15000 & dat_nuovo_2$anno==2003, ]   #49
dat_nuovo_2[dat_nuovo_2$PopTot>=15000 & dat_nuovo_2$anno==1994, ]   #58


#######KM STRADE##########
summary(dat_nuovo$KmStrade)
hist(dat_nuovo$KmStrade, main="Istogramma Km Strade non pulito", xlab="Km di strada")
boxplot(dat_nuovo$KmStrade)  #è fortemente caratterizzata da valori anomali
boxplot(log(dat_nuovo$KmStrade) ) #boxplot dei logaritmi della variabile

#k=1.5
k<-1.5
q<-quantile(dat_nuovo$KmStrade, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo$KmStrade)
hsup<-q[3]+k*IQR(dat_nuovo$KmStrade)
indici_out<-dat_nuovo$KmStrade<=hinf | dat_nuovo$KmStrade>=hsup
dat_nuovo<-dat_nuovo[-which(indici_out),]
summary(dat_nuovo$KmStrade)
summary(log(dat_nuovo$KmStrade))
hist(dat_nuovo$KmStrade, main="Istogramma Km Strade pulito", xlab="Km di strada")
hist(log(dat_nuovo$KmStrade), main="Istogramma log Km Strade pulito", xlab="Km di strada")


#GROSS OUTLIERS
k<-3
q<-quantile(dat_nuovo_2$KmStrade, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo_2$KmStrade)
hsup<-q[3]+k*IQR(dat_nuovo_2$KmStrade)
indici_out<-dat_nuovo_2$KmStrade<=hinf | dat_nuovo_2$KmStrade>=hsup
dat_nuovo_2<-dat_nuovo_2[-which(indici_out),]
summary(dat_nuovo_2$KmStrade)
summary(log(dat_nuovo_2$KmStrade))
hist(dat_nuovo_2$KmStrade, main="Istogramma Pulizia KmStrade G0", xlab="Km di strade")
hist(log(dat_nuovo_2$KmStrade), main="Istogramma Pulizia LOG KmStrade G0", xlab="Km di strade")


dat_nuovo_2[dat_nuovo_2$PopTot>=15000, ]   #363
dat_nuovo_2[dat_nuovo_2$PopTot>=15000 & dat_nuovo_2$anno==2003, ]   #44
dat_nuovo_2[dat_nuovo_2$PopTot>=15000 & dat_nuovo_2$anno==1994, ]   #49


########STUDENTI########
#numero di studenti
summary(dat_nuovo$Studenti)
hist(dat_nuovo$Studenti, main="Istogramma Studenti non pulito", xlab="Numero di studenti fino alle medie")
boxplot(dat_nuovo$Studenti)   #coda di destra molto molto pesante, tentiamo di ripurirla.
hist(log(dat_nuovo$Studenti), main="Istogramma LOG Studenti non pulito", xlab="Numero di studenti fino alle medie") #istogramma 

#k=1.5
k<-1.5
q<-quantile(dat_nuovo$Studenti, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo$Studenti)
hsup<-q[3]+k*IQR(dat_nuovo$Studenti)
indici_out<-dat_nuovo$Studenti<=hinf | dat_nuovo$Studenti>=hsup
dat_nuovo<-dat_nuovo[-which(indici_out),]
summary(dat_nuovo$Studenti)
summary(log(dat_nuovo$Studenti))
hist(dat_nuovo$Studenti, main="Istogramma Studenti pulito", xlab="Numero di studenti fino alle medie")

###GROSS-OUTLIERS
k<-3
q<-quantile(dat_nuovo_2$Studenti, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo_2$Studenti)
hsup<-q[3]+k*IQR(dat_nuovo_2$Studenti)
indici_out<-dat_nuovo_2$Studenti<=hinf | dat_nuovo_2$Studenti>=hsup
dat_nuovo_2<-dat_nuovo_2[-which(indici_out),]
summary(dat_nuovo_2$Studenti)
summary(log(dat_nuovo_2$Studenti))
hist(dat_nuovo_2$Studenti, main="Istogramma pulizia Studenti G0", xlab="Numero di studenti fino alle medie")
hist(log(dat_nuovo_2$Studenti), main="Istogramma LOG pulizia Studenti G0", xlab="Numero di studenti fino alle medie")

dat_nuovo_2[dat_nuovo_2$PopTot>=15000, ]   #33
dat_nuovo_2[dat_nuovo_2$PopTot>=15000 & dat_nuovo_2$anno==2003, ]   #5
dat_nuovo_2[dat_nuovo_2$PopTot>=15000 & dat_nuovo_2$anno==1994, ]   #3



##########NR PUNTI LUCE#########
summary(dat_nuovo$NrPuntiLuce)
hist(dat_nuovo$NrPuntiLuce, main=" Istogramma Numero punti luce non pulito", xlab="Numero di punti luce")
boxplot(dat_nuovo$NrPuntiLuce) #la coda di destra non sembra essere significativamente pesante, tuttavia
#ripuliamolo lo stesso. La differenza tra il 3 quartile che contiene il 75 per cento
#dei dati e il valore massimo è allarmante. 


k<-1.5
q<-quantile(dat_nuovo$NrPuntiLuce, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo$NrPuntiLuce)
hsup<-q[3]+k*IQR(dat_nuovo$NrPuntiLuce)
indici_out<-dat_nuovo$NrPuntiLuce<=hinf | dat_nuovo$NrPuntiLuce>=hsup
dat_nuovo<-dat_nuovo[-which(indici_out),]
summary(dat_nuovo$NrPuntiLuce)
summary(log(dat_nuovo$NrPuntiLuce))
hist(dat_nuovo$NrPuntiLuce, main=" Istogramma Numero punti luce pulito", xlab="Numero di punti luce")

###GROSS-OUTLIERS
k<-3
q<-quantile(dat_nuovo_2$NrPuntiLuce, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo_2$NrPuntiLuce)
hsup<-q[3]+k*IQR(dat_nuovo_2$NrPuntiLuce)
indici_out<-dat_nuovo_2$NrPuntiLuce<=hinf | dat_nuovo_2$NrPuntiLuce>=hsup
dat_nuovo_2<-dat_nuovo_2[-which(indici_out),]
summary(dat_nuovo_2$NrPuntiLuce)
summary(log(dat_nuovo_2$NrPuntiLuce))
hist(dat_nuovo_2$NrPuntiLuce, main="Istogramma Pulizia NrPuntiLuce GO ", xlab="Numero di punti luce")
hist(log(dat_nuovo_2$NrPuntiLuce), main="Istogramma Pulizia LOG NrPuntiLuce GO ", xlab="Numero di punti luce")

dat_nuovo_2[dat_nuovo_2$PopTot>=15000, ]   #20




##########NR PERMESSI########
#numero di permessi di costruzione rilasciati
summary(dat_nuovo$NrPermessi)
hist(dat_nuovo$NrPermessi, main="Istogramma Numero permessi non pulito", xlab="Numero di permessi rilasciati")
boxplot(dat_nuovo$NrPermessi)
hist(log(dat_nuovo$NrPermessi), main="Istogramma LOG Numero permessi non pulito", xlab="Numero di permessi rilasciati")

#Non sembra avere una coda eccessivamente lunga a destra ma i valori esterni molto 
#elevati potrebbero essere outliers
k<-1.5
q<-quantile(dat_nuovo$NrPermessi, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo$NrPermessi)
hsup<-q[3]+k*IQR(dat_nuovo$NrPermessi)
indici_out<-dat_nuovo$NrPermessi<=hinf | dat_nuovo$NrPermessi>=hsup
dat_nuovo<-dat_nuovo[-which(indici_out),]
summary(dat_nuovo$NrPermessi)
summary(log(dat_nuovo$NrPermessi))
hist(dat_nuovo$NrPermessi, main="Istogramma Numero permessi pulito", xlab="Numero di permessi rilasciati")


###GROSS-OUTLIERS
k<-3
q<-quantile(dat_nuovo_2$NrPermessi, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo_2$NrPermessi)
hsup<-q[3]+k*IQR(dat_nuovo_2$NrPermessi)
indici_out<-dat_nuovo_2$NrPermessi<=hinf | dat_nuovo_2$NrPermessi>=hsup
dat_nuovo_2<-dat_nuovo_2[-which(indici_out),]
summary(dat_nuovo_2$NrPermessi)
summary(log(dat_nuovo_2$NrPermessi))
hist(dat_nuovo_2$NrPermessi, main="Istogramma Pulizia NrPermessi GO", xlab="Numero di permessi")
hist(log(dat_nuovo_2$NrPermessi), main="Istogramma Pulizia LOG NrPermessi GO", xlab="Numero di permessi")




########NR ISCRITTI####### 
# numero di persone registrate sulle liste elettorali
summary(dat_nuovo$NrIscritti)
hist(dat_nuovo$NrIscritti, main=" Istogramma Numero iscritti non pulito", xlab="Numero di iscritti")
boxplot(dat_nuovo$NrIscritti)
#sembra avere una coda eccessivamente lunga a destra, per questo motivo ripeto 
#TF
hist(log(dat_nuovo$NrIscritti), main=" Istogramma LOG Numero iscritti non pulito", xlab="Numero di iscritti")

k<-1.5
q<-quantile(dat_nuovo$NrIscritti, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo$NrIscritti)
hsup<-q[3]+k*IQR(dat_nuovo$NrIscritti)
indici_out<-dat_nuovo$NrIscritti<=hinf | dat_nuovo$NrIscritti>=hsup
dat_nuovo<-dat_nuovo[-which(indici_out),]
summary(dat_nuovo$NrIscritti)
summary(log(dat_nuovo$NrIscritti))
hist(dat_nuovo$NrIscritti, main=" Istogramma Numero iscritti pulito", xlab="Numero di iscritti")

#GROSS OUTLIERS
k<-3
q<-quantile(dat_nuovo_2$NrIscritti, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo_2$NrIscritti)
hsup<-q[3]+k*IQR(dat_nuovo_2$NrIscritti)
indici_out<-dat_nuovo_2$NrIscritti<=hinf | dat_nuovo_2$NrIscritti>=hsup
dat_nuovo_2<-dat_nuovo_2[-which(indici_out),]
summary(dat_nuovo_2$NrIscritti)
summary(log(dat_nuovo_2$NrIscritti))
hist(dat_nuovo_2$NrIscritti, main="Istogramma Pulizia NrIscritti GO", xlab="Numero di iscritti")
hist(log(dat_nuovo_2$NrIscritti), main="Istogramma Pulizia LOG NrIscritti GO", xlab="Numero di iscritti")

dat_nuovo_2[dat_nuovo_2$PopTot>=15000, ]   #0




#######KM POLIZIA########
# km di pattugliamento effettuati dalla polizia municipale
summary(dat_nuovo$KmPolizia)
hist(dat_nuovo$KmPolizia, main="Istogramma Km Polizia non pulito", xlab="Km pattugliati dalla polizia")
boxplot(dat_nuovo$KmPolizia)
hist(log(dat_nuovo$KmPolizia), main="Istogramma LOG Km Polizia non pulito", xlab="Km pattugliati dalla polizia")


k<-1.5
q<-quantile(dat_nuovo$KmPolizia, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo$KmPolizia)
hsup<-q[3]+k*IQR(dat_nuovo$KmPolizia)
indici_out<-dat_nuovo$KmPolizia<=hinf | dat_nuovo$KmPolizia>=hsup
dat_nuovo<-dat_nuovo[-which(indici_out),]
summary(dat_nuovo$KmPolizia)
summary(log(dat_nuovo$KmPolizia))
hist(dat_nuovo$KmPolizia, main="Istogramma Km Polizia pulito", xlab="Km pattugliati dalla polizia")


#GROSS OUTLIERS
k<-3
q<-quantile(dat_nuovo_2$KmPolizia, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo_2$KmPolizia)
hsup<-q[3]+k*IQR(dat_nuovo_2$KmPolizia)
indici_out<-dat_nuovo_2$KmPolizia<=hinf | dat_nuovo_2$KmPolizia>=hsup
dat_nuovo_2<-dat_nuovo_2[-which(indici_out),]
summary(dat_nuovo_2$KmPolizia)
summary(log(dat_nuovo_2$KmPolizia))
hist(dat_nuovo_2$KmPolizia, main=" Istogramma Pulizia KmPolizia GO", xlab="Km pattugliati dalla polizia")
hist(log(dat_nuovo_2$KmPolizia), main=" Istogramma Pulizia LOG KmPolizia GO", xlab="Km pattugliati dalla polizia")




########NR CERTIFICATI#########
#numero di certificati anagrafici rilasciati
summary(dat_nuovo$NrCertificati)
hist(dat_nuovo$NrCertificati, main="Istogramma Numero Certificati non pulito", xlab="Numero di certificati")
boxplot(dat_nuovo$NrCertificati)
hist(log(dat_nuovo$NrCertificati), main="Istogramma LOG Numero Certificati non pulito", xlab="Numero di certificati")


k<-1.5
q<-quantile(dat_nuovo$NrCertificati, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo$NrCertificati)
hsup<-q[3]+k*IQR(dat_nuovo$NrCertificati)
indici_out<-dat_nuovo$NrCertificati<=hinf | dat_nuovo$NrCertificati>=hsup
dat_nuovo<-dat_nuovo[-which(indici_out),]
summary(dat_nuovo$NrCertificati)
summary(log(dat_nuovo$NrCertificati))
hist(dat_nuovo$NrCertificati, main="Istogramma Numero Certificati pulito", xlab="Numero di certificati")


#GROSS OURLIERS
k<-3
q<-quantile(dat_nuovo_2$NrCertificati, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo_2$NrCertificati)
hsup<-q[3]+k*IQR(dat_nuovo_2$NrCertificati)
indici_out<-dat_nuovo_2$NrCertificati<=hinf | dat_nuovo_2$NrCertificati>=hsup
dat_nuovo_2<-dat_nuovo_2[-which(indici_out),]
summary(dat_nuovo_2$NrCertificati)
summary(log(dat_nuovo_2$NrCertificati))
hist(dat_nuovo_2$NrCertificati, main="Istogramma Pulizia NrCertificati GO", xlab="Numero di certificati")
hist(log(dat_nuovo_2$NrCertificati), main="Istogramma Pulizia LOG NrCertificati GO", xlab="Numero di certificati")






#######IMPTOT########
# impegni di spesa corrente, totale
summary(dat_nuovo$ImpTot)
hist(dat_nuovo$ImpTot, main="Istogramma Impegni di spesa correnti non pulito", xlab="Impegni di spesa corrente")
boxplot(dat_nuovo$ImpTot)
k<-1.5
q<-quantile(dat_nuovo$ImpTot, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo$ImpTot)
hsup<-q[3]+k*IQR(dat_nuovo$ImpTot)
indici_out<-dat_nuovo$ImpTot<=hinf | dat_nuovo$ImpTot>=hsup
dat_nuovo<-dat_nuovo[-which(indici_out),]
summary(dat_nuovo$ImpTot)
summary(log(dat_nuovo$ImpTot))
hist(dat_nuovo$ImpTot, main="Istogramma Impegni di spesa correnti pulito", xlab="Impegni di spesa corrente")



#GROSS OUTLIERS
k<-3
q<-quantile(dat_nuovo_2$ImpTot, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo_2$ImpTot)
hsup<-q[3]+k*IQR(dat_nuovo_2$ImpTot)
indici_out<-dat_nuovo_2$ImpTot<=hinf | dat_nuovo_2$ImpTot>=hsup
dat_nuovo_2<-dat_nuovo_2[-which(indici_out),]
summary(dat_nuovo_2$ImpTot)
summary(log(dat_nuovo_2$ImpTot))
hist(dat_nuovo_2$ImpTot, main="Istogramma Pulizia ImpTot GO", xlab="Impegni di spesa corrente")





#########IMP PERS########
# impegni di spesa corrente, spese per il personale
summary(dat_nuovo$ImpPers)
hist(dat_nuovo$ImpPers, main="Istogramma Impegni relativi al personale non pulito", xlab="Impegni relativi al personale")

k<-1.5
q<-quantile(dat_nuovo$ImpPers, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo$ImpPers)
hsup<-q[3]+k*IQR(dat_nuovo$ImpPers)
indici_out<-dat_nuovo$ImpPers<=hinf | dat_nuovo$ImpPers>=hsup
dat_nuovo<-dat_nuovo[-which(indici_out),]
summary(dat_nuovo$ImpPers)
summary(log(dat_nuovo$ImpPers))
hist(dat_nuovo$ImpPers, main="Istogramma Impegni relativi al personale pulito", xlab="Impegni relativi al personale")


###GROSS OUTLIERS
k<-3
q<-quantile(dat_nuovo_2$ImpPers, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo_2$ImpPers)
hsup<-q[3]+k*IQR(dat_nuovo_2$ImpPers)
indici_out<-dat_nuovo_2$ImpPers<=hinf | dat_nuovo_2$ImpPers>=hsup
dat_nuovo_2<-dat_nuovo_2[-which(indici_out),]
summary(dat_nuovo_2$ImpPers)
summary(log(dat_nuovo_2$ImpPers))
hist(dat_nuovo_2$ImpPers, main="Istogramma Pulizia ImpPers GO", xlab="Impegni di spesa per il personale")






##########IMPNOPERS#######
#impegni di spesa corrente, spese non per il personale
summary(dat_nuovo$ImpNoPers)
hist(dat_nuovo$ImpNoPers, main="Impegni non per il personale non pulito", xlab="Impegni non per il personale")

k<-1.5
q<-quantile(dat_nuovo$ImpNoPers, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo$ImpNoPers)
hsup<-q[3]+k*IQR(dat_nuovo$ImpNoPers)
indici_out<-dat_nuovo$ImpNoPers<=hinf | dat_nuovo$ImpNoPers>=hsup
dat_nuovo<-dat_nuovo[-which(indici_out),]
summary(dat_nuovo$ImpNoPers)
summary(log(dat_nuovo$ImpNoPers))
hist(dat_nuovo$ImpNoPers, main="Impegni non per il personale pulito", xlab="Impegni non per il personale")


#GROSS OUTLIERS
k<-3
q<-quantile(dat_nuovo_2$ImpNoPers, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo_2$ImpNoPers)
hsup<-q[3]+k*IQR(dat_nuovo_2$ImpNoPers)
indici_out<-dat_nuovo_2$ImpNoPers<=hinf | dat_nuovo_2$ImpNoPers>=hsup
dat_nuovo_2<-dat_nuovo_2[-which(indici_out),]
summary(dat_nuovo_2$ImpNoPers)
summary(log(dat_nuovo_2$ImpNoPers))
hist(dat_nuovo_2$ImpNoPers, main="Pulizia ImpNoPers GO", xlab="Impegni di spesa corrente non per il personale")




########VECCHIAIA##########
#tasso di vecchiaia
summary(dat_nuovo$vecchiaia)
hist(dat_nuovo$vecchiaia, main="Tasso di vecchiaia non pulito", xlab="Tasso di vecchiaia")


k<-1.5
q<-quantile(dat_nuovo$vecchiaia, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo$vecchiaia)
hsup<-q[3]+k*IQR(dat_nuovo$vecchiaia)
indici_out<-dat_nuovo$vecchiaia<=hinf | dat_nuovo$vecchiaia>=hsup
dat_nuovo<-dat_nuovo[-which(indici_out),]
summary(dat_nuovo$vecchiaia)
summary(log(dat_nuovo$vecchiaia))
hist(dat_nuovo$vecchiaia, main="Tasso di vecchiaia pulito", xlab="Tasso di vecchiaia")
#GROSS OUTLIERS
k<-3
q<-quantile(dat_nuovo_2$vecchiaia, c(0.25, 0.5, 0.75))
hinf<-q[1]-k*IQR(dat_nuovo_2$vecchiaia)
hsup<-q[3]+k*IQR(dat_nuovo_2$vecchiaia)
indici_out<-dat_nuovo_2$vecchiaia<=hinf | dat_nuovo_2$vecchiaia>=hsup
dat_nuovo_2<-dat_nuovo_2[-which(indici_out),]
summary(dat_nuovo_2$vecchiaia)
summary(log(dat_nuovo_2$vecchiaia))
hist(dat_nuovo_2$vecchiaia, main="Pulizia vecchiaia G0", xlab="Tasso di vecchiaia")



#Le dummy non le ripulisco. 

nrow(dat_nuovo)
nrow(dat_nuovo_2)
#write.csv(dat_nuovo, "myxz_runoff_ripulito.csv", row.names = F )
#write.csv(dat_nuovo_2, "myxz_runoff_ripulito_go.csv", row.names = F )
library(tidyverse)

###breve analisi dei dati multivariata
###########ANALISI MULIVARIATA DELLE FEATURES #########
###1) Relazione tra IMPTOT E POPOLAZIONE TOTALE CONDIZIONATA SU RURALITA'
h<-ggplot(dat_nuovo, aes(PopTot,ImpTot, colour=ruralità ))+geom_point()
h
h+geom_smooth(method='lm')
#dal grafico notiamo che in entrambi i gruppi è presente una relazione positiva
#tra i due caratteri, tuttavia, il gruppo 1 sembra avere una retta di regressione 
#traslata verso l'alto rispetto al gruppo 0. 
cov<-dat_nuovo%>%
  group_by(ruralità)%>%
  summarise(Cov=cov(PopTot, ImpTot))
cov
#covarianza elevata, ma maggiore nel gruppo 1 

cor<-dat_nuovo%>%
  group_by(ruralità)%>%
  summarise(Cov=cor(PopTot, ImpTot))
cor
#correlazione pressocchè la stessa. 
#Verifichiamo di quanto , e se per davvero, il gruppo 1 ha una retta traslata verso l'alto. 
rgr_1=lm(ImpTot~PopTot+ruralità, data=dat_nuovo)
summary(rgr_1)
#i coefficienti risultano essere tutti significativi con un p-value prossimo allo 0. 
#Ciò che notiamo è che a parità di condizioni avere ruralità=1 implica un'impegno di spesa
#totale incrementata di 7.69*10^4, per cui conferma che la sua retta di regressione è traslata
#verso l alta rispetto al gruppo 0. 


