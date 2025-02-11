library(readxl)
library(Benchmarking)
library(plm)
setwd("C:/Users/carme/OneDrive/Desktop/UNIVERSITA'/TERZO ANNO/ESAME ECONOMIA DELLA REGOLAMENTAZIONE/Senatore_Carmela_Pia")
dat<-read.csv("myxz_runoff_ripulito_go.csv")
dat$litoraneità<-as.factor(dat$litoraneità)
dat$ruralità<-as.factor(dat$ruralità)
dat$gradourb<-ordered(dat$gradourb)



#PUNTO UNO: Analisi dell'efficienza tecnica e delle sue variazioni, mediante 
# l'analisi non parametrica dell'efficienza.
#Bisogna misurare l'efficienza 
#prendendo le osservazioni anno per anno (separatamente).

#Ho deciso di implementare l'analisi non parametrica, per cui i metodi oggetto di studio 
#sono: FDH, DEA-C E DEA-V. 
#Siccome l'analisi deve essere svolta anno per anno separatamente, 
#inizio con analizzare l'anno 1994 inteso come anno pre-policy. 


#La problematica riscontrata è che ripulendo l'intero dataset dai gross outliers
#sulle variabili stabilite nel file di pulizia(quintali di rifiuti, km strade, studenti, 
#nr punti luce , nr permessi, nr iscritti, km polizia, nr certificati, imppers, impnopers, vecchiaia)
# non riscontro nessun comune dei trattati con popolazione>15000 abitanti, per questo motivo
#possiamo considerare di effettuare un confronto tra il dataset ripulito interamente come appena descritto
#e il dataset ripulito dalla sola variabile di quintali di rifiuti raccolti. 



#Carico il file di riferimento per il problema di runoff creando un dataset in cui viene
#ripulita la sola variabile quintali di rifiuti raccolti che permette di 
#conservare i comuni con popolazione totale >15000 interessati dall'introduzione del doppio turno. 
setwd("C:/Users/carme/OneDrive/Desktop/materiale tesina")
dat_una<-read.csv("dat_una.csv")
dat_una$litoraneità<-as.factor(dat_una$litoraneità)
dat_una$ruralità<-as.factor(dat_una$ruralità)
dat_una$gradourb<-as.factor(dat_una$gradourb)


dat_una[dat_una$PopTot>=15000 & dat_una$anno==2003, ]   #49
dat_una[dat_una$PopTot>=15000 & dat_una$anno==1994, ]   #58

#Il dataset DAT_UNA contiene i dati ripuliti della sola variabile quintali di rifiuti 
#raccolti, mentre il dataset DAT contiene il dataset ripulito dalle variabili elencate 
#precedentemente. 

# ############################SLACKS #######
# div1=subset(dat_una, anno==1194 & anno==2004)
# div1=subset(dat_una, Studenti!= 15 & Studenti!= 22)
# x<-matrix(c(div1$ImpPers, div1$ImpNoPers), ncol=2, nrow=5107)
# y<-matrix(c(div1$QliRifiuti, div1$KmStrade,div1$Studenti, div1$NrPuntiLuce, div1$NrPermessi, div1$NrIscritti, div1$KmPolizia, div1$NrCertificati), nrow=5107, ncol=8)
# ei = dea(x,y,RTS="vrs",ORIENTATION="out",SLACK=T)
# summary(ei)
# # stima duale(con DEA)
# eid = dea.dual(x,y,RTS="vrs",ORIENTATION="out")
# summary(eid)
# 
# # vedo i risultati,  i peers sono le osservazioni la cui combinazione lineare
# # domina una data osservazione
# print(cbind("e"=ei$eff, "ed"=eid$eff, peers(ei), ei$sx, eid$u))
# ############################SLACKS#########






nrow(dat)
ncol(dat)
#Sono presenti 22 variabili e 14052 unità. 
#CONFRONTO FRA DAT_UNA E DAT

#Inizio dall'analisi del dataset ripulito dai gross outliers per tutte le variabili
#ANNO PRE-POLICY
div<-subset(dat, anno==1994)
ncol(div)
nrow(div)
x<-matrix(c(div$ImpPers, div$ImpNoPers), ncol=2, nrow=1782)
y<-matrix(c(div$QliRifiuti, div$KmStrade,div$Studenti, div$NrPuntiLuce, div$NrPermessi, div$NrIscritti, div$KmPolizia, div$NrCertificati), nrow=1782, ncol=8)
#FDH
fdh_1994<- dea(x,y, RTS=0, ORIENTATION=2)
summary(fdh_1994)
nrow(div)-1255  #numero di comuni inefficienti 
#Implementando la fdh visualizziamo a schermo che il 70.43% 
#delle osservazioni(ovvero 1255 comuni) sono efficienti. 
#Il restante 30% circa sono inefficienti, ovvero 527. Il punteggio piu elevato è piu inefficiente
#ha un minimo in 1 e un massimo in 2.059, ovvero l'espansione degli output per arrivare alla frontiera. 

#Passiamo ora a svolgere la DEA-V per l anno pre-policy: 
dea_v_1994<- dea(x,y, RTS=1, ORIENTATION=2)
summary(dea_v_1994)
nrow(div)-185
#Come ci aspettavamo , considerata la DEA-V come un FDH con aggiunta di convessità, il numero di 
#unità efficienti è calata in maniera drastica arrivando a ricoprire solo
#185 comuni, ovvero il 10.4 per cento del totale. il restante 90 % di osservazioni(1597 comuni), 
#da questo punto di vista,è inefficiente. 

#Ultimo passaggio è quello di considerare la DEA-C per l anno pre-policy: 
dea_c_1994<-dea(x,y,RTS=3, ORIENTATION=2)
summary(dea_c_1994)
nrow(div)-92
#Ancora, ci rendiamo conto che con la DEA-C il numero di comuni effettivamente efficienti 
#sono solo 92, ovvero il 5.2% del totale. I restanti 1690 comuni sono inefficienti sotto questo fronte. 


#Ho creato una matrice che contiene il codice istat del comune, i risultati dei punteggi
#per la FDH, DEA-V E DEA-C per l'anno pre-policy ovvero 1994, per confrontare e visualizzare a 
#schermo i diversi punteggi.
matrice_comuni_1994<-matrix(c(div$codIstat, fdh_1994$eff, dea_v_1994$eff, dea_c_1994$eff), nrow=1782, ncol=4)
colnames(matrice_comuni_1994)<- c('codice comune', 'FDH', 'DEA-V', 'DEA-C')
matrice_comuni_1994[1:10,]
#Ho messo in esecuzione a vista i primi dieci comuni, come possiamo notare 
#la maggior parte dei comuni considerati efficienti con l'FDH non lo 
#sono nè per la dea-v nè per la dea-c. 
which(matrice_comuni_1994[,3]==1.0)
matrice_comuni_1994[which(matrice_comuni_1994[,3]==1.0),]
#ad esempio, estraendo i comuni che sono efficienti con la dea-v, 185 come stabilito in precedenza, 
#questi sono chiaramente efficienti per l'FDH ma molti non lo sono per la dea-c. 
#I primi due comuni sono efficienti per l'FDH ma non lo sono per la dea-c,
#mentre il terzo comune con codice 1206 è efficiente anche per la dea-c. 





#Passiamo ora invece all'analisi dell'anno 2003, inteso come anno post-policy. 
#analizziamolo nel particolare utilizzando le stesse tecniche di analisi non parametrica. 

div<-subset(dat, anno==2003)
ncol(div)
nrow(div)
x<-matrix(c(div$ImpPers, div$ImpNoPers), ncol=2, nrow=1752)
y<-matrix(c(div$QliRifiuti, div$KmStrade,div$Studenti, div$NrPuntiLuce, div$NrPermessi, div$NrIscritti, div$KmPolizia, div$NrCertificati), nrow=1752, ncol=8)
fdh_2003<- dea(x,y, RTS=0, ORIENTATION=2)
summary(fdh_2003)
#Nel 2003, anno inteso come post-policy, il numero di comuni efficienti diminuisce rispetto
#al 1994. Infatti in questo anno i comuni efficienti sono 1031, ovvero il 58% del totale
#rispetto a 1255 comuni, ovvero il 70% del totale dell anno 1994. 

#DEA-V
dea_v_2003<- dea(x,y, RTS=1, ORIENTATION=2)
summary(dea_v_2003)
#come prima i comuni efficienti scendono rispetto al fdh a 148, ovvero l'8.4 per cento del totale. 
#i punteggi hanno un minimo in 1 e un massimo in 3.99. 

#DEA-C
dea_c_2003<- dea(x,y, RTS=3, ORIENTATION=2)
summary(dea_c_2003)
nrow(div)-73
#Anche nella dea-c i paesi efficienti calano in maniera drastica, ora i paesi considerati
#efficienti sono 73, ovvero il 4.2% complessivo. I restanti 1679 comuni sono inefficienti sotto questo fronte.

#Ho creato una matrice che contiene il codice istat del comune, i risultati dei punteggi
#per la FDH, DEA-V E DEA-C per l'anno post-policy ovvero 1994, per confrontare e visualizzare a 
#schermo i diversi punteggi.
matrice_comuni_2003<-matrix(c(div$codIstat, fdh_2003$eff, dea_v_2003$eff, dea_c_2003$eff), nrow=1752, ncol=4)
matrice_comuni_2003[1:10,]
#Ho messo in esecuzione a vista i primi dieci comuni, così come per prima anche adesso
#la maggior parte dei comuni considerati efficienti con l'FDH non lo 
#sono nè per la dea-v nè per la dea-c. 
which(matrice_comuni_2003[,4]==1.0)
matrice_comuni_2003[which(matrice_comuni_2003[,3]==1.0),]
#ad esempio, estraendo i comuni che sono efficienti con la dea-c, 73 come stabilito in precedenza, 
#questi sono chiaramente efficienti per l'FDH ma molti non lo sono per la dea-c. 
#Il primo comune è efficiente per l'FDH ma non lo è per la dea-c,
#mentre il secondo e terzo comune con codice 1300 e 1310 sono efficienti anche per la dea-c. 



#DATASET DAT_UNA RIPULITO DELLA SOLA VARIABILE QUINTALI DI RIFIUTI RACCOLTI(ANNO PRE-POLICY)
div1<-subset(dat_una, anno==1994)
ncol(div1)
nrow(div1)
x<-matrix(c(div1$ImpPers, div1$ImpNoPers), ncol=2, nrow=1963)
y<-matrix(c(div1$QliRifiuti, div1$KmStrade,div1$Studenti, div1$NrPuntiLuce, div1$NrPermessi, div1$NrIscritti, div1$KmPolizia, div1$NrCertificati), nrow=1963, ncol=8)
#FDH
fdh_1994_una<- dea(x,y, RTS=0, ORIENTATION=2)
summary(fdh_1994_una)
nrow(div1)-1394  #numero di comuni inefficienti 
#Implementando la fdh per il dataset ripulito 
#della sola variabile quintali di rifiuti raccolti, visualizziamo a schermo che il 71% 
#delle osservazioni(ovvero 1394 comuni) sono efficienti rispetto al 70% del dataset completamente ripulito. 
#Il restante 29% circa sono inefficienti, ovvero 569. 


#Passiamo ora a svolgere la DEA-V per l anno pre-policy : 
dea_v_1994_una<- dea(x,y, RTS=1, ORIENTATION=2)
summary(dea_v_1994_una)
nrow(div1)-185
#Come ci aspettavamo, il numero di 
#unità efficienti è calata in maniera decisiva arrivando a ricoprire solo
#185 comuni, ovvero il 9.4 per cento del totale. Tuttavia passando alla dea-v il numero di comuni
#efficienti rispetto al dataset gross outliers è identica, anche nel caso precedente il numero di comuni efficienti è di 185. 

#Ultimo passaggio è quello di considerare la DEA-C per l anno pre-policy: 
dea_c_1994_una<-dea(x,y,RTS=3, ORIENTATION=2)
summary(dea_c_1994_una)
nrow(div)-92
#Ancora, ci rendiamo conto che con la DEA-C il numero di comuni effettivamente efficienti 
#sono solo 88, ovvero il 4.5% del totale. In questo caso il numero è inferiore di 4 unità
#rispetto al dataset con cui stiamo facendo il confronto. 


#Ho creato una matrice che contiene il codice istat del comune, i risultati dei punteggi
#per la FDH, DEA-V E DEA-C per l'anno pre-policy ovvero 1994, per confrontare e visualizzare a 
#schermo i diversi punteggi.
matrice_comuni_1994_una<-matrix(c(div1$codIstat, fdh_1994_una$eff, dea_v_1994_una$eff, dea_c_1994_una$eff), nrow=1963, ncol=4)
colnames(matrice_comuni_1994_una)<- c('codice comune', 'FDH', 'DEA-V', 'DEA-C')
matrice_comuni_1994[1:10,]

####SUMMARY: 
#Quindi, in maniera sintetica, per l'anno pre-policy: per la fdh il numero di comuni efficienti è maggiore 
#nel caso del dataset ripulito da una sola variabile rispetto al dataset ripulito dai gross-outliers. 
#Per la dea-v il numero di comuni efficienti è identico, mentre per la dea-c
#il numero di comuni efficienti è maggiore per il dataset ripulito dai gross outliers 
#che per il dataset ripulito da una sola variabile. 



###DATASET DAT_UNA RIPULITO DELLA SOLA VARIABILE QUINTALI DI RIFIUTI RACCOLTI(ANNO POST-POLICY)

div1<-subset(dat_una, anno==2003)
ncol(div1)
nrow(div1)
x<-matrix(c(div1$ImpPers, div1$ImpNoPers), ncol=2, nrow=1907)
y<-matrix(c(div1$QliRifiuti, div1$KmStrade,div1$Studenti, div1$NrPuntiLuce, div1$NrPermessi, div1$NrIscritti, div1$KmPolizia, div1$NrCertificati), nrow=1907, ncol=8)
fdh_2003_una<- dea(x,y, RTS=0, ORIENTATION=2)
summary(fdh_2003_una)
#Nel 2003, anno inteso come post-policy, il numero di comuni efficienti diminuisce rispetto
#al 1994. Infatti in questo anno i comuni efficienti sono 1151, ovvero il 60% del totale
#rispetto a 1394 comuni, ovvero il 71% del totale dell anno 1994. 

#DEA-V
dea_v_2003_una<- dea(x,y, RTS=1, ORIENTATION=2)
summary(dea_v_2003_una)
#come prima i comuni efficienti scendono a 151, rispetto ai 185 del 1994, ovvero l'7.9 per cento del totale. 
#i punteggi hanno un minimo in 1 e un massimo in 3.99. 

#DEA-C
dea_c_2003_una<- dea(x,y, RTS=3, ORIENTATION=2)
summary(dea_c_2003_una)
#Anche nella dea-c i paesi efficienti calano in maniera drastica, ora i paesi considerati
#efficienti sono 74, ovvero il 4.2% complessivo. I restanti  comuni sono inefficienti sotto questo fronte.
#Il numero di comuni efficienti nel 1994 rispetto alla dea-c sono 92. 

#Ho creato una matrice che contiene il codice istat del comune, i risultati dei punteggi
#per la FDH, DEA-V E DEA-C per l'anno post-policy ovvero 1994, per confrontare e visualizzare a 
#schermo i diversi punteggi.
matrice_comuni_2003_una<-matrix(c(div1$codIstat, fdh_2003_una$eff, dea_v_2003_una$eff, dea_c_2003_una$eff), nrow=1907, ncol=4)
matrice_comuni_2003_una[1:10,]
#Ho messo in esecuzione a vista i primi dieci comuni, così come per prima anche adesso
#la maggior parte dei comuni considerati efficienti con l'FDH non lo 
#sono nè per la dea-v nè per la dea-c. 
summary(dea_v_1994_una)
summary(dea_v_2003_una)
#Linefficienza media aumenta nel 2003. 



#SUMMARY PER L ANNO POST-POLICY: 
#Quindi, in maniera sintetica, per l'anno post-policy: per la fdh e dea-v, il numero di comuni efficienti è maggiore 
#nel caso del dataset ripulito da una sola variabile rispetto al dataset ripulito dai gross-outliers. 
# Mentre per la dea-c
#il numero di comuni efficienti è quasi simile, si differenziano per un solo comune.


#Confronto della dea-v per l anno pre-policy(1994) e post-policy(2003)
eff = (matrix(c(dea_v_1994_una$eff,dea_v_2003_una$eff),nrow=3870,ncol=1))
eff




#PUNTO DUE: 
#Utilizzerò la dea-v per procedere all'analisi. Il dataset invece sarà DAT_UNA. 
#Analizzare le variazioni dell'efficienza tecnica alla luce del cambiamento istituzionale del doppio turno
#Per entrambi i tipi di analisi, si tratta di effettuare le analisi difference-in-differences e di statistical
#matching.

# Analisi DiD per il doppio turno
dat_1994_2003<- subset(dat_una, anno==1994|anno==2003)
dat_1994_2003[dat_1994_2003$PopTot>=15000,]  #107 comuni effettivamente trattati. 
# creiamo una dummy temporale per il trattamento (post-1994)
dopo<-ifelse(dat_1994_2003$anno>1994, 1, 0)
# creare una dummy per i comuni effettivamente trattati (popolazione>15000 ab: doppio turno)
doppio<-ifelse(dat_1994_2003$PopTot>=15000,1,0)
# creare una dummy di interazione dopo*doppio (DiD)
did = dopo*doppio


# effettuo l'analisi DiD
rgr_1 <-lm(eff ~ dopo + doppio + did, data = dat_una) #la variabile dipendente è l'efficienza. 
summary(rgr_1)
#Consideriamo la regressione in cui la variabile dipendente è l'efficienza e le variabili
#indipendenti sono dopo, doppio e did. 
#Considerando un alpha pari a 0.05, tutti i p-value associati alle variabili sono prossimi allo zero
#per l'intercetta e la dummy dopo, leggermente più elevato per doppio e did, ma comunque
#tutti statisticamente significativi, per cui significa che hanno un impatto. 
#Il concetto da fissare è che un valore dello stimatore negativo implica un miglioramento
#dell'efficienza. 
#Per cui a parità di condizioni, il valore positivo per dopo implica che per l anno dopo il 1994
# l'efficienza è diminuita. 
#Al contrario, un valore negativo per il coefficiente  doppio implica che per i paesi
#in cui è stato introdotto il doppio turno , quindi i comuni con piu di 15000 abitanti, 
#l'efficienza è aumentata rispetto ai comuni in cui vige il singolo turno. 
#In particolare, la variabile che ci interessa più
#di tutte è did, che è negativa, per cui tutti i comuni effettivamente trattati 
#dopo il 1994 l'efficienza è aumentata. 

#Effettuo l'analisi di did aumentata
rgr_aumentata<-lm(eff~vecchiaia+laurea+superficie+
                    altitudine+litoraneità+gradourb+ruralità+dopo+doppio+did, data=dat_1994_2003 )
summary(rgr_aumentata)
#Svolgendo una regressione aumentata vediamo che did continua a rimanere
#significativo e negativo a un livello di alpha=0.05. Per cui è confermato 
#che c'è un miglioramento dell'efficienza. 
#L'unico dei coefficienti che non è per nulla significativo è ruralità, mentre
#se fissiamo un livello di significatività 0.10 anche dopo rientra nelle variabili
#significative. 
#Da un analisi generale di questo modello riusciamo a dedurre che: 
#1) i coefficienti di vecchiaia, altitudine, litoraneirà, grado di urbanizzazione basso,
#ruralità=1, dopo e doppio sono positivi il che significa , che nel quadro generale, 
#sono legate a inefficenza.
#2) Mentre i coefficienti associati a laurea, superficie, grado di urbanizzazione
#alta e did sono negativi, per cui sono legati all'efficienza. 

pdat_1994_2003<-pdata.frame(dat_1994_2003, c("codIstat", "anno"))
rgr_panel_aumentata<-plm(eff~vecchiaia+laurea+superficie+
                           altitudine+litoraneità+gradourb+ruralità+did, data=pdat_1994_2003, model="within", effect="time")
summary(rgr_panel_aumentata)
fixef(rgr_panel_aumentata, effect="time")
#Tuttavia, Impostando gli effetti fissi sul tempo ci rendiamo conto che entra in contrasto
#con ciò che abbiamo appena detto, per cui sembra che ci sia stato un lieve peggioramento dell'
#efficienza dopo l'applicazione del doppio turno. 

setwd("C:/Users/carme/OneDrive/Desktop/materiale tesina")
dat_una<-read.csv("dat_una.csv")
dat_una$litoraneità<-as.factor(dat_una$litoraneità)
dat_una$ruralità<-as.factor(dat_una$ruralità)
dat_una$gradourb<-as.factor(dat_una$gradourb)

#Passo allo Statistical Matching, nel quale userò la tecnica del nearest neighbor:
library(MatchIt)
dat.wide.tot<-reshape(data=dat_una,
                  idvar="codIstat",
                  v.names=c("QliRifiuti", "KmStrade", "Studenti", "NrPuntiLuce", "NrPermessi", "NrIscritti",
                            "KmPolizia", "NrCertificati", "ImpTot", "ImpPers", "ImpNoPers","PopTot", "vecchiaia",                         "laurea","superficie", "altitudine", "litoraneità", "gradourb", "ruralità"),
                  timevar="anno",
                  direction="wide")
dat.wide<-na.omit(dat.wide.tot)
# creare una dummy per ogni periodo, per i comuni effettivamente trattati (popolazione>15000 ab.: doppio turno)
doppio.1994 = ifelse(dat.wide$PopTot.1994>=15000,1,0)
doppio.2003 = ifelse(dat.wide$PopTot.2003>=15000,1,0)
#ANNO 1994
x<-matrix(c(dat.wide$ImpPers.1994, dat.wide$ImpNoPers.1994), nrow=1885, ncol=2)

y<-matrix(c(dat.wide$QliRifiuti.1994, dat.wide$KmStrade.1994, dat.wide$Studenti.1994,
            dat.wide$NrPuntiLuce.1994, dat.wide$NrPermessi.1994, dat.wide$NrIscritti.1994,
            dat.wide$KmPolizia.1994, dat.wide$NrCertificati.1994), nrow=1885, ncol=8)
w_dea_v_1994<- dea(x,y, RTS=1, ORIENTATION=2)
summary(w_dea_v_1994)

#ANNO 2003
x<-matrix(c(dat.wide$ImpPers.2003, dat.wide$ImpNoPers.2003), nrow=1885, ncol=2)
y<-matrix(c(dat.wide$QliRifiuti.2003, dat.wide$KmStrade.2003, dat.wide$Studenti.2003,
            dat.wide$NrPuntiLuce.2003, dat.wide$NrPermessi.2003, dat.wide$NrIscritti.2003,
            dat.wide$KmPolizia.2003, dat.wide$NrCertificati.2003), nrow=1885, ncol=8)
w_dea_v_2003<- dea(x,y, RTS=1, ORIENTATION=2)
summary(w_dea_v_2003)


library(MatchIt)
# Nearest neighbor matching : Abbinamento sull'unità più vicina, 1994
nonparam.1994 = matchit(doppio.1994 ~ vecchiaia.1994+laurea.1994+superficie.1994+altitudine.1994+litoraneità.1994+
                          gradourb.1994+ruralità.1994, data=dat.wide, distance = "mahalanobis")
nonparam.1994
summary(nonparam.1994)
#Le unità di controllo sono quelle del dataset mentre i trattati sono quelli effettivamente interessati
#dall'introduzione del doppio turno, ovvero i comuni con una popolazione totale >15000. Il numero di 
#unità di controlli è decisamente maggiore rispetto ai trattati per cui l'implementazione è attendibile. 
#Guardiamo al bilanciamento : con il matching andiamo a individuare i comuni gemelli tra trattati 
#e non trattati che hanno tratti simili, come: tasso di vecchiaia simili, tasso di laurea simili, 
#superficie simile, grado di urbanizzazione simile. Analizzando la media dei dati
#in generale notiamo una leggerenza differenza nel tasso di occupazione, superficie e ruralità.
#Forte differenza in grado di urbanizzazione e altitudine.Dopo il bilanciamento per i dati matchati 
#riusciamo a comprendere che i dati sono pressocchè simili, quindi ha fatto un buon abbinamento. 
data.w_nonparam.1994 = match.data(nonparam.1994)
lm.1994 = lm(w_dea_v_1994$eff ~ doppio.1994, data = data.w_nonparam.1994)
sm.1994 = summary(lm.1994)
sm.1994
#Doppio continua a essere statisticamente significativo e negativo, quindi implica che i comuni
#trattati sono più efficienti. 
with(data.w_nonparam.1994, t.test(w_dea_v_1994$eff ~ doppio.1994))  
#confermato anche dal test che individua una media di efficienza non trattati maggiore 
#rispetto ai trattati. Un valore più alto di eff implica una maggiore inefficienza
#allora realmente i trattati sono più efficienti. 

# statistical matching nonparametrico, 2003
# Nearest neighbor matching:Abbinamento sull'unità più vicina
nonparam.2003 = matchit(doppio.2003 ~ vecchiaia.1994+laurea.1994+superficie.1994+altitudine.1994+litoraneità.1994+
                          gradourb.1994+ruralità.1994, data=dat.wide, distance = "mahalanobis")
nonparam.2003
summary(nonparam.2003)
data.w_nonparam.2003 = match.data(nonparam.2003)
lm.2003 = lm(w_dea_v_2003$eff ~ doppio.2003, data = data.w_nonparam.2003)
sm.2003 = summary(lm.2003)
sm.2003
#doppio continua a essere statisticamente significativo e negativo, il che 
#implica che i comuni trattati dopo la policy sono più efficienti dei non trattati. 

with(data.w_nonparam.2003, t.test(w_dea_v_2003$eff  ~ doppio.2003))
#come prima, viene individuata una media di eff dei non trattati maggiore della media dei trattati
#per cui continua a confermare che i trattati sono più efficienti.

# statistical matching non parametrico, treatment effect
te.doppio = (sm.1994$coefficients["doppio.1994",1]-sm.2003$coefficients["doppio.2003",1])#aumento dell'efficienza
te.doppio.test = (sm.1994$coefficients["doppio.1994",1]-sm.2003$coefficients["doppio.2003",1])/(sm.2003$coefficients["doppio.2003",2]^2/30+sm.1994$coefficients["doppio.1994",2]^2/49)^0.5
te.doppio #differenza tra prima e dopo di un aumento di efficienza di 0.13
te.doppio.test #quanto è significativa l'incidenza dell'aumento dell efficienza



#T-STUDENT, per vedere se c' è una differenza significativa tra l'intercetta di doppio prima e dopo il 94
#il valore del test è un valore molto elevato, il che ci dice che il valore dell'efficienza del 2004 è maggiore
#di quella del 1994 del 13 per cento, per cui c'è un miglioramento dell'efficienza significativo al 0.01
#L'introduzione del doppio turno ha avuto un impatto positivo. 

#PASSO TRE: 
#Come controllo di robustezza dei risultati, effettuate l'analisi difference-in-differences con i comandi 
#"lm" e "plm" (v. DiD pievda PDS panel.txt, DiD pievda runoff panel.txt), usando ImpTot come variabile 
#dipendente (input, che dipende da vari output). Si può mantenere l'analisi nell'ambito di una forma 
#funzionale Cobb-Douglas.

library(plm)
dat_1994_2003<- subset(dat_una, anno==1994|anno==2003)
dopo<-ifelse(dat_una$anno>1994,1,0)
doppio<-ifelse(dat_una$PopTot>=15000,1,0)
did<-dopo*doppio

regr<-lm(log(ImpTot)~log(QliRifiuti)+log(KmStrade)+log(Studenti)+log(NrPuntiLuce)+log(NrPermessi)+log(NrIscritti)+log(KmPolizia)+log(NrCertificati)+dopo+doppio+did, data=dat_una)
summary(regr)
regr<-lm(log(ImpTot)~log(QliRifiuti)+log(KmStrade)+log(Studenti)+log(NrPuntiLuce)+log(NrPermessi)+log(NrIscritti)+log(KmPolizia)+log(NrCertificati)+dopo+doppio+did-1, data=dat_una)
summary(regr)

#dal modello di regressione visualizziamo che tutte e tre le variabili sono significative a un livello
#alpha pari a 0.10, did è negativa mentre dopo e doppio sono positive. Il che significa che
#la spesa per i comuni effettivamente trattati è diminuita leggermente. Addiritura eliminando l'intercetta
#doppio risulta essere statisticamente a livello alpha 0.05 per cento. 
panel_dat<-pdata.frame(dat_una, c("codIstat", "anno"))
panel_regr<-plm(log(ImpTot)~log(QliRifiuti)+log(KmStrade)+log(Studenti)+log(NrPuntiLuce)+log(NrPermessi)+log(NrIscritti)+log(KmPolizia)+log(NrCertificati)+did, data=panel_dat, model="within", effect="time")
summary(panel_regr)
#Did è non significativa con i dati panel , quindi non possiamo dare una spiegazione generale, 
#è come se la politica non avesse avuto impatto. 


#Svolgendo un subset della popolazione e restringendola a >8000, otteniamo che la variabile did è significativa al 10 
#per cento, rimane negativa. 
panel_regr<-plm(log(ImpTot)~log(QliRifiuti)+log(KmStrade)+log(Studenti)+log(NrPuntiLuce)+log(NrPermessi)+log(NrIscritti)+log(KmPolizia)+log(NrCertificati)+did, data=panel_dat, subset=(PopTot>8000), model="within", effect="time")
summary(panel_regr)
fixef(panel_regr, effect="time")
fixef(panel_regr, type = "dmean") #MAN A MANO CHE I COMUNI SONO PIU GRANDI diventano sempre meno efficienti. 

#Effetto sul tempo visualizziamo che con il crescere degli anni gli impegni di spesa totale
#crescono , anche se nel n333333ostro modello il valore di did è negativo. Chiaramente non 
#è piuttosto affidabile, considerato il p-value elevatissimo. Non possiamo affidarci ai risultati di panel
#per cui traiamo le conclusioni affidandoci ai risultati posti in essere dal passo
#due e dal comando lm. az


