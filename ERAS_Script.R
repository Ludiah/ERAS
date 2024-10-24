### ERAS ###################################################################
## by Ludiah Bagakas

#libraries 
library(readxl)
library(dplyr)
library(Cronbach)
library(ltm)

#set working directory
setwd("C:/Users/lmnba/OneDrive/Documents/STA 635/Reliability")

#read data
ERAS=read.csv("ERAS data.csv", sep="," )
TR=read.csv("Test-Retest.csv", sep =",")
ALT=read.csv("Alternate.csv", sep = ",")

head(ERAS)
head(TR)
head(ALT)

#deletion of unmatched data
  #FTR
TR_Number=table(TR$Number)
FTR=TR[!TR$Number%in%as.numeric(rownames(TR_Number[TR_Number<2])),]
  #ALT
ALT_Number=table(ALT$Number) 
FALT=ALT[!ALT$Number%in%as.numeric(rownames(ALT_Number[ALT_Number<2])),]

#Part 1: Score the ERAS
  #ERAS 
ERAS['ERAS.RECREATIONAL']<-(ERAS['Q1']+ERAS['Q2']+ERAS['Q3']+ERAS['Q4']
              +ERAS['Q5']+ERAS['Q6']+ERAS['Q7']+ERAS['Q8']
              +ERAS['Q9']+ERAS['Q10'])

ERAS.ACADEMIC<-(ERAS['Q11']+ERAS['Q12']+ERAS['Q13']+ERAS['Q14']
                +ERAS['Q15']+ERAS['Q16']+ERAS['Q17']+ERAS['Q18']
                +ERAS['Q19']+ERAS['Q20'])

ERAS.TOTAL<-(ERAS['Q1']+ERAS['Q2']+ERAS['Q3']+ERAS['Q4']
            +ERAS['Q5']+ERAS['Q6']+ERAS['Q7']+ERAS['Q8']
            +ERAS['Q9']+ERAS['Q10']+ERAS['Q11']+ERAS['Q12']
            +ERAS['Q13']+ERAS['Q14']+ERAS['Q15']+ERAS['Q16']
            +ERAS['Q17']+ERAS['Q18']+ERAS['Q19']+ERAS['Q20'])

  #Test-Retest 
FTR.RECREATIONAL<-(FTR['Q1']+FTR['Q2']+FTR['Q3']+FTR['Q4']
                  +FTR['Q5']+FTR['Q6']+FTR['Q7']+FTR['Q8']
                  +FTR['Q9']+FTR['Q10'])

FTR.ACADEMIC<-(FTR['Q11']+FTR['Q12']+FTR['Q13']+FTR['Q14']
          +FTR['Q15']+FTR['Q16']+FTR['Q17']+FTR['Q18']
          +FTR['Q19']+FTR['Q20'])

FTR.TOTAL<-(FTR['Q1']+FTR['Q2']+FTR['Q3']+FTR['Q4']
      +FTR['Q5']+FTR['Q6']+FTR['Q7']+FTR['Q8']
      +FTR['Q9']+FTR['Q10']+FTR['Q11']+FTR['Q12']
      +FTR['Q13']+FTR['Q14']+FTR['Q15']+FTR['Q16']
      +FTR['Q17']+FTR['Q18']+FTR['Q19']+FTR['Q20'])

  #Alternate
FALT.RECREATIONAL<-(FALT['Q1']+FALT['Q2']+FALT['Q3']+FALT['Q4']
              +FALT['Q5']+FALT['Q6']+FALT['Q7']+FALT['Q8']
              +FALT['Q9']+FALT['Q10'])

FALT.ACADEMIC<-(FALT['Q11']+FALT['Q12']+FALT['Q13']+FALT['Q14']
          +FALT['Q15']+FALT['Q16']+FALT['Q17']+FALT['Q18']
          +FALT['Q19']+FALT['Q20'])

FALT.TOTAL<-(FALT['Q1']+FALT['Q2']+FALT['Q3']+FALT['Q4']
      +FALT['Q5']+FALT['Q6']+FALT['Q7']+FALT['Q8']
      +FALT['Q9']+FALT['Q10']+FALT['Q11']+FALT['Q12']
      +FALT['Q13']+FALT['Q14']+FALT['Q15']+FALT['Q16']
      +FALT['Q17']+FALT['Q18']+FALT['Q19']+FALT['Q20'])

#Part 2: Test-Retest Reliability
  #pearson correlation
TR1=FTR['Time']==1
TR2=FTR['Time']==2

ALT1=FALT['Time']==1
ALT2=FALT['Time']==2

  #pearson correlation TR 
tr.rec.t1.t2=cor(FTR.RECREATIONAL[TR1,], FTR.RECREATIONAL[TR2,], use="everything",
               method="pearson")
  
tr.ac.t1.t2=cor(FTR.ACADEMIC[TR1,], FTR.ACADEMIC[TR2,], use="everything",
               method="pearson")
  
tr.tot.t1.t2=cor(FTR.TOTAL[TR1,], FTR.TOTAL[TR2,], use="everything",
               method="pearson")

  #pearson correlation ALT
alt.rec.t1.t2=cor(FALT.RECREATIONAL[ALT1,], FALT.RECREATIONAL[ALT2,], use="everything",
                 method="pearson")

alt.aca.t1.t2=cor(FALT.ACADEMIC[ALT1,], FALT.ACADEMIC[ALT2,], use="everything",
                method="pearson")

alt.tot.t1.t2=cor(FALT.TOTAL[ALT1,], FALT.TOTAL[ALT2,], use="everything",
                 method="pearson")

  #pearson correlation split-halfs
eras.aca.a.b=cor(FALT.TOTAL[ALT1,], FALT.TOTAL[ALT2,], use="everything",
                 method="pearson")
  
#Part 3: Alternate Reliability 
  #deletion of unmatched data
  #pearson correlation
Grade.ALT=ALT['Grade']
alt.cor.ac.rec=case_when(
  Grade.ALT==6~cor(ALT$ACADEMIC, TR$RECREATIONAL, use="everything",
               method="pearson"))
  
alt.cor.ac.tot=case_when(
  Grade.ALT==6~cor(ALT$ACADEMIC, TR$TOTAL, use="everything",
               method="pearson"))
  
alt.cor.rec.tot=case_when(
                  Grade.ALT==6~cor(ALT$RECREATIONAL, TR$TOTAL, use="everything",
               method="pearson"))

#Part 4: Internal Consistency Reliability
  #subscales
ACADEMIC_A<-(ERAS['Q11']+ERAS['Q12']+ERAS['Q13']+ERAS['Q14']
            +ERAS['Q15'])

ACADEMIC_B<-(ERAS['Q16']+ERAS['Q17']+ERAS['Q18']
            +ERAS['Q19']+ERAS['Q20'])

RECREATIONAL_A<-(ERAS['Q1']+ERAS['Q2']+ERAS['Q3']+ERAS['Q4']
                +ERAS['Q5'])
  
RECREATIONAL_B<-(ERAS['Q6']+ERAS['Q7']+ERAS['Q8']
                +ERAS['Q9']+ERAS['Q10'])

  #pearson correlation split-halfs
eras.aca.a.b=cor(ACADEMIC_A, ACADEMIC_B, use="everything",
                 method="pearson")

eras.rec.a.b=cor(RECREATIONAL_A, RECREATIONAL_B, use="everything",
                 method="pearson")

  #pearson average correlation REC
avg.1<-cor(ERAS['Q1'], ERAS.RECREATIONAL, use="everything",
           method="pearson")
avg.2<-cor(ERAS['Q2'], ERAS.RECREATIONAL, use="everything",
           method="pearson")
avg.3<-cor(ERAS['Q3'], ERAS.RECREATIONAL, use="everything",
           method="pearson")
avg.4<-cor(ERAS['Q4'], ERAS.RECREATIONAL, use="everything",
           method="pearson")
avg.5<-cor(ERAS['Q5'], ERAS.RECREATIONAL, use="everything",
           method="pearson")
avg.6<-cor(ERAS['Q6'], ERAS.RECREATIONAL, use="everything",
           method="pearson")
avg.7<-cor(ERAS['Q7'], ERAS.RECREATIONAL, use="everything",
           method="pearson")
avg.8<-cor(ERAS['Q8'], ERAS.RECREATIONAL, use="everything",
           method="pearson")
avg.9<-cor(ERAS['Q9'], ERAS.RECREATIONAL, use="everything",
           method="pearson")
avg.10<-cor(ERAS['Q10'], ERAS.RECREATIONAL, use="everything",
           method="pearson")

eras.avg.rec<-(avg.1+avg.2+avg.3+avg.4+avg.5+avg.6+avg.7+avg.8+avg.9+avg.10)/10

#pearson average correlation ACA
ac.avg.1<-cor(ERAS['Q11'], ERAS.ACADEMIC, use="everything",
           method="pearson")
ac.avg.2<-cor(ERAS['Q12'], ERAS.ACADEMIC, use="everything",
           method="pearson")
ac.avg.3<-cor(ERAS['Q13'], ERAS.ACADEMIC, use="everything",
           method="pearson")
ac.avg.4<-cor(ERAS['Q14'], ERAS.ACADEMIC, use="everything",
           method="pearson")
ac.avg.5<-cor(ERAS['Q15'], ERAS.ACADEMIC, use="everything",
           method="pearson")
ac.avg.6<-cor(ERAS['Q16'], ERAS.ACADEMIC, use="everything",
           method="pearson")
ac.avg.7<-cor(ERAS['Q17'], ERAS.ACADEMIC, use="everything",
           method="pearson")
ac.avg.8<-cor(ERAS['Q18'], ERAS.ACADEMIC, use="everything",
           method="pearson")
ac.avg.9<-cor(ERAS['Q19'], ERAS.ACADEMIC, use="everything",
           method="pearson")
ac.avg.10<-cor(ERAS['Q20'], ERAS.ACADEMIC, use="everything",
            method="pearson")

eras.avg.aca<-(ac.avg.1+ac.avg.2+ac.avg.3+ac.avg.4+ac.avg.5+ac.avg.6+ac.avg.7+ac.avg.8+ac.avg.9+ac.avg.10)/10


#pearson average inter correlation REC
rec.inter.1<-cor(ERAS['Q1'], RECREATIONAL_A, use="everything",
           method="pearson")
rec.inter.2<-cor(ERAS['Q2'], RECREATIONAL_A, use="everything",
           method="pearson")
rec.inter.3<-cor(ERAS['Q3'], RECREATIONAL_A, use="everything",
           method="pearson")
rec.inter.4<-cor(ERAS['Q4'], RECREATIONAL_A, use="everything",
           method="pearson")
rec.inter.5<-cor(ERAS['Q5'], RECREATIONAL_A, use="everything",
           method="pearson")
rec.inter.6<-cor(ERAS['Q6'], RECREATIONAL_B, use="everything",
           method="pearson")
rec.inter.7<-cor(ERAS['Q7'], RECREATIONAL_B, use="everything",
           method="pearson")
rec.inter.8<-cor(ERAS['Q8'], RECREATIONAL_B, use="everything",
           method="pearson")
rec.inter.9<-cor(ERAS['Q9'], RECREATIONAL_B, use="everything",
           method="pearson")
rec.inter.10<-cor(ERAS['Q10'], RECREATIONAL_B, use="everything",
            method="pearson")

inter.rec<-(rec.inter.1+rec.inter.2+rec.inter.3+rec.inter.4+rec.inter.5+rec.inter.6+rec.inter.7+rec.inter.8+rec.inter.9+rec.inter.10)/10

#pearson average inter correlation ACA
aca.inter.1<-cor(ERAS['Q11'], ACADEMIC_A, use="everything",
                 method="pearson")
aca.inter.2<-cor(ERAS['Q12'], ACADEMIC_A, use="everything",
                 method="pearson")
aca.inter.3<-cor(ERAS['Q13'], ACADEMIC_A, use="everything",
                 method="pearson")
aca.inter.4<-cor(ERAS['Q14'], ACADEMIC_A, use="everything",
                 method="pearson")
aca.inter.5<-cor(ERAS['Q15'], ACADEMIC_A, use="everything",
                 method="pearson")
aca.inter.6<-cor(ERAS['Q16'], ACADEMIC_B, use="everything",
                 method="pearson")
aca.inter.7<-cor(ERAS['Q17'], ACADEMIC_B, use="everything",
                 method="pearson")
aca.inter.8<-cor(ERAS['Q18'], ACADEMIC_B, use="everything",
                 method="pearson")
aca.inter.9<-cor(ERAS['Q19'], ACADEMIC_B, use="everything",
                 method="pearson")
aca.inter.10<-cor(ERAS['Q20'], ACADEMIC_B, use="everything",
                  method="pearson")

inter.aca<-(aca.inter.1+aca.inter.2+aca.inter.3+aca.inter.4+aca.inter.5+aca.inter.6+aca.inter.7+aca.inter.8+aca.inter.9+aca.inter.10)/10

#cronbach alpha
cronbach.alpha(ERAS[paste0('Q', (11:20))], standardized = FALSE, CI = FALSE, 
               probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)
cronbach.alpha(ERAS[paste0('Q', (1:10))], standardized = FALSE, CI = FALSE, 
               probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)
