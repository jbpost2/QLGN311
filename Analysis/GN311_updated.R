## DJS - GN 311

library(readr)
library(tidyr)
library(ggplot2)
library(kableExtra)
library(formattable)
library(dplyr)
library(ggpubr)
library(RColorBrewer)


## New dataset, updated end of May 2020
gn311 <- read_csv(
  "GN311wST311currentdeID_updated.csv") %>%
  as.data.frame()

###### Questions #######
#   What are the highest math classes that students took? (See MathCourses-SPRG 2020)

# Change NA to No Record
gn311$MTHcourse[is.na(gn311$MTHcourse)] <- "No Record"

# Make data frame "A" for first graph
A<-as.data.frame(table(gn311$MTHcourse,gn311$year))
colnames(A) <- c("MTH","Year","Counts")
A$Proportion <- NA
A$Proportion[1:19] <- A$Counts[1:19]/sum(A$Counts[1:19])
A$Proportion[20:38] <- A$Counts[20:38]/sum(A$Counts[20:38])

# Graph for highest math classes
p.maxMTH <-
  ggdotchart(data=A, 
             x="MTH", y="Proportion", 
             group="MTH",
             add="segment", 
             color="MTH",
             size = 1.5,
             ggtheme = theme_pubclean() ) + 
  facet_grid(.~Year) +
  geom_text(aes(label=Counts),vjust=-.75, size=3) +
  theme(legend.position="none")

# Data Frame for table

B1<-rbind.data.frame(A$Counts[1:19],A$Proportion[1:19])
colnames(B1)<-c(levels(A$MTH))
B2<-rbind.data.frame(A$Counts[20:38],A$Proportion[20:38]) 
colnames(B2)<-c(levels(A$MTH))
B<-t(rbind.data.frame(B1,B2))
B<-rbind.data.frame(B,colSums(B))
row.names(B)<-NULL
nams <- c(levels(A$MTH),"Total")
B<-cbind.data.frame(nams,B)
colnames(B) <- c("MTH","N","%","N","%")
B[,3]<-percent(B[,3],digits=1);B[,5]<-percent(B[,5],digits=1)

t.maxMTH <-
  kable(B, booktabs = T, longtable=F,
        caption="\\textbf{Highest Math Course}") %>%
  kable_styling(bootstrap_options = "striped",full_width = F) %>%
  column_spec(1,bold=T) %>%
  row_spec(0,bold = T, color = "#CC0000")%>%
  add_header_above(c("","2019"=2,"2020"=2)) %>%
  scroll_box(width = "100%", height = "500px")

# When did they take their highest math? This year (2019-2020), last year, 2 years ago, 3 years ago, etc.?

gn311$MTHcourseTerm <- 
  factor(gn311$MTHcourseTerm,
         levels=c("2020 Spring Term",
                  "2019 Fall Term","2019 Summer Term 2","2019 Summer Term 1","2019 Spring Term",
                  "2018 Fall Term","2018 Summer Term 2","2018 Summer Term 1","2018 Spring Term",
                  "2017 Fall Term","2017 Summer Term 1","2017 Spring Term",
                  "2016 Fall Term","2016 Spring Term" ,
                  "2015 Fall Term","2015 Summer Term 2",
                  "2014 Fall Term","2014 Spring Term",
                  "2013 Fall Term","2013 Spring Term",
                  "2011 Summer Term 2","2011 Spring Term",
                  "2010 Fall Term"))

t1 <- table(gn311$MTHcourseTerm[gn311$year==2019],gn311$MTHcourse[gn311$year==2019])[,-16]
t1 <- addmargins(t1,FUN = list(Total = sum),quiet=TRUE)

t.whenMTH19 <-
  kable(t1, 
        booktabs = T, longtable=F) %>%
  kable_styling(bootstrap_options = "striped",full_width = F, fixed_thead = T) %>%
  column_spec(1,bold=T) %>%
  row_spec(0,bold = T, color = "#CC0000") %>%
  scroll_box(width = "100%", height = "500px")

t2 <- table(gn311$MTHcourseTerm[gn311$year==2020],gn311$MTHcourse[gn311$year==2020])[,-16]
t2 <- addmargins(t2,FUN = list(Total = sum),quiet=TRUE)

t.whenMTH20 <-
  kable(t2, 
        booktabs = T, longtable=F) %>%
  kable_styling(bootstrap_options = "striped",full_width = F, fixed_thead = T) %>%
  column_spec(1,bold=T) %>%
  row_spec(0,bold = T, color = "#CC0000") %>%
  scroll_box(width = "100%", height = "500px")

G19 <-
  table(gn311$MTHcourseTerm[gn311$year==2019],gn311$MTHcourse[gn311$year==2019]) %>%
  as.data.frame() %>%
  droplevels()
G19[,4] <- 2019
colnames(G19)<-c("Term","MTH","Count","Year")

G19 <-
  G19 %>%
  filter(Count > median(G19$Count[G19$Count>0]))

G20 <-
  table(gn311$MTHcourseTerm[gn311$year==2020],gn311$MTHcourse[gn311$year==2020]) %>%
  as.data.frame() %>%
  droplevels
G20[,4] <- 2020
colnames(G20)<-c("Term","MTH","Count","Year")

G20 <-
  G20 %>%
  filter(Count > median(G20$Count[G20$Count>0]))

G<-rbind.data.frame(G19,G20)
summary(G)
G <- droplevels(G)

G$Term <- factor(G$Term,c("2016 Fall Term","2017 Spring Term","2017 Fall Term","2018 Spring Term",
                          "2018 Fall Term","2019 Spring Term","2019 Fall Term","2020 Spring Term"))

g.whenMTH <-
  ggplot(data=G,aes(x=MTH,y=Term,fill=Count)) +
  geom_tile() +
  facet_grid(.~Year) +
  scale_fill_gradient(low = "#FFFFFF",
                      high = "#2774AE") +
  theme_pubclean() 

# How many students have taken ST 311? (See ST311-Completed-SPRG 2020) PRIOR to enrollment in GN 311

gn311$ST311courseTerm[gn311$ST311current=="current" & gn311$year==2020]

summary(as.factor(gn311$ST311courseTerm))
gn311$ST311courseTerm <- factor(gn311$ST311courseTerm, levels =
                                  c("2020 Spring Term",
                                    "2019 Fall Term","2019 Summer Term 2","2019 Summer Term 1","2019 Spring Term",
                                    "2018 Fall Term","2018 Summer Term 1","2018 Spring Term",
                                    "2017 Fall Term","2017 Summer Term 2","2017 Spring Term",
                                    "2016 Fall Term","2016 Spring Term" ,
                                    "2015 Fall Term",
                                    "2014 Fall Term",
                                    "2011 Fall Term"))
H19 <-
  table(gn311$ST311courseTerm[gn311$year==2019 & gn311$ST311courseTerm!="2019 Fall Term"]) %>%
  as.data.frame() %>%
  mutate(percent= Freq/sum(Freq),year=2019)

H20 <- table(gn311$ST311courseTerm[gn311$year==2020 & gn311$ST311courseTerm!="2020 Spring Term"]) %>%
  as.data.frame() %>%
  mutate(percent= Freq/sum(Freq),year=2020)

H <- cbind.data.frame(H19[,1:3],H20[,2:3])

Hh <- rbind.data.frame( H[,2:5],colSums(H[,2:5]) )
H[,1] <- as.character(H[,1])
Hh<- cbind.data.frame(c(H[,1],"Total"),Hh)
colnames(Hh) <- c("Term","N","%","N","%")
Hh[,3]<-percent(Hh[,3],digits=1); Hh[,5]<-percent(Hh[,5],digits=1)

t.ST311prior <-
  kable(Hh[-1,], 
        booktabs = T, longtable=F, row.names = FALSE) %>%
  kable_styling(bootstrap_options = "striped",full_width = F, fixed_thead = T) %>%
  column_spec(1,bold=T) %>%
  row_spec(0,bold = T, color = "#CC0000") %>%
  add_header_above(c("","2019"=2,"2020"=2)) %>%
  scroll_box(width = "100%", height = "350px")

# How many people are currently taking ST 311? (See ST322-InProgress SPRG 2020)

table(gn311$ST311current[gn311$year==2019])
table(gn311$ST311current[gn311$year==2020])

CE <- as.data.frame(
  matrix( c( 36,39/(441+36), 441,441/(441+36),
             28,28/(28+462), 462,462/(28+462) ),
          nrow=2, ncol=4, byrow=T ) )
colnames(CE) <- c("N","%","N","%")
CE[,2] <- percent(CE[,2],digits=1); CE[,4] <- percent(CE[,4],digits=1)
CE <- cbind.data.frame(" "=c("2019","2020"),CE)

t.CE <-
  kable(CE, booktabs = T, longtable=F) %>%
  kable_styling(bootstrap_options = "striped",full_width = FALSE) %>%
  column_spec(1,bold=T) %>%
  row_spec(0,bold = T, color = "#CC0000") %>%
  add_header_above(c("","Enrolled"=2,"Not Enrolled"=2))

# What was the average GPA for ST 311?
STsum <-
  rbind.data.frame(
    round( summary(na.omit(gn311$ST311grdpts[gn311$ST311current=="current" & 
                                               gn311$year==2019])),1 ), # summary of current 2019 summary
    round( summary(na.omit(gn311$ST311grdpts[gn311$ST311current=="NOT enrolled"])),1 ),# summary of before current
    round( summary(na.omit(gn311$ST311grdpts)),1 ) #overall 
  ) 
STsum[,1]<-digits(STsum[,1],1)
colnames(STsum) <- c("Min","Q1","Median","Mean","Q3", "Max")
Ns <-
  c(
    length(na.omit(gn311$ST311grdpts[gn311$ST311current=="current" & 
                                       gn311$year==2019])),#39
    length(na.omit(gn311$ST311grdpts[gn311$ST311current=="NOT enrolled"])), #412
    length(na.omit(gn311$ST311grdpts)) #457
  )

STnms <- c("Current (2019)","Prior","Overall")

STnms <- cbind.data.frame(" "=STnms,STsum,N=Ns)
#few students repeated the course and are calculated across current enrollment and previously taken

t.ST <-
  kable(STnms, booktabs = T, longtable=F) %>%
  kable_styling(bootstrap_options = "striped",full_width = FALSE) %>%
  column_spec(1,bold=T) %>%
  row_spec(0,bold = T, color = "#CC0000")

# We want to know how many students got Homework 1 question 10 correct. We also want to know how many people got Homework 2 question 9 correct.
# (Note that there is more than 1 version of question 10 and 9 in the homeworks proved. The correct answer is provided.)

correct <- vector()
correct1 <- vector()
for( i in 1:nrow(gn311))
{
  if( is.na(gn311$HW1_Q10_Ans)[i] ) 
  { correct[i] = "No Record" }
  else if( is.na(gn311$HW1_Q10)[i] & !is.na(gn311$HW1_Q10_Ans)[i] )
  { correct[i] = 0 }
  else if( gn311$HW1_Q10[i] == gn311$HW1_Q10_An[i] )
  { correct[i] = 1 } else { correct[i] = 0 }
  
  if( is.na(gn311$HW2_Q9_Ans)[i] ) 
  { correct1[i] = "No Record" }
  else if( is.na(gn311$HW2_Q9)[i] & !is.na(gn311$HW2_Q9_Ans)[i] )
  { correct1[i] = 0 }
  else if( gn311$HW2_Q9[i] == gn311$HW2_Q9_Ans[i] )
  { correct1[i] = 1 } else { correct1[i] = 0 }
}
summary(as.factor(correct))
summary(as.factor(correct1))
gn311$Q9correct <- as.factor(correct1)
gn311$Q10correct <- as.factor(correct)


length(gn311$X1[is.na(gn311$HW1_Q10_Ans)]) #33 have no record
length(gn311$X1[is.na(gn311$HW1_Q10_Ans) & is.na(gn311$HW1_Q10)]) #33
length(gn311$X1[!is.na(gn311$HW1_Q10_Ans) & is.na(gn311$HW1_Q10)]) #4 people did not answer

length(gn311$X1[is.na(gn311$HW2_Q9_Ans)]) #37 have no record
length(gn311$X1[is.na(gn311$HW2_Q9_Ans) & is.na(gn311$HW2_Q9)]) #37
length(gn311$X1[!is.na(gn311$HW2_Q9_Ans) & is.na(gn311$HW2_Q9)]) #0 did not answer

bcorrect <- vector()
for(i in 1:nrow(gn311))
{
  if(gn311$Q9correct[i] == 1 & gn311$Q10correct[i] == 1)
  { bcorrect[i] = 1 } else { bcorrect[i] = 0 }
}

gn311$bothQscorrect <- as.character(bcorrect)

summary(gn311[gn311$year==2019,c(15,16)])
gn311$X1[gn311$Q9correct=="No Record" & !(gn311$Q10correct=="No Record")]
gn311$X1[gn311$Q10correct=="No Record" & !(gn311$Q9correct=="No Record")]

gn311$bothQscorrect <- as.character(gn311$bothQscorrect)
gn311$bothQscorrect[gn311$Q10correct=="No Record"] <- "No Record"

QC19 <-
  rbind.data.frame(
    summary(as.factor(gn311$Q9correct[gn311$year==2019])),
    summary(as.factor(gn311$Q10correct[gn311$year==2019])),
    summary(as.factor(gn311$bothQscorrect[gn311$year==2019])) )
qc19nms <- c("Q9","Q10","Both")
QC19 <- cbind.data.frame(qc19nms,QC19)
colnames(QC19) <- c("","Correct","Incorrect","No Record")

QC20 <-
  rbind.data.frame(
    summary(as.factor(gn311$Q9correct[gn311$year==2020])),
    summary(as.factor(gn311$Q10correct[gn311$year==2020])),
    summary(as.factor(gn311$bothQscorrect[gn311$year==2020])) )
qc20nms <- c("Q9","Q10","Both")
QC20 <- cbind.data.frame(qc20nms,QC20)
colnames(QC20) <- c("","Correct","Incorrect","No Record")

rowSums(QC19[,2:4]/477)
rowSums(QC20[,2:4]/490)

QC <-
  cbind.data.frame(
    matrix( c( QC19[1,2],QC19[1,2]/477 , QC19[1,3],QC19[1,3]/477 , QC19[1,4],QC19[1,4]/477 ,
               QC19[2,2],QC19[2,2]/477 , QC19[2,3],QC19[2,3]/477 , QC19[2,4],QC19[2,4]/477 ,
               QC19[3,2],QC19[3,2]/477 , QC19[3,3],QC19[3,3]/477 , QC19[3,4],QC19[3,4]/477 ,
               QC20[1,2],QC20[1,2]/490 , QC20[1,3],QC20[1,3]/490 , QC20[1,4],QC20[1,4]/490 ,
               QC20[2,2],QC20[2,2]/490 , QC20[2,3],QC20[2,3]/490 , QC20[2,4],QC20[2,4]/490 ,
               QC20[3,2],QC20[3,2]/490 , QC20[3,3],QC20[3,3]/490 , QC20[3,4],QC20[3,4]/490 ),
            nrow=6, ncol=6, byrow=T ) )

QC <- QC[,c(3,4,1,2,5,6)]

QCc <- rbind.data.frame(
  c(QC[1,],QC[4,]), c(QC[2,],QC[5,]), c(QC[3,],QC[6,]) )

QCc[,2] <- percent(QCc[,2],digits=1); QCc[,4] <- percent(QCc[,4],digits=1); QCc[,6] <- percent(QCc[,6],digits=1)
QCc[,8] <- percent(QCc[,8],digits=1); QCc[,10] <- percent(QCc[,10],digits=1); QCc[,12] <- percent(QCc[,12],digits=1)
colnames(QCc) <- c(rep(c("N","%"),6))
qc20nms <- c("Q9","Q10","Both")
QCc <- cbind.data.frame(qc20nms,QCc)
colnames(QCc)[1]<-" "
row.names(QCc) <- NULL
t.QC <- 
  kable(QCc, booktabs = T, longtable=F) %>%
  kable_styling(bootstrap_options = "striped",full_width = FALSE) %>%
  column_spec(1,bold=T) %>%
  column_spec(8:13,background ="#7BAFD4" ) %>%
  row_spec(0,bold = T, color = "#CC0000") %>%
  add_header_above(c("","Correct"=2,"Incorrect"=2,"No Record"=2, "Correct"=2,"Incorrect"=2,"No Record"=2)) %>%
  add_header_above(c("","2019"=6,"2020"=6))

# Does it make a difference whether someone has taken ST 311 or not as to whether they got question 10 or 9 correct? (Maybe split this into students who have taken stats, not taken stats, and are currently taking stats?)
# Then we want to answer the same questions for Fall 2019

gn311$ST311[is.na(gn311$ST311) & gn311$ST311current=="current"] <- 311
tookST311 <- vector()
for( i in 1:nrow(gn311) )
{
  if( is.na(gn311$ST311[i]) ) 
  {tookST311[i] <- "Not Taken" } else {tookST311[i] <- "Took 311"}
}

gn311$Q10correct <- relevel(gn311$Q10correct, ref="1")
gn311$Q9correct <- relevel(gn311$Q9correct, ref="1")
gn311$bothQscorrect <- relevel(as.factor(gn311$bothQscorrect), ref="1")
gn311 <- cbind.data.frame(gn311, tookST311)

## Overall
g.binomQ9 <- glm(Q9correct ~ tookST311,
                 family="binomial",data=gn311[ gn311$Q9correct!="No Record",])
summary(g.binomQ9)

g.binomQ10 <- glm(Q10correct ~ tookST311,
                  family="binomial",data=gn311[ gn311$Q10correct!="No Record",])
summary(g.binomQ10)

g.binomb <- glm(as.factor(bothQscorrect) ~ tookST311,
                family="binomial",data=gn311[ gn311$bothQscorrect!="No Record",])
summary(g.binomb) 

## within years
# 2019
g.binomQ919 <- glm(Q9correct ~ tookST311,
                   family="binomial", data = gn311[gn311$year==2019 & gn311$Q9correct!="No Record",])
summary(g.binomQ919)

# 2020
g.binomQ920 <- glm(Q9correct ~ tookST311,
                   family="binomial", data = gn311[gn311$year==2020 & gn311$Q9correct!="No Record",])
summary(g.binomQ920)
predict(g.binomQ920,type="response")

## by year
g.binomy20 <- glm(as.factor(bothQscorrect) ~ as.factor(year),
                  family="binomial", data = gn311[gn311$Q9correct!="No Record",])
summary(g.binomy20) 

## Q9 table 2019
table(gn311$tookST311[gn311$year==2019],
      gn311$Q9correct[gn311$year==2019])
## Q10 table 2019
table(gn311$tookST311[gn311$year==2019],
      gn311$Q10correct[gn311$year==2019])

## both table 2019
table(gn311$tookST311[gn311$year==2019],
      gn311$bothQscorrect[gn311$year==2019])

cbind(table(gn311$tookST311[gn311$year==2019],
            gn311$Q9correct[gn311$year==2019]),
      table(gn311$tookST311[gn311$year==2019],
            gn311$Q10correct[gn311$year==2019]),
      table(gn311$tookST311[gn311$year==2019],
            gn311$bothQscorrect[gn311$year==2019]))

## Q9 table 2020
table(gn311$tookST311[gn311$year==2020],
      gn311$Q9correct[gn311$year==2020])
## Q10 table 2020
table(gn311$tookST311[gn311$year==2020],
      gn311$Q10correct[gn311$year==2020])

## both table 2020
table(gn311$tookST311[gn311$year==2020],
      gn311$bothQscorrect[gn311$year==2020])

cbind(table(gn311$tookST311[gn311$year==2020],
            gn311$Q9correct[gn311$year==2020]),
      table(gn311$tookST311[gn311$year==2020],
            gn311$Q10correct[gn311$year==2020]),
      table(gn311$tookST311[gn311$year==2020],
            gn311$bothQscorrect[gn311$year==2020]))

tST <-
  rbind(
    cbind(table(gn311$tookST311[gn311$year==2019],
                gn311$Q9correct[gn311$year==2019]),
          table(gn311$tookST311[gn311$year==2019],
                gn311$Q10correct[gn311$year==2019]),
          table(gn311$tookST311[gn311$year==2019],
                gn311$bothQscorrect[gn311$year==2019]) ),
    cbind(table(gn311$tookST311[gn311$year==2020],
                gn311$Q9correct[gn311$year==2020]),
          table(gn311$tookST311[gn311$year==2020],
                gn311$Q10correct[gn311$year==2020]),
          table(gn311$tookST311[gn311$year==2020],
                gn311$bothQscorrect[gn311$year==2020])))  

rownames(tST) <- NULL
tST <-
  cbind(c("2019","2019","2020","2020"),
        c("Not Taken","Took 311","Not Taken","Took 311"),
        tST)
colnames(tST) <- c(" "," ",rep(c("Correct","Incorrect","No Record"),3))

t.tST <-
  kable(tST, booktabs = T, longtable=F) %>%
  kable_styling(bootstrap_options = "striped",full_width = T) %>%
  column_spec(1,bold=T) %>%
  column_spec(6:8,background ="#7BAFD4" ) %>%
  row_spec(0,bold = T, color = "#CC0000") %>%
  collapse_rows(columns = 1, valign = "top") %>%
  add_header_above(c("","","Q9"=3,"Q10"=3,"Both"=3)) 

## Overall
summary(as.factor(as.numeric(gn311$ST311courseTerm)))
summary(gn311$ST311courseTerm)
g.full19 <- glm(bothQscorrect ~ MTHcourse + MTHcourseTerm + tookST311 + ST311current,
                family="binomial",data=gn311[gn311$bothQscorrect!="No Record",])


##Looking at homework 6 along with previous homework assignments
#Looks like there are many question from homework 6 to cover... Just need to take the score column
# and see if it is the max of that column and deal with nas or missing

gn311 %>% select(starts_with("HW6") & ends_with("score")) %>% View()

binary <- gn311 %>% select(starts_with("HW6") & ends_with("score")) %>% 
  apply(MARGIN = 2, FUN = function(x){
    ifelse(as.numeric(x)/max(as.numeric(x), na.rm = TRUE) == 1, 1, 0)
  }
  ) %>% as.data.frame()

#make sure there are no duplicate names from the original
names(binary) <- paste(names(binary), "bin", sep = "_")

#check on all questions...
binary$HW6_all_bin <- apply(binary, FUN = sum, MARGIN = 1)/length(names(binary))

#get full data
full_data <- cbind(gn311, binary)

#Now look at tables for each question as compared to the first questions - do so by cohort (fall/spring)
table(full_data$bothQscorrect, full_data$HW6_Q_3_score_bin,full_data$Cohort)
