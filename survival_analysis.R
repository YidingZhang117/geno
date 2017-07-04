#????Ԥ????
setwd("/Users/jimchen/Downloads/gene2")
cli<-read.table("clinical_data.txt",sep="\t",header = T)
geno<-read.table("genomicMatrix.txt",sep="\t",header=T, check.names=FALSE)
geno$genename<-as.matrix(geno$sample)
gene<-as.matrix(read.table("standard_gene.txt"))
#????geno?е????л???????gene?еĻ????Աȣ??ҳ?genelist??????
result<-match(gene,as.character(geno$genename),nomatch=NA)
#???????в???һ????
genenum<-result[-which(is.na(result))]
genematrix<-geno[genenum,]
genematrix$genename<-NULL
#??genematrixת?ó?t3
sample<-as.matrix(names(genematrix))
sample<-as.matrix(sample[-1,])
temp<-data.frame(genematrix,row.names = 1)
t1<-t(temp)
t2<-as.data.frame(t1,row.names = F)
t3<-as.data.frame(cbind(sample,t2))
#??os??os_ind??cli????????ȡ??��
cli_os<-data.frame(cli$sampleID,cli$X_OS,cli$X_OS_IND)
cli_os<-na.omit(cli_os)
#?ѻ???????t3???ٴ???Ϣcli_os?????????ϲ???һ??
#fix(t3)#??t3?е?????sample??ΪsampleID
colnames(t3)[1]<-"sampleID"
colnames(cli_os)[1]<-"sampleID"
total<-merge(t3,cli_os,by="sampleID")#temp<-match(t3$sampleID,cli$sampleID,nomatch=NA)
write.table(total, "bindData.txt", sep = '\t', quote = FALSE, col.names = NA)
total2<-read.table("total.txt",sep="\t",header=T, check.names=FALSE)#????????tumor??normal????Ϣ
total_tumor<-subset(total2,class=="tumor")
#survival package
library(survival)
fit1<-survfit(Surv(total_tumor$cli.X_OS,total_tumor$cli.X_OS_IND)~1)
plot(fit1,main="Kaplan-Meier curve,",xlab="time",ylab="probability")
#estimate the cumulative hazard function
temp1<-summary(survfit(Surv(total_tumor$cli.X_OS,total$cli.X_OS_IND)~1,type="fh"))
list(temp1$time,-log(temp1$surv))
temp1$surv

#cox regression model
#compute beta
coxfit1<-coxph(Surv(cli.X_OS,cli.X_OS_IND)~class,data=total_tumor)#?ο?coxfit1<-coxph(Surv(time,status)~x,data=aml)
summary(coxfit1)
#obtain the cumulative baseline hazard estimator
basehaz(coxph(Surv(cli.X_OS,cli.X_OS_IND)~class,data=total_tumor))
#obtain the survival function
survfit(coxfit1,newdata=data.frame(total_tumor$class=1))


