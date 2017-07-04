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



