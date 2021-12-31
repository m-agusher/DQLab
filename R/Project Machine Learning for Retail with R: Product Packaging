#{Machine Learning for Retail}

#[Data : "https://storage.googleapis.com/dqlab-dataset/transaksi_dqlab_retail.tsv"]

#[Petunjuk Penyelesaian Project]

library(arules)

transaksi_tabular <- read.transactions(file="https://storage.googleapis.com/dqlab-dataset/transaksi_dqlab_retail.tsv", format="single", sep="\t", cols=c(1,2), skip=1)
write(transaksi_tabular, file="test_project_retail_1.txt", sep=",")

#[Output Awal: Statistik Top 10]

library (arules)

data <- read.transactions(file = "transaksi_dqlab_retail.tsv", format="single", sep="\t", cols=c(1,2), skip=1)
top_10 <- sort(itemFrequency(data, type ="absolute"),decreasing=TRUE)[1:10]
data_frame <- data.frame("Nama Produk"=names(top_10), "Jumlah" = top_10, row.names = NULL)
write.csv(data_frame, file="top10_item_retail.txt")

#[Output Awal: Statistik Bottom 10]

library(arules) 

data <- read.transactions(file="transaksi_dqlab_retail.tsv", format = "single", sep="\t", cols=c(1,2), skip = 1)
Bot_10 <- sort(itemFrequency(data, type ="absolute"),decreasing=FALSE) [1:10]
dataframe <- data.frame("Nama Produk" = names(Bot_10), "Jumlah" = Bot_10, row.names=NULL)
write.csv(dataframe, file ="bottom10_item_retail.txt")

#[Mendapatkan Kombinasi Produk yang menarik]

library(arules)

data <- read.transactions(file="transaksi_dqlab_retail.tsv", format="single", sep="\t", col=c(1,2), skip =1)
variable_untuk_rules <- apriori(data, parameter = list(supp=10/length(data), conf=0.5, minlen= 2, maxlen= 3))
variable_untuk_rules <- head(sort(variable_untuk_rules, by='lift', decreasing=TRUE),n=10)
write(variable_untuk_rules, file="kombinasi_retail.txt")

#[Mencari Paket Produk yang bisa dipasangkan dengan Item Slow-Moving]

library(arules)

data <- read.transactions(file="transaksi_dqlab_retail.tsv", format="single", sep="\t", cols=c(1,2), skip=1)

jumlah_transaksi <- length(data)
jumlah_kemunculan_minimal <- 10
variable_untuk_rules <- apriori(data, parameter= list(supp=jumlah_kemunculan_minimal/jumlah_transaksi, conf=0.1, minlen=2, maxlen=3))

variable_untuk_rules1 <- subset(variable_untuk_rules, lift>1 & rhs %in% "Tas Makeup")
variable_untuk_rules1 <- sort(variable_untuk_rules1, by="lift", decreasing=TRUE)[1:3]
variable_untuk_rules2 <- subset(variable_untuk_rules, lift>1 & rhs %in% "Baju Renang Pria Anak-anak")
variable_untuk_rules2 <- sort(variable_untuk_rules2, by="lift", decreasing=TRUE)[1:3]

variable_untuk_rules <- c(variable_untuk_rules1, variable_untuk_rules2)
inspect(variable_untuk_rules)

write(variable_untuk_rules, file= "kombinasi_retail_slow_moving.txt")
