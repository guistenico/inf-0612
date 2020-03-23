setwd("/home/diogo/MDC/inf-0612/Trabalho")

# captura os dados da CEPAGRI presentes no arquivo .csv na página do professor
names <- c("horario", "temp", "vento", "umid", "sensa")
con <- url("https://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
cepagri <- read.table(con, header = FALSE, fill = TRUE, sep = ";", col.names = names)
close(con)

# salva os dados em um arquivo .csv
con <- file("cepagri.csv", "w")
write.csv(cepagri, con)
close(con)
