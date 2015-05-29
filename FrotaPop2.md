# População e Frota de Veículos
Mario Azevedo  
Domingo, 24/05/2015  

###Carregando as bibliotecas necessárias


```r
library(data.table)
library(dplyr)
library(stringr)
library(knitr)
library(pander)
library(ggplot2)
library(scales)
library(RCurl)
options(scipen=1, digits=2, width=105)
```

###Lendo os dados




```r
fileURL <- "http://www.denatran.gov.br/download/frota/Frota_por_Municipio_e_Tipo-ABR_15.rar"
localfilename <- "Frota_por_Municipio_e_Tipo-ABR_15.rar"
download.file(fileURL,localfilename,mode='wb')
z7 = paste('C:/"Program Files"/7-Zip/7z.exe e ',localfilename,sep='')
system(z7)
```



