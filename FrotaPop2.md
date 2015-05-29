# Frota de Veículos do Brasil - abril de 2015
Mario Azevedo  
Sexta, 29/05/2015  

###Carregando as bibliotecas necessárias


```r
library(data.table)
library(dplyr)
library(stringr)
library(knitr)
library(pander)
library(ggplot2)
options(scipen=1, digits=2, width=105)
```

###Lendo os dados



```r
#fileURL <- "http://www.denatran.gov.br/download/frota/Frota_por_Municipio_e_Tipo-ABR_15.rar"
#localfilename <- "Frota_por_Municipio_e_Tipo-ABR_15.rar"
#download.file(fileURL,localfilename,mode='wb')

dados <- fread('Frota_por_Municipio_e_Tipo-ABR_15.csv',sep=';')

sum(dados$TOTAL)
```

```
## [1] 88075445
```

```r
nrow(dados)
```

```
## [1] 5571
```

###Frota dos estados


```r
tabela <- group_by(dados,UF) %>%
        summarise(Nmun = n(),
                  Veiculos = sum(TOTAL),
                  Automoveis = sum(AUTOMOVEL),
                  Caminhoes=sum(CAMINHAO),
                  Onibus=sum(ONIBUS),
                  Motocicletas=sum(MOTOCICLETA))
kable(tabela)
```



UF    Nmun   Veiculos   Automoveis   Caminhoes   Onibus   Motocicletas
---  -----  ---------  -----------  ----------  -------  -------------
AC      22     228939        74449        6684     1020          95608
AL     102     687890       302123       20049     6606         229740
AM      62     771391       350852       19789     8835         211413
AP      16     168550        70876        3967     1078          54995
BA     417    3500576      1558977      111723    36820        1112340
CE     184    2654374       974893       66169    15247        1164977
DF       1    1611733      1162294       22802    11847         166083
ES      78    1710974       855989       68593    14623         398722
GO     246    3442771      1636121      105811    20896         790545
MA     217    1386081       369553       36291     7661         694650
MG     853    9574917      5281562      314095    71310        2247414
MS      79    1362613       632288       47884     8727         332906
MT     141    1730954       598935       64387    10526         537722
PA     144    1632071       503551       54799    16248         693582
PB     223    1061572       447855       27349     6840         407042
PE     185    2617278      1175101       88986    18828         910140
PI     224     972764       289365       25197     5852         463876
PR     399    6804610      4022829      257206    39196        1057123
RJ      92    6010416      4096249      140157    46123         827021
RN     167    1065711       470986       29671     6209         375082
RO      52     834042       235240       28521     5176         345727
RR      15     182935        59839        4228      914          70499
RS     497    6300149      3868458      216462    38712        1002160
SC     295    4505340      2591203      145425    18529         801153
SE      75     636629       287662       20406     6285         210849
SP     646   26034069     16502974      661087   151779        4181632
TO     139     586096       178092       21838     5012         201182

```r
nrow(tabela)
```

```
## [1] 27
```

