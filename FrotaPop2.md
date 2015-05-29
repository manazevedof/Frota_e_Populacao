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
cidades <- fread('estimativas_dou_2014_xls.csv',sep=';')
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

```r
cidades <- mutate(cidades,NOME_MUN = str_to_upper(iconv(NOME_MUN,to = "ASCII//TRANSLIT")))
arrange(cidades[!(cidades$NOME_MUN %in% dados$MUNICIPIO)],UF)
```

```
##     UF COD_UF COD_MUN                         NOME_MUN POPULACAO
##  1: BA     29   19058                LAJEDO DO TABOCAL      8810
##  2: BA     29   22250          MUQUEM DE SAO FRANCISCO     11552
##  3: CE     23    6306                          ITAPAGE     50671
##  4: GO     52    3500               BOM JESUS DE GOIAS     22872
##  5: MA     21    8504                    PINDARE-MIRIM     32037
##  6: MG     31    2506                  AMPARO DO SERRA      4997
##  7: MG     31    5509              BARAO DE MONTE ALTO      5738
##  8: MG     31   22900                     DONA EUSEBIA      6386
##  9: MG     31   27602                          GOUVEIA     12039
## 10: MG     31   47808                      PASSA-VINTE      2117
## 11: MG     31   50539                     PINGO-D'AGUA      4743
## 12: MG     31   53806                        QUELUZITO      1940
## 13: MG     31   65206             SAO THOME DAS LETRAS      7001
## 14: MT     51    5507 VILA BELA DA SANTISSIMA TRINDADE     15138
## 15: PA     15    6500             SANTA IZABEL DO PARA     65251
## 16: PB     25   13653                    JOCA CLAUDINO      2675
## 17: PB     25   15401                           SERIDO     10803
## 18: PB     25   16409                           TACIMA     10745
## 19: PI     22    9658  SAO FRANCISCO DE ASSIS DO PIAUI      5759
## 20: PR     41    2752             BELA VISTA DA CAROBA      3887
## 21: PR     41   16307                   MUNHOZ DE MELO      3883
## 22: PR     41   19251              PINHAL DE SAO BENTO      2732
## 23: PR     41   23303      SANTA CRUZ DE MONTE CASTELO      8194
## 24: RJ     33     233               ARMACAO DOS BUZIOS     30439
## 25: RN     24     208                              ACU     56829
## 26: RN     24    1305                   AUGUSTO SEVERO      9688
## 27: RN     24    2709                       CERRO CORA     11305
## 28: RN     24    5306                   JANUARIO CICCO      9767
## 29: RN     24    6205                     LAGOA D'ANTA      6640
## 30: RN     24    8409            OLHO-D'AGUA DO BORGES      4380
## 31: RO     11     338                      NOVA MAMORE     26925
## 32: RS     43    6932                      ENTRE-IJUIS      9068
## 33: SC     42   12809               BALNEARIO PICARRAS     19976
## 34: SC     42    9458                   LAJEADO GRANDE      1479
## 35: SC     42   13906       PRESIDENTE CASTELLO BRANCO      1670
## 36: SC     42   16909            SAO LOURENCO DO OESTE     23017
## 37: SC     42   17204              SAO MIGUEL DO OESTE     38575
## 38: SE     28    2601                   GRACHO CARDOSO      5836
## 39: SP     35   15004                   EMBU DAS ARTES    259053
## 40: SP     35   16101                         FLORINIA      2821
## 41: SP     35   30706                       MOGI GUACU    146114
## 42: SP     35   30805                       MOJI MIRIM     91027
## 43: SP     35   50001           SAO LUIS DO PARAITINGA     10726
##     UF COD_UF COD_MUN                         NOME_MUN POPULACAO
```

```r
arrange(dados[!(dados$MUNICIPIO %in% cidades$NOME_MUN)],UF)
```

```
##     UF                      MUNICIPIO  TOTAL AUTOMOVEL BONDE CAMINHAO CAMINHAO TRATOR CAMINHONETE
##  1: BA              LAGEDO DO TABOCAL   1017       388     0       37               1         100
##  2: BA        MUQUEM DO SAO FRANCISCO    793       229     0       35               8         107
##  3: CE                        ITAPAJE  13250      2695     0      324               6         671
##  4: MA                  PINDARE MIRIM   4683      1025     0       98               6         182
##  5: MG                AMPARO DA SERRA   1568       622     0       49               1          87
##  6: MG            BARAO D0 MONTE ALTO   1519       641     0       56              16          87
##  7: MG                   DONA EUZEBIA   2906      1059     0      308              34         334
##  8: MG                         GOUVEA   3796      2234     0      132               1         416
##  9: MG                    PASSA VINTE   1463       745     0      201               5         170
## 10: MG                   PINGO D'AGUA    756       329     0       34               1          44
## 11: MG                      QUELUZITA    731       338     0       24               0          64
## 12: MG            SAO TOME DAS LETRAS   1518       694     0       97               1         139
## 13: MT VILA BELA DA SANTISSIMA TRINDA   3643       711     0      132               7         355
## 14: PA           SANTA ISABEL DO PARA  14221      3768     0      683              72         802
## 15: PB               CAMPO DE SANTANA   1119       395     0       37               1          51
## 16: PB          SAO VICENTE DO SERIDO   1108       461     0       31               1          63
## 17: PI SAO FRANCISCO DE ASSIS DO PIAU    557        93     0       13               0          95
## 18: PR           BELA VISTA DO CAROBA   1998       995     0       64              13         133
## 19: PR                MUNHOZ DE MELLO   1851      1042     0       76              18         252
## 20: PR            PINHAL DO SAO BENTO   1233       590     0       50               4         108
## 21: PR    SANTA CRUZ DO MONTE CASTELO   4989      2546     0      226              10         480
## 22: RJ              ARMACAO DE BUZIOS  16402      9048     0      331               9        1128
## 23: RN                           ASSU  18636      5879     0      642              47        1246
## 24: RN                      BOA SAUDE   1608       503     0       68               1         102
## 25: RN                     CERRO-CORA   2603       767     0      102               0         216
## 26: RN                    LAGOA DANTA   1131       334     0       45               0          57
## 27: RN          OLHO D'AGUA DO BORGES    903       267     0       23               0          54
## 28: RN                   SERRA CAIADA   1522       605     0       37               2          75
## 29: RO                 NOVA DO MAMORE   7244      1297     0      292              21         720
## 30: RS                    ENTRE IJUIS   5753      3284     0      385             120         589
## 31: SC          BALNEARIO DE PICARRAS  11510      6360     0      341              70         813
## 32: SC                 LAGEADO GRANDE    882       468     0       43              10         122
## 33: SC           SAO LOURENCO D'OESTE  16713      9678     0      831             268        1257
## 34: SC             SAO MIGUEL D'OESTE  31460     17213     0     1114             432        2899
## 35: SE                GRACCHO CARDOSO   1195       316     0       20               0          27
## 36: SP                           EMBU 102610     63914     0     2838             665        6370
## 37: SP                       FLORINEA   1236       713     0       49              10         112
## 38: SP                     MOGI-GUACU 101031     54885     0     2576            1135        6302
## 39: SP                     MOGI-MIRIM  66021     36991     0     1977             167        5061
## 40: SP        MUNICIPIO NAO INFORMADO      1         1     0        0               0           0
## 41: SP         SAO LUIZ DO PARAITINGA   5144      2889     2      178               5         490
##     UF                      MUNICIPIO  TOTAL AUTOMOVEL BONDE CAMINHAO CAMINHAO TRATOR CAMINHONETE
##     CAMIONETA CHASSI PLATAF CICLOMOTOR MICRO-ONIBUS MOTOCICLETA MOTONETA ONIBUS QUADRICICLO REBOQUE
##  1:        12             0          0            5         437       20     10           0       5
##  2:         8             0          0           20         309       20     34           0      11
##  3:        85             1          1           37        8397      849     51           0      73
##  4:        23             0          8           19        2870      395     16           0      16
##  5:        15             0          0            8         723       14     35           0       7
##  6:        33             0          1            8         608       22     12           0      16
##  7:       115             0          3            3         845      124      6           0      17
##  8:        80             0          1           17         818       15     34           0      41
##  9:        62             0          1           24         215        8     11           0      15
## 10:         6             0          0            3         319        7      8           0       5
## 11:        18             0          0           10         262        5      5           0       5
## 12:        45             0          0           13         484        6     20           0      12
## 13:        46             0          3            3        1818      436     39           0      67
## 14:       330             1         85           57        6143     1705    150           0     264
## 15:        13             0          0            8         557       40     17           0       0
## 16:        27             0          0           10         487       12     10           0       5
## 17:         1             0          0            5         335       10      3           0       0
## 18:        32             0          0            3         671       35     21           0       4
## 19:        35             0          0            5         304       39      7           0      41
## 20:         8             0          0            5         421       23     19           0       1
## 21:       100             0          2           12        1146      285     49           0      96
## 22:       708             0        287          318        3094      959     58           0     199
## 23:       224             0          2           66        8282     1799     90           0     202
## 24:        20             0          0            4         814       73     12           0       6
## 25:        32             0          0           11        1357       94     12           0       8
## 26:        11             0          0           10         621       38     14           0       1
## 27:         9             0          0            3         466       76      1           0       4
## 28:        14             0          0            7         694       53     16           0      12
## 29:        65             0          1            2        3937      787     27           0      53
## 30:       108             0          2           10         880       77     25           0     108
## 31:       343             1          1           48        2042      950     48           0     267
## 32:         9             0          0            1         177       22      6           0      11
## 33:       311             0          5           52        2745      842     76           0     185
## 34:       883             0          4           68        5082     2302    104           0     560
## 35:         8             0          0           11         761       34      8           0       6
## 36:      3576             0         16          690       18979     2207    747           0     475
## 37:        38             1          5            7         245       13      9           0      21
## 38:      2635             1        610          493       23775     4984    575           0    1022
## 39:      1985             2        357          216       14762     2590    457           0     861
## 40:         0             0          0            0           0        0      0           0       0
## 41:       194             1          4           31        1196       31     29           0      57
##     CAMIONETA CHASSI PLATAF CICLOMOTOR MICRO-ONIBUS MOTOCICLETA MOTONETA ONIBUS QUADRICICLO REBOQUE
##     SEMI-REBOQUE SIDE-CAR OUTROS TRATOR ESTEI TRATOR RODAS TRICICLO UTILITARIO
##  1:            0        0      0            0            0        0          2
##  2:            9        0      0            0            0        2          1
##  3:            5        2      1            0            0        6         46
##  4:           11        0      0            0            0        8          6
##  5:            1        0      0            0            0        2          4
##  6:           12        0      0            0            0        1          6
##  7:           41        5      0            0            0        1         11
##  8:            2        0      0            0            1        0          4
##  9:            2        0      1            0            0        0          3
## 10:            0        0      0            0            0        0          0
## 11:            0        0      0            0            0        0          0
## 12:            0        0      0            0            0        0          7
## 13:           17        0      1            0            0        1          7
## 14:          105        0      3            0            5        8         40
## 15:            0        0      0            0            0        0          0
## 16:            0        0      0            0            0        0          1
## 17:            2        0      0            0            0        0          0
## 18:           24        0      0            0            2        1          0
## 19:           30        0      0            0            0        0          2
## 20:            3        0      0            0            0        0          1
## 21:           19        1      0            0            5        3          9
## 22:           17        0      0            0            2        5        239
## 23:           61        0      0            0            0       16         80
## 24:            2        0      1            0            0        0          2
## 25:            0        0      0            0            0        2          2
## 26:            0        0      0            0            0        0          0
## 27:            0        0      0            0            0        0          0
## 28:            4        0      0            0            0        1          2
## 29:           36        0      0            0            0        1          5
## 30:          140        0      6            0            2        0         17
## 31:           89        6      7            0           33        3         88
## 32:           12        0      0            0            0        0          1
## 33:          349        1      8            0           21        3         81
## 34:          571        5      7            0           10        5        201
## 35:            0        0      0            0            0        1          3
## 36:         1707        3     34            0           37       20        332
## 37:           10        0      0            0            0        1          2
## 38:         1567       14      3            0           10        8        436
## 39:          232        7      3            0           12       11        330
## 40:            0        0      0            0            0        0          0
## 41:            7        0      0            0            2        0         28
##     SEMI-REBOQUE SIDE-CAR OUTROS TRATOR ESTEI TRATOR RODAS TRICICLO UTILITARIO
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

