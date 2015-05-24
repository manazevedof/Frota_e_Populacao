# População e Frota de Veículos
Mario Azevedo  
Sunday, May 24, 2015  

###Carregando as bibliotecas necessárias


```r
library(data.table)
library(dplyr)
library(pander)
options(scipen=1, digits=2,width=100)
```

###Lendo os dados


```r
dados <- fread('FrotaBR122013.csv',sep=';')
str(dados)
```

```
## Classes 'data.table' and 'data.frame':	5570 obs. of  25 variables:
##  $ UF             : chr  "AC" "AC" "AC" "AC" ...
##  $ MUNICIPIO      : chr  "ACRELANDIA" "ASSIS BRASIL" "BRASILEIA" "BUJARI" ...
##  $ POPULACAO      : int  13353 6480 22899 9003 9836 80377 16099 32411 7147 16410 ...
##  $ TOTAL          : int  3835 963 5670 1260 1246 21991 3649 2227 62 2347 ...
##  $ AUTOMOVEL      : int  809 197 1443 374 349 4441 978 370 8 329 ...
##  $ BONDE          : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ CAMINHAO       : int  201 15 186 86 67 616 183 93 2 71 ...
##  $ CAMINHAO TRATOR: int  9 0 45 2 0 44 36 0 0 6 ...
##  $ CAMINHONETE    : int  292 75 532 160 138 1765 393 261 8 204 ...
##  $ CAMIONETA      : int  36 10 68 14 9 234 39 31 0 14 ...
##  $ CHASSI PLATAF  : int  0 0 0 1 0 1 0 0 0 0 ...
##  $ CICLOMOTOR     : int  0 0 1 2 0 0 0 0 0 1 ...
##  $ MICRO-ONIBUS   : int  6 2 3 3 1 53 1 1 0 2 ...
##  $ MOTOCICLETA    : int  2151 582 2529 570 567 10685 1443 1114 30 1388 ...
##  $ MOTONETA       : int  252 76 750 27 90 3931 493 340 14 309 ...
##  $ ONIBUS         : int  41 3 11 10 9 60 13 6 0 9 ...
##  $ QUADRICICLO    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ REBOQUE        : int  22 3 22 8 12 17 18 6 0 1 ...
##  $ SEMI-REBOQUE   : int  13 0 70 0 3 61 42 0 0 11 ...
##  $ SIDE-CAR       : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ OUTROS         : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ TRATOR ESTEI   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ TRATOR RODAS   : int  0 0 0 0 0 1 0 0 0 0 ...
##  $ TRICICLO       : int  0 0 3 2 0 4 1 1 0 0 ...
##  $ UTILITARIO     : int  3 0 7 1 1 78 9 4 0 2 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

###Características Gerais

1. Número de município, população e frota total por estado


```r
tabela <- group_by(dados,UF) %>%
        summarise(Nmun = n(),
                  Populacao = sum(POPULACAO),
                  Veiculos = sum(TOTAL),
                  Automoveis = sum(AUTOMOVEL),
                  Motocicletas = sum(MOTOCICLETA))
```

```r
pander(data.frame(tabela))
```


-----------------------------------------------
 UF   Nmun   Populacao   Veiculos   Automoveis 
---- ------ ----------- ---------- ------------
 AC    22     776463      205777      67461    

 AL   102     3300935     614566      275556   

 AM    62     3807921     700849      325698   

 AP    16     734996      152634      64386    

 BA   417    15044137    3158326     1415342   

 CE   184     8778576    2384395      881571   

 DF    1      2789761    1511110     1099719   

 ES    78     3839366    1585076      797528   

 GO   246     6434048    3169088     1512266   

 MA   217     6794301    1215478      335793   

 MG   853    20593356    8884663     4926454   

 MS    79     2587269    1253199      580821   

 MT   141     3182113    1565739      543484   

 PA   144      8e+06     1428355      455649   

 PB   223     3914421     959085      407624   

 PE   185     9208550    2396738     1088338   

 PI   224     3184166     855445      255114   

 PR   399    10997465    6351183     3759306   

 RJ    92    16369179    5568514     3839651   

 RN   167     3373959     967299      430289   

 RO    52     1728214     758308      212652   

 RR    15     488072      165339      53704    

 RS   497    11164043    5885383     3622309   

 SC   295     6634254    4201255     2428891   

 SE    75     2195662     575510      262664   

 SP   645    43663669    24560201    15643414  

 TO   139     1478164     527213      158702   
-----------------------------------------------

Table: Table continues below

 
--------------
 Motocicletas 
--------------
    86048     

    199317    

    183987    

    50326     

   1002208    

   1045078    

    154277    

    373365    

    743879    

    601923    

   2106326    

    313390    

    495288    

    593828    

    366699    

    828291    

    410745    

   1011274    

    748356    

    340918    

    319337    

    64676     

    959339    

    763019    

    190159    

   3978276    

    184135    
--------------


teste
