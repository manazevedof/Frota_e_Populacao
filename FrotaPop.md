# População e Frota de Veículos
Mario Azevedo  
Sunday, May 24, 2015  

###Carregando as bibliotecas necessárias


```r
library(data.table)
library(dplyr)
library(pander)
library(knitr)
library(stringr)
options(scipen=1, digits=2, width=120)
```

###Lendo os dados


```r
dados <- fread('FrotaBR122013.csv',sep=';')
estados <- fread('estados.csv',sep=';')
estados <- mutate(estados,CAPITAL=str_trim(CAPITAL))
estados <- mutate(estados,ESTADO=str_trim(ESTADO))
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

```r
str(estados)
```

```
## Classes 'data.table' and 'data.frame':	27 obs. of  3 variables:
##  $ SIGLA  : chr  "AC" "AL" "AP" "AM" ...
##  $ ESTADO : chr  "ACRE" "ALAGOAS" "AMAPÁ" "AMAZONAS" ...
##  $ CAPITAL: chr  "RIO BRANCO" "MACEIO" "MACAPA" "MANAUS" ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

```r
estados
```

```
##     SIGLA              ESTADO        CAPITAL
##  1:    AC                ACRE     RIO BRANCO
##  2:    AL             ALAGOAS         MACEIO
##  3:    AP               AMAPÁ         MACAPA
##  4:    AM            AMAZONAS         MANAUS
##  5:    BA               BAHIA       SALVADOR
##  6:    CE               CEARÁ      FORTALEZA
##  7:    DF    DISTRITO FEDERAL       BRASILIA
##  8:    ES      ESPÍRITO SANTO        VITORIA
##  9:    GO               GOIÁS        GOIANIA
## 10:    MA            MARANHÃO       SAO LUIS
## 11:    MT         MATO GROSSO         CUIABA
## 12:    MS  MATO GROSSO DO SUL   CAMPO GRANDE
## 13:    MG        MINAS GERAIS BELO HORIZONTE
## 14:    PA                PARÁ          BELEM
## 15:    PB             PARAÍBA    JOAO PESSOA
## 16:    PR              PARANÁ       CURITIBA
## 17:    PE          PERNAMBUCO         RECIFE
## 18:    PI               PIAUI       TERESINA
## 19:    RJ      RIO DE JANEIRO RIO DE JANEIRO
## 20:    RN RIO GRANDE DO NORTE          NATAL
## 21:    RS   RIO GRANDE DO SUL   PORTO ALEGRE
## 22:    RO            RONDÔNIA    PORTO VELHO
## 23:    RR             RORAIMA      BOA VISTA
## 24:    SC      SANTA CATARINA  FLORIANOPOLIS
## 25:    SP           SÃO PAULO      SAO PAULO
## 26:    SE             SERGIPE        ARACAJU
## 27:    TO           TOCANTINS         PALMAS
##     SIGLA              ESTADO        CAPITAL
```

###Características Gerais

1. Número de municípios, população e frota total, de automóveis e motocicletas por estado


```r
tabela <- group_by(dados,UF) %>%
        summarise(Nmun = n(),
                  Populacao = sum(POPULACAO),
                  Veiculos = sum(TOTAL),
                  Automoveis = sum(AUTOMOVEL),
                  Motocicletas = sum(MOTOCICLETA))

kable(tabela)
```



UF    Nmun   Populacao   Veiculos   Automoveis   Motocicletas
---  -----  ----------  ---------  -----------  -------------
AC      22      776463     205777        67461          86048
AL     102     3300935     614566       275556         199317
AM      62     3807921     700849       325698         183987
AP      16      734996     152634        64386          50326
BA     417    15044137    3158326      1415342        1002208
CE     184     8778576    2384395       881571        1045078
DF       1     2789761    1511110      1099719         154277
ES      78     3839366    1585076       797528         373365
GO     246     6434048    3169088      1512266         743879
MA     217     6794301    1215478       335793         601923
MG     853    20593356    8884663      4926454        2106326
MS      79     2587269    1253199       580821         313390
MT     141     3182113    1565739       543484         495288
PA     144     7999729    1428355       455649         593828
PB     223     3914421     959085       407624         366699
PE     185     9208550    2396738      1088338         828291
PI     224     3184166     855445       255114         410745
PR     399    10997465    6351183      3759306        1011274
RJ      92    16369179    5568514      3839651         748356
RN     167     3373959     967299       430289         340918
RO      52     1728214     758308       212652         319337
RR      15      488072     165339        53704          64676
RS     497    11164043    5885383      3622309         959339
SC     295     6634254    4201255      2428891         763019
SE      75     2195662     575510       262664         190159
SP     645    43663669   24560201     15643414        3978276
TO     139     1478164     527213       158702         184135

2. População e frota total, de automóveis e motocicletas da capitais


```r
tabela <- filter(dados,MUNICIPIO %in% estados$CAPITAL) %>% 
        mutate(CAPITAL = MUNICIPIO) %>%
        select (CAPITAL,UF,POPULACAO,TOTAL,AUTOMOVEL) %>%
        mutate (APM = 1000*AUTOMOVEL/POPULACAO) %>%
        arrange (desc(APM))

kable(tabela)
```



CAPITAL          UF    POPULACAO     TOTAL   AUTOMOVEL   APM
---------------  ---  ----------  --------  ----------  ----
CURITIBA         PR      1848946   1429534     1000903   541
FLORIANOPOLIS    SC       453285    305028      206845   456
BELO HORIZONTE   MG      2479165   1596081     1101919   444
SAO PAULO        SP     11821873   7010508     4971813   421
GOIANIA          GO      1393575   1045796      564554   405
BRASILIA         DF      2789761   1511110     1099719   394
PORTO ALEGRE     RS      1467816    802932      571299   389
VITORIA          ES       348268    185427      122229   351
CUIABA           MT       569830    344189      178035   312
CAMPO GRANDE     MS       832352    483039      248372   298
RIO DE JANEIRO   RJ      6429923   2451155     1824803   284
ARACAJU          SE       614577    257261      154271   251
PALMAS           PR        46294     17898       10991   237
NATAL            RN       853928    339429      200312   235
PALMAS           TO       257904    144562       60132   233
RECIFE           PE      1599513    609765      371833   232
JOAO PESSOA      PB       769607    298796      172667   224
FORTALEZA        CE      2551806    908074      511109   200
TERESINA         PI       836475    380576      166131   199
PORTO VELHO      RO       484992    222218       92648   191
SALVADOR         BA      2883682    785257      533990   185
SAO LUIS         MA      1053922    327808      177176   168
MACEIO           AL       996733    266465      161275   162
BOA VISTA        RR       308996    145678       49527   160
MANAUS           AM      1982177    581179      311179   157
RIO BRANCO       AC       357194    139683       53215   149
BELEM            PA      1425922    373846      204801   144
MACAPA           AP       437256    121519       52922   121
RIO BRANCO       MT         5063      2421         534   105
BOA VISTA        PB         6669      1388         527    79
BELEM            PB        17495      2829        1218    70
BELEM            AL         4737       760         247    52
CAMPO GRANDE     RN         9660      1666         390    40
CAMPO GRANDE     AL         9631      1604         341    35

