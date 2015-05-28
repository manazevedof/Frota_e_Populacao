# População e Frota de Veículos
Mario Azevedo  
Domingo, 24/05/2015  

###Carregando as bibliotecas necessárias


```r
library(data.table)
library(dplyr)
library(stringr)
library(knitr)
library(ggplot2)
library(scales)
options(scipen=1, digits=2, width=105)
```

###Lendo os dados

Os dados da frota são de dezembro de 2013 (DENATRAN). Os dados de população são de outubro de 2013 (estimativa do IBGE).



```r
dados <- fread('FrotaBR122013.csv',sep=';')
estados <- fread('estados.csv',sep=';')
estados <- mutate(estados,CAPITAL=str_trim(CAPITAL))
estados <- mutate(estados,ESTADO=str_trim(ESTADO))
setkey(dados,UF)
setkey(estados,SIGLA)
dados <- select(dados[estados],-c(ESTADO,CAPITAL))
```

###Características Gerais

1. Para os estados: número de municípios, população, frota total, taxa de veículos por 1000 habitantes (VPM), frota de automóveis e taxa de automóveis por 1000 habitantes (APM). Lista em ordem decrescente de APM.


```r
tabela <- group_by(dados,UF) %>%
        summarise(Nmun = n(),
                  Populacao = sum(POPULACAO),
                  Veiculos = sum(TOTAL),
                  VPM = 1000 * sum(TOTAL)/sum(POPULACAO),                
                  Automoveis = sum(AUTOMOVEL),
                  APM = 1000 * sum(AUTOMOVEL)/sum(POPULACAO)) %>%
        arrange(desc(APM))
kable(tabela)
```



UF    Nmun   Populacao   Veiculos   VPM   Automoveis   APM
---  -----  ----------  ---------  ----  -----------  ----
DF       1     2789761    1511110   542      1099719   394
SC     295     6634254    4201255   633      2428891   366
SP     645    43663669   24560201   562     15643414   358
PR     399    10997465    6351183   578      3759306   342
RS     497    11164043    5885383   527      3622309   324
MG     853    20593356    8884663   431      4926454   239
GO     246     6434048    3169088   493      1512266   235
RJ      92    16369179    5568514   340      3839651   235
MS      79     2587269    1253199   484       580821   224
ES      78     3839366    1585076   413       797528   208
MT     141     3182113    1565739   492       543484   171
RN     167     3373959     967299   287       430289   128
RO      52     1728214     758308   439       212652   123
SE      75     2195662     575510   262       262664   120
PE     185     9208550    2396738   260      1088338   118
RR      15      488072     165339   339        53704   110
TO     139     1478164     527213   357       158702   107
PB     223     3914421     959085   245       407624   104
CE     184     8778576    2384395   272       881571   100
BA     417    15044137    3158326   210      1415342    94
AP      16      734996     152634   208        64386    88
AC      22      776463     205777   265        67461    87
AM      62     3807921     700849   184       325698    86
AL     102     3300935     614566   186       275556    83
PI     224     3184166     855445   269       255114    80
PA     144     7999729    1428355   179       455649    57
MA     217     6794301    1215478   179       335793    49

2. Para as capitais: população, frota total, taxa de veículos por 1000 habitantes (VPM), frota de automóveis, taxa de automóveis por 1000 habitantes (APM), frota de motocicletas e taxa de motocicletas por 1000 habitantes (MPM). Lista em ordem decrescente de APM.


```r
tabela <- filter(dados, str_c(UF,MUNICIPIO,sep='-') %in% 
                         str_c(estados$SIGLA,estados$CAPITAL,sep='-')) %>%
        mutate(VPM = 1000 * TOTAL/POPULACAO) %>%
        mutate(APM = 1000 * AUTOMOVEL/POPULACAO) %>%
        mutate(MPM = 1000 * MOTOCICLETA/POPULACAO) %>%
        select(UF,MUNICIPIO,POPULACAO,TOTAL,VPM,AUTOMOVEL,APM,MOTOCICLETA,MPM) %>%
        arrange(desc(APM))

APMcap <- 1000 * sum(tabela$AUTOMOVEL)/sum(tabela$POPULACAO)
APMbr <- 1000 * sum(dados$AUTOMOVEL)/sum(dados$POPULACAO)
kable(tabela)
```



UF   MUNICIPIO         POPULACAO     TOTAL   VPM   AUTOMOVEL   APM   MOTOCICLETA   MPM
---  ---------------  ----------  --------  ----  ----------  ----  ------------  ----
PR   CURITIBA            1848946   1429534   773     1000903   541        128882    70
SC   FLORIANOPOLIS        453285    305028   673      206845   456         41553    92
MG   BELO HORIZONTE      2479165   1596081   644     1101919   444        197150    80
SP   SAO PAULO          11821873   7010508   593     4971813   421        799411    68
GO   GOIANIA             1393575   1045796   750      564554   405        206724   148
DF   BRASILIA            2789761   1511110   542     1099719   394        154277    55
RS   PORTO ALEGRE        1467816    802932   547      571299   389         83947    57
ES   VITORIA              348268    185427   532      122229   351         19861    57
MT   CUIABA               569830    344189   604      178035   312         74171   130
MS   CAMPO GRANDE         832352    483039   580      248372   298        114443   137
RJ   RIO DE JANEIRO      6429923   2451155   381     1824803   284        238855    37
SE   ARACAJU              614577    257261   419      154271   251         50421    82
RN   NATAL                853928    339429   397      200312   235         76403    89
TO   PALMAS               257904    144562   561       60132   233         38772   150
PE   RECIFE              1599513    609765   381      371833   232        119498    75
PB   JOAO PESSOA          769607    298796   388      172667   224         77126   100
CE   FORTALEZA           2551806    908074   356      511109   200        229154    90
PI   TERESINA             836475    380576   455      166131   199        133767   160
RO   PORTO VELHO          484992    222218   458       92648   191         71455   147
BA   SALVADOR            2883682    785257   272      533990   185        105207    36
MA   SAO LUIS            1053922    327808   311      177176   168         78601    75
AL   MACEIO               996733    266465   267      161275   162         51637    52
RR   BOA VISTA            308996    145678   471       49527   160         54343   176
AM   MANAUS              1982177    581179   293      311179   157        121656    61
AC   RIO BRANCO           357194    139683   391       53215   149         53553   150
PA   BELEM               1425922    373846   262      204801   144         88211    62
AP   MACAPA               437256    121519   278       52922   121         38673    88

```r
ordem <- reorder(tabela$MUNICIPIO,tabela$APM)
ggplot(data=tabela, aes(x=ordem, y=APM)) +
        geom_bar(stat="identity",fill="darkblue") +
        coord_flip() +
        geom_hline(aes(yintercept=APMcap),color="red") +
        geom_hline(aes(yintercept=APMbr),color="green") +
        xlab("Capitais") +
        ylab("Automóveis por 1000 Habitantes")
```

![](FrotaPop_files/figure-html/unnamed-chunk-4-1.png) 

3. População e frota total, de automóveis e motocicletas das regiões


```r
tabela <- group_by(dados,REGIAO) %>%
                summarise(Populacao = sum(POPULACAO),
                  Veiculos = sum(TOTAL),
                  Automoveis = sum(AUTOMOVEL),
                  Motocicletas = sum(MOTOCICLETA)) %>%
        mutate(APM = 1000 * Automoveis/Populacao) %>%
        mutate(MPM = 1000 * Motocicletas/Populacao) %>%
        select(REGIAO,Populacao,Veiculos,Automoveis,APM,Motocicletas,MPM) %>%
        arrange(desc(APM))
kable(tabela)
```



REGIAO    Populacao   Veiculos   Automoveis   APM   Motocicletas   MPM
-------  ----------  ---------  -----------  ----  -------------  ----
S          28795762   16437821      9810506   341        2733632    95
SE         84465570   40598454     25207047   298        7206323    85
CO         14993191    7499136      3736290   249        1706834   114
NE         55794707   13126842      5352291    96        4985338    89
N          17013559    3938475      1338252    79        1482337    87

```r
ordem <- reorder(tabela$REGIAO,tabela$APM)
ggplot(data=tabela, aes(x=ordem, y=APM)) +
        geom_bar(stat="identity",fill="darkblue") +
        geom_hline(aes(yintercept=APMbr),color="green") +
        xlab("Regiões") +
        ylab("Automóveis por 1000 Habitantes")
```

![](FrotaPop_files/figure-html/unnamed-chunk-5-1.png) 

4. Lista das 20 cidades com maiores percentuais de motocicletas na frota. Ordem decrescente do percentual de motocicletas.


```r
tabela <- mutate(dados,percM = 100 * MOTOCICLETA/TOTAL) %>%
        arrange(desc(percM)) %>%
        select(MUNICIPIO,UF,TOTAL,MOTOCICLETA,percM)
kable(tabela[1:20])
```



MUNICIPIO                      UF    TOTAL   MOTOCICLETA   percM
-----------------------------  ---  ------  ------------  ------
MARAJA DO SENA                 MA      367           341      93
SAO ROBERTO                    MA      623           537      86
LAGOA DO MATO                  MA     1162           991      85
BACURITUBA                     MA      419           356      85
LAGOA GRANDE DO MARANHAO       MA      818           695      85
CURUA                          PA      365           309      85
SAO JOAO DO CARU               MA      746           630      84
PEREIRO                        CE     7342          6180      84
DUQUE BACELAR                  MA      633           531      84
LIMOEIRO DO AJURU              PA      155           130      84
SANTA CRUZ DO ARARI            PA       93            78      84
MADEIRO                        PI      630           524      83
PEDRO DO ROSARIO               MA     1123           930      83
JOCA MARQUES                   PI      495           407      82
SUCUPIRA DO RIACHAO            MA      580           475      82
POCAO DE PEDRAS                MA     3624          2960      82
SANTANA DE MANGUEIRA           PB      747           609      82
PARNAGUA                       PI      784           638      81
PASSAGEM FRANCA                MA     2132          1726      81
SAO RAIMUNDO DO DOCA BEZERRA   MA      483           391      81

5. Lista das 20 cidades com maiores taxas de automóveis por 1000 habitantes. Ordem decrescente de APM.


```r
tabela <- mutate(dados,APM = 1000 * AUTOMOVEL/POPULACAO) %>%
        select(MUNICIPIO,UF,POPULACAO,AUTOMOVEL,APM) %>%
        arrange(desc(APM))
kable(tabela[1:20])
```



MUNICIPIO                      UF    POPULACAO   AUTOMOVEL   APM
-----------------------------  ---  ----------  ----------  ----
SAO CAETANO DO SUL             SP       156362       98738   631
SANTA BARBARA DO MONTE VERDE   MG         2972        1811   609
RIO PRETO                      MG         5487        3249   592
CURITIBA                       PR      1848946     1000903   541
VINHEDO                        SP        69845       37051   530
BOM JESUS DO NORTE             ES        10095        5290   524
CAMPINAS                       SP      1144862      565408   494
SANTO ANDRE                    SP       704942      347984   494
AGUAS DE SAO PEDRO             SP         3004        1471   490
RIO BONITO                     RJ        56942       27447   482
VALINHOS                       SP       116308       55808   480
JUNDIAI                        SP       393920      187851   477
BLUMENAU                       SC       329082      152955   465
SAO BERNARDO DO CAMPO          SP       805895      367772   456
FLORIANOPOLIS                  SC       453285      206845   456
JARDIM OLINDA                  PR         1424         646   454
GRAMADO                        RS        34110       15440   453
CASCA                          RS         8993        4037   449
NOVA PETROPOLIS                RS        20126        8981   446
BELO HORIZONTE                 MG      2479165     1101919   444

6. Relação completa dos estados, destacando o número de cidades nas quais a frota de motocicletas é maior do que a de automóveis.


```r
tabela <- mutate(dados,maismoto = (MOTOCICLETA>AUTOMOVEL)) %>%
        group_by(UF) %>%
        summarise(Nmun=n(),
                  MaisMoto = sum(maismoto),
                  percMun = 100 * sum(maismoto)/n())

kable(tabela,caption = "Cidades com mais motocicletas do que automóveis, por estado")
```



Table: Cidades com mais motocicletas do que automóveis, por estado

UF    Nmun   MaisMoto   percMun
---  -----  ---------  --------
AC      22         22    100.00
AL     102         74     72.55
AM      62         58     93.55
AP      16          9     56.25
BA     417        303     72.66
CE     184        174     94.57
DF       1          0      0.00
ES      78         29     37.18
GO     246         44     17.89
MA     217        214     98.62
MG     853        278     32.59
MS      79          8     10.13
MT     141        111     78.72
PA     144        139     96.53
PB     223        190     85.20
PE     185        145     78.38
PI     224        223     99.55
PR     399          0      0.00
RJ      92          4      4.35
RN     167        151     90.42
RO      52         49     94.23
RR      15         14     93.33
RS     497          2      0.40
SC     295          0      0.00
SE      75         55     73.33
SP     645          1      0.16
TO     139        131     94.24

7. Relação completa dos estados, destacando a frota de certos tipos de veículos e quanto isso representa em relação ao país.


```r
popBR <- sum(dados$POPULACAO)
frotaBR <- sum(dados$TOTAL)
autoBR <- sum(dados$AUTOMOVEL)
oniBR <- sum(dados$ONIBUS)
camBR <- sum(dados$CAMINHAO)
tabela <- group_by(dados,UF) %>%
        summarise(Populacao = sum(POPULACAO),
                  percPop = 100 * sum(POPULACAO)/popBR,
                  Automoveis = sum(AUTOMOVEL),
                  percAuto = 100 * sum(AUTOMOVEL)/autoBR,
                  Onibus = sum(ONIBUS),
                  percOni = 100 * sum(ONIBUS)/oniBR,
                  Caminhoes = sum(CAMINHAO),
                  percCam = 100 * sum(CAMINHAO)/camBR)
Brasil <- data.table(UF = 'TOTAL',
                     Populacao = popBR,
                     percPop = 100,
                     Automoveis = autoBR,
                     percAuto = 100,
                     Onibus = oniBR,
                     percOni = 100,
                     Caminhoes = camBR,
                     percCam = 100)
tabela<-rbind(tabela,Brasil)
kable(tabela,digits=1)
```



UF       Populacao   percPop   Automoveis   percAuto   Onibus   percOni   Caminhoes   percCam
------  ----------  --------  -----------  ---------  -------  --------  ----------  --------
AC         7.8e+05       0.4        67461        0.1      921       0.2        6297       0.3
AL         3.3e+06       1.6       275556        0.6     6098       1.1       18949       0.8
AM         3.8e+06       1.9       325698        0.7     8517       1.6       19023       0.8
AP         7.3e+05       0.4        64386        0.1      917       0.2        3678       0.1
BA         1.5e+07       7.5      1415342        3.1    34421       6.3      106213       4.3
CE         8.8e+06       4.4       881571        1.9    14014       2.6       61047       2.5
DF         2.8e+06       1.4      1099719        2.4    10755       2.0       21801       0.9
ES         3.8e+06       1.9       797528        1.8    13555       2.5       64749       2.6
GO         6.4e+06       3.2      1512266        3.3    19507       3.6      100123       4.0
MA         6.8e+06       3.4       335793        0.7     6868       1.3       33214       1.3
MG         2.1e+07      10.2      4926454       10.8    67366      12.3      299132      12.0
MS         2.6e+06       1.3       580821        1.3     8199       1.5       45487       1.8
MT         3.2e+06       1.6       543484        1.2     9620       1.8       59982       2.4
PA         8.0e+06       4.0       455649        1.0    14414       2.6       50422       2.0
PB         3.9e+06       1.9       407624        0.9     6520       1.2       25508       1.0
PE         9.2e+06       4.6      1088338        2.4    17903       3.3       83632       3.4
PI         3.2e+06       1.6       255114        0.6     5295       1.0       22737       0.9
PR         1.1e+07       5.5      3759306        8.3    36912       6.7      246111       9.9
RJ         1.6e+07       8.1      3839651        8.4    44316       8.1      132959       5.3
RN         3.4e+06       1.7       430289        0.9     5716       1.0       27473       1.1
RO         1.7e+06       0.9       212652        0.5     4876       0.9       26735       1.1
RR         4.9e+05       0.2        53704        0.1      825       0.2        3881       0.2
RS         1.1e+07       5.6      3622309        8.0    36843       6.7      206979       8.3
SC         6.6e+06       3.3      2428891        5.3    17606       3.2      139545       5.6
SE         2.2e+06       1.1       262664        0.6     5785       1.1       19056       0.8
SP         4.4e+07      21.7     15643414       34.4   145166      26.5      643241      25.8
TO         1.5e+06       0.7       158702        0.3     4530       0.8       20706       0.8
TOTAL      2.0e+08     100.0     45444386      100.0   547465     100.0     2488680     100.0
