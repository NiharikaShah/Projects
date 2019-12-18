library("data.table")
library("dplyr")
library("caret")
library("caTools")
library("ranger")


rm(list=ls())

flights_nn <- fread(file.choose())
dim(flights_nn)
colnames(flights_nn)
#making variables into factors
flights_nn[ ,c(2:5,7:9,24,25,26)]<- lapply(flights_nn[ , c(2:5,7:9,24,25,26)], as.factor)

View(flights_nn)
#Adding new column for converting scheduled departure time into hours
flights_nn$SCHEDULED_DEPARTURE_TIME_HOURS <- cut(flights_nn$SCHEDULED_DEPARTURE,
                                                 breaks= c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,2300,2400),
                                                 labels=c("0-1","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-11","11-12","12-13","13-14","14-15","15-16","16-17","17-18","18-19","19-20","20-21","21-22","22-23","23-24"))
class(flights_nn$SCHEDULED_DEPARTURE_TIME_HOURS)

View(flights_nn)

#removing columns
#year, flight number, tail number, diverted, cancelled, cancellation reason, air system delay, security delay,airline delay, late aircraft delay, weather delay
flights_EDA_nn <- flights_nn[ ,-c(1,6,7,10,11,19:31)]
dim(flights_EDA_nn)

flights_EDA_nn <- na.omit(flights_EDA_nn)
dim(flights_EDA_nn)


View(flights_EDA_nn)


#Creating models based on Months
#Model January

data_jan_sample_nn <- flights_EDA_nn %>%
  filter(MONTH==1)

set.seed(43)

View(data_jan_sample_nn)
set.seed(43)

#trying with basic traning and testing without cross validation
split_values_jan_nn <- sample.split(data_jan_sample_nn$DEPARTURE_DELAY, SplitRatio = 0.70)
train_data_jan_nn <- subset(data_jan_sample_nn, split_values_jan_nn==TRUE)
test_data_jan_nn <- subset(data_jan_sample_nn, split_values_jan_nn==FALSE)
#ranger removing elapsed time and air time

ranger_jan1 <- ranger(DEPARTURE_DELAY ~ DAY + DAY_OF_WEEK  + AIRLINE + ORIGIN_AIRPORT + DESTINATION_AIRPORT + TAXI_OUT + WHEELS_OFF + SCHEDULED_TIME +  DISTANCE + SCHEDULED_DEPARTURE_TIME_HOURS  ,
                      data=train_data_jan_nn,
                      importance = "impurity"
                      
)

save(ranger_jan1, file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_jan1.rda")
load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_jan1.rda")

ranger_jan1$r.squared #r square 0.80 mtry=3

ranger_jan1$variable.importance

predict_ranger_jan1 <- predict(ranger_jan1 , test_data_jan_nn)

jan_ranger_result1 <- data.frame(predicted = predict_ranger_jan1$predictions ,actual= test_data_jan_nn$DEPARTURE_DELAY )


jan_ranger_result1$error = jan_ranger_result1$actual - jan_ranger_result1$predicted

View(jan_ranger_result1)
View(test_data_jan_nn)

#testing purpose
data_input_jan <- data.frame(DAY="3", DAY_OF_WEEK="6" , AIRLINE="EV" , ORIGIN_AIRPORT="MKE" , DESTINATION_AIRPORT="IAH", TAXI_OUT=14, WHEELS_OFF=1932, SCHEDULED_TIME=185, DISTANCE=984, SCHEDULED_DEPARTURE_TIME_HOURS="16-17")
data_input_jan1 <- data.frame(DAY="1", DAY_OF_WEEK="4" , AIRLINE="AS" , ORIGIN_AIRPORT="ANC" , DESTINATION_AIRPORT="SEA", TAXI_OUT=21, WHEELS_OFF=15, SCHEDULED_TIME=205, DISTANCE=1448, SCHEDULED_DEPARTURE_TIME_HOURS="0-1")
data_input_jan2 <- data.frame(DAY="27", DAY_OF_WEEK="2" , AIRLINE="OO" , ORIGIN_AIRPORT="SBN" , DESTINATION_AIRPORT="MSP", TAXI_OUT=50, WHEELS_OFF=737, SCHEDULED_TIME=109, DISTANCE=411, SCHEDULED_DEPARTURE_TIME_HOURS="7-8")
data_input_jan3 <- data.frame(DAY="18", DAY_OF_WEEK="7" , AIRLINE="AS" , ORIGIN_AIRPORT="YAK" , DESTINATION_AIRPORT="JNU", TAXI_OUT=9, WHEELS_OFF=1752, SCHEDULED_TIME=55, DISTANCE=199, SCHEDULED_DEPARTURE_TIME_HOURS="18-19")
data_input_jan4 <- data.frame(DAY="6", DAY_OF_WEEK="2" , AIRLINE="OO" , ORIGIN_AIRPORT="RKS" , DESTINATION_AIRPORT="GCC", TAXI_OUT=4, WHEELS_OFF=2053, SCHEDULED_TIME=69, DISTANCE=261, SCHEDULED_DEPARTURE_TIME_HOURS="21-22")
data_input_jan5 <- data.frame(DAY="16", DAY_OF_WEEK="5" , AIRLINE="EV" , ORIGIN_AIRPORT="MEI" , DESTINATION_AIRPORT="PIB", TAXI_OUT=7, WHEELS_OFF=2042, SCHEDULED_TIME=54, DISTANCE=69, SCHEDULED_DEPARTURE_TIME_HOURS="21-22")
data_input_jan6 <- data.frame(DAY="10", DAY_OF_WEEK="6" , AIRLINE="AS" , ORIGIN_AIRPORT="KTN" , DESTINATION_AIRPORT="SEA", TAXI_OUT=10, WHEELS_OFF=1323, SCHEDULED_TIME=105, DISTANCE=680, SCHEDULED_DEPARTURE_TIME_HOURS="13-14")
data_input_jan7 <- data.frame(DAY="1", DAY_OF_WEEK="4" , AIRLINE="AA" , ORIGIN_AIRPORT="LAX" , DESTINATION_AIRPORT="MIA", TAXI_OUT=15, WHEELS_OFF=30, SCHEDULED_TIME=285, DISTANCE=2342, SCHEDULED_DEPARTURE_TIME_HOURS="0-1")
data_input_jan8 <- data.frame(DAY="26", DAY_OF_WEEK="1" , AIRLINE="B6" , ORIGIN_AIRPORT="HPN" , DESTINATION_AIRPORT="MCO", TAXI_OUT=23, WHEELS_OFF=1032, SCHEDULED_TIME=172, DISTANCE=972, SCHEDULED_DEPARTURE_TIME_HOURS="10-11")


predict_eg_jan <- predict(ranger_jan1, data_input_jan1)$predictions

rmse_jan <- sqrt(mean(jan_ranger_result1$error)^2)
rmse_jan #0.128



#Model February

data_feb_sample_nn <- flights_EDA_nn %>%
  filter(MONTH==2)

set.seed(43)

View(data_feb_sample_nn)
set.seed(43)

#trying with basic traning and testing without cross validation
split_values_feb_nn <- sample.split(data_feb_sample_nn$DEPARTURE_DELAY, SplitRatio = 0.70)
train_data_feb_nn <- subset(data_feb_sample_nn, split_values_feb_nn==TRUE)
test_data_feb_nn <- subset(data_feb_sample_nn, split_values_feb_nn==FALSE)

#ranger removing elapsed time and air time

ranger_feb1 <- ranger(DEPARTURE_DELAY ~ DAY + DAY_OF_WEEK  + AIRLINE + ORIGIN_AIRPORT + DESTINATION_AIRPORT + TAXI_OUT + WHEELS_OFF + SCHEDULED_TIME +  DISTANCE + SCHEDULED_DEPARTURE_TIME_HOURS  ,
                      data=train_data_feb_nn,
                      importance = "impurity"
)

save(ranger_feb1, file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_feb1.rda")

ranger_feb1$r.squared #r square 0.8081086 mtry=3

ranger_feb1$variable.importance

predict_ranger_feb1 <- predict(ranger_feb1 , test_data_feb_nn)

feb_ranger_result1 <- data.frame(predicted = predict_ranger_feb1$predictions ,actual= test_data_feb_nn$DEPARTURE_DELAY )


feb_ranger_result1$error = feb_ranger_result1$actual - feb_ranger_result1$predicted

View(feb_ranger_result1)

rmse_feb <- sqrt(mean(feb_ranger_result1$error)^2)
rmse_feb #0.189



#Model March

data_march_sample_nn <- flights_EDA_nn %>%
  filter(MONTH==3)

set.seed(43)

View(data_march_sample_nn)
set.seed(43)

#trying with basic traning and testing without cross validation
split_values_march_nn <- sample.split(data_march_sample_nn$DEPARTURE_DELAY, SplitRatio = 0.70)
train_data_march_nn <- subset(data_march_sample_nn, split_values_march_nn==TRUE)
test_data_march_nn <- subset(data_march_sample_nn, split_values_march_nn==FALSE)


ranger_march1 <- ranger(DEPARTURE_DELAY ~ DAY + DAY_OF_WEEK  + AIRLINE + ORIGIN_AIRPORT + DESTINATION_AIRPORT + TAXI_OUT + WHEELS_OFF + SCHEDULED_TIME +  DISTANCE + SCHEDULED_DEPARTURE_TIME_HOURS  ,
                      data=train_data_march_nn,
                      importance = "impurity"
)


save(ranger_march1, file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_march1.rda")
load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_march1.rda")

data_input_march <- data.frame(DAY= factor("25", levels=c(1:31)),
                               DAY_OF_WEEK= factor("3", levels=c(1:7)),
                              AIRLINE = factor("AS" ,levels=c("AA","AS", "B6", "DL", "EV", "F9", "HA", "MQ", "NK", "OO", "UA", "US" ,"VX", "WN")) ,
                              ORIGIN_AIRPORT =factor("YAK", levels =c("10135","10136","10140","10141","10146","10154","10155","10157","10158","10165","10170","10185","10208","10257","10268",
                                                                          "10279","10299","10333","10372","10397","10408","10423","10431","10434","10469","10529","10551","10561","10577","10581",
                                                                          "10599","10620","10627","10631","10685","10693","10713","10721","10728","10731","10732","10739","10747","10754","10779",
                                                                          "10781","10785","10792","10800","10821","10849","10868","10874","10918","10926","10980","10990","10994","11003","11013",
                                                                          "11042","11049","11057","11066","11067","11076","11097","11109","11111","11122","11140","11146","11150","11193","11203",
                                                                          "11252","11259","11267","11274","11278","11292","11298","11308","11315","11337","11413","11423","11433","11447","11471",
                                                                          "11481","11503","11525","11537","11540","11577","11587","11603","11612","11617","11618","11624","11630","11637","11638",
                                                                          "11641","11648","11695","11697","11721","11775","11778","11823","11865","11867","11884","11898","11905","11921","11953",
                                                                          "11973","11977","11980","11982","11986","11995","11996","12003","12007","12016","12094","12129","12156","12173","12177",
                                                                          "12191","12197","12206","12217","12255","12264","12265","12266","12278","12280","12323","12335","12339","12343","12389",
                                                                          "12391","12402","12441","12448","12451","12478","12511","12519","12523","12758","12819","12884","12888","12889","12891",
                                                                          "12892","12896","12898","12915","12945","12951","12953","12954","12982","12992","13029","13061","13076","13127","13158",
                                                                          "13184","13198","13204","13230","13232","13241","13244","13256","13264","13277","13290","13296","13303","13342","13344",
                                                                          "13360","13367","13377","13422","13433","13459","13476","13485","13486","13487","13495","13502","13541","13577","13795",
                                                                          "13796","13830","13851","13871","13873","13891","13930","13931","13933","13964","13970","14006","14025","14027","14057",
                                                                          "14098","14100","14107","14108","14109","14113","14122","14150","14193","14222","14252","14254","14256","14262","14307",
                                                                          "14321","14457","14487","14489","14492","14520","14524","14543","14570","14574","14576","14588","14633","14635","14674",
                                                                          "14679","14683","14685","14689","14696","14698","14709","14711","14730","14747","14771","14783","14794","14814","14828",
                                                                          "14831","14842","14843","14869","14893","14905","14908","14952","14960","14986","15016","15024","15027","15041","15048",
                                                                          "15070","15096","15249","15295","15304","15323","15356","15370","15376","15380","15389","15401","15411","15412","15497",
                                                                          "15607","15624","15841","15919","15991","16218","ABE","ABI","ABQ","ABR","ABY","ACK","ACT","ACV","ACY",
                                                                          "ADK","ADQ","AEX","AGS","AKN","ALB","ALO","AMA","ANC","APN","ASE","ATL","ATW","AUS","AVL",
                                                                          "AVP","AZO","BDL","BET","BFL","BGM","BGR","BHM","BIL","BIS","BJI","BLI","BMI","BNA","BOI",
                                                                          "BOS","BPT","BQK","BQN","BRD","BRO","BRW","BTM","BTR","BTV","BUF","BUR","BWI","BZN","CAE",
                                                                          "CAK","CDC","CDV","CEC","CHA","CHO","CHS","CID","CIU","CLD","CLE","CLL","CLT","CMH","CMI",
                                                                          "CMX","CNY","COD","COS","COU","CPR","CRP","CRW","CSG","CVG","CWA","DAB","DAL","DAY","DBQ",
                                                                          "DCA","DEN","DFW","DHN","DIK","DLG","DLH","DRO","DSM","DTW","DVL","EAU","ECP","EGE","EKO",
                                                                          "ELM","ELP","ERI","ESC","EUG","EVV","EWN","EWR","EYW","FAI","FAR","FAT","FAY","FCA","FLG",
                                                                          "FLL","FNT","FSD","FSM","FWA","GCC","GCK","GEG","GFK","GGG","GJT","GNV","GPT","GRB","GRI",
                                                                          "GRK","GRR","GSO","GSP","GST","GTF","GTR","GUC","GUM","HDN","HIB","HLN","HNL","HOB","HOU",
                                                                          "HPN","HRL","HSV","HYA","HYS","IAD","IAG","IAH","ICT","IDA","ILG","ILM","IMT","IND","INL",
                                                                          "ISN","ISP","ITH","ITO","JAC","JAN","JAX","JFK","JLN","JMS","JNU","KOA","KTN","LAN","LAR",
                                                                          "LAS","LAW","LAX","LBB","LBE","LCH","LEX","LFT","LGA","LGB","LIH","LIT","LNK","LRD","LSE",
                                                                          "LWS","MAF","MBS","MCI","MCO","MDT","MDW","MEI","MEM","MFE","MFR","MGM","MHK","MHT","MIA",
                                                                          "MKE","MKG","MLB","MLI","MLU","MMH","MOB","MOT","MQT","MRY","MSN","MSO","MSP","MSY","MTJ",
                                                                          "MVY","MYR","OAJ","OAK","OGG","OKC","OMA","OME","ONT","ORD","ORF","ORH","OTH","OTZ","PAH",
                                                                          "PBG","PBI","PDX","PHF","PHL","PHX","PIA","PIB","PIH","PIT","PLN","PNS","PPG","PSC","PSE",
                                                                          "PSG","PSP","PUB","PVD","PWM","RAP","RDD","RDM","RDU","RHI","RIC","RKS","RNO","ROA","ROC",
                                                                          "ROW","RST","RSW","SAF","SAN","SAT","SAV","SBA","SBN","SBP","SCC","SCE","SDF","SEA","SFO",
                                                                          "SGF","SGU","SHV","SIT","SJC","SJT","SJU","SLC","SMF","SMX","SNA","SPI","SPS","SRQ","STC",
                                                                          "STL","STT","STX","SUN","SUX","SWF","SYR","TLH","TOL","TPA","TRI","TTN","TUL","TUS","TVC",
                                                                          "TWF","TXK","TYR","TYS","UST","VEL","VLD","VPS","WRG","WYS","XNA","YAK","YUM"
                              )),
                              DESTINATION_AIRPORT = factor("JNU" ,levels =c("10135","10136","10140","10141","10146","10154","10155","10157","10158","10165","10170","10185","10208","10257","10268",
                                                                            "10279","10299","10333","10372","10397","10408","10423","10431","10434","10469","10529","10551","10561","10577","10581",
                                                                            "10599","10620","10627","10631","10666","10685","10693","10713","10721","10728","10731","10732","10739","10747","10754",
                                                                            "10779","10781","10785","10792","10800","10821","10849","10868","10874","10918","10926","10980","10990","10994","11003"
                                                                            ,"11013","11042","11049","11057","11066","11067","11076","11097","11109","11111","11122","11140","11146","11150","11193"
                                                                            ,"11203","11252","11259","11267","11274","11278","11292","11298","11308","11315","11337","11413","11423","11433","11447"
                                                                            ,"11471","11481","11503","11525","11537","11540","11577","11587","11603","11612","11617","11618","11624","11630","11637"
                                                                            ,"11638","11641","11648","11695","11697","11721","11775","11778","11823","11865","11867","11884","11898","11905","11921"
                                                                            ,"11953","11973","11977","11980","11982","11986","11995","11996","12003","12007","12016","12094","12129","12156","12173"
                                                                            ,"12177","12191","12197","12206","12217","12255","12264","12265","12266","12278","12280","12323","12335","12339","12343"
                                                                            ,"12389","12391","12402","12441","12448","12451","12478","12511","12519","12523","12758","12819","12884","12888","12889"
                                                                            ,"12891","12892","12896","12898","12915","12945","12951","12953","12954","12982","12992","13029","13061","13076","13127"
                                                                            ,"13158","13184","13198","13204","13230","13232","13241","13244","13256","13264","13277","13290","13296","13303","13342"
                                                                            ,"13344","13360","13367","13377","13422","13433","13459","13476","13485","13486","13487","13495","13502","13541","13577"
                                                                            ,"13795","13796","13830","13851","13871","13873","13891","13930","13931","13933","13964","13970","14006","14025","14027"
                                                                            ,"14057","14098","14100","14107","14108","14109","14113","14122","14150","14193","14222","14252","14254","14256","14262"
                                                                            ,"14307","14321","14457","14487","14489","14492","14520","14524","14543","14570","14574","14576","14588","14633","14635"
                                                                            ,"14674","14679","14683","14685","14689","14696","14698","14709","14711","14730","14747","14771","14783","14794","14814"
                                                                            ,"14828","14831","14842","14843","14869","14893","14905","14908","14952","14960","14986","15016","15024","15027","15041"
                                                                            ,"15048","15070","15096","15249","15295","15304","15323","15356","15370","15376","15380","15389","15401","15411","15412"
                                                                            ,"15497","15607","15624","15841","15919","15991","16218","ABE","ABI","ABQ","ABR","ABY","ACK","ACT","ACV"
                                                                            ,"ACY","ADK","ADQ","AEX","AGS","AKN","ALB","ALO","AMA","ANC","APN","ASE","ATL","ATW","AUS"
                                                                            ,"AVL","AVP","AZO","BDL","BET","BFL","BGM","BGR","BHM","BIL","BIS","BJI","BLI","BMI","BNA"
                                                                            ,"BOI","BOS","BPT","BQK","BQN","BRD","BRO","BRW","BTM","BTR","BTV","BUF","BUR","BWI","BZN"
                                                                            ,"CAE","CAK","CDC","CDV","CEC","CHA","CHO","CHS","CID","CIU","CLD","CLE","CLL","CLT","CMH"
                                                                            ,"CMI","CMX","CNY","COD","COS","COU","CPR","CRP","CRW","CSG","CVG","CWA","DAB","DAL","DAY"
                                                                            ,"DBQ","DCA","DEN","DFW","DHN","DIK","DLG","DLH","DRO","DSM","DTW","DVL","EAU","ECP","EGE"
                                                                            ,"EKO","ELM","ELP","ERI","ESC","EUG","EVV","EWN","EWR","EYW","FAI","FAR","FAT","FAY","FCA"
                                                                            ,"FLG","FLL","FNT","FSD","FSM","FWA","GCC","GCK","GEG","GFK","GGG","GJT","GNV","GPT","GRB"
                                                                            ,"GRI","GRK","GRR","GSO","GSP","GST","GTF","GTR","GUC","GUM","HDN","HIB","HLN","HNL","HOB"
                                                                            ,"HOU","HPN","HRL","HSV","HYA","HYS","IAD","IAG","IAH","ICT","IDA","ILG","ILM","IMT","IND"
                                                                            ,"INL","ISN","ISP","ITH","ITO","JAC","JAN","JAX","JFK","JLN","JMS","JNU","KOA","KTN","LAN"
                                                                            ,"LAR","LAS","LAW","LAX","LBB","LBE","LCH","LEX","LFT","LGA","LGB","LIH","LIT","LNK","LRD"
                                                                            ,"LSE","LWS","MAF","MBS","MCI","MCO","MDT","MDW","MEI","MEM","MFE","MFR","MGM","MHK","MHT"
                                                                            ,"MIA","MKE","MKG","MLB","MLI","MLU","MMH","MOB","MOT","MQT","MRY","MSN","MSO","MSP","MSY"
                                                                            ,"MTJ","MVY","MYR","OAJ","OAK","OGG","OKC","OMA","OME","ONT","ORD","ORF","ORH","OTH","OTZ"
                                                                            ,"PAH","PBG","PBI","PDX","PHF","PHL","PHX","PIA","PIB","PIH","PIT","PLN","PNS","PPG","PSC"
                                                                            ,"PSE","PSG","PSP","PUB","PVD","PWM","RAP","RDD","RDM","RDU","RHI","RIC","RKS","RNO","ROA"
                                                                            ,"ROC","ROW","RST","RSW","SAF","SAN","SAT","SAV","SBA","SBN","SBP","SCC","SCE","SDF","SEA"
                                                                            ,"SFO","SGF","SGU","SHV","SIT","SJC","SJT","SJU","SLC","SMF","SMX","SNA","SPI","SPS","SRQ"
                                                                            ,"STC","STL","STT","STX","SUN","SUX","SWF","SYR","TLH","TOL","TPA","TRI","TTN","TUL","TUS"
                                                                            ,"TVC","TWF","TXK","TYR","TYS","UST","VEL","VLD","VPS","WRG","WYS","XNA","YAK","YUM"
                              )),
                              TAXI_OUT= as.integer(20),
                              WHEELS_OFF= as.integer(1810),
                              SCHEDULED_TIME=as.integer(42),
                              DISTANCE= as.integer(199),
                              SCHEDULED_DEPARTURE_TIME_HOURS= factor("18-19", levels = c("0-1","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-11","11-12","12-13","13-14","14-15","15-16","16-17","17-18","18-19","19-20","20-21","21-22","22-23","23-24")))
str(data_input_march)

n<- predict(ranger_march1, data_input_march )
n$predictions
ranger_march1$r.squared #r square 0.8221276 mtry=3

ranger_march1$variable.importance

predict_ranger_march1 <- predict(ranger_march1 , test_data_march_nn)

march_ranger_result1 <- data.frame(predicted = predict_ranger_march1$predictions ,actual= test_data_march_nn$DEPARTURE_DELAY )


march_ranger_result1$error = march_ranger_result1$actual - march_ranger_result1$predicted

View(march_ranger_result1)
View(test_data_march_nn)

rmse_march <- sqrt(mean(march_ranger_result1$error)^2)
rmse_march #0.0634

#Model April

data_april_sample_nn <- flights_EDA_nn %>%
  filter(MONTH==4)

set.seed(43)

View(data_april_sample_nn)
set.seed(43)

#trying with basic traning and testing without cross validation
split_values_april_nn <- sample.split(data_april_sample_nn$DEPARTURE_DELAY, SplitRatio = 0.70)
train_data_april_nn <- subset(data_april_sample_nn, split_values_april_nn==TRUE)
test_data_april_nn <- subset(data_april_sample_nn, split_values_april_nn==FALSE)


ranger_april1 <- ranger(DEPARTURE_DELAY ~ DAY + DAY_OF_WEEK  + AIRLINE + ORIGIN_AIRPORT + DESTINATION_AIRPORT + TAXI_OUT + WHEELS_OFF + SCHEDULED_TIME +  DISTANCE + SCHEDULED_DEPARTURE_TIME_HOURS  ,
                        data=train_data_april_nn,
                        importance = "impurity"
)

save(ranger_april1, file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_april1.rda")


ranger_april1$r.squared #r square 0.8388845 mtry=3

ranger_april1$variable.importance

predict_ranger_april1 <- predict(ranger_april1 , test_data_april_nn)

april_ranger_result1 <- data.frame(predicted = predict_ranger_april1$predictions ,actual= test_data_april_nn$DEPARTURE_DELAY )


april_ranger_result1$error = april_ranger_result1$actual - april_ranger_result1$predicted

View(april_ranger_result1)

rmse_april <- sqrt(mean(april_ranger_result1$error)^2)
rmse_april #0.05594576


#Model May

data_may_sample_nn <- flights_EDA_nn %>%
  filter(MONTH==5)

set.seed(43)

View(data_may_sample_nn)
set.seed(43)

#trying with basic traning and testing without cross validation
split_values_may_nn <- sample.split(data_may_sample_nn$DEPARTURE_DELAY, SplitRatio = 0.70)
train_data_may_nn <- subset(data_may_sample_nn, split_values_may_nn==TRUE)
test_data_may_nn <- subset(data_may_sample_nn, split_values_may_nn==FALSE)


ranger_may1 <- ranger(DEPARTURE_DELAY ~ DAY + DAY_OF_WEEK  + AIRLINE + ORIGIN_AIRPORT + DESTINATION_AIRPORT + TAXI_OUT + WHEELS_OFF + SCHEDULED_TIME +  DISTANCE + SCHEDULED_DEPARTURE_TIME_HOURS  ,
                        data=train_data_may_nn,
                        importance = "impurity"
)

save(ranger_may1, file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_may1.rda")

ranger_may1$r.squared #r square 0.8579824 mtry=3

ranger_may1$variable.importance

predict_ranger_may1 <- predict(ranger_may1 , test_data_may_nn)

may_ranger_result1 <- data.frame(predicted = predict_ranger_may1$predictions ,actual= test_data_may_nn$DEPARTURE_DELAY )


may_ranger_result1$error = may_ranger_result1$actual - may_ranger_result1$predicted

View(may_ranger_result1)

rmse_may <- sqrt(mean(may_ranger_result1$error)^2)
rmse_may #0.03342867



#Model June

data_june_sample_nn <- flights_EDA_nn %>%
  filter(MONTH==6)

set.seed(43)

View(data_june_sample_nn)
set.seed(43)

#trying with basic traning and testing without cross validation
split_values_june_nn <- sample.split(data_june_sample_nn$DEPARTURE_DELAY, SplitRatio = 0.70)
train_data_june_nn <- subset(data_june_sample_nn, split_values_june_nn==TRUE)
test_data_june_nn <- subset(data_june_sample_nn, split_values_june_nn==FALSE)


ranger_june1 <- ranger(DEPARTURE_DELAY ~ DAY + DAY_OF_WEEK  + AIRLINE + ORIGIN_AIRPORT + DESTINATION_AIRPORT + TAXI_OUT + WHEELS_OFF + SCHEDULED_TIME +  DISTANCE + SCHEDULED_DEPARTURE_TIME_HOURS  ,
                      data=train_data_june_nn,
                      importance = "impurity"
)

save(ranger_june1, file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_june1.rda")

ranger_june1$r.squared #r square 0.879248 mtry=3

ranger_june1$variable.importance

predict_ranger_june1 <- predict(ranger_june1 , test_data_june_nn)

june_ranger_result1 <- data.frame(predicted = predict_ranger_june1$predictions ,actual= test_data_june_nn$DEPARTURE_DELAY )


june_ranger_result1$error = june_ranger_result1$actual - june_ranger_result1$predicted

View(june_ranger_result1)

rmse_june <- sqrt(mean(june_ranger_result1$error)^2)
rmse_june #0.06531997



#Model July

data_july_sample_nn <- flights_EDA_nn %>%
  filter(MONTH==7)

set.seed(43)

View(data_july_sample_nn)
set.seed(43)

#trying with basic traning and testing without cross validation
split_values_july_nn <- sample.split(data_july_sample_nn$DEPARTURE_DELAY, SplitRatio = 0.70)
train_data_july_nn <- subset(data_july_sample_nn, split_values_july_nn==TRUE)
test_data_july_nn <- subset(data_july_sample_nn, split_values_july_nn==FALSE)


ranger_july1 <- ranger(DEPARTURE_DELAY ~ DAY + DAY_OF_WEEK  + AIRLINE + ORIGIN_AIRPORT + DESTINATION_AIRPORT + TAXI_OUT + WHEELS_OFF + SCHEDULED_TIME +  DISTANCE + SCHEDULED_DEPARTURE_TIME_HOURS  ,
                       data=train_data_july_nn,
                       importance = "impurity"
)

save(ranger_july1, file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_july1.rda")

ranger_july1$r.squared #r square 0.8712081 mtry=3

ranger_july1$variable.importance

predict_ranger_july1 <- predict(ranger_july1 , test_data_july_nn)

july_ranger_result1 <- data.frame(predicted = predict_ranger_july1$predictions ,actual= test_data_july_nn$DEPARTURE_DELAY )


july_ranger_result1$error = july_ranger_result1$actual - july_ranger_result1$predicted

View(july_ranger_result1)

rmse_july <- sqrt(mean(july_ranger_result1$error)^2)
rmse_july #0.03826771




#Model August

data_aug_sample_nn <- flights_EDA_nn %>%
  filter(MONTH==8)

set.seed(43)

View(data_aug_sample_nn)
set.seed(43)

#trying with basic traning and testing without cross validation
split_values_aug_nn <- sample.split(data_aug_sample_nn$DEPARTURE_DELAY, SplitRatio = 0.70)
train_data_aug_nn <- subset(data_aug_sample_nn, split_values_aug_nn==TRUE)
test_data_aug_nn <- subset(data_aug_sample_nn, split_values_aug_nn==FALSE)


ranger_aug1 <- ranger(DEPARTURE_DELAY ~ DAY + DAY_OF_WEEK  + AIRLINE + ORIGIN_AIRPORT + DESTINATION_AIRPORT + TAXI_OUT + WHEELS_OFF + SCHEDULED_TIME +  DISTANCE + SCHEDULED_DEPARTURE_TIME_HOURS  ,
                       data=train_data_aug_nn,
                       importance = "impurity"
)

save(ranger_aug1, file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_aug1.rda")

ranger_aug1$r.squared #r square 0.860938 mtry=3

ranger_aug1$variable.importance

predict_ranger_aug1 <- predict(ranger_aug1 , test_data_aug_nn)

aug_ranger_result1 <- data.frame(predicted = predict_ranger_aug1$predictions ,actual= test_data_aug_nn$DEPARTURE_DELAY )


aug_ranger_result1$error = aug_ranger_result1$actual - aug_ranger_result1$predicted

View(aug_ranger_result1)

rmse_aug <- sqrt(mean(aug_ranger_result1$error)^2)
rmse_aug #0.06997541



#Model September

data_sep_sample_nn <- flights_EDA_nn %>%
  filter(MONTH==9)

set.seed(43)

View(data_sep_sample_nn)
set.seed(43)

#trying with basic traning and testing without cross validation
split_values_sep_nn <- sample.split(data_sep_sample_nn$DEPARTURE_DELAY, SplitRatio = 0.70)
train_data_sep_nn <- subset(data_sep_sample_nn, split_values_sep_nn==TRUE)
test_data_sep_nn <- subset(data_sep_sample_nn, split_values_sep_nn==FALSE)


ranger_sep1 <- ranger(DEPARTURE_DELAY ~ DAY + DAY_OF_WEEK  + AIRLINE + ORIGIN_AIRPORT + DESTINATION_AIRPORT + TAXI_OUT + WHEELS_OFF + SCHEDULED_TIME +  DISTANCE + SCHEDULED_DEPARTURE_TIME_HOURS  ,
                      data=train_data_sep_nn,
                      importance = "impurity"
)

save(ranger_sep1, file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_sep1.rda")

ranger_sep1$r.squared #r square 0.809809 mtry=3

ranger_sep1$variable.importance

predict_ranger_sep1 <- predict(ranger_sep1 , test_data_sep_nn)

sep_ranger_result1 <- data.frame(predicted = predict_ranger_sep1$predictions ,actual= test_data_sep_nn$DEPARTURE_DELAY )


sep_ranger_result1$error = sep_ranger_result1$actual - sep_ranger_result1$predicted

View(sep_ranger_result1)

rmse_sep <- sqrt(mean(sep_ranger_result1$error)^2)
rmse_sep #0.02515976




#Model October

data_oct_sample_nn <- flights_EDA_nn %>%
  filter(MONTH==10)

set.seed(43)

View(data_oct_sample_nn)
set.seed(43)

#trying with basic traning and testing without cross validation
split_values_oct_nn <- sample.split(data_oct_sample_nn$DEPARTURE_DELAY, SplitRatio = 0.70)
train_data_oct_nn <- subset(data_oct_sample_nn, split_values_oct_nn==TRUE)
test_data_oct_nn <- subset(data_oct_sample_nn, split_values_oct_nn==FALSE)


ranger_oct1 <- ranger(DEPARTURE_DELAY ~ DAY + DAY_OF_WEEK  + AIRLINE + ORIGIN_AIRPORT + DESTINATION_AIRPORT + TAXI_OUT + WHEELS_OFF + SCHEDULED_TIME +  DISTANCE + SCHEDULED_DEPARTURE_TIME_HOURS  ,
                      data=train_data_oct_nn,
                      importance = "impurity"
)

save(ranger_oct1, file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_oct1.rda")

ranger_oct1$r.squared #r square 0.780 mtry=3

ranger_oct1$variable.importance

predict_ranger_oct1 <- predict(ranger_oct1 , test_data_oct_nn)

oct_ranger_result1 <- data.frame(predicted = predict_ranger_oct1$predictions ,actual= test_data_oct_nn$DEPARTURE_DELAY )


oct_ranger_result1$error = oct_ranger_result1$actual - oct_ranger_result1$predicted

View(oct_ranger_result1)

rmse_oct <- sqrt(mean(oct_ranger_result1$error)^2)
rmse_oct #0.02912305




#Model November

data_nov_sample_nn <- flights_EDA_nn %>%
  filter(MONTH==11)

set.seed(43)

View(data_nov_sample_nn)
set.seed(43)

#trying with basic traning and testing without cross validation
split_values_nov_nn <- sample.split(data_nov_sample_nn$DEPARTURE_DELAY, SplitRatio = 0.70)
train_data_nov_nn <- subset(data_nov_sample_nn, split_values_nov_nn==TRUE)
test_data_nov_nn <- subset(data_nov_sample_nn, split_values_nov_nn==FALSE)


ranger_nov1 <- ranger(DEPARTURE_DELAY ~ DAY + DAY_OF_WEEK  + AIRLINE + ORIGIN_AIRPORT + DESTINATION_AIRPORT + TAXI_OUT + WHEELS_OFF + SCHEDULED_TIME +  DISTANCE + SCHEDULED_DEPARTURE_TIME_HOURS  ,
                      data=train_data_nov_nn,
                      importance = "impurity"
)

save(ranger_nov1, file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_nov1.rda")


ranger_nov1$r.squared #r square 0.8294339 mtry=3

ranger_nov1$variable.importance

predict_ranger_nov1 <- predict(ranger_nov1 , test_data_nov_nn)

nov_ranger_result1 <- data.frame(predicted = predict_ranger_nov1$predictions ,actual= test_data_nov_nn$DEPARTURE_DELAY )


nov_ranger_result1$error = nov_ranger_result1$actual - nov_ranger_result1$predicted

View(nov_ranger_result1)

rmse_nov <- sqrt(mean(nov_ranger_result1$error)^2)
rmse_nov #0.04595016




#Model December

data_dec_sample_nn <- flights_EDA_nn %>%
  filter(MONTH==12)

set.seed(43)

View(data_dec_sample_nn)
set.seed(43)

#trying with basic traning and testing without cross validation
split_values_dec_nn <- sample.split(data_dec_sample_nn$DEPARTURE_DELAY, SplitRatio = 0.70)
train_data_dec_nn <- subset(data_dec_sample_nn, split_values_dec_nn==TRUE)
test_data_dec_nn <- subset(data_dec_sample_nn, split_values_dec_nn==FALSE)


ranger_dec1 <- ranger(DEPARTURE_DELAY ~ DAY + DAY_OF_WEEK  + AIRLINE + ORIGIN_AIRPORT + DESTINATION_AIRPORT + TAXI_OUT + WHEELS_OFF + SCHEDULED_TIME +  DISTANCE + SCHEDULED_DEPARTURE_TIME_HOURS  ,
                      data=train_data_dec_nn,
                      importance = "impurity"
)

save(ranger_dec1, file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_dec1.rda")


ranger_dec1$r.squared #r square 0.8159429 mtry=3

ranger_dec1$variable.importance

predict_ranger_dec1 <- predict(ranger_dec1 , test_data_dec_nn)

dec_ranger_result1 <- data.frame(predicted = predict_ranger_dec1$predictions ,actual= test_data_dec_nn$DEPARTURE_DELAY )


dec_ranger_result1$error = dec_ranger_result1$actual - dec_ranger_result1$predicted

View(dec_ranger_result1)

rmse_dec <- sqrt(mean(dec_ranger_result1$error)^2)
rmse_dec #0.06364875


#for new data
#to clear the workspace
rm(list=ls())

library("data.table")
library("ranger")

data_input <- data.frame(MONTH= "8" , DAY="10", DAY_OF_WEEK="3" , AIRLINE="NK" , ORIGIN_AIRPORT="SLC" , DESTINATION_AIRPORT="MIA", TAXI_OUT=19, WHEELS_OFF=1, SCHEDULED_TIME=285, DISTANCE=2088, SCHEDULED_DEPARTURE_TIME_HOURS="23-24")

  if(data_input$MONTH=="1")
    { load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_jan1.rda")
      Predicted_depart_delay<- predict(ranger_jan1 , data_input)$predictions
        if(Predicted_depart_delay > 0)
             print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay))
        else
           print("Flight would depart on time")
       # break
    } else if(data_input$MONTH=="2")
       { load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_feb1.rda")
          Predicted_depart_delay<- predict(ranger_feb1 , data_input)$predictions
          if(Predicted_depart_delay > 0)
             print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay))
          else
             print("Flight would depart on time")
        # break
    } else if(data_input$MONTH=="3")
      { load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_march1.rda")
        Predicted_depart_delay<- predict(ranger_march1 , data_input)$predictions
         if(Predicted_depart_delay > 0)
             print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay))
         else
             print("Flight would depart on time")
       # break
    }  else if(data_input$MONTH=="4")
     { load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_april1.rda")
        Predicted_depart_delay<- predict(ranger_april1 , data_input)$predictions
           if(Predicted_depart_delay > 0)
              print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay))
          else
              print("Flight would depart on time")
       # break
     }  else if(data_input$MONTH=="5")
    {  load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_may1.rda")
      Predicted_depart_delay<- predict(ranger_may1 , data_input)$predictions
      if(Predicted_depart_delay > 0)
          print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay))
        else
          print("Flight would depart on time")
        #break
    }     else if(data_input$MONTH=="6")
    {   load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_june1.rda")
        Predicted_depart_delay<- predict(ranger_june1 , data_input)$predictions
        if(Predicted_depart_delay > 0)
            print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay))
        else
          print("Flight would depart on time")
         # break
    } else if(data_input$MONTH=="7")
   { load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_july1.rda")
      Predicted_depart_delay<- predict(ranger_july1 , data_input)$predictions
        if(Predicted_depart_delay > 0)
           print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay))
     else
        print("Flight would depart on time")
   #break
  } else if(data_input$MONTH=="8")
    { load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_aug1.rda")
      Predicted_depart_delay<- predict(ranger_aug1 , data_input)$predictions
        if(Predicted_depart_delay > 0)
           print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay))
       else
          print("Flight would depart on time")
  #break
    } else if(data_input$MONTH=="9")
      {  load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_sep1.rda")
          Predicted_depart_delay<- predict(ranger_sep1 , data_input)$predictions
            if(Predicted_depart_delay > 0)
             print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay))
        else
            print("Flight would depart on time")
       # break
     } else if(data_input$MONTH=="10")
      { load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_oct1.rda")
         Predicted_depart_delay<- predict(ranger_oct1 , data_input)$predictions
         if(Predicted_depart_delay > 0)
          print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay))
         else
           print("Flight would depart on time")
          # break
     } else if(data_input$MONTH=="11")
    {   load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_nov1.rda")
        Predicted_depart_delay<- predict(ranger_nov1 , data_input)$predictions
          if(Predicted_depart_delay > 0)
             print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay))
          else
             print("Flight would depart on time")
       #break
    } else if(data_input$MONTH=="12")
        { load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_dec1.rda")
          Predicted_depart_delay<- predict(ranger_dec1 , data_input)$predictions
         if(Predicted_depart_delay > 0)
               print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay))
         else
             print("Flight would depart on time")
     }


