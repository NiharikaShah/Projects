#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(V8)
library(shinythemes)
library(dashboard)
library(ranger)
library(shinydashboard)
library(lubridate)
rm(list=ls())


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme= shinytheme("journal"),
    skin="purple",
    # Application title
    titlePanel("Flights Delay Prediction"),

    # Sidebar with a slider input for number of bins 
   # sidebarLayout(
   #     sidebarPanel(
    #        sliderInput("bins",
          #              "Number of bins:",
           #             min = 1,
            #            max = 50,
            #            value = 30)
       # ),

        # Show a plot of the generated distribution
        #mainPanel(
           
       # )
   # )
   dashboardBody(
       skin="purple",
       fluidRow(
           
         #  box(
               #title = "MONTH",
             #  selectInput("MONTH", "MONTH", choices = c("January", "February", "March", "April" , "May" , "June" ,"July" ,"August" ,"September" , "October" , "November" , "December")),
             #  width=3          
          # ),
          ## box(
               #title = "DAY",
              # selectInput("DAY", "DAY", choices = c("1", "2", "3", "4" , "5" , "6" ,"7" ,"8" ,"9" , "10" , "11" , "12" ,"13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")),
              # width=3          
           #),
           #box(
              # title = "DAY OF WEEK",
              # selectInput("DAY_OF_WEEK", "DAY OF WEEK", choices = c("1", "2", "3", "4" , "5" , "6" ,"7")),
              # width=3          
          # ),
           box(
               #title = "AIRLINE",
               selectInput("AIRLINE", "AIRLINE", choices = c("AA","AS", "B6", "DL", "EV", "F9", "HA", "MQ", "NK", "OO", "UA", "US" ,"VX", "WN")),
               width=3          
           ),
           box(
             #title = "DATE",
             dateInput("Date", "Date",format = "yyyy-mm-dd"),
             width=3 
           )
       ),
       fluidRow(
           box(
              # title = "ORIGIN AIRPORT",
               selectInput("ORIGIN_AIRPORT", "ORIGIN AIRPORT ", choices = c("10135","10136","10140","10141","10146","10154","10155","10157","10158","10165","10170","10185","10208","10257","10268",
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
               width=3          
           ),
           box(
               #title = "DESTINATION AIRPORT",
               selectInput("DESTINATION_AIRPORT", "DESTINATION AIRPORT", choices = c("10135","10136","10140","10141","10146","10154","10155","10157","10158","10165","10170","10185","10208","10257","10268",
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
               
               width=3          
           ),
           box(
              # title = "SCHEDULED DEPARTURE ",
               selectInput("SCHEDULED_DEPARTURE_TIME_HOURS", "SCHEDULED DEPARTURE", choices = c("0-1","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-11","11-12","12-13","13-14","14-15","15-16","16-17","17-18","18-19","19-20","20-21","21-22","22-23","23-24")),
               width=3          
           ) 
           
           
           
       ),
       fluidRow(
           box(
              # title="TAXI OUT",
               textInput("TAXI_OUT", "TAXI OUT"),
               width=3
           ),
           box(
               #title="WHEELS OFF",
               textInput("WHEELS_OFF", "WHEELS OFF"),
               width=3
           ),
           box(
               #title="SCHEDULED TIME",
               textInput("SCHEDULED_TIME", "SCHEDULED TIME"),
               width=3
           ),
           box(
               #title="DISTANCE",
               textInput("DISTANCE", "DISTANCE"),
               width=3
           )
       ),
       mainPanel(
       #textOutput("string"),
       
       actionButton("delay","Check Delay"),
       
       shinyjs::useShinyjs(),
       shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
       #textInput("text", "Text", ""),
       actionButton("reset", "Reset"),
       
       
       verbatimTextOutput("value", placeholder = TRUE )
       #dataTableOutput("value")
       )
   )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # input_variables <- reactiveValuesToList({
         
     #})
   observeEvent(input$reset, {
      # shinyjs::js$refresh()
     #reset("MONTH")
     #reset("DAY")
     #reset("DAY_OF_WEEK")
     rm(list=ls())
     
     reset("AIRLINE")
     reset("ORIGIN_AIRPORT")
     reset("DESTINATION_AIRPORT")
     reset("TAXI_OUT")
     reset("WHEELS_OFF")
     reset("SCHEDULED_TIME")
     reset("DISTANCE")
    reset("Date")
   })
    
   # observeEvent(
    #    input$delay,{}
        
    #)
    
    dataInput <- eventReactive(
        input$delay,
       {
       # input$MONTH
       # input$DAY
      # input$DAY_OF_WEEK
        input$AIRLINE
        input$ORIGIN_AIRPORT
        input$DESTINATION_AIRPORT
        input$TAXI_OUT
        input$WHEELS_OFF
        input$SCHEDULED_TIME
        input$DISTANCE
       input$SCHEDULED_DEPARTURE_TIME_HOURS
       input$Date
        
    })
    
    
    #output$string <- renderPrint({
    #dataInput()
      #  print(paste0("Month :" ,input$MONTH,
                  #   "Day :", input$DAY,
                   #  "Day Of Week :" ,input$DAY_OF_WEEK,
                   #  "Airline:", input$AIRLINE,
                    # "Origin Airport:" ,input$ORIGIN_AIRPORT,
                    # "Destination Airport :", input$DESTINATION_AIRPORT,
                    # "Taxi Out:" ,input$TAXI_OUT,
                    # "Wheels Off :", input$WHEELS_OFF,
                    # "Scheduled Time:", input$SCHEDULED_TIME,
                    # "Distance:" ,input$DISTANCE,
                   #  "Scheduled Departure :", input$SCHEDULED_DEPARTURE_TIME_HOURS
                     
                     
                   #  ))
  #  })
    
     

        output$value <- renderPrint({
            #MONTH <- input$MONTH
            #DAY <- input$DAY
            #DAY_OF_WEEK <- input$DAY_OF_WEEK
            #AIRLINE <- input$AIRLINE
            #ORIGIN_AIRPORT <- input$ORIGIN_AIRPORT
            #DESTINATION_AIRPORT <- input$DESTINATION_AIRPORT
            #TAXI_OUT <- input$TAXI_OUT
            #WHEELS_OFF <- input$WHEELS_OFF
            #SCHEDULED_TIME <- input$SCHEDULED_TIME
            #DISTANCE <- input$DISTANCE
            #SCHEDULED_DEPARTURE_TIME_HOURS <- input$SCHEDULED_DEPARTURE_TIME_HOURS
             
             
             # print(paste0("i am here", MONTH DAY DAY_OF_WEEK WHEELS_OFF))
            dataInput()
          DAY1 <- day(input$Date)
          DAY_OF_WEEK1 <- wday(input$Date)
          MONTH1 <- month(input$Date)
          
        if(DAY_OF_WEEK1 == "1")
           {
             DAY_OF_WEEK1 <- "7"
            
            }
        else if(DAY_OF_WEEK1 == "2")
        {
          DAY_OF_WEEK1 <- "1"
        }
        else if(DAY_OF_WEEK1== "3")
        {
          DAY_OF_WEEK1 <- "2"
        }
        else if(DAY_OF_WEEK1== "4")
        {
          DAY_OF_WEEK1 <- "3"
        }
        else if(DAY_OF_WEEK1=="5")
        {
          DAY_OF_WEEK1 <- "4"
        }
        else if(DAY_OF_WEEK1=="6")
        {
          DAY_OF_WEEK1 <- "5"
        }
        else if(DAY_OF_WEEK1=="7")
        {
          DAY_OF_WEEK1 <- "6"
        }
          
          
           # print( paste0("DATE :" ,DAY1 , DAY_OF_WEEK1 , MONTH1))
            
            # data_input <- data.frame( DAY=as.factor(DAY1), DAY_OF_WEEK=as.factor(DAY_OF_WEEK1) , AIRLINE=as.factor(input$AIRLINE) , ORIGIN_AIRPORT=as.factor(input$ORIGIN_AIRPORT) , DESTINATION_AIRPORT=as.factor(input$DESTINATION_AIRPORT), TAXI_OUT=as.numeric(input$TAXI_OUT), WHEELS_OFF=as.numeric(input$WHEELS_OFF), SCHEDULED_TIME=as.numeric(input$SCHEDULED_TIME), DISTANCE=as.numeric(input$DISTANCE), SCHEDULED_DEPARTURE_TIME_HOURS=as.factor(input$SCHEDULED_DEPARTURE_TIME_HOURS))
            data_input <- data.frame(DAY= factor(DAY1, levels=c(1:31)),
                                           DAY_OF_WEEK= factor(DAY_OF_WEEK1, levels=c(1:7)),
                                           AIRLINE = factor(input$AIRLINE,levels=c("AA","AS", "B6", "DL", "EV", "F9", "HA", "MQ", "NK", "OO", "UA", "US" ,"VX", "WN")) ,
                                           ORIGIN_AIRPORT =factor(input$ORIGIN_AIRPORT, levels =c("10135","10136","10140","10141","10146","10154","10155","10157","10158","10165","10170","10185","10208","10257","10268",
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
                                           DESTINATION_AIRPORT = factor(input$DESTINATION_AIRPORT ,levels =c("10135","10136","10140","10141","10146","10154","10155","10157","10158","10165","10170","10185","10208","10257","10268",
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
                                           TAXI_OUT= as.integer(input$TAXI_OUT),
                                           WHEELS_OFF= as.integer(input$WHEELS_OFF),
                                           SCHEDULED_TIME=as.integer(input$SCHEDULED_TIME),
                                           DISTANCE= as.integer(input$DISTANCE),
                                           SCHEDULED_DEPARTURE_TIME_HOURS= factor(input$SCHEDULED_DEPARTURE_TIME_HOURS, levels = c("0-1","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-11","11-12","12-13","13-14","14-15","15-16","16-17","17-18","18-19","19-20","20-21","21-22","22-23","23-24")))
            
             #print(paste0(data_input$DAY_OF_WEEK))
             #print(paste0(data_input$AIRLINE))
             #print(paste0(data_input$DISTANCE))

             if(month(input$Date)=="1")
             { load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_jan1.rda")
                 Predicted_depart_delay_jan<- predict(ranger_jan1 , data_input)$predictions
                 if(Predicted_depart_delay_jan > 0)
                     print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay_jan))
                 else
                     print(paste0("Flight would depart on time (" , Predicted_depart_delay_jan, ")" ))
                 # break
             } else if(month(input$Date)=="2")
             { load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_feb1.rda")
                 Predicted_depart_delay_feb<- predict(ranger_feb1 , data_input)$predictions
                 if(Predicted_depart_delay_feb > 0)
                     print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay_feb))
                 else
                   print(paste0("Flight would depart on time (" , Predicted_depart_delay_feb, ")" ))
                 # break
             } else if(month(input$Date)=="3")
             { load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_march1.rda")
                 Predicted_depart_delay_march<- predict(ranger_march1 , data_input)$predictions
                 if(Predicted_depart_delay_march > 0)
                     print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay_march))
                 else
                   print(paste0("Flight would depart on time (" , Predicted_depart_delay_march, ")" ))
                 # break
             }  else if(month(input$Date)=="4")
             { load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_april1.rda")
                 Predicted_depart_delay_april<- predict(ranger_april1 , data_input)$predictions
                 if(Predicted_depart_delay_april > 0)
                     print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay_april))
                 else
                   print(paste0("Flight would depart on time (" , Predicted_depart_delay_april, ")" ))
                 # break
             }  else if(month(input$Date)=="5")
             {  load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_may1.rda")
                 Predicted_depart_delay_may<- predict(ranger_may1 , data_input)$predictions
                 if(Predicted_depart_delay_may > 0)
                     print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay_may))
                 else
                   print(paste0("Flight would depart on time (" , Predicted_depart_delay_may, ")" ))
                 #break
             }     else if(month(input$Date)=="6")
             {   load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_june1.rda")
                 Predicted_depart_delay_june<- predict(ranger_june1 , data_input)$predictions
                 if(Predicted_depart_delay_june > 0)
                     print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay_june))
                 else
                   print(paste0("Flight would depart on time (" , Predicted_depart_delay_june, ")" ))
                 # break
             } else if(month(input$Date)=="7")
             { load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_july1.rda")
                 Predicted_depart_delay_july<- predict(ranger_july1 , data_input)$predictions
                 if(Predicted_depart_delay_july > 0)
                     print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay_july))
                 else
                   print(paste0("Flight would depart on time (" , Predicted_depart_delay_july, ")" ))
                 #break
             } else if(month(input$Date)=="8")
             { load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_aug1.rda")
                 Predicted_depart_delay_aug<- predict(ranger_aug1 , data_input)$predictions
                 if(Predicted_depart_delay_aug > 0)
                     print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay_aug))
                 else
                   print(paste0("Flight would depart on time (" , Predicted_depart_delay_aug, ")" ))
                 #break
             } else if(month(input$Date)=="9")
             {  load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_sep1.rda")
                 Predicted_depart_delay_sep<- predict(ranger_sep1 , data_input)$predictions
                 if(Predicted_depart_delay_sep > 0)
                     print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay_sep))
                 else
                   print(paste0("Flight would depart on time (" , Predicted_depart_delay_sep, ")" ))
                 # break
             } else if(month(input$Date)=="10")
             { load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_oct1.rda")
                 Predicted_depart_delay_oct<- predict(ranger_oct1 , data_input)$predictions
                 if(Predicted_depart_delay_oct > 0)
                     print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay_oct))
                 else
                   print(paste0("Flight would depart on time (" , Predicted_depart_delay_oct, ")" ))
                 # break
             } else if(month(input$Date)=="11")
             {   load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_nov1.rda")
                 Predicted_depart_delay_nov<- predict(ranger_nov1 , data_input)$predictions
                 if(Predicted_depart_delay_nov > 0)
                     print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay_nov))
                 else
                   print(paste0("Flight would depart on time (" , Predicted_depart_delay_nov, ")" ))
                 #break
             } else if(month(input$Date)=="12")
             { load(file="D:/desktop/data science/Flight delay predictions/flights/FlightsInMins/ranger_dec1.rda")
                 Predicted_depart_delay_dec<- predict(ranger_dec1 , data_input)$predictions
                 if(Predicted_depart_delay_dec > 0)
                     print(paste0("Predicted Departure Delay of the flight is " ,Predicted_depart_delay_dec))
                 else
                   print(paste0("Flight would depart on time (" , Predicted_depart_delay_dec, ")" ))
             }
             
             
             
             
         })
         


}

# Run the application 
shinyApp(ui = ui, server = server)
