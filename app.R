


library(shiny);library(rlist)

#Prep game
afilez<-list.files("www/",pattern="(.mp3)") #find audio files; use to set up bingo game
filez<-sapply(afilez,function(x) tools::file_path_sans_ext(x),USE.NAMES = F) 
picnamez<-sapply(filez,function(x) gsub(pattern="_.+$",replacement="",x))
picfilez<-paste0(picnamez,".jpg")
specfilez<-paste0(filez,"_1.png")
answers<-sapply(filez,function(x) gsub("_XC.*","",x),USE.NAMES = F)
#all the spec names have _1 at end due to an annoying issue w/ my prepStaticSpec code



# Define UI for application that draws a histogram
ui <- navbarPage(title="Backyard Bioacoustics",position="fixed-top",theme=shinythemes::shinytheme("cerulean"),
   # Application title
   #titlePanel("Who Said That? A Backyard Bioacoustics Game"),
   #dashboardHeader("dsjks"),
   tabPanel("Quiz", 
   fluidPage(
     #set styles
     tags$head(
       tags$style(HTML("
                       img{
                       margin: 10px;
                       border-width: 1px;
                       border-style: solid;
                       max-height: 200px;
                       max-width: 100%;
                       width: auto;
                       display: inline;
                       }
                      
                      body {padding-top: 70px;}
                      @media screen and (max-width: 768px) {
                      body { padding-top: 170px; }
                        }
                      
                       .sidebar{
                         border-color: #317eac;
                         border-width: 2px;
                         border-style: solid;
                         background:#c5e6f8;
                         color: #495057;}
                       
                       .callButton{
                         background: #2FA4E7;
                         color: white;
                       }
                       .well{
                       border-width: 3px;
                       border-style: solid; 
                       border-color: #555555;
                       
                       }
                       h3{
                       background-color: #555555;
                       color: white;
                       margin-left:0px; 
                       margin-right: 0px;
                       text-indent: 8px;
                       width: 100%;
                       padding: 0px;
                       }
                       ")
                  )
     ),
   sidebarPanel(class="sidebar",width=3,title="Setup",
    #    # Sidebar with a slider input for number of bins 
    fluidRow(
      p(strong("Show?")),
      div(style="display:inline",
      checkboxInput("showpic","Picture",FALSE),
      checkboxInput("showspec","Spectrogram",F),
      checkboxInput("showaudio","Audio Clip?",T)
      ),
    sliderInput("picsize","Picture Size",20,1000,100,step=20,ticks=F,width="100%"),
    actionButton("reset","Reset",class="btn btn-secondary",icon=icon("times-circle")),
    br(),
       tags$br(),
      shinyWidgets::prettyToggle("reveal","Show Answers","Hide Answers"))
       #actionButton("reveal","",class="btn btn-info",icon=icon("bolt")))
    ), #end sidebar
     mainPanel(width=9,
         fluidRow( 
         tags$br(), 
         uiOutput('dynamic'),
         tags$br(),
         actionButton("call","Next Sound",class="btn btn-primary"),
         br(),br()
          )
       )#End main panel
     )),#End Quiz Tab
   tabPanel("Study",
          # sidebarPanel(width=2,
          # sliderInput("picsize","Picture Size",20,1000,100,step=20,ticks=F),
          # checkboxInput("showLabs","c",F)
          # ),
          mainPanel(class="body",width=10,
          lapply(1:length(filez),function(i) {
            wellPanel(fluidRow(
              h3(paste0(i,": ",answers[i])),
              img(src=picfilez[i],height="200px",width="auto"),
              img(src=specfilez[i]),
              tags$audio(src=afilez[i],type = 'audio/mp3', controls = 'false')
              
            )
            )
          })
          )
          )
   
  )#end UI



#------------------------------------------------
# Define server logic required to draw a histogram
server <- function(input, output) {
  vals<-reactiveValues()
  vals$counter=1
  
  afilez<-list.files("www/",pattern="(.mp3)") #find audio files; use to set up bingo game
  filez<-sapply(afilez,function(x) tools::file_path_sans_ext(x),USE.NAMES = F) #stripped version used to pull in audio & visual items
  
  #Sample the files initially (random)
  isolate({
  vals$ordr<-sample(1:length(filez),length(filez),replace=F)#sample(filez,length(filez),replace=F)
    })
  
  #Reset Button
  observeEvent(input$reset,{
    vals$counter <- 1
    vals$ordr<-sample(1:length(filez),length(filez),replace=F)
    print(paste0("counter=",vals$counter))
    print(paste0("order=",vals$ordr))
    
  })
  
  #next bingo call
  observeEvent(input$call,{
    vals$counter=vals$counter+1
    print("---------------------")
    key<-as.matrix(t(rbind(1:vals$counter,filez[vals$ordr[vals$counter]])))
    rownames(key)<-rep("",vals$counter)
    colnames(key)<-c("Sound","Answer")
    print(key)
  })
  
  
  fullQuiz<-reactive({
    out<-list()
    
      for(i in 1:vals$counter){
    #which sample of the randomized order are we on?
    whichSample<-vals$ordr[i]
    #assemble pieces of well panel
    audio<-if(input$showaudio){tags$audio(src=afilez[whichSample],type = 'audio/mp3', controls = 'false')}else{}
    
    pic<- if(input$showpic){img(src=picfilez[whichSample],height="200px",width="auto")}else{}
    
    spec<- if(input$showspec){img(src=specfilez[whichSample])}else{}
    
    text<- if(input$reveal){h3(paste0(i,": ",answers[whichSample]))}else{paste0("Mystery ",i)}
    
    out_i<-wellPanel(fluidRow(audio,spec,pic,text))
    out[[i]]<-out_i
      }#end for loop
    
    out
  }
  )
  
  
  output$dynamic<-renderUI({
      # for (i in 1:vals$counter){
        # tagstring<-as.list((paste0("tags$audio(src='",vals$ordr,"',type = 'audio/mp3', controls = 'false')",sep=",")))
        # tagList(tagstring)
    # tagstring<-lapply(1:vals$counter,function(i) {
    #   
    #   if(input$showpic==F){
    #   noanswer<-list(h1(paste0(i,"  "),style=" display:inline"),tags$audio(src=isolate(paste0(vals$ordr[i],".mp3")),type = 'audio/mp3', controls = 'false'))  
    #   }else{
    #   picname<-gsub(pattern="_.+$",replacement="",x=isolate(vals$ordr[i] ))
    #     noanswer<-list(wellPanel(
    #       tags$h1(paste0(i,"  "),style=" display:inline"),tags$img(src=paste0(picname,".jpg"), height=input$picsize,width= "auto"),tags$audio(src=isolate(paste0(vals$ordr[i],".mp3")),type = 'audio/mp3', controls = 'false'),
    #     tags$br(),tags$br()))
    #   }
    #   
    #   #Test if answer wanted
    #   if(input$reveal==F){
    #     out<-list.append(noanswer,tags$br(),tags$br())
    #   }else{
    #     out<-list.append(noanswer,tags$h3(vals$ordr[i],style=" display:inline"),
    #       tags$br(),tags$br())
    #   }
    #   out
    # })
    #assemble each mystery
    tagList(fullQuiz()[1:vals$counter])
  
  })
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)

