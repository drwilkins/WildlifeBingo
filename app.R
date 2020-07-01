


library(shiny);library(rlist);require(stringr)

#Prep game
afilez<-list.files("www/",pattern="(.mp3)") #find audio files; use to set up bingo game
filez<-sapply(afilez,function(x) tools::file_path_sans_ext(x),USE.NAMES = F) 
picnamez<-sapply(filez,function(x) gsub(pattern="_.+$",replacement="",x))
picfilez<-paste0(picnamez,".jpg")
answers<-sapply(filez,function(x) gsub("_XC.*","",x),USE.NAMES = F)
#all the spec names have _1 at end due to an annoying issue w/ my prepStaticSpec code
specs<-paste0(filez,"_1.png")


# Define UI for application that draws a histogram
ui <- navbarPage(title="Backyard Bioacoustics",theme=shinythemes::shinytheme("cerulean"),
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
                       border-width: 3px;
                       border-style: solid;
                       max-height: 200px;
                       max-width: 100%;
                       width: auto;
                       }
                      .acoustics{
                        width: auto;
                        max-height: 200px;
                        display: inline;
                      }")
                  )
     ),
   sidebarPanel(
    # sidebarPanel(style="position:fixed",
    #    # Sidebar with a slider input for number of bins 
    fluidRow(
      checkboxInput("showpic","Show Picture?",FALSE),
    sliderInput("picsize","Picture Size",20,1000,100,step=20,ticks=F),
    actionButton("Reset for next round","reset"),
        tags$br(),
         tags$br(),
         tags$br(),
        "--------------------------------",
        actionButton("call","Next Sound"),
        checkboxInput("reveal","Reveal Answers?",FALSE))
    ), #end sidebar
     mainPanel(
         fluidRow( 
         tags$br(), 
         uiOutput('dynamic'),
         tags$br(),tags$br(),tags$br(),tags$br(),
         tags$br()
          )
       )#End main panel
     )),#End Quiz Tab
   tabPanel("Study",
          sidebarPanel(width=2,
          sliderInput("picsize","Picture Size",20,1000,100,step=20,ticks=F),
          checkboxInput("showLabs","c",F)
          ),
          mainPanel(width=10,
          lapply(1:length(filez),function(i) {
            wellPanel(fluidRow(
              h3(paste0(i,": ",answers[i])),
              img(src=picfilez[i],height="200px",width="auto"),
              div(class="acoustics",
              img(src=specs[i]),
              tags$audio(src=afilez[i],type = 'audio/mp3', controls = 'false')
              )
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
  
  afilez<-list.files("www/",pattern="(.wav|.mp3)") #find audio files; use to set up bingo game
  filez<-sapply(afilez,function(x) tools::file_path_sans_ext(x),USE.NAMES = F) #stripped version used to pull in audio & visual items
  
  #Sample the files initially (random)
  isolate({
  vals$ordr<-sample(filez,length(filez),replace=F)
    })
  
  #Reset Button
  observeEvent(input$reset,{
    vals$counter=1
    vals$ordr<-sample(filez,length(filez),replace=F)
  })
  
  #next bingo call
  observeEvent(input$call,{
    vals$counter=vals$counter+1
    print("---------------------")
    key<-as.matrix(t(rbind(1:vals$counter,vals$ordr[1:vals$counter])))
    rownames(key)<-rep("",vals$counter)
    colnames(key)<-c("Sound","Answer")
    print(key)
  })
  
  output$dynamic<-renderUI({
      # for (i in 1:vals$counter){
        # tagstring<-as.list((paste0("tags$audio(src='",vals$ordr,"',type = 'audio/mp3', controls = 'false')",sep=",")))
        # tagList(tagstring)
    tagstring<-lapply(1:vals$counter,function(i) {
      
      if(input$showpic==F){
      noanswer<-list(h1(paste0(i,"  "),style=" display:inline"),tags$audio(src=isolate(paste0(vals$ordr[i],".mp3")),type = 'audio/mp3', controls = 'false'))  
      }else{
      picname<-gsub(pattern="_.+$",replacement="",x=isolate(vals$ordr[i] ))
        noanswer<-list(wellPanel(
          tags$h1(paste0(i,"  "),style=" display:inline"),tags$img(src=paste0(picname,".jpg"), height=input$picsize,width= "auto"),tags$audio(src=isolate(paste0(vals$ordr[i],".mp3")),type = 'audio/mp3', controls = 'false'),
        tags$br(),tags$br()))
      }
      
      #Test if answer wanted
      if(input$reveal==F){
        out<-list.append(noanswer,tags$br(),tags$br())
      }else{
        out<-list.append(noanswer,tags$h3(vals$ordr[i],style=" display:inline"),
          tags$br(),tags$br())
      }
      out
    })
    tagList(tagstring)
    #tagList(eval(parse(text=tagstring)))
    #   }
    # HTML(tags)
  })
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)

