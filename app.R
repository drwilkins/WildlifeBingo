


library(shiny);library(rlist)

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel("Animal Sound Bingo"),
    sidebarLayout(
    sidebarPanel(style="position:fixed",width=2,
       # Sidebar with a slider input for number of bins 
    checkboxInput("showpic","Show Picture?",FALSE),
    actionButton("call","Next Sound"),
        tags$br(),
         tags$br(),
         tags$br(),
        "--------------------------------",
        checkboxInput("reveal","Reveal Answers?",FALSE)), #end sidebar
     mainPanel(
       tags$br(), 
       uiOutput('dynamic'),
       tags$br(),tags$br(),tags$br(),tags$br(),
        actionButton("reset","Reset for next round"),
        tags$br(),
        tags$br()
       
      )#End main panel
   )
)


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
      noanswer<-list(tags$h1(paste0(i,"  "),style=" display:inline"),tags$audio(src=isolate(paste0(vals$ordr[i],".mp3")),type = 'audio/mp3', controls = 'false'))  
      }else{
      picname<-gsub(pattern="_.+$",replacement="",x=isolate(vals$ordr[i] ))
        noanswer<-list(
          tags$h1(paste0(i,"  "),style=" display:inline"),tags$img(src=paste0(picname,".jpg"), height=200,width= "auto"),tags$audio(src=isolate(paste0(vals$ordr[i],".mp3")),type = 'audio/mp3', controls = 'false'),
        tags$br(),tags$br())
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

