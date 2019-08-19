if(!require("shiny")){
  install.packages("shiny")
  library("shiny")
}

if(!require("shinyjs")){
  install.packages("shinyjs")
  library("shinyjs")
}

if(!require("tidyverse")){
  install.packages("tidyverse")
  library("tidyverse")
}

if(!require("shinyalert")){
  install.packages("shinyalert")
  library("shinyalert")
}

source("Shinyhelper.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  
  fluidRow(
    
    column(width = 8, 
           shinyjs::useShinyjs(),
           wellPanel(
             h2 ("Folder Exploder"),
             p("This application allows you to unravel (explode) a folder and copied the unraveled folder or files to the destination folder"),
             p("If resursive option is selected, all individual files in each subfolder will be copied"),
             p("This app is also useful if you want to extra a particular file type from a bunch of folder. 
                 For example, if you enable the resursive option and enter .pdf in the advanced option below, then all the pdf files will be copied"),
             textAreaInput(inputId = "source_folder", label = "Please enter the source directory. (Example: C:\\Users\\john.doe\\Desktop\\folder1)",
                           value = "D:\\Gatech OneDrive\\OneDrive - Georgia Institute of Technology\\R Project\\FolderExploderRshiny\\For Testing\\Test Folder Source"),
             textAreaInput(inputId = "destination_folder", label = "Please enter the desination directory. (Example:  C:\\Users\\john.doe\\Desktop\\folder2)",
                           value = "D:\\Gatech OneDrive\\OneDrive - Georgia Institute of Technology\\R Project\\FolderExploderRshiny\\For Testing\\Test Folder Destination"), 
             wellPanel(
               numericInput(inputId = "depth", label = "Please enter the depth of folder explotion in integer (0 or above).
                      (0 mean simply copying the files in the directory.1 means explode the folder by 1 level", value = 0L),
               checkboxInput(inputId = "recursive_option", label = "Explode Resursively? (If this option is selected, 
                          then all individual items will be copied to the destination folder.",value = F)
             ),
             hr(),
             wellPanel(
               h2 ("Advanced Options"),
               p('This section allows you to explode files with selected characteristics using AND Condition'),
               p('This section is most useful when resursive option are selected.'),
               
               checkboxInput(inputId = "advanced_option", label = "Enable advanced options", value = F),
               
               disabled(
                 textInput(inputId = "start_pattern", label = "Start with string (Only files start with this string will be copied)")
               ),
               
               disabled(
                 textInput(inputId = "end_pattern", label = 'End with string (Only files end this name will be copied) . 
          (A good example will be ".html", ".xlsx", ".csv")')
               ),
               disabled(
                 textInput(inputId = "contain_pattern", label = "Contains (only files contains this string will be copied)")
               ),
               
               hr()
               
             ),
             
             withBusyIndicatorUI(
               actionButton(inputId = "explode_button", label = "Explode Folders",icon = icon("fas fa-folder-open"), class = "btn-primary")
             ))
           
           
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  
  # Stop the Rapp session after user close the browser (so it won't keep running in the background)
  session$onSessionEnded(stopApp) 
  
  # stop the app in non-interactive session
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })}
  
  # 0.0  If user select the resursive option, grey out the depth element
  observe(if (input$recursive_option == T){
    shinyjs::disable("depth")
  }else{shinyjs::enable("depth")})
  
  #0.1 If user select the advance_options, enable the string options
  
  
  observe(if(input$advanced_option == T){
    shinyjs::enable("start_pattern")
    shinyjs::enable("end_pattern")
    shinyjs::enable("contain_pattern")
  } else{
    shinyjs::disable("start_pattern")
    shinyjs::disable("end_pattern")
    shinyjs::disable("contain_pattern")
  }
  )
  
  
  observeEvent(input$explode_button, {withBusyIndicatorServer("explode_button", {
    
    print ("button clicked")
    
    # 1st check directory
    if (! dir.exists(input$source_folder)){
      showModal(modalDialog("Please enter a valid path for the source folder",easyClose = T))
      stop("Please enter a valid path for the source folder")
    }
    
    if (! dir.exists(input$destination_folder)){
      showModal(modalDialog("Please enter a valid path for the destination folder",easyClose = F))
      stop("Please enter a valid path for the destination folder")
    }
    
    print ("Pass directory check")
    
    
    
    #2nd check if the user enters a valid integer in the depth as input
    
    if (input$depth == ""){
      stop("The depth you entered is not a valid integer")
      showModal(modalDialog("You did not enter a depth.
                                     Please enter a valid integer for depth"))
    }
    
    tryCatch({
      temp  <-  as.integer(input$depth)
      int_indicator <- is.integer(temp)
      
      if (int_indicator == FALSE){
        showModal(modalDialog("The depth you entered is a number but not a valid integer. Please enter a valid integer for depth", easyClose = T))
        stop("The depth you entered is a number but not a valid integer")
      } else if (int_indicator == TRUE & input$depth %%1 != 0){
        print ("xxx")
        showModal(modalDialog("The depth you entered is a number but not a valid integer. Please enter a valid integer for depth", easyClose = T))
        stop("The depth you entered is a number but not a valid integer")
        
      }
    },warning = function(w){
      print ("in warning")
      showModal(modalDialog("The depth you entered is not a valid integer. Please enter a valid integer for depth", easyClose = T))
      stop("The depth you entered is not a valid integer")
    },error = function(e){
      print ("in error")
      showModal(modalDialog("The depth you entered is not a valid integer. Please enter a valid integer for depth", easyClose = T))
      stop("The depth you entered is not a valid integer")}
    
    ) # end of try catch
    print ("Pass integer directory check")
    
    
    print ("start looping through directory")
    
    
    #--------------- define helper function-------------------
    explode_folder <- function(filepath, depth = 0,recursive = F){
      
      # if recursive == T, explode every subfolder to its end and copy all individual files
      if (recursive == T){
        filepath <- list.files(path = filepath,full.names = T, pattern = NULL, all.files = F,
                               recursive = T, ignore.case = T)
        return (filepath)
      }
      
      
      
      
      # If depth ==0 simply copy and paste
      if (depth == 0){
        filepath <- list.files(path = filepath,full.names = T, pattern = NULL, all.files = F, recursive = F, ignore.case = T)
        return (filepath)
      }
      
      else if (depth >= 1){
        for (i in c(0:depth)){
          filepath_temp <- filepath
          filepath <- list.files(path = filepath,full.names = T, pattern = NULL, all.files = F, recursive = F, ignore.case = T)
          #print (filepath_temp)
          if (length(filepath) == 0){
            return (filepath_temp)
          }
        }
      }
      return (filepath)
    }
    
    
    
    test_reg <- function(str,start_pattern = "", contain_pattern = "",end_pattern = ""){
      
      if (all(c(start_pattern, contain_pattern, end_pattern) == "")){
        # if all the input string are empty, return TRUE
        return (T)
      }
      # since only the name after the last slash contains the file name
      # thus we need to subset the last element after split
      
      str <- last(str_split(str, "/")[[1]])
      
      if (any(!missing(start_pattern), !missing (contain_pattern), !missing(end_pattern))){
        print ("condition1")
        if (start_pattern != "") {
          start_pattern <- paste0("^", start_pattern)
        }
        if (end_pattern != "") {end_pattern <- paste0( end_pattern, "$")}
        
        
        
        
        return (suppressWarnings(
          all(str_detect(str, c(tolower(start_pattern), tolower(contain_pattern), tolower(end_pattern))),
              na.rm = T)))
        
      } else {
        # if there is no pattern to match, just return true
        print ("in else condition")
        return (T)
      }
      
    }
    #---------------------------------------------------------------------------
    explode_folder(input$source_folder)
    
    
    filepath <- explode_folder(input$source_folder, depth = input$depth, recursive = input$recursive_option)
    print ("Done loop through the directory")
    
    
    # deal with advanced option (string matching)
    if (input$advanced_option == T){
      bool <- filepath %>% tolower %>% map_lgl(test_reg, start_pattern = input$start_pattern,
                                               contain_pattern = input$contain_pattern,
                                               end_pattern = input$end_pattern)
      print (bool)
      filepath <- filepath[bool]
      
    }
    
    print (filepath)
    print ("finished dealing with regular expression")
    
    
    # copy files
    
    file.copy(from = filepath , to = input$destination_folder,recursive = T)
    
    showModal(modalDialog(paste0("The files fitting your criteria of depth or string matching are copied to ( ",
                                 input$destination_folder, " ).  ", length(filepath), " files are copied.")))
    print ("Done copying files")
    
    
  })
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server,options = (display.mode="showcase"))