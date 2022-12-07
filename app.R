libs <- "pak,shiny,ggplotgui,glue,plotly,readr,stringr,readxl,haven,RColorBrewer,DT,scatterD3,dplyr,ggrepel,colourpicker,svglite,remotes,connectapi,lubridate,config,shinysurveys,googlesheets4,queryBuilder,shinyjs,ggalluvial,htmltools,sp"
libs <- unlist(strsplit(libs,","))
req<-unlist( lapply(libs,function(p) suppressPackageStartupMessages(require(p,character.only=TRUE)) ) )
need<-libs[req==FALSE]
print(need)

if (0){
  if ("pak" %in% need){
    install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))
  }
  options(install.packages.check.source = "no")
  if ("lubridate" %in% need){
    install.packages("lubridate")
  }
  for (lib in need) { pak::pkg_install(lib,ask = FALSE) }
  req<-unlist( lapply(libs,function(p) suppressPackageStartupMessages(require(p,character.only=TRUE)) ) )
  need<-libs[req==FALSE]
  print(need)
} else {
  library(ggalluvial)
  library(htmltools)
  library(sp)
  
  library(glue)
  library(shiny)
  library(plotly)
  library(readr)
  library(stringr)
  library(readxl)
  library(haven)
  library(RColorBrewer)
  library(DT)
  library(dplyr)
  library(colourpicker)
  library(svglite)
  library(remotes)
  library(lubridate)
  library(config)
  library(shinysurveys)
  library(googlesheets4)
  library(queryBuilder)
}

googlesheeturl <- "https://docs.google.com/spreadsheets/d/1IgeQizH3Gtz9wngRoD61Ro8hqYgJp2uMHTLRT1vPzVM/edit#gid=1005821039"

gs4cachefolder <- "gs4cache"
gs4email <- "stjudecab@gmail.com"

MAX_UNIQUE_VAL_AS_FACTOR <- 30

#options(gargle_oauth_cache = gs4cachefolder) 

options(shiny.maxRequestSize=1000*1024^2)

dataset <- iris 

myregex <- function(stra,strb){
  regexpr(stra,strb,ignore.case=T,TRUE) > 0
}

myregexmatches <- function(stra,strb,todel=""){
  y1 <- regexpr(stra,strb,ignore.case=T,TRUE)
  z1 <- y1 + attr(y1, "match.length")-1
  ss <- substr(strb, y1, z1)
  if (length(todel) > 0) {
    list( sub(todel,"",ss), sub(ss,"", strb) )
  } else { list(ss, sub(ss,"", strb)) }
}

transvaluebypara <- function(nums,transmode){
  #[neg]log[2]|pse[0]|plus[0]|minus[0]|multi[1]|ori]
  if (is.null(nums)){
    return(nums)
  }
  tmp <- as.numeric(nums)
  if (all(is.na(nums))){
    message("All NA!\n")
    return(nums)
  }
  if ( myregex("neg",transmode) ){
    factor = -1
    transmode <- gsub("neg","",transmode)
  } else { factor = 1 }
  if ( myregex("pse",transmode) ){
    ddtmp <- myregexmatches("pse[[:digit:].]*",transmode,"pse")
    transmode <- ddtmp[[2]]
    thepse <- ddtmp[[1]]
    if (nchar(thepse) == 0){
      thepse = 1e-300
    } else { thepse <- as.numeric(thepse) }
  } else { thepse <- 1e-300 }
  if ( myregex("log",transmode) ){
    ddtmp <- myregexmatches("log[[:digit:].]*",transmode,"log")
    transmode <- ddtmp[[2]]
    thelog <- ddtmp[[1]]
    if (nchar(thelog) == 0){
      thelog = 2
    } else { thelog <- as.numeric(thelog) }
    nums <- log(nums + thepse)/log(thelog)
  }
  if ( myregex("plus",transmode) ){
    ddtmp <- myregexmatches("plus[[:digit:].]*",transmode,"plus")
    transmode <- ddtmp[[2]]
    theplus <- ddtmp[[1]]
    if (nchar(theplus) == 0){
      theplus = 0
    } else { theplus <- as.numeric(theplus) }
  } else { theplus = 0 }
  if ( myregex("minus",transmode) ){
    ddtmp <- myregexmatches("minus[[:digit:].]*",transmode,"minus")
    transmode <- ddtmp[[2]]
    theminus <- ddtmp[[1]]
    if (nchar(theminus) == 0){
      theminus = 0
    } else { theminus <- as.numeric(theminus) }
  } else { theminus = 0 }
  if ( myregex("multi",transmode) ){
    ddtmp <- myregexmatches("multi[[:digit:].]*",transmode,"multi")
    transmode <- ddtmp[[2]]
    themulti <- ddtmp[[1]]
    if (nchar(themulti) == 0){
      themulti = 1
    } else { themulti <- as.numeric(themulti) }
  } else { themulti = 1 }
  xx <- (nums + theplus - theminus) * themulti * factor
  if ( myregex("rev",transmode) ){
    maxxx <- max(xx)
    xx <- maxxx - xx
  }
  xx
}

splitoverlap <- function(tocheck,goodlist,sep=",")
{
  sapply(tocheck,function(x)any(unlist(strsplit(x,sep)) %in% goodlist))
}

extendcolorbrewer <- function(colchoice,n){
  allc <- c()
  for (cc in colchoice){
    allc <- c(allc, rgb(t(col2rgb(cc)),maxColorValue=255))
  }
  #library(plotrix); #sapply(rainbow(4), color.id); # Hex to name
  for (ifcbcolor in c(1,0)){
    tmp <- brewer.pal.info[brewer.pal.info$colorblind == ifcbcolor,]
    for (grp in c("qual","seq","div")){
      tmp0 <- tmp[tmp$category == grp,]
      for (irow in 1:nrow(tmp0)){
        tmp00 <- tmp0[irow,]
        for (ccc in brewer.pal(tmp00$maxcolors,row.names(tmp00))){
          if (!ccc %in% allc){
            allc <- c(allc,ccc)
          }
        }
        if (length(allc) >= n){
          return(allc[1:n])
        }
      }
    }
  }
}

colorloupe <- c("grey","#ff799b","#ff8078","#fe8c5a","#e89a42","#caa736","#a8b23b","#81ba50","#4dc06e","#00c491","#00c5b6","#00c5da","#00c2f8","#00bcff","#00b3ff","#7ca7ff","#ba98ff")
colorloupe <- extendcolorbrewer(colorloupe,50)
palette_colors0 <- reactiveVal(colorloupe)
palette_fills0 <- reactiveVal(colorloupe)
palette_shapes0 <- reactiveVal(c(1,2,3,4,0,5,19, 21, 17, 24, 15, 22, 6:30))

ui <- fluidPage(
  headerPanel("ggalluvial GUI"),
  sidebarPanel(width = 2,
               conditionalPanel(
                 condition = "input.tabs=='Data upload'",
                 h4("Data upload"),
                 radioButtons(
                   "data_input", "",
                   choices = if (is.data.frame(dataset)) {
                     list("Load example data" = 1,
                          "Upload text file" = 2,
                          "Paste data" = 3,
                          "Iris example Data" = 4)
                   } else {
                     list("Load sample data" = 1,
                          "Upload file" = 2,
                          "Paste data" = 3)
                   },
                   selected = if (is.data.frame(dataset)) 1 else 4),
                 conditionalPanel(
                   condition = "input.data_input=='1'",
                   h5("Vaccina")
                 ),
                 conditionalPanel(
                   condition = "input.data_input=='2'",
                   h5("Upload file: "),
                   fileInput("upload", "", multiple = FALSE),
                   selectInput("file_type", "Type of file:",
                               list("text (csv|tsv)" = "text",
                                    "Excel" = "Excel",
                                    "SPSS" = "SPSS",
                                    "Stata" = "Stata",
                                    "SAS" = "SAS"),
                               selected = "text"),
                   conditionalPanel(
                     condition = "input.file_type=='text'",
                     selectInput("upload_delim", "Delimiter:",
                                 list("Tab" = "\t",
                                      "Comma" = ",",
                                      "Space" = " ",
                                      "Semicolon" = ";"),
                                 selected = "Space"),
                     selectInput("upload_dec", "Decimal mark:",
                                 list("Point" = ".",
                                      "Comma" = ","),
                                 selected = "Comma")
                   ),
                   conditionalPanel(
                     condition = "input.file_type=='Excel'",
                     htmlOutput("excel_sheet_choices")
                   ),
                   actionButton("submit_datafile_button",
                                "Submit datafile")
                 ),
                 conditionalPanel(
                   condition = "input.data_input=='3'",
                   h5("Paste data below:"),
                   tags$textarea(id = "data_paste",
                                 placeholder = "Add data here",
                                 rows = 10,
                                 cols = 20, "\n "),
                   actionButton("submit_data_button", "Submit data"),
                   selectInput("text_delim", "Delimiter:",
                               list("Semicolon" = ";",
                                    "Tab" = "\t",
                                    "Comma" = ",",
                                    "Space" = " "),
                               selected = "Space"),
                   selectInput("text_dec", "Decimal mark:",
                               list("Comma" = ",",
                                    "Point" = "."),
                               selected = "Comma")
                 )
               ),
               conditionalPanel(
                 condition = "input.tabs=='ggplot'",
                 h4("Create visualization"),
                 conditionalPanel(
                   condition = "input.tabs=='ggplot'",
                   selectInput(inputId = "Type",
                               label = "Type of graph:",
                               choices = c("Alluvial"),
                               selected = "Alluvial")
                 ),
                 selectInput("x_var", "X-variable", choices = ""),
                 selectInput("y_var", "Y-variable", choices = ""),
                 selectInput("id_var", "ID-variable", choices = ""),
                 selectInput("stratum_var", "alluvium-variable", choices = ""),
                 #selectInput("group", "Column to Colour", choices = "."),
                 conditionalPanel(
                   condition = "input.tabs == 'XXXX'",
                   htmlOutput("ggg_group_choices")
                 ),
                 conditionalPanel(
                   condition = "input.Type == 'XXXX'",
                   selectInput("symbol", "Column to Shape", choices = ".")
                 ),
                 conditionalPanel(
                   condition = "input.tabs == 'XXXX'",
                   htmlOutput("ggg_symbol_choices")
                 ),
                 hr(),
                 conditionalPanel(
                   condition = "input.Type == 'XXXXX'",
                   selectInput("col_scatterD3_tolabel", "Column to Label", choices = "")
                 ),
                 conditionalPanel(
                   condition = "input.Type == 'XXXXX'",
                   tags$textarea(id = "scatterD3_tolabel",
                                 placeholder = "Each line is a gene, 'all' for all",
                                 rows = 5,
                                 ""),
                   checkboxInput(inputId = "splitlabelbycomma",
                                 label = strong("SplitComma"),
                                 value = FALSE),
                   actionButton("update_scatterD3_tolabel_button",
                                "Update Labels")
                 ),
                 hr(),
                 conditionalPanel(
                   condition = "input.tabs == 'ggplot'",
                   selectInput("facet_row", "Facet Row", choices = ""),
                   selectInput("facet_col", "Facet Column", choices = "")
                 ),
                 conditionalPanel(
                   # Width of alluvia
                   condition = "input.Type != 'XXXXX'",
                   numericInput("alluvium_width", "Alluvium_width:", min = 0, max = 1, value = 0.33, step=0.1)
                 ),
                 conditionalPanel(
                   condition = "input.Type != 'XXXXX'",
                   sliderInput("alpha", "Opacity:", min = 0, max = 1, value = 0.8)
                 )
               ),
               conditionalPanel(
                 condition = "input.tabs=='Info'",
                 h4("Info")
               ),
               conditionalPanel(
                 condition = "input.tabs=='Stats'",
                 h4("Stats")
               )
  ), # CLOSE sidebarPanel
  h6("Customed by CAB based on",
     a("http://corybrunson.github.io/ggalluvial/,",
       href = "http://corybrunson.github.io/ggalluvial/"),
     a("Check CAB Wiki(DIYTools Section) for Video Tutorial",
       href = "https://wiki.stjude.org/display/CAB/Epigenetics+Group#EpigeneticsGroup-DIYTools:")),
  
  #####################################
  ########### OUPUT TABS ##############
  #####################################
  
  mainPanel(width = 6,
            tabsetPanel(
              type = "tabs",
              tabPanel(HTML("Data<br/>upload"),value="Data upload",
                       mainPanel(
                         column(12,
                                DT::DTOutput("out_table"),
                                downloadButton("download_table",
                                               "Download Table")),
                         hr(),
                         tags$div("Filter data by logical combinations:"),
                         queryBuilder::queryBuilderOutput('querybuilder',width = 800)
                       )
              ),
              tabPanel(HTML("ggplot<br/>(Quick)"),value="ggplot",
                       mainPanel(
                         downloadButton("download_plot_PDF",
                                        "Download pdf"),
                         downloadButton("download_plot_SVG",
                                        "Download svg"),
                         tags$div(
                           style = "position: relative;",
                           plotOutput("alluvial_plot", height = "650px", 
                                      hover = hoverOpts(id = "plot_hover")
                           ),
                           htmlOutput("tooltip"))
                       ) # CLOSE mainPanel
              ),
              tabPanel("Info",
                       tabsetPanel(
                         type = "tabs",
                         tabPanel("R-code", verbatimTextOutput("out_r_code")),
                         tabPanel(HTML("App<br/>Usage"),value="appusage",
                                  plotlyOutput("stat_plotly")
                         ),
                         tabPanel(HTML("Wish<br/>List"),value="wishlist",
                                  h3(
                                    p(
                                      a("Suggest Feature", target="_blank", href = "https://docs.google.com/forms/d/e/1FAIpQLSfJvKYX4cIJyJpCYtXj6kptJU0POGWB1Yz6ZTtrEu0d6D-KsQ/viewform")
                                    )
                                  ),
                                  hr(),
                                  plotlyOutput("thesurvey_plotly"),
                                  hr(),
                                  htmlOutput("thesurvey"),
                                  hr(),
                                  htmlOutput("survey_suggested")
                         ),
                         tabPanel(HTML("Notes<br/>(FAQs)"),value="faqs",
                                  h3("Notes:")
                         ),
                         tabPanel(HTML("Other<br/>Info"),value="otherinfo",
                                  h3("Background")
                         ),
                         tabPanel(HTML("Try"),value="try",
                                  HTML("<iframe width='100%' height='800' src='https://rdrr.io/snippets/embed/?code=%23%20You%20can%20run%20any%20R%20code...%0Aprint(%22Hello%20from%20CAB%22)%0A%0A%23%20Even%20packages%20like%20ggplot!%0Alibrary(ggplot2)%0Aggplot(mtcars%2C%20aes(wt%2C%20mpg%2C%20colour%20%3D%20factor(cyl)))%20%2B%20geom_point()%0A' frameborder='0'></iframe>")
                         )
                       )
              ),
              id = "tabs"
            ) # CLOSE tabsetPanel
  ), # CLOSE mainPanel
  
  #####################################
  ######### AESTHETICS TAB ############
  #####################################
  
  conditionalPanel(
    condition = "input.tabs == 'ggplot'",
    sidebarPanel(
      width = 4,  
      h4("Change aesthetics"),
      tabsetPanel(
        tabPanel(
          "Text",
          checkboxInput(inputId = "label_axes",
                        label = strong("Change labels axes"),
                        value = FALSE),
          conditionalPanel(
            condition = "input.label_axes == true",
            textInput("lab_x", "X-axis:", value = "label x-axis")
          ),
          conditionalPanel(
            condition = "input.label_axes == true",
            textInput("lab_y", "Y-axis:", value = "label y-axis")
          ),
          conditionalPanel(
            condition = "input.tabs != 'scatterD3'",
            checkboxInput(inputId = "add_title",
                          label = strong("Add title"),
                          value = FALSE)
          ),
          conditionalPanel(
            condition = "input.add_title == true && input.tabs != 'scatterD3'",
            textInput("title", "Title:", value = "Title")
          ),
          conditionalPanel(
            condition = "input.tabs == 'XXXXX'",
            checkboxInput(inputId = "add_scatterD3_tolabel",
                          label = strong("Add Label"),
                          value = FALSE)
          ),
          conditionalPanel(
            condition = "input.add_scatterD3_tolabel == true",
            fluidRow(
              column(4,
                     numericInput("genelabelsize",
                                  "Label Font Size:",
                                  value = 6, min = 0),
                     tags$div(title="larger values more space between label and point",
                              numericInput("genelabelforce",
                                           "force:",
                                           value = 0, step = 1, min = 0)
                     ),
                     numericInput("genelabelhjust",
                                  tags$a(href="https://ggrepel.slowkow.com/articles/examples.html",
                                         target="_blank",
                                         "hjust:"),
                                  value = -0.5, step = 0.5),
                     numericInput("genelabelvjust",
                                  tags$a(href="https://ggrepel.slowkow.com/articles/examples.html",
                                         target="_blank",
                                         "vjust:"),
                                  value = 0, step = 0.5)
              ),
              column(4,
                     tags$div(title="smaller values might be more arrow",
                              numericInput("genelabelpoint.padding",
                                           tags$a(href="https://ggrepel.slowkow.com/articles/examples.html",
                                                  target="_blank",
                                                  "point.padding:"),
                                           value = 0, step = 0.2, min = 0)
                     ),
                     tags$div(title="larger values more space between labels",
                              numericInput("genelabelbox.padding",
                                           tags$a(href="https://ggrepel.slowkow.com/articles/examples.html",
                                                  target="_blank",
                                                  "box.padding:"),
                                           value = 0, step = 0.2, min = 0)
                     )
              ),
              column(4,
                     colourpicker::colourInput("genelabelcolor",
                                 tags$a(href="http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/",
                                        target="_blank",
                                        "Color of Label:"),
                                 value = "red"),
                     colourpicker::colourInput("genelabelcolordot",
                                 tags$a(href="http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/",
                                        target="_blank",
                                        "Color of Dot:"),
                                 value = "red")
                     
              )
            ),
            fluidRow(
              column(6,
                     selectInput("genelabeldirection","Repel Direction",
                                 choices = c("both",
                                             "x",
                                             "y"),
                                 selected = "both")
              )
            )
          ),
          checkboxInput(inputId = "adj_fnt_sz",
                        label = strong("Change font size"),
                        value = FALSE),
          conditionalPanel(
            condition = "input.adj_fnt_sz == true",
            numericInput("fnt_sz_ttl",
                         "Size axis titles:",
                         value = 30, min=1, step=2),
            numericInput("fnt_sz_ax",
                         "Size axis labels:",
                         value = 20, min=1, step=2)
          ),
          checkboxInput(inputId = "rot_txt",
                        label = strong("Rotate text x-axis"),
                        value = FALSE),
          conditionalPanel(
            condition = "input.rot_txt",
            textInput("rot_txt_x_degree", "Degree to rotate x-axis", value = "45")
          ),
          checkboxInput(inputId = "adj_fnt",
                        label = strong("Change font"),
                        value = FALSE),
          conditionalPanel(
            condition = "input.adj_fnt == true",
            selectInput("font", "Font",
                        choices = c("Courier",
                                    "Helvetica",
                                    "Times"),
                        selected = "Helvetica")
          )
        ),
        tabPanel(
          "Scale",
          checkboxInput(inputId = "scale_x_log",
                        label = strong("Scale X log10"),
                        value = FALSE),
          checkboxInput(inputId = "scale_y_log",
                        label = strong("Scale Y log10"),
                        value = FALSE),
          tags$hr(),
          textInput("scale_x_lim", "X limits: min, max(NA means not limit)", value = ""),
          textInput("scale_y_lim", "Y limits: min, max(NA means not limit)", value = ""),
          tags$hr(),
          textInput("transform_x", "Transform X-axis:\n neg|log2|log10|pse0.00001|plus0|minus0|multi1|ori|rev", value = ""),
          textInput("transform_y", "Transform Y-axis:\n neglog10pse0.000001 for Volcano", value = ""),
          actionButton("update_transform_xy",
                       "Transform!"),
          tags$hr(),
          checkboxInput("scatterD3_ellipses", strong("Confidence ellipses"), value = FALSE)
        ),
        tabPanel(
          "Theme",
          conditionalPanel(
            condition = "input.group != '.'",
            checkboxInput(inputId = "adj_col",
                          label = strong(tags$a(href="http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/",
                                                target="_blank",
                                                "Change colour palette")),
                          value = FALSE),
            conditionalPanel(
              condition = "input.adj_col",
              selectInput(inputId = "palet",
                          label = strong("Select palette"),
                          choices = list(
                            "Qualitative" = c("Accent",
                                              "Dark2",
                                              "Paired",
                                              "Pastel1",
                                              "Pastel2",
                                              "Set1",
                                              "Set2",
                                              "Set3"),
                            "Diverging" = c("BrBG",
                                            "PiYG",
                                            "PRGn",
                                            "PuOr",
                                            "RdBu",
                                            "RdGy",
                                            "RdYlBu",
                                            "RdYlGn",
                                            "Spectral"),
                            "Sequential" = c("Blues",
                                             "BuGn",
                                             "BuPu",
                                             "GnBu",
                                             "Greens",
                                             "Greys",
                                             "Oranges",
                                             "OrRd",
                                             "PuBu",
                                             "PuBuGn",
                                             "PuRd",
                                             "Purples",
                                             "RdPu",
                                             "Reds",
                                             "YlGn",
                                             "YlGnBu",
                                             "YlOrBr",
                                             "YlOrRd")),
                          selected = "Set1")
            )
          ),
          conditionalPanel(
            condition = "input.jitter",
            checkboxInput("adj_jitter",
                          strong("Change look jitter"), FALSE),
            conditionalPanel(
              condition = "input.adj_jitter",
              textInput("col_jitter", "Colour (name or RGB):",
                        value = "black"),
              numericInput("size_jitter", "Size:", value = 1),
              sliderInput("opac_jitter", "Opacity:",
                          min = 0, max = 1, value = 0.5, step = 0.01),
              sliderInput("width_jitter", "Width jitter:",
                          min = 0, max = 0.5, value = 0.25, step = 0.01)
            )
          ),
          checkboxInput("adj_grd",
                        strong("Remove gridlines"), FALSE),
          conditionalPanel(
            condition = "input.adj_grd",
            checkboxInput("grd_maj",
                          strong("Remove major gridlines"), FALSE),
            checkboxInput("grd_min",
                          strong("Remove minor gridlines"), FALSE)
          ),
          selectInput("theme", "Theme",
                      choices = c("bw" = "theme_bw()",
                                  "classic" = "theme_classic()",
                                  "dark" = "theme_dark()",
                                  "grey" = "theme_grey()",
                                  "light" = "theme_light()",
                                  "line_draw" = "theme_linedraw()",
                                  "minimal" = "theme_minimal()"),
                      selected = "theme_bw()"),
          #### Shape, color, fill UI ####
          fluidRow(
            column(4,
                   numericInput("pt.size",
                                "Size of Shape:",
                                value = 2, min=0, step=1)),
            column(4,
                   numericInput("line.size",
                                "width of Line:",
                                value = 0.5, min=0, step=0.5)),
            column(4,
                   selectInput("line.type",
                               tags$a(href="http://sape.inf.usi.ch/quick-reference/ggplot2/linetype",
                                      target="_blank",
                                      "Type of Line:"),
                               choices = c("blank", "solid", "dashed", "dotted", 
                                           "dotdash", "longdash", "twodash", 
                                           "1F", "F1", "4C88C488", "12345678"),
                               selected = "dotted"))
          ),
          fluidRow(
            column(4, h4(tags$a(href="https://ggplot2.tidyverse.org/reference/scale_shape.html",
                                target="_blank",
                                "Shapes")), uiOutput("custom_shapes")),
            column(4, h4(tags$a(href="http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/",
                                target="_blank",
                                "Colors")), uiOutput("custom_colors")),
            column(4, h4(tags$a(href="http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/",
                                target="_blank",
                                "Fills")), uiOutput("custom_fills"))
          ),
          fluidRow(
            column(4,actionButton("read_custom_shapes","ReadCurrent"),actionButton("update_custom_shapes","Update")),
            column(4,actionButton("read_custom_colors","ReadCurrent"),actionButton("update_custom_colors","Update")),
            column(4,actionButton("read_custom_fills","ReadCurrent"),actionButton("update_custom_fills","Update"))
          )
        ),
        tabPanel(
          "Legend",
          conditionalPanel(
            condition = "input.group != '.'",
            radioButtons(inputId = "adj_leg",
                         label = NULL,
                         choices = c("Keep legend as it is",
                                     "Remove legend",
                                     "Change legend"),
                         selected = "Keep legend as it is"),
            conditionalPanel(
              condition = "input.adj_leg=='Change legend'",
              textInput("leg_ttl", "Title legend:",
                        value = "title legend"),
              selectInput("pos_leg", "Position legend",
                          choices = c("right",
                                      "left",
                                      "top",
                                      "bottom"))
            )
          )
        ),
        tabPanel(
          "Size",
          checkboxInput("fig_size",
                        strong("Adjust plot size"), FALSE),
          conditionalPanel(
            condition = "input.fig_size",
            #TODO checkboxInput("fig_size_lockratio",
            #              strong("Lock.Ratio"), TRUE),
            numericInput("fig_height", "Plot height (#pixels): ",
                         value = 640, min = 0, step = 100),
            numericInput("fig_width", "Plot width (#pixels):", value = 640, min = 0, step = 100)
          )
        ),
        tabPanel(
          "Extra",
          checkboxInput("addline",
                        strong("Add Line between dots"), FALSE),
          checkboxInput("addabline",
                        strong("Add Reference Lines"), FALSE),
          conditionalPanel(
            condition = "input.addabline",
            fluidRow(
              column(4,
                     textInput("abline_x_intercept", 
                               tags$a(href="https://ggplot2.tidyverse.org/reference/geom_abline.html",
                                      target="_blank","x intercepts(-1,1)"),
                               value = "-1,1")),
              column(4,
                     textInput("abline_y_intercept", 
                               tags$a(href="https://ggplot2.tidyverse.org/reference/geom_abline.html",
                                      target="_blank","y intercepts(-1,1)"),
                               value = ""))
            ),
            fluidRow(
              column(8,
                     textInput("abline_xy_slopeintercept", 
                               tags$a(href="https://ggplot2.tidyverse.org/reference/geom_abline.html",
                                      target="_blank","slope,intercept(1,0) -> y ~ slope * x + intercept"),
                               value = ""))
            )
          )
        )
      ) # Close tabsetPanel
    ) # Close sidebarPanel
  ) # Close conditionalPanel
) # Close fluidPage


server <- function(input, output, session) {
  
  #####################################
  ### GET VARIABLE NAMES FOR INPUT ####
  #####################################
  
  session$allowReconnect(TRUE)
  
  observe({
    datatmp <- df_shiny()
    nms_ori <- names(datatmp)
    nms <- nms_ori
    # Make list of variables that are not factors
    nms_cont <- names(Filter(function(x) is.integer(x) ||
                               is.numeric(x) ||
                               is.double(x),
                             datatmp))
    
    # Make list of variables that are factors
    max_nms_fact <- MAX_UNIQUE_VAL_AS_FACTOR
    nms_fact <- names(Filter(function(x) length(unique(x)) < max_nms_fact,
                             datatmp))
    
    nms_for_lab <- c(nms[!nms %in% nms_cont], nms_cont, "rowname", "No groups" = "." )

    avail_all <- c(nms, "No groups" = ".")
    avail_con <-
      if (identical(nms_cont, character(0)))
        c("No continuous vars available" = ".")
    else c(nms_cont)
    avail_fac <-
      if (identical(nms_fact, character(0)))
        c("No factors available" = ".")
    else c("No groups" = ".", nms_fact)
    
    updateSelectInput(session, "x_var", choices = nms, selected = nms[1])
    updateSelectInput(session, "y_var", choices = nms, selected = nms[2])
    updateSelectInput(session, "id_var", choices = nms, selected = nms[3])
    updateSelectInput(session, "stratum_var", choices = nms, selected = nms[4])
    updateSelectInput(session, "group", choices = avail_fac)
    updateSelectInput(session, "symbol", choices = avail_fac)
    updateSelectInput(session, "col_scatterD3_tolabel", choices = nms_for_lab)
    updateSelectInput(session, "facet_row",  choices = avail_fac)
    updateSelectInput(session, "facet_col",  choices = avail_fac)
  })
  
  
  #####################################
  ###### READ IN / GET DATA ###########
  #####################################
  output$excel_sheet_choices <- renderUI({
    if (is.null(input$upload)){
      thesheets0 <- c()
    } else {
      thesheets0 <- excel_sheets(input$upload$datapath)  
    }
    thesheets <- 1:length(thesheets0)
    names(thesheets) <- thesheets0
    if (length(thesheets) > 0){
      ggg_group_choices <- selectInput('excel_sheet_choices', label = "Sheet to read",
                                       choices = thesheets, multiple = F, selectize = T, selected = thesheets[1]) 
    } else {
      ggg_group_choices <- NULL
    }
    return(ggg_group_choices)
  })
  
  df_shiny <- reactive({
    if (input$data_input == 1) {
      data(vaccinations)
      data <- transform(vaccinations,
                        response = factor(response, rev(levels(response))))
      data$test <- 1:nrow(data)
      data$test <- ifelse(data$test %% 2 == 1,"Odd","Even")
    } else if (input$data_input == 2) {
      file_in <- input$upload
      # Avoid error message while file is not uploaded yet
      if (is.null(input$upload)) {
        return(data.frame(x = "Select your datafile"))
      } else if (input$submit_datafile_button == 0) {
        return(data.frame(x = "Press 'submit datafile' button"))
      } else {
        isolate({
          if (input$file_type == "text") {
            data <- as.data.frame(read_delim(file_in$datapath,
                                             delim = input$upload_delim,
                                             locale = locale(decimal_mark = input$upload_dec),
                                             col_names = TRUE, comment = '#'))
          } else if (input$file_type == "Excel") {
            data <- as.data.frame(read_excel(file_in$datapath,as.integer(input$excel_sheet_choices)))
          } else if (input$file_type == "SPSS") {
            data <- as.data.frame(read_sav(file_in$datapath))
          } else if (input$file_type == "Stata") {
            data <- as.data.frame(read_dta(file_in$datapath))
          } else if (input$file_type == "SAS") {
            data <- as.data.frame(read_sas(file_in$datapath))
          }
        })
      }
    } else if (input$data_input == 3) {
      if (input$data_paste == "") {
        data <- data.frame(x = "Copy your data into the textbox,
                           select the appropriate delimiter, and
                           press 'Submit data'")
      } else {
        if (input$submit_data_button == 0) {
          return(data.frame(x = "Press 'submit data' button"))
        } else {
          isolate({
            data <- as.data.frame(read_delim(input$data_paste,
                                             delim = input$text_delim,
                                             locale = locale(decimal_mark = input$text_dec),
                                             col_names = TRUE))
          })
        }
      }
    } else if (input$data_input == 4){
      data <- dataset
    }
    names(data) <- gsub(" ","_",names(data))
    for(nn in names(data))
    {
      ntot <- nrow(data)
      dtmp <- as.numeric(data[[nn]])
      if(sum(is.na(as.numeric(as.character(data[[nn]])))) < ntot / 20){
        data[[nn]] <- dtmp
      }
    }
    return(data)
  })
  
  df_label_shiny <- reactive({
    if (input$update_scatterD3_tolabel_button == 0) {
      df_label_shiny_tmp <- c("NONE")
      df_label_list <- df_label_shiny_tmp
    } else {
      isolate({
        df_label_shiny_tmp <- read_table(paste(input$scatterD3_tolabel,"NONE",sep="\n"),col_names = FALSE)
      })
      df_label_list <- as.data.frame(df_label_shiny_tmp)[,1]
      if (input$splitlabelbycomma)
      {
        df_label_list <- unique(unlist(sapply(df_label_list,function(x) unlist(strsplit(x,",")))))
      }
      df_label_list <- df_label_list[df_label_list != "."]
    }
    df <- df_trans_shiny()
    if (! input$col_scatterD3_tolabel %in% c("None",".",NULL) && df_label_shiny_tmp[1] != "NONE"){
      dlab <- as.character(df[,input$col_scatterD3_tolabel,drop=TRUE])      
      if (df_label_list[1] == "all"){
        df_label <- dlab
      } else {
        df_label <- rep("",length(dlab))
        if (input$splitlabelbycomma){
          xxtmp <- splitoverlap(toupper(dlab),toupper(df_label_list),sep=",")
          df_label[xxtmp] <- dlab[xxtmp]
        } else {
          xxtmp <- toupper(dlab) %in% toupper(df_label_list) 
          df_label[xxtmp] <- dlab[xxtmp]  
        }
      }
    } else {
      df_label <- ""
      dlab <- ""
    }
    df_label_nospace <- gsub(" ","_",dlab)
    list(df_label,df_label_nospace,dlab)
  })
  
  transform_x_var <- eventReactive(input$update_transform_xy,{
    input$transform_x
  },ignoreNULL = FALSE)
  
  transform_y_var <- eventReactive(input$update_transform_xy,{
    input$transform_y
  },ignoreNULL = FALSE)
  
  df_trans_shiny0 <- reactive({
    data <- df_shiny()
    
    if (!is.null(transform_x_var()) && transform_x_var() != ""){
      new_xvar <- paste(transform_x_var(),input$x_var,sep="_")
      tmp <- transvaluebypara(data[[input$x_var]],transform_x_var())
      data[[new_xvar]] <- tmp
    } else {
      new_xvar <- input$x_var
      tmp <- NULL
    }
    
    if (!is.null(transform_y_var()) && transform_y_var() != ""){
      new_yvar <- paste(transform_y_var(),input$y_var,sep="_")
      tmp <- transvaluebypara(data[[input$y_var]],transform_y_var())
      data[[new_yvar]] <- tmp
    } else {
      new_yvar <- input$y_var
      tmp <- NULL
    }
    
    if(input$col_scatterD3_tolabel %in% c("rowname")){
      data[["rowname"]] <- row.names(data)
    }
    
    # Make list of variables that are factors
    max_nms_fact <- MAX_UNIQUE_VAL_AS_FACTOR
    nms_fact <- names(Filter(function(x) length(unique(x)) < max_nms_fact,
                             data))
    
    for (i in nms_fact){
      data[,i] <- factor(data[,i],levels=unique(data[,i]))
    }
    data
  })
  
  output$querybuilder <- renderQueryBuilder({
    queryBuilder(data = df_trans_shiny0(), 
                 autoassign = TRUE,
                 default_condition = 'OR',
                 allow_empty = FALSE,
                 display_errors = FALSE,
                 display_empty_filter = FALSE,
                 chosen = TRUE
    )
  })
  
  df_trans_shiny <- reactive({
    data <- df_trans_shiny0()
    if (!is.null(input$querybuilder_out) && length(input$querybuilder_out$rules)){
      data <- as.data.frame(filterTable(input$querybuilder_out, data, 'table'))
    }
    data
  })

  #####################################
  ####### CREATE GRAPH-CODE ###########
  #####################################
  
  string_code <- reactive({
    
    new_xvar <- input$x_var
    new_yvar <- input$y_var
    
    if (transform_x_var() != ""){
      new_xvar <- paste(transform_x_var(),input$x_var,sep="_")
    }
    
    if (transform_y_var() != ""){
      new_yvar <- paste(transform_y_var(),input$y_var,sep="_")
    }
    
    if (input$x_var == "NA_Var"){
      new_xvar <- new_yvar
    } else if (input$y_var == "NA_Var"){
      new_yvar <- new_xvar
    }
    
    p <- glue("ggplot(df, aes(x = {input$x_var}, y = {input$y_var},
    stratum = {input$stratum_var}, alluvium = {input$id_var},
                fill = {input$stratum_var})) +
    geom_stratum(alpha = {input$alpha},width = {input$alluvium_width}) +
    geom_text(aes(label = {input$stratum_var}), stat = 'stratum', size = rel(5)) + 
    geom_flow(stat = 'alluvium', lode.guidance = 'forward', width = {input$alluvium_width}) + theme_bw()")
    
    # if at least one facet column/row is specified, add it
    facets <- paste(input$facet_row, "~", input$facet_col)
    if (facets != ". ~ .")
      p <- glue("{p} + facet_grid({facets}, scales = 'free_y')")
    
    # if labels specified
    if (input$label_axes)
      p <- glue("{p} + labs(x = '{input$lab_x}', y = '{input$lab_y}') ")
    
    if (input$scale_x_log)
      p <- glue("{p} + scale_x_log10() ")
    if (input$scale_y_log)
      p <- glue("{p} + scale_y_log10() ")
    if (nchar(gsub(" ","",input$scale_x_lim))){
      p <- glue("{p} + xlim({input$scale_x_lim})")
    }
    if (nchar(gsub(" ","",input$scale_y_lim))){
      p <- glue("{p} + ylim({input$scale_y_lim})")
    }
    # if title specified
    if (input$add_title)
      p <- glue("{p} + ggtitle('{input$title}')")
    
    gg_fil <- 1
    gg_fil_txt <- if (gg_fil) "fill" else "colour"
    # if legend specified
    if (input$adj_leg == "Change legend"){
      p <- glue("{p} + labs({gg_fil_txt} = '{input$leg_ttl}')")
    }
    
    # if colour legend specified
    if (input$adj_col)
      p <- glue("{p} + scale_{gg_fil_txt}_brewer(palette = '{input$palet}')")
    
    # If theme features are specified
    if (input$adj_fnt_sz ||
        input$adj_fnt ||
        input$rot_txt ||
        input$adj_leg != "Keep legend as it is" ||
        input$adj_grd) {
      p <- paste(
        p,
        paste(
          " + theme(\n    ",
          if (input$adj_fnt_sz)
            glue("axis.title = element_text(size = {input$fnt_sz_ttl}),\n    "),
          if (input$adj_fnt_sz)
            glue("axis.text = element_text(size = {input$fnt_sz_ax}),\n    "),
          if (input$adj_fnt)
            glue("text = element_text(family = '{input$font}'),\n    "),
          if (input$rot_txt)
            glue("axis.text.x = element_text(angle = {input$rot_txt_x_degree}, hjust = 1),\n    "),
          if (input$adj_leg == "Remove legend")
            "legend.position = 'none',\n    ",
          if (input$adj_leg == "Change legend")
            glue("legend.position = '{input$pos_leg}',\n    "),
          if (input$grd_maj)
            "panel.grid.major = element_blank(),\n    ",
          if (input$grd_min)
            "panel.grid.minor = element_blank(),\n    ",
          ")",
          sep = ""
        ),
        sep = ""
      )
    }
    
    p <- str_replace_all(p, "[ ]+|\n", " ")
    list(p,new_xvar,new_yvar)
  })

  output$out_table <- DT::renderDT(
    df_trans_shiny(),
    filter = "top",
    options = list(pageLength = 5)
  )
  
  width_download <- reactive ({ input$fig_width })
  height_download <- reactive ({ input$fig_height })
  
  #####################################
  #### GENERATE R-CODE FOR OUTPUT #####
  #####################################
  output$download_table <- downloadHandler(
    filename <- function() {
      paste("Data_SJCAB_", sub(" ","-",Sys.time()), ".csv", sep = "")
    },
    content <- function(file) {
      data <- df_trans_shiny()
      write.table(data.frame("Row"=row.names(data),data,check.names = FALSE),file,na = "",sep=",",row.names = FALSE)
    }
  )
  
  output$download_plot_PDF <- downloadHandler(
    filename <- function() {
      paste("Figure_SJCAB_", sub(" ","-",Sys.time()), ".pdf", sep = "")
    },
    content <- function(file) {
      p <- p_for_out_ggplot()
      ggsave(file, p, width = width_download()/60,
             height = height_download()/60, units = "in", dpi=300)
    },
    contentType = "application/pdf" # MIME type of the image
  )
  
  output$download_plot_SVG <- downloadHandler(
    filename <- function() {
      paste("Figure_SJCAB_", sub(" ","-",Sys.time()), ".svg", sep = "")
    },
    content <- function(file) {
      p <- p_for_out_ggplot()
      ggsave(file, p, width = width_download()/60,
             height = height_download()/60, units = "in", dpi=300)
    },
    contentType = "application/svg" # MIME type of the image
  )
  
  p_for_out_ggplot <- reactive({
    # evaluate the string RCode as code
    df <- df_trans_shiny()
    tmp <- df_label_shiny()
    df$df_label <- tmp[[1]]
    df$df_label_nospace <- tmp[[2]]
    ssss <- string_code()[[1]]

    print(ssss)
    #browser()
    p <- eval(parse(text = ssss))
    p
  })
  
  output$alluvial_plot <- renderPlot(width = width_download,
                                  height = height_download, 
                                  {
                                    p <- p_for_out_ggplot()
                                    p
                                  })

  re_node_width <- reactive ({ input$alluvium_width })

  # Display tooltip
  output$tooltip <- renderText({
    node_width <- re_node_width()
    offset <- 20
    alluvium_width <- node_width
    
    p <- p_for_out_ggplot()
    
    pbuilt <- ggplot_build(p)

    # Use built plot data to recalculate the locations of the flow polygons:
    
    # Add width parameter, and then convert built plot data to xsplines
    data_draw <- transform(pbuilt$data[[3]], width = alluvium_width)
    
    groups_to_draw <- split(data_draw, data_draw$group) 
    group_xsplines <- lapply(groups_to_draw,
                             data_to_alluvium) 
    
    
    # Convert xspline coordinates to grid object.
    xspline_coords <- lapply(
      group_xsplines,
      function(coords) grid::xsplineGrob(x = coords$x, 
                                         y = coords$y, 
                                         shape = coords$shape, 
                                         open = FALSE)
    )
    
    # Use grid::xsplinePoints to draw the curve for each polygon
    xspline_points <- lapply(xspline_coords, grid::xsplinePoints)
    
    # Define the x and y axis limits in grid coordinates (old) and plot
    # coordinates (new)
    xrange_old <- range(unlist(lapply(
      xspline_points,
      function(pts) as.numeric(pts$x)
    )))
    yrange_old <- range(unlist(lapply(
      xspline_points,
      function(pts) as.numeric(pts$y)
    )))
    xrange_new <- c(1 - alluvium_width/2, max(pbuilt$data[[1]]$x) + alluvium_width/2) 
    yrange_new <- c(0, sum(pbuilt$data[[2]]$count[pbuilt$data[[2]]$x == 1])) 
    
    # Define function to convert grid graphics coordinates to data coordinates
    new_range_transform <- function(x_old, range_old, range_new) {
      (x_old - range_old[1])/(range_old[2] - range_old[1]) *
        (range_new[2] - range_new[1]) + range_new[1]
    }
    
    # Using the x and y limits, convert the grid coordinates into plot coordinates. 
    polygon_coords <- lapply(xspline_points, function(pts) {
      x_trans <- new_range_transform(x_old = as.numeric(pts$x), 
                                     range_old = xrange_old, 
                                     range_new = xrange_new)
      y_trans <- new_range_transform(x_old = as.numeric(pts$y), 
                                     range_old = yrange_old, 
                                     range_new = yrange_new)
      list(x = x_trans, y = y_trans)
    })
    
    if(!is.null(input$plot_hover)) {
      hover <- input$plot_hover
      x_coord <- round(hover$x)
      
      if(abs(hover$x - x_coord) < (node_width / 2)) {
        # Display node information if cursor is over a stratum box.
        
        # Determine stratum name from x and y coord, and the n.
        node_row <- pbuilt$data[[2]]$x == x_coord & 
          hover$y > pbuilt$data[[2]]$ymin & 
          hover$y < pbuilt$data[[2]]$ymax
        node_label <- pbuilt$data[[2]]$stratum[node_row]
        node_n <- pbuilt$data[[2]]$count[node_row]
        #browser()
        
        # Render tooltip
        renderTags(
          tags$div(
            node_label, tags$br(),
            "n =", node_n,
            style = paste0(
              "position: absolute; ",
              "top: ", hover$coords_css$y + offset, "px; ",
              "left: ", hover$coords_css$x + offset, "px; ",
              "background: gray; ",
              "padding: 3px; ",
              "color: white; "
            )
          )
        )$html
      } else {
        # Display flow information if cursor is over a flow polygon: what
        # alluvia does it pass through?
        
        # Calculate whether coordinates of hovering cursor are inside one of the
        # polygons.
        hover_within_flow <- sapply(
          polygon_coords,
          function(pol) point.in.polygon(point.x = hover$x, 
                                         point.y = hover$y, 
                                         pol.x = pol$x, 
                                         pol.y = pol$y)
        )
        if (any(hover_within_flow)) {
          # Find the alluvium that is plotted on top. (last)
          coord_id <- rev(which(hover_within_flow == 1))[1]
          # Find the strata labels and n corresponding to that alluvium in the data.
          flow_label <- paste(groups_to_draw[[coord_id]]$stratum, collapse = ' -> ')
          flow_n <- groups_to_draw[[coord_id]]$count[1]
          
          # Render tooltip
          renderTags(
            tags$div(
              flow_label, tags$br(),
              "n =", flow_n,
              style = paste0(
                "position: absolute; ",
                "top: ", hover$coords_css$y + offset, "px; ",
                "left: ", hover$coords_css$x + offset, "px; ",
                "background: gray; ",
                "padding: 3px; ",
                "color: white; "
              )
            )
          )$html
        }
      }
    }
  })
}

shinyApp(ui = ui, server = server)
