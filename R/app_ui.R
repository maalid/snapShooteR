#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

# shinyManager setLabels ----
shinymanager::set_labels(
  language = "en",
  "Please authenticate" = "",
  "Username:" = "nombre de usuario:",
  "Password:" = "clave de acceso:"
)

# shinyManager credential ----
# credentials <- data.frame(
#   user = c("Luis Vera", "Marcelo Alid"),
#   password = c("lvera1234", "maalid1234"),
#   # password will automatically be hashed
#   admin = c(FALSE, TRUE),
#   stringsAsFactors = FALSE
# )
# 
# keyring::key_set("R-shinymanager-key", "obiwankenobi")
# shinymanager::create_db(
#   credentials_data = credentials,
#   sqlite_path = "inst/app/db/db.sqlite", # will be created
#   passphrase = key_get("R-shinymanager-key", "obiwankenobi")
#   # passphrase = "passphrase_wihtout_keyring"
# )

# Global parameters ----
cameraWidth <- 224
cameraHeight <- 224
cameraQuality <- 100
brightness_low  <- 110
saturation_low <- 110
brightness_high  <- 120
saturation_high <- 120
# lista_articulos <- readr::read_csv("R/Lista_Ejemplo.csv")

# Funcion showWebcam1 ----
showWebcam <- function(cameraWidth, cameraHeight, cameraQuality){
  paste8 <- function(..., sep = " ", collapse = NULL) {
    args <- c(
      lapply(list(...), enc2utf8),
      list(
        sep = if (is.null(sep)) sep else enc2utf8(sep),
        collapse = if (is.null(collapse)) collapse else enc2utf8(collapse)
      )
    )
    do.call(paste, args)
  }
  tags$div(
    tags$script(HTML(paste8(readLines(system.file("app/js/webcam.min.js", package = "snapShooteR"), warn = FALSE, encoding = "UTF-8"), collapse = "\r\n")))
    ,
    HTML(paste0('
        <div id="my_camera"></div>
        
        <script language="JavaScript">
         Webcam.set({
         width: ',(224/224)*cameraWidth,',
         height: ',(224/224)*cameraHeight,',
	     dest_width: ',cameraWidth,',
	     dest_height: ',cameraHeight,',
         image_format: \'jpeg\',
         jpeg_quality: ',cameraQuality,',
         flip_horiz: true
         });
         Webcam.on("init", function () {
        Webcam.getCameras(function (cameras) {
          if (cameras.length > 0) {
            Webcam.setAndInitCamera(cameras[cameras.length - 2].id);
          }
        });
      });
         Webcam.attach( \'#my_camera\', true);
         </script>
         '))
  )
}

# Funcion WebcamOff ----
webCamOff <- function(){
  tags$div(HTML(paste0('<script language="JavaScript">
         
         Webcam.reset(\'#my_camera\', true);
           
         </script>')))
}

# Funcion showWebcam2 ----
showWebcam2 <- function(cameraWidth, cameraHeight, cameraQuality){
  paste8 <- function(..., sep = " ", collapse = NULL) {
    args <- c(
      lapply(list(...), enc2utf8),
      list(
        sep = if (is.null(sep)) sep else enc2utf8(sep),
        collapse = if (is.null(collapse)) collapse else enc2utf8(collapse)
      )
    )
    do.call(paste, args)
  }
  tags$div(
    tags$script(HTML(paste8(readLines(system.file("app/js/webcam2.min.js", package = "snapShooteR"), warn = FALSE, encoding = "UTF-8"), collapse = "\r\n"))),
    HTML(paste0('
        <div id="my_camera_2"></div>

        <script language="JavaScript">
         Webcam2.set({
         width: ',(224/224)*cameraWidth,',
         height: ',(224/224)*cameraHeight,',
	     dest_width: ',cameraWidth,',             
	     dest_height: ',cameraHeight,',
         image_format: \'jpeg\',
         jpeg_quality: ',cameraQuality,',
         flip_horiz: true
         });
         
         Webcam2.attach( \'#my_camera_2\');
         </script>
         '))
  )
}

# Funcion Webcam2Off ----
webCam2Off <- function(){
  tags$div(HTML(paste0('<script language="JavaScript">
         
         Webcam2.reset(\'#my_camera_2\');
           
         </script>')))
}

# Funcion closeWindow ----
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# UI ----
app_ui <- function(request) {
  
  # reticulate::py_run_file(system.file("app/python/webcamSettings.py", package = "snapShooteR"))
  # shell("C: & cd C:/Users/Admin/Downloads/webcam-settings-dialog-windows-master & launchCam1.bat")
  tagList(
    
    # Adding external resources ----
    golem_add_external_resources(),
    
    # FluidPage ---- 
    fluidPage(
      theme = shinythemes::shinytheme("cosmo"), 
      shinyjs::useShinyjs(),
      
      # Funcion take_snapshot ----
      HTML('<script language="JavaScript">
         function take_snapshot() {
           // take snapshot and get image data
           Webcam.snap( function(data_uri) {
               document.getElementById(\'imageprev\').src = data_uri;
               Shiny.setInputValue("placeholder64", data_uri)
             } )
             Webcam2.snap( function(data_uri) {
               document.getElementById(\'imageprev2\').src = data_uri;
               Shiny.setInputValue("placeholder642", data_uri)
             } );
           }
         </script>'),
      
      # Funcion take_BurstSnapshot ----
      HTML('<script language="JavaScript">
                    var timer = null;

                    function take_BurstSnapshot(){
                        // take snapshot and get image data
                        Webcam.snap( function(data_uri) {
                            // display results in page
                            var img = new Image();
                            img.src = data_uri;

                            document.getElementById(\'results\').appendChild( img );
                            document.getElementById(\'imageprev\').src = data_uri;
                            Shiny.setInputValue("burstplaceholder64", data_uri)
                        } )
                        Webcam2.snap( function(data_uri) {
                            // display results in page
                            var img2 = new Image();
                            img2.src = data_uri;

                            document.getElementById(\'results2\').appendChild( img2 );
                            document.getElementById(\'imageprev2\').src = data_uri;
                            Shiny.setInputValue("burstplaceholder642", data_uri)
                        } );
                    }

                    function start_snapping() {
                        if (!timer) {
                            take_BurstSnapshot();
                            timer = setInterval( take_BurstSnapshot, 1500 );
                        }
                    }

                    function stop_snapping() {
                        if (timer) {
                            clearTimeout( timer );
                            timer = null;
                        }
                    }

                    function erase_snaps() {
                        document.getElementById(\'results\').innerHTML = \'\';
                        document.getElementById(\'results2\').innerHTML = \'\';
                    }

         </script>'),
      
      # Codigo js para cerrar app ----
      shinyjs::extendShinyjs(text = jscode, functions = c("closeWindow")),
      
        # tags$head(tags$style(HTML("
        #                    .navbar-nav {
        #                    float: none !important;
        #                    }
        #                    .navbar-nav > li:nth-child(2) {
        #                    float: right;
        #                    }
        #                    .navbar-brand {
        #                    width: 400px; 
        #                    font-size:35px; 
        #                    text-align:center;
        #                    }
        #                    
        #                    "))),
      
      
      # Head App ----
      shiny::navbarPage(div(style = "width: 400px; 
                                     font-size:40px; 
                                     font-family: sans serif;
                                     text-align:center;",
                            "snapShooteR ",
                     
                            span(style = "font-size: 18px;
                                          font-family: sans serif;
                                          color: grey;",
                                 "by AiLab UBB")),
                        windowTitle = HTML("snapShooteR"),
                        id = "navbar",
                        
                 
                 # TAB 1: Captura de Imagenes ----
                 shiny::tabPanel("",
                                 shiny::titlePanel("1. Captura de Imagenes"),
                                 shiny::sidebarLayout(
                                   
                                   # Sidebar Panel ----
                                   shiny::sidebarPanel(
                                     
                                     # Boton para elegir directorio de trabajo ----
                                     div(align = "center",
                                         shinyFiles::shinyDirButton(id = "directory", 
                                                                    label = "Seleccione Directorio", 
                                                                    title = "Please select a folder",
                                                                    style = "background-color: black;
                                                                             font-family: sans serif;
                                                                             font-size: 15px;
                                                                             color: white;
                                                                             box-shadow: 5px 5px 5px grey;
                                                                             border-radius: 5px")
                                         ),
                                     
                                     br(),
                                     
                              div(br(style = "line-height: 200px;"),
                                  
                                  align = "center",
                                  
                                  # shinyFiles::shinyDirButton('directory', 'Folder select', 'Please select a folder'),
                                  # 
                                  # br(),
                                  # 
                                  
                                  # Campo para ingresar numero de experimento ----
                                  shinyjs::disabled(shiny::numericInput(inputId = "experimento",
                                                                        label = "Experimento No.", 
                                                                        value = 0,
                                                                        min = 0,
                                                                        width = "95px")),
                                  
                                  # Boton para crear experimento ----
                                  shinyjs::disabled(shiny::actionButton(inputId = "CrearExperimento", 
                                                                        label = "Crear",
                                                                        style = "background-color: black;
                                                                                 font-family: sans serif;
                                                                                 font-size: 15px;
                                                                                 color: white;
                                                                                 box-shadow: 5px 5px 5px grey;
                                                                                 border-radius: 5px",
                                                                        size = "sm")
                                                    ),
                                  
                                  span(style = "font-size: 13px; 
                                                color: black;
                                                text-shadow: 2px 2px 5px black;",
                                       " -- "),
                                  
                                  # Boton para crear nuevo experimento ----
                                  shinyjs::disabled(shiny::actionButton(inputId = "NuevoExperimento", 
                                                                        label = "Nuevo",
                                                                        style = "background-color: black;
                                                                                 font-family: sans serif;
                                                                                 font-size: 15px;
                                                                                 color: white;
                                                                                 box-shadow: 5px 5px 5px grey;
                                                                                 border-radius: 5px",
                                                                        size = "sm")
                                                    )
                                  ),
                                  
                                  hr(style = "box-shadow: 2px 2px 2px blue;"),
                                  
                                  # Botones para escoger typeSet ----
                                  div(align = "center",
                                      
                                      # div(style = "width: 25px;",
                                      
                                      shinyjs::disabled(shiny::radioButtons(inputId = "setType",  # shiny::radioButtons
                                                                            label = "Set",
                                                                            inline = TRUE,
                                                                            choices = c("Entrenamiento" = "train",
                                                                                        "Validacion" = "validation",
                                                                                        "Test" = "test")
                                                                            )
                                                        )
                                      # )
                                      ),
                              
                              hr(style = "box-shadow: 2px 2px 2px blue;"),
                              
                              # Boton para tomar Burst-Snapshot ----
                              div(br(style = "line-height: 200px;"),
                                  
                                  align = "center",
                                  
                                  shinyjs::disabled(shiny::actionButton(inputId = "BurstSnapshot", 
                                                                        label = "",
                                                                        style = "background-color: black; 
                                                                                 font-family: sans serif; 
                                                                                 font-size: 35px; 
                                                                                 color: white;
                                                                                 box-shadow: 5px 5px 5px grey;
                                                                                 border-radius: 15px",
                                                                        icon = icon("images"),
                                                                        size = "lg",
                                                                        onclick = "start_snapping()")),
                                  
                                  
                                  span(style = "font-size: 13px; 
                                                color: black;
                                                text-shadow: 2px 2px 5px black;",
                                       " -- "),
                                  
                                  # Boton para parar Burst-Snapshot ----
                                  shinyjs::disabled(shiny::actionButton(inputId = "stop_BurstSnapshot", 
                                                                        label = "",
                                                                        style = "background-color: red; 
                                                                                 font-family: sans serif; 
                                                                                 font-size: 20px; 
                                                                                 color: white;
                                                                                 box-shadow: 5px 5px 5px grey;
                                                                                 border-radius: 10px",
                                                                        icon = icon("stop"),
                                                                        size = "sm",
                                                                        onclick = "stop_snapping()")
                                                    )
                                  ),
                              
                              
                              
                              # Boton para tomar Single-Snapshot ----
                              div(br(style = "line-height: 200px;"),
                                  align = "center",
                                  shinyjs::disabled(shiny::actionButton(inputId = "snapshot", 
                                                                        label = "",
                                                                        style = "background-color: black; 
                                                                                 font-family: sans serif; 
                                                                                 font-size: 35px; 
                                                                                 color: white;
                                                                                 box-shadow: 5px 5px 5px grey;
                                                                                 border-radius: 15px",
                                                                        icon = icon("camera"),
                                                                        size = "lg",
                                                                        onclick = "take_snapshot()")
                                                    )
                                  ),
                              
                              hr(style = "box-shadow: 2px 2px 2px blue;"),
                              
                              div(align = "right",
                                  shiny::actionButton(inputId = "saveAnnotations",
                                                  label = "",
                                                  style = "background-color: black; 
                                                           font-family: sans serif;
                                                           color: white;
                                                           box-shadow: 5px 5px 5px grey;
                                                           border-radius: 3px",
                                                  icon = icon("save"),
                                                  size = "sm")),
                              
                              # Area de anotaciones ----
                              div(br(style = "line-height: 200px;"),
                                  
                                  align = "center",
                                  
                                  shiny::textAreaInput(inputId = "annotations",
                                                       label = "Anotaciones", 
                                                       value = "", 
                                                       width = '500px',
                                                       height = '150px',
                                                       placeholder = "Escriba aca sus anotaciones"),
                                  # shiny::actionButton(inputId = "saveAnnotations",
                                  #                     label = "",
                                  #                     style = "background-color: black; 
                                  #                              font-family: sans serif;
                                  #                              color: white;
                                  #                              box-shadow: 5px 5px 5px grey;
                                  #                              border-radius: 7px",
                                  #                     icon = icon("save"),
                                  #                     size = "sm")
                                  )
                              
                              
                              
                            ), # end sidebarPanel
                            
                            # MainPanel App ----
                            shiny::mainPanel(
                              
                              # Columna central App ----
                              shiny::column(width = 5,
                                            offset = 1,
                                     
                                            div(style = "box-shadow: 5px 5px 5px grey;
                                                         border-radius: 25px;",
                                         
                                            br(),
                                         
                                            align = "center",
                                         
                                            # Boton para encender-apagar camara 1 ----
                                            shinyWidgets::switchInput(inputId = "OnOffCam1",
                                                                      # label = "On-Off Camara 1",
                                                                      onStatus = NULL,
                                                                      offStatus = NULL,
                                                                      size = "mini",
                                                                      inline = TRUE),
                                           # actionButton(inputId = "OnCam1",
                                           #              label = "",
                                           #              style = "background-color: transparent;
                                           #                       border-color: transparent;
                                           #                       font-family: sans serif;
                                           #                       font-size: 15px;
                                           #                       color: white;
                                           #                       border-radius: 5px",
                                           #              icon = icon("toggle-on"),
                                           #              size = "sm"),
                                           # actionButton(inputId = "OffCam1",
                                           #              label = "",
                                           #              style = "background-color: transparent;
                                           #                       border-color: transparent;
                                           #                       font-family: sans serif;
                                           #                       font-size: 15px;
                                           #                       color: white;
                                           #                       border-radius: 5px",
                                           #              icon = icon("toggle-off"),
                                           #              size = "sm"),
                                         
                                           # Boton para settings camara 1 ----
                                           actionButton(inputId = "cam1Settings",
                                                        label = "",
                                                        style = "background-color: transparent;
                                                                 border-color: transparent;
                                                                 font-family: sans serif;
                                                                 font-size: 15px;
                                                                 color: white;
                                                                 border-radius: 5px",
                                                        icon = icon("sliders"),
                                                        size = "sm"),
                                         
                                           # Lugar para mostrar la imagen de la camara 1 ----
                                           div(style = "border-radius: 25px;
                                                        height: 215px;
                                                        width: 235px;",
                                               uiOutput("cam1")),
                                           # showWebcam(cameraWidth, cameraHeight, cameraQuality),
                                         
                                           # br(),
                                           # br(),
                                           # br(),
                                           # br(),
                                           # br(),
                                         
                                           # Boton para encender-apagar camara 2 ----
                                           shinyWidgets::switchInput(inputId = "OnOffCam2",
                                                                     # label = "On-Off Camara 2",
                                                                     onStatus = NULL,
                                                                     offStatus = NULL,
                                                                     size = "mini",
                                                                     inline = TRUE),
                                           # actionButton(inputId = "OnCam2",
                                           #              label = "",
                                           #              style = "background-color: transparent;
                                           #                       border-color: transparent;
                                           #                       font-family: sans serif;
                                           #                       font-size: 15px;
                                           #                       color: white;
                                           #                       border-radius: 5px",
                                           #              icon = icon("toggle-on"),
                                           #              size = "sm"),
                                           # actionButton(inputId = "OffCam2",
                                           #              label = "",
                                           #              style = "background-color: transparent;
                                           #                       border-color: transparent;
                                           #                       font-family: sans serif;
                                           #                       font-size: 15px;
                                           #                       color: white;
                                           #                       border-radius: 5px",
                                           #              icon = icon("toggle-off"),
                                           #              size = "sm"),
                                         
                                           # Boton para settings camara 2 ----
                                           actionButton(inputId = "cam2Settings",
                                                        label = "",
                                                        style = "background-color: transparent;
                                                                 border-color: transparent;
                                                                 font-family: sans serif;
                                                                 font-size: 15px;
                                                                 color: white;
                                                                 border-radius: 5px",
                                                        icon = icon("sliders"),
                                                        size = "sm"),
                                         
                                           # Lugar para mostrar la imagen de la camara 2 ----
                                           div(style = "border-radius: 25px;
                                                        height: 215px;
                                                        width: 235px;",
                                               uiOutput("cam2")),
                                           # showWebcam2(cameraWidth, cameraHeight, cameraQuality),
                                         
                                           # br(),
                                         
                                           # Campo para ingresar la etiqueta ----
                                           div(align = "center",
                                               style = "color: black; 
                                                        font-family: sans serif;
                                                        text-shadow: 3px 3px 4px grey;",
                                             
                                               shinyjs::disabled(shiny::selectizeInput(inputId = "etiqueta",
                                                                                       label = strong("Ingresar Etiqueta"),
                                                                                       choices = c("", lista_articulos$objeto),
                                                                                       width = '80%',
                                                                                       options = list(placeholder = "--> etiqueta <--",
                                                                                                      create = TRUE)
                                                                                       )
                                                                 ),
                                             
                                               tags$style(type="text/css",
                                                          "#etiqueta {text-align:center;
                                                                      font-family: sans serif; 
                                                                      font-style: italic;
                                                                      display: block;}")
                                           ),
                                         
                                           # br(),
                                         
                                           # Contador para Single-Snapshot y Burst-Snapshot ----
                                           div(align = "center",
                                               style = "color: black;",
                                               shiny::verbatimTextOutput(outputId = "photoCounter"),
                                               tags$style("#photoCounter {text-align: center; 
                                                                          color: black; 
                                                                          font-family: sans serif; 
                                                                          font-size:12px; 
                                                                          font-style: bold; 
                                                                          overflow-y: scroll; 
                                                                          max-height: 80px; 
                                                                          max-width: 70px; 
                                                                          background: white;
                                                                          border-radius: 5px}")
                                           ),
                                         
                                           style = "height:710px;
                                                    width:290px;
                                                    background-image: linear-gradient(#000000, #b4b4b4);"
                                       )
                                ),
                              
                              # Columna derecha App ----
                              shiny::column(width = 6,
                                     
                                     br(),
                                     br(),
                                     
                                     shinyjs::hidden(shiny::textInput(inputId = 'placeholder64',
                                                                      label = '',
                                                                      value = "not_valid")),
                                     
                                     shinyjs::hidden(shiny::textInput(inputId = 'placeholder642',
                                                                      label = '',
                                                                      value = "not_valid")),
                                     
                                     shinyjs::hidden(shiny::textInput(inputId = 'burstplaceholder64',
                                                                      label = '',
                                                                      value = "not_valid")), 
                                     
                                     shinyjs::hidden(shiny::textInput(inputId = 'burstplaceholder642',
                                                                      label = '',
                                                                      value = "not_valid")), 
                                     
                                     shinyjs::hidden(shiny::textInput(inputId = 'results',
                                                                      label = '',
                                                                      value = "not_valid")), 
                                     
                                     shinyjs::hidden(shiny::textInput(inputId = 'results2',
                                                                      label = '',
                                                                      value = "not_valid")),
                                     
                                     
                                     # Un lugar donde mostrar las snapshot cam1 + cam2 ----
                                     div(
                                       br(style = "line-height: 32px;"),
                                       style = "box-shadow: 5px 5px 5px grey;
                                                border-radius: 25px;
                                                height: 580px;
                                                background-image: linear-gradient(#000000, #b4b4b4);",
                                       align = "center",
                                       div(shiny::textOutput(outputId = "labelImagenCam1"),
                                           tags$style("#labelImagenCam1 {text-align: center; 
                                                                         color: white; 
                                                                         font-family: sans serif; 
                                                                         font-size: 15px; 
                                                                         max-height: 20px; 
                                                                         max-width: 700px;}")),
                                       
                                       br(style = "line-height: 12px;"),
                                       
                                       shiny::plotOutput(outputId = "imagenCam1",
                                                         width = "224px", 
                                                         height = "224px"),
                                       shinyjs::hidden(img(id = 'imageprev')),
                                       
                                       br(style = "line-height: 32px;"),
                                       
                                       div(shiny::textOutput(outputId = "labelImagenCam2"),
                                           tags$style("#labelImagenCam2 {text-align: center; 
                                                                         color: white; 
                                                                         font-family: sans serif; 
                                                                         font-size: 15px; 
                                                                         max-height: 20px; 
                                                                         max-width: 700px;}")),
                                       
                                       br(style = "line-height: 12px;"),
                                       
                                       shiny::plotOutput(outputId = "imagenCam2",
                                                         width = "224px", 
                                                         height = "210px"),
                                       shinyjs::hidden(img(id = 'imageprev2'))
                                       
                                     ) # end lugar donde mostrar las snapshot cam1 + cam2
                                     
                              ) # end columna derecha
                            ) # end mainPanel
                          ) # end sidebarLayout
                        ), # end TAB 1: captura de imagenes
                 
                 # Espacios en blanco en encabezado ----
                 # tags$head(tags$script(type="text/javascript", src = "code.js")),
                 # tags$head(tags$script(type="text/javascript", src = "code.js")),
                 # tags$head(tags$script(type="text/javascript", src = "code.js")),
                 # tags$head(tags$script(type="text/javascript", src = "code.js")),
                 
                 # Estilo para los tags
                 # tags$head(tags$style(HTML('.navbar-brand {width: 400px; font-size:35px; text-align:center;}'))),
                 
                 # TAB 2: Boton cerrar ventana y cerar sesion de R ----
                 shiny::tabPanel(title = "", value = "Stop", icon = icon("power-off"))
                 
                 
      ) # end navbarPage
    ) # end fluidPage
  ) # end tagList
} # end app_ui

# shinyManager secure_app ----
app_ui <- shinymanager::secure_app(app_ui,
                                   head_auth = tags$div(shinyjs::useShinyjs(),
                                                        shinyjs::extendShinyjs(text = jscode, functions = c("closeWindow"))),
                                   status = "default",
                                   theme = shinythemes::shinytheme("cosmo"),
                                   tags_top = tags$div(
                                                       tags$h2("snapShooteR ", 
                                                               style = "align:center;
                                                                        font-family: sans serif;
                                                                        text-shadow: 5px 5px 5px grey;
                                                                        font-size:55px;",
                                                            span(style = "font-size: 18px;
                                                                          font-family: sans serif;
                                                                          color: grey;",
                                                                 "by AiLab UBB")
                                                            )
                                                       ),
                                   tags_bottom = tags$div(
                                                          actionButton(inputId = "authStop",
                                                                       label ="",
                                                                       style = "float: right;
                                                                                border-radius: 5px;
                                                                                background-color: red;
                                                                                border-color: transparent;
                                                                                font-size: 10px",
                                                                       icon = icon("power-off")),
                                                          # tags$style("#authStop {float: right;
                                                          #                        border-radius: 5px;
                                                          #                        background-color: red;
                                                          #                        border-color: transparent;
                                                          #                        font-size: 10px}"),
                                                          tags$p(
                                                            "Si tienes problemas con la aplicacion, envia un mail al ",
                                                            tags$a(
                                                              href = "mailto:maalid@gmail.com?Subject=snapShooteR%20Manager",
                                                              target="_top", "administrador."
                                                              )
                                                            ),
                                                          tags$br(),
                                                          tags$p("AiLab UBB - 2020",
                                                                 style = "text-align:center;
                                                                          font-family: sans serif;
                                                                          font-weight: bold;
                                                                          color: grey;
                                                                          font-size:10px")
                                                          ),
                                   enable_admin = TRUE,
                                   background  = "linear-gradient(#000000, #b4b4b4)")


#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    tags$style(HTML("
                     .navbar-nav {
                     float: none !important;
                     }
                     .navbar-nav > li:nth-child(2) {
                     float: right;
                     }
                    ")),
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'snapShooteR'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    
  )
}

