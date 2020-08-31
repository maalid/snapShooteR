#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # observeEvent(input$cam0Settings, {
  #   py$cam0Settings()
  # })
  # observeEvent(input$cam1Settings, {
  #   py$cam1Settings()
  # })
  
  ######## shinymanager 
  # shinyManager setLabels
  shinymanager::set_labels(
    language = "en",
    "Please authenticate" = "",
    "Username:" = "nombre de usuario:",
    "Password:" = "clave de acceso:"
  )
  
  # check_credentials directly on sqlite db
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(# credentials
                                                        db = system.file("app/db/db1.sqlite", package = "snapShooteR"),
                                                        # passphrase = key_get("R-shinymanager-key", "obiwankenobi")
                                                        passphrase = "passphrase"
                                                        )
    )
  
  observe({
    if(is.null(input$shinymanager_where) || (!is.null(input$shinymanager_where) && input$shinymanager_where %in% "application")){
  ######## shinymanager
  
  observeEvent(input$cam1Settings, {
    # # # # # shell("C: & cd C:/Users/Admin/Downloads/webcam-settings-dialog-windows-master & launchCam1.bat")
    # # webcamSettingsPath <- system.file("app/webcamSettings", package = "snapShooteR")
    # # # # shell(glue::glue("C: & cd {webcamSettingsPath} & launchCam1.bat"))
    # # # camNames <- system(glue::glue('{webcamSettingsPath} & chcp 65001 > nul & ffmpeg -list_devices true -f dshow -i dummy -hide_banner'), intern = TRUE)
    # # # cam1Name <- substr(camNames[[3]], start = 50, stop = nchar(camNames[[3]]) - 1)
    # # # system(glue::glue('{webcamSettingsPath} & ffmpeg -f dshow -show_video_device_dialog true -video_pin_name 1 -i video="{cam1Name}"'), wait = FALSE)
    # # camNames <- system(paste(glue::glue('"{webcamSettingsPath}/ffmpeg.exe"'), 'ffmpeg -list_devices true -f dshow -i dummy -hide_banner'), intern = TRUE)
    # camNames <- system(paste('ffmpeg -list_devices true -f dshow -i dummy -hide_banner'), intern = TRUE)
    # cam1Name <- substr(camNames[[3]], start = 50, stop = nchar(camNames[[3]]) - 1)
    # # system(sprintf("%s %s %s", 
    # #                glue::glue('"{webcamSettingsPath}/ffmpeg.exe"'), 
    # #                'chcp 65001 > nul', 
    # #                glue::glue('ffmpeg -f dshow -show_video_device_dialog true -video_pin_name 2 -i video="{cam1Name}"')),
    # #        wait = FALSE,
    # #        invisible = FALSE,
    # #        # # intern = TRUE,
    # #        # ignore.stdout = T,
    # #        # show.output.on.console = F,
    # #        minimized = TRUE)
    # system(paste(glue::glue('ffmpeg -f dshow -show_video_device_dialog true -video_pin_name 2 -i video="{cam1Name}"')),
    #        wait = FALSE,
    #        invisible = FALSE,
    #        # # intern = TRUE,
    #        # ignore.stdout = T,
    #        # show.output.on.console = F,
    #        minimized = TRUE)
    system(paste('guvcview --control_only'))
  })
  observeEvent(input$cam2Settings, {
    # # # # shell("C: & cd C:/Users/Admin/Downloads/webcam-settings-dialog-windows-master & launchCam2.bat")
    # webcamSettingsPath <- system.file("app/webcamSettings", package = "snapShooteR")
    # # # shell(glue::glue("C: & cd {webcamSettingsPath} & launchCam2.bat"))
    # # camNames <- shell(glue::glue('C: & cd {webcamSettingsPath} & chcp 65001 > nul & ffmpeg -list_devices true -f dshow -i dummy -hide_banner'), intern = TRUE)
    # # cam2Name <- substr(camNames[[5]], start = 50, stop = nchar(camNames[[5]]) - 1)
    # # shell(glue::glue('C: & cd {webcamSettingsPath} & ffmpeg -f dshow -show_video_device_dialog true -video_pin_name 2 -i video="{cam2Name}"'), wait = FALSE)
    # camNames <- system(paste(glue::glue('"{webcamSettingsPath}/ffmpeg.exe"'), 'ffmpeg -list_devices true -f dshow -i dummy -hide_banner'), intern = TRUE)
    camNames <- system(paste('ffmpeg -list_devices true -f dshow -i dummy -hide_banner'), intern = TRUE)
    cam2Name <- substr(camNames[[5]], start = 50, stop = nchar(camNames[[5]]) - 1)
    # system(sprintf("%s %s %s", 
    #                glue::glue('"{webcamSettingsPath}/ffmpeg.exe"'), 
    #                'chcp 65001 > nul', 
    #                glue::glue('ffmpeg -f dshow -show_video_device_dialog true -video_pin_name 2 -i video="{cam2Name}"')),
    #        wait = FALSE,
    #        invisible = FALSE,
    #        # # intern = TRUE,
    #        # ignore.stdout = T,
    #        # show.output.on.console = F,
    #        minimized = TRUE)
    system(paste(glue::glue('ffmpeg -f dshow -show_video_device_dialog true -video_pin_name 2 -i video="{cam2Name}"')),
           wait = FALSE,
           invisible = FALSE,
           # # intern = TRUE,
           # ignore.stdout = T,
           # show.output.on.console = F,
           minimized = TRUE)
  })
  
  observeEvent(input$OnOffCam1, {
    OnOffCam1Value <- input$OnOffCam1
    # print(OnOffCam1Value)
    if (OnOffCam1Value == "TRUE") {
      output$cam1 <- renderUI({showWebcam(cameraWidth, cameraHeight, cameraQuality)})
    } else {output$cam1 <- renderUI({webCamOff()})}
  })
  # observeEvent(input$OnCam1, {
  #   output$cam1 <- renderUI({showWebcam(cameraWidth, cameraHeight, cameraQuality)})
  # })
  # observeEvent(input$OffCam1, {
  #   output$cam1 <- renderUI({webCamOff()})
  # })
  
  observeEvent(input$OnOffCam2, {
    OnOffCam2Value <- input$OnOffCam2
    # print(OnOffCam2Value)
    if (OnOffCam2Value == "TRUE") {
      output$cam2 <- renderUI({showWebcam2(cameraWidth, cameraHeight, cameraQuality)})
    } else {output$cam2 <- renderUI({webCam2Off()})}
  })
  # observeEvent(input$OnCam2, {
  #   output$cam2 <- renderUI({showWebcam2(cameraWidth, cameraHeight, cameraQuality)})
  # })
  # observeEvent(input$OffCam2, {
  #   output$cam2 <- renderUI({webCam2Off()})
  # })
  
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())
  shinyFiles::shinyDirChoose(input, "directory", roots = volumes, session = session, restrictions = system.file(package = "base"))
  observeEvent(input$directory, {
    
    shinyjs::enable(id = "experimento")
    shinyjs::enable(id = "CrearExperimento")
    
    shinyjs::disable(id = "directory")
    
    # workingFolderName <- shinyFiles::parseDirPath(volumes, input$directory)
    # setwd(workingFolderName)
    # cat("\ninput$directory value:\n\n")
    print(shinyFiles::parseDirPath(volumes, input$directory))
  })
  
  # CREAR FOLDERS ASOCIADOS A UN EXPERIMENTO ----     
  observeEvent(input$CrearExperimento, {
    
    shinyjs::enable(id = "NuevoExperimento")
    shinyjs::enable(id = "setType")
    
    shinyjs::enable(id = "saveAnnotations")
    
    shinyjs::enable(id = "etiqueta")
    
    workingFolderName <- shinyFiles::parseDirPath(volumes, input$directory)
    experimentFolderName <- "experimento"
    experimentNumber <- input$experimento
    experimentFolderPath <- glue::glue("{workingFolderName}/{experimentFolderName}_{experimentNumber}")
    
    imagenesFolderName <- "imagenes"
    imagenesFolderPath <- glue::glue("{experimentFolderPath}/{imagenesFolderName}")
    
    trainFolderPath <- glue::glue("{imagenesFolderPath}/train")
    validationFolderPath <- glue::glue("{imagenesFolderPath}/validation")
    testFolderPath <- glue::glue("{imagenesFolderPath}/test")
    
    anotacionesFolderName <- "anotaciones"
    anotacionesFolderPath <- glue::glue("{experimentFolderPath}/{anotacionesFolderName}")
    
    if(!dir.exists(experimentFolderPath)){
      dir.create(file.path(experimentFolderPath))
    }
    
    if(!dir.exists(imagenesFolderPath)){
      dir.create(file.path(imagenesFolderPath))
    }
    
    if(!dir.exists(trainFolderPath)){
      dir.create(file.path(trainFolderPath))
    }
    
    if(!dir.exists(validationFolderPath)){
      dir.create(file.path(validationFolderPath))
    }
    
    if(!dir.exists(testFolderPath)){
      dir.create(file.path(testFolderPath))
    }
    
    if(!dir.exists(anotacionesFolderPath)){
      dir.create(file.path(anotacionesFolderPath))
    }
    
    shinyjs::disable(id = "directory")
    
    shinyjs::disable(id = "experimento")
    shinyjs::disable(id = "CrearExperimento")
  })
  
  # HABILITAR O DESHABILITAR BOTONES AL CREAR NUEVO EXPERIMENTO ----   
  observeEvent(input$NuevoExperimento, {
    
    shinyjs::enable(id = "directory")
    
    shinyjs::enable(id = "experimento")
    shinyjs::enable(id = "CrearExperimento")
    
    shinyjs::disable(id = "NuevoExperimento")
    shinyjs::disable(id = "setType")
    
    shinyjs::disable(id = "saveAnnotations")
    
    shinyjs::disable(id = "BurstSnapshot")
    shinyjs::disable(id = "stop_BurstSnapshot")
    shinyjs::disable(id = "snapshot")
    
    shinyjs::disable(id = "etiqueta")
    
    # Resetear contador
    counter(0)
    
    experimentNumber <- input$experimento + 1
    
    shiny::updateSelectInput(session, inputId = "experimento", selected = experimentNumber)
    shiny::updateSelectInput(session, inputId = "setType", selected = "train")
    shiny::updateSelectInput(session, inputId = "etiqueta", selected = "--> etiqueta <--")
    shiny::updateTextAreaInput(session, inputId = "annotations", label = "Anotaciones", value = "", placeholder = "Escriba aca sus anotaciones")
    
  })
  
  # HABILITAR O DESHABILITAR BOTONES SNAPSHOT DEPENDIENDO DE SI LA ETIQUETA ES VACIO O NO ----
  observe({
    
    OnOffCam1Value <- input$OnOffCam1
    OnOffCam2Value <- input$OnOffCam2
    
    # shinyjs::toggleState(id = "BurstSnapshot", 
    #             condition = nchar(input$etiqueta) > 0)
    
    # shinyjs::toggleState(id = "snapshot",
    #             condition = nchar(input$etiqueta) > 0)
    
    shinyjs::toggleState(id = "BurstSnapshot",
                         condition = {nchar(input$etiqueta) > 0 & OnOffCam1Value == "TRUE" & OnOffCam2Value == "TRUE"} )
    shinyjs::toggleState(id = "snapshot",
                         condition = {nchar(input$etiqueta) > 0 & OnOffCam1Value == "TRUE" & OnOffCam2Value == "TRUE"} )
    
  })
  
  # CREAR E INICIALIZAR CONTADOR DE SNAPSHOTS ----
  counter <- reactiveVal(0)
  output$photoCounter <- renderText({ counter() })
  
  liveCounter <- reactiveVal(0)
  output$livePhotoCounter <- renderText({ liveCounter() })
  
  # CAPTURAR IMAGENES EN MODO SINGLE-SNAPSHOT (USING webcam.min.js + Rvision pkg) ----
  observeEvent(input$snapshot, {
    
    if(!identical(input$placeholder64,"not_valid")){
      
      shinyjs::disable(id = "directory")
      
      shinyjs::disable(id = "NuevoExperimento")
      shinyjs::disable(id = "setType")
      
      shinyjs::disable(id = "BurstSnapshot")
      shinyjs::disable(id = "stop_BurstSnapshot")
      shinyjs::disable(id = "snapshot")
      
      # Limpiar etiqueta
      photoLabel <- stringr::str_remove_all(input$etiqueta,"[^[:alnum:]]") 
      # photoLabel <- stringr::str_remove_all(photoLabel,'[á é í ó ú ä ë ï ö ü Á É Í Ó Ú Ä Ë Ï Ö Ü]') 
      photoLabel <- stringr::str_remove_all(photoLabel,' ') 
      # Limpiar código base64
      inconn <- stringr::str_remove(input$placeholder64,'data:image/jpeg;base64,')
      
      inconn2 <- stringr::str_remove(input$placeholder642,'data:image/jpeg;base64,')
      
      # Crear archivo donde guardar imagen
      workingFolderName <- shinyFiles::parseDirPath(volumes, input$directory)
      experimentFolderName <- "experimento"
      experimentNumber <- input$experimento
      experimentFolderPath <- glue::glue("{workingFolderName}/{experimentFolderName}_{experimentNumber}")
      
      imagenesFolderName <- "imagenes"
      imagenesFolderPath <- glue::glue("{experimentFolderPath}/{imagenesFolderName}")
      
      setFolderName <- as.character(input$setType)
      imagesPath <- glue::glue("{imagenesFolderPath}/{setFolderName}/")
      
      if (setFolderName == "train" | setFolderName == "validation") {
        classFolderName <- glue::glue("{imagesPath}/{photoLabel}/")
        if(!dir.exists(classFolderName)){
          dir.create(file.path(classFolderName))
        }
        
        saveImagesPath <- classFolderName #glue::glue("{imagesPath}/{photoLabel}/")
      } 
      else {
        saveImagesPath <- glue::glue("{imagenesFolderPath}/test/")
      }
      
      fileName_cam1 <- sprintf("%s_%s_%s_%s_%s",
                               photoLabel,
                               counter(),
                               "Original", 
                               "cam1",
                               chartr(" :-", "___", format(Sys.time(), "%F %X")))
      
      output$labelImagenCam1 <- renderText({ 
        fileName_cam1
      })
      
      fileName_cam2 <- sprintf("%s_%s_%s_%s_%s",
                               photoLabel,
                               counter(),
                               "Original", 
                               "cam2",
                               chartr(" :-", "___", format(Sys.time(), "%F %X")))
      
      output$labelImagenCam2 <- renderText({ 
        fileName_cam2
      })
      
      outconn <- file(description = paste0(saveImagesPath, fileName_cam1, ".jpeg"),
                      open = "wb")
      # Guardar imagen a disco duro
      base64enc::base64decode(what = inconn, output = outconn)
      close(outconn)
      
      outconn2 <- file(description = paste0(saveImagesPath, fileName_cam2, ".jpeg"),
                       open = "wb")
      # Guardar imagen a disco duro
      base64enc::base64decode(what = inconn2, output = outconn2)
      close(outconn2)
      
      output$imagenCam1 <- renderImage({
        cam1Filename <- glue::glue("{saveImagesPath}/{fileName_cam1}.jpeg")
        list(src = cam1Filename,
             width = 224,
             height = 210)
      }, deleteFile = FALSE)
      
      output$imagenCam2 <- renderImage({
        cam2Filename <- glue::glue("{saveImagesPath}/{fileName_cam2}.jpeg")
        list(src = cam2Filename,
             width = 224,
             height = 210)
      }, deleteFile = FALSE)
      
      # Aumentar contador
      nwCnt <- counter() + 1
      counter(nwCnt)
      
      shinyjs::enable(id = "NuevoExperimento")
      shinyjs::enable(id = "setType")
      
      shinyjs::enable(id = "BurstSnapshot")
      
      shinyjs::enable(id = "snapshot")
      
    }
  })
  
  # CAPTURAR IMAGENES EN MODO BURST-SNAPSHOT (USING webcam.min.js + Rvision pkg) ----
  observeEvent(input$burstplaceholder64, {
    
    if(!identical(input$burstplaceholder64,"not_valid")){
      
      shinyjs::enable(id = "stop_BurstSnapshot")
      
      shinyjs::disable(id = "directory")
      
      shinyjs::disable(id = "NuevoExperimento")
      shinyjs::disable(id = "setType")
      
      shinyjs::disable(id = "BurstSnapshot")
      shinyjs::disable(id = "snapshot")
      
      # shinyjs::disable(id = "OnOffCam1")
      # shinyjs::disable(id = "OnOffCam2")
      shinyjs::disable(id = "cam1Settings")
      shinyjs::disable(id = "cam2Settings")
      
      shinyjs::disable(id = "etiqueta")
      
      # Limpiar etiqueta
      photoLabel <- stringr::str_remove_all(input$etiqueta,"[^[:alnum:]]") 
      # photoLabel <- stringr::str_remove_all(photoLabel,'[á é í ó ú ä ë ï ö ü Á É Í Ó Ú Ä Ë Ï Ö Ü]') 
      photoLabel <- stringr::str_remove_all(photoLabel,' ') 
      
      # Limpiar código base64
      inconn <- stringr::str_remove(input$burstplaceholder64,'data:image/jpeg;base64,')
      
      inconn2 <- stringr::str_remove(input$burstplaceholder642,'data:image/jpeg;base64,')
      
      # Crear archivo donde guardar imagen
      workingFolderName <- shinyFiles::parseDirPath(volumes, input$directory)
      experimentFolderName <- "experimento"
      experimentNumber <- input$experimento
      experimentFolderPath <- glue::glue("{workingFolderName}/{experimentFolderName}_{experimentNumber}")
      
      imagenesFolderName <- "imagenes"
      imagenesFolderPath <- glue::glue("{experimentFolderPath}/{imagenesFolderName}")
      
      setFolderName <- as.character(input$setType)
      imagesPath <- glue::glue("{imagenesFolderPath}/{setFolderName}/")
      
      if (setFolderName == "train" | setFolderName == "validation") {
        classFolderName <- glue::glue("{imagesPath}/{photoLabel}/")
        if(!dir.exists(classFolderName)){
          dir.create(file.path(classFolderName))
        }
        
        saveImagesPath <- classFolderName #glue::glue("{imagesPath}/{photoLabel}/")
      } 
      else {
        saveImagesPath <- glue::glue("{imagenesFolderPath}/test/")
      }
      
      fileName_cam1 <- sprintf("%s_%s_%s_%s_%s",
                               photoLabel,
                               counter(),
                               "Original",
                               "cam1",
                               chartr(" :-", "___", format(Sys.time(), "%F %X")))
      
      output$labelImagenCam1 <- renderText({ 
        fileName_cam1
      })
      
      fileName_cam2 <- sprintf("%s_%s_%s_%s_%s",
                               photoLabel,
                               counter(),
                               "Original",
                               "cam2",
                               chartr(" :-", "___", format(Sys.time(), "%F %X")))
      
      output$labelImagenCam2 <- renderText({ 
        fileName_cam2
      })
      
      outconn <- file(description = paste0(saveImagesPath, fileName_cam1, ".jpeg"),
                      open = "wb")
      
      # Guardar imagen a disco duro
      base64enc::base64decode(what = inconn, output = outconn)
      close(outconn)
      
      outconn2 <- file(description = paste0(saveImagesPath, fileName_cam2, ".jpeg"),
                       open = "wb")
      # Guardar imagen a disco duro
      base64enc::base64decode(what = inconn2, output = outconn2)
      close(outconn2)
      
      output$imagenCam1 <- renderImage({
        cam1Filename <- glue::glue("{saveImagesPath}/{fileName_cam1}.jpeg")
        list(src = cam1Filename,
             width = 224,
             height = 210)
      }, deleteFile = FALSE)
      
      output$imagenCam2 <- renderImage({
        cam2Filename <- glue::glue("{saveImagesPath}/{fileName_cam2}.jpeg")
        list(src = cam2Filename,
             width = 224,
             height = 210)
      }, deleteFile = FALSE)
      
      # Aumentar contador
      nwCnt <- counter() + 1
      counter(nwCnt)
      
    }
  })
  
  # GUARDAR EN ARCHIVO DE TEXTO LAS ANOTACIONES ----
  observeEvent(input$saveAnnotations, {
    
    workingFolderName <- shinyFiles::parseDirPath(volumes, input$directory)
    experimentFolderName <- "experimento"
    experimentNumber <- input$experimento
    experimentFolderPath <- glue::glue("{workingFolderName}/{experimentFolderName}_{experimentNumber}")
    
    anotacionesFolderName <- "anotaciones"
    anotacionesFolderPath <- glue::glue("{experimentFolderPath}/{anotacionesFolderName}")
    
    anotacionesFileName <- glue::glue("anotaciones_{experimentFolderName}_{experimentNumber}")
    anotacionesFilePath <- glue::glue("{anotacionesFolderPath}/{anotacionesFileName}")
    
    if(!dir.exists(anotacionesFolderPath)){
      dir.create(file.path(anotacionesFolderPath))
    }
    
    myAnnotations <- input$annotations
    
    utils::write.table(myAnnotations, 
                       file = glue::glue("{anotacionesFilePath}.txt"), 
                       sep = "", 
                       col.names = FALSE, 
                       row.names = FALSE,
                       quote = FALSE)
    
    
  })
  
  
  # HABILITAR O DESHABILITAR BOTONES APRETAR BOTON stop_BurstSnapshot ----     
  observeEvent(input$stop_BurstSnapshot, {
    
    shinyjs::disable(id = "stop_BurstSnapshot")
    
    shinyjs::enable(id = "NuevoExperimento")
    shinyjs::enable(id = "setType")
    
    shinyjs::enable(id = "BurstSnapshot")
    shinyjs::enable(id = "snapshot")
    
    # shinyjs::enable(id = "OnOffCam1")
    # shinyjs::enable(id = "OnOffCam2")
    shinyjs::enable(id = "cam1Settings")
    shinyjs::enable(id = "cam2Settings")
    
    shinyjs::enable(id = "etiqueta")
    
    # experimentNumber <- input$experimento
    
  })
  
  # SETEAR EN "train" EL TIPO DE SET DE IMAGENES CUANDO LA ETIQUETA CAMBIE ----
  observeEvent(input$etiqueta, {
    
    # Resetear contador
    counter(0)
    
    updateSelectInput(session, inputId = "setType", selected = "train") # updateSelectInput
  })
  
  observe({
    if (input$navbar == "Stop") {
      shinyjs::js$closeWindow()
      stopApp()
    }
  })
  
  ######## shinymanager
    }
  })
  
  observe({
    # if (!is.null(input$authStop) && input$authStop == 1) {
    if (req(input$authStop) == 1) {
    shinyjs::js$closeWindow()
    stopApp()
    }
  })
  ######## shinymanager
  
}
