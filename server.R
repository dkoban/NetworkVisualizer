library(shiny)
require(dplyr)
require(magrittr)
require(igraph)
require(visNetwork)
require(data.table)
require(DT)
#require(SNAToolKit)

rm(list=ls())
#DTMS Data 
base <- read.csv("TaskSupportingTask.csv")      
base <- data.frame(source = base$SupportingTaskNumber, 
                   target = base$TaskNumber)

ColTasks <- read.csv("CollectiveTask.csv")
ColTasks$TaskName <- gsub("\x96", "-", ColTasks$TaskName)
IndTasks <- read.csv("IndividualTask.csv")
nodeList <- data.frame(number = c(as.character(ColTasks$TaskNumber), as.character(IndTasks$TaskNumber)),
                       desc = c(as.character(ColTasks$TaskName), as.character(IndTasks$TaskName)),
                       type = c(as.character(ColTasks$TaskType), as.character(IndTasks$TaskType)),
                       idx = 1:length(c(ColTasks$TaskNumber, IndTasks$TaskNumber)))
ColTasks <- ColTasks$TaskNumber
IndTasks <- IndTasks$TaskNumber

##TMD Data
TMDMETL <- read.csv("MarkMETLs.csv", header = FALSE)
colnames(TMDMETL) <- c("TOE", "Desc", "MET", "MET-desc", "SCT", "SCT-desc")
TOENums <- unique(TMDMETL$TOE)

TOENumProp <- data.frame(num = TOENums, 
                         proponent = c("ARMOR", "ENGINEERS", "AVIATION/AVIATION LOGISTICS", 
                                       "MULTIFUNCTIONAL LOGISTICS", "ARMOR", "MILITARY INTELLIGENCE", "INFANTRY") )
toeNum2METL <- list()
for(i in 1:length(TOENums)){
METs <- TMDMETL %>% filter(TOE == TOENums[i]) %>% select(Desc, MET) %>% unique
METs <- data.frame(toe = paste0(TOENums[i], ": ", METs$Desc[1]), SMETL = "MET", 
                   number = METs$MET, proponent = TOENumProp$proponent[i])
SCTs <- TMDMETL %>% filter(TOE == TOENums[i]) %>% select(Desc, SCT) %>% unique
SCTs <- data.frame(toe = paste0(TOENums[i], ": ", SCTs$Desc[1]), SMETL = "SCT", 
                   number = SCTs$SCT, proponent = TOENumProp$proponent[i])
toeNum2METL[[i]] <- rbind(METs, SCTs)}
toeNum2METL <- rbind_list(toeNum2METL)

UTL <- read.csv("MarkUTLs.csv", header = TRUE, stringsAsFactors = FALSE)

shinyServer(function(input, output) {
  
  net <- reactiveValues(MET = NULL, SCT = NULL, nodes=NULL, 
                        edges=NULL, g = NULL, list2 = NULL, 
                        list3 = NULL, list4 = NULL, check = NULL)
  
  unitType <- reactive({                                                 #reactively generate list of TOEs based on proponent selection
    req(input$Proponent)
    unitType <- toeNum2METL %>% 
      filter(proponent == input$Proponent) %>% 
      select(toe) %>% arrange(desc(toe)) %>% unique
    unitType$toe
  })
  
  output$TOESelection <- renderUI({                                      #select a unit type based on TOEs
    selectInput("unitType", 
                label = h5("Select a unit type by TOE"), 
                c("", as.character(unitType())), selected = "")
  })
  
  output$METList <- renderUI({                                           #generate list of METs that make up a unit's s.METL
    req(input$unitType)                                   
    METS <- toeNum2METL %>% 
      filter(toe == input$unitType, SMETL == "MET") %>% 
      select(number)
    MET <- nodeList[nodeList$number %in% as.character(METS$number),]
    MET <- MET %>% arrange(number)
    MET$full <- paste0(MET$number, ": ", MET$desc)
    MET$full <- gsub("[?]", " - ", MET$full)
    MET$full <- gsub("(DELETE)", " ", MET$full)
    METList <- c(as.character(MET$number))
    net$MET <- c(as.character(MET$number))
    names(METList) <- MET$full
    METList <- as.list(METList)
    checkboxGroupInput("root", 
                       label = h5("Mission Essential Tasks (METs)"),
                       choices = METList)
  })
  
  output$SCTList <- renderUI({                                           #generate list of SCTs that make up a unit's s.METL
    req(input$unitType)
    SCTS <- toeNum2METL %>% 
      filter(toe == input$unitType, SMETL == "SCT") %>% 
      select(number) %>% arrange(number)
    SCT <- nodeList[nodeList$number %in% as.character(SCTS$number),]
    SCT <- SCT %>% arrange(number)
    SCT$full <- paste0(SCT$number, ": ", SCT$desc)
    SCT$full <- gsub("[?]", " - ", SCT$full)
    SCT$full <- gsub("(DELETE)", " ", SCT$full)
    SCTList <- c(as.character(SCT$number))
    net$SCT <- c(as.character(SCT$number))
    SCTList <- sort(SCTList)
    names(SCTList) <- SCT$full
    SCTList <- as.list(SCTList)
    checkboxGroupInput("root2", 
                       label = h5("Supporting Collective Tasks (SCTs)"),
                       choices = SCTList)
  })
  
  output$plot <- renderVisNetwork({                                     #generate a graph of supporting relationship
    
    check <- paste(input$root, input$root2)
    validate(need(check != "", " "))
    
    EL <- base
    toe <- substr(input$unitType, 0, 9)
    
    UTL <- UTL %>% filter(UTL$ToeNumber == as.character(toe))      #apply a UTL filter
    EL <- EL %>% filter(source %in% c(as.character(UTL$TaskNumber),        
                                      as.character(net$MET),
                                      as.character(net$SCT),
                                      as.character(IndTasks))) 
    
    #UTL <- UTL %>% filter(UTL$Organization.. == as.character(toe))      #apply a UTL filter
    #EL <- EL %>% filter(source %in% c(as.character(UTL$Task..),        
    #                                  as.character(net$MET),
    #                                  as.character(net$SCT),
    #                                  as.character(IndTasks)))  
    
    if(input$step == 1 & input$taskType == 1){
      ## 1-step by in degree
      inEL1 <- EL[which(EL$target %in% as.character(c(input$root, input$root2))),]
      inEL1c <- inEL1[inEL1$source %in% as.character(ColTasks),]
      inEL1i <- inEL1[inEL1$source %in% as.character(IndTasks),]
      
      ELfinal <- inEL1c}
    if(input$step == 1 & input$taskType == 2){
      ## 1-step by in degree
      inEL1 <- EL[which(EL$target %in% as.character(c(input$root, input$root2))),]
      inEL1c <- inEL1[inEL1$source %in% as.character(ColTasks),]
      inEL1i <- inEL1[inEL1$source %in% as.character(IndTasks),]
      
      ELfinal <- inEL1i}
    if(input$step == 1 & input$taskType == 3){
      ## 1-step by in degree
      inEL1 <- EL[which(EL$target %in% as.character(c(input$root, input$root2))),]
      inEL1c <- inEL1[inEL1$source %in% as.character(ColTasks),]
      inEL1i <- inEL1[inEL1$source %in% as.character(IndTasks),]
      
      ELfinal <- rbind(inEL1c, inEL1i)}
    
    if(input$step == 1.5 & input$taskType == 1){
      ## 1-step by in degree
      inEL1 <- EL[which(EL$target %in% as.character(c(input$root, input$root2))),]
      inEL1c <- inEL1[inEL1$source %in% as.character(ColTasks),]
      inEL1i <- inEL1[inEL1$source %in% as.character(IndTasks),]
      
      ## 1.5-step by in degree
      validate(need(sum(inEL1$source %in% as.character(ColTasks))>0, "There is no graph to display. Please select a different task or task type."))
      inEL1.5 <- EL[which(EL$source %in% as.character(inEL1$source)),]
      inEL1.5 <- inEL1.5[which(inEL1.5$target %in% as.character(inEL1$source)),]
      inEL1.5c <- inEL1.5[inEL1.5$source %in% as.character(ColTasks),]
      branch <- c(as.character(inEL1c$source), as.character(inEL1.5c$source))
      inEL1.5 <- list()
      for (i in 1:length(branch)){
        inEL1.5[[i]] <- EL[which(EL$target %in% as.character(branch[i])),]} 
      inEL1.5 <- rbindlist(inEL1.5)
      inEL1.5i <- inEL1.5[inEL1.5$source %in% as.character(IndTasks),]
      
      ELfinal <- rbind(inEL1c, inEL1.5c)}
    if(input$step == 1.5 & input$taskType == 2){
      ## 1-step by in degree
      inEL1 <- EL[which(EL$target %in% as.character(c(input$root, input$root2))),]
      inEL1c <- inEL1[inEL1$source %in% as.character(ColTasks),]
      inEL1i <- inEL1[inEL1$source %in% as.character(IndTasks),]
      
      ## 1.5-step by in degree
      validate(need(sum(inEL1$source %in% as.character(ColTasks))>0, "There is no graph to display. Please select a different task or task type."))
      inEL1.5 <- EL[which(EL$source %in% as.character(inEL1$source)),]
      inEL1.5 <- inEL1.5[which(inEL1.5$target %in% as.character(inEL1$source)),]
      inEL1.5c <- inEL1.5[inEL1.5$source %in% as.character(ColTasks),]
      branch <- c(as.character(inEL1c$source), as.character(inEL1.5c$source))
      inEL1.5 <- list()
      for (i in 1:length(branch)){
        inEL1.5[[i]] <- EL[which(EL$target %in% as.character(branch[i])),]} 
      inEL1.5 <- rbindlist(inEL1.5)
      inEL1.5i <- inEL1.5[inEL1.5$source %in% as.character(IndTasks),]
      
      ELfinal <- rbind(inEL1i, inEL1.5i)}
    if(input$step == 1.5 & input$taskType == 3){
      ## 1-step by in degree
      inEL1 <- EL[which(EL$target %in% as.character(c(input$root, input$root2))),]
      inEL1c <- inEL1[inEL1$source %in% as.character(ColTasks),]
      inEL1i <- inEL1[inEL1$source %in% as.character(IndTasks),]
      
      ## 1.5-step by in degree
      validate(need(sum(inEL1$source %in% as.character(ColTasks))>0, "There is no graph to display. Please select a different task or task type."))
      inEL1.5 <- EL[which(EL$source %in% as.character(inEL1$source)),]
      inEL1.5 <- inEL1.5[which(inEL1.5$target %in% as.character(inEL1$source)),]
      inEL1.5c <- inEL1.5[inEL1.5$source %in% as.character(ColTasks),]
      branch <- c(as.character(inEL1c$source), as.character(inEL1.5c$source))
      inEL1.5 <- list()
      for (i in 1:length(branch)){
        inEL1.5[[i]] <- EL[which(EL$target %in% as.character(branch[i])),]} 
      inEL1.5 <- rbindlist(inEL1.5)
      inEL1.5i <- inEL1.5[inEL1.5$source %in% as.character(IndTasks),]
      
      ELfinal <- rbind(inEL1c, inEL1i, inEL1.5c, inEL1.5i)}
    
    if(input$step == 2 & input$taskType == 1){
      ## 1-step by in degree
      inEL1 <- EL[which(EL$target %in% as.character(c(input$root, input$root2))),]
      inEL1c <- inEL1[inEL1$source %in% as.character(ColTasks),]
      inEL1i <- inEL1[inEL1$source %in% as.character(IndTasks),]
      
      ## 1.5-step by in degree
      validate(need(sum(inEL1$source %in% as.character(ColTasks))>0, "There is no graph to display. Please select a different task or task type."))
      inEL1.5 <- EL[which(EL$source %in% as.character(inEL1$source)),]
      inEL1.5 <- inEL1.5[which(inEL1.5$target %in% as.character(inEL1$source)),]
      inEL1.5c <- inEL1.5[inEL1.5$source %in% as.character(ColTasks),]
      branch <- c(as.character(inEL1c$source), as.character(inEL1.5c$source))
      inEL1.5 <- list()
      for (i in 1:length(branch)){
        inEL1.5[[i]] <- EL[which(EL$target %in% as.character(branch[i])),]} 
      inEL1.5 <- rbindlist(inEL1.5)
      inEL1.5i <- inEL1.5[inEL1.5$source %in% as.character(IndTasks),]
      
      ## 2-step by in degree
      branch <- inEL1c$source
      
      inEL2 <- list()
      for (i in 1:length(branch)){
        inEL2[[i]] <- EL[which(EL$target %in% as.character(branch[i])),]}
      inEL2 <- rbindlist(inEL2)
      inEL2c <- inEL2[inEL2$source %in% as.character(ColTasks),]
      
      branch <- c(as.character(inEL1c$source), as.character(inEL1.5c$source), as.character(inEL2c$source))
      inEL2 <- list()
      for (i in 1:length(branch)){
        inEL2[[i]] <- EL[which(EL$target %in% as.character(branch[i])),]}
      inEL2 <- rbindlist(inEL2)
      inEL2i <- inEL2[inEL2$source %in% as.character(IndTasks),]
      
      ELfinal <- rbind(inEL1c, inEL2c)}
    if(input$step == 2 & input$taskType == 2){
      ## 1-step by in degree
      inEL1 <- EL[which(EL$target %in% as.character(c(input$root, input$root2))),]
      inEL1c <- inEL1[inEL1$source %in% as.character(ColTasks),]
      inEL1i <- inEL1[inEL1$source %in% as.character(IndTasks),]
      
      ## 1.5-step by in degree
      validate(need(sum(inEL1$source %in% as.character(ColTasks))>0, "There is no graph to display. Please select a different task or task type."))
      inEL1.5 <- EL[which(EL$source %in% as.character(inEL1$source)),]
      inEL1.5 <- inEL1.5[which(inEL1.5$target %in% as.character(inEL1$source)),]
      inEL1.5c <- inEL1.5[inEL1.5$source %in% as.character(ColTasks),]
      branch <- c(as.character(inEL1c$source), as.character(inEL1.5c$source))
      inEL1.5 <- list()
      for (i in 1:length(branch)){
        inEL1.5[[i]] <- EL[which(EL$target %in% as.character(branch[i])),]} 
      inEL1.5 <- rbindlist(inEL1.5)
      inEL1.5i <- inEL1.5[inEL1.5$source %in% as.character(IndTasks),]
      
      ## 2-step by in degree
      branch <- inEL1c$source
      
      inEL2 <- list()
      for (i in 1:length(branch)){
        inEL2[[i]] <- EL[which(EL$target %in% as.character(branch[i])),]}
      inEL2 <- rbindlist(inEL2)
      inEL2c <- inEL2[inEL2$source %in% as.character(ColTasks),]
      
      branch <- c(as.character(inEL1c$source), as.character(inEL1.5c$source), as.character(inEL2c$source))
      inEL2 <- list()
      for (i in 1:length(branch)){
        inEL2[[i]] <- EL[which(EL$target %in% as.character(branch[i])),]}
      inEL2 <- rbindlist(inEL2)
      inEL2i <- inEL2[inEL2$source %in% as.character(IndTasks),]
      
      ELfinal <- rbind(inEL1i, inEL1.5i, inEL2i)}
    if(input$step == 2 & input$taskType == 3){
      ## 1-step by in degree
      inEL1 <- EL[which(EL$target %in% as.character(c(input$root, input$root2))),]
      inEL1c <- inEL1[inEL1$source %in% as.character(ColTasks),]
      inEL1i <- inEL1[inEL1$source %in% as.character(IndTasks),]
      
      ## 1.5-step by in degree
      validate(need(sum(inEL1$source %in% as.character(ColTasks))>0, "There is no graph to display. Please select a different task or task type."))
      inEL1.5 <- EL[which(EL$source %in% as.character(inEL1$source)),]
      inEL1.5 <- inEL1.5[which(inEL1.5$target %in% as.character(inEL1$source)),]
      inEL1.5c <- inEL1.5[inEL1.5$source %in% as.character(ColTasks),]
      branch <- c(as.character(inEL1c$source), as.character(inEL1.5c$source))
      inEL1.5 <- list()
      for (i in 1:length(branch)){
        inEL1.5[[i]] <- EL[which(EL$target %in% as.character(branch[i])),]} 
      inEL1.5 <- rbindlist(inEL1.5)
      inEL1.5i <- inEL1.5[inEL1.5$source %in% as.character(IndTasks),]
      
      ## 2-step by in degree
      branch <- inEL1c$source
      
      inEL2 <- list()
      for (i in 1:length(branch)){
        inEL2[[i]] <- EL[which(EL$target %in% as.character(branch[i])),]}
      inEL2 <- rbindlist(inEL2)
      inEL2c <- inEL2[inEL2$source %in% as.character(ColTasks),]
      
      branch <- c(as.character(inEL1c$source), as.character(inEL1.5c$source), as.character(inEL2c$source))
      inEL2 <- list()
      for (i in 1:length(branch)){
        inEL2[[i]] <- EL[which(EL$target %in% as.character(branch[i])),]}
      inEL2 <- rbindlist(inEL2)
      inEL2i <- inEL2[inEL2$source %in% as.character(IndTasks),]
      
      ELfinal <- rbind(inEL1c, inEL1i, inEL1.5c, inEL1.5i, inEL2c, inEL2i)}
    
    if(input$hidePendants == TRUE){
      ELfinal <- ELfinal[!(duplicated(ELfinal)),]
      nonPendant <- ELfinal$source[duplicated(ELfinal$source)]  
      ELfinal <- ELfinal[ELfinal$source %in% c(as.character(nonPendant)), ]} 
    
    # Create iGraph object
    ifelse(nrow(ELfinal) == 0, visG <- 1, {
    g <- graph.edgelist(as.matrix(ELfinal), directed = TRUE)
    net$g <- g
    
    # Node size
    if(input$sizeby == 1){
      deg <- degree(simplify(g), mode = "out")}
    if(input$sizeby == 2){
      deg <- degree(simplify(g), mode = "in")}
    
    # Node colors
    NODELIST <- data.frame(number = V(g)$name)
    NODELIST <- left_join(NODELIST, nodeList, by = "number")
    NODELIST$color <- "red"
    NODELIST$color[NODELIST$type == "Individual"] <- "orange"
    NODELIST$color[NODELIST$number %in% c(input$root, input$root2)] <- "blue"
    
    # Node shapes
    NODELIST$shape <- "circle"
    NODELIST$shape[NODELIST$color == "blue"] <- "square"
    NODELIST$size <- deg + 1
    
    #convert to VisNetwork
    g2 <- toVisNetworkData(g, idToLabel = TRUE)
    NODELIST <- rename(NODELIST, id = number)
    NODELIST <- rename(NODELIST, group = type)
    g2$nodes <- left_join(g2$nodes, NODELIST, by = "id")
    g2$nodes <- g2$nodes[, c(1, 2, 3, 4, 8)]
    if(input$labelToggle == TRUE){
      g2$nodes <- rename(g2$nodes, id2 = label)
      g2$nodes <- rename(g2$nodes, label = desc)}
    g2$nodes$group <- as.character(g2$nodes$group)
    if(input$labelToggle == FALSE){
      g2$nodes$group[(g2$nodes$id %in% c(as.character(input$root), 
                                         as.character(input$root2)))] <- "Root"}
    g2$nodes$group <- as.character(g2$nodes$group)
    if(input$labelToggle == TRUE){
      g2$nodes$group[(g2$nodes$id %in% c(as.character(input$root), 
                                         as.character(input$root2)))] <- "Root"}
    g2$nodes <- g2$nodes[, c(3, 1, 2, 4, 5)]
    g2$nodes$group <- as.character(g2$nodes$group)
    g2$nodes$size <- input$scaleFactor*(g2$nodes$size + 1)
    g2$edges$arrows <- "to"
    
    net$nodes <- g2$nodes
    net$edges <- g2$edges
    
    set.seed(123)
    visG <- visNetwork(g2$nodes, g2$edges)%>%
      visOptions(highlightNearest=list(enabled=TRUE,hover=FALSE, labelOnly=FALSE, 
                                       degree=list(from = 0, to = 1), algorithm="hierarchical"),
                 nodesIdSelection=TRUE)%>%
      visPhysics(stabilization = "enabled", barnesHut = list(avoidOverlap = 1)) %>%
      visGroups(groupname = "Root", color = list(background = "#3498db", border = " #154360",highlight = " #154360"),shape = "square") %>%
      visGroups(groupname = "Collective", color = list(background = " #cb4335", border = "#641e16",highlight = "#641e16")) %>%
      visGroups(groupname = "Individual", color = list(background = "#f8c471", border = "#9c640c",highlight = "#9c640c")) %>%
      visLegend(width=0.1,position="right",main="Group") %>%
      visIgraphLayout(layout = "layout_with_fr") 
    
    visG})
    
    if(input$showCommunities == TRUE){                                   #generate a graph of related tasks
      check <- paste(input$root, input$root2)
      validate(need(check != "", " "))   
      
      tasks <- toeNum2METL$number[toeNum2METL$proponent == input$Proponent]
      inEL1x <- EL[which(EL$target %in% as.character(tasks)),]
      inEL1x <- inEL1x[inEL1x$source %in% as.character(IndTasks),]
      
      ELfinal <- data.frame(source = as.character(inEL1x[,2]), 
                            target = as.character(inEL1x[,1]))
      
      sourceIdx <- as.numeric(as.factor(ELfinal[, 1]))
      targetIdx <- max(sourceIdx) + as.numeric(as.factor(ELfinal[, 
                                                                  2]))
      n <- length(unique(sourceIdx))
      m <- length(unique(targetIdx))
      biGraph <- graph.empty()
      biGraph <- add.vertices(biGraph, nv = n, attr = list(name = paste0(unique(ELfinal[, 
                                                                                         1])), type = rep(TRUE, n)))
      biGraph <- add.vertices(biGraph, nv = m, attr = list(name = paste0(unique(ELfinal[, 
                                                                                         2])), type = rep(FALSE, m)))
      edgeListVec <- t(ELfinal)
      biGraph <- add.edges(biGraph, edgeListVec)
      B <- get.incidence(biGraph)
      A <- t(B) %*% B
      sociogram <- graph.adjacency(A, diag = FALSE, weighted = TRUE)
      sociogram <- as.undirected(sociogram, mode = "collapse")
      sourceTarget <- get.edgelist(sociogram, names = TRUE)
      edge.weights <- get.edge.attribute(sociogram, "weight")/2
      df <- data.frame(sourceTarget[, 1], sourceTarget[, 2], edge.weights)
      colnames(df) <- c("source", "target", "weight")
      ELfinal <- df
      
      
      denom <- base %>% filter(source %in% IndTasks)
      denom <- denom %>% group_by(target) %>% summarize(tot = n())
      denom1 <- left_join(ELfinal, denom, by = "target")
      colnames(denom) <- c("source", "tot2")
      denom <- left_join(denom1, denom, by = "source")
      denom$tot3 <- denom$tot - denom$weight
      denom$tot4 <- denom$tot2 - denom$weight
      denom$tot5 <- denom$tot3 + denom$tot4 + denom$weight
      denom$perc <- round(100*denom$weight/denom$tot5, 0)
      
      ELfinal <- data.frame(source = denom$source, 
                            target = denom$target,
                            weight = denom$perc/100)
      
      ELfinal <- ELfinal %>% 
        filter(source %in% as.character(c(input$root, input$root2)) | target %in% as.character(c(input$root, input$root2)))
      
      net$list4 <- ELfinal
      
      ROOT <- nodeList$number[which(nodeList$number %in% as.character(c(input$root, input$root2)))]
      if(input$labelToggle == TRUE){
        nodeList2 <- nodeList[, 1:2]
        colnames(nodeList2) <- c("source", "desc")
        ELfinal <- left_join(ELfinal, nodeList2, by = "source")
        colnames(nodeList2) <- c("target", "desc")
        ELfinal <- left_join(ELfinal, nodeList2, by = "target")
        ELfinal <- data.frame(source = ELfinal$desc.x, target = ELfinal$desc.y, weight = ELfinal$weight)
        ROOT <- nodeList$desc[which(nodeList$number %in% as.character(c(input$root, input$root2)))]
        ROOT <- as.character(ROOT)
        ROOT <- gsub("[?]", " - ", ROOT)
        ROOT <- gsub("(DELETE)", " ", ROOT)
      }
      
      edgelist <- ELfinal  
      g <- graph_from_edgelist(as.matrix(edgelist[, 1:2]), directed = FALSE)
      g <- as.undirected(g, mode = "collapse")
      grps <- cluster_edge_betweenness(g)
      #  grptable <- data.frame(grps$names, grps$membership)
      grps$names <- gsub("[?]", " - ", grps$names)
      grps$names <- gsub("(DELETE)", " ", grps$names)
      E(g)$weight <- as.numeric(edgelist[,3])
      edge.weights <- get.edge.attribute(g, "weight")
      edge.label <- paste0(round(100*edge.weights,0), "%")
      igraphEL <- get.edgelist(g, names = FALSE)
      
      nodes <- data.frame(id = c(1:length(grps$names)), label = grps$names
                          , group = "Collective", size = 6)
      nodes$group <- as.character(nodes$group)
      nodes$group[which(nodes$label %in% ROOT)] <- "Root"
      edges <- data.frame(from = igraphEL[, 1], to = igraphEL[, 2], label = edge.label, width = 10*edge.weights) 
      
      visG2 <- visNetwork(nodes, edges) %>% 
        visOptions(highlightNearest = list(enabled = TRUE, hover = FALSE, labelOnly=FALSE), nodesIdSelection = TRUE) %>% 
        visPhysics(stabilization = "enabled", barnesHut = list(avoidOverlap = 1)) %>% visIgraphLayout(layout = "layout_with_fr") %>%
        visGroups(groupname = "Root", color = list(background = "#3498db", border = " #154360",highlight = " #154360"),shape = "square") %>%
        visGroups(groupname = "Collective", color = list(background = " #cb4335", border = "#641e16",highlight = "#641e16")) 
      
      net$list2 <- edges
      net$list3 <- nodes
      
      visG2}
    ifelse(input$showCommunities == TRUE, visG <- visG2, visG  <- visG)
    
    net$check <- visG
    validate(need(class(visG)[1] == "visNetwork", "There is no graph to display. Please select a different task or task type."))
    visG
    
  })
  
  output$DT <- renderDataTable({
    
    check <- paste(input$root, input$root2)
    validate(need(check != "", " ")) 
    validate(need(class(net$g) == "igraph", " "))
    
    #Results Table
    net$nodes$pay <- degree(simplify(net$g), mode = "out")
    net$nodes$comp <- degree(simplify(net$g), mode = "in")
    net$nodes$group[net$nodes$group=="Root"] <- "Collective"
    
    # Collective Payoff
    if(input$taskType == 1 & input$sizeby == 1 & input$labelToggle == FALSE){
      tbl <- net$nodes %>% filter(group == "Collective") %>%
        select(id, desc, group, pay, comp) %>% arrange(desc(pay))
      colnames(tbl) <- c("Task Number", "Description", "Type", "Payoff", "Complexity")}
    #Collective Complexity
    if(input$taskType == 1 & input$sizeby == 2 & input$labelToggle == FALSE){
      tbl <- net$nodes %>% filter(group == "Collective") %>%
        select(id, desc, group, pay, comp) %>% arrange(desc(comp))
      colnames(tbl) <- c("Task Number", "Description", "Type", "Payoff", "Complexity")}
    #Individual Payoff
    if(input$taskType == 2 & input$sizeby == 1 & input$labelToggle == FALSE){
      tbl <- net$nodes %>% filter(group == "Individual") %>%
        select(id, desc, group, pay) %>% arrange(desc(pay))
      colnames(tbl) <- c("Task Number", "Description", "Type", "Payoff")}
    #Individual Complexity
    if(input$taskType == 2 & input$sizeby == 2 & input$labelToggle == FALSE){
      tbl <-  net$nodes %>% filter(group == "Collective") %>%
        select(id, desc, group, comp) %>% arrange(desc(comp))
      colnames(tbl) <- c("Task Number", "Description", "Type", "Complexity")}
    #Both Payoff
    if(input$taskType == 3 & input$sizeby == 1 & input$labelToggle == FALSE){
      tbl <-  net$nodes %>% 
        select(id, desc, group, pay) %>% arrange(desc(pay))
      colnames(tbl) <- c("Task Number", "Description", "Type", "Payoff")}
    #Both Complexity
    if(input$taskType == 3 & input$sizeby == 2 & input$labelToggle == FALSE){
      tbl <-  net$nodes %>% filter(group == "Collective") %>%
        select(id, desc, group, comp) %>% arrange(desc(pay))
      colnames(tbl) <- c("Task Number", "Description", "Type", "Complexity")}
    
    # Collective Payoff
    if(input$taskType == 1 & input$sizeby == 1 & input$labelToggle == TRUE){
      tbl <- net$nodes %>% filter(group == "Collective") %>%
        select(id, label, group, pay, comp) %>% arrange(desc(pay))
      colnames(tbl) <- c("Task Number", "Description", "Type", "Payoff", "Complexity")}
    #Collective Complexity
    if(input$taskType == 1 & input$sizeby == 2 & input$labelToggle == TRUE){
      tbl <- net$nodes %>% filter(group == "Collective") %>%
        select(id, label, group, pay, comp) %>% arrange(desc(comp))
      colnames(tbl) <- c("Task Number", "Description", "Type", "Payoff", "Complexity")}
    #Individual Payoff
    if(input$taskType == 2 & input$sizeby == 1 & input$labelToggle == TRUE){
      tbl <- net$nodes %>% filter(group == "Individual") %>%
        select(id, label, group, pay) %>% arrange(desc(pay))
      colnames(tbl) <- c("Task Number", "Description", "Type", "Payoff")}
    #Individual Complexity
    if(input$taskType == 2 & input$sizeby == 2 & input$labelToggle == TRUE){
      tbl <- net$nodes %>% filter(group == "Collective") %>%
        select(id, label, group, comp) %>% arrange(desc(comp))
      colnames(tbl) <- c("Task Number", "Description", "Type", "Complexity")}
    #Both Payoff
    if(input$taskType == 3 & input$sizeby == 1 & input$labelToggle == TRUE){
      tbl <-  net$nodes %>%
        select(id, label, group, pay) %>% arrange(desc(pay))
      colnames(tbl) <- c("Task Number", "Description", "Type", "Payoff")}
    #Both Complexity
    if(input$taskType == 3 & input$sizeby == 2 & input$labelToggle == TRUE){
      tbl <-  net$nodes %>% filter(group == "Collective") %>%
        select(id, label, group, comp) %>% arrange(desc(comp))
      colnames(tbl) <- c("Task Number", "Description", "Type", "Complexity")}
    
    tbl <- datatable(tbl, options = list(pageLength = 10))
    tbl
    
    if(input$showCommunities == TRUE){
      tbl2 <- net$list4
      colnames(tbl2) <- c("number", "target", "similiarity")
      tbl2 <- left_join(tbl2, nodeList, by = "number")
      tbl2 <- tbl2[, 1:4]
      colnames(tbl2) <- c("source", "number", "similiarity", "task1 desc")
      tbl2 <- left_join(tbl2, nodeList, by = "number")
      tbl2 <- data.frame(tbl2[,1], tbl2[,4], tbl2[,2], tbl2[,5], tbl2[,3])
      colnames(tbl2) <- c("Task 1", "Task 1 Description", "Task 2", "Task 2 Description", "Similarity")
      temp1 <- tbl2 %>% filter(`Task 1` %in% as.character(c(input$root, input$root2)))
      temp2 <- tbl2 %>% filter(!(`Task 1` %in% as.character(c(input$root, input$root2))))
      temp2 <- data.frame(temp2[,3], temp2[,4], temp2[,1], temp2[,2], temp2[,5])
      colnames(temp2) <- c("Task 1", "Task 1 Description", "Task 2", "Task 2 Description", "Similarity")
      tbl2 <- rbind(temp1, temp2)
      tbl2 <- tbl2 %>% arrange(desc(Similarity))
      colnames(tbl2) <- c("Selected Task(s)", "Description", "Related Tasks", "Description ", "Similarity")
      tbl2$Similarity <- paste0(round(100*tbl2$Similarity,0), "%")
    }
    
    ifelse(input$showCommunities == TRUE, tbl <- tbl2, tbl  <- tbl)
    
    validate(need(class(net$check)[1] == "visNetwork", " "))
    tbl 
  })
  
})
