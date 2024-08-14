library(shiny)
# Define server logic required to generate and plot a random distribution
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dfa <- read.table("data/dbf_c.csv", header=T, sep=";",dec=",")


k_pmn <- rcspline.eval(dfa$pmn_adm, knots.only = TRUE)
k_ldh <- rcspline.eval(dfa$ldh_adm, knots.only = TRUE)
k_alb <- rcspline.eval(dfa$albumin_adm, knots.only = TRUE)
# dfa$adm_ecog2 <- ifelse(dfa$adm_ecog2 == "Ecog 0-1",0,1)
# dfa$oncotreat_response2 <- ifelse(dfa$adm_ecog2 == "Ecog 0-1",0,1)


cutoff1 <- 0.2733
cutoff2 <- 0.5304

model1 <- glm(predeath ~    rcspline.eval(pmn_adm, knots=k_pmn) + rcspline.eval(ldh_adm,knots=k_ldh) + rcspline.eval(albumin_adm,knots=k_alb) + as.factor(adm_ecog2) + as.factor(oncotreat_response2) ,family=binomial(),data=dfa)
preds1 <- predict(model1, dfa, type="response")
preds2 <- ifelse(preds1<cutoff1, "low",ifelse(preds1>cutoff2,"High","intermediate"))
  



server <- (function(input, output){

    model2 <- reactive({
        
         paste0(round(predict(model1,newdata=data.frame(pmn_adm=input$pmn, 
                                                        ldh_adm=input$ldh, 
                                                        albumin_adm=input$album, 
                                                        adm_ecog2=ifelse(input$ecog=="2+","Ecog 2-3-4","Ecog 0-1"),
                                                        oncotreat_response2=ifelse(input$oncotreat=="PR/CR","CR-PR",ifelse(input$oncotreat=="SD","SD","PD"))), 
                              type="response")*100,0),"%")
  
    })
    
    
    calcul <- reactive({

      paste0(round(sum(preds1[preds2==ifelse(predict(model1,
                                                      newdata=data.frame(pmn_adm=input$pmn, 
                                                                         ldh_adm=input$ldh, 
                                                                         albumin_adm=input$album, 
                                                                         adm_ecog2=ifelse(input$ecog=="2+","Ecog 2-3-4","Ecog 0-1"),
                                                                         oncotreat_response2=ifelse(input$oncotreat=="PR/CR","CR-PR",ifelse(input$oncotreat=="SD","SD","PD"))), 
                                                      type="response")>cutoff2,"High", 
                                              ifelse(predict(model1,
                                                             newdata=data.frame(pmn_adm=input$pmn, 
                                                                                ldh_adm=input$ldh, 
                                                                                albumin_adm=input$album, 
                                                                                adm_ecog2=ifelse(input$ecog=="2+","Ecog 2-3-4","Ecog 0-1"),
                                                                                oncotreat_response2=ifelse(input$oncotreat=="PR/CR","CR-PR",ifelse(input$oncotreat=="SD","SD","PD"))), 
                                                             type="response")<cutoff1,"low","intermediate"))]>
                          predict(model1,
                                  newdata=data.frame(pmn_adm=input$pmn, 
                                                     ldh_adm=input$ldh, 
                                                     albumin_adm=input$album, 
                                                     adm_ecog2=ifelse(input$ecog=="2+","Ecog 2-3-4","Ecog 0-1"),
                                                     oncotreat_response2=ifelse(input$oncotreat=="PR/CR","CR-PR",ifelse(input$oncotreat=="SD","SD","PD"))), 
                                  type="response"))/sum(preds2==ifelse(predict(model1,
                                                                               newdata=data.frame(pmn_adm=input$pmn, 
                                                                                                  ldh_adm=input$ldh,
                                                                                                  albumin_adm=input$album, 
                                                                                                  adm_ecog2=ifelse(input$ecog=="2+","Ecog 2-3-4","Ecog 0-1"),
                                                                                                  oncotreat_response2=ifelse(input$oncotreat=="PR/CR","CR-PR",ifelse(input$oncotreat=="SD","SD","PD"))), 
                                                                               type="response")>cutoff2,"High",
                                                                       ifelse(predict(model1,
                                                                                       newdata=data.frame(pmn_adm=input$pmn, 
                                                                                                          ldh_adm=input$ldh,
                                                                                                          albumin_adm=input$album, 
                                                                                                          adm_ecog2=ifelse(input$ecog=="2+","Ecog 2-3-4","Ecog 0-1"),
                                                                                                          oncotreat_response2=ifelse(input$oncotreat=="PR/CR","CR-PR",ifelse(input$oncotreat=="SD","SD","PD"))), 
                                                                                       type="response")< cutoff1,"low","intermediate")))*100,0),"%")
    })
    
    
    output$alive <-renderText({      
       return(model2())
    })
    
    output$plotly <- renderPlotly({

      actp <- predict(model1,newdata=data.frame(pmn_adm=input$pmn, 
                                                ldh_adm=input$ldh, 
                                                albumin_adm=input$album, 
                                                adm_ecog2=ifelse(input$ecog=="2+","Ecog 2-3-4","Ecog 0-1"),
                                                oncotreat_response2=ifelse(input$oncotreat=="PR/CR","CR-PR",ifelse(input$oncotreat=="SD","SD","PD"))), 
                      type="response")*100
      
      fig <- plot_ly(alpha = 0.7)
      fig <- fig %>% add_trace(x = ifelse(density(preds1[preds2=="low"])$x*100<0,0, density(preds1[preds2=="low"])$x*100), type="scatter", y = density(preds1[preds2=="low"])$y, name ="Low-risk category population", mode = "lines", fill = "tozeroy", yaxis = "y", text="Low-risk category population", hoverinfo = 'text',fillcolor="rgba(136, 150, 171, 0.3)", line = list(color="rgba(136, 150, 171, 1)", width=2))
      fig <- fig %>% add_trace(x = ifelse(density(preds1[preds2=="intermediate"])$x*100>100, 100, density(preds1[preds2=="intermediate"])$x*100), type="scatter", y = density(preds1[preds2=="intermediate"])$y, name ="Intermediate-risk category population" , mode = "lines", fill = "tozeroy", yaxis = "y", text="Intermediate-risk category population", hoverinfo = 'text', fillcolor="rgba(255, 166, 43, 0.3)",  line = list(color="rgba(255, 166, 43, 1)", width=2))
      fig <- fig %>% add_trace(x = ifelse(density(preds1[preds2=="High"])$x*100>100, 100, density(preds1[preds2=="High"])$x*100), type="scatter", y = density(preds1[preds2=="High"])$y, name ="High-risk category population" , mode = "lines", fill = "tozeroy", yaxis = "y", text="High-risk category population", hoverinfo = 'text', fillcolor="rgba(44, 66, 81, 0.3)",  line = list(color="rgba(44, 66, 81, 1)", width=2))
      fig <- fig %>% layout(yaxis2 = list(overlaying = "y", side = "right", showgrid=FALSE), xaxis=list(title="Risk of 90 days mortality (%)\n"))
      fig <- fig %>% add_segments(x=actp, xend=actp, y=0, yend=4.4, name="Patient",  text="Patient", hoverinfo = 'text', color="#CC7700", line=list(width=3))
      fig
      
      
    })
    
    output$group <- renderText({
      paste0(ifelse(predict(model1,
                            newdata=data.frame(pmn_adm=input$pmn, 
                                               ldh_adm=input$ldh,
                                               albumin_adm=input$album, 
                                               adm_ecog2=ifelse(input$ecog=="2+","Ecog 2-3-4","Ecog 0-1"),
                                               oncotreat_response2=ifelse(input$oncotreat=="PR/CR","CR-PR",ifelse(input$oncotreat=="SD","SD","PD"))), 
                            type="response")>cutoff2,"High",
                    ifelse(predict(model1,
                                   newdata=data.frame(pmn_adm=input$pmn, 
                                                      ldh_adm=input$ldh,
                                                      albumin_adm=input$album, 
                                                      adm_ecog2=ifelse(input$ecog=="2+","Ecog 2-3-4","Ecog 0-1"),
                                                      oncotreat_response2=ifelse(input$oncotreat=="PR/CR","CR-PR",ifelse(input$oncotreat=="SD","SD","PD"))), 
                                   type="response")< cutoff1,"Low","Intermediate")))
    })
    
    output$prisk <-renderText({  
      paste0("Your patient risk is ", model2(), " (top ", calcul() ," of ", ifelse(predict(model1,
                                                                                           newdata=data.frame(pmn_adm=input$pmn, 
                                                                                                              ldh_adm=input$ldh,
                                                                                                              albumin_adm=input$album, 
                                                                                                              adm_ecog2=ifelse(input$ecog=="2+","Ecog 2-3-4","Ecog 0-1"),
                                                                                                              oncotreat_response2=ifelse(input$oncotreat=="PR/CR","CR-PR",ifelse(input$oncotreat=="SD","SD","PD"))), 
                                                                                           type="response")>cutoff2,"High",
                                                                                   ifelse(predict(model1,
                                                                                                  newdata=data.frame(pmn_adm=input$pmn, 
                                                                                                                     ldh_adm=input$ldh,
                                                                                                                     albumin_adm=input$album, 
                                                                                                                     adm_ecog2=ifelse(input$ecog=="2+","Ecog 2-3-4","Ecog 0-1"),
                                                                                                                     oncotreat_response2=ifelse(input$oncotreat=="PR/CR","CR-PR",ifelse(input$oncotreat=="SD","SD","PD"))), 
                                                                                                  type="response")< cutoff1,"Low","Intermediate")) ,"-risk category population).")
    })
    
  })
