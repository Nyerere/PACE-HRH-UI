library(pacehrh)
library(plotly)
library(treemapify)
library(mgcv)
library(RColorBrewer)

#-------------------Global Variables-------------------

sc_fillScale <- scale_fill_manual(name = "ServiceCat",values = sc_colours)
sc_colorScale <- scale_color_manual(name = "ServiceCat",values = sc_colours)
cc_fillScale <- scale_fill_manual(name = "Category",values = cc_colours)
cc_colorScale <- scale_color_manual(name = "Category",values = cc_colours)

#set hours per week for secondary axis
#hrsperweek <- ServiceCat_Clinical$HrsPerWeek[1]

# ------------------Fertility Plot --------------------
get_fertility_rates_time_series_plot <- function(rv) {
  StartYear <-  rv$start_year 
  EndYear <-  rv$end_year 
  
  age20_39 <- c("AnnualBirthRate20_24", "AnnualBirthRate25_29", "AnnualBirthRate30_34", "AnnualBirthRate35_39")
  rates_sim <- rv$fertilityrates %>% 
    filter(Year >= StartYear & Year <= EndYear) %>% 
    filter(Label %in% age20_39)
  plot <- ggplot(rates_sim, aes(x = Year, y = Rate, color = Label, group = Year)) +
    theme_bw() +
    geom_boxplot() +
    theme(legend.position = "none", axis.text.x = element_text(angle=-90, vjust = .5, hjust=1)) +
    facet_grid(Label ~ test_name)
  ggplotly(plot)
}

#--------------------Population plot------------------
get_population_plot <- function(rv) {
  StartYear <-  rv$start_year 
  EndYear <-  rv$end_year 
  
  pop_sim <- rv$popsummary %>%
    group_by(test_name, Year, Gender, Age) %>%
    dplyr::summarize(Population=mean(Population)) %>% 
    filter(Year == StartYear | Year == EndYear) %>% 
    group_by(test_name, Year) %>% 
    mutate(totalpop = sum(Population)) %>% 
    group_by(test_name) %>% 
    mutate(startpop = round(min(totalpop),0), endpop = round(max(totalpop),0)) %>% 
    mutate(Scenario_label = paste(test_name, 
                                  format(startpop, big.mark = ","),
                                  "Start Pop",
                                  format(endpop, big.mark = ","),
                                  "End Pop",
                                  sep=" "))
  
  plot <- ggplot()+
    theme_bw()+
    geom_point(data = pop_sim, aes(x=Age, y=Population, color=Gender, shape=as.factor(Year)), position = "jitter")+
    facet_wrap(~Scenario_label) +
    labs(color = "Gender", shape = "Year")
  ggplotly(plot)
  
}

# -----------------by Clinical Cat plot---------------------
get_slide_4_plot <- function(rv, plotly=TRUE){
  StartYear <-  rv$start_year + 1
  EndYear <-  rv$end_year 
  temp_clin <- rv$Mean_ClinCat %>%
    filter(Year >= StartYear & Year <= EndYear) %>% 
    mutate(Category = case_when(
      ClinicalOrNon != "Clinical" ~ ClinicalOrNon,
      ClinicalOrNon == "Clinical" ~ paste("Clinical -", ClinicalCat))) %>%
    mutate(Alpha = case_when(
      ClinicalOrNon == "Clinical" ~ 0.3,
      ClinicalOrNon != "Clinical" ~ 1)) %>%
    mutate(Scenario_label = paste(test_name, " - Starting Pop=", format(BaselinePop, big.mark = ",")," - Hrs Per Week=",HrsPerWeek," - Weeks Per Year=",WeeksPerYr,sep=""))
  
  # Calculate total hours for each Category
  total_hours <- temp_clin %>%
    group_by(Category) %>%
    summarize(TotalHrs = sum(MeanHrs))

  # Reorder Category based on total hours
  temp_clin$Category <- factor(temp_clin$Category, levels = total_hours$Category[order(total_hours$TotalHrs, decreasing = FALSE)])

  #temp_clin$Category <- factor(temp_clin$Category, ordered=TRUE, levels=unique(temp_clin$Category))
  
  temp_total <- rv$Mean_Total %>%
    filter(Year >= StartYear & Year <= EndYear) %>% 
    mutate(Scenario_label = paste(test_name, " - Starting Pop=", format(BaselinePop, big.mark = ",")," - Hrs Per Week=",HrsPerWeek," - Weeks Per Year=",WeeksPerYr,sep=""))
  
  ylabel <- "Hours per Week for Catchment Pop"
  maxyval <- max(rv$Mean_Total$CI95/rv$Mean_Total$WeeksPerYr)*1.05
  
  plot <- ggplot()+
    geom_bar(data = temp_clin, aes(x=Year,y=MeanHrs/WeeksPerYr,fill=Category),stat="identity",alpha=.9)+
    geom_line(data = temp_total, aes(x=Year,y=MeanHrs/WeeksPerYr),linewidth=1.2)+
    geom_point(data = temp_total, aes(x=Year,y=MeanHrs/WeeksPerYr))+
    geom_errorbar(data =temp_total, aes(x=Year,ymin=CI05/WeeksPerYr, ymax=CI95/WeeksPerYr), colour="black", width=.3)+
    ylim(0,maxyval)+
    theme_bw()+
    theme(
      axis.text.x = element_text(angle=-90, vjust = .5, hjust=1), 
      legend.title=element_blank(), 
      strip.text = element_text(size=12,margin=margin(1,1,1,1,"cm"))
    )+
    labs(y=ylabel,x="")+
    cc_fillScale+
    facet_wrap(~Scenario_label,labeller=labeller(Scenario_label=label_wrap_gen(80)))
   # scale_fill_viridis_d(option = "A")+
  #   scale_y_continuous(
  #   name = "Left Axis (y)",                                # Label for left axis
  #   sec.axis = sec_axis(~ . * 2, name = "Right Axis (y2)") # Secondary axis with reverse transformation
  # ) +
   
  
  if(plotly){
    print("plotly")
     p_plotly <- ggplotly(plot) #%>%
    #   layout(
    #     margin = list(l = 50, r = 50, b = 100, t = 50) ,
    #     yaxis2 = list(
    #       title = "Logarithmic (y2)",
    #       overlaying = "y",  # Overlay second y-axis
    #       side = "right",     # Put y2 on the right
    #       range = c(min(0) -5, maxyval+7)
    #      # range = c(min(temp_clin$MeanHrs/temp_clin$WeeksPerYr), max(temp_clin$MeanHrs/temp_clin$WeeksPerYr)),
    #     ),
    #     legend = list(
    #       x = 1.3,            # Move it to the right (beyond 1 moves it outside the plot area)
    #       y = 1,              # Align it to the top
    #       xanchor = "left",   # Position the legend box to the left of its x position
    #       yanchor = "top"     # Position the legend box to the top of its y position
    #     )
    #   ) %>% 
    #   add_trace(
    #     x = temp_clin$Year, y = temp_clin$MeanHrs/temp_clin$WeeksPerYr,  # Dummy trace with no data
    #     yaxis = "y2",       # Tie it to the second y-axis
    #     showlegend = FALSE  # Hide the trace from the legend
    #   )
    p_plotly
  }
  else{
    print("plot")
    plot
  }
  
}

# -----------------by ServiceCat bar plot---------------------
byServiceCat_plot <- function(rv, plotly=TRUE){
  StartYear <-  rv$start_year + 1 
  EndYear <-  rv$end_year 
  
  ServiceCat_Clinical <- rv$Mean_ServiceCat %>%
    subset(ClinicalOrNon=="Clinical") %>%
    filter(Year >= StartYear & Year <= EndYear) %>% 
    dplyr::mutate(Scenario_label = paste(test_name, " - Starting Pop=", format(BaselinePop, big.mark = ",")," - Hrs Per Week=",HrsPerWeek," - Weeks Per Year=",WeeksPerYr,sep="")) %>%
    group_by(Scenario_label, Year) %>%
    dplyr::mutate(TotalHrs=sum(MeanHrs)) 
  
  # Calculate total hours for each ServiceCat
  total_hours <- ServiceCat_Clinical %>%
    group_by(ServiceCat) %>%
    summarize(TotalHrs = sum(MeanHrs))

  # Reorder ServiceCat based on total hours
  ServiceCat_Clinical$ServiceCat <- factor(ServiceCat_Clinical$ServiceCat, levels = total_hours$ServiceCat[order(total_hours$TotalHrs, decreasing = FALSE)])


  temp_TotClin <- rv$Stats_TotClin %>% 
    filter(Year >= StartYear & Year <= EndYear) %>% 
    dplyr::mutate(Scenario_label = paste(test_name, " - Starting Pop=", format(BaselinePop, big.mark = ",")," - Hrs Per Week=",HrsPerWeek," - Weeks Per Year=",WeeksPerYr,sep=""))
  
  ymax <- max(temp_TotClin$CI95/temp_TotClin$WeeksPerYr)*1.05
  
  plot <- ggplot() +
    theme_bw()+
    geom_bar(data=ServiceCat_Clinical,aes(x=Year,y=MeanHrs/WeeksPerYr,fill=ServiceCat),stat="identity",alpha=.9)+
    geom_line(data=temp_TotClin,aes(x=Year,y=CI50/WeeksPerYr),linewidth=1.2)+
    geom_point(data=temp_TotClin,aes(x=Year,y=CI50/WeeksPerYr))+
    geom_errorbar(data=temp_TotClin,aes(x=Year,ymin=CI05/WeeksPerYr, ymax=CI95/WeeksPerYr), colour="black", width=.3)+
    ylim(0, ymax) +
    facet_wrap(~Scenario_label,labeller=labeller(Scenario_label=label_wrap_gen(80))) +
    theme(axis.text.x = element_text(angle=-90, vjust = .5, hjust=1))+
    theme(legend.position = c(0.02, 1), legend.justification = c(0.02, 1), legend.key.size=unit(0.3, 'cm'), legend.direction="vertical", legend.background = element_rect(fill = 'transparent'))+
    theme(axis.text.x = element_text(angle=-90, vjust = .5, hjust=1), legend.title=element_blank(), strip.text = element_text(size=12,margin=margin(1,1,1,1,"cm")))+
    #scale_fill_brewer(palette = "BrBG", direction = -1)+
    sc_fillScale +
    labs(x="", y="Hours per Week for Catchment Pop")
  
  if(plotly){
    ggplotly(plot)
  } else{
    plot
  }
  
}

# -----------------by CadreRoles plot---------------------
byCadreRoles_plot <-  function(rv, plotly=TRUE){

  StartYear <-  rv$start_year + 1 
  EndYear <-  rv$end_year 
  
  Cadre_labelled <- rv$Mean_Alloc %>% 
    filter(CI50!=0 ) %>% 
    group_by(test_name, Year) %>% 
    dplyr::mutate(sum_CI50 = sum(CI50), sum_CI05 = sum(CI05), sum_CI95 = sum(CI95)) 
  
  plot <-  ggplot(data=Cadre_labelled)+
    geom_bar(aes(x=Year,y=CI50/WeeksPerYr,fill=RoleDescription),stat="identity",alpha=.9)+
    geom_line(aes(x=Year,y=sum_CI50/WeeksPerYr),linewidth=1.2)+
    geom_point(aes(x=Year,y=sum_CI50/WeeksPerYr))+
    geom_errorbar(aes(x=Year,ymin=sum_CI05/WeeksPerYr, ymax=sum_CI95/WeeksPerYr), colour="black", width=.3)+
    theme_bw()+
    scale_x_continuous(breaks =  c(2021,2025, 2030, 2035))+
    theme(legend.title=element_blank(),legend.position = c(0.02, 0.99), legend.justification = c(0.02, 0.99), 
          legend.key.size=unit(0.3, 'cm'), legend.direction="vertical", legend.background = element_rect(fill = 'transparent'))+
    scale_fill_brewer(palette = "Paired", direction = -1)+
    facet_wrap(~test_name)+
    labs(x="Year", y="Hours per Week per Catchment", fill = "Cadre")
  print(plot)
  
  
  if(plotly){
    ggplotly(plot)
  } else{
    plot
  }
  
}

# -----------------by CadreFTE plot---------------------
byCadreFTE_plot <- function(rv, plotly=TRUE){
  StartYear <-  rv$start_year + 1 
  EndYear <-  rv$end_year 
  write.csv(rv$Mean_Alloc , "Mean_Alloc.csv", row.names = FALSE)
  print(str(rv$Mean_Alloc))
  prev <- rv$Mean_Alloc %>% 
  mutate(
      test_name = as.factor(test_name),
      RoleDescription = as.factor(RoleDescription)
    )
  write.csv(prev, "prev.csv", row.names = FALSE)
  print(str(prev))
  Cadre_labelled <- prev %>% 
    filter(CI50!=0 ) %>% 
    group_by(test_name, Year) %>% 
    dplyr::mutate(sum_CI50 = sum(CI50), sum_CI05 = sum(CI05), sum_CI95 = sum(CI95))  

ma <- rv$Mean_Alloc

ymax = max(ceiling(Cadre_labelled$CI50/Cadre_labelled$WeeksPerYr/(Cadre_labelled$HrsPerWeek*Cadre_labelled$MaxUtilization))) + 1
#Cadre_labelled$ScenarioLabel = paste(Cadre_labelled$Scenario_ID,":", Cadre_labelled$MaxUtilization, "MaxUtil")
#unique(Cadre_labelled$ScenarioLabel)

write.csv(Cadre_labelled, "Cadre_labelled.csv", row.names = FALSE)


plot <-ggplot(data=Cadre_labelled)+
  geom_bar(aes(x=Year,y=ceiling(CI50/WeeksPerYr/(HrsPerWeek*MaxUtilization)),fill=RoleDescription),stat="identity")+
  theme_bw()+
  scale_x_continuous(breaks = c(2021,2025,2030,2035))+
 ylim(0,ymax)+
  facet_grid(RoleDescription ~ test_name )+
 labs(x="Year",y="Minimum staff count",fill="Cadre",title=paste("Minimum staff count by cadre"))+
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = .1, colour = 'white'),
    plot.title = element_text(
      hjust = 0,  # Left align
      size = 16,
      margin = margin(b = 10)),  # Add some bottom margin
    plot.title.background = element_rect(
      fill = "#B3B3B3",  # Light grey background
      color = "#B3B3B3",  # Border color
      linewidth = 1
    )
  )

  if(plotly){
    ggplotly(plot)
  } else{
    plot
  }
}

# -----------------by ServiceCat tile plot---------------------
byServiceTile_plot <- function(rv, plotly=TRUE){

  StartYear <-  rv$start_year + 1
  EndYear <-  rv$end_year 
  
  temp_ServiceCat <- rv$Mean_ServiceCat %>% 
    filter(Year >= StartYear & Year <= EndYear) %>% 
    dplyr::mutate(Scenario_label = paste(test_name, " - Starting Pop=", format(BaselinePop, big.mark = ",")," - Hrs Per Week=",HrsPerWeek," - Weeks Per Year=",WeeksPerYr,sep="")) %>%
    mutate(ServiceLabel = case_when(
      ServiceCat == "Family planning" ~ "Family planning",
      ServiceCat == "Immunization" ~ "Immunization",
      ServiceCat == "Overhead 1" | ServiceCat == "Overhead 2" ~ "Admin",
      ServiceCat == "Record keeping" ~ "Records",
      ServiceCat == "Nutrition" ~ "Nutrition",
      ServiceCat == "NCDs" ~ "NCD",
      ServiceCat == "Sick child" ~ "IMNCI",
      ServiceCat == "Disease surveillance for reportable diseases" ~ "Surveillance", 
      T ~ ServiceCat)) %>% 
    filter(ClinicalOrNon == "Clinical") %>%
    group_by(Year)
  
  # p <- ggplot(temp_ServiceCat,aes(area=MeanHrs,fill=ServiceLabel,label=ServiceLabel,subgroup=ServiceLabel))+
  #   geom_treemap()+geom_treemap_text(color="black",place="center",size=16)+
  #   geom_treemap_subgroup_border(color="black",size=2.5)+
  #   facet_wrap(~test_name) +
  #   theme_bw()+theme(legend.position = "none")+
  #   scale_fill_viridis_d()+
  #   theme(strip.text = element_text(size = 16))
  
  temp_ServiceCatTotal <- rv$Mean_ServiceCat %>%
    filter(ClinicalOrNon == "Clinical") %>%
    group_by(Year) %>%
    summarize(TotalValue = sum(MeanHrs,na.rm=TRUE)) 
  
  temp_ServiceCat$Denominator = temp_ServiceCatTotal$TotalValue[match(temp_ServiceCat$Year,temp_ServiceCatTotal$Year)]
  
  plot <- ggplot(temp_ServiceCat,aes(x=Year,y=MeanHrs/Denominator,fill=ServiceLabel))+
    geom_bar(stat="identity",position="fill")+
    theme_bw() + 
    #scale_fill_viridis_d()+
    sc_fillScale +
    geom_text(aes(label=paste(round(MeanHrs/Denominator*100,0),sep="")), position = position_fill(vjust = 0.5))+
    facet_wrap(~Scenario_label,labeller=labeller(Scenario_label=label_wrap_gen(80))) +
    theme(axis.text.x = element_text(angle=-90, vjust = .5, hjust=1), legend.title=element_blank(), strip.text = element_text(size=12,margin=margin(1,1,1,1,"cm")))+
    xlab("")+ylab("% of workload by type")+
    labs(fill="Service")+
    scale_y_continuous(labels = scales::percent)
  
  if(plotly){
    ggplotly(plot)
  } else{
    plot
  }
  
}

# -----------------service over time plot---------------------
serviceOverTime_plot <- function(rv, plotly=TRUE){

  StartYear <-  rv$start_year + 1
  EndYear <-  rv$end_year  
  
  ServiceCat_Clinical <- rv$Mean_ServiceCat %>%
    subset(ClinicalOrNon=="Clinical") %>%
    filter(Year >= StartYear & Year <= EndYear) %>% 
    dplyr::mutate(Scenario_label = paste(test_name, " - Starting Pop=", format(BaselinePop, big.mark = ",")," - Hrs Per Week=",HrsPerWeek," - Weeks Per Year=",WeeksPerYr,sep="")) %>%
    group_by(Scenario_label, ServiceCat) %>% 
    dplyr::mutate(MeanHrs_Start = dplyr::first(MeanHrs), RatioTo1 = MeanHrs/MeanHrs_Start) %>% 
    dplyr::mutate(RatioLabel = case_when(
      Year == max(Year) ~ paste(ServiceCat, round(RatioTo1,1), sep = ","))) 
  
  ServiceCat_Clinical$ServiceCat = as.factor(ServiceCat_Clinical$ServiceCat)
  
  plot <- ggplot(ServiceCat_Clinical,aes(x=Year,y=RatioTo1,group=ServiceCat,label=round(RatioTo1,2)) )+
    geom_line(aes(color=ServiceCat),size=1.1) +
    geom_hline(yintercept = 1,color="black") +
    theme_bw() +
    sc_colorScale+
    geom_text(data=subset(ServiceCat_Clinical,Year==max(ServiceCat_Clinical$Year))) +
    facet_wrap(~Scenario_label,labeller=labeller(Scenario_label=label_wrap_gen(80))) +
    theme(axis.text.x = element_text(angle=-90, vjust = .5, hjust=1), legend.title=element_blank(), strip.text = element_text(size=12,margin=margin(1,1,1,1,"cm")))+
    labs(x = "", y = "Ratio of Workload vs. Baseline Year")
  
  if(plotly){
    ggplotly(plot)
  } else{
    plot
  }
  
}

# -----------------seasonality plot---------------------
seasonality_plot <- function(rv, plotly=TRUE){
 
  StartYear <-  rv$start_year + 1
  EndYear <-  rv$end_year  
  
  Monthly_NonClinical <- rv$Mean_ClinCat %>% 
    subset(ClinicalOrNon != "Clinical") %>% 
    group_by(test_name, Year) %>% 
    dplyr::summarize(NonClinical_Monthly = sum(MeanHrs)/12)
  
  RatioToAvg_ByMonth <- rv$ByRun_ClinMonth %>% 
    subset(Year == EndYear) %>% 
    left_join(Monthly_NonClinical, by = c("test_name", "Year"))  %>% 
    dplyr::mutate(NonClinical_Monthly = replace_na(NonClinical_Monthly,0)) %>% 
    group_by(test_name, Trial_num, Year) %>% 
    dplyr::mutate(MeanMonthHrs = (mean(TotHrs)+NonClinical_Monthly), RatioToMean = (TotHrs+NonClinical_Monthly)/(MeanMonthHrs)) %>% 
    ungroup() %>% 
    group_by(test_name, Month) %>% 
    dplyr::summarize(RatioToMean_p05 = quantile(RatioToMean, 0.05),
                     RatioToMean_p25 = quantile(RatioToMean, 0.25),
                     RatioToMean_p50 = quantile(RatioToMean, 0.50),
                     RatioToMean_p75 = quantile(RatioToMean, 0.75),
                     RatioToMean_p95 = quantile(RatioToMean, 0.95))
  
  plot <- ggplot(data=RatioToAvg_ByMonth)+
    theme_bw()+
    geom_smooth(aes(x = Month, y=RatioToMean_p50), method ="loess",se=TRUE, fill = "#80B1D3", span=0.5, alpha = 0.25)+
    geom_hline(yintercept = 1, color = "blue", linetype="dashed")+
    scale_color_manual("#80B1D3")+
    scale_x_continuous(breaks =  seq(1, 12))+
    facet_wrap(~test_name)+
    labs(x = "Month", y="Ratio of workload for the month to annual average")
  
  if(plotly){
    ggplotly(plot)
  } else{
    plot
  }
  
}

#INDIVIDUAL SERVICE CATEGORIES
individual_service_category_plot <- function(rv, plotly=TRUE){
  StartYear <-  rv$start_year + 1 
  EndYear <-  rv$end_year 
  
  sc <- rv$Mean_ServiceCat
  ServiceCat_Clinical <- rv$Mean_ServiceCat %>%
    filter(ClinicalOrNon == "Clinical", Year >= StartYear & Year <= EndYear) %>% 
    mutate(Scenario_label = paste(test_name, " - Starting Pop=", format(BaselinePop, big.mark = ",")," - Hrs Per Week=",HrsPerWeek," - Weeks Per Year=",WeeksPerYr,sep="")) %>%
    group_by(Scenario_label, Year) %>%
    mutate(TotalHrs = sum(MeanHrs)) %>%
    arrange(desc(MeanHrs / WeeksPerYr))
	
	num_facets <- length(unique(ServiceCat_Clinical$test_name)) * length(unique(ServiceCat_Clinical$ServiceCat))
  min_width_per_facet <- 400  # Minimum width per facet
  plot_width <- min_width_per_facet * (num_facets / 2)  # Calculate total width
  
  plot <- ggplot(ServiceCat_Clinical, aes(x = Year, y = MeanHrs / WeeksPerYr, fill = ServiceCat)) +
    theme_minimal() +
    geom_bar( stat = "identity", position ="dodge", alpha = 0.9) +
    geom_smooth(aes(x = Year, y = MeanHrs / WeeksPerYr, group = ServiceCat), method = "loess", se = FALSE, color = "black", size = 1) +
    facet_grid(test_name~reorder(ServiceCat, -MeanHrs)) +
    scale_x_continuous(breaks = c(2021, 2025, 2030, 2035)) +
    scale_fill_manual(values = sc_colours) +  # Ensure this is defined
    labs(
      x = "Year",
      y = "Hours per Week per Catchment Pop",
      title = paste("Time Allocation by Service Category") #Adjusted to avoid confusion during comparison
    ) +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      legend.text = element_text(size = 12,color = "black" ),
      axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 1, size = 10, color = "black"),
      axis.text.y = element_text(size = 7, color = "black"),
      axis.title.y = element_text(size = 12, face = "bold",color = "black", margin = margin(r = 10)),
      axis.title.x = element_text(size = 12, face = "bold", color = "black"),
      strip.text = element_text(size = 12, face = "bold", color = "black"),  # Adjusted for clarity
      strip.text.x = element_text(size = 11, face = "bold", angle = 0, margin = margin(t = 10, b = 10), hjust = 0.5),  # Improved readability
      strip.placement = "outside",
      plot.title = element_text(size = 12, face = "bold", margin = margin(b = 10)),
      panel.spacing.x = unit(5, "pt"),
      panel.spacing.y = unit(5, "pt"),
      plot.margin = margin(t = 20, r = 10, b = 10, l = 10, unit = "pt")
    )
  
  if(plotly){
    ggplotly(plot) %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)),
	  width = plot_width,  # Set fixed width
        height = 600,  # Set fixed height
        margin = list(l = 50, r = 50, t = 50, b = 50))
  } else{
    plot
  }
}


# -----------------individual clinical category plot---------------------
individual_clinical_category_plot <- function(rv, plotly=FALSE){
  
  # temp_clin <- rv$Mean_ClinCat %>% 
  #   filter(Year >= 2021 & Year <= 2040) %>% 
  #   dplyr::mutate(Category = case_when(
  #     ClinicalOrNon != "Clinical" ~ ClinicalOrNon,
  #     ClinicalOrNon == "Clinical" ~ paste("Clinical -", ClinicalCat))) %>% 
  #   dplyr::mutate(Alpha = case_when(
  #     ClinicalOrNon == "Clinical" ~ 0.3,
  #     ClinicalOrNon != "Clinical" ~ 1)) %>% 
  #   dplyr::mutate(Scenario_label = paste(Scenario_ID, format(BaselinePop, big.mark = ","),"Starting Pop", sep=" "))  
  # 
  # temp_clin$Category <- factor(temp_clin$Category,ordered=TRUE,levels=unique(temp_clin$Category))

  mc <- rv$Mean_ClinCat

  StartYear <-  rv$start_year + 1
  EndYear <-  rv$end_year 
  temp_clin <- rv$Mean_ClinCat %>%
    filter(Year >= StartYear & Year <= EndYear) %>% 
    mutate(Category = case_when(
      ClinicalOrNon != "Clinical" ~ ClinicalOrNon,
      ClinicalOrNon == "Clinical" ~ paste("Clinical -", ClinicalCat))) %>%
    mutate(Alpha = case_when(
      ClinicalOrNon == "Clinical" ~ 0.3,
      ClinicalOrNon != "Clinical" ~ 1)) %>%
    mutate(Scenario_label = paste(test_name, " - Starting Pop=", format(BaselinePop, big.mark = ",")," - Hrs Per Week=",HrsPerWeek," - Weeks Per Year=",WeeksPerYr,sep=""))
  temp_clin$Category <- factor(temp_clin$Category,ordered=TRUE,levels=unique(temp_clin$Category))
  
  temp_total <- rv$Mean_Total %>%
    filter(Year >= StartYear & Year <= EndYear) %>% 
    mutate(Scenario_label = paste(test_name, " - Starting Pop=", format(BaselinePop, big.mark = ",")," - Hrs Per Week=",HrsPerWeek," - Weeks Per Year=",WeeksPerYr,sep=""))
  
  ylabel <- "Hours per Week for Catchment Pop"
  maxyval <- max(rv$Mean_Total$CI95/rv$Mean_Total$WeeksPerYr)*1.05
  
  # Plot the hours per week using geom_bar for each clinical category 
  plot <- ggplot(temp_clin, aes(x = Year, y = MeanHrs/WeeksPerYr, fill = Category)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.9) +
    facet_grid(test_name ~ reorder(Category, -temp_clin$MeanHrs))+ #this is for rows
    theme(panel.spacing = unit(1, "pt"))+
    labs(title = paste("Hours per Week by Clinical Category"),
         x = "Year", y = "Hours per Week per Catchment Pop") +
    theme_minimal() +  # Use minimal theme for clarity
    cc_fillScale +  # Use the defined fill color scale
    cc_colorScale +
    theme(
      legend.position = "none",  # Position legend at the bottom
      legend.title = element_blank(),  # Remove legend title
      legend.text = element_text(size=12, color="black"),  # Adjust legend text size and color
      axis.text.x = element_text(angle=-90, vjust=0.5, hjust=1, size=13, color="black"),  # Dark black x-axis text
      axis.text.y = element_text(size=12, color="black"),  # Dark black y-axis text
      axis.title = element_text(size=12, face="bold", color="black"),  # Dark black axis titles
      strip.text = element_text(size=12, face="bold", color="black"), # Dark black facet labels
	  plot.margin = margin(10, 10, 10, 10),
      plot.title = element_text(size=14, face="bold", color="black", margin=margin(b=10))) # Dark black plot title
  
  
  # Add trend line (smoothed line) to the plot
  plot <- plot +
    geom_smooth(aes(group = Category), method = "loess", se = FALSE, color = "black", size = 1)


    if(plotly){
    ggplotly(plot) %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)),
	  width = plot_width,  # Set fixed width
        height = 600,  # Set fixed height
        margin = list(l = 50, r = 50, t = 50, b = 50))
  } else{
    plot
  }
}
  

###

# Calculate mean hours per year
mean_hours <- function(dataset, years, service_cats) {
  dataset %>%
    filter(Year %in% years, ServiceCat %in% service_cats) %>%
    group_by(Year, ServiceCat) %>%
    summarise(MeanHrs = round(mean(MeanHrs, na.rm = TRUE), 1), .groups = 'drop') %>%
    arrange(desc(MeanHrs))
}

# Compute differences from the reference year
yearly_diff <- function(mean_hours_data, ref_year) {
  df_base <- mean_hours_data %>% filter(Year == ref_year) %>% select(ServiceCat, MeanHrs)
  
  df_diff <- mean_hours_data %>%
    pivot_wider(names_from = Year, values_from = MeanHrs, names_prefix = "MeanHrs_") %>%
    left_join(df_base, by = "ServiceCat", suffix = c("", "_base"))
  
  ref_col <- paste0("MeanHrs_", ref_year)
  if (!ref_col %in% colnames(df_diff)) return(NULL)
  
  df_diff %>%
    mutate(across(starts_with("MeanHrs_"), ~ round(. - get(ref_col), 1), .names = "YearlyDiff_{.col}")) %>%
    select(ServiceCat, starts_with("YearlyDiff_")) %>%
    pivot_longer(cols = -ServiceCat, names_to = "Year", values_to = "YearlyDiff") %>%
    mutate(Year = gsub("YearlyDiff_MeanHrs_", "", Year)) %>%
    arrange(desc(YearlyDiff))
}

# Compute weekly differences
weekly_diff <- function(yearly_diff_data, dataset, years, service_cats) {
  weeks_data <- dataset %>%
    filter(Year %in% years, ServiceCat %in% service_cats) %>%
    mutate(Year = as.character(Year)) %>%
    select(Year, ServiceCat, WeeksPerYr)
  
  yearly_diff_data %>%
    mutate(Year = as.character(Year)) %>%
    left_join(weeks_data, by = c("Year", "ServiceCat")) %>%
    mutate(WeeklyDiff = round(YearlyDiff / WeeksPerYr, 1)) %>%
    arrange(desc(WeeklyDiff))
}

# -----------------pdf report---------------------
get_pdf_report <- function(rv){
  # print to pdf
  current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  filename <- file.path(result_root, paste0("report_", current_datetime, ".pdf"))
  scenariocount = length(unique(rv$Mean_ServiceCat$test_name))
  pdf(file = filename, width = (scenariocount * 9.5), height = 8)
  p1 <- get_slide_4_plot(rv, plotly = FALSE)
  p2 <- byServiceCat_plot(rv, plotly = FALSE)
  p3 <- byServiceTile_plot(rv, plotly = FALSE)
  p4 <- serviceOverTime_plot(rv, plotly = FALSE)
  p5 <- seasonality_plot(rv, plotly = FALSE)
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  print(p5)
  dev.off()
  return (filename)
}


