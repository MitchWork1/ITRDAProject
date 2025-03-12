library(tidyverse)
library(ggpubr)

#Capitalization function
cap_first_letter <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

#Function to return top values in df column and also group/make "*Other" row.
#Will be used heavily throughout Q2 and Q3
top_in_column <- function(df,column, column_name, top_num, delim, delim2 = NULL,
                          is_other = TRUE, include_other = TRUE)
{
  #Remove values based on delim (This is what we replaced previous blanks with,
  #example: replaced "" with "Not Stated"
  column[column == delim] <- NA
  
  #Created for the need of 2 delims (Some rows in that standardization got
  #multi blank options based on factors.)
  if(!is.null(delim2))
  {
    column[column == delim2] <- NA
  }
  
  df <- na.omit(df)
  
  #Splitting values inside rows based on ";", assigned in standardization.
  split_columns <- unlist(strsplit(column, ";"))
  #Remove leading spaces.
  split_columns <- trimws(split_columns)
  #Capitalizing first letter.
  split_columns <- sapply(split_columns, cap_first_letter)
  #Getting counts of values.
  column_counts <- as.data.frame(table(split_columns))
  #Giving column name.
  colnames(column_counts)[1] <- column_name
  
  #Looks for rows with "Other" in them.
  rows_with_other <- grep("Other", column_counts[[1]])
  
  if(length(rows_with_other) > 0)
  {
    #splits if there is row with other in it.
    df_other_rows <- column_counts[rows_with_other, ]
    column_counts <- column_counts[-rows_with_other, ]
  }
  
  column_counts <- column_counts %>% arrange(desc(Freq))
  
  #Check if wanted to include an "*Other" row.
  if(!include_other)
  {
    top_column <- column_counts |> slice(1:top_num)
  }
  #Checks if there will be "*Other" values in top
  else if(top_num < nrow(column_counts))
  {
    #Must select 1 less than top to leave space for future "*Other" row.
    top_column <- column_counts |> slice(1:top_num-1)
  }
  else
  {
    #There is no other values so use exact top
    top_column <- column_counts |> slice(1:top_num)
  }
  #Anti join for getting the values that are going to be in other categories.
  other_column_names <- anti_join(column_counts, top_column, 
                                  by = colnames(column_counts))
  #Add separation for other values names.
  other_column_names <- paste(other_column_names[[1]], collapse = ", ")
  
  #Getting other_count which will be useful for graphing function later.
  if(length(rows_with_other) > 0)
  {
    other_count <- sum(column_counts$Freq) - sum(top_column$Freq) + 
      sum(df_other_rows$Freq)
  }
  else
  {
    other_count <- sum(column_counts$Freq) - sum(top_column$Freq)
  }
  
  #Include_other is used to tell if there is actually a value that was "other",
  #i.e. not a amalgamation of "*Other" values.
  if(is_other == FALSE)
  {
    other_row <- data.frame(column_name = "Other", Freq = other_count)
  }
  else
  {
    #Checks if need "*Other" or rather just top categories.
    if(top_num < nrow(column_counts))
    {
      other_row <- data.frame(column_name = "*Other", Freq = other_count)
    }
  }
  
  #If "*Other" needed then add this row to the df.
  if(top_num < nrow(column_counts) && include_other)
  {  
    colnames(other_row) <- colnames(top_column)
    top_column <- rbind(top_column, other_row)
  }
  
  top_column <- top_column %>% arrange(desc(Freq))
  
  count = nrow(column_counts)
  
  
  if(!include_other)
  {
    #Make null so that graphing knows to not include "*Other" legend.
    other_column_names = NULL
  }
  
  return(list(top_column = top_column, other_column_names = other_column_names,
              count = count))
}

#Plotting function 
plot_gg <- function(df, column_name, x_label, y_label, title, x_ticks,
                    other_text, number_column, axis_size = 10,
                    legend_size = 4, percentage_size = 4, round_num = 1) {
  
  #Making sure values are numeric, needed for question 3.
  axis_size <- as.numeric(axis_size)
  legend_size <- as.numeric(legend_size)
  percentage_size <- as.numeric(percentage_size)
  round_num <- as.numeric(round_num)
  
  #Seeing if need "*Other" legend string.
  if(nrow(df) < number_column)
  {
    legend_text <- str_wrap(paste("*Other includes (Descending Frequency): ",
                                  other_text), width = 40)
  }
  #Calculating percentages for categories. 
  total_freq <- sum(df$Freq)
  df$percentage <- (df$Freq / total_freq) * 100
  
  #Used fill as I like the colors and don't want to assign manually.
  plot = ggplot(df, aes(x = reorder(column_name, -Freq), y = Freq,
                        fill = column_name)) + 
    geom_bar(stat="identity",show.legend = FALSE) + 
    #Percentages above category bars. Allow for custom rounding, and size.
    geom_text(aes(label = paste0(round(percentage, round_num), "%")), 
              vjust = -0.5, size = percentage_size) + 
    theme_minimal() + 
    #Allow for custom size. Auto scaling for some sizes.
    theme(axis.text.x = element_text(angle = 40, hjust = 1),
          axis.text = element_text(face = "bold", size = axis_size),
          axis.title = element_text(face = "bold", size = axis_size * 1.1),
          plot.title = element_text(face = "bold", hjust = 0.5,
                                    size =axis_size * 1.32)) +
    #Adding spacing for x, y and title as it looks better to me.
    labs(x = paste("\n",x_label), y = paste(y_label, "\n"),
         title = paste(title,"\n")) +
    #Custom ticks and scaling.
    scale_y_continuous(breaks = seq(0, max(df$Freq) + x_ticks, by = x_ticks), 
                       limits = c(0, max(df$Freq) * 1.08),
                       expand = c(0, 0))
  #Adding legend, which is technically a label, to graph if need for "*Other"
  #category.
  if(is.null(other_text))
  {
    return(plot)
  }
  else if(nrow(df) < number_column)
  {
    plot <- plot + annotate("label", x = nrow(df), y = max(df$Freq),
                            label = legend_text, hjust = 0.93, vjust = 0.9,
                            size = legend_size, color = "black")
  }
  
  return(plot)
}


server <- function(input, output) { 
  
  df_top_5 <-  read.csv("df.csv")
  employment <- df_top_5 %>% select(StudyField, Employment)
  
  employment <- employment[!grepl("Student", employment$Employment,
                                  ignore.case = TRUE), ] 
  
  employment <- employment[!grepl("Not Stated", employment$Employment,
                                  ignore.case = TRUE), ]
  
  employment <- employment[!grepl("Retired", employment$Employment,
                                  ignore.case = TRUE), ]
  
  employment <- employment[!grepl("prefer not to say", employment$Employment,
                                  ignore.case = TRUE), ]
  
  #Standardizing
  employment$Employment[grepl("Not Employed", employment$Employment,
                              ignore.case = TRUE)] <- "Unemployed"
  
  employment$Employment[grepl("Self-Employed", employment$Employment,
                              ignore.case = TRUE)] <- "Employed"
  
  employment$Employment[grepl("Employed,", employment$Employment,
                              ignore.case = TRUE)] <- "Employed"
  
  employment <- as.data.frame(table(employment))
  
  total_freq <- sum(employment$Freq)
  employment$percentage <- (employment$Freq / total_freq) * 100
  
  # Languages
  top_languages <- reactive({
    top_in_column(df_top_5, df_top_5$ProgLang, "ProgLang",
                  input$numLang, "Not Stated",
                  include_other = input$includeOther)
  })
  
  output$langPlot <- renderPlot({
    plot_gg(
      top_languages()[[1]], 
      top_languages()[[1]]$ProgLang, 
      "Coding Languages", "Frequency",
      paste("Top", min(top_languages()[[3]], input$numLang),
            "Programming Languages Used By Graduates"), 
      300, 
      top_languages()[[2]],
      top_languages()[[3]], input$axisSize,
      input$legendSize, input$percentageSize, input$roundNum)
  })
  
  # Databases
  top_databases <- reactive({
    top_in_column(df_top_5, df_top_5$Databases, "Databases",
                  input$numDb, "None",
                  include_other = input$includeOther)
  })
  
  output$databasesPlot <- renderPlot({
    plot_gg(top_databases()[[1]],
            top_databases()[[1]]$Databases,
            "Databases", "Frequency",
            paste("Top", min(top_databases()[[3]], input$numDb),
                  "Databases Used By Graduates"),
            200, top_databases()[[2]],
            top_databases()[[3]], input$axisSize,
            input$legendSize, input$percentageSize, input$roundNum)
  })
  
  # AI Search
  top_ai_search <- reactive({
    top_in_column(df_top_5, df_top_5$AISearch, "AISearch",
                  input$numAISearch, "None",
                  include_other = input$includeOther)
  })
  
  output$aiSearchPlot <- renderPlot({
    plot_gg(top_ai_search()[[1]], top_ai_search()[[1]]$AISearch, "AI",
            "Frequency", paste("Top ", min(top_ai_search()[[3]],
                                           input$numAISearch),
                               "AI Used By Graduates"), 200,
            top_ai_search()[[2]], top_ai_search()[[3]],
            input$axisSize, input$legendSize, input$percentageSize,
            input$roundNum)
  })
  
  # AI Currently Used For / Dev Tools
  top_ai_used_for <- reactive({
    top_in_column(df_top_5, df_top_5$AIToolCurrently.Using, "AIUsedFor",
                  input$numAIUsedFor, "None", NULL, FALSE,
                  include_other = input$includeOther)
  })
  
  output$aiUsedForPlot <- renderPlot({
    plot_gg(top_ai_used_for()[[1]], top_ai_used_for()[[1]]$AIUsedFor,
            "AI Used For", "Frequency Of Usage",
            paste("Top", min(top_ai_used_for()[[3]], input$numAIUsedFor),
                  "Ways Graduates Use AI"), 100, top_ai_used_for()[[2]],
            top_ai_used_for()[[3]], input$axisSize, input$legendSize,
            input$percentageSize, input$roundNum)
  })
  
  # Platforms
  top_platforms <- reactive({
    top_in_column(df_top_5, df_top_5$Platform, "Platform", input$numPlatform,
                  "None", include_other = input$includeOther)
  })
  
  output$platformsPlot <- renderPlot({
    plot_gg(top_platforms()[[1]], top_platforms()[[1]]$Platform, "Platforms",
            "Frequency Of Usage",
            paste("Top", min(top_platforms()[[3]], input$numPlatform),
                  "Platforms Used By Graduates"), 100, top_platforms()[[2]],
            top_platforms()[[3]], input$axisSize, input$legendSize,
            input$percentageSize, input$roundNum)
  })
  
  # Web Frameworks
  top_web_frameworks <- reactive({
    top_in_column(df_top_5, df_top_5$WebFramework, "WebFramework",
                  input$numWebFrameworks, "None",
                  include_other = input$includeOther)
  })
  
  output$webFrameworksPlot <- renderPlot({
    plot_gg(top_web_frameworks()[[1]], top_web_frameworks()[[1]]$WebFramework,
            "Web Frameworks", "Frequency Of Usage", 
            paste("Top", min(top_web_frameworks()[[3]],
                              input$numWebFrameworks),
                  "Web Frameworks Used By Graduates"),
            300, top_web_frameworks()[[2]], top_web_frameworks()[[3]],
            input$axisSize, input$legendSize, input$percentageSize,
            input$roundNum)
  })
  
  # Industries
  top_industries <- reactive({
    top_in_column(df_top_5, df_top_5$Industry, "Industry", input$numIndustries,
                  "Not Stated", "N/A", include_other = input$includeOther)
  })
  
  output$industriesPlot <- renderPlot({
    plot_gg(top_industries()[[1]], top_industries()[[1]]$Industry,
            "Industries", "Frequency", paste("Top ", min(top_industries()[[3]],
                                                         input$numIndustries),
                                             "Industries Graduates Are In"),
            100, top_industries()[[2]], top_industries()[[3]],
            input$axisSize, input$legendSize, input$percentageSize,
            input$roundNum)
  })
  
  # Job Roles
  top_job_roles <- reactive({
    top_in_column(df_top_5, df_top_5$Role, "Role", input$numJobRoles,
                  "Not Stated", "N/A", include_other = input$includeOther)
  })
  
  output$jobRolesPlot <- renderPlot({
    plot_gg(top_job_roles()[[1]], top_job_roles()[[1]]$Role, "Job Roles",
            "Frequency", paste("Top ", min(top_job_roles()[[3]],
                                           input$numJobRoles),
                               "Job Roles Among Graduates"), 200,
            top_job_roles()[[2]], top_job_roles()[[3]], input$axisSize,
            input$legendSize, input$percentageSize, input$roundNum)
  })
  
  #Employment
  output$employmentPlot <- renderPlot({
    
    employment$percentage <- as.numeric(employment$percentage)
    
    ggplot(employment, aes(x = Employment, y = Freq, fill = StudyField)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = paste0(round(percentage,
                                         as.numeric(input$roundNum)), "%")), 
                position = position_dodge(width = 0.9),
                vjust = -0.5, size = as.numeric(input$percentageSize)) +
      labs(x = "\nEmployment", y = "Frequency\n",
           title = "Graduate Employment Rates For Each Study") +
      scale_y_continuous(breaks = seq(0, 1000, by = 100), limits = c(0, 950)) +
      theme_minimal() +
      theme(
        axis.text = element_text(face = "bold",
                                 size = as.numeric(input$axisSize)),
        axis.title = element_text(face = "bold",
                                  size = as.numeric(input$axisSize) * 1.1),
        plot.title = element_text(face = "bold", hjust = 0.5,
                                  size = as.numeric(input$axisSize) * 1.32),
        legend.text = element_text(size = as.numeric(input$legendSize) * 3),
        legend.title = element_text(face = "bold",
                                    size = as.numeric(input$legendSize) * 3.3)
      )
  })
}