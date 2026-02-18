# Health & Fitness Analytics Dashboard
# R Shiny Application - Complete Version with All Plots

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(ggridges)
library(gridExtra)
library(hexbin)
library(lattice)
library(scales)
library(RColorBrewer)

# Read the data
setwd('C:/Users/DELL/OneDrive/Desktop/DV_Final_ Version')
data <- read.csv("Final_data.csv")

# =====================================
# PRE-PROCESSING: CARDIO LOAD SCORE
# =====================================
data <- data %>%
  mutate(
    # Heart Rate Reserve (HRR) - how much capacity was used
    HRR = Max_BPM - Resting_BPM,
    
    # Relative Heart Intensity (0-1 scale)
    Relative_Heart_Intensity = (Avg_BPM - Resting_BPM) / HRR,
    
    # Time Under Load (in hours)
    Time_Under_Load = Session_Duration..hours.,
    
    # Cardio Load Score
    Cardio_Load_Score = Relative_Heart_Intensity * Time_Under_Load * 100,
    
    # Categorize Cardio Load for visualization
    Cardio_Load_Category = cut(
      Cardio_Load_Score,
      breaks = quantile(Cardio_Load_Score, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
      labels = c("Light Load", "Moderate Load", "High Load", "Extreme Load"),
      include.lowest = TRUE
    )
  )

# =====================================
# PRE-PROCESSING: NUTRITION EFFICIENCY RATIO
# =====================================
data <- data %>%
  mutate(
    # Total Calories Consumed (from macronutrients)
    # 1g Carbs = 4 cal, 1g Protein = 4 cal, 1g Fat = 9 cal
    Total_Calories_Consumed = (Carbs * 4) + (Proteins * 4) + (Fats * 9),
    
    # Nutrition Efficiency Ratio
    Nutrition_Efficiency_Ratio = Calories_Burned / Total_Calories_Consumed,
    
    # Categorize efficiency for insights
    Efficiency_Category = cut(
      Nutrition_Efficiency_Ratio,
      breaks = c(0, 0.5, 1, 1.5, Inf),
      labels = c("Low Efficiency (<0.5)", 
                 "Moderate (0.5-1.0)", 
                 "High (1.0-1.5)", 
                 "Very High (>1.5)"),
      include.lowest = TRUE
    )
  )

# Filter out any infinite or extreme values
data <- data %>%
  filter(is.finite(Nutrition_Efficiency_Ratio) & 
           Nutrition_Efficiency_Ratio > 0 & 
           Nutrition_Efficiency_Ratio < 10)


# UI
ui <- dashboardPage(
  dashboardHeader(title = "Health & Fitness Analytics"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home / Overview", tabName = "home", icon = icon("dashboard")),
      menuItem("Personal Insights", tabName = "personal", icon = icon("user")),
      menuItem("Nutrition Analysis", tabName = "nutrition", icon = icon("apple-alt")),
      menuItem("Workout Analysis", tabName = "workout", icon = icon("dumbbell"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # =====================================
      # TAB 1: HOME / OVERVIEW
      # =====================================
      tabItem(tabName = "home",
              h2("Health & Fitness Analytics Dashboard"),
              h4("Dashboard Summary"),
              br(),
              fluidRow(
                valueBoxOutput("total_users"),
                valueBoxOutput("avg_bmi"),
                valueBoxOutput("avg_calories")
              ),
              fluidRow(
                valueBoxOutput("common_workout"),
                valueBoxOutput("avg_duration"),
                valueBoxOutput("avg_bpm")
              ),
              fluidRow(
                box(
                  title = "Gender Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("gender_pie")
                ),
                box(
                  title = "Workout Types Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("workout_pie")
                )
              ),
              fluidRow(
                box(
                  title = "Macronutrient Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("macro_pie")
                ),
                box(
                  title = "BMI Categories Overview",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("bmi_categories")
                )
              )
              
      ),
      
      # =====================================
      # TAB 2: PERSONAL INSIGHTS
      # =====================================
      tabItem(
        tabName = "personal",
        h2("Personal Insights - User Characteristics"),
        
        tabsetPanel(
          
          # ---------- TAB 1 ----------
          tabPanel("Age Distribution",
                   br(),
                   box(
                     title = "Age Distribution",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("age_distribution")
                   )
          ),
          
          # ---------- TAB 2 ----------
          tabPanel("Gender → Average Calories",
                   br(),
                   box(
                     title = "Gender → Average Calories Burned",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("gender_calories")
                   )
          ),
          
          # ---------- TAB 3 ----------
          tabPanel("Experience Level → Session Duration",
                   br(),
                   box(
                     title = "Experience Level → Session Duration",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("experience_duration")
                   )
          ),
          
          # ---------- TAB 4 ----------
          tabPanel("BMI vs Calories",
                   br(),
                   box(
                     title = "BMI vs Calories Burned",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("personal_bmi_cal_hex")
                   )
          ),
          
          # ---------- NEW: PLOT 8 ----------
          tabPanel("BMI vs Calories (Enhanced)",
                   br(),
                   box(
                     title = "BMI vs Calories (Enhanced)",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("plot8_bmi_calories_enhanced")
                   )
          )
          
        ) # end tabsetPanel
      ),
      
      # =====================================
      # TAB 3: NUTRITION ANALYSIS
      # =====================================
      tabItem(
        tabName = "nutrition",
        h2("Nutrition Analysis"),
        
        tabsetPanel(
          
          tabPanel("Macronutrient Balance vs Calories Burned",
                   br(),
                   box(
                     title = "Macronutrient Balance vs Calories Burned",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("macro_heatmap")
                   )
          ),

          tabPanel("Hydration vs Calories Burned",
                   br(),
                   box(
                     title = "Hydration vs Calories Burned",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("hydration_ridge")
                   )
          ),
          
          tabPanel("Nutrition Ceiling Plot",
                   br(),
                   box(
                     title = "Can You Out-Train a Bad Diet?",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("nutrition_ceiling")
                   )
          ),
          
          # ========== NUTRITION EFFICIENCY RATIO ==========
          tabPanel("Nutrition Efficiency Ratio",
                   br(),
                   box(
                     title = "Nutrition Efficiency Trend Analysis",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("nutrition_efficiency_plot")
                   )
          ),
          
          # ========== NEW: PLOT 4 ==========
          tabPanel("Hydration × Nutrition Path",
                   br(),
                   box(
                     title = "Hydration × Nutrition Performance Pathway",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("plot4_hydration_nutrition_path")
                   )
          )
          
        )
      ),
      
      # =====================================
      # TAB 4: WORKOUT ANALYSIS
      # =====================================
      tabItem(
        tabName = "workout",
        h2("Workout Analysis - Performance & Training Behaviour"),
        
        tabsetPanel(
          
          tabPanel("BPM / Calories by Workout Type",
                   br(),
                   box(
                     title = "Average BPM and Calories Burned by Workout Type",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     selectInput(
                       "metric_selector",
                       "Select Metric:",
                       choices = c("Average BPM", "Calories Burned"),
                       selected = "Average BPM"
                     ),
                     plotOutput("bpm_workout_boxplot", height = "330px")
                   )
          ),
          
          tabPanel("Duration vs Calories",
                   br(),
                   box(
                     title = "Workout Duration vs Calories Burned",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("duration_calories")
                   )
          ),
          
          tabPanel("Frequency vs Calories",
                   br(),
                   box(
                     title = "Workout Frequency vs Calories Burned",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("frequency_calories")
                   )
          ),
          
          tabPanel("Sweet Spot Matrix",
                   br(),
                   box(
                     title = "Sweet Spot Matrix – Optimal Training Parameters",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("sweet_spot_matrix")
                   )
          ),
          
          # ========== CARDIO LOAD SCORE ==========
          tabPanel("Cardio Load Score",
                   br(),
                   box(
                     title = "Cardio Load Score by Workout Type and Gender",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("cardio_load_plot")
                   )
          ),
          
          # ========== NEW: PLOT 9 ==========
          tabPanel("Workout Performance with Uncertainty",
                   br(),
                   box(
                     title = "Average Calories Burned by Workout Type",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("plot9_workout_performance_uncertainty")
                   )
          ),
          
          # ========== NEW: PLOT B ==========
          tabPanel("Performance Progression",
                   br(),
                   box(
                     title = "Workout Performance Over Experience",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("plotb_timeseries")
                   )
          ),
          
          # ========== NEW: PLOT J ==========
          tabPanel("Sweet Spot Density",
                   br(),
                   box(
                     title = "Workout Parameter Density Analysis",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("plotj_contour_density")
                   )
          )
        )
      )
      
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  # =====================================
  # TAB 1: HOME / OVERVIEW - VALUE BOXES
  # =====================================
  output$total_users <- renderValueBox({
    valueBox(
      value = nrow(data),
      subtitle = "Total Users",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$avg_bmi <- renderValueBox({
    valueBox(
      value = round(mean(data$BMI, na.rm = TRUE), 1),
      subtitle = "Average BMI",
      icon = icon("weight"),
      color = "green"
    )
  })
  
  output$avg_calories <- renderValueBox({
    valueBox(
      value = round(mean(data$Calories_Burned, na.rm = TRUE), 0),
      subtitle = "Avg Calories Burned",
      icon = icon("fire"),
      color = "red"
    )
  })
  
  output$common_workout <- renderValueBox({
    most_common <- names(sort(table(data$Workout_Type), decreasing = TRUE))[1]
    valueBox(
      value = most_common,
      subtitle = "Most Common Workout",
      icon = icon("running"),
      color = "purple"
    )
  })
  
  output$avg_duration <- renderValueBox({
    valueBox(
      value = paste(round(mean(data$Session_Duration..hours., na.rm = TRUE), 2), "hrs"),
      subtitle = "Avg Session Duration",
      icon = icon("clock"),
      color = "orange"
    )
  })
  
  output$avg_bpm <- renderValueBox({
    valueBox(
      value = round(mean(data$Avg_BPM, na.rm = TRUE), 0),
      subtitle = "Average BPM",
      icon = icon("heartbeat"),
      color = "maroon"
    )
  })
  
  # =====================================
  # TAB 1: HOME / OVERVIEW - PLOTS
  # =====================================
  output$gender_pie <- renderPlot({
    df_gender <- data %>%
      count(Gender) %>%
      mutate(
        Gender = as.factor(Gender),
        percent = n / sum(n) * 100,
        label = paste0(Gender, ": ", round(percent, 1), "%")
      )
    
    ggplot(df_gender, aes(x = "", y = percent, fill = Gender)) +
      geom_col(width = 1, color = "white") +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c(
        "Male" = "#8EC5FC",
        "Female" = "#F7A1C4"
      )) +
      geom_text(aes(label = label), 
                position = position_stack(vjust = 0.5),
                color = "white",
                size = 5,
                fontface = "bold") +
      labs(
        title = "Gender Distribution",
        x = NULL,
        y = NULL,
        fill = "Gender"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = "#003F8C", hjust = 0.5),
        legend.position = "none"
      )
  })
  
  output$workout_pie <- renderPlot({
    df_workout <- data %>%
      count(Workout_Type) %>% 
      mutate(
        Workout_Type = as.factor(Workout_Type),
        percent = n / sum(n) * 100,
        label = paste0(Workout_Type, ": ", round(percent, 1), "%")
      )
    
    ggplot(df_workout, aes(x = "", y = percent, fill = Workout_Type)) +
      geom_col(color = "white") +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = colorRampPalette(
        c("#D6E9FF", "#5A8DEE", "#003F8C")
      )(length(unique(df_workout$Workout_Type)))) +
      geom_text(
        aes(label = label),
        position = position_stack(vjust = 0.5),
        color = "white",
        size = 4.8,
        fontface = "bold"
      ) +
      labs(
        title = "Workout Types Distribution",
        x = NULL,
        y = NULL,
        fill = "Workout Type"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = "#003F8C", hjust = 0.5),
        legend.position = 'None'
      )
  })
  
  output$bmi_categories <- renderPlot({
    data_bmi <- data %>% mutate(BMI_Group = cut(BMI, breaks=c(0,18.5,25,30,100), labels=c("Underweight","Normal","Overweight","Obese")))
    ggplot(data_bmi, aes(BMI_Group)) +
      geom_bar(fill="steelblue") +
      labs(title="BMI Categories", x="BMI Category", y="Count") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = "#003F8C",hjust = 0.5),
        axis.title = element_text(size = 13, color = "#003F8C")
      )
  })
  
  # =====================================
  # TAB 2: PERSONAL INSIGHTS
  # =====================================
  
  output$age_distribution <- renderPlot({
    ggplot(data, aes(x = Age)) +
      geom_histogram(binwidth = 3, fill = "skyblue", color = "white", alpha = 0.8) +
      labs(title = "Age Distribution of Users", x = "Age", y = "Count") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = "#003F8C",hjust = 0.5),
        axis.title = element_text(size = 13, color = "#003F8C")
      )
  })
  
  output$gender_calories <- renderPlot({
    ggplot(data, aes(x = Gender, y = Calories_Burned, fill = Gender)) +
      geom_bar(stat = "summary", fun = "mean", width = 0.6) +
      scale_fill_manual(
        values = c(
          "Male" = "#8EC5FC",
          "Female" = "#F7A1C4"
        )
      ) +
      labs(
        title = "Average Calories Burned by Gender",
        x = "Gender",
        y = "Calories Burned"
      ) +
      theme_minimal() + 
      theme(
        plot.title = element_text(
          size = 16, face = "bold", color = "#003F8C", hjust = 0.5
        ),
        axis.title = element_text(size = 13, color = "#003F8C"),
        legend.position = "none"
      )
  })
  
  
  output$experience_duration <- renderPlot({
    data_exp <- data %>% mutate(Exp_Level_Group = round(Experience_Level))
    ggplot(data_exp, aes(x = as.factor(Exp_Level_Group), y = Session_Duration..hours.)) +
      geom_boxplot(fill="#D6E9FF", color="#003F8C") +
      labs(title = "Session Duration by Experience Level", x = "Experience Level (1=Beginner, 2=Intermediate, 3=Advanced)",
           y = "Session Duration") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = "#003F8C",hjust = 0.5),
        axis.title = element_text(size = 13, color = "#003F8C")
      )
  })
  
  output$personal_bmi_cal_hex <- renderPlot({
    
    df <- data  
    
    # Main Hexbin Plot
    main_plot <- ggplot(df, aes(x = BMI, y = Calories_Burned)) +
      geom_hex(bins = 30) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      theme_minimal() +
      theme(legend.position = "right") +
      labs(x = "BMI", y = "Calories Burned", fill = "Count")
    
    # Top Histogram
    top_hist <- ggplot(df, aes(x = BMI)) +
      geom_histogram(fill = "steelblue", alpha = 0.5, bins = 30) +
      theme_void()
    
    # Right Histogram
    right_hist <- ggplot(df, aes(x = Calories_Burned)) +
      geom_histogram(fill = "steelblue", alpha = 0.5, bins = 30) +
      coord_flip() +
      theme_void()
    
    # Combine the 3 Plots
    grid.arrange(
      top_hist, ggplot() + theme_void(),
      main_plot, right_hist,
      ncol = 2, nrow = 2,
      widths = c(4, 1),
      heights = c(1, 4)
    )
  })
  
  # =====================================
  # NEW: PLOT 8 - BMI vs Calories Enhanced
  # =====================================
  output$plot8_bmi_calories_enhanced <- renderPlot({
    ggplot(data, aes(x = BMI, y = Calories_Burned)) +
      geom_hex(bins = 30, alpha = 0.7) +
      geom_point(alpha = 0.15, size = 1, color = "#003F8C",
                 position = position_jitter(width = 0.3, height = 15)) +
      scale_fill_gradient(low = "#D6E9FF", high = "#003F8C", name = "Count") +
      geom_smooth(method = "loess", color = "#E74C3C", 
                  se = TRUE, alpha = 0.2, size = 1.2, fill = "#E74C3C") +
      scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
      labs(
        title = "BMI vs Calories Burned with Density Distribution",
        subtitle = "Hexagons show density | Jittered points reveal individual data | Red trend line",
        x = "BMI",
        y = "Calories Burned"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "right",
        panel.grid.minor = element_blank()
      )
  })
  
  # =====================================
  # TAB 3: NUTRITION ANALYSIS
  # =====================================
  output$macro_pie <- renderPlot({
    macro_totals <- data %>%
      summarise(
        Carbs = sum(Carbs, na.rm = TRUE),
        Proteins = sum(Proteins, na.rm = TRUE),
        Fats = sum(Fats, na.rm = TRUE)
      ) %>%
      pivot_longer(everything(), names_to = "Macro", values_to = "Amount") %>%
      mutate(
        Percentage = Amount / sum(Amount) * 100,
        Label = paste0(Macro, "\n", round(Percentage, 1), "%")
      )
    
    ggplot(macro_totals, aes(x = "", y = Amount, fill = Macro)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(label = Label), position = position_stack(vjust = 0.5), 
                size = 5, fontface = "bold", color = "white") +
      scale_fill_manual(values = c("Carbs" = "#e74c3c", "Proteins" = "#3498db", "Fats" = "#f39c12")) +
      labs(title = "Macronutrient Distribution") +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  })
  
  output$macro_heatmap <- renderPlot({
    data_macro <- data %>%
      mutate(
        carb_category = cut(Carbs, breaks = 5, labels = c("Very Low", "Low", "Medium", "High", "Very High")),
        protein_category = cut(Proteins, breaks = 5, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
      )
    
    heatmap_data <- data_macro %>%
      group_by(carb_category, protein_category) %>%
      summarise(avg_calories = mean(Calories_Burned, na.rm = TRUE), .groups = "drop")
    
    ggplot(heatmap_data, aes(x = carb_category, y = protein_category, fill = avg_calories)) +
      geom_tile(color = "white", size = 0.5) +
      geom_text(aes(label = round(avg_calories, 0)), color = "white", size = 4, fontface = "bold") +
      scale_fill_gradient2(low = "#3B4CC0", mid = "#B40426", high = "#F0E442", 
                           midpoint = median(heatmap_data$avg_calories, na.rm = TRUE),
                           name = "Avg Calories\nBurned") +
      labs(title = "Average Calorie Burn by Macronutrient Intake",
           subtitle = "Heatmap showing optimal carb-protein combinations",
           x = "Carbohydrate Intake Level",
           y = "Protein Intake Level") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
      )
  })
  
  output$hydration_ridge <- renderPlot({
    data_hydration <- data %>%
      mutate(hydration_level = cut(Water_Intake..liters., 
                                   breaks = quantile(Water_Intake..liters., probs = seq(0, 1, 0.25), na.rm = TRUE),
                                   labels = c("Low", "Moderate", "Good", "Excellent"),
                                   include.lowest = TRUE))
    
    ggplot(data_hydration, aes(x = Calories_Burned, y = hydration_level, fill = stat(x))) +
      geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
      scale_fill_viridis_c(option = "turbo", name = "Calories\nBurned") +
      labs(title = "Impact of Hydration on Calorie Burn Distribution",
           subtitle = "Ridge plots showing performance across hydration levels",
           x = "Calories Burned",
           y = "Hydration Level") +
      theme_ridges() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
        legend.position = "right"
      )
  })
  
  output$nutrition_ceiling <- renderPlot({
    nutrition_ceiling <- data %>%
      mutate(
        nutrition_score = (
          (Proteins / Weight..kg. * 10) +
            (Water_Intake..liters. * 15) +
            (50 - abs(50 - (Carbs / (Carbs + Proteins + Fats) * 100)))
        ) / 2,
        nutrition_quality = cut(nutrition_score,
                                breaks = quantile(nutrition_score, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE),
                                labels = c("Poor Nutrition", "Average Nutrition", "Excellent Nutrition")),
        training_volume = Workout_Frequency..days.week. * Session_Duration..hours.,
        training_level = cut(training_volume,
                             breaks = c(0, 3, 6, Inf),
                             labels = c("Low Training", "Medium Training", "High Training"))
      ) %>%
      filter(!is.na(nutrition_quality) & !is.na(training_level))
    
    ggplot(nutrition_ceiling, aes(x = training_volume, y = Calories_Burned, 
                                  color = nutrition_quality)) +
      geom_point(alpha = 0.5, size = 2) +
      geom_smooth(method = "loess", se = TRUE, size = 1.5, alpha = 0.2) +
      geom_hline(data = nutrition_ceiling %>% 
                   group_by(nutrition_quality) %>% 
                   summarise(ceiling = quantile(Calories_Burned, 0.95, na.rm = TRUE)),
                 aes(yintercept = ceiling, color = nutrition_quality),
                 linetype = "dashed", size = 1.2) +
      annotate("text", x = 10, y = 2500, 
               label = "Performance Ceiling →", 
               size = 4, fontface = "bold", color = "gray30") +
      scale_color_manual(values = c("Poor Nutrition" = "#D73027",
                                    "Average Nutrition" = "#FEE08B",
                                    "Excellent Nutrition" = "#1A9850"),
                         name = "Nutrition Quality") +
      scale_x_continuous(expand = c(0,0), limits = c(0, NA)) +
      scale_y_continuous(expand = c(0,0), limits = c(0, NA)) +
      labs(title = "Can You Out-Train a Bad Diet? The Performance Ceiling Effect",
           subtitle = "Dashed lines show performance ceilings | Poor nutrition caps results regardless of training volume",
           x = "Training Volume (Hours per Week)",
           y = "Calories Burned per Session") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40", margin = margin(b = 12)),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
  })
  
  output$nutrition_efficiency_plot <- renderPlot({
    ggplot(data, aes(x = Nutrition_Efficiency_Ratio, y = Calories_Burned)) +
      geom_point(
        aes(color = Nutrition_Efficiency_Ratio),
        alpha = 0.3,
        size = 2.5
      ) +
      geom_smooth(
        method = "loess",
        se = TRUE,
        color = "#E74C3C",
        fill = "#E74C3C",
        alpha = 0.2,
        size = 1.5,
        level = 0.95
      ) +
      geom_smooth(
        method = "lm",
        se = FALSE,
        color = "#3498DB",
        linetype = "dashed",
        size = 1.2,
        alpha = 0.7
      ) +
      scale_color_gradientn(
        colors = c("#3498DB", "#9B59B6", "#E74C3C"),
        name = "Efficiency\nRatio"
      ) +
      geom_vline(
        xintercept = 1,
        linetype = "dotted",
        color = "#2C3E50",
        size = 1,
        alpha = 0.5
      ) +
      labs(
        title = "Nutrition Efficiency Trend Analysis: Does Eating Less Boost Performance?",
        subtitle = "Smoothed trend with 95% confidence interval | Points colored by efficiency ratio",
        x = "Nutrition Efficiency Ratio (Calories Burned ÷ Calories Consumed)",
        y = "Calories Burned per Session",
        caption = "Note: Shaded area represents 95% confidence interval"
      ) +
      scale_x_continuous(
        breaks = seq(0, max(data$Nutrition_Efficiency_Ratio, na.rm = TRUE), by = 0.5),
        limits = c(0, NA)
      ) +
      scale_y_continuous(
        labels = comma,
        limits = c(0, NA)
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = "#2C3E50", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40", margin = margin(b = 15)),
        plot.caption = element_text(size = 9, color = "gray50", hjust = 1, margin = margin(t = 10)),
        axis.title = element_text(size = 12, face = "bold", color = "#2C3E50"),
        axis.text = element_text(color = "#34495E", size = 11),
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 11),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90", size = 0.3)
      )
  })
  
  # =====================================
  # NEW: PLOT 4 - Hydration × Nutrition Performance Path
  # =====================================
  output$plot4_hydration_nutrition_path <- renderPlot({
    path_data <- data %>%
      mutate(
        nutrition_score = (Proteins / Weight..kg. * 10) + 
          (Water_Intake..liters. * 15) +
          (50 - abs(50 - (Carbs / (Carbs + Proteins + Fats) * 100))) / 2,
        hydration_group = cut(Water_Intake..liters., 
                              breaks = quantile(Water_Intake..liters., 
                                                probs = c(0, 0.25, 0.5, 0.75, 1), 
                                                na.rm = TRUE),
                              labels = c("Low", "Moderate", "Good", "Excellent"),
                              include.lowest = TRUE)
      ) %>%
      filter(!is.na(hydration_group) & !is.na(nutrition_score)) %>%
      group_by(hydration_group) %>%
      summarise(
        avg_nutrition = mean(nutrition_score, na.rm = TRUE),
        avg_calories = mean(Calories_Burned, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(hydration_group)
    
    ggplot(path_data, aes(x = avg_nutrition, y = avg_calories)) +
      geom_path(arrow = arrow(type = "closed", length = unit(0.3, "cm")),
                color = "#003F8C", size = 1.5, alpha = 0.7) +
      geom_point(aes(color = hydration_group), size = 4) +
      scale_color_manual(values = c("Low" = "#D73027", 
                                    "Moderate" = "#FEE08B",
                                    "Good" = "#91BFDB",
                                    "Excellent" = "#1A9850")) +
      scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
      labs(
        title = "Hydration × Nutrition Performance Pathway",
        subtitle = "Arrow shows progression from low to excellent hydration",
        x = "Average Nutrition Score",
        y = "Average Calories Burned",
        color = "Hydration Level"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.minor = element_blank()
      )
  })
  
  # =====================================
  # TAB 4: WORKOUT ANALYSIS
  # =====================================
  
  output$bpm_workout_boxplot <- renderPlot({
    
    metric <- ifelse(input$metric_selector == "Average BPM", "Avg_BPM", "Calories_Burned")
    y_label <- input$metric_selector
    
    df_plot <- data %>%
      select(Workout_Type, Burns_Calories_Bin, Avg_BPM, Calories_Burned) %>%
      mutate(
        Workout_Type = as.factor(Workout_Type),
        Burns_Calories_Bin = factor(
          Burns_Calories_Bin,
          levels = c("Low", "Medium", "High", "Very High")
        )
      )
    
    ggplot(df_plot, aes(
      x = Workout_Type,
      y = .data[[metric]],
      fill = Burns_Calories_Bin
    )) +
      geom_boxplot(alpha = 0.85, width = 0.65) +
      scale_fill_manual(
        values = c(
          "Low" = "#D6E9FF",
          "Medium" = "#8BB8FF",
          "High" = "#4A90E2",
          "Very High" = "#003F8C"
        ),
        name = "Intensity Level"
      ) +
      labs(
        title = paste0(y_label, " by Workout Type"),
        x = "Workout Type",
        y = y_label
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          size = 16, face = "bold", color = "#003F8C", hjust = 0.5
        ),
        axis.text.x = element_text(angle = 0 , hjust = 0.5, face = "bold", size = 11),
        axis.title = element_text(size = 13, color = "#003F8C"),
        legend.position = "right",
        legend.title = element_text(face = "bold")
      )
  })
  
  
  #Duration VS calories
  output$duration_calories <- renderPlot({
    df_scatter <- data %>%
      select(Session_Duration..hours., Calories_Burned, Workout_Type) %>%
      rename(
        Duration = Session_Duration..hours.,
        Calories = Calories_Burned
      )
    
    ggplot(df_scatter, aes(x = Duration, y = Calories, color = Calories)) +
      geom_point(alpha = 0.65, size = 2.8) +
      geom_smooth(method = "lm", color = "#003F8C", linewidth = 1.2, se = FALSE) +
      scale_color_gradientn(
        colors = c("#D6E9FF", "#67A9CF", "#003F8C"),
        name = "Calories\nBurned"
      ) +
      labs(
        title = "Workout Duration vs Calories Burned",
        x = "Workout Duration (hours)",
        y = "Calories Burned"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = "#003F8C",hjust = 0.5),
        axis.title = element_text(size = 13, color = "#003F8C"),
        axis.text = element_text(color = "#003F8C"),
        legend.title = element_text(face = "bold", color = "#003F8C"),
        legend.text = element_text(color = "#003F8C")
      )
  })
  
  output$frequency_calories <- renderPlot({
    data_freq <- data %>%
      mutate(Workout_Freq_Group = round(Workout_Frequency..days.week.))
    
    ggplot(data_freq, aes(x = as.factor(Workout_Freq_Group), y = Calories_Burned)) +
      geom_boxplot(fill = "#8BB8FF", alpha = 0.6, color = "#1f4e79") +
      labs(
        title = "Calories Burned by Workout Frequency",
        x = "Workout Frequency (days/week)",
        y = "Calories Burned"
      ) +
      theme_minimal(base_size = 14) + 
      theme(
        plot.title = element_text(size = 16, face = "bold", color = "#003F8C",hjust = 0.5),
        axis.title = element_text(size = 13, color = "#003F8C")
      )
  })
  
  output$sweet_spot_matrix <- renderPlot({
    
    sweet_spot <- data %>%
      mutate(
        duration_cat = cut(Session_Duration..hours.,
                           breaks = c(0, 0.8, 1.3, Inf),
                           labels = c("Short (<0.8h)", "Medium (0.8–1.3h)", "Long (>1.3h)")),
        
        frequency_cat = cut(Workout_Frequency..days.week.,
                            breaks = c(0, 2.5, 4.5, Inf),
                            labels = c("Low (1–2)", "Medium (3–4)", "High (5+)")),
        
        intensity = (Avg_BPM - Resting_BPM) / (Max_BPM - Resting_BPM),
        
        intensity_cat = cut(intensity,
                            breaks = c(0, 0.6, 0.8, 1),
                            labels = c("Low", "Medium", "High"))
      ) %>%
      filter(!is.na(duration_cat) & !is.na(frequency_cat) & !is.na(intensity_cat)) %>%
      group_by(duration_cat, frequency_cat, intensity_cat) %>%
      summarise(
        avg_calories = mean(Calories_Burned, na.rm = TRUE),
        recovery = mean((Max_BPM - Avg_BPM) / (Avg_BPM - Resting_BPM), na.rm = TRUE),
        efficiency = mean(Calories_Burned / Session_Duration..hours., na.rm = TRUE),
        count = n(),
        .groups = "drop"
      ) %>%
      filter(count > 10) %>%
      mutate(
        sweet_spot_score = scales::rescale(efficiency) * 0.5 +
          scales::rescale(avg_calories) * 0.3 +
          scales::rescale(recovery) * 0.2
      )
    
    sweet_spot$intensity_cat <- recode(
      sweet_spot$intensity_cat,
      "Low" = "High",
      "High" = "Low",
      "Medium" = "Medium"
    )
    
    sweet_spot$intensity_cat <- factor(
      sweet_spot$intensity_cat,
      levels = c("Low", "Medium", "High")
    )
    
    sweet_spot$frequency_cat <- factor(
      sweet_spot$frequency_cat,
      levels = c("Low (1–2)", "Medium (3–4)", "High (5+)")
    )
    
    sweet_spot$duration_cat <- factor(
      sweet_spot$duration_cat,
      levels = c("Short (<0.8h)", "Medium (0.8–1.3h)", "Long (>1.3h)")
    )
    
    ggplot(sweet_spot, aes(x = frequency_cat, y = duration_cat, fill = sweet_spot_score)) +
      geom_tile(color = "white", size = 1.5) +
      geom_text(aes(label = paste0(
        round(sweet_spot_score * 100, 0), "%", 
        "\n", round(avg_calories, 0), " cal"
      )),
      color = "white", size = 3.4, fontface = "bold", lineheight = 0.9) +
      facet_wrap(~ intensity_cat, ncol = 3) +
      scale_fill_gradient2(
        low = "#2166AC", mid = "#FEE08B", high = "#D73027",
        midpoint = 0.5,
        name = "Sweet Spot\nScore (%)"
      ) +
      labs(
        title = "The Sweet Spot Matrix: Optimal Training Parameters",
        subtitle = "★ Score% | Calories — Best results from balanced duration + high frequency + medium intensity",
        x = "Workout Frequency (days/week)",
        y = "Session Duration"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray40", margin = margin(b = 12)),
        axis.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 11, face = "bold"),
        strip.text = element_text(face = "bold", size = 11, color = "white"),
        strip.background = element_rect(fill = "gray30"),
        legend.position = "right",
        panel.grid = element_blank()
      )
  })
  
  output$cardio_load_plot <- renderPlot({
    ggplot(data, aes(x = Cardio_Load_Score, fill = Gender)) +
      geom_density(alpha = 0.7, color = "white", size = 1) +
      facet_wrap(~ Workout_Type, ncol = 2, scales = "free_y") +
      scale_fill_manual(
        values = c("Male" = "#3498DB", "Female" = "#E91E63"),
        name = "Gender"
      ) +
      labs(
        title = "Cardio Load Score by Workout Type and Gender",
        subtitle = "Comparative cardiovascular demand across different workout modalities",
        x = "Cardio Load Score",
        y = "Density"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = "#2C3E50", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40", margin = margin(b = 12)),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(face = "bold", size = 11, color = "white"),
        strip.background = element_rect(fill = "#34495E"),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
  })
  
  # =====================================
  # NEW: PLOT 9 - Workout Performance with Error Bars
  # =====================================
  output$plot9_workout_performance_uncertainty <- renderPlot({
    workout_stats <- data %>%
      group_by(Workout_Type) %>%
      summarise(
        mean_cal = mean(Calories_Burned, na.rm = TRUE),
        sd_cal = sd(Calories_Burned, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      mutate(
        se_cal = sd_cal / sqrt(n),
        ci_lower = mean_cal - 1.96 * se_cal,
        ci_upper = mean_cal + 1.96 * se_cal
      ) %>%
      arrange(desc(mean_cal))
    
    ggplot(workout_stats, aes(x = reorder(Workout_Type, mean_cal), y = mean_cal)) +
      geom_col(fill = "#4A90E2", alpha = 0.7) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                    width = 0.3, size = 1, color = "#003F8C") +
      geom_text(aes(label = round(mean_cal, 0)), 
                hjust = -0.3, fontface = "bold", size = 3.5) +
      coord_flip() +
      labs(
        title = "Average Calories Burned by Workout Type",
        subtitle = "Error bars represent 95% confidence intervals",
        x = "Workout Type",
        y = "Average Calories Burned"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
        axis.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold")
      )
  })
  
  # =====================================
  # NEW: PLOT 10 - Workout Performance Over Experience
  # =====================================
  output$plotb_timeseries <- renderPlot({
    timeseries_data <- data %>%
      mutate(
        Experience_Bin = cut(Experience_Level, 
                             breaks = seq(1, 3, by = 0.2),
                             include.lowest = TRUE)
      ) %>%
      group_by(Experience_Bin, Workout_Type) %>%
      summarise(
        avg_calories = mean(Calories_Burned, na.rm = TRUE),
        avg_duration = mean(Session_Duration..hours., na.rm = TRUE),
        count = n(),
        .groups = "drop"
      ) %>%
      filter(count > 5) %>%
      mutate(Experience_Numeric = as.numeric(Experience_Bin))
    
    # Reorder legend
    timeseries_data$Workout_Type <- factor(
      timeseries_data$Workout_Type,
      levels = c("HIIT", "Strength", "Cardio", "Yoga")
    )
    
    ggplot(timeseries_data, aes(x = Experience_Numeric, y = avg_calories, 
                                color = Workout_Type, group = Workout_Type)) +
      geom_line(size = 1.5, alpha = 0.8) +
      geom_point(size = 3, alpha = 0.9) +
      scale_color_brewer(palette = "Set2") +
      labs(
        title = "Workout Performance Progression by Experience Level",
        subtitle = "How calorie burn evolves as users gain experience",
        x = "Experience Level (Beginner → Advanced)",
        y = "Average Calories Burned",
        color = "Workout Type"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
        legend.position = "right",
        panel.grid.minor = element_blank()
      )
  })
  
  # =====================================
  # NEW: PLOT 11 - Sweet Spot Contour Density
  # =====================================
  output$plotj_contour_density <- renderPlot({
    ggplot(data, aes(x = Workout_Frequency..days.week., 
                     y = Session_Duration..hours.)) +
      stat_density_2d(aes(fill = after_stat(level)), 
                      geom = "polygon", alpha = 0.5) +
      geom_point(aes(color = Calories_Burned), alpha = 0.3, size = 1.5) +
      scale_fill_gradient(low = "#D6E9FF", high = "#003F8C", 
                          name = "Density") +
      scale_color_gradient(low = "#F39C12", high = "#E74C3C",
                           name = "Calories\nBurned") +
      facet_wrap(~ Workout_Type, ncol = 2) +
      labs(
        title = "Workout Parameter Density: Finding the Sweet Spot",
        subtitle = "Contours show common frequency-duration combinations | Points colored by calorie burn",
        x = "Workout Frequency (days/week)",
        y = "Session Duration (hours)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
        strip.text = element_text(face = "bold", size = 11),
        legend.position = "right"
      )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
