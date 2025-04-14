##### Code for Demographic Graphs#####
library(plotrix)
library(ggplot2)
library(dplyr)
library(gt)
library(tidyr)
library(scales)
library(plotly)
# Set background color
par(bg = "lightblue")
# Remove rows with missing Class_Label just in case
PandL_clean <- PandL %>% filter(!is.na(Class_Label))

# Create class_counts table
class_counts <- table(PandL_clean$Class_Label)


# Create the exploding 3D pie chart for Different Classes###
pie3D(
  class_counts,
  labels = labels,
  explode = 0.1,
  height = 0.2,         # Taller slices
  
  main = "Number of Different Account from 2019-2023",
  labelcex = 1.1,
  col = rainbow(length(class_counts))
)

#### Bar plot for Number of Accounts per Year###

#### Need to Summarize to get counts First###

accounts_per_year <- PandL %>%
  
  group_by(Year) %>%
  summarise(account_count = n()) %>%
  arrange(Year)

####Code for the Plot #####
ggplot(accounts_per_year, aes(x = factor(Year), y = account_count)) +
  geom_col(fill = "steelblue", color = "black", width = 0.7) +
  geom_text(aes(label = account_count), vjust = -0.5, size = 5, fontface = "bold") +
  labs(
    title = "Number of Accounts by Year",
    x = "Year",
    y = "Number of Accounts"
  ) +
  ylim(0, 110) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    panel.grid.major.y = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "lightyellow", color = NA),  # Entire plot area
    panel.background = element_rect(fill = "lightyellow", color = NA)  # Plot panel background
  )

####Line graph for net income by year #####
#  Summarize Net Income by Year
net_income_by_year <- PandL %>%
  group_by(Year) %>%
  summarise(Net_Income = sum(Net.Income, na.rm = TRUE)) %>%
  arrange(Year)

#  Calculate max y for padding
max_income <- max(net_income_by_year$Net_Income, na.rm = TRUE)
y_limit <- max_income * 1.25  # Add 25% padding

#  Fancy Line Chart with Dollar Signs as Points
ggplot(net_income_by_year, aes(x = Year, y = Net_Income)) +
  geom_line(color = "steelblue", linewidth = 1.5) +
  
  # Dollar sign symbols as data points
  geom_text(
    aes(label = "$"),
    size = 8,         # Adjust size as needed
    vjust = 0.5,
    color = "darkgreen",
    fontface = "bold"
  ) +
  
  # Actual Net Income values above each point
  geom_text(
    aes(label = comma(round(Net_Income))),
    vjust = -2.5,
    size = 3.5,
    fontface = "bold"
  ) +
  
  labs(
    title = "Total Net Income by Year",
    x = "Year",
    y = "Net Income ($)"
  ) +
  scale_y_continuous(
    labels = dollar_format(),
    limits = c(500000, y_limit)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.background = element_rect(fill = "lightblue", color = NA),
    plot.background = element_rect(fill = "lightgrey", color = NA)
  )

#####  Line Chart for Expenses####

##### Summarize and reshape data  ########
expense_long <- PandL %>%
  group_by(Year) %>%
  summarise(
    Cleaning = sum(Cleaning.Supplies, na.rm = TRUE),
    Paper = sum(Paper.Goods, na.rm = TRUE),
    Payroll = sum(Payroll.Expenses, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(Cleaning, Paper, Payroll),
    names_to = "Expense_Type",
    values_to = "Amount"
  )

# ###### Y-axis padding
max_expense <- max(expense_long$Amount, na.rm = TRUE)
y_limit <- max_expense * 1.2

###### Fancy Plot #########
ggplot(expense_long, aes(x = Year, y = Amount, group = Expense_Type, color = Expense_Type)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  geom_text(
    aes(label = comma(round(Amount))),
    vjust = -1.8,
    size = 3,
    fontface = "bold",
    color = "black"  # Make number labels black
  ) +
  labs(
    title = "Expenses by Category Over Time",
    x = "Year",
    y = "Total Expense ($)",
    color = "Expense Type"
  ) +
  scale_y_continuous(
    labels = dollar_format(),
    limits = c(0, y_limit)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "lightyellow", color = NA)
  )



##### 3D Plot of Net Income by Year by Payroll ####
PandL$Year <- as.factor(PandL$Year)
PandL$YearNum <- as.numeric(as.character(PandL$Year))  # Needed for axis ordering

plot_ly(PandL, 
        x = ~Payroll.Expenses,        # X = Payroll
        y = ~YearNum,                 # Y = Year
        z = ~Net.Income, 
        color = ~Year,
        colors = c("cyan", "orange", "pink", "yellow", "purple"),
        type = "scatter3d", 
        mode = "lines+markers",
        marker = list(
          size = 8,
          line = list(color = 'black', width = 2)
        ),
        line = list(width = 5),
        width = 1000,   # <-- WIDTH in pixels
        height = 700    # <-- HEIGHT in pixels
) %>%
  layout(
    margin = list(l = 100, r = 100, b = 80, t = 80),
    scene = list(
      camera = list(eye = list(x = 2, y = -1.8, z = 1.2)),  # Tilted left
      xaxis = list(
        title = list(text = "Payroll Expenses", font = list(size = 14)),
        tickfont = list(size = 11)
      ),
      yaxis = list(
        title = list(text = "Year", font = list(size = 14)),
        tickvals = c(2019, 2020, 2021, 2022, 2023),
        ticktext = c("2019", "2020", "2021", "2022", "2023"),
        tickfont = list(size = 11)
      ),
      zaxis = list(
        title = list(text = "Net Income", font = list(size = 14)),
        tickfont = list(size = 11)
      )
    ),
    title = list(
      text = "Predicted Net Income by Payroll and Year",
      font = list(size = 20),
      x = 0.5
    ),
    plot_bgcolor = 'white',
    paper_bgcolor = 'white'
  )

