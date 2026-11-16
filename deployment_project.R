# Homework 7: Deploying your Model
# Step 1: Train the Model
# Load dataset
credit_data <- read.csv("credit_default.csv")

# Rename columns
colnames(credit_data) <- c(
  "ID", "LIMIT_BAL", "SEX", "EDUCATION", "MARRIAGE", "AGE",
  "PAY_0", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6",
  "BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4",
  "BILL_AMT5", "BILL_AMT6",
  "PAY_AMT1", "PAY_AMT2", "PAY_AMT3",
  "PAY_AMT4", "PAY_AMT5", "PAY_AMT6",
  "DEFAULT"
)

# Select variables for the regression model
credit_clean <- credit_data[, c(2, 6, 7, 13)]

colnames(credit_clean) <- c(
  "LIMIT_BAL",
  "AGE",
  "PAY_0",
  "BILL_AMT1"
)

# Convert variables to numeric
credit_clean$LIMIT_BAL <- as.numeric(as.character(credit_clean$LIMIT_BAL))
credit_clean$AGE <- as.numeric(as.character(credit_clean$AGE))
credit_clean$PAY_0 <- as.numeric(as.character(credit_clean$PAY_0))
credit_clean$BILL_AMT1 <- as.numeric(as.character(credit_clean$BILL_AMT1))

# Remove missing values
credit_clean <- na.omit(credit_clean)

# Review the data
summary(credit_clean)
str(credit_clean)

# Train the regression model
bill_model <- lm(
  BILL_AMT1 ~ LIMIT_BAL + AGE + PAY_0,
  data = credit_clean
)

# View model summary
summary(bill_model)

# Test prediction on new data
new_customer <- data.frame(
  LIMIT_BAL = 150000,
  AGE = 35,
  PAY_0 = 0
)

predict(bill_model, newdata = new_customer)

# Step 2: Save the Model
saveRDS(bill_model, "bill_prediction_model.rds")

# Step 3. Create a Prediction Function
predict_bill <- function(limit_bal, age, pay_0) {
  
  model <- readRDS("bill_prediction_model.rds")
  
  new_customer <- data.frame(
    LIMIT_BAL = limit_bal,
    AGE = age,
    PAY_0 = pay_0
  )
  
  prediction <- predict(model, newdata = new_customer)
  
  return(round(prediction, 2))
}

# Run an Example
predict_bill(
  limit_bal = 150000,
  age = 35,
  pay_0 = 0
)

# Step 4. Deploy with Shiny
install.packages("promises")
install.packages("shiny")

library(shiny)

# Load the saved regression model
model <- readRDS("bill_prediction_model.rds")

# User Interface (UI)
ui <- fluidPage(
  
  # Title displayed at the top of the application
  titlePanel("Credit Card Bill Predictor"),
  
  # Create a sidebar layout
  sidebarLayout(
    
    # Sidebar contains the user inputs
    sidebarPanel(
      
      # Input for the customer's credit limit
      numericInput(
        "limit",
        "Credit Limit",
        value = 100000),
      
      # Input for the customer's age
      numericInput(
        "age",
        "Age",
        value = 30),
      
      # Input for the customer's repayment status (PAY_0)
      numericInput(
        "pay",
        "PAY_0 Repayment Status",
        value = 0),
      
      # Button to calculate the prediction
      actionButton("predict", "Predict")),
    
    # Main panel displays the prediction
    mainPanel(h3("Predicted September Bill Amount"),
              
              # Output area for the predicted value
              verbatimTextOutput("prediction"))))

# Server Logic
server <- function(input, output) {
  
  # Run the prediction when the button is clicked
  observeEvent(input$predict, {
    
    # Create a new data frame from the user's inputs
    new_customer <- data.frame(
      LIMIT_BAL = input$limit,
      AGE = input$age,
      PAY_0 = input$pay)
    
    # Generate the prediction using the saved model
    pred <- predict(model, newdata = new_customer)
    
    # Display the prediction rounded to two decimal places
    output$prediction <- renderPrint({
      round(pred, 2)})})}

# Launch the Shiny application
shinyApp(ui, server)