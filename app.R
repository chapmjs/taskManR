# Add note from within the edit task modal
observeEvent(input$addNoteInEdit, {
  req(rv$selectedTaskID, input$editTaskNewNote != "")
  
  # Create new note
  newNote <- data.frame(
    NoteID = getNewNoteID(),
    TaskID = rv$selectedTaskID,
    NoteDateTime = as.character(now()),
    NoteText = input$editTaskNewNote,
    stringsAsFactors = FALSE
  )
  
  # Add to database
  add_note(db_path, newNote)
  
  # Add to notes dataframe
  rv$notes <- rbind(rv$notes, newNote)
  
  # Reset input
  updateTextAreaInput(session, "editTaskNewNote", value = "")
  
  showNotification("Note added", type = "message")
})# Task Manager Shiny App with SQLite Persistence and Authentication
# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(writexl)
library(dplyr)
library(lubridate)
library(DBI)
library(RSQLite)
library(shinymanager) # For authentication

# Database setup function - creates tables if they don't exist
setup_database <- function(db_path) {
  # Connect to SQLite database (creates it if it doesn't exist)
  db <- dbConnect(RSQLite::SQLite(), db_path)
  
  # Create tasks table if it doesn't exist
  if (!dbExistsTable(db, "tasks")) {
    dbExecute(db, "CREATE TABLE tasks (
      TaskID INTEGER PRIMARY KEY,
      TaskName TEXT NOT NULL,
      TaskCreateDateTime TEXT NOT NULL,
      TaskCategory TEXT NOT NULL,
      importance TEXT,
      urgency TEXT,
      status TEXT,
      estimated_time REAL
    )")
  }
  
  # Create notes table if it doesn't exist
  if (!dbExistsTable(db, "notes")) {
    dbExecute(db, "CREATE TABLE notes (
      NoteID INTEGER PRIMARY KEY,
      TaskID INTEGER NOT NULL,
      NoteDateTime TEXT NOT NULL,
      NoteText TEXT,
      FOREIGN KEY (TaskID) REFERENCES tasks(TaskID)
    )")
  }
  
  # Create categories table if it doesn't exist
  if (!dbExistsTable(db, "categories")) {
    dbExecute(db, "CREATE TABLE categories (
      CategoryID INTEGER PRIMARY KEY,
      CategoryName TEXT NOT NULL UNIQUE
    )")
    
    # Insert default categories
    categories <- data.frame(
      CategoryID = 1:46,
      CategoryName = c(
        "God",
        "Relationship with God",
        "Spouse",
        "Family",
        "Family Finances",
        "Kids One on One",
        "Family Finances",
        "Family History",
        "Family Learning",
        "Extended Family",
        "Family Chores",
        "Church",
        "Young Men",
        "Seminary",
        "Work",
        "Work-Education",
        "Acequia Group",
        "Research",
        "Learning Analytics",
        "Team-Based Learning",
        "EVI",
        "NVS",
        "NVS Idaho",
        "SVU",
        "BUS 498 Strategic Management",
        "BUS 250 Biz Analysis Excel",
        "Student Advising",
        "BUS 355 Data Science",
        "TBL Research",
        "BYUI",
        "BA 211",
        "SCM 461",
        "SCM 478",
        "SCM 361",
        "IBC",
        "BYUI TBL",
        "SCM Society",
        "BYUI Prof-Dev",
        "BYUI-Pathways",
        "Career-Job Search",
        "Community-Friends",
        "Hobby-Interest",
        "Personal",
        "Personal-Fitness",
        "Work",
        "Home"
      ),
      stringsAsFactors = FALSE
    )
    
    # Remove duplicate categories (keeping first occurrence)
    categories <- categories[!duplicated(categories$CategoryName),]
    
    # Reset IDs to be sequential
    categories$CategoryID <- 1:nrow(categories)
    
    # Insert categories into database
    dbWriteTable(db, "categories", categories, append = TRUE, row.names = FALSE)
  }
  
  # Disconnect from database
  dbDisconnect(db)
  
  return(TRUE)
}

# Database helper functions
get_all_tasks <- function(db_path) {
  db <- dbConnect(RSQLite::SQLite(), db_path)
  tasks <- dbGetQuery(db, "SELECT * FROM tasks")
  dbDisconnect(db)
  return(tasks)
}

get_all_notes <- function(db_path) {
  db <- dbConnect(RSQLite::SQLite(), db_path)
  notes <- dbGetQuery(db, "SELECT * FROM notes")
  dbDisconnect(db)
  return(notes)
}

get_all_categories <- function(db_path) {
  db <- dbConnect(RSQLite::SQLite(), db_path)
  categories <- dbGetQuery(db, "SELECT * FROM categories ORDER BY CategoryName")
  dbDisconnect(db)
  return(categories)
}

get_max_task_id <- function(db_path) {
  db <- dbConnect(RSQLite::SQLite(), db_path)
  max_id <- dbGetQuery(db, "SELECT MAX(TaskID) as max_id FROM tasks")$max_id
  dbDisconnect(db)
  return(ifelse(is.na(max_id), 0, max_id))
}

get_max_note_id <- function(db_path) {
  db <- dbConnect(RSQLite::SQLite(), db_path)
  max_id <- dbGetQuery(db, "SELECT MAX(NoteID) as max_id FROM notes")$max_id
  dbDisconnect(db)
  return(ifelse(is.na(max_id), 0, max_id))
}

get_max_category_id <- function(db_path) {
  db <- dbConnect(RSQLite::SQLite(), db_path)
  max_id <- dbGetQuery(db, "SELECT MAX(CategoryID) as max_id FROM categories")$max_id
  dbDisconnect(db)
  return(ifelse(is.na(max_id), 0, max_id))
}

add_task <- function(db_path, task) {
  db <- dbConnect(RSQLite::SQLite(), db_path)
  dbWriteTable(db, "tasks", task, append = TRUE, row.names = FALSE)
  dbDisconnect(db)
}

add_note <- function(db_path, note) {
  db <- dbConnect(RSQLite::SQLite(), db_path)
  dbWriteTable(db, "notes", note, append = TRUE, row.names = FALSE)
  dbDisconnect(db)
}

add_category <- function(db_path, category) {
  db <- dbConnect(RSQLite::SQLite(), db_path)
  dbWriteTable(db, "categories", category, append = TRUE, row.names = FALSE)
  dbDisconnect(db)
}

update_task <- function(db_path, task_id, updates) {
  db <- dbConnect(RSQLite::SQLite(), db_path)
  
  # Build SET clause for SQL
  set_clause <- paste(names(updates), "=", paste0("'", updates, "'"), collapse = ", ")
  
  # Construct and execute the UPDATE statement
  sql <- sprintf("UPDATE tasks SET %s WHERE TaskID = %d", set_clause, task_id)
  dbExecute(db, sql)
  
  dbDisconnect(db)
}

update_category <- function(db_path, category_id, category_name) {
  db <- dbConnect(RSQLite::SQLite(), db_path)
  
  # Update category name
  sql <- sprintf("UPDATE categories SET CategoryName = '%s' WHERE CategoryID = %d", 
                 category_name, category_id)
  dbExecute(db, sql)
  
  dbDisconnect(db)
}

delete_category <- function(db_path, category_id) {
  db <- dbConnect(RSQLite::SQLite(), db_path)
  
  # Check if category is in use
  query <- sprintf("SELECT COUNT(*) as count FROM tasks WHERE TaskCategory = (SELECT CategoryName FROM categories WHERE CategoryID = %d)", 
                   category_id)
  count <- dbGetQuery(db, query)$count
  
  # Only delete if category is not in use
  if (count == 0) {
    sql <- sprintf("DELETE FROM categories WHERE CategoryID = %d", category_id)
    dbExecute(db, sql)
    dbDisconnect(db)
    return(TRUE)
  } else {
    dbDisconnect(db)
    return(FALSE)
  }
}

is_category_in_use <- function(db_path, category_id) {
  db <- dbConnect(RSQLite::SQLite(), db_path)
  
  # Get category name first
  cat_name_query <- sprintf("SELECT CategoryName FROM categories WHERE CategoryID = %d", category_id)
  category_name <- dbGetQuery(db, cat_name_query)$CategoryName
  
  # Check if any tasks use this category
  query <- sprintf("SELECT COUNT(*) as count FROM tasks WHERE TaskCategory = '%s'", category_name)
  count <- dbGetQuery(db, query)$count
  
  dbDisconnect(db)
  return(count > 0)
}

# Setup authentication credentials (in real world, use more secure methods)
credentials <- data.frame(
  user = c("admin", "user"),
  password = c("admin123", "user123"),
  admin = c(TRUE, FALSE),
  stringsAsFactors = FALSE
)

# Define UI
ui <- function(request) {
  dashboardPage(
    dashboardHeader(title = "Task Manager"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Task List", tabName = "tasklist", icon = icon("list")),
        menuItem("Add Task", tabName = "addtask", icon = icon("plus")),
        menuItem("Task Notes", tabName = "tasknotes", icon = icon("sticky-note")),
        menuItem("Settings", tabName = "settings", icon = icon("cog"))
      )
    ),
    dashboardBody(
      tabItems(
        # Dashboard tab
        tabItem(tabName = "dashboard",
                fluidRow(
                  valueBoxOutput("totalTasks"),
                  valueBoxOutput("openTasks"),
                  valueBoxOutput("closedTasks")
                ),
                fluidRow(
                  box(title = "Tasks by Category", status = "primary", solidHeader = TRUE,
                      plotOutput("categoryPlot", height = 250)),
                  box(title = "Tasks by Status", status = "primary", solidHeader = TRUE,
                      plotOutput("statusPlot", height = 250))
                ),
                fluidRow(
                  box(title = "Recent Tasks", status = "info", solidHeader = TRUE,
                      DTOutput("recentTasks"))
                )
        ),
        
        # Task List tab
        tabItem(tabName = "tasklist",
                fluidRow(
                  box(width = 12, title = "Filter Tasks", status = "primary", solidHeader = TRUE,
                      fluidRow(
                        column(3, selectInput("statusFilter", "Status:", 
                                              c("All", "Idea", "Open", "Closed"), selected = "All")),
                        column(3, selectInput("categoryFilter", "Category:", c("All"), selected = "All")),
                        column(3, textInput("nameFilter", "Search by Name:", "")),
                        column(3, actionButton("applyFilter", "Apply Filter", icon = icon("filter"), 
                                               class = "btn-primary"))
                      )
                  )
                ),
                fluidRow(
                  box(width = 12, title = "Task List", status = "primary", solidHeader = TRUE,
                      DTOutput("taskTable"),
                      actionButton("editTaskBtn", "Edit Selected Task", class = "btn-info"),
                      actionButton("changeStatusBtn", "Change Status", class = "btn-warning")
                  )
                )
        ),
        
        # Add Task tab
        tabItem(tabName = "addtask",
                fluidRow(
                  box(width = 12, title = "Add New Task", status = "success", solidHeader = TRUE,
                      fluidRow(
                        column(6, textInput("taskName", "Task Name:", "")),
                        column(6, selectInput("taskCategory", "Category:", c("Select a category" = "")))
                      ),
                      fluidRow(
                        column(4, selectInput("importance", "Importance:", 
                                              c("Low", "Medium", "High"), selected = "Medium")),
                        column(4, selectInput("urgency", "Urgency:", 
                                              c("Low", "Medium", "High"), selected = "Medium")),
                        column(4, selectInput("status", "Status:", 
                                              c("Idea", "Open", "Closed"), selected = "Open"))
                      ),
                      fluidRow(
                        column(6, numericInput("estTime", "Estimated Time (hours):", 1, min = 0.1, step = 0.5)),
                        column(6, textAreaInput("initialNote", "Initial Note:", ""))
                      ),
                      actionButton("addTaskBtn", "Add Task", class = "btn-success")
                  )
                )
        ),
        
        # Task Notes tab
        tabItem(tabName = "tasknotes",
                fluidRow(
                  box(width = 6, title = "Select Task", status = "info", solidHeader = TRUE,
                      selectInput("taskForNotes", "Choose Task:", c("Select a task" = "")),
                      DTOutput("taskNotesList")
                  ),
                  box(width = 6, title = "Add Note", status = "success", solidHeader = TRUE,
                      textAreaInput("newNote", "New Note:", ""),
                      actionButton("addNoteBtn", "Add Note", class = "btn-success")
                  )
                )
        ),
        
        # Categories tab
        tabItem(tabName = "categories",
                fluidRow(
                  box(width = 6, title = "Category List", status = "primary", solidHeader = TRUE,
                      DTOutput("categoryTable"),
                      br(),
                      actionButton("editCategoryBtn", "Edit Selected Category", class = "btn-info"),
                      actionButton("deleteCategoryBtn", "Delete Selected Category", class = "btn-danger")
                  ),
                  box(width = 6, title = "Add New Category", status = "success", solidHeader = TRUE,
                      textInput("newCategoryName", "Category Name:", ""),
                      actionButton("addCategoryBtn", "Add Category", class = "btn-success")
                  )
                )
        ),
        
        # Settings tab
        tabItem(tabName = "settings",
                fluidRow(
                  box(width = 12, title = "Data Management", status = "warning", solidHeader = TRUE,
                      fileInput("uploadExcel", "Upload Excel File:",
                                accept = c(".xlsx", ".xls")),
                      downloadButton("downloadData", "Export Data", class = "btn-primary"),
                      hr(),
                      actionButton("resetData", "Reset All Data", class = "btn-danger"),
                      helpText("Warning: This will delete all task data!")
                  )
                )
        )
      )
    )
  )
}

# Secure the app with authentication
ui <- secure_app(ui, enable_admin = TRUE)

# Define server logic
server <- function(input, output, session) {
  # Check and validate credentials
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # Store user information
  user_info <- reactive({
    credentials[credentials$user == res_auth$user, ]
  })
  
  # Define database path in app directory
  db_path <- "task_manager.sqlite"
  
  # Setup database
  setup_database(db_path)
  
  # Reactive values to store data
  rv <- reactiveValues(
    tasks = data.frame(),
    notes = data.frame(),
    categories = data.frame(),
    maxTaskID = 0,
    maxNoteID = 0,
    maxCategoryID = 0,
    selectedTaskID = NULL,
    selectedCategoryID = NULL
  )
  
  # Initialize data from database
  observe({
    rv$tasks <- get_all_tasks(db_path)
    rv$notes <- get_all_notes(db_path)
    rv$categories <- get_all_categories(db_path)
    rv$maxTaskID <- get_max_task_id(db_path)
    rv$maxNoteID <- get_max_note_id(db_path)
    rv$maxCategoryID <- get_max_category_id(db_path)
    
    # Initialize with sample data if database is empty
    if (nrow(rv$tasks) == 0) {
      # Create sample tasks
      sampleTasks <- data.frame(
        TaskID = 1:3,
        TaskName = c("Create project plan", "Buy groceries", "Schedule meeting"),
        TaskCreateDateTime = as.character(now() - days(c(2, 1, 0))),
        TaskCategory = c("Work", "Home", "Work"),
        importance = c("High", "Medium", "Medium"),
        urgency = c("High", "Medium", "Low"),
        status = c("Open", "Closed", "Idea"),
        estimated_time = c(4, 1, 0.5),
        stringsAsFactors = FALSE
      )
      
      # Create sample notes
      sampleNotes <- data.frame(
        NoteID = 1:4,
        TaskID = c(1, 1, 2, 3),
        NoteDateTime = as.character(now() - hours(c(48, 24, 12, 1))),
        NoteText = c(
          "Initial project planning phase",
          "Need to involve the marketing team",
          "Remember to get milk and eggs",
          "Should include all team leads"
        ),
        stringsAsFactors = FALSE
      )
      
      # Add sample data to database
      db <- dbConnect(RSQLite::SQLite(), db_path)
      dbWriteTable(db, "tasks", sampleTasks, append = TRUE, row.names = FALSE)
      dbWriteTable(db, "notes", sampleNotes, append = TRUE, row.names = FALSE)
      dbDisconnect(db)
      
      # Update reactive values
      rv$tasks <- sampleTasks
      rv$notes <- sampleNotes
      rv$maxTaskID <- 3
      rv$maxNoteID <- 4
    }
    
    # Update UI elements
    if (nrow(rv$tasks) > 0) {
      # Update category filter choices
      updateSelectInput(session, "categoryFilter",
                        choices = c("All", unique(rv$tasks$TaskCategory)))
      
      # Update task selection for notes
      updateSelectInput(session, "taskForNotes",
                        choices = c("Select a task" = "", 
                                    setNames(rv$tasks$TaskID, rv$tasks$TaskName)))
    }
    
    # Update category dropdowns
    if (nrow(rv$categories) > 0) {
      updateSelectInput(session, "taskCategory",
                        choices = setNames(rv$categories$CategoryName, rv$categories$CategoryName))
    }
  })
  
  # Function to generate a new unique TaskID
  getNewTaskID <- function() {
    rv$maxTaskID <- rv$maxTaskID + 1
    return(rv$maxTaskID)
  }
  
  # Function to generate a new unique NoteID
  getNewNoteID <- function() {
    rv$maxNoteID <- rv$maxNoteID + 1
    return(rv$maxNoteID)
  }
  
  # Handle Excel file upload
  observeEvent(input$uploadExcel, {
    req(input$uploadExcel)
    
    # Read the uploaded Excel file
    tryCatch({
      # Assuming the Excel file has two sheets: Tasks and Notes
      tasks_df <- readxl::read_excel(input$uploadExcel$datapath, sheet = "Tasks")
      notes_df <- readxl::read_excel(input$uploadExcel$datapath, sheet = "Notes")
      
      # Convert to data frames and ensure consistent column names
      tasks_df <- as.data.frame(tasks_df)
      notes_df <- as.data.frame(notes_df)
      
      # Connect to database
      db <- dbConnect(RSQLite::SQLite(), db_path)
      
      # Clear existing tables
      dbExecute(db, "DELETE FROM notes")
      dbExecute(db, "DELETE FROM tasks")
      
      # Import new data
      dbWriteTable(db, "tasks", tasks_df, append = TRUE, row.names = FALSE)
      dbWriteTable(db, "notes", notes_df, append = TRUE, row.names = FALSE)
      
      # Disconnect from database
      dbDisconnect(db)
      
      # Update reactive values
      rv$tasks <- tasks_df
      rv$notes <- notes_df
      rv$maxTaskID <- max(tasks_df$TaskID)
      rv$maxNoteID <- max(notes_df$NoteID)
      
      # Update UI elements
      updateSelectInput(session, "categoryFilter",
                        choices = c("All", unique(rv$tasks$TaskCategory)))
      
      updateSelectInput(session, "taskForNotes",
                        choices = c("Select a task" = "", 
                                    setNames(rv$tasks$TaskID, rv$tasks$TaskName)))
      
      showNotification("Data imported successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error importing data:", e$message), type = "error")
    })
  })
  
  # Dashboard outputs
  output$totalTasks <- renderValueBox({
    valueBox(
      nrow(rv$tasks), "Total Tasks",
      icon = icon("tasks"),
      color = "blue"
    )
  })
  
  output$openTasks <- renderValueBox({
    valueBox(
      sum(rv$tasks$status %in% c("Open", "Idea")), "Open Tasks",
      icon = icon("clipboard-list"),
      color = "yellow"
    )
  })
  
  output$closedTasks <- renderValueBox({
    valueBox(
      sum(rv$tasks$status == "Closed"), "Completed Tasks",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$categoryPlot <- renderPlot({
    req(nrow(rv$tasks) > 0)
    category_counts <- table(rv$tasks$TaskCategory)
    barplot(category_counts, main = "", col = "steelblue",
            ylab = "Number of Tasks", las = 2)
  })
  
  output$statusPlot <- renderPlot({
    req(nrow(rv$tasks) > 0)
    status_counts <- table(rv$tasks$status)
    colors <- c("Idea" = "orange", "Open" = "blue", "Closed" = "green")
    
    # Handle case when not all statuses exist in the data
    status_colors <- colors[names(status_counts)]
    
    pie(status_counts, labels = paste0(names(status_counts), " (", status_counts, ")"),
        col = status_colors, main = "")
    legend("topright", legend = names(status_colors), fill = status_colors, cex = 0.8)
  })
  
  output$recentTasks <- renderDT({
    req(nrow(rv$tasks) > 0)
    recent <- rv$tasks %>%
      arrange(desc(TaskCreateDateTime)) %>%
      head(5) %>%
      select(TaskName, TaskCategory, status)
    
    datatable(recent, options = list(dom = 't', paging = FALSE),
              rownames = FALSE)
  })
  
  # Filtered task table
  filteredTasks <- reactive({
    tasks_df <- rv$tasks
    
    # Apply status filter
    if (input$statusFilter != "All") {
      tasks_df <- tasks_df %>% filter(status == input$statusFilter)
    }
    
    # Apply category filter
    if (input$categoryFilter != "All") {
      tasks_df <- tasks_df %>% filter(TaskCategory == input$categoryFilter)
    }
    
    # Apply name filter
    if (input$nameFilter != "") {
      tasks_df <- tasks_df %>% 
        filter(grepl(input$nameFilter, TaskName, ignore.case = TRUE))
    }
    
    return(tasks_df)
  })
  
  output$taskTable <- renderDT({
    tasks <- filteredTasks() %>%
      select(TaskID, TaskName, TaskCategory, importance, urgency, status, estimated_time)
    
    datatable(tasks, selection = 'single', options = list(pageLength = 10),
              rownames = FALSE)
  })
  
  # Task selection for edit/status change
  observeEvent(input$taskTable_rows_selected, {
    req(input$taskTable_rows_selected)
    selectedRow <- input$taskTable_rows_selected
    taskData <- filteredTasks()[selectedRow, ]
    rv$selectedTaskID <- taskData$TaskID
  })
  
  # Handle status change
  observeEvent(input$changeStatusBtn, {
    req(rv$selectedTaskID)
    
    # Show a modal for status change
    showModal(modalDialog(
      title = "Change Task Status",
      selectInput("newStatus", "New Status:",
                  choices = c("Idea", "Open", "Closed")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("saveNewStatus", "Save", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$saveNewStatus, {
    req(rv$selectedTaskID)
    
    # Update the task status in database
    update_task(db_path, rv$selectedTaskID, list(status = input$newStatus))
    
    # Update the task status in reactive data
    taskIndex <- which(rv$tasks$TaskID == rv$selectedTaskID)
    if (length(taskIndex) > 0) {
      rv$tasks$status[taskIndex] <- input$newStatus
      
      # Add a note about status change
      newNote <- data.frame(
        NoteID = getNewNoteID(),
        TaskID = rv$selectedTaskID,
        NoteDateTime = as.character(now()),
        NoteText = paste("Status changed to", input$newStatus),
        stringsAsFactors = FALSE
      )
      
      # Add note to database
      add_note(db_path, newNote)
      
      # Update reactive data
      rv$notes <- rbind(rv$notes, newNote)
      
      showNotification("Task status updated", type = "message")
      removeModal()
    }
  })
  
  # Handle task edit
  observeEvent(input$editTaskBtn, {
    req(rv$selectedTaskID)
    
    # Get the task data
    taskData <- rv$tasks[rv$tasks$TaskID == rv$selectedTaskID, ]
    
    # Ensure categories are loaded for the dropdown
    updateSelectInput(session, "editTaskCategory", 
                      choices = setNames(rv$categories$CategoryName, rv$categories$CategoryName),
                      selected = taskData$TaskCategory)
    
    # Show a modal for editing
    showModal(modalDialog(
      title = paste("Edit Task:", taskData$TaskName),
      size = "l", # Make modal larger
      fluidRow(
        column(6,
               fluidRow(
                 column(12, textInput("editTaskName", "Task Name:", taskData$TaskName)),
                 column(12, selectInput("editTaskCategory", "Category:", 
                                        setNames(rv$categories$CategoryName, rv$categories$CategoryName),
                                        selected = taskData$TaskCategory))
               ),
               fluidRow(
                 column(6, selectInput("editImportance", "Importance:", 
                                       c("Low", "Medium", "High"), 
                                       selected = taskData$importance)),
                 column(6, selectInput("editUrgency", "Urgency:", 
                                       c("Low", "Medium", "High"), 
                                       selected = taskData$urgency))
               ),
               fluidRow(
                 column(12, numericInput("editEstTime", "Est. Time (hours):", 
                                         value = taskData$estimated_time, 
                                         min = 0.1, step = 0.5))
               )
        ),
        column(6,
               h4("Task Notes"),
               # Display the notes for this task
               DTOutput("editTaskNotes"),
               br(),
               # Add a new note
               textAreaInput("editTaskNewNote", "Add Note:", ""),
               actionButton("addNoteInEdit", "Add Note", class = "btn-success")
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("saveTaskEdit", "Save Changes", class = "btn-primary")
      ),
      easyClose = FALSE
    ))
    
    # Render the notes table inside the modal
    output$editTaskNotes <- renderDT({
      req(rv$selectedTaskID)
      task_notes <- rv$notes %>%
        filter(TaskID == rv$selectedTaskID) %>%
        arrange(desc(NoteDateTime)) %>%
        select(NoteDateTime, NoteText)
      
      datatable(task_notes, options = list(pageLength = 5, dom = 't'),
                rownames = FALSE)
    })
  })
  
  observeEvent(input$saveTaskEdit, {
    req(rv$selectedTaskID)
    
    # Prepare updates
    updates <- list(
      TaskName = input$editTaskName,
      TaskCategory = input$editTaskCategory,
      importance = input$editImportance,
      urgency = input$editUrgency,
      estimated_time = input$editEstTime
    )
    
    # Update the task in database
    update_task(db_path, rv$selectedTaskID, updates)
    
    # Update the task in reactive data
    taskIndex <- which(rv$tasks$TaskID == rv$selectedTaskID)
    if (length(taskIndex) > 0) {
      rv$tasks$TaskName[taskIndex] <- input$editTaskName
      rv$tasks$TaskCategory[taskIndex] <- input$editTaskCategory
      rv$tasks$importance[taskIndex] <- input$editImportance
      rv$tasks$urgency[taskIndex] <- input$editUrgency
      rv$tasks$estimated_time[taskIndex] <- input$editEstTime
      
      # Add a note about the edit
      newNote <- data.frame(
        NoteID = getNewNoteID(),
        TaskID = rv$selectedTaskID,
        NoteDateTime = as.character(now()),
        NoteText = "Task details edited",
        stringsAsFactors = FALSE
      )
      
      # Add note to database
      add_note(db_path, newNote)
      
      # Update reactive data
      rv$notes <- rbind(rv$notes, newNote)
      
      # Update task selection dropdown
      updateSelectInput(session, "taskForNotes",
                        choices = c("Select a task" = "", 
                                    setNames(rv$tasks$TaskID, rv$tasks$TaskName)))
      
      # Update category filter choices
      updateSelectInput(session, "categoryFilter",
                        choices = c("All", unique(rv$tasks$TaskCategory)))
      
      # Update edit category dropdown
      updateSelectInput(session, "editTaskCategory", 
                        choices = setNames(rv$categories$CategoryName, rv$categories$CategoryName),
                        selected = input$editTaskCategory)
      
      showNotification("Task updated successfully", type = "message")
      removeModal()
    }
  })
  
  # Add new task
  observeEvent(input$addTaskBtn, {
    req(input$taskName, input$taskCategory != "")
    
    # Create new task
    newTask <- data.frame(
      TaskID = getNewTaskID(),
      TaskName = input$taskName,
      TaskCreateDateTime = as.character(now()),
      TaskCategory = input$taskCategory,
      importance = input$importance,
      urgency = input$urgency,
      status = input$status,
      estimated_time = input$estTime,
      stringsAsFactors = FALSE
    )
    
    # Add to database
    add_task(db_path, newTask)
    
    # Add to reactive data
    rv$tasks <- rbind(rv$tasks, newTask)
    
    # Add initial note if provided
    if (input$initialNote != "") {
      newNote <- data.frame(
        NoteID = getNewNoteID(),
        TaskID = newTask$TaskID,
        NoteDateTime = as.character(now()),
        NoteText = input$initialNote,
        stringsAsFactors = FALSE
      )
      
      # Add note to database
      add_note(db_path, newNote)
      
      # Add to reactive data
      rv$notes <- rbind(rv$notes, newNote)
    }
    
    # Reset input fields
    updateTextInput(session, "taskName", value = "")
    updateTextAreaInput(session, "initialNote", value = "")
    updateNumericInput(session, "estTime", value = 1)
    
    # Update dropdowns
    updateSelectInput(session, "taskForNotes",
                      choices = c("Select a task" = "", 
                                  setNames(rv$tasks$TaskID, rv$tasks$TaskName)))
    
    updateSelectInput(session, "categoryFilter",
                      choices = c("All", unique(rv$tasks$TaskCategory)))
    
    showNotification("Task added successfully!", type = "message")
  })
  
  # Task notes handling
  observeEvent(input$taskForNotes, {
    req(input$taskForNotes)
    rv$selectedTaskID <- as.numeric(input$taskForNotes)
  })
  
  output$taskNotesList <- renderDT({
    req(rv$selectedTaskID)
    task_notes <- rv$notes %>%
      filter(TaskID == rv$selectedTaskID) %>%
      arrange(desc(NoteDateTime)) %>%
      select(NoteDateTime, NoteText)
    
    datatable(task_notes, options = list(pageLength = 5),
              rownames = FALSE)
  })
  
  # Add note to task
  observeEvent(input$addNoteBtn, {
    req(rv$selectedTaskID, input$newNote)
    
    # Create new note
    newNote <- data.frame(
      NoteID = getNewNoteID(),
      TaskID = rv$selectedTaskID,
      NoteDateTime = as.character(now()),
      NoteText = input$newNote,
      stringsAsFactors = FALSE
    )
    
    # Add to database
    add_note(db_path, newNote)
    
    # Add to notes dataframe
    rv$notes <- rbind(rv$notes, newNote)
    
    # Reset input
    updateTextAreaInput(session, "newNote", value = "")
    
    showNotification("Note added", type = "message")
  })
  
  # Export data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("task_manager_export_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx", sep = "")
    },
    content = function(file) {
      # Get the latest data from the database
      latest_tasks <- get_all_tasks(db_path)
      latest_notes <- get_all_notes(db_path)
      
      # Create a list with each dataframe as a sheet
      data_list <- list(Tasks = latest_tasks, Notes = latest_notes)
      writexl::write_xlsx(data_list, file)
    }
  )
  
  # Category management
  output$categoryTable <- renderDT({
    req(rv$categories)
    datatable(rv$categories, selection = 'single', options = list(pageLength = 10),
              rownames = FALSE)
  })
  
  # Category selection
  observeEvent(input$categoryTable_rows_selected, {
    req(input$categoryTable_rows_selected)
    selectedRow <- input$categoryTable_rows_selected
    categoryData <- rv$categories[selectedRow, ]
    rv$selectedCategoryID <- categoryData$CategoryID
  })
  
  # Add new category
  observeEvent(input$addCategoryBtn, {
    req(input$newCategoryName)
    
    # Check if category already exists
    if (input$newCategoryName %in% rv$categories$CategoryName) {
      showNotification("Category already exists!", type = "error")
      return()
    }
    
    # Create new category
    newCategory <- data.frame(
      CategoryID = get_max_category_id(db_path) + 1,
      CategoryName = input$newCategoryName,
      stringsAsFactors = FALSE
    )
    
    # Add to database
    add_category(db_path, newCategory)
    
    # Add to reactive data
    rv$categories <- rbind(rv$categories, newCategory)
    rv$maxCategoryID <- newCategory$CategoryID
    
    # Reset input
    updateTextInput(session, "newCategoryName", value = "")
    
    # Update dropdowns
    updateSelectInput(session, "taskCategory",
                      choices = setNames(rv$categories$CategoryName, rv$categories$CategoryName))
    
    showNotification("Category added successfully!", type = "message")
  })
  
  # Edit category
  observeEvent(input$editCategoryBtn, {
    req(rv$selectedCategoryID)
    
    # Get the category data
    categoryData <- rv$categories[rv$categories$CategoryID == rv$selectedCategoryID, ]
    
    # Show a modal for editing
    showModal(modalDialog(
      title = "Edit Category",
      textInput("editCategoryName", "Category Name:", categoryData$CategoryName),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("saveCategoryEdit", "Save Changes", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$saveCategoryEdit, {
    req(rv$selectedCategoryID, input$editCategoryName)
    
    # Check if new name already exists (and it's not the current category)
    existing <- rv$categories$CategoryName[rv$categories$CategoryID != rv$selectedCategoryID]
    if (input$editCategoryName %in% existing) {
      showNotification("Category name already exists!", type = "error")
      return()
    }
    
    # Get the old category name
    oldCategoryName <- rv$categories$CategoryName[rv$categories$CategoryID == rv$selectedCategoryID]
    
    # Update category in database
    update_category(db_path, rv$selectedCategoryID, input$editCategoryName)
    
    # Update the category in reactive data
    categoryIndex <- which(rv$categories$CategoryID == rv$selectedCategoryID)
    rv$categories$CategoryName[categoryIndex] <- input$editCategoryName
    
    # Update any tasks using this category
    db <- dbConnect(RSQLite::SQLite(), db_path)
    dbExecute(db, sprintf("UPDATE tasks SET TaskCategory = '%s' WHERE TaskCategory = '%s'", 
                          input$editCategoryName, oldCategoryName))
    dbDisconnect(db)
    
    # Update tasks in reactive data
    if (any(rv$tasks$TaskCategory == oldCategoryName)) {
      rv$tasks$TaskCategory[rv$tasks$TaskCategory == oldCategoryName] <- input$editCategoryName
    }
    
    # Update dropdowns
    updateSelectInput(session, "taskCategory",
                      choices = setNames(rv$categories$CategoryName, rv$categories$CategoryName))
    
    updateSelectInput(session, "categoryFilter",
                      choices = c("All", unique(rv$tasks$TaskCategory)))
    
    showNotification("Category updated successfully", type = "message")
    removeModal()
  })
  
  # Delete category
  observeEvent(input$deleteCategoryBtn, {
    req(rv$selectedCategoryID)
    
    # Check if category is in use
    if (is_category_in_use(db_path, rv$selectedCategoryID)) {
      showNotification("Cannot delete category because it is used by one or more tasks", type = "error")
      return()
    }
    
    # Show a modal for confirmation
    showModal(modalDialog(
      title = "Confirm Delete",
      "Are you sure you want to delete this category?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmDeleteCategory", "Yes, Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirmDeleteCategory, {
    req(rv$selectedCategoryID)
    
    # Delete from database
    success <- delete_category(db_path, rv$selectedCategoryID)
    
    if (success) {
      # Remove from reactive data
      rv$categories <- rv$categories[rv$categories$CategoryID != rv$selectedCategoryID, ]
      
      # Update dropdowns
      updateSelectInput(session, "taskCategory",
                        choices = setNames(rv$categories$CategoryName, rv$categories$CategoryName))
      
      showNotification("Category deleted successfully", type = "message")
    } else {
      showNotification("Cannot delete category because it is used by one or more tasks", type = "error")
    }
    
    removeModal()
  })
  
  # Reset data
  observeEvent(input$resetData, {
    # Only admin users can reset data
    if (!user_info()$admin) {
      showNotification("You need admin privileges to reset data", type = "error")
      return()
    }
    
    showModal(modalDialog(
      title = "Confirm Reset",
      "Are you sure you want to reset all data? This cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmReset", "Yes, Reset All Data", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirmReset, {
    # Connect to database
    db <- dbConnect(RSQLite::SQLite(), db_path)
    
    # Clear all data
    dbExecute(db, "DELETE FROM notes")
    dbExecute(db, "DELETE FROM tasks")
    
    # Disconnect from database
    dbDisconnect(db)
    
    # Reset reactive values
    rv$tasks <- data.frame()
    rv$notes <- data.frame()
    rv$maxTaskID <- 0
    rv$maxNoteID <- 0
    rv$selectedTaskID <- NULL
    
    # Reset UI elements
    updateSelectInput(session, "categoryFilter", choices = c("All"))
    updateSelectInput(session, "taskForNotes", choices = c("Select a task" = ""))
    
    removeModal()
    showNotification("All data has been reset", type = "warning")
  })
}

# Run the application
shinyApp(ui = ui, server = server)