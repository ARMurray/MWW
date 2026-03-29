# --- 1. Load Libraries ---
library(shiny)
library(bslib)
library(htmltools)
library(dplyr)
library(RSQLite)
library(jsonlite)
library(shinyjs) 

# --- 2. Configuration & Initialization ---

# Define the path to the local SQLite database
# NOTE: You must ensure the 'data/shop_inventory.sqlite' file is in the correct location 
# relative to this app.R file for the database functions to work.
DB_PATH <- "data/shop_inventory.sqlite"

# Initialize Reactive Values for Global State Management
app_state <- reactiveValues(
  db_ready = FALSE,
  user_id = NULL,
  # This structure is critical: 'price' is a numeric type
  cart = data.frame(serial = character(), p_num = character(), name = character(), price = numeric(), stringsAsFactors = FALSE),
  cart_total = 0.00
)

# --- 3. Database Functions ---

# Function to get detailed inventory for a single product number (p_num)
get_detailed_inventory <- function(p_num) {
  con <- tryCatch({
    dbConnect(RSQLite::SQLite(), DB_PATH)
  }, error = function(e) {
    message(paste("ERROR: Could not connect to database at", DB_PATH))
    return(NULL)
  })
  
  if (is.null(con)) return(data.frame())
  
  details <- tryCatch({
    tbl(con, "inventory") %>%
      filter(p_num == !!p_num, status == "Available") %>%
      select(serial, material, height, width, thickness, img_link, weight) %>%
      collect()
  }, error = function(e) {
    message("ERROR: Could not read 'inventory' table or apply filter.")
    print(e)
    data.frame()
  })
  
  dbDisconnect(con)
  return(details)
}

# Function to get the specific product name and list price for a serial number
get_product_info_by_serial <- function(serial_id) {
  con <- tryCatch({
    dbConnect(RSQLite::SQLite(), DB_PATH)
  }, error = function(e) {
    message(paste("ERROR: Could not connect to database at", DB_PATH))
    return(data.frame())
  })
  
  info <- tryCatch({
    tbl(con, "inventory") %>%
      filter(serial == !!serial_id) %>%
      inner_join(tbl(con, "products"), by = "p_num") %>%
      select(serial, p_num, name, list_price) %>%
      collect()
  }, error = function(e) {
    message(paste("ERROR: Could not fetch info for serial", serial_id))
    print(e)
    data.frame()
  })
  
  dbDisconnect(con)
  return(info)
}

# Function to get the overall stock summary
get_stock_summary <- function() {
  con <- tryCatch({
    dbConnect(RSQLite::SQLite(), DB_PATH)
  }, error = function(e) {
    message(paste("ERROR: Could not connect to database at", DB_PATH))
    return(data.frame())
  })
  
  if (is.null(con)) return(data.frame())
  
  summary_data <- tryCatch({
    tbl(con, "inventory") %>%
      filter(status == "Available") %>%
      group_by(p_num) %>%
      summarise(in_stock = n(), .groups = 'drop') %>%
      inner_join(tbl(con, "products"), by = "p_num") %>%
      select(p_num, name, description, stock_link, list_price, in_stock) %>%
      collect()
  }, error = function(e) {
    message("ERROR: Could not read tables or perform aggregation.")
    print(e)
    data.frame()
  })
  
  dbDisconnect(con)
  return(summary_data)
}

# Use reactivePoll to automatically refresh inventory data when DB file changes
product_summary <- reactivePoll(
  intervalMillis = 30000,
  session = getDefaultReactiveDomain(),
  checkFunc = function() {
    file.info(DB_PATH)$mtime
  },
  valueFunc = get_stock_summary
)

# --- 4. UI Component Helper Functions ---

create_id <- function(prefix, p_num) {
  paste0(prefix, "_", p_num)
}

# UI for the Add to Cart button
add_to_cart_button_ui <- function(serial) {
  # FIX: Uses the native 'onclick' handler to guarantee event firing
  tags$button(
    id = paste0("add_to_cart_btn_", serial),
    type = "button",
    class = "btn-sm btn-primary w-100", 
    `data-serial` = serial,
    # Calls the global JS function defined in the server's startup block
    onclick = paste0("cartClickHandler('", serial, "')"),
    tags$i(class = "fa fa-cart-plus"), 
    " Add to Cart"
  )
}

# UI for an individual inventory item piece (the content inside the accordion)
inventory_item_ui <- function(item_details, product_name, price, p_num) {
  
  div(class = "col-12 border-bottom p-2",
      div(class = "row align-items-center",
          div(class = "col-4 p-0",
              tags$img(
                src = item_details$img_link,
                class = "img-fluid rounded-lg shadow-sm",
                onerror = "this.onerror=null;this.src='https://placehold.co/150x150/fca5a5/7f1d1d?text=Image+Missing';"
              )
          ),
          div(class = "col-5",
              tags$h6(class = "mb-1 text-sm text-gray-700", product_name),
              tags$p(class = "mb-1 text-xs text-muted", paste("Serial:", item_details$serial)),
              tags$p(class = "mb-1 text-xs text-muted", paste("Material:", item_details$material))
          ),
          div(class = "col-3 d-flex flex-column justify-content-center align-items-end",
              add_to_cart_button_ui(item_details$serial)
          )
      )
  )
}

# UI for the main product card
product_card_ui <- function(item) {
  
  accordion_id <- create_id("inventory_acc", item$p_num)
  accordion_heading_id <- create_id("inventory_header", item$p_num)
  accordion_collapse_id <- create_id("inventory_collapse", item$p_num)
  accordion_target <- paste0("#", accordion_collapse_id)
  
  details <- get_detailed_inventory(item$p_num)
  
  if (nrow(details) == 0 || !item$in_stock) {
    inventory_items_ui <- list(
      tags$div(class="col-12 text-center text-muted py-3", "No unique inventory items currently available.")
    )
    in_stock_display <- "Out of Stock"
    card_bg <- "bg-red-50"
  } else {
    inventory_items_ui <- lapply(1:nrow(details), function(i) {
      inventory_item_ui(details[i, ], item$name, item$list_price, item$p_num)
    })
    in_stock_display <- paste(item$in_stock, "Pieces Available")
    card_bg <- "bg-white"
  }
  
  div(class = "col-xxl-4 col-lg-6 col-md-6 mb-4 d-flex align-items-stretch",
      tags$div(class = paste("card w-100 shadow-xl rounded-xl border-t-4 border-indigo-600", card_bg),
               tags$div(class = "card-body d-flex flex-column",
                        div(class="d-flex mb-3",
                            tags$img(
                              src = item$stock_link,
                              class = "w-20 h-20 object-cover rounded-md shadow-inner me-3",
                              onerror = "this.onerror=null;this.src='https://placehold.co/80x80/1f2937/9ca3af?text=NA';"
                            ),
                            div(class="flex-grow-1",
                                tags$h5(class = "card-title text-xl font-bold text-gray-800 mb-0", item$name),
                                tags$p(class = "text-indigo-600 text-lg font-semibold", paste0("$", format(item$list_price, nsmall = 2)))
                            )
                        ),
                        tags$p(class = "card-text text-sm text-gray-600 mb-2", item$description),
                        tags$p(class = paste("text-xs font-medium mb-3", if(item$in_stock) "text-green-600" else "text-red-600"),
                               in_stock_display),
                        
                        # Bootstrap Accordion for Inventory List
                        div(class="accordion mt-auto", id=accordion_id, 
                            div(class="accordion-item border border-gray-200 rounded-lg",
                                h2(class="accordion-header", id=accordion_heading_id,
                                   tags$button(
                                     class = paste("accordion-button py-2 px-3 text-sm font-semibold", 
                                                   if(!item$in_stock) "collapsed disabled bg-gray-100 text-gray-500" else "bg-indigo-50 text-indigo-700"),
                                     type="button",
                                     `data-bs-toggle`="collapse",
                                     `data-bs-target`=accordion_target,
                                     `aria-expanded`="false",
                                     `aria-controls`=accordion_collapse_id,
                                     if(item$in_stock) "View Unique Inventory Pieces" else "Inventory Unavailable"
                                   )
                                ),
                                div(id=accordion_collapse_id, class="accordion-collapse collapse",
                                    `aria-labelledby`=accordion_heading_id,
                                    div(class="accordion-body p-0", 
                                        div(class="row m-0", inventory_items_ui)
                                    )
                                )
                            )
                        )
               ) 
      ) 
  ) 
}


# --- 5. User Interface (UI) ---

ui <- page_navbar(
  title = tags$span(class = "font-extrabold", icon("gem"), "Gemini Inventory Shop"),
  theme = bs_theme(
    version = 5,
    bootswatch = "lux", 
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ) %>%
    bs_add_rules(c(
      # Custom CSS for Tailwind-like styling and better visuals
      ".card { transition: all 0.3s ease; }",
      ".card:hover { transform: translateY(-5px); box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05); }",
      ".accordion-button:not(.collapsed) { background-color: #6366f1 !important; color: white !important; }",
      ".bg-red-50 { background-color: #fef2f2; }",
      ".border-t-4 { border-top-width: 4px; }",
      ".border-indigo-600 { border-color: #4f46e5; }"
    )),
  
  useShinyjs(), 
  
  # --- Inventory Tab ---
  nav_panel("Inventory", icon = icon("boxes-stacked"),
            div(class = "container-fluid py-4",
                uiOutput("inventory_cards")
            )
  ),
  
  # --- Cart Tab ---
  nav_panel("Cart", icon = icon("shopping-cart"),
            div(class = "container py-4",
                tags$h2("Your Shopping Cart", class="text-2xl font-bold mb-4"),
                uiOutput("cart_items_ui"),
                hr(),
                tags$h4("Total: ", textOutput("cart_total_display", inline = TRUE), class="font-bold text-xl")
            )
  )
)

# --- 6. Server Logic ---

server <- function(input, output, session) {
  
  # --- 6.1. Initialization ---
  observeEvent(TRUE, {
    app_state$user_id <- "MOCK-USER-ID"
    app_state$db_ready <- TRUE
  }, once = TRUE)
  
  # --- 6.2. CRITICAL FIX: Inject Native JavaScript Click Handler (Run once) ---
  observeEvent(TRUE, {
    
    js_code <- "
      // Define the single, global function called by the button's onclick attribute
      window.cartClickHandler = function(serial) {
          Shiny.setInputValue(
              'add_to_cart_click', 
              { serial: serial, time: new Date().getTime() }, 
              {priority: 'event'}
          );
      };
    "
    shinyjs::runjs(js_code)
  }, once = TRUE) 
  
  
  # --- 6.3. Inventory Data & UI Rendering ---
  output$inventory_cards <- renderUI({
    req(product_summary()) 
    
    card_list <- lapply(1:nrow(product_summary()), function(i) {
      product_card_ui(product_summary()[i, ])
    })
    
    div(class = "row", card_list)
  })
  
  
  # --- 6.4. Cart Logic ---
  
  # Observer for "Add to Cart" button clicks
  observeEvent(input$add_to_cart_click, {
    serial_to_add <- input$add_to_cart_click$serial
    
    product_info <- get_product_info_by_serial(serial_to_add)
    
    if (nrow(product_info) == 1 && !serial_to_add %in% app_state$cart$serial) {
      
      # FIX: Convert list_price to numeric type immediately
      new_item <- data.frame(
        serial = product_info$serial, 
        p_num = product_info$p_num, 
        name = product_info$name, 
        price = as.numeric(product_info$list_price),
        stringsAsFactors = FALSE
      )
      
      app_state$cart <- bind_rows(app_state$cart, new_item)
      
    }
  })
  
  # Observer for "Remove from Cart" button clicks
  observeEvent(input$remove_from_cart_click, {
    serial_to_remove <- input$remove_from_cart_click$serial
    
    if (serial_to_remove %in% app_state$cart$serial) {
      
      # Filter the cart: keep all rows EXCEPT the one matching the serial_to_remove
      app_state$cart <- app_state$cart %>%
        filter(serial != serial_to_remove)
      
    } 
  })
  
  # Observer to calculate and update the total price
  observe({
    if (nrow(app_state$cart) > 0) {
      # Works because app_state$cart$price is guaranteed to be numeric
      current_total <- sum(app_state$cart$price, na.rm = TRUE)
      app_state$cart_total <- current_total
    } else {
      app_state$cart_total <- 0.00
    }
  })
  
  # Render cart items UI
  output$cart_items_ui <- renderUI({
    if (nrow(app_state$cart) == 0) {
      return(tags$p("Your cart is empty.", class="text-muted italic"))
    }
    
    cart_list <- app_state$cart
    
    lapply(1:nrow(cart_list), function(i) {
      item <- cart_list[i, ]
      
      # Inline JS for the remove button
      remove_btn_ui <- tags$button(
        id = paste0("remove_btn_", item$serial),
        type = "button",
        class = "btn-sm btn-danger ms-3", 
        onclick = paste0("Shiny.setInputValue('remove_from_cart_click', { serial: '", item$serial, "', time: new Date().getTime() }, {priority: 'event'});"),
        tags$i(class = "fa fa-trash-alt"), 
        " Remove"
      )
      
      div(class="d-flex justify-content-between align-items-center border-bottom py-2",
          div(
            tags$strong(item$name), 
            tags$p(class="text-xs text-muted mb-0", paste("Serial:", item$serial))
          ),
          div(class="d-flex align-items-center",
              tags$span(paste0("$", format(item$price, nsmall = 2)), class="font-semibold text-lg text-indigo-600"),
              remove_btn_ui
          )
      )
    })
  })
  
  # Render cart total
  output$cart_total_display <- renderText({
    paste0("$", format(app_state$cart_total, nsmall = 2))
  })
  
}

# --- 7. Run App ---
shinyApp(ui = ui, server = server)