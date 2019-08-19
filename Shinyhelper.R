# This is helper file for adding busy indicator when clicking a button,
# the only change I made was to delete the checkmark click fadeaway
# animation so it will always there after clicking ref:
# https://github.com/daattali/advanced-shiny/blob/master/busy-indicator/helpers.R


withBusyIndicatorCSS <- "\n    .btn-loading-container {\n    margin-left: 10px;\n    font-size: 1.2em;\n    }\n    .btn-done-indicator {\n    color: green;\n    }\n    .btn-err {\n    margin-top: 10px;\n    color: red;\n    }\n"

withBusyIndicatorUI <- function(button)
{
  id <- button[["attribs"]][["id"]]
  div(shinyjs::useShinyjs(), singleton(tags$head(tags$style(withBusyIndicatorCSS))), 
      `data-for-btn` = id, button, span(class = "btn-loading-container", 
                                        shinyjs::hidden(icon("spinner", class = "btn-loading-indicator fa-spin"), 
                                                        icon("check", class = "btn-done-indicator"))), shinyjs::hidden(div(class = "btn-err", 
                                                                                                                           div(icon("exclamation-circle"), tags$b("Error: "), span(class = "btn-err-msg")))))
}

# Call this function from the server with the button id that is clicked
# and the expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr)
{
  # UX stuff: show the 'busy' message, hide the other messages, disable
  # the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })
  
  # Try to run the code when the button is clicked and show an error
  # message if an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::show(selector = doneEl, anim = TRUE, animType = "fade", 
                  time = 0.1)  # this is the line that changed
    value
  }, error = function(err)
  {
    errorFunc(err, buttonId)
  })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId)
{
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}