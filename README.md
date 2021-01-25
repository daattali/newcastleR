# Tips for shiny app development

- `options(shiny.port=5555L)` in .Rprofile
- first parameter on new line so it doesnt go very far
- encoding ("r readcsv letters with accents")
- read csv outside of server function
- use shinybrowser to warn if on IE
- add a min and max to min_matches
- use update function isntead of renderUI
- Give explicit values to selectInput so that choice names can be changed without breaking the app
- give a funciton name to the table names changer and use simpler logic
- give a function name to unappreciated_prolific_strikers
- anything we do on client side (on the browser) is only for user friendliness - we always have to check in shiny on the server to make sure the input is fine. For example if i type a letter or a larger number, I'll get NA or a larger number
- FYI - I could even use javascript to send a value to shiny. Just be aware of that and remember that you can't trust any value coming from the brwoser - every check has to be made on the server side
- even better in my opinion - but this is completely subjective - use a slider. sliders and numeric inputs essentially provide the same functionality but using a different user experience. I use sliders when there aren't a lot of values
- defensive programming - add an `else` when you reach a place you think you should never reach and throw an error
- add a fixed column
- remove the row number
- only show prediction button when a row is selected
- prediction logic shouldnt be inside render - renders are for outputting, not for calculations. its business logic, put in its own function so we can test it. as little as possible should happen inside render. there should be a reactive variable that calls the prediction function and try-catches it in case an error happens it can show a message
- dont use magic numbers for column visiblity
- dont use magic strings for positions list
- render inside observer anti pattern
- make the plot button btn-lg and btn-primary
- user bootstrap toggle instead of shinyjs
- pull out business logic and plot code so easily testable/developable without having to run the app
- DEAN_TESTING <- TRUE
