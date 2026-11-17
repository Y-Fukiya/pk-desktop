# Synthetic, isolated browser fixture: no runtime authentication, no user project/data.
root <- normalizePath(Sys.getenv('PK_BROWSER_SOURCE',unset='.'),mustWork=TRUE)
project <- normalizePath(Sys.getenv('PK_BROWSER_PROJECT'),mustWork=TRUE)
port <- as.integer(Sys.getenv('PK_BROWSER_PORT'))
stopifnot(is.finite(port),port>1024,port<65536)
source(file.path(root,'apps/pk-desktop/pkapp.R'))
core <- environment(pkapp_open)
e <- core$desktop_nca_engine(project)
d <- data.frame(ID=rep(letters[1:4],each=4),PERIOD=rep(c('1','2','3','4'),each=4),
 TIME=rep(0:3,4),CONC=rep(c(0,8,4,2),4),UNIT='mg/L')
id <- core$desktop_table_save(e,'Browser synthetic',list(source=d),list(kind='prepare',steps=list()))$id
role <- paste0('group_role_column_',digest::digest('PERIOD',algo='sha256',serialize=FALSE))
ui <- bslib::page_fluid(core$desktop_workbench_styles(),
 shiny::tags$script(shiny::HTML(paste0('window.PK_TEST_SOURCE=',jsonlite::toJSON(id,auto_unbox=TRUE),';window.PK_TEST_ROLE=',jsonlite::toJSON(role,auto_unbox=TRUE),';'))),
 core$desktop_nca_ui('nca'))
server <- function(input,output,session) core$desktop_nca_server('nca',e)
shiny::runApp(shiny::shinyApp(ui,server),host='127.0.0.1',port=port,launch.browser=FALSE,quiet=TRUE)
