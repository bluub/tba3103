library(shiny)

setwd('/root/rshiny_app')

options(shiny.host = '0.0.0.0')
options(shiny.port = 12345)

runApp('main')