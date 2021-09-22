# library(DT) 

tab_data_tables <- tabPanel(
  'Data tables',
  h1("Triangle data"),
  dataTableOutput("tbl_paid_triangle"),
  h1("Detailed data"), 
  dataTableOutput("tbl_records")
  
)

expr_data_tables <- quote({

  output$tbl_paid_triangle <- renderDataTable(tbl_paid_triangle())
  output$tbl_records <- renderDataTable(tbl_records())
  
})
