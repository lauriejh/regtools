# CLI stable for sample CSV

    Code
      invisible(read_diag_data(file_path = test_csv, id_col = "id", date_col = "diag_year",
        code_col = "code", log_path = l_path))
    Message
      Reading <PATH> file...
      v Succesfully read file: <PATH>
    Output
      
    Message
      Checking column requirements:
    Output
      
    Message
      v ID column
      v Code column
      v Date column
    Output
      
    Message
      
      --------------------------------------------------------------------------------
    Output
      Diagnostic dataset succesfully read and columns validated
    Message
      
      -- Data Summary ----------------------------------------------------------------
    Output
      
    Message
      i Number of rows: 120256. Number of columns: 3.
    Output
      
      
      Rows: 120,256
      Columns: 3
      $ id        <chr> "P000000704", "P000000704", "P000000704", "P000000704", "P00~
      $ code      <chr> "F4522", "F305", "F65", "F840", "F728", "F450", "F187", "F73~
      $ diag_year <int> 2016, 2020, 2014, 2017, 2014, 2017, 2018, 2020, 2016, 2013, ~

