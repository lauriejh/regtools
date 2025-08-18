# CLI stable for sample CSV

    Code
      invisible(read_diag_data(file_path = test_csv, id_col = "id", date_col = "diag_year",
        code_col = "code", log_path = l_path))
    Message
      Reading <PATH> file...
      v Succesfully read file: <PATH>
      Checking column requirements:
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
      i Number of rows: 30024. Number of columns: 3.
    Output
      
      
      'data.frame':	30024 obs. of  3 variables:
       $ id       : chr  "P000000037" "P000000052" "P000000059" "P000000111" ...
       $ code     : chr  "F773" "F1011" "F898" "F1010" ...
       $ diag_year: int  2012 2017 2014 2015 2018 2020 2012 2017 2020 2015 ...

