# CLI stable for sample CSV

    Code
      invisible(read_demo_data(file_path = test_csv, data_type = "t_invariant",
        id_col = "id", date_col = "diag_year", log = log_path))
    Output
      
    Message
      Reading <PATH> file...
      v Succesfully read file: <PATH>
      Checking column requirements:
      v ID column
      v Date column
    Output
      
    Message
      Data type: time invariant. Checking requirements...
      v No duplicate IDs
    Output
      
    Message
      
      --------------------------------------------------------------------------------
    Output
      Demographic dataset succesfully read and columns validated
    Message
      
      -- Data Summary ----------------------------------------------------------------
    Output
      
    Message
      i Number of rows: 30024. Number of columns: 4.
      i Unique IDs in dataset: 30024.
    Output
      
      'data.frame':	30024 obs. of  4 variables:
       $ id               : chr  "P000000037" "P000000052" "P000000059" "P000000111" ...
       $ sex              : int  0 0 0 1 1 0 1 0 0 0 ...
       $ y_birth          : int  2008 2000 2007 2003 2000 2003 2009 2005 2004 2002 ...
       $ innvandringsgrunn: chr  "FAMM" "FAMM" "FAMM" "FAMM" ...

