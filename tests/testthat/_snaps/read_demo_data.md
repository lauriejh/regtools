# CLI stable for sample CSV

    Code
      invisible(read_demo_data(file_path = test_csv, data_type = "t_invariant",
        id_col = "id", log = log_path))
    Output
      
    Message
      Reading <PATH> file...
      v Succesfully read file: <PATH>
      Checking column requirements:
      v ID column
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
      
      
      Rows: 30,024
      Columns: 4
      $ id                <chr> "P000000037", "P000000052", "P000000059", "P00000011~
      $ sex               <int> 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1~
      $ y_birth           <int> 2008, 2000, 2007, 2003, 2000, 2003, 2009, 2005, 2004~
      $ innvandringsgrunn <chr> "FAMM", "FAMM", "FAMM", "FAMM", "UTD", "FAMM", "UTD"~

