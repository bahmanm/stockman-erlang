-record(invoice_line,
        {
         line_no :: integer(),
         product :: string(),
         qty :: integer(),
         price :: float(),
         line_amt :: float()
        }).


-record(invoice,
        {
         doc_no :: string(),
         bpartner :: string(),
         type :: atom(),
         trx_ts :: string(),
         discount :: integer(),
         total :: float(),
         lines :: [#invoice_line{}]
        }).

-record(inventory,
        {
         product :: string(),
         qty :: integer()
        }).
