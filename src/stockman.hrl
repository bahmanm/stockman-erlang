-type doc_no() :: string().
-type line_no() :: integer().
-type product() :: string().

-record(invoice_line, {
    line_no :: line_no(),
    product :: product(),
    qty :: integer(),
    price :: float(),
    line_amt :: float()
}).

-record(invoice, {
    doc_no :: doc_no(),
    bpartner :: string(),
    type :: sales | purchase,
    trx_ts :: string(),
    discount :: integer(),
    total :: float(),
    lines :: [#invoice_line{}]
}).

-record(inventory, {
    product :: product(),
    qty :: integer()
}).

-type invoices() :: dict:dict(doc_no(), #invoice{}).
-type inventories() :: dict:dict(product(), #inventory{}).
