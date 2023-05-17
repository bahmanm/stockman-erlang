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

-type invoice_line() :: #invoice_line{}.

-record(invoice, {
    doc_no :: doc_no(),
    bpartner :: string(),
    type :: sales | purchase,
    trx_ts :: string(),
    discount :: integer(),
    total :: float(),
    lines :: [#invoice_line{}]
}).

-type invoice() :: #invoice{}.

-record(inventory, {
    product :: product(),
    qty :: integer()
}).

-type inventory() :: #inventory{}.

-type invoices() :: dict:dict(doc_no(), #invoice{}).
-type inventories() :: dict:dict(product(), #inventory{}).
