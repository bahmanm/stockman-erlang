-record(invoice_line,
        {
         line_no,
         product,
         qty,
         price,
         line_amt
        }).


-record(invoice,
        {
         doc_no,
         customer,
         trx_ts,
         discount,
         total,
         lines
        }).

-record(inventory,
        {
         product,
         qty
        }).
