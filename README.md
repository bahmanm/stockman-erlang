# Stockman in Erlang
*Stockman: my favourite set of exercises to learn a new programming language.*

# v1.0
1. Read a CSV file into memory which contain sales invoice lines.
2. Pretty print the invoices.

### Status ###
done

```
$ rebar3 escriptize
$ _build/default/bin/stockman resources/sales-invoices-tiny.csv
```

# v2.0 #
1. Read a CSV file into memory which contain sales invoice lines.
2. Find the total sales.
3. Find the most expensive invoice and pretty print it.
4. Find the most expensive product and print its code.
5. Calculate the average price of each product, and print the the list of prices descendingly.
6. Find the total sales per customer and print a descending list in the form "customer, total".
7. Find the customer with the largest total sales.
8. Find the 3 customers with the least total sales and print a descending list in the form "customer, total".
9. Find the date with the largest total sales amount.

### Status ###
TODO

# v3.0 #
1. Read a CSV file into memory which contains product inventory.
2. Read a CSV file into memory which contain sales invoice lines.
  3. Check if the product referenced on each line has enough inventory.
    4. If no, do not import the invoice the line belongs to (not just the line).
    5. If yes, import the invoice line and update the inventory of the product
       accordingly.
  6. It is important to process the lines in the order they appear in the CSV.
7. Print a list of invoices listing the problematic line(s) for each invoice.

### Status ###
TODO

# License #
All files are under [Apache License v2.0](http://www.apache.org/licenses/LICENSE-2.0), unless otherwise specified.
