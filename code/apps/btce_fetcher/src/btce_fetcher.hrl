-type transaction_type() :: 'bid' | 'ask'.

-record(transaction,
        {timestamp :: integer(),
         rate      :: float(),
         amount    :: float(),
         type      :: transaction_type(),
         tid       :: integer()}).

-type transaction() :: #transaction{}.
