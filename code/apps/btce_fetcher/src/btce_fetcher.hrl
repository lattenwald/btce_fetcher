-define(STORAGE, btce_store).

-record(transaction,
        {timestamp,
         rate,
         amount,
         type,
         tid}).
