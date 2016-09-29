btce_fetcher
=====

An OTP application, fetches trades from [BTC-e](https://btc-e.com) and stores them in [dets](http://erlang.org/doc/man/dets.html) table.

Build
-----

    $ docker-compose up builder

Run
-----

    $ docker-compose up runner

Shell
-----

To connect to running app, do

    $ ./shell

**DO NOT** `q().` in that shell, doing it will stop application.

Do `init:stop().` or `^G q` instead.
