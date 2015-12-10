Get exchange rates from [xe.com](http://www.xe.com/).

# Usage

``` bash
$ xe --help
xe - Command-line utility to get xe.com's exchange rates

Usage: xe (-s|--src CUR) (-t|--target CUR) [-d|--date YYYY-MM-DD]
          [-a|--amount AMOUNT]
  Get exchange rates from xe.com.

Available options:
  -s,--src CUR             three letter symbol for the source currency
  -t,--target CUR          three letter symbol for the target currency
  -d,--date YYYY-MM-DD     (Optional) date we want the exchange rate for. When
                           no date is specified, the program gets today's
                           exchange rate
  -a,--amount AMOUNT       (Optional) amount of money in source currency to
                           convert into the target currency. Defaults to 1 unit.
  -h,--help                Show this help text

```

Example:

``` bash
$ xe -s SEK -t EUR -d 2015-12-07 -a 502
502.00 SEK = 54.21 EUR (Exchange rate from XE.com on 2015-12-07: 1 SEK = 0.1079850103 EUR)
```
