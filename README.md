Get exchange rates from [xe.com](http://www.xe.com/).

# Usage

``` bash
$ xe --help
xe - Command-line utility to get xe.com's exchange rates

Usage: xe (-s|--src CUR) (-t|--target CUR) [-d|--date YYYY-MM-DD]
  Get exchange rates from xe.com.

Available options:
  -s,--src CUR             three letter symbol for the source currency
  -t,--target CUR          three letter symbol for the target currency
  -d,--date YYYY-MM-DD     (Optional) date we want the exchange rate for. When
                           no date is specified, the program gets today's
                           exchange rate
  -h,--help                Show this help text
```

Example:

``` bash
$ xe -s USD -t EUR -d 2015-12-07
USD to EUR: 0.9225356379
```
