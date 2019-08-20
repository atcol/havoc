![Havoc](assets/logo-small.png)

# havoc

A highly configurable, low footprint reverse proxy for simulating failure between remote systems.

Havoc exposes _strategies_ that allow you to quickly & easily test your applications against
remote service failure.

# Quick Start

## Strategies

| Strategy | Parameter  | JSON Example | Command Line Example | Effect 
|---|---|---|---|---|
| Transparent | _n/a_   | `"strategy": { }`               | _n/a_ | Accept all requests |
| ReqLimit    | `Int`   | `"strategy": { "limit": 1000 }` | `ReqLimit 1000`  | Accept 1000 requests, then always fail |
| DropRatio   | `Float` | `"strategy": { "drop":  0.05 }` | `DropRatio 0.05 `| Fail 5% of requests |
| Delay       | `Int`   | `"strategy": { "delay": 1000 }` | `Delay 1000 `| Wait 1 second before performing the request |

## Examples

### REST API Failure

Scenario: drop 50% of requests from my Single Page App (SPA) to my REST API

    $ ./havoc solo --url "http://myservice" --strat "DropRatio 0.5" --port 8080

Pointing the SPA to `localhost:8080` will fail 50 % of its requests.

### Multiple Services & Strategies

Scenario: wrap multiple microservices with different failure strategies

With this settings JSON file:

    [
      { "iden": "User", "url": "http://users.system/", "port": 1111, "strategy": { "drop":  0.05 } },
      { "iden": "Sales", "url": "http://sales.system/", "port": 2222, "strategy": {} },
      { "iden": "Marketing", "url": "http://marketing.system/", "port": 3333, "strategy": { "delay": 1000 } },
      { "iden": "Support", "url": "http://support.system/", "port": 4444, "strategy": { "limit": 100 } }
    ]
 we have:
 
  * a proxy for the User service on port `1111` that drops 5% of its requests
  
  * a proxy for the Sales service on port `2222` that is transparent
  
  * a proxy for the Marketing service on port `3333` that delays requests by 1 second
  
  * a proxy for the Support XX service on port `4444` that will accept the first 100 requests the fail
  
To run:

    $ ./havoc farm --file settings.json

## Docker Support

There's a docker image at `athc/havoc`. 

You could run the `solo` example above with:

    $ docker run -it -p 8080:8080 athc/havoc solo --url "http://myservice" --strat "DropRatio 0.5" --port 8080

You could run the `farm` example above with:

    $ docker run -v ${PWD}:/havoc-work  --expose=1111-4444:1111-4444 -it havoc farm  --file /havoc-work/settings.json  

# Development

If you wish to contribute please submit a merge request.

## Future Plans

 * HTTPS support
 
 * request & response mutators
 
 * TCP/UDP socket support (e.g. proxy a message broker)
