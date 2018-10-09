-----
author: Alexander Vershilov
title: Metric a Haskell application
date: 2018-09-29
tags: haskell
license: cc-by
-----

In one of my [recept posts](https://qnikst.github.io/posts/2018-08-23-ht-no-more.html),
I've mentioned gathering metrics for my Haskell application. Some people asked me about
my setup, so I will try to describe how I configured and structured my application.

I will try to split the description in a series of posts, in this one I'll describe
the general setup. After reading this post, you'll be able to set up the metrics system
for your Haskell application (or suggest me how to do that better). At this point, you'll
be able to get some information about your application and set up alerts on that. In the
following posts, we will try to go deeper inside each metrics and check if metrics are
helpful if there are any missing pieces there, and how that can be improved.

## Haskell and metrics.

Let's spend a bit of time on defining the problem that we want to solve and describing
solution area. The purpose of the metrics system is to tell if your application is alive
and behaves expected. It should not give you more than statistical information about
your application. We can split information into two categories:

  1. general information: memory use, runtime system statistics.
  2. application specific information.

The border between those two is quite fuzzy, for example, you may have general web-server
statistics, like the number of processed requests or time to reply that is application
specific, but applies to all web-servers. So I'd add them to the first group. 
But the main trap here is not to try to solve an incorrect problem; metrics may not work
as an exact information source. Nor be a mechanism for tracing or log server, you need
other tools for that purpose.

In the Haskell ecosystem, there are few packages providing metrics support.  One that is
the best known and has a long history is [EKG](https://hackage.haskell.org/package/ekg-core)
this package offers few metrics types and a large variety of the systems that you can integrate
with.  While EKG is generally a good generic solution, I found that few companies are trying
to move from that package. (I was not able to gather concrete reports why that was a problem,
so will try to avoid answering that question).

So we can take a specific solution that works great with a single system. In Tweag we used to
use [Prometheus]( https://prometheus.io/). With Prometheus, you can dump your metrics to the
well-maintained package, that other people usually familiar with. Hackage offers excellent
library for working with Prometheus: [prometheus-client](https://hackage.haskell.org/package/prometheus-client).
Even if you like EKG more or have projects that are using it, you can use the adapter for EKG
[ekg-prometheus-adapter](https://hackage.haskell.org/package/ekg-prometheus-adapter).
I have not used that package myself, but I hope that it just works, or at least can be easily fixed.

## Setup.

For the application setup, I'm going to use Docker compose. With this approach, we will
be able to cover all details, and this approach may be adapted to the more complex system like kubernetes.

Let's start writing the docker compose files. I omitted all irrelevant links and configuration.

```docker
version: "3"
services:

  haskell-app:
    image: <your-image>
    ports:
    - '8080:8080'

  node_exporter:
    image: prom/node-exporter
    expose:
      - 9100
  
  prometheus:
    image: prom/prometheus:latest
    volumes:
    - ./config/prometheus:/etc/prometheus
    - prometheus_data:/prometheus
    command:
    - '--config.file=/etc/prometheus/prometheus.yml'
    ports:
    - '9090:9090'
    links:
    - haskell-app
  grafana:
    image: grafana/grafana
    environment:
      - GF_SECURITY_ADMIN_PASSWORD=XxXchangemeXxX
    depends_on:
      - prometheus
    ports:
      - "3000:3000"
    links:
      - prometheus
    volumes:
      - grafana_data:/var/lib/grafana
    user: "104"    

volumes:
  prometheus_data: {}
  grafana_data: {}
```

Prometheus config:

```docker
global:
    scrape_interval: 5s
    external_labels:
      monitor: 'my-monitor'
scrape_configs:
  - job_name: 'myapp'
    static_configs:
      - targets: 
        - haskell-app:8080
```

It's possible to configure graphana declaratively as well, but as I don't have the final
solution that I can use out of the box on any system, I tend to setup Grafana manually.
Just log into the instance and go through the onboarding process.

Now we are ready to set up our Haskell application.

Setup of the Haskell application may be pretty simple. To dump GHC statistics, you can use
[prometheus-metrics-ghc](https://hackage.haskell.org/package/prometheus-metrics-ghc).
To make the full use of this package you need to enable the gathering of the runtime statistics:

```cabal
  build-depends: prometheus-metrics-ghc
  ghc-options: "-with-rtsopts=-T"
```

Then add to your main:

```haskell
import  Prometheus
import  Prometheus.Metric.GHC

main  :: IO ()
main = do
   register ghcMetrics
```

At this point, you gather RTS stats, but you don't export you metrics yet.
To export your data, you may want to use
[wai-middleware-prometheus](https://hackage.haskell.org/package/wai-middleware-prometheus).
This package allows you to provide metrics inside any `wai`/`warp` application.

```haskell
import Network.Wai.Middleware.Prometheus
import Prometheus
import Prometheus.Metric.GHC

main = do 
   register ghcMetrics
   Warp.run 9090
        $   prometheus def
              { prometheusInstrumentPrometheus  =False }
         $ yourApplication
```

Or use [metricsApp](https://hackage.haskell.org/package/wai-middleware-prometheus-1.0.0/docs/Network-Wai-Middleware-Prometheus.html#v:metricsApp)
 function if you don't have any web application. And Prometheus will scrape that data from your application.
At this point, you'll have some basic information about endpoints and GC stats.
And you can add your application-specific data using Prometheus interface.

We will cover interesting stats in the next posts, but for now, you may be
interested in the following data:

   * `ghc_allocated_bytes_total` - to build `rate` plot based on that
   * `ghc_num_gcs` - to build rate of number of GCs
   * `ghc_mutator_wall_seconds_total/(ghc_mutator_wall_seconds_total+ghc_gc_wall_seconds_total)` -
       to understand the proportion of time spent in GC
   * information about memory usage split by the type of memory
   * and a number of metrics from `ghc_gcdetails` category. This data may not be very useful
      as it shows the data since last GC, so you may report the same GC multiple times if
      no GC happened during the report period, or miss some reports if more that one GC happened.

I hope that this information will be useful and will try to dig into concrete
metrics examples in the following posts.
