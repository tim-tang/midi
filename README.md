              _____     ___   _____ 
      /\/\    \_   \   /   \  \_   \
     /    \    / /\/  / /\ /   / /\/
    / /\/\ \/\/ /_   / /_// /\/ /_  
    \/    \/\____/  /___,'  \____/  
                                 
===========

Riak Core Distributed Application For Log Analysis.

Prerequisition
-----

- Rebar3
- Erlang/OTP 18

Build
-----

    make rel

Run
---

    make run

Try
---

    1> midi:ping().
    {pong,753586781748746817198774991869333432010090217472}

Quit
----

    2> q().

Play with Clustering
--------------------

Build 3 releases that can run on the same machine::

    make devrel

Start them in different consoles::

    make dev1-console
    make dev2-console
    make dev3-console

join 2 nodes to the first one::

    make devrel-join

check the status of the cluster::

    make devrel-status

you should see something like this::

    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    joining     0.0%      --      'midi2@127.0.0.1'
    joining     0.0%      --      'midi3@127.0.0.1'
    valid     100.0%      --      'midi1@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:1 / Leaving:0 / Exiting:0 / Joining:2 / Down:0

it should say that 3 nodes are joining, now check the cluster plan::

    make devrel-cluster-plan

it should display the cluster plan, now we can commit the plan::

    make devrel-cluster-commit

check the status of the cluster again::

    make devrel-status

you could see the vnodes transfering::

    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    valid      75.0%     25.0%    'midi1@127.0.0.1'
    valid       9.4%     25.0%    'midi2@127.0.0.1'
    valid       7.8%     25.0%    'midi3@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:3 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

at some point you should see something like this::

    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    valid      33.3%      --      'midi1@127.0.0.1'
    valid      33.3%      --      'midi2@127.0.0.1'
    valid      33.3%      --      'midi3@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:3 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

when you are bored you can stop them::

    make devrel-stop


Sample Parser:
---

    [_Host, _, _User, _Time, Req, Code, BodySize, _Referer, Agent]
    0.0.0.0 - - [21/Mar/2011:18:47:27 +0000] "GET /blog/2011/aol_meet_riak.html HTTP/1.1" 200 12754 "-" "Java/1.6.0_24"

Testing::
----

1- Load web log into bucket 'rc':

    midi:parse_log("rc","/Users/tim-tang/playground/midi/midi.access.log").

2- Query bucket by following keys:

    midi:read("rc", "200").
    midi:read("rc", "300").
    midi:read("rc", "400").
    midi:read("rc", "GET").
    midi:read("rc", "HEAD").
    midi:read("rc", "total_reqs").
    midi:read("rc", "total_sent").
    midi:read("rc", "agents").

3- Add & Remove items:

    midi:sadd("rc", "midi", "100").
    midi:read("rc", "midi").
    midi:srem("rc", "midi", "100").

4- Query All Keys:

    midi:keys().


Testing Cluster::
---

Scenario-1 => 

    3 node cluster, to check preflist spread on 3 different nodes:
    midi:get_dbg_preflist("rc", "total_reqs").

> Kill one node to compare before and after preflist, check out the fallback vnode.

Scenario-2 => 

    3 nodes cluster, add an item first, then kill one node. remove this item. restore dead node. query this item.
    
Scenario-3 (Read Repair Testing)=> 

    - 3 nodes cluster.
    - Take a node down -- this will cause fallback vnodes to be created.
    - Write some data -- this will cause the fallback vnode to be populated with parallel/conflicting objects relative to the other vnodes. It's important that you not perform a rts:get or else read repair will reconcile them.
    - Restart the downed node -- this will cause the primary to come online with no data.
    - Perform a rts:get to invoke read repair. At this point all primaries have the correct data but you have a fallback that has conflicting data. After some time the fallback will realize the primary is up and will begin handoff.
    - Wait for handoff messages to appear in the console. Retry the rts:get and make sure the data is still correct and no further read repair was made. This proves that the data was reconciled prior to writing it.

More testing scenario, please refer => https://github.com/rzezeski/try-try-try/tree/master/2011/riak-core-conflict-resolution
