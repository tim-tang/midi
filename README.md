              _____     ___   _____ 
      /\/\    \_   \   /   \  \_   \
     /    \    / /\/  / /\ /   / /\/
    / /\/\ \/\/ /_   / /_// /\/ /_  
    \/    \/\____/  /___,'  \____/  
                                 
===========

Riak Core Distributed Application For Log Analysis.

Build
-----

    make rel

Test
----

    make ct

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

Sample Usage
-----

Parser sample:

    [_Host, _, _User, _Time, Req, Code, BodySize, _Referer, Agent]
    0.0.0.0 - - [21/Mar/2011:18:47:27 +0000] "GET /blog/2011/aol_meet_riak.html HTTP/1.1" 200 12754 "-" "Java/1.6.0_24"

1- Load web log into bucket 'Riak':

    midi:parse_log("riak","/Users/tim-tang/playground/midi/midi.access.log").

2- Query bucket by following keys:

    midi:read("riak", "200").
    midi:read("riak", "300").
    midi:read("riak", "400").
    midi:read("riak", "GET").
    midi:read("riak", "total_reqs").
    midi:read("riak", "total_sent").
    midi:read("riak", "agents").

3- Add & Remove items:

    midi:sadd("riak", "agents", "MAC OSX").
    midi:srem("riak", "agents", "MAC OSX").
