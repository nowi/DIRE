DIRE - A Distributed Reasoner for Description Logic in Scala
=================

Method
---------
The idea of DIRE is to take advantage of distributed computation for answering queries to a set of linked ontologies without merging all ontologies and copying all axioms to one site. The method consists of a resolution calculus applied to every ontology separately and a communication strategy that defines propagation of axioms between the reasoner instances.
For ALC ontologies, the resolution calculus is ordered resolution, for ontologies containing number restrictions the more involved basic superposition calculus is necessary to deal with the induced equalities. The communication strategy is based on the top symbols of terms that are unified on application of inference rules.


Implementation
---------------

DIRE is implemented in the [Scala][1] programming language and utilizes the [Akka Framework][2] for the distribution of computation. The distributed reasoner is realized as a network of [Actors][3] interacting with each other through message passing. DIRE offers flexible deployment possibilites ranging from single-workstation usage utilizing multiple processing cores up to a cluster deployment.


Building DIRE
------------------
DIRE is using the [SBT][4] build system. So the first thing you have to do is to download and install SBT as described on the SBT homepage.


Download the source code for DIRE, update/download all dependencies and compile the project :

    git clone git://github.com/nowi/DIRE.git

    cd DIRE

    sbt udpate

    sbt compile


Running DIRE (single workstation mode)
-------------

Start the Scala [REPL][5], import the DIRE commands into scope and start a local saturation of the OntoFarm ontology. (handles to the involved reasoners are stored in the variable 'reasoners')

    sbt console-quick

    import DIREShell._

    val reasoners = runOntoFarmLocal


Running DIRE (cluster mode)
-------------

On each machine in the cluster execute: 
    sbt server *PORT* RDL (e.g. sbt server 30000 RDL)
         

On any of those machines execute: 

    sbt console-quick

    import DIREShell._

    val remoteReasoners = runOntoFarmCluster


  [1]: http://scala-lang.org
  [2]: http://akkasource.com
  [3]: http://en.wikipedia.org/wiki/Actor_model
  [4]: http://code.google.com/p/simple-build-tool/
  [5]: http://en.wikipedia.org/wiki/Read-eval-print_loop
