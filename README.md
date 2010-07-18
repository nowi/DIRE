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


1.    Get the source code for DIRE

    git clone git://github.com/nowi/DIRE.git

2.    cd DIRE

3.    sbt udpate

4.    sbt compile


Running DIRE ( single workstation mode )
-------------

1.    sbt console-quick

2.    import DIREShell._

3.    val reasoners = runOntoFarmLocal (this starts a distributed reasoning process that saturates the OntoFarm example ontology.)


Running DIRE ( cluster mode )
-------------

On each machine of the cluster execute : sbt server *PORT* RDL (e.g. sbt server 30000 RDL)
         

On any of those machines: 

1.    sbt console-quick

2.    import DIREShell._

3.    val remoteReasoners = runOntoFarmCluster


  [1]: http://scala-lang.org
  [2]: http://akkasource.com
  [3]: http://en.wikipedia.org/wiki/Actor_model
  [4]: http://code.google.com/p/simple-build-tool/
