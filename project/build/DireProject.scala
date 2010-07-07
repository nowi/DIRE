/** 

 * User: no
 i
* Date: 05.07.20
 0
* Time: 16:22:
 1
*/

/*-------------------------------------------------------------------------------
   Copyright (C) 2009-2010 Scalable Solutions AB <http://scalablesolutions.se>

   ----------------------------------------------------
   -------- sbt buildfile for the Akka project --------
   ----------------------------------------------------

   Akka implements a unique hybrid of:
    * Actors , which gives you:
        * Simple and high-level abstractions for concurrency and parallelism.
        * Asynchronous, non-blocking and highly performant event-driven programming model.
        * Very lightweight event-driven processes (create ~6.5 million actors on 4 G RAM).
    * Supervision hierarchies with let-it-crash semantics. For writing highly
      fault-tolerant systems that never stop, systems that self-heal.
    * Software Transactional Memory (STM). (Distributed transactions coming soon).
    * Transactors: combine actors and STM into transactional actors. Allows you to
      compose atomic message flows with automatic rollback and retry.
    * Remoting: highly performant distributed actors with remote supervision and
      error management.
    * Cluster membership management.

  Akka also has a set of add-on modules:
    * Persistence: A set of pluggable back-end storage modules that works in sync with the STM.
        * Cassandra distributed and highly scalable database.
        * MongoDB document database.
        * Redis data structures database (upcoming)
    * Camel: Expose Actors as Camel endpoints.
    * REST (JAX-RS): Expose actors as REST services.
    * Comet: Expose actors as Comet services.
    * Security: Digest and Kerberos based security.
    * Spring: Spring integration
    * Guice: Guice integration
    * Microkernel: Run Akka as a stand-alone kernel.

-------------------------------------------------------------------------------*/

import sbt._
import java.io.File
import java.util.jar.Attributes

import webbytest.HtmlTestsProject


class DIREProject(info: ProjectInfo) extends DefaultProject(info) with HtmlTestsProject  {
  // ------------------------------------------------------------
  // repositories
//  val embeddedrepo = "embedded repo" at new File(akkaHome, "embedded-repo").toURI.toString
  val sunjdmk = "sunjdmk" at "http://wp5.e-taxonomy.eu/cdmlib/mavenrepo"
  val databinder = "DataBinder" at "http://databinder.net/repo"
  val configgy = "Configgy" at "http://www.lag.net/repo"
  val codehaus = "Codehaus" at "http://repository.codehaus.org"
  val codehaus_snapshots = "Codehaus Snapshots" at "http://snapshots.repository.codehaus.org"
  val jboss = "jBoss" at "https://repository.jboss.org/nexus/content/groups/public/"
  val guiceyfruit = "GuiceyFruit" at "http://guiceyfruit.googlecode.com/svn/repo/releases/"
  val google = "google" at "http://google-maven-repository.googlecode.com/svn/repository"
  val m2 = "m2" at "http://download.java.net/maven/2"
  val localartifactory = "artifactory" at "http://localhost:8081/artifactory/repo"


  // deployment
  val artifactorydeploy = "artifactorydeply" at "http://localhost:8081/artifactory/libs-snapshots-local"
   Credentials(Path.userHome / ".m2" / ".credentials", log)

  // ------------------------------------------------------------
  // project defintions
  val slf4j = "org.slf4j" % "slf4j-api" % "1.6.0" % "compile"
  val typica = "com.google.code.typica" % "typica" % "1.7" % "compile"
  val specs = "org.scala-tools.testing" % "specs_2.8.0.Beta1" % "1.6.4" % "test"
  val neo4jkernel = "org.neo4j" % "neo4j-kernel" % "1.0" % "compile"
  val neo4jindex = "org.neo4j" % "neo4j-index" % "1.0-b1" % "compile"
  val akkacore = "se.scalablesolutions.akka" % "akka-core_2.8.0.RC3" % "0.10" % "compile"
//  val akkacluster = "se.scalablesolutions.akka" % "akka-cluster_2.8.0.RC3" % "0.10" % "compile"
//  val akkautil = "se.scalablesolutions.akka" % "akka-util_2.8.0.RC3" % "0.10" % "compile"
  val scalatest = "org.scalatest" % "scalatest" % "1.0-for-scala-2.8.0-SNAPSHOT" % "test"
  val google_coll = "com.google.collections" % "google-collections" % "1.0" % "compile"






  override def mainClass = Some("de.unima.dire.DIRENewShell")














}
