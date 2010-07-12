import sbt._
class DIREProject(info: ProjectInfo) extends DefaultProject(info) {
  // ------------------------------------------------------------
  // repositories
  //  val embeddedrepo = "embedded repo" at new File(akkaHome, "embedded-repo").toURI.toString
  val akka = "Akka" at "http://www.scalablesolutions.se/akka/repository/"
  val neo4j = "Neo4J" at "http://m2.neo4j.org/"
  val sunjdmk = "sunjdmk" at "http://wp5.e-taxonomy.eu/cdmlib/mavenrepo"
  val databinder = "DataBinder" at "http://databinder.net/repo"
  val configgy = "Configgy" at "http://www.lag.net/repo"
  val codehaus = "Codehaus" at "http://repository.codehaus.org"
  val codehaus_snapshots = "Codehaus Snapshots" at "http://snapshots.repository.codehaus.org"
  val jboss = "jBoss" at "https://repository.jboss.org/nexus/content/groups/public/"
  val guiceyfruit = "GuiceyFruit" at "http://guiceyfruit.googlecode.com/svn/repo/releases/"
  val google = "google" at "http://google-maven-repository.googlecode.com/svn/repository"
  //val m2 = "m2" at "http://download.java.net/maven/2"
  //val localartifactory = "artifactory" at "http://localhost:8081/artifactory/repo"

  // deployment
//  val artifactorydeploy = "artifactorydeply" at "http://localhost:8081/artifactory/libs-snapshots-local"
//  Credentials(Path.userHome / ".m2" / ".credentials", log)

  // ------------------------------------------------------------
  // project defintions
  val akkacore = "se.scalablesolutions.akka" % "akka-core_2.7.7" % "0.7"
  val akkacluster = "se.scalablesolutions.akka" % "akka-cluster-jgroups_2.7.7" % "0.7"
  val akkautil = "se.scalablesolutions.akka" % "akka-util_2.7.7" % "0.7"
  val slf4j = "org.slf4j" % "slf4j-api" % "1.6.0" % "compile"
  val typica = "com.google.code.typica" % "typica" % "1.7" % "compile"
  val specs = "org.scala-tools.testing" % "specs_2.8.0.Beta1" % "1.6.4" % "test"
  val neo4jkernel = "org.neo4j" % "neo4j-kernel" % "1.0" % "compile"
  val neo4jindex = "org.neo4j" % "neo4j-index" % "1.0-b1" % "compile"
  val scalatest = "org.scala-tools.testing" % "scalatest" % "0.9.5" % "test"
  //val google_coll = "com.google.collections" % "google-collections" % "1.0" % "compile"
  //val jteigenrunner = "com.jteigen.scalatest" % "junit4runner" % "1.0-SNAPSHOT" % "test"




  override def unmanagedClasspath = super.unmanagedClasspath +++ ("config" / "akka.conf")

  System.setProperty("akka.config", "config/akka.conf")

  // specify run tasks here
  lazy val shell = runTask(Some("kernel.DIRENewShell"), super.runClasspath).dependsOn(compile) describedAs "Runs the shell."
  //lazy val server =

  lazy val server =
  task {
    args =>
      if (args.length == 2)
        serverConstructor(args(0), args(1))
      else
        task {Some("Please specify a port (integer) and a reasoner type (RDL or FOL)")}
  }

  def serverConstructor(a: String, b: String) =
    {
      println("Arguments were: " + a + " and " + b)
      runTask(Some("kernel.DIREServer"), super.runClasspath,a,b).dependsOn(compile) describedAs "Starts a DIRE reasoner on specifed port and type ( FOL | RDL )"
    }

  //override def mainClass = Some("DIREServer")


}