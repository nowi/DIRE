package reasoner


import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy
import se.scalablesolutions.akka.dispatch.Dispatchers
import core._
import containers.{NonEmptyClauseStore, ClauseStorage, CNFClauseStore}
import domain.fol.ast.FOLClause
import domain.fol.parsers.SPASSIntermediateFormatParser
import helpers.Subject
import java.io.File
import core.ordering.{CustomSPASSModule1Precedence, ALCLPOComparator}
import core.reduction._
import core.resolution.{OrderedResolver}
import core.rewriting.{Substitutor, VariableRewriter}
import ProvingState._
import ProvingResult._
import core.selection.NegativeLiteralsSelection
import se.scalablesolutions.akka.actor.Actor
import se.scalablesolutions.akka.util.Logging
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
 * User: nowi
 * Date: 03.02.2010
 * Time: 17:11:44
 */

trait ProvingActor extends Actor

class OrderedResolutionProver1Actor(
        val dispatcherActor: DispatchingActor) extends ProvingActor {

  // cofigure a native os thread based dispatcher for the proving actor
  val d = Dispatchers.newThreadBasedDispatcher(this)


  messageDispatcher = d


  val coreProverConfig = new Object() {
    // empty initial clauses
    lazy val initialClauses = CNFClauseStore()

    lazy val tautologyDeleter = new TautologyDeleter()
    lazy val variableRewriter = new VariableRewriter()
    lazy val subsumptionDeleter = new SubsumptionDeleter(this)
    lazy val standardizer = new Standardizer(this)
    lazy val unificator = new Unificator(this)
    lazy val substitutor = new Substitutor(this)
    lazy val factorizer = new OrderedFactorizer(this)
    lazy val resolver = new OrderedResolver(this)
    lazy val subsumptionStrategy = new StillmannSubsumer(this)

    // ordered resolution needs comparator and selection too
    lazy val precedence = new CustomSPASSModule1Precedence
    lazy val literalComparator = new ALCLPOComparator(this)
    lazy val selector = new NegativeLiteralsSelection()

    // settings
    val recordProofSteps = true
    val removeDuplicates = false
    val useLightesClauseHeuristic = true
    val usableBackSubsumption = false
    val forwardSubsumption = true
    val dropSeenClauses = false
    val useIndexing = true

    // set a time limit
    val timeLimit: Long = (20 * 1000)  // 10 sec
  }

  val prover = new ResolutionProover1(coreProverConfig)
  prover.addObserver(this)

  log.info("Core prooving subsystem started up with kernel : %s", prover)


  // add this actor as observer for derived clauses





  private var state: ProvingState = STOPPED

  private var initialClauses: ClauseStorage = CNFClauseStore()
  private var keptClauses: ClauseStorage = CNFClauseStore()


  private[this] def setState(newState: ProvingState, supervisor: Actor) {
    log.info("%s transitions from %s to %s state", this, state, newState)
    state = newState
    // inform supervisor about state change
    supervisor ! ProverStatus(state)


  }


  /**
   * Callback from proving alogorithm
   *
   */
  def receiveUpdate(subject: Any) {
    //println("Recieved derived clauses , sending them to dispatcher actor")
    // send derived clauses to clause dispatcher
    subject match {
      case clauses: ClauseStorage => dispatcherActor ! (Entail(clauses), this)
      case _ => throw new IllegalArgumentException("Callback can only process ClauseStores")
    }


  }


  def doSaturate(sender: Actor) {
    keptClauses match {
      case NonEmptyClauseStore(clauses) => log.info("ReStarting saturation ...")
      case _ => log.info("Starting saturation ...")
    }

    setState(SATURATING, sender);
    val result = prover.saturate(initialClauses, keptClauses)
    // finished saturatign

    result match {
      case (COMPLETION, clauses: ClauseStorage) => {
        // save the kept clauses
        log.info("LOCALY SATISFIED ... ")
        keptClauses = clauses
        // clear the initial clauses
        initialClauses = CNFClauseStore()
        setState(SATISFIABLE, sender)
      }
      case (PROOF, clauses: ClauseStorage) => {
        log.info("LOCALY UNSATISFIED ! ")
        setState(UNSATISFIABLE, sender)
      }

      case (TIMEUP, clauses: ClauseStorage) => {
        log.info("TIMELIMIT REACHED ! ")
        keptClauses = clauses
        setState(SATISFIABLE, sender)
      }

    }
  }

  protected def receive = {
    case (Entail(clauses), sender: Actor) => {
      log.debug("Recieved Entail Message with clauses to entail  %s", clauses)
      // first check the state
      state match {
        case SATISFIABLE => {
          // the kb is satisfiable, we can resume prooving with the keptclauses , and add the recieved clause
          initialClauses = clauses ::: initialClauses
          doSaturate(sender)
        }
        case _ => {
          // drop the recived clause

        }
      }

    }


     case msg @ GetStatus(bla) => {
      log.debug("Recieved Status Request Message")
      reply(Status("IDLE ! , Loaded Clauses Are : %s , workedoff CLauses are  : %s  " format (initialClauses,keptClauses))) 


    }

    case (LoadClauses(clauses), sender: Actor) => {
      log.trace("%s Recieved Load Message with clauses to load %s", this, clauses)
      if (state == STOPPED) {
        initialClauses = clauses;
        setState(LOADED, sender)
      } else {
        throw new IllegalStateException("Cannot load initial clauses while in state : %s" format (state))
      }

    }

    case GetKeptClauses(bla) => {
      log.info("Recieved GetKeptClauses Request Message")
      state match {
        case SATISFIABLE => reply(KeptClauses(keptClauses))
        case _ => {
          log.warning("KeptCLauses have been requested but prover is not in state SATISFIABLE")
          reply(KeptClauses(keptClauses))
        }
      }

    }


    case (StartSatisfy(message), sender: Actor) => {
      log.trace("%s Recieved StartSatisfy Message", this)

      state match {
        case LOADED | SATISFIABLE | UNSATISFIABLE => {
           doSaturate(sender)
        // reply the current status to the original sender of the message
        reply(ProverStatus(state))
        }
        case _ => {
            throw new IllegalStateException("Cannot load initial clauses while in state : %s" format (state))
        }
      }
      

    }

    case (StopSatisfy(message), sender: Actor) =>
      log.trace("%s Recieved StopStatisfy Message", this)
  }

  override def shutdown = {
    log.info("Core prooving subsystem is shutting down...")
  }


}





