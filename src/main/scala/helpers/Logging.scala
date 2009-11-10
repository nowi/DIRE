package helpers


import org.slf4j.LoggerFactory

/**
 * User: nowi
 * Date: 10.11.2009
 * Time: 13:57:53
 */

trait Logging {
  val log = LoggerFactory getLogger (this getClass)
}